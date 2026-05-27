
rehinge <- function(a, gamma, y, fit) {
  # fit: n x k 矩阵，每行是各类别的决策值
  # y: 真实标签 (1..k)
  n <- length(y)
  k <- ncol(fit)
  loss <- 0
  for (i in 1:n) {
    yi <- y[i]
    for (j in 1:k) {
      if (j == yi) next
      margin <- fit[i, yi] - fit[i, j]
      loss <- loss + max(0, 1 - margin)
    }
  }
  return(loss / n)
}

# ---------- 生成候选特征子集 (基于 LASSO 排序的嵌套集合) ----------
generate.candidates <- function(x, y, k = NULL) {
  # k: 候选模型个数 (默认取变量数)
  p <- ncol(x)
  if (is.null(k)) k <- p
  
  if (length(unique(y)) > 2) {
    
    y.bin <- ifelse(y == sample(unique(y), 1), 1, 0)
  } else {
    y.bin <- y - 1   
  }
  
  cvfit <- cv.glmnet(x, y.bin, family = "binomial", alpha = 1)
  coefs <- abs(as.vector(coef(cvfit, s = "lambda.min"))[-1])
  ord <- order(coefs, decreasing = TRUE)
  # 取前 k 个变量，构造嵌套模型：第一个模型包含最重要的1个变量，第二个包含前2个，...
  cand.list <- lapply(1:k, function(i) ord[1:i])
  return(cand.list)
}

train.remsvm <- function(x, y, a, gamma, lambda.seq, kernel.type, kernel.par = 1, cv.fold = 5) {
  model.full <- remsvm(x, y, a, gamma, lambda.seq, kernel.type, kernel.par,
                       criterion = "0-1", cv = TRUE, fold = cv.fold)
  return(model.full)
}

# ---------- J-fold CV 优化模型平均权重 (最小化 hinge loss) ----------
opt.ma.weights <- function(train.x, train.y, candidate.models, a, gamma, kernel,
                           J = 5, w.init = NULL) {
  # candidate.models: 列表，每个元素是 remsvm 返回的模型（包含 model$vmat, model$bvec）
  M <- length(candidate.models)
  n <- nrow(train.x)
  
  # 划分 J 折
  fold.id <- sample(rep(1:J, length.out = n))
  
  fit.list <- lapply(candidate.models, function(mod) {
    K <- eval.kernel(train.x, train.x, kernel)
    predict.remsvm.compact(K, mod$model)
  })
  
  # 定义 J-fold CV 目标函数：1/J sum_{j} hinge loss on validation fold
  cv.loss <- function(w) {
    total.loss <- 0
    for (j in 1:J) {
      valid.idx <- which(fold.id == j)
      train.idx <- setdiff(1:n, valid.idx)
      
      combined.fit <- 0
      for (m in 1:M) {
        combined.fit <- combined.fit + w[m] * fit.list[[m]][valid.idx, , drop = FALSE]
      }
      total.loss <- total.loss + rehinge(a, gamma, train.y[valid.idx], combined.fit)
    }
    return(total.loss / J)
  }
  
  # 权重约束：sum(w) = 1, w >= 0
  ui <- rbind(rep(1, M), diag(M))
  ci <- c(1, rep(0, M))
  
  if (is.null(w.init)) w.init <- rep(1/M, M)
  
  opt <- constrOptim(theta = w.init, f = cv.loss, grad = NULL,
                     ui = ui, ci = ci, control = list(reltol = 1e-10))
  return(opt$par)
}

# ---------- SIC 权重（基于 hinge loss） ----------
sic.weights <- function(hinge.losses) {
  min.loss <- min(hinge.losses)
  diff <- hinge.losses - min.loss
  w <- exp(-diff / 2)
  w <- w / sum(w)
  return(w)
}

# ---------- 主函数：REMSVM 模型平均 ----------
rem.ma <- function(x, y, a = 0, gamma = 1, lambda.seq = seq(-2, 2, by = 0.5),
                   kernel.type = "rbf", kernel.par = 1,
                   candidate.indices = NULL,   # 若为 NULL 则使用 LASSO 嵌套
                   n.candidates = min(ncol(x), 30),
                   method = c("JCV", "SIC", "average"),
                   J = 5, cv.fold = 5,
                   x.test = NULL, y.test = NULL) {
  
  method <- match.arg(method)
  
  y <- as.integer(as.factor(y))
  x <- as.matrix(x)
  
  kernel <- list(type = kernel.type, par = kernel.par)
  
  # 1. 生成候选特征子集
  if (is.null(candidate.indices)) {
    cand.sets <- generate.candidates(x, y, k = n.candidates)
  } else {
    cand.sets <- candidate.indices   # 用户提供的列表，每个元素是列下标向量
  }
  
  # 2. 对每个子集训练 REMSVM 
  cat("Training candidate REMSVM models...\n")
  M <- length(cand.sets)
  models <- vector("list", M)
  hinge.train <- numeric(M)
  
  for (m in 1:M) {
    cols <- cand.sets[[m]]
    if (length(cols) < 1) next
    subx <- x[, cols, drop = FALSE]
    mod <- train.remsvm(subx, y, a, gamma, lambda.seq, kernel.type, kernel.par, cv.fold)
    models[[m]] <- list(model = mod$model, columns = cols)
    Ksub <- eval.kernel(subx, subx, kernel)
    fit.sub <- predict.remsvm.compact(Ksub, mod$model)
    hinge.train[m] <- rehinge(a, gamma, y, fit.sub)
    cat(sprintf("Model %d/%d trained, hinge = %.4f\n", m, M, hinge.train[m]))
  }
  
  # 3. 计算权重
  if (method == "average") {
    w <- rep(1/M, M)
  } else if (method == "SIC") {
    w <- sic.weights(hinge.train)
  } else if (method == "JCV") {
    cat("Optimizing weights via J-fold CV...\n")
    w <- opt.ma.weights(x, y, models, a, gamma, kernel, J = J)
  }
  names(w) <- paste0("mod", 1:M)
  
  # 4. 组合预测函数
  predict.rem.ma <- function(newx) {
    newx <- as.matrix(newx)
    n.new <- nrow(newx)
    # 初始化组合决策值矩阵
    k <- max(y)
    combined.fit <- matrix(0, n.new, k)
    for (m in 1:M) {
      if (w[m] < 1e-10) next
      cols <- models[[m]]$columns
      sub.newx <- newx[, cols, drop = FALSE]
      sub.trainx <- x[, cols, drop = FALSE]
      K.test <- eval.kernel(sub.newx, sub.trainx, kernel)
      fit.m <- predict.remsvm.compact(K.test, models[[m]]$model)
      combined.fit <- combined.fit + w[m] * fit.m
    }
    # 返回预测类别
    pred.class <- apply(combined.fit, 1, which.max)
    return(list(class = pred.class, fit = combined.fit))
  }
  
  result <- list(models = models, weights = w, method = method,
                 predict = predict.rem.ma)
  if (!is.null(x.test) && !is.null(y.test)) {
    y.test <- as.integer(as.factor(y.test))
    pred <- predict.rem.ma(x.test)
    test.error <- mean(pred$class != y.test)
    test.hinge <- rehinge(a, gamma, y.test, pred$fit)
    result$test.error <- test.error
    result$test.hinge <- test.hinge
    cat(sprintf("Test error = %.4f, Test hinge = %.4f\n", test.error, test.hinge))
  }
  
  return(result)
}

# # ---------- Bagging 版本（Bootstrap 重采样 + REMSVM + 平均） ----------
# rem.bagging <- function(x, y, a = 0, gamma = 1, lambda.seq = seq(-2, 2, by = 0.5),
#                         kernel.type = "rbf", kernel.par = 1,
#                         n.models = 50, cv.fold = 5) {
#   y <- as.integer(as.factor(y))
#   x <- as.matrix(x)
#   kernel <- list(type = kernel.type, par = kernel.par)
#   n <- nrow(x)
#   models <- vector("list", n.models)
#   
#   for (b in 1:n.models) {
#     idx <- sample(1:n, replace = TRUE)
#     xb <- x[idx, , drop = FALSE]
#     yb <- y[idx]
#     # 训练 REMSVM (自动 CV)
#     mod <- train.remsvm(xb, yb, a, gamma, lambda.seq, kernel.type, kernel.par, cv.fold)
#     models[[b]] <- mod$model
#     cat(sprintf("Bagging model %d/%d\n", b, n.models))
#   }
#   
#   predict.bag <- function(newx) {
#     newx <- as.matrix(newx)
#     n.new <- nrow(newx)
#     k <- max(y)
#     sum.fit <- matrix(0, n.new, k)
#     for (b in 1:n.models) {
#       K.test <- eval.kernel(newx, x, kernel)
#       fit <- predict.remsvm.compact(K.test, models[[b]])
#       sum.fit <- sum.fit + fit
#     }
#     avg.fit <- sum.fit / n.models
#     pred.class <- apply(avg.fit, 1, which.max)
#     return(list(class = pred.class, fit = avg.fit))
#   }
#   
#   return(list(models = models, predict = predict.bag))
# }

# test-模拟二分类数据
set.seed(123)
n <- 200; p <- 50
x <- matrix(rnorm(n * p), n, p)
y <- rep(1:2, each = n/2)  # 两类，1 和 2
# 添加一些区分信息
x[y == 1, 1:10] <- x[y == 1, 1:10] + 1

# 划分训练/测试
train.idx <- sample(1:n, 150)
test.idx <- setdiff(1:n, train.idx)

# 执行 JCV 加权的 REMSVM 模型平均
res.jcv <- rem.ma(x[train.idx, ], y[train.idx],
                  a = 0.05, gamma = 0.8,
                  lambda.seq = seq(-2, 2, by = 0.5),
                  kernel.type = "rbf", kernel.par = 1,
                  n.candidates = 15, method = "JCV", J = 5,
                  x.test = x[test.idx, ], y.test = y[test.idx])

# 查看权重
print(res.jcv$weights)

# 预测新数据
pred.new <- res.jcv$predict(x[test.idx, ])
table(pred.new$class, y[test.idx])

