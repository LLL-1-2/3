

label.to.binary <- function(y) {
  y <- as.integer(as.factor(y))
  if (length(unique(y)) != 2) stop("仅支持二分类")
  return(ifelse(y == 2, 1, -1))
}

get.score <- function(fit, classmap = c(1,2)) {
  if (ncol(fit) != 2) stop("不支持")
  return(fit[,2] - fit[,1])
}

# Hinge loss 
hinge.binary <- function(y, score) {
  pmax(0, 1 - y * score) |> mean()
}

# 错误率
error.rate.binary <- function(y, score) {
  pred <- ifelse(score > 0, 1, -1)
  mean(pred != y)
}

# TPR, FPR
TPR.FPR <- function(y, score) {
  ybin <- ifelse(y == 1, 1, 0)  # 正类为 1
  pred <- ifelse(score > 0, 1, -1)
  predpos <- ifelse(pred == 1, 1, 0)
  TP <- sum(predpos & ybin)
  FP <- sum(predpos & !ybin)
  P <- sum(ybin)
  N <- sum(!ybin)
  c(TPR = TP/P, FPR = FP/N)
}

generate_candidates <- function(x, y, k = NULL) {
  ynum <- label.to.binary(y)   # -1/1
  y01 <- ifelse(ynum == 1, 1, 0)
  cvfit <- cv.glmnet(x, y01, family = "binomial", alpha = 1)
  coefs <- abs(as.vector(coef(cvfit, s = "lambda.min"))[-1])
  ord <- order(coefs, decreasing = TRUE)
  p <- ncol(x)
  if (is.null(k)) k <- p
  k <- min(k, p)
  lapply(1:k, function(i) ord[1:i])
}

# ---------- 训练单个 REMSVM 候选模型 ----------
train_remsvm_cand <- function(x, y, cols, a, gamma, lambda.seq, kernel.type, kernel.par, cv.fold = 5) {
  subx <- x[, cols, drop = FALSE]
  yint <- as.integer(as.factor(y))
  mod.full <- remsvm(subx, yint, a, gamma, lambda.seq, kernel.type, kernel.par,
                     criterion = "0-1", cv = TRUE, fold = cv.fold)
  kernel <- list(type = kernel.type, par = kernel.par)
  Ksub <- eval.kernel(subx, subx, kernel)
  fit.sub <- predict.remsvm.compact(Ksub, mod.full$model)
  score <- get.score(fit.sub)
  ybin <- label.to.binary(y)
  hinge <- hinge.binary(ybin, score)
  list(model = mod.full$model, columns = cols, hinge = hinge)
}

# ---------- 信息准则 (ICL / ICH) ----------
icl_score <- function(hinge, nvars, n) {
  hinge + nvars * log(n)
}
ich_score <- function(hinge, nvars, n, Ln = sqrt(log(n))) {
  hinge + Ln * nvars * log(n)
}

# ---------- SIC 权重 ----------
sic_weights <- function(scores) {
  w <- exp(- (scores - min(scores)) / 2)
  w / sum(w)
}

# ---------- J‑fold CV 优化权重 (最小化 hinge) ----------
ma_jcv_weights <- function(x, y, cand.models, a, gamma, kernel, J = 5) {
  M <- length(cand.models)
  n <- nrow(x)
  ybin <- label.to.binary(y)
  score.list <- lapply(cand.models, function(cm) {
    subx <- x[, cm$columns, drop = FALSE]
    K <- eval.kernel(subx, x[, cm$columns, drop = FALSE], kernel)
    fit <- predict.remsvm.compact(K, cm$model)
    get.score(fit)
  })
  
  fold.id <- sample(rep(1:J, length.out = n))
  
  cv_loss <- function(w) {
    total <- 0
    for(j in 1:J) {
      idx <- which(fold.id == j)
      s <- 0
      for(m in 1:M) s <- s + w[m] * score.list[[m]][idx]
      total <- total + hinge.binary(ybin[idx], s)
    }
    total / J
  }
  
  ui <- rbind(rep(1, M), diag(M))
  ci <- c(1, rep(0, M))
  w0 <- rep(1/M, M)
  opt <- constrOptim(w0, cv_loss, grad = NULL, ui = ui, ci = ci,
                     control = list(reltol = 1e-10))
  opt$par
}

predict_ma <- function(newx, x, y, cand.models, weights, kernel) {
  n.new <- nrow(newx)
  M <- length(cand.models)
  score <- rep(0, n.new)
  for(m in 1:M) {
    w <- weights[m]
    if(w < 1e-12) next
    cols <- cand.models[[m]]$columns
    sub.train <- x[, cols, drop = FALSE]
    sub.new <- newx[, cols, drop = FALSE]
    K <- eval.kernel(sub.new, sub.train, kernel)
    fit <- predict.remsvm.compact(K, cand.models[[m]]$model)
    score <- score + w * get.score(fit)
  }
  score
}

# ---------- Bagging (Bootstrap REMSVM) ----------
rem_bagging <- function(x, y, a, gamma, lambda.seq, kernel.type, kernel.par,
                        n.models = 50, cv.fold = 5) {
  yint <- as.integer(as.factor(y))
  kernel <- list(type = kernel.type, par = kernel.par)
  n <- nrow(x)
  models <- vector("list", n.models)
  for(b in 1:n.models) {
    idx <- sample(1:n, replace = TRUE)
    mod.full <- remsvm(x[idx,,drop=FALSE], yint[idx], a, gamma, lambda.seq,
                       kernel.type, kernel.par, criterion="0-1", cv=TRUE, fold=cv.fold)
    models[[b]] <- mod.full$model
  }
  predict.bag <- function(newx) {
    newx <- as.matrix(newx)
    n.new <- nrow(newx)
    k <- length(unique(yint))
    sum.fit <- matrix(0, n.new, k)
    for(b in 1:n.models) {
      K <- eval.kernel(newx, x, kernel)
      fit <- predict.remsvm.compact(K, models[[b]])
      sum.fit <- sum.fit + fit
    }
    avg.fit <- sum.fit / n.models
    list(score = get.score(avg.fit), class = apply(avg.fit,1,which.max))
  }
  return(list(models = models, predict = predict.bag))
}

# -------------------- AdaBoost --------------------
rem_adaboost <- function(x, y, a, gamma, lambda.seq, kernel.type, kernel.par,
                         cand.columns = NULL, max.iter = 50, cv.fold = 5) {
  yint <- as.integer(as.factor(y))
  ybin <- label.to.binary(y)
  kernel <- list(type = kernel.type, par = kernel.par)
  n <- nrow(x); p <- ncol(x)
  
  if(is.null(cand.columns)) {
    cand.columns <- generate_candidates(x, y, k = max.iter)
  }
  M <- length(cand.columns)
  models <- list()
  alphas <- numeric()
  
  D <- rep(1/n, n)
  
  for(m in 1:min(M, max.iter)) {
    cols <- cand.columns[[m]]
    subx <- x[, cols, drop = FALSE]
    # 有放回采样
    idx <- sample(1:n, size=n, replace=TRUE, prob=D)
    xb <- subx[idx,,drop=FALSE]; yb <- yint[idx]
    
    mod.full <- remsvm(xb, yb, a, gamma, lambda.seq, kernel.type, kernel.par,
                       criterion="0-1", cv=TRUE, fold=cv.fold)
    model <- mod.full$model
    
    Ksub <- eval.kernel(subx, subx, kernel)
    fit.all <- predict.remsvm.compact(Ksub, model)
    score.all <- get.score(fit.all)
    pred.all <- ifelse(score.all > 0, 1, -1)
    epsilon <- sum(D * (pred.all != ybin))
    
    if(epsilon <= 0 || epsilon >= 0.5) break
    
    alpha <- 0.5 * log((1 - epsilon) / epsilon)
    alphas <- c(alphas, alpha)
    models[[length(models)+1]] <- model
    
    D <- D * exp(-alpha * ybin * pred.all)
    D <- D / sum(D)
  }
  
  predict.ada <- function(newx) {
    newx <- as.matrix(newx)
    n.new <- nrow(newx)
    M <- length(models)
    if(M == 0) return(rep(NA, n.new))
    
    score <- 0
    for(m in 1:M) {
      cols <- cand.columns[[m]]
      sub.train <- x[, cols, drop = FALSE]
      sub.new <- newx[, cols, drop = FALSE]
      K <- eval.kernel(sub.new, sub.train, kernel)
      fit <- predict.remsvm.compact(K, models[[m]])
      score <- score + alphas[m] * get.score(fit)
    }
    list(score = score, class = ifelse(score > 0, 2, 1))
  }
  
  list(models = models, alphas = alphas, predict = predict.ada)
}

# ---------- 综合对比主函数 ----------
#
# 参数:
#   x.train, y.train, x.test, y.test
#   a, gamma, lambda.seq: REMSVM 参数
#   kernel.type, kernel.par: 核参数
#   n.cand: 候选模型数量（若未提供特征子集则用 LASSO 生成）
#   J: J‑fold CV 折数
#
run_comparison <- function(x.train, y.train, x.test, y.test,
                           a=0, gamma=1, lambda.seq=seq(-2,2,by=0.5),
                           kernel.type="rbf", kernel.par=1,
                           n.cand=30, J=5, cv.fold=5) {
  
  ytrain <- as.integer(as.factor(y.train))
  ytest  <- as.integer(as.factor(y.test))
  ybin.train <- label.to.binary(ytrain)
  ybin.test  <- label.to.binary(ytest)
  
  kernel <- list(type=kernel.type, par=kernel.par)
  n <- nrow(x.train)
  
  # 生成候选特征子集
  cand.cols <- generate_candidates(x.train, ytrain, k = n.cand)
  M <- length(cand.cols)
  
  # 训练所有候选 REMSVM
  cat(sprintf("训练 %d 个候选 REMSVM...\n", M))
  cand.models <- lapply(1:M, function(i) {
    train_remsvm_cand(x.train, ytrain, cand.cols[[i]], a, gamma, lambda.seq,
                      kernel.type, kernel.par, cv.fold)
  })
  
  hinges <- sapply(cand.models, `[[`, "hinge")
  nvars <- sapply(cand.cols, length)
  
  # 1. ICL 选择
  icl.scores <- icl_score(hinges, nvars, n)
  icl.best <- which.min(icl.scores)
  w_icl <- rep(0, M); w_icl[icl.best] <- 1
  score_icl <- predict_ma(x.test, x.train, ytrain, cand.models, w_icl, kernel)
  
  # 2. ICH 选择
  ich.scores <- ich_score(hinges, nvars, n, Ln=sqrt(log(n)))
  ich.best <- which.min(ich.scores)
  w_ich <- rep(0, M); w_ich[ich.best] <- 1
  score_ich <- predict_ma(x.test, x.train, ytrain, cand.models, w_ich, kernel)
  
  # 3. SCL 权重
  w_scl <- sic_weights(icl.scores)
  score_scl <- predict_ma(x.test, x.train, ytrain, cand.models, w_scl, kernel)
  
  # 4. SCH 权重
  w_sch <- sic_weights(ich.scores)
  score_sch <- predict_ma(x.test, x.train, ytrain, cand.models, w_sch, kernel)
  
  # 5. MA (J‑fold CV 优化)
  w_ma <- ma_jcv_weights(x.train, ytrain, cand.models, a, gamma, kernel, J=J)
  score_ma <- predict_ma(x.test, x.train, ytrain, cand.models, w_ma, kernel)
  
  # 6. UNIF 等权
  w_unif <- rep(1/M, M)
  score_unif <- predict_ma(x.test, x.train, ytrain, cand.models, w_unif, kernel)
  
  # 7. Bagging
  bag <- rem_bagging(x.train, ytrain, a, gamma, lambda.seq, kernel.type, kernel.par,
                     n.models=50, cv.fold=cv.fold)
  pred.bag <- bag$predict(x.test)
  score_bag <- pred.bag$score
  
  # 8. AdaBoost
  ada <- rem_adaboost(x.train, ytrain, a, gamma, lambda.seq, kernel.type, kernel.par,
                      cand.columns = cand.cols, max.iter=M, cv.fold=cv.fold)
  pred.ada <- ada$predict(x.test)
  score_ada <- pred.ada$score
  
  # --- 计算指标 ---
  scores <- list(ICL=score_icl, ICH=score_ich, SCL=score_scl, SCH=score_sch,
                 MA=score_ma, UNIF=score_unif, Bagging=score_bag, AdaBoost=score_ada)
  
  # 最优 hinge loss (用作比率分母，使用原文 Python 逻辑：对测试集优化权重得到的最小 hinge)
  # 此处使用等权重下的 hinge 作为近似，或可调用优化函数；简单起见，我们用所有方法的最小 hinge 作为分母
  min.loss <- min(sapply(scores, function(s) hinge.binary(ybin.test, s)))
  
  results <- data.frame(
    method = names(scores),
    MSE = sapply(scores, function(s) error.rate.binary(ybin.test, s)),
    Hinge = sapply(scores, function(s) hinge.binary(ybin.test, s)),
    Ratio = sapply(scores, function(s) hinge.binary(ybin.test, s) / min.loss)
  )
  
  # TPR/FPR
  tprfpr <- t(sapply(scores, function(s) TPR.FPR(ybin.test, s)))
  results <- cbind(results, tprfpr)
  
  weights.out <- list(ICL=w_icl, ICH=w_ich, SCL=w_scl, SCH=w_sch, MA=w_ma, UNIF=w_unif)
  
  return(list(results = results, weights = weights.out, 
              cand.models = cand.models, cand.columns = cand.cols))
}
