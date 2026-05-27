library(ramsvm)
library(gtools)
set.seed(123)

K <- 3
d <- 4
n <- 15

simplex <- function(K, d, distance_factor = 2) {
  # 生成 K 个点，每个点是 d 维向量，满足点的和为 1
  mat <- matrix(runif(K * d), nrow = K)
  mat <- apply(mat, 1, function(x) x / sum(x))  # 归一化
  
  # 调整均值向量之间的距离
  # 增加一个因子来增加类别之间的间距
  mat <- mat * distance_factor
  return(t(mat))
}  # 均值向量的生成方式
means <- simplex(K, d, distance_factor = 30)  # 使用调整过的均值向量生成数据
cov_matrix <- diag(d)

generate_data <- function(n, means, cov_matrix) {
  data <- NULL
  labels <- NULL
  for (i in 1:K) {
    x <- mvrnorm(n, means[i, ], cov_matrix)  # 从正态分布生成数据
    y <- rep(i, n)  # 类别标签
    data <- rbind(data, x)
    labels <- c(labels, y)
  }
  return(list(data = data, labels = labels))
}
data <- generate_data(n, means, cov_matrix)
x <- data$data
y <- data$labels


# 定义候选特征组合，构建候选模型
# 循环从1个特征到所有特征，生成组合

candidate_results <- function(x, y){
  candidate_models <- list()
  for (i in 1:(d)){
    feature_combinations <- combn(ncol(x), i, simplify = FALSE)
    candidate_models[[i]] <- lapply(feature_combinations, function(features) {
      selected_x <- as.matrix(x[, features])
      ramsvm(selected_x, y, kernel = "linear", lambda = 0.2) 
    })
  }
  return(candidate_models)
}


# 分折
f <- 5
folds <- sample(1:f, size = nrow(x), replace = TRUE)

# 初始化存储结果
candicate_results_2 <- list()
test_x <- list()
test_y <- list()

for (k in 1:f) {
  # 划分训练集与测试集
  train_index <- which(folds != k)
  test_index <- which(folds == k)
  
  train_x <- x[train_index, ]
  train_y <- y[train_index]
  test_x <- x[test_index, ]
  test_y <- y[test_index]
  
  # 当前一折 得所有候选模型的结果提取系数
  fold_results <- candidate_results(train_x, train_y)
  # candicate_results_2[[k]] <- fold_results
  beta_s <- fold_results[[1]]$beta
  beta_0 <- fold_results[[1]]$beta0
  
  # 当前这一折验证集 用loss function，用刚刚的β
  #loss_j <- loss_function(w_s, test_x, test_y, beta_s, beta_0, gamma=0.5) 
  
    
}
# #存好了f折训练集的结果：每一折都包括了所有候选组合的k-1个f（x）的β值
# #存好了f折验证集的数据：test_x and test_y
# 之后对每一折验证集数据，构造候选组合，用对应折数对应候选组合的训练集结果（β_s）*w_s的β_w
# 对每一折验证集数据，把β_w用进loss function，加总，优化w_s
# 提出w_s的值

# 定义W_j
XI.gen <- function(k, kd) {
  
  tempA <- - (1.0 + sqrt(kd)) / ((kd - 1.0)^(1.5))
  tempB <- tempA + sqrt(kd / (kd - 1.0))
  
  XI <- matrix(data = tempA, nrow = k-1L, ncol = k)
  
  XI[,1L] <- 1.0/sqrt(kd - 1.0)
  
  for( ii in 2L:k ) XI[ii-1L,ii] <- tempB
  
  return(XI)
}


# 定义损失函数
# return(loss / n)
loss_function <- function(w_s, X, y, beta_s, beta_0, gamma=0.5) {
  n <- nrow(X)  # 样本数量
  d <- ncol(X) # 特征维度
  XI <- XI.gen(k, kd = as.double(k))
  loss <- 0
  
  for (i in 1:n) {
    f_xi <- X[i, ] %*% beta_s * w_s + beta_0 * w_s # f(x) 的形式
    inner_loss <- 0
    
    for (j in 1:k) {
      if (j != y[i]) {
        # 计算分类间的损失项
        inner_loss <- inner_loss + max(0, 1 + f_xi %*% XI[, j])
      }
    }
    
    # 计算类别正确的项
    correct_loss <- max(0, gamma * ((k - 1) - f_xi %*% XI[, y[i]]))
    
    # 更新总损失
    loss <- loss + (1 - gamma) * inner_loss + gamma * correct_loss
  }
  
  return(loss / n)
}

# 使用优化函数最小化损失，带约束条件
# return(result$par)
generate_optimizer <- function(X, y, beta_s, beta_0, gamma=0.5) {
  XI <- XI.gen(k, kd = as.double(k)) # 生成 W_j
  
  # 定义目标函数供优化器使用
  objective <- function(w_s) {
    loss_function(w_s, X, y, beta_s, XI, gamma, k)
  }
  
  # 线性约束条件：sum(w_s) = 1
  constraint <- function(w_s) {
    sum(w_s) - 1
  }
  
  # 使用 constrOptim 进行优化
  result <- constrOptim(theta = rep(1 / ncol(X), ncol(X)), 
                        f = objective, 
                        grad = NULL, 
                        ui = rbind(rep(1, ncol(X))), 
                        ci = 1)
  
  return(result$par)
}




# 分折交叉验证
define_folds <- function(x, y, f=5) {
  folds <- sample(1:f, size = nrow(x), replace = TRUE)
  
  results <- list()
  
  for (k in 1:f) {
    # 划分训练集与测试集
    train_index <- which(folds != k)
    test_index <- which(folds == k)
    
    train_x <- x[train_index, ]
    train_y <- y[train_index]
    test_x <- x[test_index, ]
    test_y <- y[test_index]
    
    # 当前一折的所有候选模型结果
    fold_results <- candidate_results(train_x, train_y)
    beta_list <- lapply(fold_results, function(res) res$beta)
    
    # 验证集优化 w_s
    XI <- XI.gen(k, ncol(X))
    objective <- function(w_s) {
      loss_function(w_s, test_x, test_y, beta_list, XI, gamma = 0.5, k)
    }
    
    # 优化 w_s
    ui <- rbind(rep(1, length(beta_list)))
    ci <- 1
    result <- constrOptim(theta = rep(1 / length(beta_list), length(beta_list)), 
                          f = objective, 
                          grad = NULL, 
                          ui = ui, 
                          ci = ci)
    
    results[[k]] <- list(w_s = result$par, beta_list = beta_list, test_x = test_x, test_y = test_y)
  }
  
  return(results)
}














# 权重优化
optimize_weights <- function(candidate_models, val_x, val_y, gamma, k) {
  losses <- sapply(candidate_models, function(model) {
    loss_function(model, val_x, val_y, gamma, k)
  })
  
  weights <- exp(-losses) / sum(exp(-losses)) # Softmax权重分配
  return(weights)
}


# 在每一折验证集上优化权重
final_weights <- lapply(1:K, function(k) {
  val_index <- which(folds == k)
  val_x <- x[val_index, ]
  val_y <- y[val_index]
  
  optimize_weights(candidate_models, val_x, val_y, gamma = 0.5, k = num_classes)
})


final_model <- function(candidate_models, final_weights) {
  weighted_models <- Map(function(model, weight) {
    coef(model) * weight
  }, candidate_models, final_weights)
  
  final_coef <- Reduce("+", weighted_models) # 加权平均
  return(final_coef)
}

final_coefs <- final_model(candidate_models, final_weights)











