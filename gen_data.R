# 实验1-生成三分类数据的函数
generate_three_class_data <- function(n, p = 1000, q = 5, seed = 123) {
  
  # set.seed(seed)
  
  n_per_class <- n %/% 3  # 每个类别的样本数
  mu0 <- c(rep(0.6, q), rep(0, p - q))  # 类别0的均值向量
  mu1 <- c(rep(0.0, q), rep(0, p - q))  # 类别1的均值向量
  mu2 <- c(rep(-0.6, q), rep(0, p - q)) # 类别2的均值向量
  
  Sigma <- matrix(0.2, nrow = p, ncol = p)
  diag(Sigma) <- 1
  
  library(MASS)
  X0 <- mvrnorm(n_per_class, mu = mu0, Sigma = Sigma)
  y0 <- rep(1, n_per_class)
  
  X1 <- mvrnorm(n_per_class, mu = mu1, Sigma = Sigma)
  y1 <- rep(2, n_per_class)
  
  X2 <- mvrnorm(n_per_class, mu = mu2, Sigma = Sigma)
  y2 <- rep(3, n_per_class)
  
  X <- rbind(X0, X1, X2)
  y <- c(y0, y1, y2)
  
  indices <- sample(1:nrow(X))
  X <- X[indices, ]
  y <- y[indices]
  
  return(list(X = X, y = y, params = list(p = p, q = q, 
                                          mu0 = mu0, mu1 = mu1, mu2 = mu2,
                                          Sigma = Sigma)))
}


# 实验2的三分类数据
generate_three_class_data <- function(n, p = 1000, q = 4, seed = 123) {
  set.seed(seed)
  
  if (n %% 3 != 0) {
    n <- n - (n %% 3)
    warning(paste("调整样本量为", n, "以确保三类样本数相等"))
  }
  n_per_class <- n %/% 3
  
  beta1 <- c(rep(2, q), rep(0, p - q))                    # 类别1：前q个特征重要
  beta2 <- c(rep(0, q), rep(2, q), rep(0, p - 2*q))       # 类别2：中间q个特征重要
  beta3 <- c(rep(0, 2*q), rep(2, q), rep(0, p - 3*q))     # 类别3：再后面q个特征重要
  
  Sigma <- matrix(0, nrow = p, ncol = p)
  for(i in 1:p) {
    for(j in 1:p) {
      Sigma[i, j] <- 0.4^abs(i - j)
    }
  }
  
  generate_class_samples <- function(class_beta, n_samples, class_label) {
    X_class <- MASS::mvrnorm(n_samples, mu = rep(0, p), Sigma = Sigma)
    
    s_target <- X_class %*% class_beta
    s_other1 <- X_class %*% beta1
    s_other2 <- X_class %*% beta2
    s_other3 <- X_class %*% beta3
    
    scores <- cbind(s_other1, s_other2, s_other3)
    
    y_class <- rep(class_label, n_samples)
    
    return(list(X = X_class, y = y_class))
  }
  
  class1_data <- generate_class_samples(beta1, n_per_class, 1)
  class2_data <- generate_class_samples(beta2, n_per_class, 2)  
  class3_data <- generate_class_samples(beta3, n_per_class, 3)
  
  X <- rbind(class1_data$X, class2_data$X, class3_data$X)
  y <- c(class1_data$y, class2_data$y, class3_data$y)
  
  indices <- sample(1:nrow(X))
  X <- X[indices, ]
  y <- y[indices]
  
  return(list(X = X, y = y))
}


# 实验3
generate_three_class_data <- function(n, p = 1000, q = 4, seed = 123, separability = 2.0) {
  
  #set.seed(seed)
  
  if (n %% 3 != 0) {
    n <- n - (n %% 3)
    warning(paste("调整样本量为", n, "以确保三类样本数相等"))
  }
  
  beta <- c(rep(separability, q), rep(0, p - q))
  
  Sigma <- matrix(0, nrow = p, ncol = p)
  for(i in 1:p) {
    for(j in 1:p) {
      Sigma[i, j] <- 0.4^abs(i - j)
    }
  }
  
  sigma_beta <- sqrt(t(beta) %*% Sigma %*% beta)[1,1]
  mu1 <- qnorm(1/3, sd = sigma_beta)
  mu2 <- qnorm(2/3, sd = sigma_beta)
  
  X <- MASS::mvrnorm(n, mu = rep(0, p), Sigma = Sigma)
  
  linear_score <- X %*% beta
  
  y <- rep(1, n)  # 初始化为第1类
  y[linear_score > mu1 & linear_score <= mu2] <- 2
  y[linear_score > mu2] <- 3
  
  cat("类别分布:", table(y), "\n")
  cat("理论阈值: μ1 =", round(mu1, 3), "μ2 =", round(mu2, 3), "\n")
  cat("线性得分标准差:", round(sd(linear_score), 3), "\n")
  cat("理论标准差:", round(sigma_beta, 3), "\n")
  
  return(list(X = X, y = y, beta = beta, thresholds = c(mu1, mu2)))
}


# REAL DATA
data <- read_excel("C:/Users/....../DATA.xlsx", sheet = 1)

X <- as.matrix(data[, 3:ncol(data)])
y <- as.integer(data[[2]])

X_scaled <- scale(X)

cat("特征矩阵维度:", dim(X_scaled), "\n")
cat("标签分布:\n")
print(table(y))

