# 
# 参数设置
K <- 8  # 类别数
d <- 6  # 数据维度
n_train <- 160  # 训练集总样本数
n_test <- 800    # 测试集总样本数
distance_factor <- 15

# 生成不平衡比例（类别 1~K 的样本数占比）
imbalance_ratio_train <- c(1, 2, 3, 1, 5, 2, 1, 1)  # 训练集类别比例
imbalance_ratio_test <- c(1, 2, 2, 1, 4, 1, 1, 1)   # 测试集类别比例

# 归一化，使其符合总样本数
imbalance_ratio_train <- round(imbalance_ratio_train / sum(imbalance_ratio_train) * n_train)
imbalance_ratio_test <- round(imbalance_ratio_test / sum(imbalance_ratio_test) * n_test)

# 确保样本数与 n_train 和 n_test 一致（可能会四舍五入导致总和变化，需调整）
imbalance_ratio_train[length(imbalance_ratio_train)] <- imbalance_ratio_train[length(imbalance_ratio_train)] + (n_train - sum(imbalance_ratio_train))
imbalance_ratio_test[length(imbalance_ratio_test)] <- imbalance_ratio_test[length(imbalance_ratio_test)] + (n_test - sum(imbalance_ratio_test))

# 生成均值向量
simplex <- function(K, d, distance_factor) {
  mat <- matrix(runif(K * d), nrow = K)
  mat <- apply(mat, 1, function(x) x / sum(x))  # 归一化
  mat <- mat * distance_factor
  return(t(mat))
}

# 使用调整过的均值向量生成数据
means <- simplex(K, d, distance_factor)
cov_matrix <- diag(d)  # 协方差矩阵


generate_data <- function(class_counts, means, cov_matrix) {
  data <- NULL
  labels <- NULL
  for (i in 1:K) {
    count <- ifelse(is.na(class_counts[i]), 0, class_counts[i])  # 处理 NA，确保是数值
    if (count > 0) {  # 只生成大于 0 的样本数
      x <- mvrnorm(count, means[i, ], cov_matrix)
      y <- rep(i, count)
      data <- rbind(data, x)
      labels <- c(labels, y)
    }
  }
  return(list(data = data, labels = as.integer(labels)))  # 修改这里，将因子转换为整数
}


# 生成训练集（不平衡）
train_data <- generate_data(imbalance_ratio_train, means, cov_matrix)
X_train <- train_data$data
y_train <- train_data$labels

# 生成测试集（不平衡）
test_data <- generate_data(imbalance_ratio_test, means, cov_matrix)
X_test <- test_data$data
y_test <- test_data$labels

# 输出不平衡数据的类别分布
print(table(y_train))
print(table(y_test))
