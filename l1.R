# 
# ############################################################
# # DGP1 三分类数据
# ############################################################
# 
# generate_three_class_data <- function(n,
#                                       p = 1000,
#                                       q = 5,
#                                       seed = 123) {
#   
#   set.seed(seed)
#   
#   n_per_class <- n %/% 3
#   
#   mu0 <- c(rep(0.6, q), rep(0, p - q))
#   mu1 <- c(rep(0.2, q), rep(0, p - q))
#   mu2 <- c(rep(-0.6, q), rep(0, p - q))
#   
#   Sigma <- matrix(0.2, nrow = p, ncol = p)
#   diag(Sigma) <- 1
#   
#   X0 <- MASS::mvrnorm(n_per_class, mu0, Sigma)
#   X1 <- MASS::mvrnorm(n_per_class, mu1, Sigma)
#   X2 <- MASS::mvrnorm(n_per_class, mu2, Sigma)
#   
#   y0 <- rep(1, n_per_class)
#   y1 <- rep(2, n_per_class)
#   y2 <- rep(3, n_per_class)
#   
#   X <- rbind(X0, X1, X2)
#   y <- c(y0, y1, y2)
#   
#   idx <- sample(1:nrow(X))
#   
#   X <- X[idx, ]
#   y <- y[idx]
#   
#   return(list(X = X, y = y))
# }
# 
# build_nested_candidate_models <- function(X,
#                                           y,
#                                           nlambda = 50) {
#   
#   K <- length(unique(y))
#  
#   for (k in 1:K) {
#     
#     cat("Processing Class", k, "\n")
#     
#     y_bin <- ifelse(y == k, 1, 0)
#     
#     #######################################################
#     # LASSO Logistic
#     #######################################################
#     
#     fit <- glmnet(
#       X,
#       y_bin,
#       family = "binomial",
#       alpha = 1,
#       nlambda = nlambda,
#       standardize = TRUE
#     )
#     
#     lambda_seq <- fit$lambda
#     
#     #######################################################
#     # 每个 lambda 提取非零变量
#     #######################################################
#     
#     for (i in 1:length(lambda_seq)) {
#       
#       beta <- as.vector(coef(fit, s = lambda_seq[i]))[-1]
#       
#       selected <- which(abs(beta) > 1e-8)
#       
#       if (length(selected) > 0) {
#         
#         name_i <- paste0("lambda_", i)
#         
#         if (is.null(lambda_selected[[name_i]])) {
#           
#           lambda_selected[[name_i]] <- selected
#           
#         } else {
#           
#           lambda_selected[[name_i]] <-
#             union(lambda_selected[[name_i]],
#                   selected)
#         }
#       }
#     }
#   }
#   
#   ##########################################################
#   # 构造嵌套模型
#   ##########################################################
#   
#   nested_models <- list()
#   
#   cumulative_set <- c()
#   
#   feature_entry_order <- c()
#   
#   cat("\n")
#   cat("=================================================\n")
#   cat("Nested Candidate Models\n")
#   cat("=================================================\n")
#   
#   for (i in 1:length(lambda_selected)) {
#     
#     current_set <- lambda_selected[[i]]
#     
#     #######################################################
#     # 新进入变量
#     #######################################################
#     
#     new_features <- setdiff(current_set,
#                             cumulative_set)
#     
#     #######################################################
#     # 更新累计集合
#     #######################################################
#     
#     cumulative_set <- union(cumulative_set,
#                             current_set)
#     
#     nested_models[[i]] <- sort(cumulative_set)
#     
#     #######################################################
#     # 记录进入顺序
#     #######################################################
#     
#     if (length(new_features) > 0) {
#       
#       feature_entry_order <-
#         c(feature_entry_order,
#           new_features)
#     }
#     
#     #######################################################
#     # 输出
#     #######################################################
#     
#     cat("\n")
#     cat("Model", i, "\n")
#     
#     cat("New Features Entered:\n")
#     
#     if (length(new_features) == 0) {
#       
#       cat("None\n")
#       
#     } else {
#       
#       cat(paste0("X", new_features,
#                  collapse = ", "), "\n")
#     }
#     
#     cat("Nested Feature Set:\n")
#     
#     cat(paste0("X",
#                nested_models[[i]],
#                collapse = ", "),
#         "\n")
#     
#     cat("Feature Size:",
#         length(nested_models[[i]]),
#         "\n")
#   }
#   
#   ##########################################################
#   # 特征进入重要性排序
#   ##########################################################
#   
#   feature_ranking <- unique(feature_entry_order)
#   
#   cat("\n")
#   cat("=================================================\n")
#   cat("Feature Importance Ranking\n")
#   cat("=================================================\n")
#   
#   for (i in 1:length(feature_ranking)) {
#     
#     cat(i,
#         ": X",
#         feature_ranking[i],
#         "\n",
#         sep = "")
#   }
#   
#   ##########################################################
#   # 返回结果
#   ##########################################################
#   
#   return(list(
#     
#     nested_models = nested_models,
#     
#     feature_ranking = feature_ranking,
#     
#     lambda_selected = lambda_selected
#   ))
# }
# 
# ############################################################
# # 运行实验
# ############################################################
# 
# set.seed(123)
# 
# dat <- generate_three_class_data(
#   n = 600,
#   p = 1000,
#   q = 5
# )
# 
# X <- dat$X
# y <- dat$y
# 
# 
# n <- nrow(X)
# 
# train_idx <- 1:floor(0.8 * n)
# 
# X <- X[train_idx, ]
# y <- y[train_idx]
# 
# ############################################################
# # 构造嵌套候选模型
# ############################################################
# 
# result <- build_nested_candidate_models(
#   X,
#   y,
#   nlambda = 30
# )
# 
# ############################################################
# # 提取结果
# ############################################################
# 
# nested_models <- result$nested_models
# 
# feature_ranking <- result$feature_ranking
# 
# ############################################################
# # 查看前10个进入的重要变量
# ############################################################
# 
# cat("\n")
# cat("Top 10 Important Features:\n")
# 
# print(feature_ranking[1:10])
# 
# ############################################################
# # 查看前5个嵌套模型
# ############################################################
# 
# cat("\n")
# cat("First 5 Nested Models:\n")
# 
# for (i in 1:5) {
#   
#   cat("\n")
#   
#   cat("Model", i, ":\n")
#   
#   print(nested_models[[i]])
# }


remsvm_l1_path <- function(X, y, a=1, gamma=0.5, lambda_l2=2^0, n_lambdas=20) {
  n <- nrow(X)
  p <- ncol(X)
  
  # 用于存储每一步的特征权重和活跃特征集（Active Set）
  path_W <- list()
  active_sets <- list()
  
  # 1. 拟合初始的 L2 模型，用于界定 L1 惩罚项 lambda 的最大搜索边界
  K_init <- eval.kernel(X, X, list(type="linear", par=1))
  model_init <- remsvm.compact(K_init, y, a, gamma, lambda=lambda_l2)
  W_init <- t(X) %*% model_init$vmat
  w_norms_init <- sqrt(rowSums(W_init^2)) # 计算每个特征在多分类上的 Group L2 范数
  
  # 构造 lambda_L1 的衰减序列（从极大值逐渐衰减到极小值，特征会逐渐进入模型）
  lambda_max <- max(w_norms_init) * 5
  lambda_min <- lambda_max * 1e-4
  lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out=n_lambdas))
  
  feature_entry_order <- c()      # 记录特征首次进入模型的顺序
  entered_features <- logical(p)  # 布尔向量，标记特征是否已经进入过
  
  cat("开始沿 L1 正则化路径拟合嵌套模型...\n")
  
  # 2. 沿着 lambda 路径依次计算
  for(i in 1:length(lambda_seq)) {
    lam1 <- lambda_seq[i]
    scale_fac <- rep(1, p) # 初始化缩放因子
    W_orig <- matrix(0, p, ncol(model_init$vmat))
    
    # LLA 迭代 (通常 3-5 次迭代即可收敛到 L1 稀疏解)
    for(iter in 1:3) { 
      # 按缩放因子调整 X
      X_scaled <- X %*% diag(scale_fac)
      K <- eval.kernel(X_scaled, X_scaled, list(type="linear", par=1))
      
      # 为保证二次规划数值稳定，在核矩阵对角线加微小扰动
      diag(K) <- diag(K) + 1e-5 
      
      # 拟合 L2 REMSVM
      model <- remsvm.compact(K, y, a, gamma, lambda=lambda_l2)
      
      # 提取当前缩放空间下的权重，并还原回原始空间
      W_scaled <- t(X_scaled) %*% model$vmat
      W_orig <- diag(scale_fac) %*% W_scaled
      w_norms <- sqrt(rowSums(W_orig^2))
      
      # 迭代更新缩放因子 (逼近 L1 惩罚)。加极小数防止除零。
      scale_fac <- sqrt(w_norms / (lam1 + 1e-8))
      scale_fac[scale_fac > 100] <- 100 # 防止数值爆炸
    }
    
    path_W[[i]] <- W_orig
    
    # 根据权重范数确定当前步的活跃特征集 (阈值设为 1e-3 过滤噪音)
    active_idx <- which(w_norms > 1e-3)
    active_sets[[i]] <- active_idx
    
    # 记录在这一步中 **新进入** 模型的特征，作为重要性排序的依据
    new_features <- setdiff(active_idx, which(entered_features))
    if(length(new_features) > 0) {
      feature_entry_order <- c(feature_entry_order, new_features)
      entered_features[new_features] <- TRUE
    }
    
    cat(sprintf("-> 步骤 %2d: Lambda = %7.4f, 选入特征数 = %d\n", i, lam1, length(active_idx)))
  }
  
  # 把从未进入过模型的冗余特征排在最末尾
  never_entered <- setdiff(1:p, feature_entry_order)
  final_importance <- c(feature_entry_order, never_entered)
  
  return(list(
    lambda_seq = lambda_seq,
    active_sets = active_sets,          # 嵌套候选子集
    feature_entry_order = final_importance, # 特征进入重要性排序
    path_W = path_W
  ))
}



cat("\n[1] 正在生成测试数据...\n")
data <- generate_three_class_data(n = 150, p = 20, q = 5, seed = 123)
X <- data$X
y <- data$y

cat("\n[2] 开始执行 L1-REMSVM 路径搜索...\n")
res <- remsvm_l1_path(X, y, a=1, gamma=0.5, lambda_l2=1, n_lambdas=15)
# 
# for(i in 1:length(res$lambda_seq)) {
#   subset_str <- if(length(res$active_sets[[i]]) == 0) "空集 {}" else paste(res$active_sets[[i]], collapse=", ")
#   cat(sprintf("第 %2d 步候选集 (Lambda=%.4f): 包含 %2d 个特征 -> {%s}\n", 
#               i, res$lambda_seq[i], length(res$active_sets[[i]]), subset_str))
# }

