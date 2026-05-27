
# ============================================================
# 评估指标
# ============================================================

# 错误率
#   pred   : 预测概率矩阵 (n x K) 或 得分矩阵 (n x K) 或 预测类别向量 (n)
#   y_true : 真实标签 (整数向量)
error.rate <- function(pred, y_true) {
  y_true <- as.integer(y_true)
  
  if (is.matrix(pred) || is.data.frame(pred)) {
    class_pred <- apply(pred, 1, which.max)
  } else {
    class_pred <- as.integer(pred)
  }
  
  class_pred <- as.integer(factor(class_pred, levels = sort(unique(y_true))))
  y_map <- as.integer(factor(y_true, levels = sort(unique(y_true))))
  
  mean(class_pred != y_map)
}

# NHL
#   probs   : 预测概率矩阵 (n x K)，每一行概率和为1
#   y_true  : 真实标签 (整数向量)
multiclass_hinge_loss <- function(y_true, probs) {
  y_true <- as.integer(y_true)
  n <- length(y_true)
  K <- ncol(probs)
  
  if (K < max(y_true)) stop("概率矩阵列数小于最大类别标签")
  
  loss <- 0
  for (i in 1:n) {
    true_class <- y_true[i]
    true_score <- probs[i, true_class]
    
    max_margin <- 0
    for (j in 1:K) {
      if (j != true_class) {
        margin <- max(0, probs[i, j] - true_score + 1)
        max_margin <- max(max_margin, margin)
      }
    }
    loss <- loss + max_margin
  }
  return(loss / n)
}


# ============================================================
# ROC/AUC
calculate_multiclass_roc_auc <- function(probs, true_labels) {
  
  # 标签为数值型，并保持类别编码 1,2,...,K
  true_labels <- as.numeric(as.factor(true_labels))
  classes <- sort(unique(true_labels))
  n_class <- length(classes)
  
  roc_list <- list()
  auc_values <- numeric(n_class)
  
  # 对每个类别计算 One-vs-Rest ROC
  for (i in 1:n_class) {
    binary_true <- ifelse(true_labels == classes[i], 1, 0)
    class_probs <- probs[, i]
    
    if (sd(class_probs, na.rm = TRUE) == 0) {
      class_probs <- class_probs + rnorm(length(class_probs), 0, 1e-6)
    }
    
    roc_obj <- tryCatch({
      roc(binary_true, class_probs, quiet = TRUE, direction = "auto")
    }, error = function(e) {
      return(roc(c(0,1), c(0,1), quiet = TRUE))
    })
    
    roc_list[[i]] <- roc_obj
    auc_values[i] <- auc(roc_obj)
  }
  
  # 宏观平均 AUC
  macro_auc <- mean(auc_values)
  
  # Micro 平均
  all_binary <- unlist(lapply(classes, function(k) ifelse(true_labels == k, 1, 0)))
  all_probs  <- as.vector(probs)
  micro_roc  <- roc(all_binary, all_probs, quiet = TRUE, direction = "auto")
  micro_auc  <- auc(micro_roc)
  
  # 计算 Macro ROC 曲线
  fpr_seq <- seq(0, 1, length.out = 100)
  macro_tpr <- numeric(length(fpr_seq))
  for (j in 1:length(fpr_seq)) {
    tpr_vals <- sapply(roc_list, function(r) {
      idx <- which.min(abs(1 - r$specificities - fpr_seq[j]))
      if (length(idx)) r$sensitivities[idx] else NA
    })
    macro_tpr[j] <- mean(tpr_vals, na.rm = TRUE)
  }
  macro_roc <- list(fpr = fpr_seq, tpr = macro_tpr)
  
  return(list(
    roc_curves = roc_list,         # 各类别的 ROC 对象
    micro_roc  = micro_roc,        # Micro ROC 对象
    macro_roc  = macro_roc,        # Macro ROC 曲线（列表含 fpr, tpr）
    class_auc  = auc_values,       # 各类别 AUC
    macro_auc  = macro_auc,        # Macro AUC
    micro_auc  = micro_auc,        # Micro AUC
    classes    = classes           # 类别标签（数值）
  ))
}

# ============================================================
# 绘图
plot_multiclass_roc_complete <- function(roc_result, title = "Multiclass ROC Curves") {
  n_class <- length(roc_result$classes)

  class_colors <- c("red", "blue", "green", "orange", "brown", "pink")[1:n_class]
  
  par(mar = c(5, 5, 4, 2) + 0.1)
  plot(NULL, xlim = c(0,1), ylim = c(0,1),
       xlab = "False Positive Rate", ylab = "True Positive Rate",
       main = title, type = "n")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  # 各类别 ROC 曲线
  for (i in 1:n_class) {
    lines(1 - roc_result$roc_curves[[i]]$specificities,
          roc_result$roc_curves[[i]]$sensitivities,
          col = class_colors[i], lwd = 2)
  }
  # Micro ROC（黑色长虚线）
  lines(1 - roc_result$micro_roc$specificities,
        roc_result$micro_roc$sensitivities,
        col = "black", lwd = 3, lty = 5)
  
  # Macro ROC（紫色点划线）
  lines(roc_result$macro_roc$fpr, roc_result$macro_roc$tpr,
        col = "purple", lwd = 3, lty = 4)
  
  # 图例
  legend_lbl <- sprintf("Class %d (AUC=%.3f)", roc_result$classes, roc_result$class_auc)
  legend_lbl <- c(legend_lbl,
                  sprintf("Micro (AUC=%.3f)", roc_result$micro_auc),
                  sprintf("Macro (AUC=%.3f)", roc_result$macro_auc))
  legend_col <- c(class_colors, "black", "purple")
  legend_lty <- c(rep(1, n_class), 5, 4)
  legend_lwd <- c(rep(2, n_class), 3, 3)
  legend("bottomright", legend = legend_lbl, col = legend_col,
         lty = legend_lty, lwd = legend_lwd, cex = 0.8, bg = "white")
  grid()
}

# 保存函数
save_roc_plot_complete <- function(roc_result, filename, title = "Multiclass ROC Curves") {
  png(filename, width = 1000, height = 800, res = 150)
  plot_multiclass_roc_complete(roc_result, title)
  dev.off()
  cat("ROC curve saved to:", filename, "\n")
}