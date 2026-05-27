library(openxlsx)
library(dplyr)
library(knitr)
library(kableExtra)


file_paths <- c(
  "C:/Users/....../DGP1-test/DGP1-smo_rem_time_result2.xlsx",
  "C:/Users/....../DGP1-test/DGP1-smo_rem+ma+XGBoost_time_result2.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/WW_SVMICL_NHL_result2.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/WW_SVMICH_NHL_result2.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/WW_SCL_result.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/WW_SCH_result.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/DGP1-Bagging_time_result.xlsx",
  "C:/Users/.../Desktop/result/DGP1-test/DGP1-Adaboosting_time_result2.xlsx",
  "C:/Users//Desktop/result/DGP1-test/WW_UNIF_result.xlsx",
  "C:/Users//Desktop/result/DGP1-test/DGP1-GBM_result.xlsx",
  "C:/Users//Desktop/result/DGP1-test/DGP1-RF_result.xlsx"
)

# file_paths <- c(
#   "C:/Users//Desktop/result/DGP2-test/DGP2-smo_rem_time_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-smo_rem+ma+XGBoost_time_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-WW_SVMICL_NHL_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-WW_SVMICH_NHL_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-WW_SCL_result.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-WW_SCH_result.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-Bagging_time_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-Adaboosting_time_result2.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-WW_UNIF_result.xlsx",
#   "C:/Users//Desktop/result/DGP2-test/DGP2-GBM_result.xlsx",
#   "C:/Users/sktop/result/DGP2-test/DGP2-RF_result.xlsx"
# )
# 
# file_paths <- c(
# "C:/Users/result/DGP3/DGP3-smo_rem_time_result2.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-smo_rem+ma+XGBoost_time_result2.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-WW_SVMICL_NHL_result2.xlsx",
# "C:/Users/Desktop/result/DGP3/DGP3-WW_SVMICH_NHL_result2.xlsx",
# "C:/Users/Desktop/result/DGP3/DGP3-WW_SCL_result.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-WW_SCH_result.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-Bagging_time_result2.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-Adaboosting_time_result2.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-WW_UNIF_result.xlsx",
# "C:/Users//Desktop/result/DGP3/DGP3-GBM_result.xlsx",
# "C:/Users/ktop/result/DGP3/DGP3-RF_result.xlsx"
# )

# file_paths <- c(
#   "C:/Users//Desktop/result/r-smo_rem_time_result2.xlsx",
#   "C:/Users//Desktop/result/r-smo_rem+ma+XGBoost_time_result2.xlsx",
#   "C:/Users//Desktop/result/r-WW_SVMICL_NHL_result2.xlsx",
#   "C:/Users//Desktop/result/r-WW_SVMICH_NHL_result2.xlsx",
#   "C:/Users//Desktop/result/r-WW_SCL_result.xlsx",
#   "C:/Users//Desktop/result/r-WW_SCH_result.xlsx",
#   "C:/Users//Desktop/result/r-Bagging_time_result2.xlsx",
#   "C:/Users//Desktop/result/r-Adaboosting_time_result2.xlsx",
#   "C:/Users//Desktop/result/r-WW_UNIF_result.xlsx",
#   "C:/Users//Desktop/result/r-GBM_result.xlsx",
#   "C:/Users//Desktop/result/r-RF_result.xlsx"
# )

method_names <- c("REMSVM", "REMSVMMA", "SVMICL", "SVMICH","SCL", "SCH",
                  "BAG", "ADA", "UNIF", "GBM", "RF")



all_summary_data <- data.frame()

for (i in seq_along(file_paths)) {
  
  if (file.exists(file_paths[i])) {
    
    sheets <- openxlsx::getSheetNames(file_paths[i])
    
    if (!"Summary" %in% sheets) next
    
    df <- openxlsx::read.xlsx(file_paths[i], sheet = "Summary")
    
    needed_cols <- c("n_train","mean_error","sd_error","mean_nhl","sd_nhl")
    
    for (col in needed_cols) {
      if (!col %in% colnames(df)) df[[col]] <- NA
    }
    
    df <- df[,needed_cols]
    
    # ===== 统一数据类型 =====
    
    df$n_train <- suppressWarnings(as.numeric(trimws(df$n_train)))
    df$mean_error <- as.numeric(df$mean_error)
    df$sd_error <- as.numeric(df$sd_error)
    df$mean_nhl <- as.numeric(df$mean_nhl)
    df$sd_nhl <- as.numeric(df$sd_nhl)
    
    df$Method <- method_names[i]
    
    all_summary_data <- dplyr::bind_rows(all_summary_data, df)
  }
}

# ===============================
# 如果是 Real Data（只有一行）
# ===============================

if(all(is.na(all_summary_data$n_train))){
  all_summary_data$n_train <- "Real Data"
}

# ===============================
# 计算综合排名
# ===============================

calculate_overall_rank <- function(data){
  
  n_sizes <- sort(unique(data$n_train))
  
  method_ranks <- data.frame(Method = unique(data$Method))
  
  for(n_size in n_sizes){
    
    current_data <- data %>%
      dplyr::filter(n_train == n_size) %>%
      dplyr::arrange(mean_error) %>%
      dplyr::mutate(rank = dplyr::row_number())
    
    method_ranks <- dplyr::left_join(
      method_ranks,
      current_data %>% dplyr::select(Method,rank),
      by="Method"
    )
    
    colnames(method_ranks)[ncol(method_ranks)] <- paste0("rank_",n_size)
  }
  
  rank_cols <- grep("^rank_",names(method_ranks))
  
  method_ranks$avg_rank <- rowMeans(
    as.matrix(method_ranks[,rank_cols]),
    na.rm=TRUE
  )
  
  method_ranks <- method_ranks %>%
    dplyr::arrange(avg_rank)
  
  return(method_ranks)
}

overall_ranking <- calculate_overall_rank(all_summary_data)

# ===============================
# 生成 Error Rate 表
# ===============================

error_table_data <- all_summary_data %>%
  dplyr::select(Method,n_train,mean_error,sd_error) %>%
  dplyr::mutate(
    Error_Rate = sprintf("%.4f(%.4f)",mean_error,sd_error)
  ) %>%
  dplyr::select(Method,n_train,Error_Rate) %>%
  tidyr::pivot_wider(
    names_from = n_train,
    values_from = Error_Rate
  ) %>%
  dplyr::left_join(
    overall_ranking %>% dplyr::select(Method,avg_rank),
    by="Method"
  ) %>%
  dplyr::arrange(avg_rank)

# 删除排名列
error_table_data <- dplyr::select(error_table_data,-avg_rank)

# ===============================
# 显示表格
# ===============================

cat("Error Rate 比较表 (按性能从优到差排序)\n")

error_table_data %>%
  knitr::kable("html",caption="Error Rate Comparison") %>%
  kableExtra::kable_styling(
    bootstrap_options=c("striped","hover","condensed"),
    full_width=FALSE
  ) %>%
  kableExtra::column_spec(1,bold=TRUE)

output_path <- "C:/Users//Desktop/result/Error_Rate_Table.xlsx"

wb <- openxlsx::createWorkbook()

openxlsx::addWorksheet(wb,"Error_Rate")
openxlsx::addWorksheet(wb,"Ranking")

openxlsx::writeData(wb,"Error_Rate",error_table_data)
openxlsx::writeData(wb,"Ranking",overall_ranking)

openxlsx::saveWorkbook(wb,output_path,overwrite=TRUE)

cat("Excel 已生成:\n",output_path,"\n")
