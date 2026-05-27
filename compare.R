# 加载必要的库
library(ggplot2)
library(dplyr)
library(tidyr)

parse_model_file <- function(file_path, model_name) {
  # 读取文件内容
  content <- readLines(file_path)
  
  # 提取平均错误率
  avg_error <- as.numeric(gsub("平均错误率: ", "", content[1]))
  
  # 提取平均耗时
  avg_time <- as.numeric(gsub("平均每次迭代耗时 \\(秒\\): ", "", content[2]))
  
  # 提取每次迭代错误率
  errors <- as.numeric(unlist(strsplit(gsub("每次迭代错误率: ", "", content[3]), ", ")))
  
  # 返回一个数据框
  data.frame(
    Model = model_name,
    Error = errors,
    AvgError = avg_error,
    Time = avg_time
  )
}

#df1 <- parse_model_file("C:/Users/liyux/Desktop/result/svmovo+ma_final_result.txt", "svmovO+ma")
#df2 <- parse_model_file("C:/Users/liyux/Desktop/result/svmova+ma_final_result.txt", "svmovA+ma")
df3 <- parse_model_file("C:/Users/liyux/Desktop/result/rem_final_result.txt", "Remsvm")
df4 <- parse_model_file("C:/Users/liyux/Desktop/result/rem+ma_final_result.txt", "Remsvm+ma")
#df5 <- parse_model_file("C:/Users/liyux/Desktop/result/svmovo_final_result.txt", "svmovO")
#df6 <- parse_model_file("C:/Users/liyux/Desktop/result/svmova_final_result.txt", "svmovA")
df7 <- parse_model_file("C:/Users/liyux/Desktop/result/osqp_rem_final_result.txt", "osqp_Remsvm")
df8 <- parse_model_file("C:/Users/liyux/Desktop/result/osqp_rem+ma_final_result.txt", "osqp_Remsvm+ma")
df9 <- parse_model_file("C:/Users/liyux/Desktop/result/smo_rem_final_result.txt", "smo_Remsvm")
df10 <- parse_model_file("C:/Users/liyux/Desktop/result/smo_rem+ma_final_result.txt", "smo_Remsvm+ma")

df11 <- parse_model_file("C:/Users/liyux/Desktop/result/osqp_rem+ma+XGBoost_final_result.txt", "osqp_Remsvm+ma+XGBoost")
df12 <- parse_model_file("C:/Users/liyux/Desktop/result/smo_rem+ma+XGBoost_final_result.txt", "smo_Remsvm+ma+XGBoost")

#df13 <- parse_model_file("C:/Users/liyux/Desktop/result/2-osqp_rem+ma_final_result.txt", "2-osqp_Remsvm+ma")
#df14 <- parse_model_file("C:/Users/liyux/Desktop/result/2-smo_rem+ma_final_result.txt", "2-smo_Remsvm+ma")
df15 <- parse_model_file("C:/Users/liyux/Desktop/result/2-osqp_rem+ma+XGBoost_final_result.txt", "2-osqp_Remsvm+ma+XGBoost")
#df16 <- parse_model_file("C:/Users/liyux/Desktop/result/2-smo_rem+ma+XGBoost_final_result.txt", "2-smo_Remsvm+ma+XGBoost")


# 合并所有数据
#all_data <- bind_rows(df3, df4, df7, df8, df9, df10)

all_data <- bind_rows(df3, df4, df7, df8, df9, df10, df11, df12, df15)

# 创建箱线图并添加耗时信息
ggplot(all_data, aes(x = Model, y = Error, fill = Model)) +
  geom_boxplot() +
  # 添加平均错误率的点
  geom_point(aes(y = AvgError), color = "black", size = 3, shape = 18) +
  # 添加平均错误率数值标签
  geom_text(aes(y = AvgError, 
                label = sprintf("%.4f", AvgError)),
            color = "black", vjust = -1, size = 3.5) +
  # 添加耗时文本，位置设在y轴最大值以上
  geom_text(aes(label = paste("Time:", round(Time, 2), "s"), 
                y = max(Error) * 1.1), 
            size = 4, color = "blue") +
  # 调整y轴范围以容纳耗时文本
  scale_y_continuous(limits = c(0, max(all_data$Error) * 1.15)) +
                       labs(title = "模型错误率比较与耗时",
                            y = "错误率",
                            x = "模型",
                            caption = "黑数字表示平均错误率，上面蓝字每次迭代平均耗时") +
                       theme_minimal() +
                       theme(legend.position = "none")