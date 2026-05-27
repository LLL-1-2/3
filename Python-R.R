# 从R调用Python的代码也可
# models <- list()
# train_fits <- list()
# test_fits <- list()
# for (col in candidate_sets) {
#   model <- train_remsvm(x[,col], y, ...)
#   K_train <- eval.kernel(x[,col], x[,col], kernel)
#   K_test <- eval.kernel(x_test[,col], x[,col], kernel)
#   train_fit <- predict.remsvm.compact(K_train, model$model)
#   test_fit <- predict.remsvm.compact(K_test, model$model)
#   train_fits[[length(train_fits)+1]] <- train_fit
#   test_fits[[length(test_fits)+1]] <- test_fit
# }
# 
# library(reticulate)
# source_python("ma_core.py")
# w <- jcv_weights(train_fits, as.integer(y), J = 5L)
# err <- eval_performance(w, test_fits, as.integer(y_test))