nnet_cv <- function(data_table, folds = 10){
  library(doParallel)
  library(foreach)
  library(nnet)
  library(data.table)
  library(cvTools)
  
  ## need to address low frequency response
  subset <- cvFolds(n.sample, K)
  
  cl <- makeCluster(folds)
  registerDoParallel(cl)
  predOut <- foreach(i = 1:folds, .packages = c('data.table', 'nnet'), .combine = 'rbind') %dopar% {
    idx_test <- subset$subsets[subset$which==i]
    df_train <- data_table[-idx, with = F]
    df_test <- data_table[idx, with = F]
    fitnnet <- nnet(Category~., data = df_train, size = 10, maxit = 100)
    pred <- predict(fitnnet, newdata=df_test)
  }
  stopCluster(cl)
  
  z <- list(predOut = predOut)
}