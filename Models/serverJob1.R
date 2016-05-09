library(doParallel)
library(randomForest)
library(data.table)
load('sf_train.Rda')
system('mkdir log')
cl <- makeCluster(40)
registerDoParallel(cl)
train_rf <- foreach(i = 1:40, .combine = 'combine', .packages = c('randomForest', 'data.table')) %dopar% {
  log_file <- paste('log/', Sys.Date(), '_log_', i, '.txt', sep = '')
  sink(log_file, append = T, type = 'output')
  train_rf <- randomForest(sf_train[, -1, with = F], sf_train$Category, ntree = 25, replace = F, do.trace = T)
}
save(train_rf, file = 'train_rf.Rda')
stopCluster(cl)




