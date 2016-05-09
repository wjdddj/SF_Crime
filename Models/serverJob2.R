library(nnet)

mcll <- function(pred, true_label){
  votes <- pmax(pmin(pred, 1-1e-15), 1e-15)
  classes <- levels(true_label)
  n_record <- length(true_label)
  score <- sum(sapply(1:n_record, function(x){
    log(votes[x,][classes == true_label[x]])
  }))
  score <- -score/n_record
  score
}

load('sf_train.Rda')
sf_train$Address <- NULL
sf_train$X <- NULL
sf_train$Y <- NULL

train_nn <- nnet(Category~., data = sf_train, size = 5, maxit = 100)
mcll(train_nn1$fitted.values, sf_train1$Category)
save(train_nn)