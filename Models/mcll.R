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







