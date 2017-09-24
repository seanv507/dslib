

logistic<-function(x) 1/(1 + exp (- x))
#inverse logistic
logit<-function (p) log(p) -log(1-p)

stats_data<- function(mat, weights=NULL){
  if (is.null(weights)) {
    weights <- 1
    counts <- nrow(mat)
  }else {
    counts <- sum(weights)
    
  }
  
  sums <- colSums(mat * weights)
  stds <-  sqrt(((colSums(mat * mat * weights)  - sums * sums /counts) /(counts-1)))
  list(mean=sums/counts, std = stds)
}


gen_grouped_sample <- function( y, nfolds){
  # if we have grouped data so we sample 'instances' from the grouped data, not groups 
  # y is a n_groups x n_classes matrix (eg success/fail) so want to split the number of successes and failures between nfolds 
  # prob gets normalised just need to make vector of nfolds
  y_replicated <- sapply(t(y), function(x) rmultinom(1, x, prob=rep(1,nfolds)))
  #y_replicated is nfolds x ( row1 then row2,..)
  y_reshaped <- t(matrix(t(y_replicated), nrow=ncol(y)))
  
}


non_zero_coefs <- function( coef_sp_matrix) {
  nzc <- coef_sp_matrix@x
  names(nzc) <- coef_sp_matrix@Dimnames[[1]][coef_sp_matrix@i + 1]
  nzc
}