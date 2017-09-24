gen_cross_val_times <- function (time_indices, estimation_length, test_length, skip_length = NULL,nfolds = NULL, start_time = NULL, end_time = NULL){
  # time_indices: vector length of x flagging the time index of each row.
  # estimation_length: how many time_indices used for training
  # test_length: how many time_indices for validation
  # skip_length: how many time indices to jump for next test (default = test_length)
  # nfolds: how many separate tests to do, defaults to maximum
  # start_time: starting index
  
  min_time <- min(time_indices)
  max_time <- max(time_indices)
  
  if (is.null(start_time) ) start_time = min_time
  if (is.null(end_time) ) end_time = max_time
  if (is.null(skip_length)) skip_length = test_length
  max_length <- end_time - start_time +1
  
  max_nfolds <- (max_length  - estimation_length - test_length )/skip_length + 1
  if (is.null(nfolds)) nfolds = max_nfolds
  if (nfolds > max_nfolds) stop("Not enough data for cross validation")
  
  # generate train_start, train_end, test_start, test_end
  # test should be done on no overlapping set
  train_start <- start_time +(seq(nfolds) - 1) * skip_length
  test_start   <- train_start + estimation_length
  test_end <- test_start + test_length
  list(folds = data.frame(train_start,test_start,test_end ), start_time=start_time, end_time=end_time, nfolds=nfolds, skip_length=skip_length)
}


# gen_cross_val_times(seq(20),5,3)
# $folds
# train_start test_start test_end
# 1           1         6       9
# 2           4         9      12
# 3           7        12      15
# 4          10        15      18
# 5          13        18      21

gen_cross_val_times_data <- function(x, y, weights, offset, time_indices, time_folds){
  # takes complete data set
  foldid_list <- list()
  x_list <- list()
  y_list <- list()
  weights_list <- list()
  offset_list <- list()
  for ( i in seq(nrow(time_folds$folds))){
    ind <- (time_indices >= time_folds$folds[i,'test_start']) & 
      (time_indices < time_folds$folds[i,'test_end'])   
    x_list[[i]] <- x[ind,]
    
    if (is.matrix(y)) 
      y_list[[i]] <- y[ind,]
    else y_list[[i]] <- y[ind]

    weights_list[[i]] <- weights[ind]
    
    foldid_list[[i]] <- rep(i, nrow(x[ind,]))
    
    if (!is.null(offset)){
      offset_list[[i]] <- as.matrix(offset)[ind, ]
    }
  }
  
  x_test <- do.call(rbind, x_list)
  if (is.matrix(y_list[1])){
    y_test <- do.call(rbind, y_list)  
  }else{
    y_test <- do.call(c, y_list)  
  }
  
  # rbind doing something odd with vectors!! not same size
  weights_test <- do.call(c, weights_list)
  
  foldid <- do.call(c, foldid_list)
  
  if (!is.null(offset)){
    offset_test <- do.call(rbind, offset_list)
  }else offset_test <-offset
  
  list(x_test=x_test, y_test=y_test, weights_test=weights_test, offset_test=offset_test, foldid=foldid)
}

