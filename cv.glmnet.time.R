cv.glmnet.time <-
  function (x, y, weights, offset = NULL, lambda = NULL, 
            type.measure = c("mse", "deviance", "class", "auc", "mae"), 
            time_indices, time_folds,
            grouped = TRUE, keep = FALSE, parallel = FALSE, ...) {
    
    
  warning("original cv.glmnet expects all training data and crossvalidation data to be contiguous....")
  # trains on full dataset.
  # so ideally the data set should be fixed to max length (ie dataset that will be used to train full model after crossval)
  # added train_test_which to perform this... not ideal (should it instead be done before?)
  if (missing(type.measure)) 
    type.measure = "default"
  else type.measure = match.arg(type.measure)
  if (!is.null(lambda) && length(lambda) < 2) 
    stop("Need more than one value of lambda for cv.glmnet")
  
  N = nrow(x)
  if (missing(weights)) 
    weights = rep(1, N)
  else weights = as.double(weights)
  y = drop(y)
  glmnet.call = match.call(expand.dots = TRUE)
  which = match(c("type.measure", "grouped", 
    "keep", "time_indices", "time_folds"), names(glmnet.call), F)
  if (any(which)) 
    glmnet.call = glmnet.call[-which]
    
  glmnet.call[[1]] = as.name("glmnet")
  
  train_test_which = (time_indices >= time_folds$folds[1,'train_start'])  & 
    (time_indices < time_folds$folds[nrow(time_folds$folds),'test_end']) 
  if (is.matrix(y)) 
    y_sub = y[train_test_which, ]
  else y_sub = y[train_test_which]
  if (!is.null(offset)) 
    offset_sub = as.matrix(offset)[train_test_which, ]
  else offset_sub = NULL
  #TODO what is 
  
  # TODO decide what this is for????
  glmnet.object = glmnet(x[train_test_which,], y_sub, weights = weights[train_test_which], 
                         offset = offset_sub, lambda = lambda, ...)
  glmnet.object$call = glmnet.call
  is.offset = glmnet.object$offset
###Next line is commented out so each call generates its own lambda sequence
# lambda=glmnet.object$lambda
 if (inherits(glmnet.object, "multnet") && !glmnet.object$grouped) {
    nz = predict(glmnet.object, type = "nonzero")
    nz = sapply(nz, function(x) sapply(x, length))
    nz = ceiling(apply(nz, 1, median))
  }
  else nz = sapply(predict(glmnet.object, type = "nonzero"), 
         length)
  
  nfolds <- time_folds$nfolds
  if ( nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  cross_val_times_data <- gen_cross_val_times_data(x, y, weights, offset, 
                                                   time_indices, time_folds)
  
  if (parallel) {
#  if (parallel && require(foreach)) {
    outlist = foreach(i = seq(nfolds), .packages = c("glmnet")) %dopar% 
    {
      train_which = (time_indices >= time_folds$folds[i,'train_start'])  & 
                    (time_indices < time_folds$folds[i,'test_start']) 
      if (is.matrix(y)) 
        y_sub = y[train_which, ]
      else y_sub = y[train_which]
      if (is.offset) 
        offset_sub = as.matrix(offset)[train_which, ]
      else offset_sub = NULL
      glmnet(x[train_which, , drop = FALSE], y_sub, lambda = lambda, 
             offset = offset_sub, weights = weights[ train_which], 
             ...)
    }
  }
  else {
    outlist = as.list(seq(nfolds))
    for (i in seq(nfolds)) {
      train_which = (time_indices >= time_folds$folds[i,'train_start'])  & 
        (time_indices < time_folds$folds[i,'test_start']) 
      if (is.matrix(y)) 
        y_sub = y[train_which, ]
      else y_sub = y[train_which]
      if (is.offset) 
        offset_sub = as.matrix(offset)[train_which, ]
      else offset_sub = NULL
      outlist[[i]] = glmnet(x[train_which, , drop = FALSE], 
               y_sub, lambda = lambda, offset = offset_sub, 
               weights = weights[train_which], ...)
    }
  }
  fun = paste("cv", class(glmnet.object)[[1]], sep = ".")
  lambda = glmnet.object$lambda
  
  x <- cross_val_times_data$x_test
  y <- cross_val_times_data$y_test
  weights <- cross_val_times_data$weights_test
  offset <- cross_val_times_data$offset_test
  foldid <- cross_val_times_data$foldid
  
  cvstuff = do.call(fun, list(outlist, lambda, 
                              x,y, weights, 
                              offset, foldid, type.measure, grouped, keep))
  cvm = cvstuff$cvm
  cvsd = cvstuff$cvsd
  nas=is.na(cvsd)
  if(any(nas)){
    lambda=lambda[!nas]
    cvm=cvm[!nas]
    cvsd=cvsd[!nas]
    nz=nz[!nas]
  }
  cvname = cvstuff$name
  out = list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + 
    cvsd, cvlo = cvm - cvsd, nzero = nz, name = cvname, glmnet.fit = glmnet.object)
  if (keep) 
    out = c(out, list(fit.preval = cvstuff$fit.preval, foldid = foldid, time_folds))
  lamin=if(cvname=="AUC")getmin(lambda,-cvm,cvsd)
  else getmin(lambda, cvm, cvsd)
  obj = c(out, as.list(lamin))
  class(obj) = "cv.glmnet"
  obj
  }
