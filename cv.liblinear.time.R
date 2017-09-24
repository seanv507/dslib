cv.liblinear.time <-function (x, y, weights,  lambda = NULL, 
                         type.measure = c("mse", "deviance", "class", "auc", "mae"),
			time_indices, time_folds, 
                         grouped = TRUE, keep = FALSE, parallel = FALSE, type=0,...) 
{
  # warning liblinear uses type which matches type.measure if not explicitly in cv.liblinear forma arguments list
  # consider using type=0
  if (missing(type.measure)) 
    type.measure = "default"
  else type.measure = match.arg(type.measure)
  if (!is.null(lambda) && length(lambda) < 2) 
    stop("Need more than one value of lambda for cv.liblinear")
  N = nrow(x)
  if (missing(weights)) 
    weights = rep(1, N)
  else {
    weights = as.double(weights)
  }
  y = drop(y)
  liblinear.call = match.call(expand.dots = TRUE)
  which = match(c("type.measure",  
                  "keep", "time_indices", "time_folds"), names(liblinear.call), F)
  if (any(which)) 
    liblinear.call = liblinear.call[-which]
  liblinear.call[[1]] = as.name("LiblineaR")
  
  train_test_which = (time_indices >= time_folds$folds[1,'train_start'])  & 
    (time_indices < time_folds$folds[nrow(time_folds$folds),'test_end']) 
  if (is.matrix(y)) 
    y_sub = y[train_test_which, ]
  else y_sub = y[train_test_which]
  
  liblinear.object = LiblineaR(x[train_test_which,], y_sub, sample_weights = weights[train_test_which], 
                               lambda = lambda, type=type,...)
  liblinear.object$call = liblinear.call
  # TODO
  #nz = sapply(predict(liblinear.object, type = "nonzero"), 
  #            length)
  nz=c()
  
  nfolds <- time_folds$nfolds
  if ( nfolds < 3) 
    stop("nfolds must be bigger than 3; nfolds=10 recommended")
  cross_val_times_data <- gen_cross_val_times_data(x, y, weights, NULL, 
                                                   time_indices, time_folds)
  
  
  if (parallel) {
    outlist = foreach(i = seq(nfolds), .packages = c("LiblineaR")) %dopar% 
    {
      train_which = (time_indices >= time_folds$folds[i,'train_start'])  & 
                    (time_indices < time_folds$folds[i,'test_start']) 
      if (is.matrix(y)) 
        y_sub = y[train_which, ]
      else y_sub = y[train_which]
      LiblineaR(x[train_which, , drop = FALSE], y_sub, sample_weights = weights[train_which],
                lambda = lambda, type=type,...)
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
      
      outlist[[i]] = LiblineaR(x[train_which, , drop = FALSE], 
                               y_sub, sample_weights = weights[train_which], 
                               lambda = lambda, type=type,...)
    }
  }
  fun = paste("cv.liblinearnet")
  lambda = liblinear.object$lambda
  x <- cross_val_times_data$x_test
  y <- cross_val_times_data$y_test
  weights <- cross_val_times_data$weights_test
  offset <- cross_val_times_data$offset_test
  foldid <- cross_val_times_data$foldid
  cvstuff = do.call(fun, list(outlist, lambda, x, y, weights, 
                              foldid, type.measure, type, keep))
  
  cvm = cvstuff$cvm
  cvsd = cvstuff$cvsd
  nas = is.na(cvsd)
  if (any(nas)) {
    lambda = lambda[!nas]
    cvm = cvm[!nas]
    cvsd = cvsd[!nas]
    #nz = nz[!nas]
  }
  cvname = cvstuff$name
  out = list(lambda = lambda, cvm = cvm, cvsd = cvsd, cvup = cvm + 
               cvsd, cvlo = cvm - cvsd, nzero = nz, name = cvname, liblinear.fit = liblinear.object)
  if (keep) 
    out = c(out, list(fit.preval = cvstuff$fit.preval, foldid = foldid, time_folds))
  lamin = if (cvname == "AUC") 
    glmnet::getmin(lambda, -cvm, cvsd)
  else glmnet::getmin(lambda, cvm, cvsd)
  obj = c(out, as.list(lamin))
  class(obj) = "cv.glmnet"
  obj
}
  
