lognet_errors <-
function (predmat, y, weights, type.measure){
  
  typenames = c(mse = "Mean-Squared Error", mae = "Mean Absolute Error", 
    deviance = "Binomial Deviance", auc = "AUC", class = "Misclassification Error")
  if (type.measure == "default") 
    type.measure = "deviance"
  if (!match(type.measure, c("mse", "mae", "deviance", "auc", 
                             "class"), FALSE)) {
    warning("Only 'deviance', 'class', 'auc', 'mse' or 'mae'  available for binomial models; 'deviance' used")
    type.measure = "deviance"
  }
  N = nrow(predmat)
  if (missing(weights)) 
    weights = rep(1, N)
  else weights = as.double(weights)
  #if (is.data.frame(y)){
  #  if (ncol>1) y = as.matrix(y)
  #}
  
  y = as.matrix(y)
  y = drop(y)
  
  prob_min = 1e-05
  prob_max = 1 - prob_min
  
  nc = dim(y)
  # for logistic regression, 
  # y is either matrix of integer counts 
  # or the class as a factor (or eg integer which gets treated as factor).
  if (is.null(nc)) {
    y = as.factor(y)
    ntab = table(y)
    nc = as.integer(length(ntab))
    y = diag(nc)[as.numeric(y), ]
  }
  N = nrow(y)
  
  if ((N < 10) && type.measure == "auc") {
    warning("Too few (< 10) observations for type.measure='auc'; changed to type.measure='deviance'", 
            call. = FALSE)
    type.measure = "deviance"
  }
  
  n_models <- ncol(predmat)
      
  if (type.measure == "auc") {
    error = rep(NA, n_models)
    #good = rep(0, n_models)
    #  good is for flagging the nas 
    
    #good[i, seq(nlams[i])] = 1
     
    for (j in seq(n_models)) {
        error[ j] = auc.mat(y, predmat[, j], weights)
    }
    #N = apply(good, 2, sum)
    
    weights = sum(weights)
  }else {
    # normalise y to sum to 1 (if matrix of counts only? and put weighting on weights)
    ywt = apply(y, 1, sum)
    y = y/ywt
    weights = weights * ywt
    # create vector of valid rows for each lambda
    N = nrow(y) - apply(is.na(predmat), 2, sum)

    error = switch(type.measure, 
      mse = (y[, 1] - (1 - predmat))^2 + 
        (y[, 2] - predmat)^2, 
      mae = abs(y[, 1] - (1 - predmat)) + 
        abs(y[, 2] - predmat), 
      deviance = {
        predmat = pmin(pmax(predmat, prob_min), prob_max)
        lp = y[, 1] * log(1 - predmat) + y[, 2] * log(predmat)
        ly = log(y)
        ly[y == 0] = 0
        ly = drop((y * ly) %*% c(1, 1))
        2 * (ly - lp)
      }, 
      class = y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <= 
           0.5))
    # calculate mean error in each fold.
    # weights are sum of weights in each fold
    
    error = apply(error,2,weighted.mean,w=weights,na.rm=TRUE)
    weight = sum(weights)
      
  }
  list(error=error, weight = weight)
}

cvlognet <- function(prediction, y, weights, foldid, 
                     type.measure = c("mse", "deviance", "class", "auc", "mae")){
  typenames = c(mse = "Mean-Squared Error", mae = "Mean Absolute Error", 
                deviance = "Binomial Deviance", auc = "AUC", class = "Misclassification Error")
  if (type.measure == "default") 
    type.measure = "deviance"
  if (!match(type.measure, c("mse", "mae", "deviance", "auc", 
                             "class"), FALSE)) {
    warning("Only 'deviance', 'class', 'auc', 'mse' or 'mae'  available for binomial models; 'deviance' used")
    type.measure = "deviance"
  }
  
  N = nrow(prediction)
  if (missing(weights)) 
    weights = rep(1, N)
  else weights = unlist(weights) # incase df
  y = as.matrix(y)
  y = drop(y)
  
  nc = dim(y)
  # for logistic regression, 
  # y is either matrix of integer counts 
  # or the class as a factor (or eg integer which gets treated as factor).
  if (is.null(nc)) {
    y = as.factor(y)
    ntab = table(y)
    nc = as.integer(length(ntab))
    y = diag(nc)[as.numeric(y), ]
  }
  
  nfolds=max(foldid)
  outmat=matrix(NA,nfolds,ncol(prediction))
  wts <- numeric(nfolds)
  for (i_fold in seq(nfolds)){
    err_wgt <- lognet_errors(prediction[foldid==i_fold,,drop=F],
                             y[foldid==i_fold,,drop=F],
                             weights[foldid==i_fold],
                             type.measure)
    outmat[i_fold,] <- err_wgt$error
    wts[i_fold] <-  err_wgt$weight
 
  }
  list(errors=outmat, weights=wts)
}



# nb cannot get Mean SQUARE Error of revenue by weighting by revenue  
#cvm = apply(cvraw, 2, weighted.mean, w = weights, na.rm = TRUE)
#cvsd = sqrt(apply(scale(cvraw, cvm, FALSE)^2, 2, weighted.mean, w = weights, na.rm = TRUE)/(N - 1))
#  out = list(cvm = cvm, cvsd = cvsd, name = typenames[type.measure])
#  if (keep) 
#    out$fit.preval = predmat
#  out
#}


