test_metric <- function (y, predmat, weights, type.measure){
  prob_min = 1e-05
  prob_max = 1 - prob_min
  nc = dim(y)
  if (is.null(nc)) {
    y = as.factor(y)
    ntab = table(y)
    nc = as.integer(length(ntab))
    y = diag(nc)[as.numeric(y), ]
  }
  ywt = apply(y, 1, sum)
  y = y/ywt
  
  weights = weights * ywt
  N = nrow(y) - apply(is.na(predmat), 2, sum)
  cvraw = switch(type.measure, 
                 mse = (y[, 1] - (1 - predmat))^2 + (y[, 2] - predmat)^2, mae = abs(y[, 1] - (1 - predmat)) + abs(y[, 2] - predmat), 
                 deviance = {
                     predmat = pmin(pmax(predmat, prob_min), prob_max)
                     lp = y[, 1] * log(1 - predmat) + y[, 2] * log(predmat)
                     ly = log(y)
                     ly[y == 0] = 0
                     ly = drop((y * ly) %*% c(1, 1))
                     2 * (ly - lp)
                 }, 
                 class = y[, 1] * (predmat > 0.5) + y[, 2] * (predmat <=  0.5))
  ly = log(y)
  ly[y == 0] = 0
  ly = drop((y * ly) %*% c(1, 1))
  #nlams <- ncol(predmat)
  #cvob = cvcompute(cvraw, weights, foldid, nlams)
  #cvraw = cvob$cvraw
  #weights = cvob$weights
  #N = cvob$N
  list(cvraw=cvraw,weights=weights, N=N, y, ly)
}

create_ymat <- function(y){
  nc = dim(y)
  if (is.null(nc)) {
    y = as.factor(y)
    ntab = table(y)
    nc = as.integer(length(ntab))
    y = diag(nc)[as.numeric(y), ]
  }
}
