source('../../crossvalidate.R')
source('../../crossvalidate.lognet.R')
source('../../gen_logreg_test_data.R')
source('../../cross_val_times.R')


#test_that("crossvalidate works for simple datafile"),{

test_crossvalidate<- function(){
  data <- gen_logreg_test_data()
  cv <- data$dfsmry
  nfolds <- 5
  cv$foldid <- sample(rep(seq(nfolds), length = nrow(cv)))
  
  cv$time_indices <- rep(seq(15),rep(nrow(cv)/15,15))
  time_folds <- gen_cross_val_times (cv$time_indices, estimation_length=10, test_length=1, nfolds=5)
    # time_indices: vector length of x flagging the time index of each row.
  # cv_glmnet <- cv.glmnet(x=as.matrix(cv[c('x1','x2','x3')]),
  #                 y=as.matrix(cv['conv']),
  #                 weights=as.matrix(cv['cnt']),
  #                 family='binomial',type.measure='deviance', foldid=cv$foldid, keep=T)
  cv_glmnet <- cv.glmnet(x=as.matrix(cv[c('x1','x2','x3')]),
                        y=as.matrix(cv[c('ny','y')]),
                        family='binomial',type.measure='deviance', foldid=cv$foldid, keep=T)
  lambda=cv_glmnet$lambda
  #get_fold <- create_simple_kfold(cv, nfolds)
  get_fold <- create_time_kfold(cv, cv$time_indices, time_folds )
  get_fold_1 <- create_aggregate(get_fold)
  formla <- '~ x1 + x2 + x3' # we remove intercept in formula
  data_to_matrix <- create_sparse_matrix_data(formla, no_intercept=T)
  #get_y_weights <- create_get_y_weights('conv', 'cnt')
  get_y_weights <- create_get_y_weights(c('ny','y'))
  train_model <- create_train_glmnet('binomial',lambda)
  test_model <- create_test_glmnet()
  errs <- crossvalidate(get_fold, nfolds, 
                        data_to_matrix, get_y_weights,
                        train_model, test_model)
  err_wts <- cvlognet(errs$prediction, errs$y, errs$weights, errs$foldid, 'deviance')
  cv_errs <- cv_errors (err_wts$errors, err_wts$weights, 'deviance')
  #plot(cv_glmnet$lambda,cv_glmnet$cvm)
  #points(cv_glmnet$lambda,cv_errs$cvm, col='red')
  #plot(cv_glmnet$lambda,cv_glmnet$cvsd)
  #points(cv_glmnet$lambda,cv_errs$cvsd, col='red')
  
  #list(cv_glmnet=cv_glmnet, prediction=errs$prediction, cv_errors=cv_errs)
}








a2 <- crossvalidate_test(cv, a1$lambda)  
#})