library(data.table)
library(MatrixModels)
library(glmnet)
if(!require(glmnetUtils)){
  install.packages('../glmnetUtils/',repos=NULL,type='source')
  library(glmnetUtils)  
}

library(pryr) # for memory usage

create_sparse_matrix_data <- function (formla, no_intercept=TRUE){
  if (no_intercept) formla = paste(formla,'-1')
  function (data) {
    sparse.model.matrix(as.formula(formla), data)
    #glmnetUtils:::makeModelComponents(as.formula(formla), data, sparse=TRUE)
  }
}

mat_concat <- function (l_mat){
  # is.matrix is false for Matrix class
  if (is.null(dim(l_mat[[1]]))){
    ans <- do.call(c, l_mat)
  }else{
    ans <- do.call(rbind, l_mat)
  }
}

create_simple_kfold <- function (data, nfolds){
  if (! ('foldid' %in% colnames(data))){
    foldid = sample(rep(seq(nfolds), length = nrow(data)))
  }else{
    foldid = data$foldid
  }
  function( i_fold) {
    which = foldid == i_fold
    list(train=data[!which, ], test = data[which, ])
  }
}


create_time_kfold <- function (data, time_indices, time_folds){
  f.call <- match.call(expand.dots = T)
  function( i_fold) {
    train_which = (time_indices >= time_folds$folds[i_fold,'train_start'])  & 
                    (time_indices < time_folds$folds[i_fold,'test_start'])
    test_which <- (time_indices >= time_folds$folds[i_fold,'test_start']) & 
      (time_indices < time_folds$folds[i_fold,'test_end'])
    list(train=data[train_which, ], test = data[test_which, ])
  }
  
}


create_aggregate_kfold <- function( get_fold, group_vars, sum_vars){
  f.call <- match.call(expand.dots = T)
  function (i_fold){
    train_test <- get_fold(i_fold)
    tr <- as.data.table(train_test$train)
    te <- as.data.table(train_test$test)
    # setdiff(names(tr), sum_vars)
  
    list(train=as.data.frame(tr[,lapply(.SD,sum),by= group_vars,.SDcols= sum_vars]),
         test =as.data.frame(te[,lapply(.SD,sum),by= group_vars,.SDcols= sum_vars]))
  }
}


create_train_glmnet <- function (family, lambda=NULL, ...){
  # we do not support the offset
  function(x, y, weights){
    if (is.null(weights)){
      # glmnet tests for missing rather than is.null
      glmnet(x, as.matrix(y), family=family, lambda = lambda, offset = NULL,   
             ...)  
    }else{
      # need as.vector to remove from dataframe and not as.matrix as otherwise can't do elementwise mult of y
      glmnet(x, as.matrix(y), family=family, weights = as.vector(weights), lambda = lambda, offset = NULL, 
             ...)  
    }
    
  }
}

create_train_liblinear <- function (type, lambda, ...){
  # we do not support the offset
  function(x, y, weights){
      LiblineaR(data=x, target=as.matrix(y), type=type, sample_weights = as.vector(weights), lambda = lambda, ...)  
  }
}

create_train_base <- function(){
  function(x,y, weights){
    if (is.matrix(y) && ncol(y)==2){
      # assume aggregated!!!
    }
  }
}




create_test_glmnet <- function (...){
  # we do not support the offset
  function(model, x){ 
    predict(model, newx=x, type='response', ...)
  }
}

create_test_liblinear <- function (...){
  # we do not support the offset
  function(model, x){ 
    predict(model, newx=x, type='response', ...)
  }
}



create_get_y_weights <- function (y_names, weights_name=NULL){
  function (data){
    if(is.null(weights_name)) {
      weights = rep(1, nrow(data))
    }  else {
      weights = data[weights_name]  
    }
    list(y=data[y_names], weights = weights)
  }
}

# for (model in models){
# function should do single model at time so easier to concatenate


crossvalidate <- function(get_fold, nfolds, 
  make_model_data, 
  get_y_weights,
  train_model, test_model,
  parallel = FALSE) {
  
  # don't want to specify nfolds!
  
  
  # what about x? how do we bind it together?
  foldid_list <- as.list(seq(nfolds))
  pred_list <- as.list(seq(nfolds))
  y_list <- as.list(seq(nfolds))
  # warning setting list element to NULL deletes it, shortening the list!!! not what we want
  # easiest to always generate list
  weights_list <- as.list(seq(nfolds))
  

  for (i_fold in seq(nfolds)) {
    # functions should work on single data set
    
    # API: dataframe containing x train, y, weights
    data <- get_fold(i_fold)
    cat('got data', mem_used()/1e9)
    # for logistic regression, y  is presented as matrix of counts (if so provided)
    # change liblinear to support counts

    # df with x, y, weights train and test
    # get y
    # problem that diff models expect ys differently vector/matrix/..
    # can we really assume that diff models have same inputs or should we rather do one model at time and diff folds.
    #then each model does its own data extraction.
    #but want to ensure have same data set!
    
    
    # estimate_model - create list of inputs for model, eg sparse matrix x, y, weights
    y_weights_train <- get_y_weights(data$train)
    # y_weights needed for metrics calculations, so should be independent of model type

    x_train <- make_model_data( data$train)
    cat('made model data', mem_used()/1e9)
    mod <- train_model(x_train, y_weights_train$y, y_weights_train$weights)
    # model name?
    cat('trained model',  mem_used()/1e9)
    #test model
    x_test <- make_model_data(data$test)
    
    pred <- test_model(mod, x_test)
    cat('tested model', mem_used()/1e9)
    foldid_list[[i_fold]] <- rep(i_fold, nrow(x_test))
    pred_list[[i_fold]] <- pred
    y_weights_test <- get_y_weights(data$test)
    y_list[[i_fold]] <- y_weights_test$y
    weights_list[[i_fold]] <- y_weights_test$weights

  }
  
  foldid <- mat_concat(foldid_list)
  prediction <- mat_concat(pred_list)
  y <- mat_concat(y_list)
  weights <-  mat_concat(weights_list)
  # y and weights are dataframes and keep their original column names if used in data.frame
  # note that y could be two dimensional
  # and prediction to ( multiple lambdas)
  list(foldid = foldid, y = y, prediction = prediction,  weights = weights)
}


# need ys, and weights
    

cv_errors <- function(errors, weights, name){
# calculate mean and std error of mean over folds
  N = nrow(errors)
  cvm = apply(errors, 2, weighted.mean, w = weights, na.rm = TRUE)
  cvsd = sqrt(apply(scale(errors, cvm, FALSE)^2, 2, weighted.mean, 
                  w = weights, na.rm = TRUE)/(N - 1))
  out = list(cvm = cvm, cvsd = cvsd, name = name)
  
}



