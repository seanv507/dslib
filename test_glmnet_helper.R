source('glmnet_helper.R')

x = matrix(c(1,2,3,4), dimnames=list(NULL, c('a','b')), nrow=2)

y = matrix(c(5,6,7,8), dimnames=list(NULL,c( 'succ','fail')), nrow=2)
nfolds <- 2

y_s <- gen_grouped_sample(y, nfolds)
y_ind <- rep(seq(nrow(y)),nfolds)

print(y)
print(rowsum(y_s, y_ind))

#replicate x data nfolds times
x_s <- x[y_ind,]