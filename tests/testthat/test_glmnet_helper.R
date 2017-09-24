library(testthat)
source('../../glmnet_helper.R')

test_stats_data <- function(){
  a <- matrix(c(1,2,3,4,5,6), nrow=3)
  mn <-apply(a,2,mean)
  std <- apply(a,2,sd)
  a_s <- stats_data(a)
  a_s_1 <- stats_data(a, rep(5, nrow(a)))
  expect_equal(a_s$mean, mn)
  expect_equal(a_s$std, std)
  # test weighting
  expect_equal(a_s$mean, a_s_1$mean)
  # TODO std ?
}