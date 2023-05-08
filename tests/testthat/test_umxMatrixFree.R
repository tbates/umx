# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxMatrixFree.R") 
# test_package("umx")

test_that("umxMatrixFree", {
  require(umx)
  data(twinData) 
  umxMatrixFree('E', type='Symm',  nrow = 3,
                labels =c("eb2",NA,NA,
                          NA,"es2",NA,
                          NA,NA,"au"))
})