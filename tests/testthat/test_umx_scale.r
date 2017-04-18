# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_scale.r") 
# 
# testthat::test_package("umx")

context("umx_ data helper functions")

test_that("umx_scale works for different inputs", {
	# no error on expected input
	mxData(mtcars)
	expect_error(umx_scale(mtcars), regex = NA)

	# can ignore factors...
	mtcars$am = umxFactor(mtcars$am)
	expect_error(umx_scale(mtcars), regex = NA)
	
	err = "umx_scale takes a dataframe \\(or numeric vector\\) as its first argument\\.df isn't a dataframe, it's aMxDataStatic"  	
	expect_error(umx_scale(mxData(mtcars[, c("mpg" , "cyl" , "disp")], type = "raw")), regex = err)
})
