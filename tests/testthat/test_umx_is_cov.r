# library(testthat)
# library(umx)
# testthat::test_file("~/bin/umx/tests/testthat/test_umx_is_cov.r")
# 
# test_package("umx")

test_that("umx_is_cov works for different inputs", {
	require(OpenMx)
	data(myFADataRaw)
	df = cov(mtcars)
	# test cov
	expect_equal(umx_is_cov(df), "cov")
	# test cor
	expect_equal(umx_is_cov(umxCov2cor(df)), "cor")
	# test raw
	expect_equal(umx_is_cov(myFADataRaw), "raw")
	# test boolean F
	expect_equal(umx_is_cov(myFADataRaw, boolean = T), FALSE)
	# test boolean T
	expect_equal(umx_is_cov(df, boolean = T), TRUE)
})