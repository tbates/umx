# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxPath.r") 
# test_package("umx")
test_that("fin_* works", {	
	require(umx)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 190, numYears = 7), 0.096)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 50, numYears = 5), -0.129)
	expect_equal(fin_CAGR(beginningValue = 100, endingValue = 50, numYears = 5, digits = 2), -0.13)
	
	# Error: Inputs must be positive
	expect_error(fin_CAGR(beginningValue =  0   , endingValue = 50, numYears =  5), "Inputs must be positive values")
	expect_error(fin_CAGR(beginningValue = 25   , endingValue = 50, numYears = -1), "Inputs must be positive values")
	expect_error(fin_CAGR(beginningValue = "100", endingValue = 50, numYears = -1), "All inputs must be numeric")

})
