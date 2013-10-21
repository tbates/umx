# test_file("~/bin/umx/inst/tests/test_umx_is_cov.r") 
# library(testthat)
# library(umx)
# 
# test_package("umx")

test_that("umx_is_cov works for different inputs", {
	require(OpenMx)
	cov_data = cov(myFADataRaw[,paste0("x",1:4)], use="pairwise.complete.obs")
	# data.frame(matrix(1))
	expect_equal(umx_is_cov(myFADataRaw), 0)
	expect_equal(umx_is_cov(cov_data), 1)
	expect_equal(umx_is_cov(umxCov2cor(cov_data)), 2)
	# expect_that(umx_is_cov(data, verbose = F), matches("= 0.001") )
	# expect_that(umx_is_cov(data, verbose = F), matches( c("= 0.001", "= 0.5") ) )
	# expect_that(umx_is_cov(data, verbose = F)Comparison = F), equals(c(0.001, 0.500)) )
})
# x1    x2     x3    x4    x5     x6    y1    y2     y3 z1 z2 z3
