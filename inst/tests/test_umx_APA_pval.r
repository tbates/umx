# test_file("~/bin/umx/inst/tests/test_umx_is_cov.r") 
# library(testthat)
# library(umx)
# 
# test_package("umx")
test_that("umx_APA_pval works for different values", {
	expect_match(umx_APA_pval(p = .04, min = .05), "< 0.05")
	expect_match(umx_APA_pval(1.23E-3)           , "= 0.001")
	expect_match(umx_APA_pval(c(1.23E-3, .5))    ,  c("= 0.001", "= 0.5") )
	expect_equal(umx_APA_pval(c(1.23E-3, .5), addComparison = F), c(0.001, 0.500) )
})
