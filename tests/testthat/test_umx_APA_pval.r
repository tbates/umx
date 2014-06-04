# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_APA_pval.r") 
# 
# test_package("umx")
test_that("umx_APA_pval works for different values", {
	expect_match(umx_APA_pval(p = .04, min = .05), "< 0.05")
	expect_match(umx_APA_pval(1.23E-3, addComparison = T), "= 0.001")
	expect_equal(umx_APA_pval(1.23E-3, addComparison = F), 0.001)
	expect_equal(umx_APA_pval(c(1.23E-6, .5))    ,  c("< 0.001", "0.500") )
	expect_equal(umx_APA_pval(c(1.23E-3, .5), addComparison = F), c(0.001, 0.500) )
	expect_equal(umx_APA_pval(c(1.23E-3, .5), addComparison = T), c("= 0.001", "= 0.500") )
})
