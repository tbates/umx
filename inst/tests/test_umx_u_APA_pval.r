test_that("umx_u_APA_pval works for different values", {
	expect_that(umx_u_APA_pval(p = .04, min = .05), matches("< 0.05") )
	expect_that(umx_u_APA_pval(1.23E-3)           , matches("= 0.001") )
	expect_that(umx_u_APA_pval(c(1.23E-3, .5))    , matches( c("= 0.001", "= 0.5") ) )
	expect_that(umx_u_APA_pval(c(1.23E-3, .5), addComparison = F), equals(c(0.001, 0.500)) )
})
