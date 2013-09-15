# test_that("umx_u_APA_pval works for different values", {
# 	expect_that(umx_u_APA_pval(1.23E-3), "= 0.001")
# 	expect_that(umx_u_APA_pval(c(1.23E-3, .5)), c("= 0.001", "= 0.5"))
# 	expect_that(umx_u_APA_pval(c(1.23E-3, .5), addComparison = F), c(0.001, 0.500))
# 	expect_that(, "= 0.001")
# 	expect_that(, "= 0.001")
# })