# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_set_optimizer.r") 
# 
# test_package("umx")
context("get_and_set")

test_that("umx_set_optimizer works for different values", {
	expect_match(umx_set_optimizer(), "CSOLNP|SLSQP|NPSOL")
	expect_error(umx_set_optimizer("JUNK"))
})
