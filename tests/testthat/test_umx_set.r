# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_set.r") 
# 
# test_package("umx")
context("get_and_set")

test_that("umx_set_optimizer works for different values", {
	expect_match(umx_set_optimizer(), "CSOLNP|SLSQP|NPSOL")
	expect_error(umx_set_optimizer("JUNK"))
})

test_that("umx_set_cores works for different values", {
	x = umx_set_cores()
	expect_true(isTRUE(all.equal(x, as.integer(x))))
	expect_error(umx_set_cores("JUNK"))
})
