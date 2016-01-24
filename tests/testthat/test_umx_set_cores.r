# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_set_cores.r") 
# 
# test_package("umx")
context("get_and_set")

test_that("umx_set_cores works for different values", {
	x = umx_set_cores()
	expect_true(isTRUE(all.equal(x, as.integer(x))))
	expect_error(umx_set_cores("JUNK"))
})
