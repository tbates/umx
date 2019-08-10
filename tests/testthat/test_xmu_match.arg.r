# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_xmu_match.arg.r") 
# 
# test_package("umx")

test_that("testing xmu_match.arg", {
	oList = c("a", "b", "c")
	expect_match(xmu_match.arg(oList, oList), "a")	
	expect_match(xmu_match.arg("b", oList), "b")
	expect_error(xmu_match.arg("bad", oList))
	expect_match(xmu_match.arg("allow me", oList, check = FALSE), "allow me")
	
	# fails with NULL!!!!!
	# oList = c(NULL, "b", "c")
	# xmu_match.arg(oList, oList)
	
	oList = c(NA, "b", "c")
	# use NA instead
	oList = c(TRUE, FALSE, NA)
	expect_true(xmu_match.arg(TRUE, oList), TRUE)
})
