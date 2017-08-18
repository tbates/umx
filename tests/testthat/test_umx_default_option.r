# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_default_option.r") 
# 
# test_package("umx")

test_that("testing umx_default_option", {
	oList = c("a", "b", "c")
	expect_match(umx_default_option(oList, oList), "a")	
	expect_match(umx_default_option("b", oList), "b")
	expect_error(umx_default_option("bad", oList))
	expect_match(umx_default_option("allow me", oList, check = FALSE), "allow me")
	
	# fails with NULL!!!!!
	# oList = c(NULL, "b", "c")
	# umx_default_option(oList, oList)
	
	oList = c(NA, "b", "c")
	# use NA instead
	oList = c(TRUE, FALSE, NA)
	expect_true(umx_default_option(TRUE, oList), TRUE)
})
