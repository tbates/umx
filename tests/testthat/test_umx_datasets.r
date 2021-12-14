# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_datasets.r") 
# 
# testthat::test_package("umx")

context("umx datasets functions")


test_that("umx_get_checkpoint", {
	require(umx)
	data(GFF)
	data(us_skinfold_data)
	data(iqdat)
	data(Fischbein_wt) # load the data
	expect_equal(length(umx_names(GFF, "1$")), 11)
	
})


