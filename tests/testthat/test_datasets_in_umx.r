# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_datasets.r") 
# 
# testthat::test_package("umx")

test_that("umx_get_checkpoint", {
	require(umx)
	data(GFF)
	data(us_skinfold_data)
	data(iqdat)
	data(Fischbein_wt) # load the data
	expect_equal(length(umx_names(GFF, "1$")), 11)
	
})

test_that("umx_data lists datasets and vignettes neatly", {
	outputLines = capture.output(umx_data())
	expect_true(any(grepl("Data sets in package 'umx':", outputLines)))
	expect_true(any(grepl("HSwls:", outputLines)))
	expect_true(any(grepl("Vignettes in `umx` \\(access as vignette\\(\"name\", package=\"umx\"\\)\\)", outputLines)))
})


