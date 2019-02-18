# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_xmu_make_mxData.r") 
# 
# test_package("umx")

test_that("testing xmu_make_mxData", {	
	manVars = c("mpg", "cyl", "disp")
	# 1. handle raw data
	tmp = xmu_make_mxData(data= mtcars, type = "Auto"); # class(tmp);
	expect_true(class(tmp) == "MxDataStatic", "Auto data isn't MxDataStatic")
	# 2. handle selecting columns
	tmp = xmu_make_mxData(data= mtcars, type = "Auto", manifests = manVars)
	expect_true(all(names(tmp$observed) == c("mpg", "cyl", "disp")), "observed != Chosen cols")

	# 3. handle WLS
	tmp = xmu_make_mxData(data= mtcars, type = "WLS" , manifests = manVars, verbose= TRUE)
	expect_true(class(tmp) == "MxDataStatic", "Auto data isn't MxDataStatic for WLS")
	expect_true(all(names(tmp$observed) == c("mpg", "cyl", "disp")), "observed != Chosen cols for WLS")
	
	# Missing data WLS example
	# Does WLS handle missing or not?
	tmp = mtcars; tmp[1, "mpg"] = NA # add NA
	tmp = xmu_make_mxData(data= tmp, type = "WLS", manifests = manVars, verbose= TRUE)

	tmp = xmu_make_mxData(data= mtcars, type = "cov")
	tmp = xmu_make_mxData(data= mtcars, type = "cor")
	# Pass string through
	expect_equal(xmu_make_mxData(data= c("a", "b", "c"), type = "Auto"), c("a","b","c"))
	
})
