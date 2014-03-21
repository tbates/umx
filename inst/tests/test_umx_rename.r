# library(testthat)
# library(devtools)
# library(umx)
# testthat::test_file("~/bin/umx/inst/tests/test_umx_rename.r") 
# 
# test_package("umx")
test_that("umx_rename works for different values", {
	x = mtcars
	oldNames <- c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")
	newNames = oldNames
	newNames[2] = "cylinder"
	x = umx_rename(x, replace = c(cyl = "cylinder"))
	expect_equal(names(x), newNames)

	# ==============================
	# = test update a missing name =
	# ==============================
	# Warning message:
	# In umx_rename(tmp, replace = c(cyl = "cylinder")) :
	#   The following names did not appear in the dataframe:cyl
	# perhaps you already updated them"

	expect_warning(umx_rename(x, replace = c(cyl = "cylinder")))

	expect_warning(umx_rename(x, replace = c(cyl = "cylinder")))

	# alternate style
	x = mtcars
	x = umx_rename(x, old = c("cyl"), replace = c("cylinder"))
	expect_equal(names(x), newNames)
})
