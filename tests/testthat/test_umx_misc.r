# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_scale.r") 
# 
# testthat::test_package("umx")

context("umx_ data helper functions")

test_that("umx_scale works for different inputs", {
	# ==============================
	# = no error on expected input =
	# ==============================
	expect_error(umx_scale(mtcars), regex = NA) # NA = no error expected
		
	# ==================
	# = error expected =
	# ==================
	expect_error(umx_scale(mxData(mtcars, type = "raw")))

	# can ignore factors...
	mtcars$am = umxFactor(mtcars$am)
	expect_error(umx_scale(mtcars), regex = NA)
	
	err = "umx_scale takes a dataframe \\(or numeric vector\\) as its first argument\\.df isn't a dataframe, it's aMxDataStatic"  	
	expect_error(umx_scale(mxData(mtcars[, c("mpg" , "cyl" , "disp")], type = "raw")), regex = err)
})

test_that("umx_get_checkpoint", {
	require(umx)
	umx_get_checkpoint()
	data(demoOneFactor)
	manifests = names(demoOneFactor)
   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var     = manifests),
		umxPath(var     = "G", fixedAt = 1)
	)
	umx_get_checkpoint(model = m1)
})

test_that("umx_is_exogenous", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var     = manifests),
		umxPath(var     = "G", fixedAt = 1)
	)
	umx_is_exogenous(m1, manifests_only = TRUE)
	umx_is_exogenous(m1, manifests_only = FALSE)
	umx_is_endogenous(m1, manifests_only = TRUE)
	umx_is_endogenous(m1, manifests_only = FALSE)
	
})

test_that("umx_explode_twin_names", {
	require(umx)
	data("twinData")
	umx_explode_twin_names(twinData, sep = "")
	umx_explode_twin_names(twinData, sep = NULL)
	
	# Ignore this: just a single-character/single variable test case
	x = round(10 * rnorm(1000, mean = -.2))
	y = round(5 * rnorm(1000))
	x[x < 0] = 0; y[y < 0] = 0
	umx_explode_twin_names(data.frame(x_T1 = x, x_T2 = y), sep = "_T")
	umx_explode_twin_names(data.frame(x_T11 = x, x_T22 = y), sep = "_T")
	umx_explode_twin_names(c("x_T11", "x_T22"), sep = "_T")
})


