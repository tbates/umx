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

test_that("xmu_check_variance collates twin pairs and suppresses duplicate warnings", {
	options(umx_last_variance_warnings = NULL)
	df = data.frame(
		wt1 = c(50, 60, 70), ht1 = c(0.01, 0.02, 0.015),
		wt2 = c(55, 65, 75), ht2 = c(0.012, 0.022, 0.017)
	)
	msgs = capture_messages(xmu_check_variance(df, maxVarRatio = 100))
	expect_true(any(grepl("Variance of variables differ by more than 100x", msgs)))
	expect_true(any(grepl("'wt1' var > 100 times that of 'ht1'", msgs)))
	expect_true(any(grepl("'wt2' var > 100 times that of 'ht2'", msgs)))

	# Duplicate call on same variance structure produces no new messages
	msgs2 = capture_messages(xmu_check_variance(df, maxVarRatio = 100))
	expect_equal(length(msgs2), 0)
})

test_that("xmu_mxRun and xmu_mxRefModels execute safely", {
	manifests = names(demoOneFactor)
	latents = "g"
	model = mxModel("OneFactorTest", type = "RAM",
		manifestVars = manifests, latentVars = latents,
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2),
		mxPath(from = latents, arrows = 2, free = FALSE, values = 1),
		mxData(cov(demoOneFactor), type = "cov", numObs = 500)
	)
	fit = xmu_mxRun(model, beginMessage = FALSE)
	expect_true(umx_has_been_run(fit))

	refs = xmu_mxRefModels(fit, run = TRUE, beginMessage = FALSE)
	expect_true(inherits(refs, "list"))
	expect_true(any(grepl("Saturated", names(refs))))
})


