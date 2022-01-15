# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_misc.r") 
# 
# testthat::test_package("umx")

context("umx_ data helper functions")

test_that("umx_scale works for different inputs", {
	expect_equal(rad2deg(pi), 180)
	expect_equal(deg2rad(180),pi)
})

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

test_that("umx_time", {
	require(umx)
	umx_time('stop') # alert user stop called when not yet started... 
	expect_named(umx_time('stop')) # elapsed time: 13.24 sec
	expect_named(umx_time('start'))
	data(demoOneFactor)
	latents   = c("G")
	manifests = names(demoOneFactor)
	myData    = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
	m1 = umxRAM("umx_time_example", data = myData,
		umxPath(from = latents, to = manifests),
		umxPath(var  = manifests),
		umxPath(var  = latents, fixedAt = 1)
	)
	umx_time(m1) # report time from mxModel
	m2 = umxRun(m1)
	umx_time(c(m1, m2)) # print comparison table
	umx_time('stop') # report the time since timer last started, and restart
	umx_time('stop') # report the time since timer was restarted.
})

test_that("xmu_standardize_ACEcov", {
	require(umx)
	data(twinData)
	selDVs  = c("bmi")
	selCovs = c("ht") # silly example
	selVars = umx_paste_names(c(selDVs, selCovs), sep = "", suffixes= 1:2)
	mzData = subset(twinData, zyg == 1, selVars)[1:80, ]
	dzData = subset(twinData, zyg == 3, selVars)[1:80, ]
	m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, sep = "", autoRun = TRUE)
	fit = xmu_standardize_ACEcov(m1)

})

test_that("test umx_check_names", {
	require(umx)
	data(demoOneFactor) # "x1" "x2" "x3" "x4" "x5"
	# Find variables that do exist
	expect_true(umx_check_names(namesNeeded = c("x1", "x2"), demoOneFactor))
	# Needed name missing
	expect_false(umx_check_names(namesNeeded = c("z1", "x2"), data = demoOneFactor, die = FALSE))
	# Test when no others are permitted
	expect_false(umx_check_names(namesNeeded = c("x1", "x2"), data = demoOneFactor, die = FALSE, no_others = TRUE))
	# No others doesn't break
	expect_true(umx_check_names(namesNeeded = paste0("x", 1:5), data = demoOneFactor, die = FALSE, no_others = TRUE))
	# Works with no names
	expect_true(umx_check_names(c(), data = demoOneFactor, die = FALSE, no_others = TRUE))
	# Error when var doesn't exist in the data and die = TRUE
	expect_error(umx_check_names(c("bad_var_name", "x2"), data = demoOneFactor, die = TRUE))

	# Error when illegal found and die = TRUE
	expect_error(umx_check_names("x2", illegal = "x3", data = demoOneFactor, die = TRUE))
	expect_false(umx_check_names("x2", illegal = "x3", data = demoOneFactor, die = FALSE))

	# Take a matrix
	expect_true(umx_check_names(namesNeeded = c("x1", "x2"), as.matrix(demoOneFactor)))
	# Take a cov
	expect_true(umx_check_names(namesNeeded = c("x1", "x2"), cov(demoOneFactor[, c("x1","x2")])))
	# Take an mxData
	expect_true(umx_check_names(namesNeeded = c("x1", "x2"), mxData(demoOneFactor, type="raw")))

})

test_that("umx_get_checkpoint", {
	require(umx)
	umx_get_checkpoint()
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	twinData$age1 = twinData$age2 = twinData$age
   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var     = manifests),
		umxPath(var     = "G", fixedAt = 1)
	)
	umx_get_checkpoint(model = m1)
})

test_that("standardize", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var     = manifests),
		umxPath(var     = "G", fixedAt = 1)
	)
	m1 = xmu_standardize_RAM(m1)
	m1 = umx_standardize(m1)
	umxSummary(m1)
})

test_that("umx_is_exogenous umx_is_endogenous", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_null(umx_is_exogenous(m1 , manifests_only = TRUE))
	expect_equal(umx_is_exogenous(m1 , manifests_only = FALSE), "G")
	expect_equal(umx_is_endogenous(m1, manifests_only = TRUE), paste0("x", 1:5) )
	expect_equal(umx_is_endogenous(m1, manifests_only = FALSE), paste0("x", 1:5) )
	expect_true(umx_has_been_run(m1))
})

test_that("umx_has_means", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)   	
	m1 = umxRAM("has_means_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_false(umx_has_means(m1))

	m1 = mxModel(m1, mxData(demoOneFactor[1:100,], type = "raw"),
		mxPath(from = "one", to = manifests)
	)
	expect_true(umx_has_means(m1))
	m1 = mxRun(m1)
	expect_true(umx_has_means(m1))
})

test_that("umx_explode_twin_names", {
	require(umx)
	data("twinData")
	umx_explode_twin_names(twinData, sep = "")
	umx_explode_twin_names(twinData, sep = NULL)
	
	# Single-character/single variable test case
	x = round(10 * rnorm(1000, mean = -.2))
	y = round(5 * rnorm(1000))
	x[x < 0] = 0; y[y < 0] = 0
	umx_explode_twin_names(data.frame(x_T1  = x, x_T2  = y), sep = "_T")
	umx_explode_twin_names(data.frame(x_T11 = x, x_T22 = y), sep = "_T")
	umx_explode_twin_names(c("x_T11", "x_T22"), sep = "_T")
})

test_that("umx_check", {
	require(umx)
	data(twinData) # ?twinData from Australian twins.
	twinData[, c("ht1", "ht2")] = 10 * twinData[, c("ht1", "ht2")]
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	m1  = umxACE(selDVs= "ht", sep= "", dzData= dzData, mzData= mzData, autoRun= FALSE)
	tmp = umxRenameMatrix(m1$top, matrixName = "a", name="hello")
	expect_true(umx_check(tmp$hello$labels == "hello_r1c1") ) # new is there
	expect_true(umx_check(is.null(tmp$a)) ) # old is gone

})

test_that("xmu_cell_is_on", {
	a_cp = umxMatrix("a_cp", "Lower", 3, 3, free = TRUE, values = 1:6)
	expect_false(xmu_cell_is_on(r = 3, c = 3, "left", mat = a_cp))
	expect_error(xmu_cell_is_on(r=4,c = 3, "any", mat = a_cp))
})


