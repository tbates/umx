library(testthat)
library(umx)
library(OpenMx)

context("umxLISREL tests")

test_that("umxLISREL works with covariance data and automatic partitioning", {
	vNames <- paste("v", as.character(1:6), sep="")
	dimList <- list(vNames, vNames)
	covData <- matrix(
	  c(0.9223099, 0.1862938, 0.4374359, 0.8959973, 0.9928430, 0.5320662,
		0.1862938, 0.2889364, 0.3927790, 0.3321639, 0.3371594, 0.4476898,
		0.4374359, 0.3927790, 1.0069552, 0.6918755, 0.7482155, 0.9013952,
		0.8959973, 0.3321639, 0.6918755, 1.8059956, 1.6142005, 0.8040448,
		0.9928430, 0.3371594, 0.7482155, 1.6142005, 1.9223567, 0.8777786,
		0.5320662, 0.4476898, 0.9013952, 0.8040448, 0.8777786, 1.3997558
		), nrow=6, ncol=6, byrow=TRUE, dimnames=dimList)
	
	tmpData <- mxData(observed=covData, type="cov", numObs=100)
	
	m1 = umxLISREL("two_factor_cov", data = tmpData,
	               umxPath(c("G1", "G2"), to = vNames),
	               umxPath(var = vNames),
	               umxPath(var = c("G1", "G2"), fixedAt = 1),
	               umxPath("G1", with = "G2"))
	
	expect_true(umx_is_LISREL(m1))
	expect_false(umx_is_RAM(m1))
	
	# Verify expectation slots
	expect_equal(m1$expectation@LX, "LX")
	expect_equal(m1$expectation@TD, "TD")
	expect_equal(m1$expectation@PH, "PH")
	
	# Check that it runs successfully
	expect_error(umxSummary(m1), NA)
	
	# Check standardization
	m1_std = umx_standardize(m1)
	expect_true(umx_is_LISREL(m1_std))
	
	# Check plotting
	expect_error(plot(m1, file = NA), NA)
})

test_that("umxLISREL works with raw data and automatically adds means", {
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	
	m2 = umxLISREL("one_factor_raw", data = demoOneFactor,
	               umxPath("G", to = manifests),
	               umxPath(var = manifests),
	               umxPath(var = "G", fixedAt = 1))
	
	expect_true(umx_is_LISREL(m2))
	# Verify that TX/TY are created
	expect_equal(m2$expectation@TX, "TX")
	expect_equal(m2$expectation@KA, "KA")
	
	# Check summary runs
	expect_error(umxSummary(m2), NA)
})

test_that("umxLISREL works with list override for manifestVars and latentVars", {
	data(demoOneFactor)
	m3 = umxLISREL("forced_exogenous", data = demoOneFactor,
	               manifestVars = list(endogenous = c("x4", "x5"), exogenous = c("x1", "x2")),
	               latentVars = list(endogenous = "G", exogenous = "xi"),
	               umxPath("xi", to = c("x1", "x2")),
	               umxPath("G", to = c("x4", "x5")),
	               umxPath("xi", to = "G"),
	               umxPath(var = c("x1", "x2", "x4", "x5")),
	               umxPath(var = "xi", fixedAt = 1),
	               umxPath(var = "G"),
	               umxPath(means = c("x1", "x2", "x4", "x5")))
	
	expect_true(umx_is_LISREL(m3))
	expect_equal(m3$expectation@LY, "LY")
	expect_equal(m3$expectation@LX, "LX")
	expect_equal(m3$expectation@BE, "BE")
	expect_equal(m3$expectation@GA, "GA")
	expect_error(umxSummary(m3), NA)
})


test_that("umxLISREL works with character vector manifestVars", {
	data(demoOneFactor)
	m4 = umxLISREL("subset_manifests", data = demoOneFactor,
	               manifestVars = c("x1", "x2", "x3"),
	               umxPath("G", to = c("x1", "x2", "x3")),
	               umxPath(var = c("x1", "x2", "x3")),
	               umxPath(var = "G", fixedAt = 1))
	
	expect_true(umx_is_LISREL(m4))
	expect_equal(rownames(m4$LX$values), c("x1", "x2", "x3"))
	expect_error(umxSummary(m4), NA)
})

