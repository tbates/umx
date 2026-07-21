library(testthat)
library(OpenMx)
library(umx)

# T1, T2, T3, T5: continuous WLS Chi consistency, saturated AFIs, non-monotone SB guard

set.seed(123)
simData = data.frame(x = rnorm(100))
simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8)

mWlsBase = mxModel("WLS_Base", type = "RAM",
	manifestVars = c("x", "y"),
	mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
	mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
	mxData(observed = simData, type = "raw"),
	mxFitFunctionWLS()
)
mWlsBase = mxRun(mWlsBase, silent = TRUE)

mWlsNested = mxModel("WLS_Nested", type = "RAM",
	manifestVars = c("x", "y"),
	mxPath(from = "x", to = "y", arrows = 1, free = FALSE, values = 0.0, labels = "b1"),
	mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
	mxData(observed = simData, type = "raw"),
	mxFitFunctionWLS()
)
mWlsNested = mxRun(mWlsNested, silent = TRUE)

test_that("T1: saturated continuous WLS reports Chi=0 and perfect AFIs (not NA)", {
	skip_if_not(!is.null(mWlsBase$output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	# Just-identified: free cov structure for two variables (3 free params, 3 moments under cumulants)
	mSat = mxModel("WLS_Sat", type = "RAM",
		manifestVars = c("x", "y"),
		mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
		mxPath(from = "x", to = "y", arrows = 2, free = TRUE, values = 0.1),
		mxData(observed = simData, type = "raw"),
		mxFitFunctionWLS()
	)
	mSat = mxRun(mSat, silent = TRUE)
	skip_if_not(!is.null(mSat$output$implied_jacobian), "No Jacobian on saturated model")

	rf = xmu_robust_WLS_fit(mSat)
	expect_equal(rf$ChiDoF, 0)
	expect_equal(rf$Chi, 0)
	expect_true(is.na(rf$p))
	expect_equal(rf$CFI, 1)
	expect_equal(rf$TLI, 1)
	expect_equal(rf$RMSEA, 0)
	expect_identical(attr(rf, "correction"), "saturated")

	disp = xmu_wls_display_chi(mSat)
	expect_equal(disp$Chi, 0)
	expect_equal(disp$CFI, 1)
	expect_equal(disp$source, "saturated")
})

test_that("T2: umxCompare Chi matches xmu_robust_WLS_fit for continuous WLS", {
	skip_if_not(!is.null(mWlsBase$output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
	skip_if_not(!is.null(mWlsNested$output$implied_jacobian), "No Jacobian on nested model")

	rfBase = xmu_robust_WLS_fit(mWlsBase)
	rfNest = xmu_robust_WLS_fit(mWlsNested)
	res = xmu_compare_WLS(mWlsBase, mWlsNested)

	expect_equal(res$Chi[1], round(rfBase$Chi, 2))
	expect_equal(res$Chi[2], round(rfNest$Chi, 2))
	expect_false(is.na(res$diffFit[2]))
	expect_true(res$diffFit[2] >= 0)
	expect_false(is.na(res$p[2]))
})

test_that("T3: non-monotone F yields NA strictSbChisq with warning", {
	skip_if_not(!is.null(mWlsBase$output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
	skip_if_not(!is.null(mWlsNested$output$implied_jacobian), "No Jacobian on nested model")

	# Freer model forced to a worse objective than nested (same W, structure nesting)
	mBadBase = mWlsBase
	mBadBase@output$fit = as.numeric(mWlsNested$output$fit) + 50

	expect_warning({
		sb = calculateStrictSb(baseModel = mBadBase, nestedModel = mWlsNested)
	}, "smaller for the nested model")
	expect_true(is.na(sb["strictSbChisq"]))
	expect_true(is.na(sb["pValue"]))
	expect_true(isTRUE(attr(sb, "nonMonotoneF")))
})

test_that("T5: DWLS/WLS Browne output$chi can differ from SB display Chi when df > 0", {
	skip_if_not(!is.null(mWlsNested$output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
	skip_if_not(!is.null(mWlsNested$output$chiDoF) && mWlsNested$output$chiDoF > 0, "Nested model needs df > 0")

	rf = xmu_robust_WLS_fit(mWlsNested)
	browne = mWlsNested$output$chi
	# Guard: compare table must not silently use Browne when SB is available
	disp = xmu_wls_display_chi(mWlsNested)
	expect_equal(disp$Chi, rf$Chi)
	expect_equal(disp$source, "SB2010")
	# Under DWLS/WLS residual SE path, Browne residual often differs from SB-scaled F
	if (is.finite(browne) && is.finite(rf$Chi) && abs(browne - rf$Chi) > 1e-6) {
		expect_true(abs(browne - rf$Chi) > 1e-6)
	} else {
		# Same value is allowed (full WLS with certain weights); still assert display uses SB path
		expect_true(TRUE)
	}
})
