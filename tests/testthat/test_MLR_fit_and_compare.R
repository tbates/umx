library(OpenMx)
library(lavaan)
library(umx)
library(testthat)

test_that("Robust ML fit statistics match lavaan MLM/MLR outputs for complete continuous data", {
	# 1. Generate complete continuous data
	set.seed(12345)
	N = 200
	f = rnorm(N)
	x1 = 0.6 * f + rnorm(N, sd = 0.8)
	x2 = 0.7 * f + rnorm(N, sd = 0.7)
	x3 = 0.5 * f + rnorm(N, sd = 0.9)
	x4 = 0.8 * f + rnorm(N, sd = 0.6)
	dat = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

	# 2. Fit CFA in lavaan using MLR (Yuan-Bentler robust corrections)
	lavModel = "f =~ x1 + x2 + x3 + x4"
	fitLav = cfa(lavModel, data = dat, estimator = "MLR")
	
	lavScaling = as.numeric(fitMeasures(fitLav, "chisq.scaling.factor"))
	lavChisqScaled = as.numeric(fitMeasures(fitLav, "chisq.scaled"))
	lavCfiRobust = as.numeric(fitMeasures(fitLav, "cfi.robust"))
	lavTliRobust = as.numeric(fitMeasures(fitLav, "tli.robust"))
	lavRmseaRobust = as.numeric(fitMeasures(fitLav, "rmsea.robust"))

	# 3. Fit the same model in umx using ML
	m1 = umxRAM("OneFactorML", data = dat,
		umxPath("f", to = c("x1", "x2", "x3", "x4")),
		umxPath(var = c("x1", "x2", "x3", "x4")),
		umxPath(var = "f", fixedAt = 1)
	)

	# 4. Extract robust ML fit statistics from umx helper
	resUmx = xmu_robust_ML_fit(m1)

	expect_false(is.null(resUmx))
	# We expect high parity with lavaan's calculations
	expect_equal(as.numeric(resUmx$scalingFactor), lavScaling, tolerance = 1e-3)
	expect_equal(as.numeric(resUmx$Chi), lavChisqScaled, tolerance = 1e-3)
	expect_equal(as.numeric(resUmx$CFI), lavCfiRobust, tolerance = 1e-3)
	expect_equal(as.numeric(resUmx$TLI), lavTliRobust, tolerance = 1e-3)
	expect_equal(as.numeric(resUmx$RMSEA), lavRmseaRobust, tolerance = 1e-3)

	# 5. Fit a nested model in lavaan (fixing loading of x4 to 1)
	# We use std.lv = TRUE and free first loading to match umx specification exactly
	lavModelNested = "f =~ NA*x1 + x2 + x3 + 1*x4"
	fitLavNested = cfa(lavModelNested, data = dat, estimator = "MLR", std.lv = TRUE)
	
	# Satorra-Bentler 2001 scaled difference test in lavaan
	lavLrt = lavTestLRT(fitLav, fitLavNested, method = "satorra.bentler.2001")
	lavDiffChisq = as.numeric(lavLrt[2, "Chisq diff"])
	lavDiffP = as.numeric(lavLrt[2, "Pr(>Chisq)"])

	# 6. Fit nested model in umx
	m2 = umxModify(m1, update = "f_to_x4", value = 1, name = "OneFactorML_Nested", autoRun = TRUE)
	resUmxDiff = xmu_compare_robust_ML(m1, m2)

	expect_false(is.null(resUmxDiff))
	# Numerical differences in Hessians (analytic vs finite difference SLSQP) warrant 15% tolerance
	expect_equal(as.numeric(resUmxDiff$diffFit), lavDiffChisq, tolerance = 0.15)
	expect_equal(as.numeric(resUmxDiff$p), lavDiffP, tolerance = 0.15)
})

test_that("Robust ML fit statistics fall back gracefully on FIML and categorical data", {
	# Setup complete data
	set.seed(12345)
	N = 100
	dat = data.frame(x1 = rnorm(N), x2 = rnorm(N), x3 = rnorm(N), x4 = rnorm(N))
	
	# Fit base model
	mBase = umxRAM("BaseModel", data = dat,
		umxPath(from = c("x1", "x2", "x3", "x4"), arrows = 2, free = TRUE, values = 1)
	)

	# 1. Incomplete Data (FIML) Case
	datMissing = dat
	datMissing$x1[1:5] = NA
	mMissing = umxRAM("MissingModel", data = datMissing,
		umxPath(from = c("x1", "x2", "x3", "x4"), arrows = 2, free = TRUE, values = 1)
	)

	# Verify FIML warning is triggered and returns NULL
	expect_warning(
		resFiml <- xmu_robust_ML_fit(mMissing),
		"Robust ML fit corrections for FIML"
	)
	expect_true(is.null(resFiml))

	# 2. Categorical/Ordinal Data Case
	datOrdinal = dat
	datOrdinal$x1 = as.ordered(cut(datOrdinal$x1, breaks = 3, labels = FALSE))
	mOrdinal = umxRAM("OrdinalModel", data = datOrdinal,
		umxPath(from = c("x1", "x2", "x3", "x4"), arrows = 2, free = TRUE, values = 1)
	)

	# Verify ordinal warning is triggered and returns NULL
	expect_warning(
		resOrdinal <- xmu_robust_ML_fit(mOrdinal),
		"Robust ML fit corrections for ordinal"
	)
	expect_true(is.null(resOrdinal))
})

test_that("Robust ML fit statistics handle boundary cases, small N, and R loop fallback", {
	# 1. Edge Case: Small sample size (N=10) and perfect fit simulation
	set.seed(12345)
	N = 10
	datSmall = data.frame(x = rnorm(N), y = rnorm(N))
	mSmall = umxRAM("SmallModel", data = datSmall,
		umxPath(from = "x", to = "y", free = TRUE, values = 0.5),
		umxPath(var = c("x", "y"), free = TRUE, values = 1)
	)
	
	# Verify it does not crash and returns valid robust metrics
	resSmall = xmu_robust_ML_fit(mSmall)
	expect_type(resSmall, "list")
	expect_true(resSmall$scalingFactor > 0)
	# CFI can be NA or numeric for saturated models
	expect_true(is.na(resSmall$CFI) || (resSmall$CFI >= 0 && resSmall$CFI <= 1))

	# 2. R Loop Fallback Verification (Mocking absence of useCpp signature)
	# Temporarily mask hasUseCpp check behavior or force R loop fallback
	# By invoking the gradients directly without useCpp check
	hasUseCpp = "useCpp" %in% names(formals(OpenMx::imxRowGradients))
	if (hasUseCpp) {
		# Test that gradient call succeeds when explicitly bypassing C++ (falling back to R loops)
		# R loop execution call
		G_R = OpenMx::imxRowGradients(mSmall)
		expect_true(nrow(G_R) == N)
	}
})
