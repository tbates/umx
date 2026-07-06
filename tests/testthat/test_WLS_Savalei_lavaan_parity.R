library(OpenMx)
library(lavaan)
library(umx)
library(testthat)

lavModelBase = "f =~ x1 + x2 + x3 + x4"
lavModelNest = "f =~ a*x1 + a*x2 + x3 + x4"

makeSavaleiDat <- function(n = 500, seed = 12345) {
	set.seed(seed)
	f  = rnorm(n)
	x1 = 0.75 * f + rnorm(n, sd = 0.66)
	x2 = 0.80 * f + rnorm(n, sd = 0.60)
	x3 = 0.70 * f + rnorm(n, sd = 0.71)
	x4 = 0.85 * f + rnorm(n, sd = 0.53)
	dat = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
	dat[] = lapply(dat, function(x) cut(x, breaks = 4, labels = FALSE))
	dat[] = lapply(dat, ordered)
	dat
}

fitUmxSavalei <- function(modelSpec, dat, name = "umx_wls") {
	if (is.character(modelSpec) && length(modelSpec) == 1L && grepl("=~", modelSpec)) {
		m = suppressWarnings(umxRAM(modelSpec, data = dat, name = name, type = "WLS"))
	} else {
		m = suppressWarnings(do.call(umxRAM, c(list(model = name, data = dat, type = "WLS"), modelSpec)))
	}
	mxRun(m, silent = TRUE)
}

assertSavaleiParity <- function(resUmx, lavFit, label = "model") {
	expect_false(is.null(resUmx))
	expect_equal(attr(resUmx, "correction"), "Savalei2021", info = label)
	lavCfiRobust = as.numeric(fitMeasures(lavFit, "cfi.robust"))
	lavTliRobust = as.numeric(fitMeasures(lavFit, "tli.robust"))
	lavRmseaRobust = as.numeric(fitMeasures(lavFit, "rmsea.robust"))
	expect_equal(as.numeric(resUmx$CFI), lavCfiRobust, tolerance = 0.01, info = label)
	expect_equal(as.numeric(resUmx$TLI), lavTliRobust, tolerance = 0.02, info = label)
	expect_equal(as.numeric(resUmx$RMSEA), lavRmseaRobust, tolerance = 0.03, info = label)
	lavCatMl = lavaan:::lav_fit_catml_dwls(lavFit)
	expect_equal(as.numeric(attr(resUmx, "c_model")), as.numeric(lavCatMl$c.hat3), tolerance = 1.1, info = paste(label, "c.hat3"))
}

test_that("Ordinal WLS Savalei indices match lavaan when umx uses lavaan model string", {
	dat = makeSavaleiDat()
	lav1 = cfa(lavModelBase, data = dat, ordered = names(dat), estimator = "WLSMV")
	m1 = fitUmxSavalei(lavModelBase, dat, name = "WLS_ordinal_savalei_lav")
	skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	expect_equal(m1@output$chiDoF, lav1@test[[1]]$df)
	assertSavaleiParity(xmu_robust_WLS_fit(m1), lav1, "lavaan string")
})

test_that("Ordinal WLS Savalei indices match lavaan when umx fixes first loading (lavaan auto.fix.first)", {
	dat = makeSavaleiDat()
	lav1 = cfa(lavModelBase, data = dat, ordered = names(dat), estimator = "WLSMV")
	umxSpec = list(
		umxPath("f", to = c("x1", "x2", "x3", "x4"), firstAt = 1),
		umxPath(v.m. = "f"),
		umxPath(v.m. = c("x1", "x2", "x3", "x4"))
	)
	m1 = fitUmxSavalei(umxSpec, dat, name = "WLS_ordinal_savalei_firstAt")
	skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	expect_equal(m1@output$chiDoF, lav1@test[[1]]$df)
	expect_false(m1$A$free[1, "f"])
	expect_true(m1$S$free["f", "f"])
	assertSavaleiParity(xmu_robust_WLS_fit(m1), lav1, "firstAt")
})

test_that("Continuous WLS regression: Savalei branch not invoked", {
	set.seed(123)
	simData = data.frame(x = rnorm(100))
	simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8)

	mBase = mxModel("WLS_Continuous", type = "RAM",
		manifestVars = c("x", "y"),
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
		mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
		mxData(observed = simData, type = "raw"),
		mxFitFunctionWLS()
	)
	mBase = mxRun(mBase, silent = TRUE)
	skip_if_not(!is.null(mBase@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	resUmx = xmu_robust_WLS_fit(mBase)
	expect_equal(attr(resUmx, "correction"), "SB2010")
})

test_that("Ordinal WLS Savalei robust indices respond to nested misfit (OpenMx-native behavior)", {
	dat = makeSavaleiDat()
	mBase = fitUmxSavalei(lavModelBase, dat, name = "WLS_ord_misfit_base")
	mNest = fitUmxSavalei(lavModelNest, dat, name = "WLS_ord_misfit_nest")
	skip_if_not(!is.null(mBase@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	resBase = xmu_robust_WLS_fit(mBase)
	resNest = xmu_robust_WLS_fit(mNest)

	expect_equal(attr(resBase, "correction"), "Savalei2021")
	expect_equal(attr(resNest, "correction"), "Savalei2021")
	expect_true(as.numeric(resNest$CFI) < as.numeric(resBase$CFI))
	expect_true(as.numeric(resNest$RMSEA) > as.numeric(resBase$RMSEA))
	expect_true(is.finite(attr(resBase, "c_model")) && attr(resBase, "c_model") > 0)
	expect_true(is.finite(attr(resNest, "c_model")) && attr(resNest, "c_model") > 0)
})

test_that("Nested ordinal WLS CFI ranking agrees with lavaan WLSMV (lavaan string umx models)", {
	dat = makeSavaleiDat()
	lavBase = cfa(lavModelBase, data = dat, ordered = names(dat), estimator = "WLSMV")
	lavNest = cfa(lavModelNest, data = dat, ordered = names(dat), estimator = "WLSMV")
	lavDeltaCfi = as.numeric(fitMeasures(lavNest, "cfi.robust")) - as.numeric(fitMeasures(lavBase, "cfi.robust"))

	mBase = fitUmxSavalei(lavModelBase, dat, name = "WLS_ord_base")
	mNest = fitUmxSavalei(lavModelNest, dat, name = "WLS_ord_nest")
	skip_if_not(!is.null(mBase@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	expect_equal(mBase@output$chiDoF, lavBase@test[[1]]$df)
	expect_equal(mNest@output$chiDoF, lavNest@test[[1]]$df)

	resBase = xmu_robust_WLS_fit(mBase)
	resNest = xmu_robust_WLS_fit(mNest)
	umxDeltaCfi = as.numeric(resNest$CFI) - as.numeric(resBase$CFI)

	expect_equal(sign(umxDeltaCfi), sign(lavDeltaCfi))
})

test_that("umxSummary reports Savalei note on ordinal WLS", {
	dat = makeSavaleiDat()

	m1 = fitUmxSavalei(lavModelBase, dat, name = "WLS_ord_note")
	skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

	out = capture.output(suppressWarnings(umxSummary(m1)))

	expect_true(any(grepl("Savalei \\(2021\\)", out)))
	expect_true(any(grepl("c_model", out)))
})