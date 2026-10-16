# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxLGCM.r")

test_that("umxLGCM builds an unrun MxModelLGCM", {
	set.seed(42)
	nv = 4
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(50))
	dz = data.frame(row.names = seq_len(50))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(50)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = rnorm(50)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	m0 = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE)
	expect_true(is(m0, "MxModelLGCM"))
	expect_false(umx_has_been_run(m0))
	expect_equal(sort(names(m0@submodels)), c("DZ", "MZ", "top"))
	expect_equal(length(m0$MZ$expectation$dims), 2 * nv)
	expect_true(all(c("A_1_1", "C_1_1", "E_1_1", "mu_Int", "mu_Shape", "es_y") %in% names(omxGetParameters(m0))))
	expect_false("T_1_1" %in% names(omxGetParameters(m0)))
})

test_that("umxLGCM recovers simulated twin growth parameters, with table summary", {
	set.seed(42)
	nv = 4
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(300))
	dz = data.frame(row.names = seq_len(300))
	for (s in 1:2) {
		base_mz = rnorm(300, 0, .3)
		base_dz = rnorm(300, 0, .3)
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = base_mz + (w - 1) * .5 + rnorm(300, 0, .05)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = base_dz + (w - 1) * .5 + rnorm(300, 0, .05)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	m0 = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE)
	m0 = omxSetParameters(m0, labels = c("A_1_1", "A_2_2", "C_1_1", "C_2_2", "E_1_1", "E_2_2", "es_y", "mu_Int", "mu_Shape"), values = c(.5, .2, .2, .05, .3, .1, .4, 1, -.5))
	invisible(capture.output(sim <- mxGenerateData(m0)))
	m1 = umxLGCM(var = var, age = age, mzData = sim$MZ, dzData = sim$DZ, tryHard = "no")
	expect_equal(m1$output$status$code, 0)
	est = omxGetParameters(m1)
	expect_equal(unname(est[c("A_1_1", "C_1_1", "E_1_1", "es_y", "mu_Int", "mu_Shape")]), c(.5, .2, .3, .4, 1, -.5), tolerance = .25)
	tab = umxSummary(m1)
	expect_equal(nrow(tab$fit), 1)
	expect_true(all(is.finite(c(tab$fit$minus2LL, tab$fit$df, tab$fit$AIC))))
	expect_equal(colnames(tab$parameters), c("name", "Estimate", "Std.Error"))
	estTab = tab$parameters
	expect_equal(unname(estTab[estTab$name == "mu_Int", "Estimate"]), unname(est[["mu_Int"]]))
	expect_true(all(is.finite(estTab$Std.Error)))
})

test_that("umxLGCM models trios, estimating T only with siblings", {
	set.seed(42)
	nv = 4
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz3 = data.frame(row.names = seq_len(50))
	dz3 = data.frame(row.names = seq_len(50))
	mz2 = data.frame(row.names = seq_len(50))
	dz2 = data.frame(row.names = seq_len(50))
	for (s in 1:3) {
		for (w in 1:nv) {
			mz3[[paste0(age[w], "_T", s)]] = rnorm(50)
			mz3[[paste0(var[w], "_T", s)]] = 0
			dz3[[paste0(age[w], "_T", s)]] = rnorm(50)
			dz3[[paste0(var[w], "_T", s)]] = 0
		}
	}
	for (s in 1:2) {
		for (w in 1:nv) {
			mz2[[paste0(age[w], "_T", s)]] = rnorm(50)
			mz2[[paste0(var[w], "_T", s)]] = 0
			dz2[[paste0(age[w], "_T", s)]] = rnorm(50)
			dz2[[paste0(var[w], "_T", s)]] = 0
		}
	}
	expect_message(m3 <- umxLGCM(var = var, age = age, mzData = mz3, dzData = dz3, tVar = TRUE, autoRun = FALSE), "trios")
	expect_equal(length(m3$MZ$expectation$dims), 3 * nv)
	expect_true("T_1_1" %in% names(omxGetParameters(m3)))
	expect_message(m2 <- umxLGCM(var = var, age = age, mzData = mz2, dzData = dz2, tVar = TRUE, autoRun = FALSE), "NOT be estimated")
	expect_false("T_1_1" %in% names(omxGetParameters(m2)))
	m4 = umxLGCM(var = var, age = age, mzData = mz2, dzData = dz2, sibData = mz2, tVar = TRUE, autoRun = FALSE)
	expect_equal(sort(names(m4@submodels)), c("DZ", "MZ", "SIB", "top"))
	expect_true("T_1_1" %in% names(omxGetParameters(m4)))
})

test_that("umxLGCM builds all links, quadratic, chol, and invariant forms", {
	set.seed(42)
	nv = 4
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(50))
	dz = data.frame(row.names = seq_len(50))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(50)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = rnorm(50)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	mz$prs_T1 = rnorm(50)
	mz$prs_T2 = rnorm(50)
	dz$prs_T1 = rnorm(50)
	dz$prs_T2 = rnorm(50)
	for (lnk in c("linear", "exponential", "logistic", "gompertz")) {
		m = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, link = lnk, autoRun = FALSE)
		expect_equal(length(m$MZ$expectation$dims), 2 * nv)
	}
	mq = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, quadratic = TRUE, autoRun = FALSE)
	expect_true("mu_Quad" %in% names(omxGetParameters(mq)))
	mc = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, chol = TRUE, autoRun = FALSE)
	expect_true("a_1_1" %in% names(omxGetParameters(mc)))
	mi = umxLGCM(var = var, age = age, invariant = "prs", mzData = mz, dzData = dz, autoRun = FALSE)
	expect_true("b_Int_on_Z1" %in% names(omxGetParameters(mi)))
})

test_that("umxLGCM splits data by zyg column", {
	set.seed(42)
	nv = 4
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(50))
	dz = data.frame(row.names = seq_len(50))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(50)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = rnorm(50)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	stacked = rbind(cbind(zygosity = "MZ", mz), cbind(zygosity = "DZ", dz))
	expect_message(m <- umxLGCM(var = var, age = age, data = stacked, autoRun = FALSE), "groups from")
	expect_equal(c(nrow(m$MZ$data$observed), nrow(m$DZ$data$observed)), c(50, 50))
})

test_that("umxPlotLGCM draws implied curve and observed means for every link", {
	set.seed(42)
	nv = 2
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(60))
	dz = data.frame(row.names = seq_len(60))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(60)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = rnorm(60)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	for (lnk in c("linear", "exponential", "logistic", "gompertz")) {
		m = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, link = lnk, autoRun = FALSE)
		mf = suppressMessages(mxRun(m, useOptimizer = FALSE, silent = TRUE))
		p = umxPlotLGCM(mf)
		b = ggplot2::ggplot_build(p)
		expect_equal(nrow(b$data[[1]]), 100)
		est = omxGetParameters(mf)
		r = mf$top$rate$values[1, 1]
		md = mf$top$mid$values[1, 1]
		f = switch(lnk, linear = b$data[[1]]$x, exponential = 1 - exp(-r * b$data[[1]]$x),
			logistic = 1 / (1 + exp(-r * (b$data[[1]]$x - md))), gompertz = exp(-exp(-r * (b$data[[1]]$x - md))))
		expect_equal(b$data[[1]]$y, unname(est[["mu_Int"]] + est[["mu_Shape"]] * f))
		expect_equal(nrow(b$data[[2]]), 4)
		expect_true(all(b$data[[2]]$x < 90000))
	}
	expect_equal(p$labels$x, "Age")
	expect_equal(p$labels$y, "y1")
})

test_that("umxPlotLGCM respects age range, placeholders, and the fitted gate", {
	set.seed(42)
	nv = 2
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(60))
	dz = data.frame(row.names = seq_len(60))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(60)
			mz[[paste0(var[w], "_T", s)]] = 0
			dz[[paste0(age[w], "_T", s)]] = rnorm(60)
			dz[[paste0(var[w], "_T", s)]] = 0
		}
	}
	m = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE)
	mf = suppressMessages(mxRun(m, useOptimizer = FALSE, silent = TRUE))
	p = umxPlotLGCM(mf, ageMin = -1, ageMax = 1, n = 11, observed = FALSE)
	b = ggplot2::ggplot_build(p)
	expect_equal(nrow(b$data[[1]]), 11)
	expect_equal(range(b$data[[1]]$x), c(-1, 1))
	expect_equal(length(b$data), 1)
	expect_error(umxPlotLGCM(m), "run")
	mz$age1_T1[1:3] = NA
	mz$age2_T2[1:2] = NA
	mn = suppressMessages(umxLGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE))
	mnf = suppressMessages(mxRun(mn, useOptimizer = FALSE, silent = TRUE))
	bn = ggplot2::ggplot_build(umxPlotLGCM(mnf))
	expect_true(max(bn$data[[2]]$x) < 90000)
})

test_that("umxPlotLGCM observed means equal the data wave means", {
	set.seed(42)
	nv = 2
	var = paste0("y", 1:nv)
	age = paste0("age", 1:nv)
	mz = data.frame(row.names = seq_len(30))
	dz = data.frame(row.names = seq_len(30))
	for (s in 1:2) {
		for (w in 1:nv) {
			mz[[paste0(age[w], "_T", s)]] = rnorm(30)
			mz[[paste0(var[w], "_T", s)]] = w
			dz[[paste0(age[w], "_T", s)]] = rnorm(30)
			dz[[paste0(var[w], "_T", s)]] = w
		}
	}
	m = umxLGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE)
	mf = suppressMessages(mxRun(m, useOptimizer = FALSE, silent = TRUE))
	b = ggplot2::ggplot_build(umxPlotLGCM(mf))
	pts = b$data[[2]]
	expect_equal(nrow(pts), 4)
	expect_equal(sort(pts$y), c(1, 1, 2, 2))
})
