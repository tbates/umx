library(umx)
library(testthat)
context("umxACE_GLM")

test_that("umxACE_GLM builds univariate RAM ACE with S=0 and six latents", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(11)
	n = 20
	mzData = data.frame(y_T1 = rpois(n, 2), y_T2 = rpois(n, 2))
	dzData = data.frame(y_T1 = rpois(n, 2), y_T2 = rpois(n, 2))
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", autoRun = FALSE)
	expect_true(is(m, "MxModelACE_GLM"))
	expect_true(is(m$MZ$fitfunction, "MxFitFunctionGLM"))
	expect_true(is(m$DZ$fitfunction, "MxFitFunctionGLM"))
	expect_equal(as.integer(m$MZ$fitfunction$nAGQ), 1L)
	expect_equal(as.numeric(m$MZ$S$values["y_T1", "y_T1"]), 0)
	expect_equal(as.numeric(m$MZ$S$values["y_T2", "y_T2"]), 0)
	expect_false(isTRUE(m$MZ$S$free["y_T1", "y_T1"]))
	latMZ = m$MZ@latentVars
	latDZ = m$DZ@latentVars
	expect_true(all(c("a1", "c1", "e1_T1", "e1_T2") %in% latMZ))
	expect_false("a1_T1" %in% latMZ)
	expect_true(all(c("a1_T1", "a1_T2", "c1", "e1_T1", "e1_T2") %in% latDZ))
	expect_equal(as.numeric(m$DZ$S$values["a1_T1", "a1_T2"]), 0.5)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "a1"]), as.numeric(m$MZ$A$values["y_T2", "a1"]))
	expect_equal(as.numeric(m$MZ$S$values["e1_T1", "e1_T2"]), 0)
	expect_true(isTRUE(m$MZ$A$free["y_T1", "e1_T1"]))
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "a1"]), sqrt(0.8), tolerance = 1e-8)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "e1_T1"]), sqrt(0.2), tolerance = 1e-8)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "c1"]), 0.2, tolerance = 1e-8)
	expect_equal(as.numeric(m$MZ$A$lbound["y_T1", "a1"]), 1e-4)
	expect_equal(as.numeric(m$MZ$A$lbound["y_T1", "c1"]), 1e-4)
	expect_equal(as.numeric(m$MZ$A$lbound["y_T1", "e1_T1"]), 1e-4)
	dot = plot(m, file = NA, std = FALSE, means = FALSE)
	expect_true(grepl("a -> y", dot, fixed = TRUE))
	expect_true(grepl("c -> y", dot, fixed = TRUE))
	expect_true(grepl("e -> y", dot, fixed = TRUE))
	expect_false(grepl("e1_T1", dot, fixed = TRUE))
})

test_that("umxACE_GLM rejects nAGQ>1 and builds a 3-trait Cholesky", {
	skip_if_not(exists("mxFamily", mode = "function"))
	mzData = data.frame(
		y_T1 = 1:5, y_T2 = 1:5,
		z_T1 = 1:5, z_T2 = 1:5,
		w_T1 = 1:5, w_T2 = 1:5
	)
	dzData = mzData
	expect_error(umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", nAGQ = 5L, autoRun = FALSE), "nAGQ")
	m3 = umxACE_GLM(selDVs = c("y", "z", "w"), mzData = mzData, dzData = dzData, sep = "_T", autoRun = FALSE)
	expect_true(all(c("a1", "a2", "a3", "c1", "c2", "c3", "e1_T1", "e2_T1", "e3_T1", "e1_T2", "e2_T2", "e3_T2") %in% m3$MZ@latentVars))
	expect_false("a1_T1" %in% m3$MZ@latentVars)
	expect_true(all(c("a1_T1", "a2_T1", "a3_T1", "a1_T2", "a2_T2", "a3_T2") %in% m3$DZ@latentVars))
	expect_equal(as.numeric(m3$MZ$S$values["y_T1", "y_T1"]), 0)
	expect_equal(as.numeric(m3$MZ$S$values["w_T1", "w_T1"]), 0)
	expect_false(isTRUE(m3$MZ$S$free["w_T1", "w_T1"]))
	expect_equal(m3$MZ$A$labels["w_T1", "a1"], "a_r3c1")
	expect_equal(m3$MZ$A$labels["w_T1", "a2"], "a_r3c2")
	expect_equal(m3$MZ$A$labels["w_T1", "a3"], "a_r3c3")
	expect_equal(as.numeric(m3$MZ$A$values["y_T1", "a1"]), as.numeric(m3$MZ$A$values["y_T2", "a1"]))
	expect_equal(as.numeric(m3$DZ$S$values["a3_T1", "a3_T2"]), 0.5)
	expect_equal(as.numeric(m3$MZ$A$values["w_T1", "a3"]), sqrt(0.8), tolerance = 1e-8)
	expect_equal(as.numeric(m3$MZ$A$values["w_T1", "e3_T1"]), sqrt(0.2), tolerance = 1e-8)
	expect_equal(as.numeric(m3$MZ$A$lbound["w_T1", "a3"]), 1e-4)
	dot3 = plot(m3, file = NA, std = FALSE)
	expect_true(grepl("a3 -> w", dot3, fixed = TRUE))
	expect_true(grepl("e3 -> w", dot3, fixed = TRUE))
	expect_false(grepl("e3_T1", dot3, fixed = TRUE))
})

test_that("umxACE_GLM 3-trait Poisson recovers a Cholesky on eta", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(24)
	n = 60
	trueA = matrix(0, 3, 3)
	trueA[1, 1] = 0.50
	trueA[2, 1] = 0.25
	trueA[2, 2] = 0.40
	trueA[3, 1] = 0.15
	trueA[3, 2] = 0.20
	trueA[3, 3] = 0.35
	trueC = matrix(0, 3, 3)
	trueC[1, 1] = 0.25
	trueC[2, 2] = 0.25
	trueC[3, 3] = 0.25
	trueE = matrix(0, 3, 3)
	trueE[1, 1] = 0.40
	trueE[2, 2] = 0.40
	trueE[3, 3] = 0.40
	trueB0 = c(0.20, 0.10, 0.00)
	simPair = function(rA, n) {
		yT1 = integer(n)
		zT1 = integer(n)
		wT1 = integer(n)
		yT2 = integer(n)
		zT2 = integer(n)
		wT2 = integer(n)
		for (i in 1:n) {
			Af = rnorm(3)
			Cf = rnorm(3)
			E1 = rnorm(3)
			E2 = rnorm(3)
			A2 = rA * Af + sqrt(max(0, 1 - rA * rA)) * rnorm(3)
			eta1 = trueB0 + as.numeric(trueA %*% Af) + as.numeric(trueC %*% Cf) + as.numeric(trueE %*% E1)
			eta2 = trueB0 + as.numeric(trueA %*% A2) + as.numeric(trueC %*% Cf) + as.numeric(trueE %*% E2)
			yT1[i] = rpois(1, lambda = exp(eta1[1]))
			zT1[i] = rpois(1, lambda = exp(eta1[2]))
			wT1[i] = rpois(1, lambda = exp(eta1[3]))
			yT2[i] = rpois(1, lambda = exp(eta2[1]))
			zT2[i] = rpois(1, lambda = exp(eta2[2]))
			wT2[i] = rpois(1, lambda = exp(eta2[3]))
		}
		data.frame(y_T1 = yT1, z_T1 = zT1, w_T1 = wT1, y_T2 = yT2, z_T2 = zT2, w_T2 = wT2)
	}
	mzData = simPair(1, n)
	dzData = simPair(0.5, n)
	m = umxACE_GLM(selDVs = c("y", "z", "w"), mzData = mzData, dzData = dzData, sep = "_T", autoRun = TRUE, tryHard = "yes")
	pars = omxGetParameters(m)
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	expect_equal(as.numeric(pars["a_r1c1"]), trueA[1, 1], tolerance = 0.35)
	expect_equal(as.numeric(pars["a_r2c2"]), trueA[2, 2], tolerance = 0.35)
	expect_equal(as.numeric(pars["a_r3c3"]), trueA[3, 3], tolerance = 0.35)
	expect_equal(as.numeric(pars["a_r2c1"]), trueA[2, 1], tolerance = 0.40)
	expect_equal(as.numeric(pars["e_r1c1"]), trueE[1, 1], tolerance = 0.35)
	tab = umxSummary(m, std = FALSE)
	expect_true("a3" %in% names(tab))
	expect_true("e3" %in% names(tab))
})

test_that("umxACE_GLM Gamma and inverse Gaussian fix leftover theta at 1 and keep e free", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(16)
	n = 12
	mzData = data.frame(y_T1 = rgamma(n, 2, 1), y_T2 = rgamma(n, 2, 1))
	dzData = mzData
	mG = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::Gamma(link = "log"), autoRun = FALSE)
	expect_equal(mG$MZ$fitfunction$family$y_T1@family, "gamma")
	expect_equal(as.numeric(mG$MZ$fitfunction$family$y_T1@theta), 1)
	expect_equal(mG$MZ$fitfunction$family$y_T1@thetaLabel, "")
	expect_true(isTRUE(mG$MZ$A$free["y_T1", "e1_T1"]))
	expect_equal(as.numeric(mG$MZ$A$lbound["y_T1", "a1"]), 1e-4)
	expect_equal(as.numeric(mG$MZ$A$lbound["y_T1", "e1_T1"]), 1e-4)
	expect_equal(as.numeric(mG$MZ$A$values["y_T1", "a1"]), 0.2, tolerance = 1e-8)
	mI = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::inverse.gaussian(link = "log"), autoRun = FALSE)
	expect_equal(mI$MZ$fitfunction$family$y_T1@family, "inversegaussian")
	expect_equal(as.numeric(mI$MZ$fitfunction$family$y_T1@theta), 1)
	expect_true(isTRUE(mI$MZ$A$free["y_T1", "e1_T1"]))
	m2 = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::Gamma(link = "log"), theta = 2, autoRun = FALSE)
	expect_equal(as.numeric(m2$MZ$fitfunction$family$y_T1@theta), 2)
	expect_error(umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::Gamma(link = "log"), theta = "shp", autoRun = FALSE), "collides")
	famFree = mxFamily("y", stats::Gamma(link = "log"), theta = "shp")
	expect_error(umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = famFree, autoRun = FALSE), "collides")
})

test_that("umxACE_GLM dzCr=0.25 uses two DZ c latents and equateMeans=FALSE splits intercepts", {
	skip_if_not(exists("mxFamily", mode = "function"))
	mzData = data.frame(y_T1 = 1:6, y_T2 = 2:7)
	dzData = mzData
	mAde = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", dzCr = 0.25, autoRun = FALSE)
	expect_true(all(c("c1_T1", "c1_T2") %in% mAde$DZ@latentVars))
	expect_false("c1_T1" %in% mAde$MZ@latentVars)
	expect_true("c1" %in% mAde$MZ@latentVars)
	expect_equal(as.numeric(mAde$DZ$S$values["c1_T1", "c1_T2"]), 0.25)
	mMeans = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", equateMeans = FALSE, autoRun = FALSE)
	labs = as.character(mMeans$MZ$M$labels)
	expect_true("one_to_y_T1" %in% labs)
	expect_true("one_to_y_T2" %in% labs)
	expect_false("one_to_y" %in% labs)
})

test_that("umxACE_GLM recovers a,c,e on Poisson eta and umxSummary has no mxRefModels", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(12)
	n = 50
	trueA = 0.5
	trueC = 0.3
	trueE = 0.4
	trueB0 = 0.2
	simPair = function(rA, n) {
		A = rnorm(n)
		C = rnorm(n)
		E1 = rnorm(n)
		E2 = rnorm(n)
		A2 = rA * A + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
		y1 = rpois(n, lambda = exp(trueB0 + trueA * A + trueC * C + trueE * E1))
		y2 = rpois(n, lambda = exp(trueB0 + trueA * A2 + trueC * C + trueE * E2))
		data.frame(y_T1 = y1, y_T2 = y2)
	}
	mzData = simPair(1, n)
	dzData = simPair(0.5, n)
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", autoRun = TRUE, tryHard = "yes")
	pars = omxGetParameters(m)
	a = as.numeric(pars["a_r1c1"])
	c = as.numeric(pars["c_r1c1"])
	e = as.numeric(pars["e_r1c1"])
	b0 = as.numeric(pars[grep("^one_to_y", names(pars))[1]])
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	expect_equal(a, trueA, tolerance = 0.25)
	expect_equal(c, trueC, tolerance = 0.25)
	expect_equal(e, trueE, tolerance = 0.25)
	expect_equal(b0, trueB0, tolerance = 0.25)
	out = capture.output(umxSummary(m))
	expect_false(grepl("mxRefModels", paste(out, collapse = "\n")))
	expect_false(grepl("Factor correlations", paste(out, collapse = "\n")))
	expect_message(umxSummary(m), "-2LL")
	tabRaw = umxSummary(m, std = FALSE)
	expect_equal(as.numeric(tabRaw$e1), e, tolerance = 0.05)
	tabStd = umxSummary(m, std = TRUE)
	ss = as.numeric(tabStd$a1)^2 + as.numeric(tabStd$c1)^2 + as.numeric(tabStd$e1)^2
	expect_equal(ss, 1, tolerance = 0.05)
})

test_that("umxACE_GLM Poisson meat SEs are finite and match a pair bootstrap on the intercept", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(82)
	n = 80
	trueA = 0.5
	trueC = 0.3
	trueE = 0.4
	trueB0 = 0.2
	simPair = function(rA, n) {
		A = rnorm(n)
		C = rnorm(n)
		E1 = rnorm(n)
		E2 = rnorm(n)
		A2 = rA * A + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
		y1 = rpois(n, lambda = exp(trueB0 + trueA * A + trueC * C + trueE * E1))
		y2 = rpois(n, lambda = exp(trueB0 + trueA * A2 + trueC * C + trueE * E2))
		data.frame(y_T1 = y1, y_T2 = y2)
	}
	mzData = simPair(1, n)
	dzData = simPair(0.5, n)
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", autoRun = TRUE, tryHard = "yes")
	mSe = mxModel(m, mxComputeSequence(list(
		mxComputeOnce("fitfunction", "information", "meat"),
		mxComputeStandardError(),
		mxComputeReportDeriv())))
	mSe = mxRun(mSe)
	se = mSe$output$standardErrors
	expect_true(!is.null(se))
	expect_true(all(is.finite(se)))
	B = 25
	bootInt = numeric(B)
	for (b in 1:B) {
		mzB = mzData[sample.int(n, n, replace = TRUE), , drop = FALSE]
		dzB = dzData[sample.int(n, n, replace = TRUE), , drop = FALSE]
		mb = umxACE_GLM(selDVs = "y", mzData = mzB, dzData = dzB, sep = "_T", autoRun = TRUE, tryHard = "no")
		bootInt[b] = as.numeric(omxGetParameters(mb)["one_to_y"])
	}
	seIntMeat = as.numeric(se["one_to_y", 1])
	seIntBoot = sd(bootInt)
	expect_true(is.finite(seIntMeat))
	expect_true(is.finite(seIntBoot) && seIntBoot > 0)
	expect_equal(seIntMeat, seIntBoot, tolerance = 0.6)
})

test_that("umxACE_GLM univariate binomial ACE fixes e diagonal at 1", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(13)
	n = 80
	trueA = 0.5
	trueC = 0.3
	trueE = 0.4
	trueB0 = 0.2
	simPair = function(rA, n) {
		A = rnorm(n)
		C = rnorm(n)
		E1 = rnorm(n)
		E2 = rnorm(n)
		A2 = rA * A + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
		p1 = plogis(trueB0 + trueA * A + trueC * C + trueE * E1)
		p2 = plogis(trueB0 + trueA * A2 + trueC * C + trueE * E2)
		y1 = rbinom(n, 1, p1)
		y2 = rbinom(n, 1, p2)
		data.frame(y_T1 = y1, y_T2 = y2)
	}
	mzData = simPair(1, n)
	dzData = simPair(0.5, n)
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::binomial(link = "logit"), autoRun = TRUE, tryHard = "yes")
	expect_equal(m$MZ$fitfunction$family$y_T1@family, "binomial")
	expect_equal(m$MZ$fitfunction$family$y_T1@link, "logit")
	expect_equal(as.numeric(m$MZ$S$values["y_T1", "y_T1"]), 0)
	pars = omxGetParameters(m)
	a = as.numeric(pars["a_r1c1"])
	c = as.numeric(pars["c_r1c1"])
	b0 = as.numeric(pars[grep("^one_to_y", names(pars))[1]])
	expect_false("e_r1c1" %in% names(pars))
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "e1_T1"]), 1)
	expect_false(isTRUE(m$MZ$A$free["y_T1", "e1_T1"]))
	expect_equal(as.numeric(m$MZ$A$values["y_T2", "e1_T2"]), 1)
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	expect_true(is.finite(a) && a >= -1e-8)
	expect_true(is.finite(c) && c >= -1e-8)
	expect_true(is.finite(b0) && abs(b0) < 5)
	tabRaw = umxSummary(m, std = FALSE)
	# Printed e is chol(1 + pi^2/3) ≈ 2.07, not leftover alone (1.81) or the path (1).
	expect_true(as.numeric(tabRaw$e1) > 1.9)
	dotB = plot(m, file = NA, std = FALSE)
	expect_true(grepl("e -> y", dotB, fixed = TRUE))
	expect_true(grepl("2.07", dotB, fixed = TRUE))
	tabStd = umxSummary(m, std = TRUE)
	ss = as.numeric(tabStd$a1)^2 + as.numeric(tabStd$c1)^2 + as.numeric(tabStd$e1)^2
	expect_equal(ss, 1, tolerance = 0.05)
})

test_that("umxACE_GLM bivariate Cholesky builds shared a/c and per-twin e", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(14)
	n = 15
	mzData = data.frame(y_T1 = rpois(n, 2), z_T1 = rpois(n, 2), y_T2 = rpois(n, 2), z_T2 = rpois(n, 2))
	dzData = mzData
	m = umxACE_GLM(selDVs = c("y", "z"), mzData = mzData, dzData = dzData, sep = "_T", autoRun = FALSE)
	expect_true(all(c("a1", "a2", "c1", "c2", "e1_T1", "e2_T1", "e1_T2", "e2_T2") %in% m$MZ@latentVars))
	expect_false("a1_T1" %in% m$MZ@latentVars)
	expect_true(all(c("a1_T1", "a2_T1", "a1_T2", "a2_T2") %in% m$DZ@latentVars))
	expect_equal(as.numeric(m$DZ$S$values["a1_T1", "a1_T2"]), 0.5)
	expect_equal(as.numeric(m$MZ$S$values["y_T1", "y_T1"]), 0)
	expect_equal(as.numeric(m$MZ$S$values["z_T1", "z_T1"]), 0)
	expect_true("a_r2c1" %in% m$MZ$A$labels)
	expect_true("e_r2c1" %in% m$MZ$A$labels)
	expect_true("a_r2c2" %in% m$MZ$A$labels)
	expect_true(isTRUE(m$MZ$A$free["z_T1", "a1"]))
	expect_false(isTRUE(m$MZ$A$free["y_T1", "a2"]))
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "a1"]), sqrt(0.8), tolerance = 1e-8)
	expect_equal(as.numeric(m$MZ$A$values["z_T1", "a2"]), sqrt(0.8), tolerance = 1e-8)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "e1_T1"]), sqrt(0.2), tolerance = 1e-8)
	expect_true(is.na(m$MZ$A$lbound["z_T1", "a1"]) || abs(as.numeric(m$MZ$A$lbound["z_T1", "a1"]) - 1e-4) > 1e-12)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "a1"]), as.numeric(m$MZ$A$values["y_T2", "a1"]))
	expect_equal(as.numeric(m$MZ$S$values["e1_T1", "e2_T1"]), 0)
	expect_equal(as.numeric(m$MZ$S$values["a1", "a2"]), 0)
})

test_that("umxACE_GLM bivariate Poisson runs and keeps off-diagonal e", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(15)
	n = 30
	simPair = function(rA, n) {
		A1 = rnorm(n)
		A2 = rnorm(n)
		C1 = rnorm(n)
		C2 = rnorm(n)
		E1a = rnorm(n)
		E2a = rnorm(n)
		E1b = rnorm(n)
		E2b = rnorm(n)
		A1b = rA * A1 + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
		A2b = rA * A2 + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
		# Cholesky on eta: y <- a11,z <- a21+a22 etc.
		ey1 = 0.2 + 0.4 * A1 + 0.2 * C1 + 0.4 * E1a
		ez1 = 0.1 + 0.2 * A1 + 0.35 * A2 + 0.15 * C1 + 0.25 * C2 + 0.2 * E1a + 0.35 * E2a
		ey2 = 0.2 + 0.4 * A1b + 0.2 * C1 + 0.4 * E1b
		ez2 = 0.1 + 0.2 * A1b + 0.35 * A2b + 0.15 * C1 + 0.25 * C2 + 0.2 * E1b + 0.35 * E2b
		data.frame(
			y_T1 = rpois(n, lambda = exp(ey1)),
			z_T1 = rpois(n, lambda = exp(ez1)),
			y_T2 = rpois(n, lambda = exp(ey2)),
			z_T2 = rpois(n, lambda = exp(ez2))
		)
	}
	mzData = simPair(1, n)
	dzData = simPair(0.5, n)
	m = umxACE_GLM(selDVs = c("y", "z"), mzData = mzData, dzData = dzData, sep = "_T", autoRun = TRUE, tryHard = "yes")
	pars = omxGetParameters(m)
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	expect_true(is.finite(pars["a_r1c1"]) && pars["a_r1c1"] >= -1e-8)
	expect_true(is.finite(pars["e_r2c1"]))
	expect_true(is.finite(pars["a_r2c1"]))
	expect_true(is.finite(pars["a_r2c2"]) && pars["a_r2c2"] >= -1e-8)
	expect_true(is.finite(pars["e_r2c2"]) && pars["e_r2c2"] >= -1e-8)
	out = capture.output(umxSummary(m))
	expect_false(grepl("mxRefModels", paste(out, collapse = "\n")))
	expect_false(grepl("Bernoulli leftover on E diagonal", paste(out, collapse = "\n")))
	expect_true(grepl("Factor correlations", paste(out, collapse = "\n")))
	expect_true(grepl("rE", paste(out, collapse = "\n")))
	expect_message(umxSummary(m), "-2LL")
	tabRaw = umxSummary(m, std = FALSE)
	expect_equal(as.numeric(tabRaw[1, "e1"]), as.numeric(pars["e_r1c1"]), tolerance = 0.05)
	expect_equal(as.numeric(tabRaw[2, "e1"]), as.numeric(pars["e_r2c1"]), tolerance = 0.05)
	expect_equal(as.numeric(tabRaw[2, "e2"]), as.numeric(pars["e_r2c2"]), tolerance = 0.05)
	e11 = as.numeric(pars["e_r1c1"])
	e21 = as.numeric(pars["e_r2c1"])
	e22 = as.numeric(pars["e_r2c2"])
	E11 = e11 * e11
	E22 = e21 * e21 + e22 * e22
	if (E11 > 1e-12 && E22 > 1e-12) {
		rePath = (e21 * e11) / sqrt(E11 * E22)
		expect_true(is.finite(rePath))
	}
})

test_that("umxACE_GLM Poisson supermodel -2LL is MZ plus DZ at the same theta", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(17)
	n = 20
	mzData = data.frame(y_T1 = rpois(n, 2), y_T2 = rpois(n, 2))
	dzData = data.frame(y_T1 = rpois(n, 2), y_T2 = rpois(n, 2))
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", autoRun = TRUE, tryHard = "no")
	expect_true(is.finite(as.numeric(m$output$fit)))
	mzOnce = mxRun(mxModel(m$MZ, mxComputeOnce("fitfunction", "fit")))
	dzOnce = mxRun(mxModel(m$DZ, mxComputeOnce("fitfunction", "fit")))
	expect_equal(as.numeric(m$output$fit), as.numeric(mzOnce$output$fit) + as.numeric(dzOnce$output$fit), tolerance = 1e-4)
	mFit = mxRun(mxModel(m, mxComputeOnce("fitfunction", "fit")))
	mGrad = mxRun(mxModel(m, mxComputeOnce("fitfunction", c("fit", "gradient"))))
	expect_equal(as.numeric(mFit$output$fit), as.numeric(mGrad$output$fit), tolerance = 1e-6)
})

test_that("umxACE_GLM Gamma run with theta=1 is finite and e stays free", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(18)
	n = 30
	mzData = data.frame(y_T1 = rgamma(n, 2, 1), y_T2 = rgamma(n, 2, 1))
	dzData = data.frame(y_T1 = rgamma(n, 2, 1), y_T2 = rgamma(n, 2, 1))
	m = umxACE_GLM(selDVs = "y", mzData = mzData, dzData = dzData, sep = "_T", family = stats::Gamma(link = "log"), autoRun = TRUE, tryHard = "no")
	expect_equal(as.numeric(m$MZ$fitfunction$family$y_T1@theta), 1)
	expect_true(isTRUE(m$MZ$A$free["y_T1", "e1_T1"]))
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	e = as.numeric(omxGetParameters(m)["e_r1c1"])
	expect_true(is.finite(e) && e >= -1e-8)
})

test_that("umxACE_GLM bivariate binomial fixes e diag at 1 and rE is diluted by leftover", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(19)
	n = 20
	mzData = data.frame(
		y_T1 = rbinom(n, 1, 0.5), z_T1 = rbinom(n, 1, 0.5),
		y_T2 = rbinom(n, 1, 0.5), z_T2 = rbinom(n, 1, 0.5)
	)
	dzData = mzData
	m = umxACE_GLM(selDVs = c("y", "z"), mzData = mzData, dzData = dzData, sep = "_T", family = stats::binomial(link = "logit"), autoRun = FALSE)
	expect_equal(as.numeric(m$MZ$A$values["y_T1", "e1_T1"]), 1)
	expect_false(isTRUE(m$MZ$A$free["y_T1", "e1_T1"]))
	expect_equal(as.numeric(m$MZ$A$values["z_T1", "e2_T1"]), 1)
	expect_false(isTRUE(m$MZ$A$free["z_T1", "e2_T1"]))
	expect_true(isTRUE(m$MZ$A$free["z_T1", "e1_T1"]))
	expect_false("e_r1c1" %in% names(omxGetParameters(m)))
	expect_true("e_r2c1" %in% names(omxGetParameters(m)))
	m = omxSetParameters(m, labels = "e_r2c1", values = 0.4)
	m = mxRun(mxModel(m, mxComputeOnce("fitfunction", "fit")))
	expect_true(is.finite(as.numeric(m$output$fit)))
	e11 = 1
	e21 = 0.4
	e22 = 1
	d = pi^2 / 3
	Epath11 = e11 * e11
	Epath22 = e21 * e21 + e22 * e22
	Epath21 = e21 * e11
	rePath = Epath21 / sqrt(Epath11 * Epath22)
	reTot = Epath21 / sqrt((Epath11 + d) * (Epath22 + d))
	expect_true(abs(reTot) < abs(rePath) - 1e-8)
	out = capture.output(umxSummary(m, std = FALSE))
	expect_true(grepl("rE", paste(out, collapse = "\n")))
	expect_true(grepl("diluted", paste(out, collapse = "\n")))
	expect_message(umxSummary(m, std = FALSE), "Bernoulli leftover")
	reStr = formatC(reTot, format = "f", digits = 2)
	expect_true(grepl(reStr, paste(out, collapse = "\n"), fixed = TRUE))
})
