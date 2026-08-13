# test_active_file("~/bin/umx/tests/testthat/test_umxRAM_DE.R")
library(umx)
library(testthat)

test_that("T0 umxRAM_DE structure with fixCensorThresholds = no", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	cutLitres = 2
	prep = umx_make_double_entry_data(tmp, cols = list(litres = cutLitres), sep = "", nSib = 1)

	mNo = umxRAM_DE("T0no", data = prep, DEvars = "litres",
		umxPath("litres", to = "mpg"),
		umxPath("wt", to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("litres", "wt", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "no"
	)

	expect_true("litres" %in% mNo$latentVars)
	expect_true("litres_cont" %in% mNo$manifestVars)
	expect_true("litres_cens" %in% mNo$manifestVars)
	expect_false("litres" %in% mNo$manifestVars)

	expect_false(isTRUE(mNo$A$free["litres_cont", "litres"]))
	expect_false(isTRUE(mNo$A$free["litres_cens", "litres"]))
	expect_equal(as.numeric(mNo$A$values["litres_cont", "litres"]), 1)
	expect_equal(as.numeric(mNo$A$values["litres_cens", "litres"]), 1)
	expect_true(isTRUE(mNo$A$free["mpg", "litres"]))
	expect_false(isTRUE(mNo$A$free["mpg", "litres_cont"]))
	expect_false(isTRUE(mNo$A$free["mpg", "litres_cens"]))

	expect_false(isTRUE(mNo$S$free["litres_cont", "litres_cont"]))
	expect_false(isTRUE(mNo$S$free["litres_cens", "litres_cens"]))
	expect_equal(as.numeric(mNo$S$values["litres_cont", "litres_cont"]), 0)
	expect_equal(as.numeric(mNo$S$values["litres_cens", "litres_cens"]), 0)
	expect_true(isTRUE(mNo$S$free["litres", "litres"]))

	expect_false(isTRUE(mNo$M$free[1, "litres_cont"]))
	expect_false(isTRUE(mNo$M$free[1, "litres_cens"]))
	expect_equal(as.numeric(mNo$M$values[1, "litres_cont"]), 0)
	expect_equal(as.numeric(mNo$M$values[1, "litres_cens"]), 0)
	expect_true(isTRUE(mNo$M$free[1, "litres"]))

	expect_true(isTRUE(mNo$S$free["wt", "litres"]))
	expect_false(isTRUE(mNo$S$free["litres_cont", "litres_cens"]))
	expect_equal(as.numeric(mNo$S$values["litres_cont", "litres_cens"]), 0)

	expect_true(!is.null(mNo$deviations_for_thresh))
	expect_equal(as.character(mNo$deviations_for_thresh$labels[1, "litres_cens"]), "litres_cens_dev1")
	expect_true(isTRUE(mNo$deviations_for_thresh$free[1, "litres_cens"]))
	startTau = as.numeric(mNo$deviations_for_thresh$values[1, "litres_cens"])
	expect_true(is.finite(startTau))
	expect_false(isTRUE(all.equal(startTau, 0.1)))
	expect_equal(startTau, as.numeric(mNo$M$values[1, "litres"]))

	deAttr = attr(mNo, "umxDE")
	expect_false(isTRUE(deAttr$fixedCensorThresholds))
	expect_equal(deAttr$DEvars, "litres")
	expect_equal(unname(deAttr$contByCens["litres_cens"]), "litres_cont")

	pars = omxGetParameters(mNo)
	expect_equal(sum(names(pars) == "wt_to_mpg"), 1L)
	expect_equal(sum(names(pars) == "litres_to_mpg"), 1L)
	expect_true("litres_cens_dev1" %in% names(pars))
})

test_that("T1 umxRAM_DE structure with fixCensorThresholds = yes", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	cutLitres = 2
	prep = umx_make_double_entry_data(tmp, cols = list(litres = cutLitres), sep = "", nSib = 1)

	mYes = umxRAM_DE("T1yes", data = prep, DEvars = "litres",
		umxPath("litres", to = "mpg"),
		umxPath("wt", to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("litres", "wt", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)

	expect_true("litres" %in% mYes$latentVars)
	expect_false(isTRUE(mYes$A$free["litres_cont", "litres"]))
	expect_equal(as.numeric(mYes$A$values["litres_cont", "litres"]), 1)
	expect_equal(as.numeric(mYes$A$values["litres_cens", "litres"]), 1)
	expect_true(isTRUE(mYes$A$free["mpg", "litres"]))
	expect_false(isTRUE(mYes$S$free["litres_cont", "litres_cont"]))
	expect_false(isTRUE(mYes$S$free["litres_cens", "litres_cens"]))
	expect_equal(as.numeric(mYes$S$values["litres_cens", "litres_cens"]), 0)
	expect_true(isTRUE(mYes$S$free["litres", "litres"]))
	expect_false(isTRUE(mYes$M$free[1, "litres_cont"]))
	expect_false(isTRUE(mYes$M$free[1, "litres_cens"]))
	expect_equal(as.numeric(mYes$M$values[1, "litres_cens"]), 0)
	expect_true(isTRUE(mYes$M$free[1, "litres"]))
	expect_false(isTRUE(mYes$S$free["litres_cont", "litres_cens"]))
	expect_equal(as.numeric(mYes$S$values["litres_cont", "litres_cens"]), 0)

	expect_false(isTRUE(mYes$deviations_for_thresh$free[1, "litres_cens"]))
	expect_equal(as.numeric(mYes$deviations_for_thresh$values[1, "litres_cens"]), cutLitres)

	deAttr = attr(mYes, "umxDE")
	expect_true(isTRUE(deAttr$fixedCensorThresholds))
	expect_equal(unname(deAttr$fixedCuts["litres_cens"]), cutLitres)
	expect_equal(unname(deAttr$contByCens["litres_cens"]), "litres_cont")

	pars = omxGetParameters(mYes)
	expect_false("litres_cens_dev1" %in% names(pars))
	expect_false("litres_cens_to_mpg" %in% names(pars))
	expect_false("litres_cont_to_mpg" %in% names(pars))
	expect_true("litres_to_mpg" %in% names(pars))
	expect_equal(sum(names(pars) == "wt_to_mpg"), 1L)
})

test_that("T2 umxRAM_DE equates latent loadings onto a DE trait", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = 2), sep = "", nSib = 1)

	mLat = umxRAM_DE("T2lat", data = prep, DEvars = "litres",
		umxPath("F", to = "litres"),
		umxPath(v1m0 = "F"),
		umxPath(v.m. = c("litres", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)

	expect_true("F" %in% mLat$latentVars)
	expect_true("litres" %in% mLat$latentVars)
	expect_false("F" %in% colnames(prep))
	expect_true(isTRUE(mLat$A$free["litres", "F"]))
	expect_false(isTRUE(mLat$A$free["litres_cont", "F"]))
	expect_false(isTRUE(mLat$A$free["litres_cens", "F"]))
	expect_equal(as.numeric(mLat$A$values["litres_cont", "litres"]), 1)
	expect_equal(as.numeric(mLat$A$values["litres_cens", "litres"]), 1)
	expect_false(isTRUE(mLat$S$free["litres_cens", "litres_cens"]))
	expect_equal(as.numeric(mLat$S$values["litres_cens", "litres_cens"]), 0)
	expect_true(isTRUE(mLat$S$free["litres", "litres"]))
	expect_true(isTRUE(mLat$M$free[1, "litres"]))
	expect_false(isTRUE(mLat$M$free[1, "litres_cens"]))
})

test_that("T3 umxRAM_DE API errors", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = 2), sep = "", nSib = 1)

	expect_error(
		umxRAM_DE("T3null", data = prep, umxPath(v.m. = "mpg"), autoRun = FALSE),
		regexp = "DEvars"
	)
	expect_error(
		umxRAM_DE("T3suf", data = prep, DEvars = "litres_cont", umxPath(v.m. = "litres"), autoRun = FALSE),
		regexp = "base names"
	)
	expect_error(
		umxRAM_DE("T3path", data = prep, DEvars = "litres", umxPath("litres_cont", to = "mpg"), autoRun = FALSE),
		regexp = "base name"
	)
	expect_error(
		umxRAM_DE("T3cuts", data = prep, DEvars = "litres",
			umxPath(v.m. = "litres"),
			autoRun = FALSE, fixCensorThresholds = "no", censorCuts = c(litres = 2)
		),
		regexp = "censorCuts"
	)
	expect_error(
		umxRAM_DE("T3wls", data = prep, DEvars = "litres",
			umxPath(v.m. = "litres"),
			autoRun = FALSE, type = "WLS"
		),
		regexp = "not valid for umxRAM_DE.*umxRAM"
	)
	expect_error(
		umxRAM_DE("T3dwls", data = prep, DEvars = "litres",
			umxPath(v.m. = "litres"),
			autoRun = FALSE, type = "DWLS"
		),
		regexp = "not valid for umxRAM_DE.*umxRAM"
	)
	expect_error(
		umxRAM_DE("T3cov", data = prep, DEvars = "litres",
			umxPath(v.m. = "litres"),
			autoRun = FALSE, type = "cov"
		),
		regexp = "not valid for umxRAM_DE.*umxRAM"
	)
	expect_error(
		umxRAM_DE("T3cor", data = prep, DEvars = "litres",
			umxPath(v.m. = "litres"),
			autoRun = FALSE, type = "cor"
		),
		regexp = "not valid for umxRAM_DE.*umxRAM"
	)
	expect_error(
		umxRAM_DE("mpg ~ litres", data = prep, DEvars = "litres", autoRun = FALSE),
		regexp = "lavaan"
	)
	expect_error(
		umxRAM_DE("T3miss", data = tmp, DEvars = "litres", umxPath(v.m. = "litres"), autoRun = FALSE),
		regexp = "umx_make_double_entry_data"
	)
})

test_that("umxHetCor skips one-level DE cens columns instead of warning", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = 2), sep = "", nSib = 1)
	hetCols = prep[, c("mpg", "wt", "litres_cont", "litres_cens")]
	warns = character(0)
	R = NULL
	withCallingHandlers({
		R = umxHetCor(hetCols)
	}, warning = function(w) {
		warns <<- c(warns, conditionMessage(w))
		invokeRestart("muffleWarning")
	})
	expect_false(any(grepl("polyserial|polychoric|no cases for pair|couldn't be computed", warns)))
	expect_equal(unname(R["litres_cens", "litres_cens"]), 1)
	expect_equal(unname(R["litres_cens", "mpg"]), 0)
	expect_equal(unname(R["mpg", "litres_cens"]), 0)
	expect_true(is.finite(R["mpg", "wt"]))
	expect_true(abs(R["mpg", "wt"]) < 1)

	# Building a DE model must not emit hetcor empty-pair warnings
	buildWarns = character(0)
	withCallingHandlers({
		m = umxRAM_DE("hetStarts", data = prep, DEvars = "litres",
			umxPath("litres", to = "mpg"),
			umxPath(v.m. = c("litres", "mpg")),
			autoRun = FALSE, fixCensorThresholds = "yes"
		)
	}, warning = function(w) {
		buildWarns <<- c(buildWarns, conditionMessage(w))
		invokeRestart("muffleWarning")
	})
	expect_false(any(grepl("polyserial|polychoric|no cases for pair|couldn't be computed", buildWarns)))
})

test_that("T4 umxRAM_DE right-censor structure", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = ">= 4"), sep = "", nSib = 1)
	expect_equal(levels(prep$litres_cens), c("observed", "censored"))

	mRight = umxRAM_DE("T4right", data = prep, DEvars = "litres",
		umxPath("litres", to = "mpg"),
		umxPath(v.m. = c("litres", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)

	expect_false(isTRUE(mRight$deviations_for_thresh$free[1, "litres_cens"]))
	expect_equal(as.numeric(mRight$deviations_for_thresh$values[1, "litres_cens"]), 4)
	expect_true("litres" %in% mRight$latentVars)
	expect_equal(as.numeric(mRight$A$values["litres_cont", "litres"]), 1)
	expect_equal(as.numeric(mRight$A$values["litres_cens", "litres"]), 1)
	expect_true(isTRUE(mRight$A$free["mpg", "litres"]))
	expect_false(isTRUE(mRight$S$free["litres_cens", "litres_cens"]))
	expect_equal(as.numeric(mRight$S$values["litres_cens", "litres_cens"]), 0)
	expect_false(isTRUE(mRight$M$free[1, "litres_cens"]))
	expect_equal(unname(attr(mRight, "umxDE")$fixedCuts["litres_cens"]), 4)
})

test_that("T5 umxRAM_DE univariate hand -2LL at known theta", {
	set.seed(1)
	n = 200
	muPop = 0.4
	sdPop = 1
	cutVal = 0
	dat = data.frame(litres = rnorm(n, mean = muPop, sd = sdPop))
	prep = umx_make_double_entry_data(dat, cols = list(litres = cutVal), sep = "", nSib = 1)

	mUniv = umxRAM_DE("T5univ", data = prep, DEvars = "litres",
		umxPath(v.m. = "litres"),
		autoRun = FALSE, fixCensorThresholds = "yes", setValues = TRUE
	)
	mUniv = omxSetParameters(mUniv, labels = "one_to_litres", free = FALSE, values = muPop)
	mUniv = omxSetParameters(mUniv, labels = "litres_with_litres", free = FALSE, values = sdPop^2)
	mUniv = omxAssignFirstParameters(mUniv)
	mUniv = mxRun(mUniv, silent = TRUE)

	hand = 0
	for (i in 1:n) {
		yCont = prep$litres_cont[i]
		if (!is.na(yCont)) {
			hand = hand + (-2) * dnorm(yCont, mean = muPop, sd = sdPop, log = TRUE)
		} else {
			hand = hand + (-2) * pnorm(cutVal, mean = muPop, sd = sdPop, log.p = TRUE)
		}
	}
	expect_true(is.finite(mUniv$output$Minus2LogLikelihood))
	expect_equal(as.numeric(mUniv$output$Minus2LogLikelihood), hand, tolerance = 1e-6)
})

test_that("T6 umxRAM_DE recovers continuous umxRAM regression after left-censor", {
	set.seed(2)
	n = 800
	muL = 0.5
	sdL = 1
	aPop = 2
	bPop = -0.8
	sdE = 1
	litres = rnorm(n, mean = muL, sd = sdL)
	mpg = aPop + bPop * litres + rnorm(n, mean = 0, sd = sdE)
	raw = data.frame(litres = litres, mpg = mpg)

	mCont = umxRAM("T6cont", data = raw,
		umxPath("litres", to = "mpg"),
		umxPath(v.m. = c("litres", "mpg")),
		autoRun = TRUE
	)
	expect_true(is.finite(mCont$output$Minus2LogLikelihood))

	cutL = as.numeric(quantile(raw$litres, 0.2))
	prep = umx_make_double_entry_data(raw, cols = list(litres = cutL), sep = "", nSib = 1)

	mDE0 = umxRAM_DE("T6de0", data = prep, DEvars = "litres",
		umxPath("litres", to = "mpg"),
		umxPath(v.m. = c("litres", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)
	contPars = omxGetParameters(mCont)
	compareLabs = c("litres_to_mpg", "litres_with_litres", "one_to_litres", "one_to_mpg", "mpg_with_mpg")
	for (lab in compareLabs) {
		expect_true(lab %in% names(contPars))
		mDE0 = omxSetParameters(mDE0, labels = lab, free = FALSE, values = as.numeric(contPars[[lab]]))
	}
	mDE0 = omxAssignFirstParameters(mDE0)
	mDE0 = mxRun(mDE0, silent = TRUE)
	expect_true(is.finite(mDE0$output$Minus2LogLikelihood))

	mDE = umxRAM_DE("T6de", data = prep, DEvars = "litres",
		umxPath("litres", to = "mpg"),
		umxPath(v.m. = c("litres", "mpg")),
		autoRun = TRUE, tryHard = "yes", fixCensorThresholds = "yes"
	)
	expect_true(is.finite(mDE$output$Minus2LogLikelihood))
	dePars = omxGetParameters(mDE)
	seMat = mDE$output$standardErrors
	for (lab in compareLabs) {
		estC = as.numeric(contPars[[lab]])
		estD = as.numeric(dePars[[lab]])
		expect_true(is.finite(estC))
		expect_true(is.finite(estD))
		seD = 0.08
		if (!is.null(seMat) && lab %in% rownames(seMat)) {
			seTry = as.numeric(seMat[lab, 1])
			if (is.finite(seTry) && seTry > 0) {
				seD = seTry
			}
		}
		tol = max(0.08, 3 * seD)
		expect_true(abs(estD - estC) < tol, info = paste0(lab, " DE=", estD, " cont=", estC, " tol=", tol))
		expect_true(abs(estD - estC) < abs(estD - 0) || abs(estC) < 0.05, info = paste0(lab, " not closer to continuous MLE than to 0"))
	}
	# Implied E[mpg] = ((I-A)^{-1} M) should match the sample mean on both models
	Acont = mCont$A$values
	muCont = as.numeric(solve(diag(nrow(Acont)) - Acont, as.numeric(mCont$M$values[1, rownames(Acont)])))
	names(muCont) = rownames(Acont)
	Ade = mDE$A$values
	muDE = as.numeric(solve(diag(nrow(Ade)) - Ade, as.numeric(mDE$M$values[1, rownames(Ade)])))
	names(muDE) = rownames(Ade)
	expect_equal(as.numeric(muCont[["mpg"]]), mean(raw$mpg), tolerance = 0.05)
	expect_equal(as.numeric(muDE[["mpg"]]), mean(raw$mpg), tolerance = 0.05)
})

test_that("T7 umxRAM_DE smoke fit and umxSummary", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = 2), sep = "", nSib = 1)

	sumWarns = character(0)
	mFit = NULL
	withCallingHandlers({
		mFit = umxRAM_DE("T7smoke", data = prep, DEvars = "litres",
			umxPath("litres", to = "mpg"),
			umxPath("wt", to = "mpg"),
			umxPath("wt", with = "litres"),
			umxPath(v.m. = c("litres", "wt", "mpg")),
			autoRun = TRUE, tryHard = "yes", fixCensorThresholds = "yes"
		)
		umxSummary(mFit, std = TRUE, file = NA)
	}, warning = function(w) {
		sumWarns <<- c(sumWarns, conditionMessage(w))
		invokeRestart("muffleWarning")
	})
	expect_true(is.finite(mFit$output$Minus2LogLikelihood))
	st = as.integer(mFit$output$status$code)
	expect_true(st %in% c(0L, 1L), info = paste0("status code ", st))
	expect_false(any(grepl("data length differs from size of matrix", sumWarns)))
})

test_that("T8 two DE traits: one latent-latent S, no 4-way indicator paths", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	prep = umx_make_double_entry_data(tmp, cols = list(litres = 2, wt = 3), sep = "", nSib = 1)

	m2 = umxRAM_DE("T8twoDE", data = prep, DEvars = c("litres", "wt"),
		umxPath("litres", to = "mpg"),
		umxPath("wt", to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("litres", "wt", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)

	expect_true("litres" %in% m2$latentVars)
	expect_true("wt" %in% m2$latentVars)
	expect_false("litres" %in% m2$manifestVars)
	expect_false("wt" %in% m2$manifestVars)

	# Trait covariance is the single latent–latent cell
	expect_true(isTRUE(m2$S$free["litres", "wt"]))
	expect_true(isTRUE(m2$S$free["wt", "litres"]))
	expect_equal(as.character(m2$S$labels["litres", "wt"]), as.character(m2$S$labels["wt", "litres"]))

	# No 4-way indicator residual covariances
	inds = c("litres_cont", "litres_cens", "wt_cont", "wt_cens")
	for (i in 1:length(inds)) {
		for (j in 1:length(inds)) {
			a = inds[i]
			b = inds[j]
			expect_false(isTRUE(m2$S$free[a, b]), info = paste0("S free ", a, "–", b))
			expect_equal(as.numeric(m2$S$values[a, b]), 0, info = paste0("S value ", a, "–", b))
		}
	}

	# Outcomes attach to latents, not to indicators
	expect_true(isTRUE(m2$A$free["mpg", "litres"]))
	expect_true(isTRUE(m2$A$free["mpg", "wt"]))
	expect_false(isTRUE(m2$A$free["mpg", "litres_cont"]))
	expect_false(isTRUE(m2$A$free["mpg", "litres_cens"]))
	expect_false(isTRUE(m2$A$free["mpg", "wt_cont"]))
	expect_false(isTRUE(m2$A$free["mpg", "wt_cens"]))
	expect_equal(as.numeric(m2$A$values["mpg", "litres_cont"]), 0)
	expect_equal(as.numeric(m2$A$values["mpg", "wt_cens"]), 0)

	pars = omxGetParameters(m2)
	expect_equal(sum(grepl("litres_with_wt|wt_with_litres", names(pars))), 1L)
	expect_false(any(grepl("litres_cont_with_wt", names(pars))))
	expect_false(any(grepl("litres_cens_with_wt", names(pars))))
	expect_false("litres_cont_to_mpg" %in% names(pars))
	expect_false("litres_cens_to_mpg" %in% names(pars))
	expect_true("litres_to_mpg" %in% names(pars))
	expect_true("wt_to_mpg" %in% names(pars))
})

test_that("T9 group= with fixed DE threshold applies τ@cut in every group", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	cutLitres = 2
	prep = umx_make_double_entry_data(tmp, cols = list(litres = cutLitres), sep = "", nSib = 1)

	mG = umxRAM_DE("T9g", data = prep, DEvars = "litres", group = "am",
		umxPath("litres", to = "mpg"),
		umxPath("wt", to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("litres", "wt", "mpg")),
		autoRun = FALSE, fixCensorThresholds = "yes"
	)
	expect_true(isTRUE(attr(mG, "umxDE")$fixedCensorThresholds))
	expect_equal(unname(attr(mG, "umxDE")$fixedCuts["litres_cens"]), cutLitres)
	expect_true(length(mG$submodels) >= 2L)
	subNames = names(mG$submodels)
	for (nm in subNames) {
		sm = mG$submodels[[nm]]
		expect_true("litres" %in% sm$latentVars, info = nm)
		expect_false(isTRUE(sm$deviations_for_thresh$free[1, "litres_cens"]), info = paste(nm, "τ free"))
		expect_equal(as.numeric(sm$deviations_for_thresh$values[1, "litres_cens"]), cutLitres, info = paste(nm, "τ value"))
		expect_equal(as.numeric(sm$A$values["litres_cont", "litres"]), 1)
		expect_equal(as.numeric(sm$S$values["litres_cens", "litres_cens"]), 0)
	}
})


