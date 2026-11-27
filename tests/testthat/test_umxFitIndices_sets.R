library(testthat)
library(umx)
library(OpenMx)

test_that("core indices match summary with the same reference models", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("core_ex", data = demoOneFactor[, v], autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(v.m. = v),
		umxPath(v1m0 = "G")
	)
	m1 = mxRun(m1, silent = TRUE)
	refs = umx:::xmu_mxRefModels(m1, run = TRUE, beginMessage = FALSE)
	s = summary(m1, refModels = refs)
	f = umxFitIndices(m1)
	expect_s3_class(f, "data.frame")
	expect_equal(nrow(f), 1)
	expect_equal(f$model, "core_ex")
	pairs = list(Chi = "Chi", ChiDoF = "ChiDoF", p = "p", RMSEA = "RMSEA",
		CFI = "CFI", TLI = "TLI", Minus2LogLikelihood = "Minus2LogLikelihood",
		degreesOfFreedom = "degreesOfFreedom", numObs = "numObs",
		AIC = "AIC.Mx", BIC = "BIC.Mx")
	for (col in names(pairs)) {
		expect_equal(as.numeric(f[[col]]), as.numeric(s[[pairs[[col]]]]))
	}
	expect_equal(as.numeric(f$RMSEA_lo), as.numeric(s$RMSEACI[1]))
	expect_equal(as.numeric(f$RMSEA_hi), as.numeric(s$RMSEACI[2]))
	expect_equal(f$N_eff, s$numObs)
})

test_that("defvar models get valid chi-square and count likelihood rows", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	mDef = umxRAM("def_ex", data = dd[, c(names(demoOneFactor)[1:3], "age")], autoRun = FALSE,
		umxPath("G", to = names(demoOneFactor)[1:3]),
		umxPath(v.m. = names(demoOneFactor)[1:3]),
		umxPath(v1m0 = "G"),
		umxPath(defn = "def", labels = "data.age"),
		umxPath("def", to = "x1")
	)
	mDef = mxRun(mDef, silent = TRUE)
	f = umxFitIndices(mDef)
	expect_true(f$Chi >= 0)
	# untruncated CFI can sit just above 1 when Chi < df
	expect_true(f$CFI <= 1.01)
	expect_equal(f$N_eff, f$numObs)
	# rows with missing defvars leave the likelihood
	dd2 = dd
	dd2$age[1:10] = NA
	mNA = mxModel(mDef, mxData(dd2, type = "raw"))
	expect_equal(xmu_eff_nobs(mNA), nrow(dd2) - 10)
})

test_that("set selects indices and rejects unknown names", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("set_ex", data = demoOneFactor[, v], autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(v.m. = v),
		umxPath(v1m0 = "G")
	)
	m1 = mxRun(m1, silent = TRUE)
	f = umxFitIndices(m1, set = c("RMSEA", "CFI"))
	expect_equal(names(f), c("model", "RMSEA", "CFI"))
	expect_error(umxFitIndices(m1, set = "RMESA"), "Unknown index names")
	fa = umxFitIndices(m1, set = "all")
	expect_true(all(c("SRMR", "GFI", "IC_deviance_AIC_par") %in% names(fa)))
})

test_that("lists of models give one row each", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("list_a", data = demoOneFactor[, v], autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(v.m. = v),
		umxPath(v1m0 = "G")
	)
	m1 = mxRun(m1, silent = TRUE)
	m2 = umxRAM("list_b", data = demoOneFactor[, v], type = "cov", autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	m2 = mxRun(m2, silent = TRUE)
	f = umxFitIndices(list(m1, m2))
	expect_equal(nrow(f), 2)
	expect_equal(f$model, c("list_a", "list_b"))
	expect_true(is.na(f$N_eff[2]))
})

test_that("multigroup core works, residual indices are NA", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(v.m. = v),
			umxPath(v1m0 = "G")
		)
	}
	mg = umxSuperModel("mg_ex", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]))
	fc = umxFitIndices(mg)
	expect_equal(nrow(fc), 1)
	expect_true(fc$Chi >= 0)
	expect_message(fa <- umxFitIndices(mg, set = "all"), "report NA")
	expect_true(is.na(fa$SRMR))
	expect_true(is.na(fa$GFI))
	expect_true(is.na(fa$MECVI))
	expect_false(is.na(fa$Chi))
})

test_that("ordinal definition-variable models refuse plainly", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	dd$x1 = ordered(cut(dd$x1, breaks = 3, labels = c("lo", "mid", "hi")))
	mOrd = umxRAM("ord_ex", data = dd, autoRun = FALSE,
		umxPath("G", to = "x1"),
		umxPath(v.m. = "x1"),
		umxPath(v1m0 = "G"),
		umxPath(defn = "def", labels = "data.age"),
		umxPath("def", to = "x1")
	)
	expect_error(umxFitIndices(mOrd), "cannot handle ordinal data")
})

test_that("print shows one row transposed and many rows as a table", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("print_a", data = demoOneFactor[, v], autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(v.m. = v),
		umxPath(v1m0 = "G")
	)
	m1 = mxRun(m1, silent = TRUE)
	m2 = umxRAM("print_b", data = demoOneFactor[, v], type = "cov", autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	m2 = mxRun(m2, silent = TRUE)
	f1 = umxFitIndices(m1)
	expect_output(print(f1), "Fit indices for")
	expect_output(print(f1), "RMSEA")
	f2 = umxFitIndices(list(m1, m2))
	expect_output(print(f2), "Fit indices")
})

test_that("SRMR and GFI match independent matrix formulas", {
	data(demoOneFactor)
	v = names(demoOneFactor)
	S = cov(demoOneFactor)
	m1 = umxRAM("formula_ex", data = mxData(S, type = "cov", numObs = 500), autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	m1 = mxRun(m1, silent = TRUE)
	f = umxFitIndices(m1, set = "all")
	E = mxGetExpected(m1, "covariance")
	O = m1$data$observed[colnames(E), colnames(E)]
	bent = (O - E) / sqrt(diag(O) %*% t(diag(O)))
	low = lower.tri(O, diag = TRUE)
	expect_equal(f$SRMR, sqrt(mean(bent[low]^2)))
	invSigS = solve(E) %*% O
	diffMat = invSigS - diag(length(v))
	gfi_exp = 1 - sum(diag(diffMat %*% diffMat)) / sum(diag(invSigS %*% invSigS))
	q = length(v) * (length(v) + 1) / 2
	expect_equal(f$GFI, gfi_exp)
	expect_equal(f$AGFI, 1 - (q / f$ChiDoF) * (1 - gfi_exp))
	expect_equal(f$PGFI, gfi_exp * f$ChiDoF / q)
	expect_true(f$SRMR < 0.01)
	expect_true(f$GFI > 0.98 && f$GFI < 1)
})

test_that("GFI, AGFI, PGFI, and residual indices match lavaan on misspecified and raw models", {
	skip_if_not_installed("lavaan")
	hs_vars = paste0("x", 1:9)
	hs_dat = lavaan::HolzingerSwineford1939[, hs_vars]
	S_hs = cov(hs_dat) * (nrow(hs_dat) - 1) / nrow(hs_dat)
	m_hs = umxRAM("hs_1f",
		data = mxData(S_hs, type = "cov", numObs = nrow(hs_dat)),
		umxPath("G", to = hs_vars),
		umxPath(var = hs_vars),
		umxPath(var = "G", fixedAt = 1),
		autoRun = FALSE
	)
	m_hs = mxRun(m_hs, silent = TRUE)
	f_hs = umxFitIndices(m_hs, set = c("RMR", "SRMR", "SRMR_mplus", "CRMR", "GFI", "AGFI", "PGFI"))
	lav_hs = lavaan::cfa(
		"G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9",
		sample.cov = S_hs, sample.nobs = nrow(hs_dat),
		sample.cov.rescale = FALSE, std.lv = TRUE
	)
	fm_hs = lavaan::fitMeasures(lav_hs)
	gfi_key = if ("gfi_lisrel" %in% names(fm_hs)) "gfi_lisrel" else "gfi"
	agfi_key = if ("agfi_lisrel" %in% names(fm_hs)) "agfi_lisrel" else "agfi"
	expect_equal(f_hs$RMR, unname(fm_hs["rmr"]), tolerance = 1e-3)
	expect_equal(f_hs$SRMR, unname(fm_hs["srmr_bentler"]), tolerance = 1e-3)
	expect_equal(f_hs$SRMR_mplus, unname(fm_hs["srmr_mplus"]), tolerance = 1e-3)
	expect_equal(f_hs$GFI, unname(fm_hs[gfi_key]), tolerance = 1e-3)
	expect_equal(f_hs$AGFI, unname(fm_hs[agfi_key]), tolerance = 1e-3)
	expect_equal(f_hs$PGFI, unname(fm_hs["pgfi"]), tolerance = 1e-3)

	# Raw data with non-zero mean residuals across all manifests (verifies full mean residual vector)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$x2 = dd$x2 + 1.0
	dd$x3 = dd$x3 - 1.5
	v = names(demoOneFactor)
	m_raw = umxRAM("raw_means_ex", data = dd, autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(means = v, fixedAt = 0),
		umxPath(v1m0 = "G")
	)
	m_raw = mxRun(m_raw, silent = TRUE)
	f_raw = umxFitIndices(m_raw, set = c("RMR", "SRMR", "SRMR_mplus", "CRMR"))
	E_raw = mxGetExpected(m_raw, "covariance")
	dims_raw = colnames(E_raw)
	O_raw = cov(dd[, dims_raw])
	obs_m = colMeans(dd[, dims_raw])
	est_m = as.numeric(mxGetExpected(m_raw, "means"))
	bollen_m = obs_m / sqrt(diag(O_raw)) - est_m / sqrt(diag(E_raw))
	bentler_m = (obs_m - est_m) / sqrt(diag(O_raw))
	expect_equal(f_raw$CRMR, sqrt(mean(c(OpenMx::vech(cov2cor(O_raw) - cov2cor(E_raw)), bollen_m)^2)))
	expect_equal(f_raw$SRMR, sqrt(mean(c(OpenMx::vech((O_raw - E_raw) / sqrt(diag(O_raw) %*% t(diag(O_raw)))), bentler_m)^2)))

	# Correlation-matrix data does not error in xmuResiduals
	m_cor = umxRAM("cor_ex",
		data = mxData(cor(demoOneFactor), type = "cor", numObs = 500),
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1),
		autoRun = FALSE
	)
	m_cor = suppressWarnings(mxRun(m_cor, silent = TRUE))
	f_cor = umxFitIndices(m_cor, set = c("CRMR", "GFI", "AGFI", "PGFI"))
	expect_true(is.finite(f_cor$CRMR) && f_cor$CRMR < 0.02)
	expect_true(is.finite(f_cor$GFI) && f_cor$GFI > 0.98 && f_cor$GFI < 1)
})
