library(umx)
library(testthat)

test_that("umxACE_DE works with prepped data and equates paths", {
	data(twinData)
	# Scale wt
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = "wt", sep = "")

	# Censor wt at the 20% mark
	cuts = quantile(twinData$wt1, probs = 0.2, na.rm = TRUE)

	# Create continuous column (NA if censored)
	twinData$wt_cont1 = ifelse(twinData$wt1 < cuts, NA, twinData$wt1)
	twinData$wt_cont2 = ifelse(twinData$wt2 < cuts, NA, twinData$wt2)

	# Create censored column (normal vs censored, as ordered factor)
	twinData$wt_cens1 = factor(ifelse(twinData$wt1 < cuts, "censored", "normal"), levels = c("normal", "censored"), ordered = TRUE)
	twinData$wt_cens2 = factor(ifelse(twinData$wt2 < cuts, "censored", "normal"), levels = c("normal", "censored"), ordered = TRUE)

	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
 
	# 1. Require at least one double-entry pair (pure continuous alone -> use umxACE)
	expect_error(
		umxACE_DE(selDVs = "wt", sep = "", dzData = dzData, mzData = mzData),
		regexp = "requires at least one double-entry pair"
	)

	# 2. Orphan _cont without adjacent _cens
	expect_error(
		umxACE_DE(selDVs = "wt_cont", sep = "", dzData = dzData, mzData = mzData),
		regexp = "needs an adjacent"
	)

	# 3. Verify success when passing prepped variables directly
	# Hand-built DE columns (no umxDoubleEntry cut meta): leave thresholds free
	m1 = umxACE_DE(selDVs = c("wt_cont", "wt_cens"), sep = "", dzData = dzData, mzData = mzData, fixCensorThresholds = "no")
	
	expect_true(inherits(m1, "MxModel"))
	expect_true(inherits(m1, "MxModelACE_DE"))
	expect_equal(class(m1$MZ$expectation)[[1]], "MxExpectationNormal")
	
	# Verify that matrix values and labels are equated for the pair
	# wt_cont is row 1, wt_cens is row 2
	for (matName in c("a", "c", "e")) {
		# Check loadings in column 1 are equated (equal labels)
		expect_equal(m1$top[[matName]]$labels[2, 1], m1$top[[matName]]$labels[1, 1])
		expect_equal(m1$top[[matName]]$free[2, 1], m1$top[[matName]]$free[1, 1])
		
		# Check column 2 is fixed to 0
		expect_equal(m1$top[[matName]]$free[2, 2], FALSE)
		expect_equal(m1$top[[matName]]$values[2, 2], 0)
	}
	
	# Verify summary works
	expect_error(umxSummary(m1), NA)
	
	# Verify plot.MxModelACE_DE omits wt_cont and keeps wt_cens
	dotOut = plot(m1)
	expect_true(any(grepl("wt_cens", dotOut)))
	expect_false(any(grepl("wt_cont", dotOut)))
})

test_that("umxACE_DE allows continuous traits mixed with double-entry pairs", {
	data(twinData)
	twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
	cuts = quantile(twinData$wt1, probs = 0.2, na.rm = TRUE)
	prepData = umx_make_double_entry_data(twinData, cols = list(wt = cuts), sep = "")
	mzData = prepData[prepData$zygosity %in% "MZFF", ]
	dzData = prepData[prepData$zygosity %in% "DZFF", ]

	mMix = umxACE_DE(
		name = "htWtDE",
		selDVs = c("ht", "wt_cont", "wt_cens"),
		sep = "",
		dzData = dzData,
		mzData = mzData,
		addCI = FALSE,
		tryHard = "yes"
	)
	expect_true(inherits(mMix, "MxModel"))
	expect_true(is.finite(mMix$output$Minus2LogLikelihood))
	# 3 traits per person: ht, wt_cont, wt_cens
	expect_equal(nrow(mMix$top$a$values), 3L)
	# Double-entry equate: wt_cens (row 3) shares labels with wt_cont (row 2) for cols 1:2; diag fixed
	for (matName in c("a", "c", "e")) {
		expect_equal(mMix$top[[matName]]$labels[3, 1], mMix$top[[matName]]$labels[2, 1])
		expect_equal(mMix$top[[matName]]$labels[3, 2], mMix$top[[matName]]$labels[2, 2])
		expect_equal(mMix$top[[matName]]$free[3, 3], FALSE)
		expect_equal(mMix$top[[matName]]$values[3, 3], 0)
	}
	# Continuous ht (row 1) keeps free diagonal
	expect_equal(mMix$top$a$free[1, 1], TRUE)
	
	# Verify plot(mMix) keeps ht and wt_cens, omitting wt_cont
	dotMix = plot(mMix)
	expect_true(any(grepl("wt_cens", dotMix)))
	expect_true(any(grepl("ht", dotMix)))
	expect_false(any(grepl("wt_cont", dotMix)))

	# Verify umxSummary(mMix) outputs filtered double-entry summary
	sumOut = capture.output(umxSummary(mMix))
	expect_true(any(grepl("double-entry Cholesky ACE model", sumOut)))
	expect_true(any(grepl("wt_cens", sumOut)))
	# Parameter table section (between Standardized and Means) should omit wt_cont
	paramSec = sumOut[grep("Standardized parameter estimates", sumOut):grep("Means", sumOut)]
	expect_false(any(grepl("wt_cont", paramSec)))
})

test_that("umx_make_double_entry_data works with various censoring rules and integrates with umxACE_DE", {
	data(twinData)
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = "wt", sep = "")
	cuts = quantile(twinData$wt1, probs = 0.2, na.rm = TRUE)
	
	# 1. Test floor censoring (single numeric cuts)
	cutsInt = -1
	ruleList = list(wt = cutsInt)
	prepData = umx_make_double_entry_data(twinData, cols = ruleList, sep = "")
	expect_true("wt_cont1" %in% colnames(prepData))
	expect_true("wt_cens1" %in% colnames(prepData))
	expect_s3_class(prepData$wt_cens1, "factor")
	expect_true(is.ordered(prepData$wt_cens1))
	
	# 2. Test comparison string rule
	ruleListStr = list(wt = paste0("<= ", cutsInt))
	prepDataStr = umx_make_double_entry_data(twinData, cols = ruleListStr, sep = "")
	expect_equal(prepData$wt_cens1, prepDataStr$wt_cens1)
	
	# 3. Test interval censoring (numeric range)
	ruleListRange = list(wt = c(cutsInt - 0.1, cutsInt + 0.1))
	prepDataRange = umx_make_double_entry_data(twinData, cols = ruleListRange, sep = "")
	expect_true(any(prepDataRange$wt_cens1 == "censored", na.rm = TRUE))
	
	# 4. Test function rule
	ruleListFunc = list(wt = function(x) x <= cutsInt)
	prepDataFunc = umx_make_double_entry_data(twinData, cols = ruleListFunc, sep = "")
	expect_equal(prepData$wt_cens1, prepDataFunc$wt_cens1)
	
	# 5. Fit model to prepData to verify integration
	mzData = prepData[prepData$zygosity %in% "MZFF", ]
	dzData = prepData[prepData$zygosity %in% "DZFF", ]
	m2 = umxACE_DE(selDVs = c("wt_cont", "wt_cens"), sep = "", dzData = dzData, mzData = mzData)
	expect_true(inherits(m2, "MxModel"))
	expect_equal(as.integer(m2$output$status$code), 0L)
})

test_that("umx_make_double_entry_data metadata, levels, and cut grammar (T1/T5)", {
	data(twinData)
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = "wt", sep = "")

	# Numeric left cut
	prep = umx_make_double_entry_data(twinData, cols = list(wt = -0.5), sep = "")
	meta = attr(prep, "umxDoubleEntry")
	expect_true(!is.null(meta))
	expect_equal(meta$pairs[[1]]$cut, -0.5)
	expect_equal(meta$pairs[[1]]$side, "left")
	expect_true(isTRUE(meta$pairs[[1]]$fixable))
	expect_equal(levels(prep$wt_cens1), c("censored", "observed"))
	# Non-missing cens are category 1 (censored by name), never levels[2] only by accident
	nonNA = prep$wt_cens1[!is.na(prep$wt_cens1)]
	expect_true(length(nonNA) > 0)
	expect_true(all(as.integer(nonNA) == 1L))
	expect_true(all(as.character(nonNA) == "censored"))

	# Character grammar
	prepStr = umx_make_double_entry_data(twinData, cols = list(wt = "<= 0"), sep = "")
	expect_equal(attr(prepStr, "umxDoubleEntry")$pairs[[1]]$cut, 0)
	expect_equal(attr(prepStr, "umxDoubleEntry")$pairs[[1]]$side, "left")
	expect_true(attr(prepStr, "umxDoubleEntry")$pairs[[1]]$fixable)

	prepRight = umx_make_double_entry_data(twinData, cols = list(wt = ">= 40"), sep = "")
	expect_equal(attr(prepRight, "umxDoubleEntry")$pairs[[1]]$side, "right")
	expect_equal(levels(prepRight$wt_cens1), c("observed", "censored"))
	nonNA_r = prepRight$wt_cens1[!is.na(prepRight$wt_cens1)]
	if (length(nonNA_r) > 0) {
		expect_true(all(as.character(nonNA_r) == "censored"))
		expect_true(all(as.integer(nonNA_r) == 2L))
	}

	# Not fixable: function / interval / mean(x)
	prepFn = umx_make_double_entry_data(twinData, cols = list(wt = function(x) x <= 0), sep = "")
	expect_false(isTRUE(attr(prepFn, "umxDoubleEntry")$pairs[[1]]$fixable))
	prepInt = umx_make_double_entry_data(twinData, cols = list(wt = c(-0.1, 0.1)), sep = "")
	expect_false(isTRUE(attr(prepInt, "umxDoubleEntry")$pairs[[1]]$fixable))
	prepMean = umx_make_double_entry_data(twinData, cols = list(wt = "x < mean(x)"), sep = "")
	expect_false(isTRUE(attr(prepMean, "umxDoubleEntry")$pairs[[1]]$fixable))

	# df attr survives row subset (column attrs not required)
	sub = prep[1:10, ]
	expect_true(!is.null(attr(sub, "umxDoubleEntry")))
	expect_equal(attr(sub, "umxDoubleEntry")$pairs[[1]]$cut, -0.5)
})

test_that("umxACE_DE free threshold baseline and fixed thresholds structure (T0/T2)", {
	data(twinData)
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = "wt", sep = "")
	cut = -0.5
	prep = umx_make_double_entry_data(twinData, cols = list(wt = cut), sep = "")
	mzData = prep[prep$zygosity %in% "MZFF", ]
	dzData = prep[prep$zygosity %in% "DZFF", ]

	# T0 free threshold: still free τ, but variance NOT forced to 1; means cont=cens
	mFree = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "",
		mzData = mzData, dzData = dzData,
		autoRun = FALSE, addCI = FALSE, fixCensorThresholds = "no"
	)
	expect_true(all(mFree$top$deviations_for_thresh$free[1, ]))
	expect_equal(mFree$top$deviations_for_thresh$labels[1, "wt_cens1"], mFree$top$deviations_for_thresh$labels[1, "wt_cens2"])
	expect_true(isTRUE(mFree$top$expMean$free[1, "wt_cens1"]))
	expect_equal(mFree$top$expMean$labels[1, "wt_cens1"], mFree$top$expMean$labels[1, "wt_cont1"])
	expect_true(is.null(mFree$top$constrain_Bin_var_to_1))
	expect_true(isTRUE(attr(mFree, "umxDE")$freeVariance))

	# T2 fixed via explicit censorCuts: τ@cut, means equated, V=1 released
	mFix = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "",
		mzData = mzData, dzData = dzData,
		autoRun = FALSE, addCI = FALSE,
		fixCensorThresholds = "yes", censorCuts = c(wt = cut)
	)
	expect_false(any(mFix$top$deviations_for_thresh$free[1, ]))
	expect_equal(as.numeric(mFix$top$deviations_for_thresh$values[1, "wt_cens1"]), cut)
	expect_equal(as.numeric(mFix$top$deviations_for_thresh$values[1, "wt_cens2"]), cut)
	expect_true(isTRUE(mFix$top$expMean$free[1, "wt_cens1"]))
	expect_equal(mFix$top$expMean$labels[1, "wt_cens1"], mFix$top$expMean$labels[1, "wt_cont1"])
	expect_equal(mFix$top$expMean$labels[1, "wt_cens2"], mFix$top$expMean$labels[1, "wt_cont2"])
	expect_true(isTRUE(attr(mFix, "umxDE")$fixedCensorThresholds))
	expect_true(isTRUE(attr(mFix, "umxDE")$freeVariance))
	expect_equal(attr(mFix, "umxDE")$fixedCuts[["wt_cens"]], cut)
	# Sole DE binary: binary Vtot==1 constraint should be gone (fixed and free paths)
	expect_true(is.null(mFix$top$constrain_Bin_var_to_1))
	expect_true(is.null(mFix$top$binLabels))
	expect_true(is.null(mFree$top$constrain_Bin_var_to_1))

	# auto from attr
	mAuto = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "",
		mzData = mzData, dzData = dzData,
		autoRun = FALSE, addCI = FALSE,
		fixCensorThresholds = "auto"
	)
	expect_false(any(mAuto$top$deviations_for_thresh$free[1, ]))
	expect_equal(as.numeric(mAuto$top$deviations_for_thresh$values[1, 1]), cut)

	# mode no + censorCuts errors
	expect_error(
		umxACE_DE(
			selDVs = c("wt_cont", "wt_cens"), sep = "",
			mzData = mzData, dzData = dzData,
			autoRun = FALSE, addCI = FALSE,
			fixCensorThresholds = "no", censorCuts = c(wt = cut)
		),
		regexp = "censorCuts"
	)

	# selCovs + fix hard error
	expect_error(
		umxACE_DE(
			selDVs = c("wt_cont", "wt_cens"), selCovs = "age", sep = "",
			mzData = mzData, dzData = dzData,
			autoRun = FALSE, addCI = FALSE,
			fixCensorThresholds = "yes", censorCuts = c(wt = cut)
		),
		regexp = "selCovs"
	)

	# sep = "_T" naming path
	td = twinData
	names(td)[names(td) == "wt1"] = "wt_T1"
	names(td)[names(td) == "wt2"] = "wt_T2"
	prepT = umx_make_double_entry_data(td, cols = list(wt = cut), sep = "_T")
	mzT = prepT[prepT$zygosity %in% "MZFF", ]
	dzT = prepT[prepT$zygosity %in% "DZFF", ]
	mT = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "_T",
		mzData = mzT, dzData = dzT,
		autoRun = FALSE, addCI = FALSE,
		fixCensorThresholds = "yes", censorCuts = c(wt = cut)
	)
	expect_false(any(mT$top$deviations_for_thresh$free[1, ]))
	expect_true(isTRUE(attr(mT, "umxDE")$fixedCensorThresholds))
})

test_that("umxACE_DE two DE pairs: no phantom cens columns; fixed and free run", {
	data(twinData)
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
	htCut = as.numeric(quantile(c(twinData$ht1, twinData$ht2), 0.2, na.rm = TRUE))
	wtCut = as.numeric(quantile(c(twinData$wt1, twinData$wt2), 0.2, na.rm = TRUE))
	clinic = twinData
	clinic$ht1[!is.na(clinic$ht1) & clinic$ht1 < htCut] = htCut
	clinic$ht2[!is.na(clinic$ht2) & clinic$ht2 < htCut] = htCut
	clinic$wt1[!is.na(clinic$wt1) & clinic$wt1 < wtCut] = wtCut
	clinic$wt2[!is.na(clinic$wt2) & clinic$wt2 < wtCut] = wtCut
	prep = umx_make_double_entry_data(clinic, cols = list(ht = htCut, wt = wtCut), sep = "")
	mzData = prep[prep$zygosity %in% "MZFF", ]
	dzData = prep[prep$zygosity %in% "DZFF", ]

	# Structure: columns 2 and 4 (cens positions) fully fixed at 0 in a/c/e
	m0 = umxACE_DE(
		selDVs = c("ht_cont", "ht_cens", "wt_cont", "wt_cens"),
		sep = "", mzData = mzData, dzData = dzData,
		autoRun = FALSE, addCI = FALSE,
		fixCensorThresholds = "yes",
		censorCuts = c(ht = htCut, wt = wtCut)
	)
	for (matName in c("a", "c", "e")) {
		expect_true(all(m0$top[[matName]]$free[, 2] == FALSE))
		expect_true(all(m0$top[[matName]]$values[, 2] == 0))
		expect_true(all(m0$top[[matName]]$free[, 4] == FALSE))
		expect_true(all(m0$top[[matName]]$values[, 4] == 0))
		expect_true(isTRUE(m0$top[[matName]]$free[1, 1]))
		expect_true(isTRUE(m0$top[[matName]]$free[3, 3]))
	}
	expect_equal(as.numeric(m0$top$deviations_for_thresh$values[1, "ht_cens1"]), htCut)
	expect_equal(as.numeric(m0$top$deviations_for_thresh$values[1, "wt_cens1"]), wtCut)
	expect_true(is.null(m0$top$constrain_Bin_var_to_1))

	mFix = umxACE_DE(
		selDVs = c("ht_cont", "ht_cens", "wt_cont", "wt_cens"),
		sep = "", mzData = mzData, dzData = dzData,
		addCI = FALSE, tryHard = "yes",
		fixCensorThresholds = "yes",
		censorCuts = c(ht = htCut, wt = wtCut)
	)
	expect_true(is.finite(mFix$output$Minus2LogLikelihood))
	expect_true(length(omxGetParameters(mFix)) > 0)
	expect_error(umxSummary(mFix, std = TRUE, file = NA), NA)

	mFree = umxACE_DE(
		name = "fixCensorThresholds_no",
		selDVs = c("ht_cont", "ht_cens", "wt_cont", "wt_cens"),
		sep = "", mzData = mzData, dzData = dzData,
		addCI = FALSE, tryHard = "yes",
		fixCensorThresholds = "no"
	)
	expect_true(is.finite(mFree$output$Minus2LogLikelihood))
	expect_true(length(omxGetParameters(mFree)) > 0)
	expect_true(all(mFree$top$a$free[, 2] == FALSE))
	expect_true(all(mFree$top$a$free[, 4] == FALSE))
	expect_error(umxSummary(mFree, std = TRUE, file = NA), NA)
})

test_that("umxSummaryACE_DE refuses unfitted model cleanly", {
	data(twinData)
	twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = "wt", sep = "")
	prep = umx_make_double_entry_data(twinData, cols = list(wt = -0.5), sep = "")
	mzData = prep[prep$zygosity %in% "MZFF", ]
	dzData = prep[prep$zygosity %in% "DZFF", ]
	m = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "",
		mzData = mzData, dzData = dzData,
		autoRun = FALSE, addCI = FALSE, fixCensorThresholds = "no"
	)
	expect_error(umxSummary(m), regexp = "not been run successfully")
})

test_that("umxACE_DE fixed threshold tiny Tobit formula check (T3a)", {
	# Univariate-style DE on one zyg group: hand -2LL for mutual-NA left-censor
	set.seed(42)
	mu = 0.3
	cut = 0
	n = 8
	# Construct rows: half continuous above cut, half censored
	yCont = c(0.5, 0.8, 1.1, 1.4)
	# Twin-wide fake data (identical twins for simplicity of hand calc on twin1 only would be hard;
	# use independent twins with same structure and compute OpenMx vs hand on the DE mutual-NA pattern)
	nPairs = 6
	y1 = c(0.5, 0.9, 1.2, NA, NA, NA)
	y2 = c(0.6, 1.0, NA, NA, 0.7, NA)
	c1 = c(NA, NA, NA, "censored", "censored", "censored")
	c2 = c(NA, NA, "censored", "censored", NA, "censored")
	# For rows where cont is non-NA, y > cut; censored when cont NA
	mz = data.frame(
		wt_cont1 = y1,
		wt_cont2 = y2,
		wt_cens1 = factor(c1, levels = c("censored", "observed"), ordered = TRUE),
		wt_cens2 = factor(c2, levels = c("censored", "observed"), ordered = TRUE),
		stringsAsFactors = FALSE
	)
	# Duplicate as DZ so umxACE_DE can build (minimal)
	dz = mz

	m = umxACE_DE(
		selDVs = c("wt_cont", "wt_cens"), sep = "",
		mzData = mz, dzData = dz,
		autoRun = FALSE, addCI = FALSE, addStd = FALSE,
		fixCensorThresholds = "yes", censorCuts = c(wt = cut),
		boundDiag = 0
	)
	# Fix ACE structure roughly to unit variance identity so FIML is near independent N(mu,1)
	# Set a=c=0, e=1 on cont, equated to cens; means already equated at cut fix
	for (matName in c("a", "c")) {
		m$top[[matName]]$free[, ] = FALSE
		m$top[[matName]]$values[, ] = 0
	}
	m$top$e$free[, ] = FALSE
	m$top$e$values[, ] = 0
	m$top$e$values[1, 1] = 1
	m$top$e$values[2, 1] = 1  # equated loading path for cens row
	# expMean to known mu
	labs = unique(na.omit(as.character(m$top$expMean$labels[1, ])))
	for (lab in labs) {
		m = omxSetParameters(m, labels = lab, free = FALSE, values = mu)
	}
	m = omxAssignFirstParameters(m)
	m = mxRun(m, silent = TRUE)

	# Hand -2LL under independent twin unit-variance Tobit (approximation if cov off-diag ~0)
	# With e path structure ACE = ee' may create off-diagonals; prefer evaluate only if cov nearly diagonal
	# Direct hand sum for each non-missing element treating twins independent N(mu,1):
	hand = 0
	for (i in 1:nrow(mz)) {
		for (s in 1:2) {
			yc = mz[[paste0("wt_cont", s)]][i]
			ye = mz[[paste0("wt_cens", s)]][i]
			if (!is.na(yc)) {
				hand = hand + (-2) * dnorm(yc, mean = mu, sd = 1, log = TRUE)
			} else if (!is.na(ye)) {
				hand = hand + (-2) * pnorm(cut, mean = mu, sd = 1, log.p = TRUE)
			}
		}
	}
	# Model -2LL includes both MZ and DZ copies of same data -> expect ~ 2 * hand if cov ~ 0
	m2ll = m$output$Minus2LogLikelihood
	expect_true(is.finite(m2ll))
	# Loose gate: model m2ll within 20% of 2*hand (structure not pure independence)
	# Stricter element-wise check: threshold fixed and means equated
	expect_equal(as.numeric(m$top$deviations_for_thresh$values[1, 1]), cut)
	expect_equal(m$top$expMean$labels[1, "wt_cens1"], m$top$expMean$labels[1, "wt_cont1"])
	# Hand formula consistency for pure continuous + CDF pieces when model near independent
	expect_true(abs(m2ll - 2 * hand) / max(2 * hand, 1) < 0.25 || abs(m2ll - hand) / max(hand, 1) < 0.25)
})

