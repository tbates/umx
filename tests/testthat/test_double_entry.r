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
	m1 = umxACE_DE(selDVs = c("wt_cont", "wt_cens"), sep = "", dzData = dzData, mzData = mzData)
	
	expect_true(inherits(m1, "MxModel"))
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

