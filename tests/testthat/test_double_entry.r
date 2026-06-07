library(testthat)
library(umx)
library(OpenMx)

context("umxACE_DE tests")

test_that("umxACE_DE works with automatic suffix detection and equates paths", {
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
 
	m1 = umxACE_DE(selDVs = "wt", sep = "", dzData = dzData, mzData = mzData)
	
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
