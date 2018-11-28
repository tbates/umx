# library(testthat)
# test_file("~/bin/umx/tests/testthat/test_umxACE.r") 
# test_package("umx")
library(umx)
context("twin models")

test_that("umxGXE works", {
	data(twinData) # ?twinData set from Australian twins.
	mzData <- twinData[twinData$zygosity %in%  "MZFF", ]
	dzData <- twinData[twinData$zygosity %in% "DZFF", ]

	# Do weight
	m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "")
	# m1$output$Minus2LogLikelihood
	# Check -2ll low enough
	expect_lt(-2*logLik(m1), 27287.24)

	# Check no error with boundDiag
	m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag=0)

	# Can't pass TRUE to boundDiag
	expect_error({
		# TRUE is not legal
		m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = TRUE)		
	})

	# Do height
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "")
	# Check -2ll low enough
	expect_lt(-2*logLik(m1), -11985.56)	

	# Check across optimizers
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "", opt= "NPSOL"); expect_lt(-2*logLik(m1), -11985.56)	
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "", opt= "SLSQP"); expect_lt(-2*logLik(m1), -11985.56)	

	# CSOLNP used to code 6...
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "", opt= "CSOLNP"); expect_lt(-2*logLik(m1), -11985.56)	

	# ========================================================
	# = Evidence for dominance ? (DZ correlation set to .25) =
	# ========================================================
	m2 = umxACE("ADE", selDVs = selDVs, dzData = dzData, mzData = mzData, dzCr = .25)
	umxCompare(m2, m1) # ADE is better
	umxSummary(m2, comparison = m1) # nb: though this is ADE, columns are labeled ACE
	# TODO Add more umxACE model tests
	# Add umxACEcov comparison test with lm-based solution
	# expect_gt()
	# expect_match(as.numeric(logLik(m1)))
})


