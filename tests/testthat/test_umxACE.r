# library(testthat)
# test_file("~/bin/umx/tests/testthat/test_umxACE.r") 
# test_package("umx")
library(umx)
context("twin models")

test_that("umxACE works", {
	data(twinData) # ?twinData set from Australian twins.
	mzData <- twinData[twinData$zygosity %in%  "MZFF", ]
	dzData <- twinData[twinData$zygosity %in% "DZFF", ]

	# Check no error with boundDiag
	m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag=0)
	expect_error({
		# TRUE is not legal
		m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = TRUE)		
	})

	# Do weight
	m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "")
	expect_lt(m1$output$Minus2LogLikelihood, 27287.24)

	# Do height
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "")
	expect_lt(m1$output$Minus2LogLikelihood, -11985.56)	
	# TODO Add more umxACE model tests
	# Add umxACEcov comparison test with lm-based solution
	# expect_gt()
	# expect_match(as.numeric(logLik(m1)))
})


dAIC      = c(2, 0, 4, 4, 12)
rel_LL    = exp(-.5 * dAIC)
sumRel_LL = sum(rel_LL)
rel_LL/sumRel_LL










