# library(testthat)
# test_file("~/bin/umx/tests/testthat/test_umxACE.r") 
# 
# test_package("umx")
library(umx)
context("twin models")

test_that("umxACE works", {
	data(twinData) # ?twinData set from Australian twins.
	mzData <- twinData[twinData$zygosity %in%  "MZFF", ]
	dzData <- twinData[twinData$zygosity %in% "DZFF", ]
	# no error with boundDiag
	m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag=0)
	expect_error({
		# TRUE is not legal
		m1 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag=TRUE)		
	})
	m1 = umxACE(selDVs = "ht", dzData = dzData, mzData = mzData, sep = "")
	expect_lt(as.numeric(logLik(m1)), 5992.784)	
	# expect_gt()
	# expect_match(as.numeric(logLik(m1)))
})
