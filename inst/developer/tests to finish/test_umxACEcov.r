# library(testthat)
# test_file("~/bin/umx/tests/testthat/test_umxACE.r") 
# test_package("umx")
library(umx)
context("twin models")

test_that("umxACEcov works", {
	require(umx)
	data(twinData)
	# Replicate age to age1 & age2
	twinData$age1 = twinData$age2 = twinData$age
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")
	# BMI, covarying for age in an ACE model

	# As of 2017-04-27 giving far too high estimate for C?
	m1 = umxACEcov(selDVs = "bmi", selCovs = "age", dzData = dzData, mzData = mzData, sep = "")
	# 'log Lik.' -18277.81 (df=8)
	# |    |   a1|   c1|   e1|
	# |:---|----:|----:|----:|
	# |bmi | 0.71| 0.48| 0.52|
	m2 = umxACE(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
	# 'log Lik.' 9659.215 (df=4)
	# |     |   a1|c1 |   e1|
	# |:----|----:|:--|----:|
	# |bmi1 | 0.86|.  | 0.51|

	umxSummary(m1)
	plot(m1)

	expect_error({
		# no covs specified
		m1 = umxACEcov(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
	})

	# Do weight
	mzData = subset(twinData, zygosity == "MZFF") # Include all columns
	dzData = subset(twinData, zygosity == "DZFF")
	m1 = umxACEcov(selDVs = "wt", selCovs = "age", dzData = dzData, mzData = mzData, sep = "")
	umxSummary(m1)
	expect_lt(m1$output$Minus2LogLikelihood, -11471.91)

	# Do height
	m1 = umxACEcov(selDVs = "ht", selCovs = "age", dzData = dzData, mzData = mzData, sep = "", boundDiag = 0)
	umxSummary(m1)
	expect_lt(m1$output$Minus2LogLikelihood, 264563.4)	
	# 'log Lik.' -11985.57 (df=4)
	# Standardized solution
	#
	#
	# |    |   a1|   c1|   e1|
	# |:---|----:|----:|----:|
	# |ht1 | 0.92| 0.14| 0.36|
	
	# expect_gt()
	# expect_match(as.numeric(logLik(m1)))
})

# Did I live?
# Did I love?
# Did I matter?