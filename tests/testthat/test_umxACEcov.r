# Did I live?
# Did I love?
# Did I matter?

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
	selDVs  = c("bmi") # Set the DV
	selCovs = c("age") # Set the IV
	selVars = umx_paste_names(selDVs, covNames = selCovs, sep = "", suffixes = 1:2)
	mzData = subset(twinData, zygosity == "MZFF", selVars)
	dzData = subset(twinData, zygosity == "DZFF", selVars)
	# BMI, covarying for age in an ACE model
	m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, sep = "")
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
	m1 = umxACEcov(selDVs = "ht", selCovs = "age", dzData = dzData, mzData = mzData, sep = "")
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


dAIC      = c(2, 0, 4, 4, 12)
rel_LL    = exp(-.5 * dAIC)
sumRel_LL = sum(rel_LL)
rel_LL/sumRel_LL










