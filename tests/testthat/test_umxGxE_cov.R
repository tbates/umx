library(testthat)
library(umx)

test_that("umxGxE with covariates", {
	require(umx)
	data(twinData) 
	twinData$age1 = twinData$age2 = twinData$age
	twinData$cohort1 = twinData$cohort2 = ifelse(twinData$cohort == "1960", 1, 2)
	
	selDVs  = "bmi"
	selDefs = "age"
	selCovs = "cohort"
	mzData  = subset(twinData, zygosity == "MZFF")[1:100,]
	dzData  = subset(twinData, zygosity == "DZFF")[1:100,]
	
	m1 = umxGxE(selDVs= "bmi", selDefs= "age", selCovs = "cohort", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
	
	# Verify that the model ran successfully and has output
	expect_true(m1$output$status$code == 0 || m1$output$status$code == 1)
	
	# Verify that the cohort_b_Var1 parameter exists in summary parameters
	params = summary(m1)$parameters
	expect_true("cohort_b_Var1" %in% params$name)
})
