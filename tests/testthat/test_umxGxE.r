library(testthat)
library(umx)

test_that("umxGxE", {
	require(umx)
	data(twinData) 
	twinData$age1 = twinData$age2 = twinData$age
	selDVs  = "bmi"
	selDefs = "age"
	mzData  = subset(twinData, zygosity == "MZFF")[1:100,]
	dzData  = subset(twinData, zygosity == "DZFF")[1:100,]
	m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
	
	# Select the data on the fly with data= and zygosity levels
	m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep="", dzData= "DZFF", mzData= "MZFF", data= twinData)
})

test_that("umxGxE ordinal", {
	require(umx)
	data(twinData)
	twinData$age1 = twinData$age2 = twinData$age

	# Cut to form binary DV (2 levels)
	obLevels = c('normal', 'obese')
	cuts = quantile(twinData[, "bmi1"], probs = 0.5, na.rm = TRUE)
	twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cuts, Inf), labels = obLevels)
	twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cuts, Inf), labels = obLevels)
	ordDVs = c("obese1", "obese2")
	twinData[, ordDVs] = umxFactor(twinData[, ordDVs])

	mzData = subset(twinData, zygosity == "MZFF")[1:100, ]
	dzData = subset(twinData, zygosity == "DZFF")[1:100, ]

	m1 = umxGxE(selDVs = "obese", selDefs = "age", sep = "", dzData = dzData, mzData = mzData, tryHard = "yes")
	expect_true(m1$output$status$code == 0 || m1$output$status$code == 1)

	# Test reduction
	m2 = umxReduce(m1)
	expect_true(!is.null(m2))
})


test_that("umxGxE_window", {
	require(umx)
	data(twinData) # Dataset of Australian twins, built into OpenMx
	twinData = twinData[!is.na(twinData["age"]), ]
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")

	m1 = umxGxE_window(selDVs = "bmi", moderator = "age", sep= "", mzData = mzData, dzData = dzData, target = 40, plotWindow = TRUE)
	m1 = umxGxE_window(selDVs = "bmi", moderator = "age", sep= "", mzData = mzData, dzData = dzData, plotWindow = TRUE, tryHard = "yes")

})
