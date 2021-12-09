# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxBuild_run_modify.r") 
# test_package("umx")

context("umx Build_run_modify Functions")

test_that("umxEquate works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	# By default, umxEquate just equates master and slave labels: doesn't run model
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", name = "Eq x1 x2 loadings")
	
	# Set autoRun = TRUE and comparison = TRUE to run and output a comparison
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2")
	
	# rename the equated paths
	m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", newlabels = "equated", autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2")
	expect_true(parameters(m2)$name[1] == "equated")

})

test_that("umxFixAll works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	m2 = umxFixAll(m1, run = TRUE, verbose = TRUE)
	mxCompare(m1, m2)
})

test_that("umxSetParameters works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	parameters(m1)
	# Match all labels
	# Test run, showing all updated with an "m1_" in front
	umxSetParameters(m1, regex = "^", newlabels= "m1_", test = TRUE)
	# Change path to x1 to x2, equating these two paths
	m2 = umxSetParameters(m1, "G_to_x1", newlabels= "G_to_x2", test = FALSE)
	parameters(m2) 
	
	expect_equal("G_to_x2", namez(parameters(m2)$name, patt="G_to_x2"))
})

test_that("umxRAM works with xmuRAM2Ordinal", {
	require(umx)
	data(twinData)
	# Cut to form category of 20% obese subjects
	obesityLevels   = c('normal', 'obese')
	cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
	twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
	twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
	ordDVs = c("obese1", "obese2")
	twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
	mzData = twinData[twinData$zygosity %in% "MZFF",]
	m1 = umxRAM("tim", data = mzData, tryHard="yes",
			umxPath("bmi1", with = "bmi2"),
			umxPath(v.m.= c("bmi1", "bmi2"))
	)

	m2 = umxRAM("tim", data = mzData, tryHard="yes",
		umxPath("obese1", with = "obese2"),
		umxPath(v.m.= c("obese1", "obese2"))
	)
})
