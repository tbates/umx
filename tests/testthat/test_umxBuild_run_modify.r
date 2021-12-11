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

test_that("umx_check_model() works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_true(umx_check_model(m1)) # TRUE, this is a model
	expect_true(umx_check_model(m1, type = "RAM")) # equivalent to umx_is_RAM()
	expect_true(umx_check_model(m1, hasData = TRUE))
	
	expect_error(umx_check_model(m1, hasMeans = TRUE), regexp = "does not have means")
	# Model with no data
	m1 = umxRAM("x ~~ .3*y", autoRun = FALSE)
	expect_true(umx_check_model(m1, beenRun = FALSE))
	expect_true(umx_check_model(m1, hasData = FALSE))
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

test_that("umxGetParameters works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	# Show all parameters
	umxGetParameters(m1)
	umxGetParameters(m1, free = TRUE)  # Only free parameters
	umxGetParameters(m1, free = FALSE) # Only fixed parameters
	# Complex regex pattern
	umxGetParameters(m1, regex = "x[1-3]_with_x[2-5]", free = TRUE)
})

test_that("umxModify works", {
	# First we'll just build a 1-factor model
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	
	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	
	# 1. Drop the path to x1 (also updating the name so it's
	#    self-explanatory, and get a fit comparison
	m2 = umxModify(m1, update = "G_to_x1", name = "drop_X1", comparison = TRUE)
	
	# 2. Add the path back (setting free = TRUE)
	m2 = umxModify(m1, update = "G_to_x1", free= TRUE, name = "addback_X1", comparison = TRUE)
	# 3. Fix a value at a non-zero value
	m3 = umxModify(m1, update = "G_to_x1", value = .35, name = "fix_G_x1_at_35", comp = TRUE)
	# You can add objects to models. For instance this would add a path (overwriting the existing one)
	# (thanks Johannes!)
	m3 = umxModify(m1, umxPath("G", with = "x1"), name= "addedPath")
	
	# Use regular expression to drop multiple paths: e.g. G to x3, x4, x5
	m3 = umxModify(m1, regex = "^G_to_x[3-5]", name = "tried_hard", comp = TRUE, tryHard="yes")
	
	# Same, but don't autoRun
	m2 = umxModify(m1, regex  = "^G_to_x[3-5]", name = "no_G_to_x3_5", autoRun = FALSE) 
	
	# Re-write a label
	newLabel = "A_rose_by_any_other_name"
	newModelName = "model_doth_smell_as_sweet"
	m2 = umxModify(m1, update = "G_to_x1", newlabels= newLabel, name = newModelName, comparison = TRUE)
	# Change labels in 2 places
	labsToUpdate = c("G_to_x1", "G_to_x2")
	newLabel = "G_to_1_or_2"
	m2 = umxModify(m1, update = labsToUpdate, newlabels= newLabel, name = "equated", comparison = TRUE)
	
	# Advanced!
	# Regular expressions let you use pieces of the old names in creating new ones!
	searchString = "G_to_x([0-9])"
	newLabel = "loading_for_path\\1" # use value in regex group 1
	m2 = umxModify(m1, regex = searchString, newlabels= newLabel, name = "grep", comparison = TRUE)
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

