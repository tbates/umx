# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxFitAndReporting.r") 
# test_package("umx")

context("umxFitAndReporting_Functions")

test_that("umxCompare works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
	umxCompare(m1, m2)
	umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
	umxCompare(m1, m2, report = "html") # Open table in browser

	# Two comparison models
	m3 = umxModify(m2, update = "G_to_x3", name = "drop_path_2_x2_and_3")

	umxCompare(m1, c(m2, m3))
	umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE)
	umxCompare(c(m1, m2), c(m2, m3), all = TRUE)

	# WLS not working for umxSummary or umxCompare
	# manifests = names(demoOneFactor)
	# 	m1 = umxRAM("WLS", data = demoOneFactor, type = "DWLS",
	# 		umxPath("G", to = manifests),
	# 		umxPath(var = manifests),
	# 		umxPath(var = "G", fixedAt = 1)
	# 	)
	#
	# 	m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
	# 	umxCompare(m1, m2)
	# 	umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
	# 	umxCompare(m1, m2, report = "html") # Open table in browser
	# })
})

test_that("umxDiagnose works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	umxDiagnose(m1)
})

test_that("umxExpCov works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	vcov(m1) # supplied by OpenMx
	umxExpCov(m1, digits = 3)
})

test_that("umxParameters works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)

	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	umxParameters(m1, "below", .1)
	# Parameters with values above .5
	umxParameters(m1, "above", .5)
	# Parameters with values below .1 and containing "_to_" in their label
	umxParameters(m1, "below", .1, "_to_")
})

test_that("umxExpMeans works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	
	m1 = umxRAM("One Factor", data = demoOneFactor,
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	
	umxExpMeans(m1)
	umxExpMeans(m1, digits = 3)
})

test_that("umxMI works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	
	m1 = umxRAM("One Factor", data = demoOneFactor,
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	
	umxMI(m1, full=FALSE)
})

test_that("RMSEA works", {
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	
	m1 = umxRAM("One Factor", data = demoOneFactor, type= "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1.0)
	)
	tmp = summary(m1)
	RMSEA(tmp)
})