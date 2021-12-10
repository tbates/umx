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

test_that("umxEFA works", {
	require(umx)
	myVars = c("mpg", "disp", "hp", "wt", "qsec")
	m1 = umxEFA(name = "test", factors = 2, data = mtcars[, myVars])
	loadings(m1)
})

test_that("umxSummary works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	umxSummary(m1, std = TRUE)
	# output as latex
	umx_set_table_format("latex")
	umxSummary(m1, std = TRUE)
	umx_set_table_format("markdown")
	# output as raw
	umxSummary(m1, std = FALSE)
	# switch to a raw data model
	m1 = umxRAM("One Factor", data = demoOneFactor[1:100, ],
		umxPath("G", to = manifests),
		umxPath(v.m. = manifests),
		umxPath(v1m0 = "G")
	)
	umxSummary(m1, std = TRUE, filter = "NS")
})

test_that("umxSummaryACE works", {
	require(umx)
	data(twinData)
	selDVs = c("bmi1", "bmi2")
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")
	m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
	umxSummary(m1)
	umxSummaryACE(m1, file = NA);
	umxSummaryACE(m1, file = "name", std = TRUE)
	stdFit = umxSummaryACE(m1, returnStd = TRUE);
})

test_that("umxSummaryGxE works", {
	require(umx)
	data(twinData) 
	twinData$age1 = twinData$age2 = twinData$age
	selDVs  = c("bmi1", "bmi2")
	selDefs = c("age1", "age2")
	selVars = c(selDVs, selDefs)
	mzData  = subset(twinData, zygosity == "MZFF", selVars)
	dzData  = subset(twinData, zygosity == "DZMM", selVars)
	# Exclude cases with missing Def
	mzData = mzData[!is.na(mzData[selDefs[1]]) & !is.na(mzData[selDefs[2]]),]
	dzData = dzData[!is.na(dzData[selDefs[1]]) & !is.na(dzData[selDefs[2]]),]
	m1 = umxGxE(selDVs = "bmi", selDefs = "age", sep="", dzData = dzData, mzData = mzData)
	# Plot Moderation
	umxSummary(m1)
	umxSummaryGxE(m1, location = "topright")
	umxSummaryGxE(m1, separateGraphs = FALSE)
})

test_that("umxSummaryIP works", {
	require(umx)
	data(GFF) # family function and well-being data
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")
	selDVs = c("hap", "sat", "AD") # These will be expanded into "hap_T1" "hap_T2" etc.
	m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
	umxSummaryIP(m1)
	plot(m1)
	umxSummaryIP(m1, digits = 2, file = "Figure3", showRg = FALSE, CIs = TRUE);
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