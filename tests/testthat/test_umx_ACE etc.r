# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_ACE etc.r") 
# 
# test_package("umx")

test_that("testing umx twin models", {

	# 1. Test sep enforcement
	require(umx)
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")
	selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
	m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
	# m2 = umxIPold(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
	m1 = umxIP(selDVs = selDVs, dzData = dzData, mzData = mzData)
	
	# Use "marginals" method to enable all continuous data with missingness.
	m3 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, type = "DWLS", allContinuousMethod='marginals')
	# omit missing to enable default WLS method to work on all continuous data
	dzD = na.omit(dzData[, tvars(selDVs, "_T")])
	mzD = na.omit(dzData[, tvars(selDVs, "_T")])
	m4 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzD, mzData = mzD, type = "DWLS")

	expect_warning(umx_lower2full(tmp, diag = TRUE), NA) # NA = no warning
	x = umx_lower2full(tmp, diag = TRUE)
	expect_true(isSymmetric(x), TRUE)

	
})
