library(umx)
library(testthat)

test_that("umxCP works", {
	# ========================================================
	# = Run a 3-factor Common pathway twin model of 6 traits =
	# ========================================================
	require(umx)
	umx_set_optimizer("SLSQP")
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")

	# These will be expanded into "gff_T1" "gff_T2" etc.
	selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
	m1 = umxCP("new", selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData, tryHard = "yes")

	# Shortcut using "data ="
	selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
	m1 = umxCP(selDVs = selDVs, nFac = 3, data=GFF, zyg="zyg_2grp")

	# ===================
	# = Do it using WLS =
	# ===================
	m2 = umxCP("new", selDVs = selDVs, sep = "_T", nFac = 3, optimizer = "SLSQP",
			dzData = dzData, mzData = mzData, tryHard = "ordinal", 
			type= "DWLS", allContinuousMethod='marginals'
	)

	# =================================================
	# = Find and test dropping of shared environment  =
	# =================================================
	# Show all labels for C parameters 
	umxParameters(m1, patt = "^c")
	# Test dropping the 9 specific and common-factor C paths
	m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC", comp = TRUE)
	umxSummaryCP(m2, comparison = m1, file = NA)
	umxCompare(m1, m2)

	# =======================================
	# = Mixed continuous and binary example =
	# =======================================
	data(GFF)
	# Cut to form umxFactor 20% depressed  DEP
	cutPoints = quantile(GFF[, "AD_T1"], probs = .2, na.rm = TRUE)
	ADLevels  = c('normal', 'depressed')
	GFF$DEP_T1 = cut(GFF$AD_T1, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
	GFF$DEP_T2 = cut(GFF$AD_T2, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
	ordDVs = c("DEP_T1", "DEP_T2")
	GFF[, ordDVs] = umxFactor(GFF[, ordDVs])

	# # These will be expanded into "gff_T1" "gff_T2" etc.
	selDVs = c("gff","fc","qol","hap","sat","DEP") 
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")

	# umx_set_optimizer("NPSOL")
	# umx_set_mvn_optimization_options("mvnRelEps", .01)
	m1 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
	m2 = umxModify(m1, regex = "(cs_r[3-5]|c_cp_r[12])", name = "dropC", comp= TRUE)

	# Do it using WLS
	m3 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData, tryHard = "ordinal", type= "DWLS")
	# TODO umxCPL fix WLS here
	# label at row 1 and column 1 of matrix 'top.binLabels'' in model 'CP3fac' : object 'Vtot'

	# Correlated factors example
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")
	selDVs = c("gff", "fc", "qol", "hap", "sat", "AD")
	m1 = umxCP("new", selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, nFac = 3, correlatedA = TRUE, tryHard = "yes")
})