library(umx)
library(testthat)

test_that("umxCP works", {
	# ========================================================
	# = Run a 3-factor Common pathway twin model of 6 traits =
	# ========================================================
	require(umx)
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

test_that("umxPlotCP with CIs works", {
	require(umx)
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")[1:60, ]
	dzData = subset(GFF, zyg_2grp == "DZ")[1:60, ]
	selDVs = c("gff", "fc", "qol")
	m1 = umxCP("new", selDVs = selDVs, sep = "_T", nFac = 1, dzData = dzData, mzData = mzData, addCI = TRUE, autoRun = FALSE)
	m1 = umxRun(m1)

	# Mock some CIs
	CIlist = m1@output$confidenceIntervals
	if (is.null(CIlist)) {
		CIlist = matrix(NA, nrow = 9, ncol = 3, dimnames = list(
			c("a_cp_r1c1", "c_cp_r1c1", "e_cp_r1c1", "top.as_std[2,2]", "top.cs_std[2,2]", "top.es_std[2,2]", "top.cp_loadings_std[1,1]", "top.cp_loadings_std[2,1]", "top.cp_loadings_std[3,1]"),
			c("lbound", "estimate", "ubound")
		))
	}
	CIlist["top.cp_loadings_std[1,1]", ] = c(0.54, 0.70, 0.96)
	CIlist["top.as_std[2,2]", ] = c(0.40, 0.51, 0.65)
	CIlist["a_cp_r1c1", ] = c(0.80, 0.90, 0.98)
	m1@output$confidenceIntervals = CIlist

	digraph = umxPlotCP(m1, file = NA)
	expect_true(grepl("0\\.90\n\\[-?0\\.80, 0\\.98\\]", digraph))
	expect_true(grepl("0\\.70\n\\[-?0\\.54, 0\\.96\\]", digraph))
	expect_true(grepl("0\\.51\n\\[-?0\\.40, 0\\.65\\]", digraph))

	# Test SEstyle = "mxSE"
	digraph_se = umxPlotCP(m1, SEstyle = "mxSE", file = NA)
	expect_true(grepl("([0-9]+\\.[0-9]+)\n\\(([0-9]+\\.[0-9]+)\\)", digraph_se))
})