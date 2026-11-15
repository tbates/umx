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

test_that("umxPlotCP with CIs and uncertainty works", {
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

	# SEstyle is deprecated: maps to uncertainty = "SE" with identical output
	depMsg = testthat::capture_messages(digraph_se <- umxPlotCP(m1, SEstyle = "mxSE", file = NA))
	expect_true(any(grepl("deprecated", depMsg, fixed = TRUE)))
	seRef = suppressMessages(umxPlotCP(m1, uncertainty = "SE", file = NA))
	expect_identical(digraph_se, seRef)
	# uncertainty = "SE" shows fresh SEs and ignores stored rows (one type per plot)
	expect_false(grepl("\n(", digraph, fixed = TRUE))
	seMsg = testthat::capture_messages(digraphSE <- umxPlotCP(m1, uncertainty = "SE", file = NA))
	expect_false(grepl("0\\.90\n\\[-?0\\.80, 0\\.98\\]", digraphSE))
	seShown = grepl("\n(", digraphSE, fixed = TRUE)
	seExplained = any(grepl("umxPlotCP note", seMsg, fixed = TRUE))
	expect_true(seShown || seExplained)
	# uncertainty = "CI" shows stored intervals only; with nothing stored, a tip and a bare diagram
	m1@output$confidenceIntervals = NULL
	tipMsg = testthat::capture_messages(digraphCItip <- umxPlotCP(m1, uncertainty = "CI", file = NA))
	expect_true(any(grepl("no stored intervals", tipMsg, fixed = TRUE)))
	expect_false(grepl("\n[", digraphCItip, fixed = TRUE))
	m1@output$confidenceIntervals = CIlist
	digraphCI = umxPlotCP(m1, uncertainty = "CI", file = NA)
	expect_true(grepl("0\\.90\n\\[-?0\\.80, 0\\.98\\]", digraphCI))
	expect_true(grepl("0\\.70\n\\[-?0\\.54, 0\\.96\\]", digraphCI))
	# std = FALSE stays bare: standardized intervals are never pasted on raw diagrams
	expect_false(grepl("\n[", umxPlotCP(m1, std = FALSE, file = NA), fixed = TRUE))
	expect_false(grepl("\n[", umxPlotCP(m1, std = FALSE, file = NA, uncertainty = "CI"), fixed = TRUE))
	digRawSE = umxPlotCP(m1, std = FALSE, file = NA, uncertainty = "SE")
	expect_false(grepl("\n[", digRawSE, fixed = TRUE))
	expect_false(grepl("\n(", digRawSE, fixed = TRUE))
	# junk level errors fast
	expect_error(umxPlotCP(m1, uncertainty = "bogus", file = NA))
})

test_that("umxSummaryCP uncertainty shows stored, computed, and SE intervals", {
	require(umx)
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")[1:60, ]
	dzData = subset(GFF, zyg_2grp == "DZ")[1:60, ]
	selDVs = c("gff", "fc", "qol")
	# Silent mode leaks across files in full-suite runs; force it off (restored on exit)
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	fitMsg = testthat::capture_messages(m1 <- umxCP("new", selDVs = selDVs, sep = "_T", nFac = 1, dzData = dzData, mzData = mzData))
	# fit-time summary tips but never computes
	expect_true(any(grepl("no stored intervals", fitMsg, fixed = TRUE)))
	# default with no stored intervals: bare tables plus a tip, nothing computed
	defaultMsg = testthat::capture_messages(defaultOut <- capture.output(umxSummaryCP(m1, file = NA)))
	expect_true(any(grepl("no stored intervals", defaultMsg, fixed = TRUE)))
	expect_false(any(grepl("[", defaultOut, fixed = TRUE)))
	# stored intervals display without recomputing
	CIlist = matrix(c(0.54, 0.70, 0.96), nrow = 1, dimnames = list("top.cp_loadings_std[1,1]", c("lbound", "estimate", "ubound")))
	m1@output$confidenceIntervals = CIlist
	storedMsg = testthat::capture_messages(storedOut <- capture.output(umxSummaryCP(m1, file = NA)))
	expect_false(any(grepl("on the fly", storedMsg, fixed = TRUE)))
	expect_true(any(grepl("0.7 [0.54, 0.96]", storedOut, fixed = TRUE)))
	# none shows bare estimates, as does the deprecated alias
	bareOut = capture.output(umxSummaryCP(m1, file = NA, uncertainty = "none"))
	expect_false(any(grepl("[", bareOut, fixed = TRUE)))
	aliasOut = capture.output(umxSummaryCP(m1, file = NA, CIs = FALSE))
	expect_false(any(grepl("[", aliasOut, fixed = TRUE)))
	# SE shows est (se)
	seMsg = testthat::capture_messages(seOut <- capture.output(umxSummaryCP(m1, file = NA, uncertainty = "SE")))
	seShown = any(grepl("[0-9] \\([0-9]", seOut))
	seExplained = any(grepl("umxSummary note", seMsg, fixed = TRUE))
	expect_true(seShown || seExplained)
})