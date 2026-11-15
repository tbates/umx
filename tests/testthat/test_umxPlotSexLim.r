test_that("umxPlotSexLim uncertainty shows SE and CI", {
	require(umx)
	data("us_skinfold_data")
	mzmData = subset(us_skinfold_data, zyg == 1)
	mzfData = subset(us_skinfold_data, zyg == 2)
	dzmData = subset(us_skinfold_data, zyg == 3)
	dzfData = subset(us_skinfold_data, zyg == 4)
	dzoData = subset(us_skinfold_data, zyg == 5)
	# Silent mode leaks across files in full-suite runs; force it off (restored on exit)
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	s1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", mzmData = mzmData, dzmData = dzmData, mzfData = mzfData, dzfData = dzfData, dzoData = dzoData)
	freeLab = names(OpenMx::omxGetParameters(s1, free = TRUE))[1]
	seProbe = suppressWarnings(tryCatch(OpenMx::mxSE(freeLab, s1, silent = TRUE)[1, 1], error = function(e) NA_real_))
	testthat::skip_if_not(is.finite(seProbe), "mxSE unavailable on this build")
	# default equals explicit none; junk level errors fast
	m1 = testthat::capture_messages(digNone1 <- umxPlotSexLim(s1, file = NA))
	m2 = testthat::capture_messages(digNone2 <- umxPlotSexLim(s1, file = NA, uncertainty = "none"))
	expect_identical(digNone1, digNone2)
	expect_error(testthat::capture_messages(umxPlotSexLim(s1, file = NA, uncertainty = "bogus")))
	# CI with nothing stored tips and stays bare
	m3 = testthat::capture_messages(digCIbare <- umxPlotSexLim(s1, file = NA, uncertainty = "CI"))
	expect_true(any(grepl("no stored intervals", m3, fixed = TRUE)))
	expect_false(grepl("[0-9] \\[[0-9.\\-]", digCIbare))
	# CI shows a stored row on the raw display only (a raw interval would mislabel standardized values)
	# (requests accompany the table, as umxConfint leaves them)
	s1 = mxModel(s1, mxCI("am_r1c1"))
	out = s1@output
	out$confidenceIntervals = matrix(c(0.11, 0.42, 0.73), nrow = 1, dimnames = list("am_r1c1", c("lbound", "estimate", "ubound")))
	s1@output = out
	m4 = testthat::capture_messages(digCIraw <- umxPlotSexLim(s1, file = NA, std = FALSE, uncertainty = "CI"))
	expect_true(grepl("0?\\.42 \\[0?\\.11, 0?\\.73\\]", digCIraw))
	m5 = testthat::capture_messages(digCIstd <- umxPlotSexLim(s1, file = NA, std = TRUE, uncertainty = "CI"))
	expect_false(grepl("0?\\.42 \\[0?\\.11, 0?\\.73\\]", digCIstd))
	# SE shows est (se) and ignores stored rows (one type per plot)
	expect_false(grepl("[0-9] \\([0-9.\\-]", digNone1))
	m6 = testthat::capture_messages(digSE <- umxPlotSexLim(s1, file = NA, uncertainty = "SE"))
	expect_true(grepl("[0-9] \\([0-9.\\-]", digSE))
	expect_false(grepl("0?\\.42 \\[0?\\.11, 0?\\.73\\]", digSE))
	# Standardized SEs use the sqrt-delta transform: se(path) = se(variance) / (2 * path)
	amLine = grep("am1 -> M_bic", strsplit(digSE, "\n", fixed = TRUE)[[1]], value = TRUE, fixed = TRUE)
	amLabel = sub('.*label="([^"]+)".*', "\\1", amLine)
	amNums = as.numeric(regmatches(amLabel, gregexpr("-?\\.?[0-9]+\\.?[0-9]*", amLabel))[[1]])
	amVar = s1$top$AmStd$result[1, 1]
	amSEvar = OpenMx::mxSE("top.AmStd[1,1]", s1, silent = TRUE)[1, 1]
	expect_true(abs(amNums[2] - amSEvar/(2 * sqrt(amVar))) < 0.015)
	# showCIs hides stored rows; explicitly requested SEs still show
	m7 = testthat::capture_messages(digHide <- umxPlotSexLim(s1, file = NA, std = FALSE, uncertainty = "CI", showCIs = FALSE))
	expect_false(grepl("0?\\.42 \\[0?\\.11, 0?\\.73\\]", digHide))
	m8 = testthat::capture_messages(digSEhide <- umxPlotSexLim(s1, file = NA, uncertainty = "SE", showCIs = FALSE))
	expect_true(grepl("[0-9] \\([0-9.\\-]", digSEhide))
	# SEstyle is deprecated: maps to uncertainty = "SE" with identical output
	m9 = testthat::capture_messages(digDep <- umxPlotSexLim(s1, SEstyle = TRUE, file = NA))
	expect_true(any(grepl("deprecated", m9, fixed = TRUE)))
	expect_identical(digDep, digSE)
})

test_that("umxPlotSexLim means shows fitted male and female means", {
	require(umx)
	data("us_skinfold_data")
	mzmData = subset(us_skinfold_data, zyg == 1)
	mzfData = subset(us_skinfold_data, zyg == 2)
	dzmData = subset(us_skinfold_data, zyg == 3)
	dzfData = subset(us_skinfold_data, zyg == 4)
	dzoData = subset(us_skinfold_data, zyg == 5)
	# Silent mode leaks across files in full-suite runs; force it off (restored on exit)
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	s1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", mzmData = mzmData, dzmData = dzmData, mzfData = mzfData, dzfData = dzfData, dzoData = dzoData)
	# default shows no triangles and equals explicit means = FALSE
	m1 = testthat::capture_messages(digDef <- umxPlotSexLim(s1, file = NA))
	m2 = testthat::capture_messages(digNoMeans <- umxPlotSexLim(s1, file = NA, means = FALSE))
	expect_identical(digDef, digNoMeans)
	expect_false(grepl("shape=triangle", digDef, fixed = TRUE))
	# file = NA prints the diagram once (no R-string dump) and returns it
	noPrint = capture.output(digQuiet <- umxPlotSexLim(s1, file = NA))
	expect_false(any(grepl('[1] "digraph', noPrint, fixed = TRUE)))
	expect_identical(digQuiet, digDef)
	# means = TRUE adds one triangle per cluster
	m3 = testthat::capture_messages(digMeans <- umxPlotSexLim(s1, file = NA, means = TRUE))
	expect_true(grepl("one_m [shape=triangle];", digMeans, fixed = TRUE))
	expect_true(grepl("one_f [shape=triangle];", digMeans, fixed = TRUE))
	# paths carry the fitted twin-1 means
	meanLines = strsplit(digMeans, "\n", fixed = TRUE)[[1]]
	mLine = grep("one_m -> M_bic", meanLines, value = TRUE, fixed = TRUE)
	fLine = grep("one_f -> F_bic", meanLines, value = TRUE, fixed = TRUE)
	expect_equal(length(mLine), 1)
	expect_equal(length(fLine), 1)
	mLabel = sub('.*label="([^"]+)".*', "\\1", mLine)
	fLabel = sub('.*label="([^"]+)".*', "\\1", fLine)
	mNum = as.numeric(regmatches(mLabel, gregexpr("-?\\.?[0-9]+\\.?[0-9]*", mLabel))[[1]])
	fNum = as.numeric(regmatches(fLabel, gregexpr("-?\\.?[0-9]+\\.?[0-9]*", fLabel))[[1]])
	expect_true(abs(mNum[1] - s1$top$expMeanGm$values[1, 1]) < 0.015)
	expect_true(abs(fNum[1] - s1$top$expMeanGf$values[1, 1]) < 0.015)
	# twin-2 columns repeat twin-1 (shared labels); DZO reuses both sets
	expect_equal(s1$top$expMeanGm$values[1, 2], s1$top$expMeanGm$values[1, 1])
	expect_equal(s1$top$expMeanGf$values[1, 2], s1$top$expMeanGf$values[1, 1])
	expect_equal(s1$top$expMeanGo$values[1, 1], s1$top$expMeanGm$values[1, 1])
	expect_equal(s1$top$expMeanGo$values[1, 2], s1$top$expMeanGf$values[1, 1])
	# means stay raw on standardized diagrams
	m4 = testthat::capture_messages(digStd <- umxPlotSexLim(s1, file = NA, means = TRUE, std = TRUE))
	stdLine = grep("one_m -> M_bic", strsplit(digStd, "\n", fixed = TRUE)[[1]], value = TRUE, fixed = TRUE)
	stdNum = as.numeric(regmatches(stdLine, gregexpr("-?\\.?[0-9]+\\.?[0-9]*", stdLine))[[1]])
	expect_true(abs(stdNum[1] - s1$top$expMeanGm$values[1, 1]) < 0.015)
	# SE routes per cell when computable
	meanSEprobe = suppressWarnings(tryCatch(OpenMx::mxSE("bic_mean_m", s1, silent = TRUE)[1, 1], error = function(e) NA_real_))
	m5 = testthat::capture_messages(digSE <- umxPlotSexLim(s1, file = NA, means = TRUE, uncertainty = "SE"))
	seLine = grep("one_m -> M_bic", strsplit(digSE, "\n", fixed = TRUE)[[1]], value = TRUE, fixed = TRUE)
	expect_true(grepl("(", seLine, fixed = TRUE) || !is.finite(meanSEprobe))
	# stored intervals paste onto raw means paths
	s1 = mxModel(s1, mxCI("bic_mean_m"))
	out = s1@output
	out$confidenceIntervals = matrix(c(14.1, 15.2, 16.3), nrow = 1, dimnames = list("bic_mean_m", c("lbound", "estimate", "ubound")))
	s1@output = out
	m6 = testthat::capture_messages(digCI <- umxPlotSexLim(s1, file = NA, means = TRUE, std = FALSE, uncertainty = "CI"))
	expect_true(grepl("15.20 [14.10, 16.30]", digCI, fixed = TRUE))
})
