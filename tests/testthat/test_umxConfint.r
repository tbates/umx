# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxConfint.r") 
# test_package("umx")

test_that("umxConfint works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	
	# There are no existing CI requests...
	expect_message(umxConfint(m1, run = TRUE), regexp = "Polite note: This model has no CIs yet")
	
	# Add a CI request for "G_to_x1", run, and report. Save with this CI computed
	m2 = umxConfint(m1, parm = "G_to_x1", run = TRUE) 
	
	# Just print out any existing CIs
	umxConfint(m2)
	
	# CI requests added for free matrix parameters. User prompted to set run = TRUE
	expect_message(umxConfint(m1, "all"), regexp = "Polite note: Some CIs have been requested but not run. Add 'run = TRUE'")
	
	# Run the requested CIs
	m2 = umxConfint(m1, "all", run = TRUE) 
	
	# Run CIs for free one-headed (asymmetric) paths in RAM model. 
	#   note: Deletes other existing requests,
	tmp = umxConfint(m1, parm = "A", run = TRUE)
	
	# Wipe existing CIs, add G_to_x1
	tmp = umxConfint(m1, parm = "G_to_x1", run = TRUE, wipeExistingRequests = TRUE) 
	
	# RAM smart requests every free parameter label and does not run them.
	freeLabels = names(omxGetParameters(m1, free = TRUE))
	mSmart = umxConfint(m1, "smart")
	expect_true(all(freeLabels %in% names(mSmart$intervals)))
	expect_false(umx_has_CIs(mSmart, "output"))

})

test_that("umxModify intervals=TRUE recomputes requested CIs", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	m1 = mxModel(m1, mxCI("G_to_x1"))
	m2 = umxModify(m1, update = "G_to_x2", name = "drop_x2", intervals = TRUE, comparison = FALSE)
	expect_true(umx_has_CIs(m2, "intervals"))
	expect_true(umx_has_CIs(m2, "output"))
	m3 = umxModify(m2, update = "G_to_x3", name = "drop_x3", comparison = FALSE)
	expect_true(umx_has_CIs(m3, "intervals"))
	expect_false(umx_has_CIs(m3, "output"))
})

test_that("umxConfint smart requests free standardized cells for ACE and IP", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	mzData = mzData[1:80, ]
	dzData = dzData[1:80, ]
	ace = umxACE(selDVs = c("ht", "wt"), sep = "", dzData = dzData, mzData = mzData, autoRun = FALSE)
	aceSmart = umxConfint(ace, "smart")
	aceNames = names(aceSmart$intervals)
	expect_true("top.a_std[1,1]" %in% aceNames)
	expect_true("top.a_std[2,1]" %in% aceNames)
	expect_true("top.a_std[2,2]" %in% aceNames)
	expect_false("top.a_std[1,2]" %in% aceNames)
	expect_true("top.c_std[2,1]" %in% aceNames)
	expect_true("top.e_std[2,2]" %in% aceNames)
	expect_false(umx_has_CIs(aceSmart, "output"))

	ip = umxIP(selDVs = c("ht", "wt"), sep = "", dzData = dzData, mzData = mzData, autoRun = FALSE)
	ipSmart = umxConfint(ip, "smart")
	ipNames = names(ipSmart$intervals)
	expect_true("top.ai_std[2,1]" %in% ipNames)
	expect_true("top.as_std[1,1]" %in% ipNames)
	expect_true("top.as_std[2,2]" %in% ipNames)
	expect_false("top.as_std[2,1]" %in% ipNames)
	expect_false(umx_has_CIs(ipSmart, "output"))
})

test_that("umxConfint run=TRUE restores an ACE model that has submodels", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	mzData = mzData[1:40, ]
	dzData = dzData[1:40, ]
	ace = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
	ace = umxConfint(ace, parm = "top.a_std[1,1]", run = TRUE, wipeExistingRequests = TRUE)
	expect_true(umx_has_CIs(ace, "output"))
	expect_true("top.a_std[1,1]" %in% rownames(ace$output$confidenceIntervals))
	expect_false(is.null(ace@output$vcov))
})

test_that("summary and plot print a computed profile interval", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	mzData = mzData[1:60, ]
	dzData = dzData[1:60, ]
	ciRow = function(address, lb, est, ub) {
		out = matrix(c(lb, est, ub), nrow = 1, dimnames = list(address, c("lbound", "estimate", "ubound")))
		return(out)
	}
	putCI = function(model, row) {
		out = model@output
		out$confidenceIntervals = row
		model@output = out
		return(model)
	}

	ace = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
	expect_false(grepl("0.55", paste(capture.output(umxPlotACE(ace, file = NA)), collapse = "\n")))
	ace = putCI(ace, ciRow("top.a_std[1,1]", 0.25, 0.55, 0.85))
	aceText = paste(capture.output(umxSummaryACE(ace, file = NA)), collapse = "\n")
	expect_match(aceText, "0.55 \\[0.25, 0.85\\]")
	acePlot = paste(capture.output(umxPlotACE(ace, file = NA)), collapse = "\n")
	expect_match(acePlot, "0\\.55\\s*\\n\\[0\\.25, 0\\.85\\]")

	ip = umxIP(selDVs = c("ht", "wt"), sep = "", dzData = dzData, mzData = mzData)
	ip = putCI(ip, ciRow("top.ai_std[1,1]", 0.15, 0.45, 0.75))
	ipText = paste(capture.output(umxSummaryIP(ip, file = NA)), collapse = "\n")
	expect_match(ipText, "0.45 \\[0.15, 0.75\\]")
	ipPlot = paste(capture.output(umxPlotIP(ip, file = NA)), collapse = "\n")
	expect_match(ipPlot, "0\\.45\\s*\\n\\[0\\.15, 0\\.75\\]")

	cp = umxCP(selDVs = c("ht", "wt"), sep = "", dzData = dzData, mzData = mzData)
	cp = putCI(cp, ciRow("top.cp_loadings_std[1,1]", 0.05, 0.35, 0.65))
	cpText = paste(capture.output(umxSummaryCP(cp, file = NA)), collapse = "\n")
	expect_match(cpText, "0.35 \\[0.05, 0.65\\]")
	cpPlot = paste(capture.output(umxPlotCP(cp, file = NA)), collapse = "\n")
	expect_match(cpPlot, "0\\.35\\s*\\n\\[0\\.05, 0\\.65\\]")

	data(demoOneFactor)
	manifests = names(demoOneFactor)
	ram = umxRAM("OneFactorCI", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	ram = mxModel(ram, mxCI("G_to_x1"))
	ram = putCI(ram, ciRow("G_to_x1", 0.11, 0.42, 0.73))
	ramText = paste(capture.output(umxSummary(ram, std = FALSE, file = NA)), collapse = "\n")
	expect_match(ramText, "\\[0.11, 0.73\\]")
	ramPlot = paste(capture.output(plot(ram, file = NA, means = FALSE)), collapse = "\n")
	expect_match(ramPlot, "0.42 \\[0.11, 0.73\\]")
	ramStd = paste(capture.output(plot(ram, file = NA, means = FALSE, std = TRUE)), collapse = "\n")
	expect_false(grepl("0.42 \\[0.11, 0.73\\]", ramStd))
})

test_that("umxConfint prints an existing table only when report is TRUE", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("OneFactorReport", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	m1 = mxModel(m1, mxCI("G_to_x1"))
	out = m1@output
	out$confidenceIntervals = matrix(c(0.11, 0.42, 0.73), nrow = 1, dimnames = list("G_to_x1", c("lbound", "estimate", "ubound")))
	out$confidenceIntervalCodes = matrix(c(1, 1), nrow = 1, dimnames = list("G_to_x1", c("lbound", "ubound")))
	m1@output = out
	quiet = capture.output(umxConfint(m1, parm = "existing", run = FALSE, report = FALSE))
	expect_false(any(grepl("lbound Code", quiet)))
	expect_false(eval(formals(umxConfint)$report, envir = list(run = TRUE)))
	expect_true(eval(formals(umxConfint)$report, envir = list(run = FALSE)))
})

test_that("umxCI is deprecated and points at umxConfint", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("OneFactorDep", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_warning(umxCI(m1, which = NA), "umxConfint")
})

test_that("umxConfint mirrors wild sides and stamps the table", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("MirrorRepair", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	ciLabels = c("G_to_x1", "G_to_x2", "G_to_x3", "G_to_x4", "G_to_x5", "x1_with_x1", "x2_with_x2", "x3_with_x3", "x4_with_x4", "x5_with_x5")
	m1 = mxModel(m1, mxCI(ciLabels))
	fakeCI = matrix(c(
		-0.84, 0.74, 0.84,
		-0.85, -0.51, 0.71,
		-0.10, 0.10, 0.30,
		0.30, 0.40, 0.50,
		-0.10, 0.50, 0.80,
		0.00, 0.05, 0.40,
		-0.50, 0.10, 0.25,
		-0.84, 0.35, 0.84,
		-0.35, 0.08, 0.35,
		-0.40, 0.30, 0.30
	), nrow = 10, byrow = TRUE, dimnames = list(ciLabels, c("lbound", "estimate", "ubound")))
	out = m1@output
	out$confidenceIntervals = fakeCI
	out$confidenceIntervalCodes = matrix(0, nrow = 10, ncol = 2, dimnames = list(ciLabels, c("lbound", "ubound")))
	m1@output = out

	msgLines = testthat::capture_messages(tmp <- suppressWarnings(umxConfint(m1, report = FALSE)))
	expect_true(any(grepl("mirrored the wild lower", msgLines, fixed = TRUE)))
	expect_true(any(grepl("mirrored the wild upper", msgLines, fixed = TRUE)))
	expect_warning(suppressMessages(umxConfint(m1, report = FALSE)), "untrustworthy")
	warnLines = testthat::capture_warnings(tmpw <- suppressMessages(umxConfint(m1, report = FALSE)))
	expect_equal(length(warnLines), 4)
	expect_true(any(grepl("degenerate", warnLines, fixed = TRUE)))

	m2 = suppressMessages(suppressWarnings(umxConfint(m1, report = FALSE)))
	gotCI = m2$output$confidenceIntervals
	expect_equal(round(unname(gotCI["G_to_x1", "lbound"]), 3), 0.64)
	expect_equal(round(unname(gotCI["G_to_x1", "ubound"]), 3), 0.84)
	expect_equal(round(unname(gotCI["G_to_x2", "lbound"]), 3), -0.85)
	expect_equal(round(unname(gotCI["G_to_x2", "ubound"]), 3), -0.17)
	expect_equal(round(unname(gotCI["G_to_x3", c("lbound", "ubound")]), 3), c(-0.10, 0.30))
	expect_equal(round(unname(gotCI["G_to_x4", c("lbound", "ubound")]), 3), c(0.30, 0.50))
	expect_equal(round(unname(gotCI["G_to_x5", c("lbound", "ubound")]), 3), c(-0.10, 0.80))
	expect_equal(round(unname(gotCI["x1_with_x1", c("lbound", "ubound")]), 3), c(0.00, 0.40))
	expect_equal(round(unname(gotCI["x2_with_x2", c("lbound", "ubound")]), 3), c(-0.50, 0.25))
	expect_equal(round(unname(gotCI["x3_with_x3", c("lbound", "ubound")]), 3), c(-0.84, 0.84))
	expect_equal(round(unname(gotCI["x4_with_x4", c("lbound", "ubound")]), 3), c(-0.35, 0.35))
	expect_equal(round(unname(gotCI["x5_with_x5", c("lbound", "ubound")]), 3), c(-0.40, 0.30))

	printedLines = capture.output(m4 <- suppressMessages(suppressWarnings(umxConfint(m1, report = TRUE))))
	expect_true(any(grepl("G_to_x1 (mirrored)", printedLines, fixed = TRUE)))
	expect_true(any(grepl("G_to_x2 (mirrored)", printedLines, fixed = TRUE)))
	x3Line = printedLines[grepl("G_to_x3", printedLines, fixed = TRUE)]
	expect_false(any(grepl("mirrored", x3Line, fixed = TRUE)))
	x2Line = printedLines[grepl("x2_with_x2", printedLines, fixed = TRUE)]
	expect_false(any(grepl("mirrored", x2Line, fixed = TRUE)))
	x33Line = printedLines[grepl("x3_with_x3", printedLines, fixed = TRUE)]
	expect_false(any(grepl("mirrored", x33Line, fixed = TRUE)))

	msgLines2 = testthat::capture_messages(tmp2 <- suppressWarnings(umxConfint(m2, report = FALSE)))
	expect_equal(length(msgLines2), 0)
})

test_that("umxConfint type wald matches estimate plus-minus 1.96 SE", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("WaldCheck", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	freeEst = omxGetParameters(m1, free = TRUE)
	freeSE = m1$output$standardErrors

	waldLines = capture.output(umxConfint(m1, parm = "all", type = "wald"))
	x1Line = waldLines[grepl("G_to_x1", waldLines, fixed = TRUE)]
	x1Bits = strsplit(trimws(x1Line), "\\s+")[[1]]
	x1Nums = as.numeric(x1Bits[2:5])
	sePos = which(names(freeEst) == "G_to_x1")
	handLB = unname(freeEst["G_to_x1"] - 1.96 * freeSE[sePos])
	handUB = unname(freeEst["G_to_x1"] + 1.96 * freeSE[sePos])
	expect_equal(x1Nums[1], handLB, tolerance = 0.002)
	expect_equal(x1Nums[2], round(unname(freeEst["G_to_x1"]), 3))
	expect_equal(x1Nums[3], handUB, tolerance = 0.002)

	oxCI = confint(m1, parm = "G_to_x1", level = 0.95)
	expect_equal(x1Nums[1], round(unname(oxCI[1, 1]), 3))
	expect_equal(x1Nums[3], round(unname(oxCI[1, 2]), 3))

	wald99Lines = capture.output(umxConfint(m1, parm = "G_to_x1", type = "wald", level = 0.99))
	oxCI99 = confint(m1, parm = "G_to_x1", level = 0.99)
	x1Line99 = wald99Lines[grepl("G_to_x1", wald99Lines, fixed = TRUE)]
	x1Nums99 = as.numeric(strsplit(trimws(x1Line99), "\\s+")[[1]][2:5])
	expect_equal(x1Nums99[1], round(unname(oxCI99[1, 1]), 3))
	expect_equal(x1Nums99[3], round(unname(oxCI99[1, 2]), 3))

	smartLines = capture.output(umxConfint(m1, parm = "smart", type = "wald"))
	expect_true(any(grepl("G_to_x1", smartLines, fixed = TRUE)))

	m9msg = testthat::capture_messages(m9 <- umxConfint(m1, parm = "all", type = "wald", report = FALSE))
	expect_true(any(grepl("stored 10 Wald intervals", m9msg, fixed = TRUE)))
	expect_true(umx_has_CIs(m9, "output"))
	expect_true(umx_has_CIs(m9, "intervals"))
	expect_equal(round(unname(m9$output$confidenceIntervals["G_to_x1", c("lbound", "estimate", "ubound")]), 3), round(x1Nums[1:3], 3))
	expect_true(all(m9$output$confidenceIntervalCodes == 0))

	reMsg = testthat::capture_messages(m10 <- umxConfint(m9, report = FALSE))
	expect_equal(length(reMsg), 0)
	expect_identical(m10$output$confidenceIntervals, m9$output$confidenceIntervals)

	mMerge = mxModel(m1, mxCI("G_to_x1"))
	outMerge = mMerge@output
	outMerge$confidenceIntervals = matrix(c(0.11, 0.42, 0.73), nrow = 1, dimnames = list("G_to_x1", c("lbound", "estimate", "ubound")))
	outMerge$confidenceIntervalCodes = matrix(c(0, 0), nrow = 1, dimnames = list("G_to_x1", c("lbound", "ubound")))
	mMerge@output = outMerge
	mMerged = suppressMessages(umxConfint(mMerge, parm = "G_to_x2", type = "wald", report = FALSE, wipeExistingRequests = FALSE))
	expect_equal(sort(rownames(mMerged$output$confidenceIntervals)), c("G_to_x1", "G_to_x2"))
	expect_equal(round(unname(mMerged$output$confidenceIntervals["G_to_x1", "estimate"]), 3), 0.42)
	mWipeMsg = testthat::capture_messages(mWiped <- umxConfint(mMerge, parm = "all", type = "wald", report = FALSE))
	expect_true(any(grepl("Removed existing CIs", mWipeMsg, fixed = TRUE)))
	expect_equal(length(rownames(mWiped$output$confidenceIntervals)), 10)
	expect_equal(round(unname(mWiped$output$confidenceIntervals["G_to_x1", "estimate"]), 3), round(unname(freeEst["G_to_x1"]), 3))

	mUnrun = umxRAM("WaldUnrun", data = demoOneFactor, type = "cov", autoRun = FALSE,
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_error(umxConfint(mUnrun, parm = "all", type = "wald"), "no standard errors")
	expect_error(umxConfint(m1, parm = "no_such_label", type = "wald"), "No SE for")
	expect_error(umxConfint(m1, parm = "top.nope[1,1]", type = "wald"), "No SE for")
	expect_warning(mMix <- suppressMessages(umxConfint(m1, parm = c("G_to_x1", "no_such_label"), type = "wald", report = FALSE)), "skipped")
	expect_equal(rownames(mMix$output$confidenceIntervals), "G_to_x1")
	expect_warning(mMix2 <- suppressMessages(umxConfint(m1, parm = c("G_to_x1", "top.nope[1,1]"), type = "wald", report = FALSE)), "top.nope")
	expect_equal(rownames(mMix2$output$confidenceIntervals), "G_to_x1")
})

test_that("umxConfint profile and wald run end to end on ACE", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	mzData = mzData[1:40, ]
	dzData = dzData[1:40, ]
	ace = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
	firstFree = names(omxGetParameters(ace, free = TRUE))[1]
	aceWaldLines = capture.output(umxConfint(ace, parm = "all", type = "wald"))
	expect_true(any(grepl(firstFree, aceWaldLines, fixed = TRUE)))
	aceCI = umxConfint(ace, parm = "top.a_std[1,1]", run = TRUE, wipeExistingRequests = TRUE)
	expect_true(umx_has_CIs(aceCI, "output"))
	aceSmartLines = capture.output(umxConfint(ace, parm = "smart", type = "wald"))
	expect_true(any(grepl("top.a_std[1,1]", aceSmartLines, fixed = TRUE)))
	aceSmartMsg = testthat::capture_messages(umxConfint(ace, parm = "smart", type = "wald", report = FALSE))
	expect_true(any(grepl("delta-method", aceSmartMsg, fixed = TRUE)))
})

test_that("umxConfint wald smart works on CP", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	mzData = mzData[1:60, ]
	dzData = dzData[1:60, ]
	cp = umxCP(selDVs = c("ht", "wt"), sep = "", dzData = dzData, mzData = mzData)
	cpSmartLines = capture.output(umxConfint(cp, parm = "smart", type = "wald"))
	expect_true(any(grepl("top.cp_loadings_std[1,1]", cpSmartLines, fixed = TRUE)))
	loadLine = cpSmartLines[grepl("top.cp_loadings_std[1,1]", cpSmartLines, fixed = TRUE)]
	loadNums = as.numeric(strsplit(trimws(loadLine), "\\s+")[[1]][2:5])
	loadEst = mxEvalByName("top.cp_loadings_std", cp)[1, 1]
	expect_equal(loadNums[2], round(loadEst, 3))
	loadSE = mxSE("top.cp_loadings_std[1,1]", cp, silent = TRUE)[1, 1]
	expect_equal(loadNums[4], round(loadSE, 3))
	expect_equal(loadNums[3] - loadNums[1], round(2 * qnorm(0.975) * loadSE, 3), tolerance = 0.002)
	cpWald = suppressMessages(umxConfint(cp, parm = "smart", type = "wald", report = FALSE))
	waldLB = round(unname(cpWald$output$confidenceIntervals["top.cp_loadings_std[1,1]", "lbound"]), 2)
	waldEst = round(unname(cpWald$output$confidenceIntervals["top.cp_loadings_std[1,1]", "estimate"]), 2)
	waldUB = round(unname(cpWald$output$confidenceIntervals["top.cp_loadings_std[1,1]", "ubound"]), 2)
	cpText = paste(capture.output(umxSummaryCP(cpWald, file = NA)), collapse = "\n")
	expect_true(any(grepl(paste0(waldEst, " [", waldLB, ", ", waldUB, "]"), cpText, fixed = TRUE)))
	cpPlot = paste(capture.output(umxPlotCP(cpWald, file = NA)), collapse = "\n")
	plotWant = paste0(umx_APA_pval(waldEst, min = -1, digits = 2), "\n[", umx_APA_pval(waldLB, min = -1, digits = 2), ", ", umx_APA_pval(waldUB, min = -1, digits = 2), "]")
	expect_true(any(grepl(plotWant, cpPlot, fixed = TRUE)))
})

test_that("umxConfint smart stops politely on unknown classes", {
	require(umx)
	bareModel = mxModel("bare")
	expect_error(umxConfint(bareModel, parm = "smart", type = "wald"), "does not know class")
	expect_error(umxConfint(bareModel, parm = "smart"), "does not know class")
})
