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
