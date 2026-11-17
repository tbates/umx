library(testthat)
library(umx)
library(OpenMx)

makeSelSingle = function() {
	obsCov = matrix(c(1.0, 0.42, 0.02, 0.42, 1.0, 0.25, 0.02, 0.25, 1.0), 3, 3)
	colnames(obsCov) = rownames(obsCov) = c("x1", "x2", "x3")
	mxModel("selML", type = "RAM", manifestVars = c("x1", "x2", "x3"),
		mxPath("x1", "x2", arrows = 0, free = TRUE, values = 0.3, labels = "s12"),
		mxPath("x1", "x3", arrows = 0, free = TRUE, values = 0.2),
		mxPath(c("x1", "x2", "x3"), arrows = 2, free = TRUE, values = 0.8),
		mxPath("x2", "x3", arrows = 2, free = TRUE, values = 0.2, labels = "c23"),
		mxData(obsCov, type = "cov", numObs = 400), mxFitFunctionML())
}

makeSelMulti = function() {
	oc = matrix(c(1, 0.4, 0.4, 1), 2, 2)
	colnames(oc) = rownames(oc) = c("y1", "y2")
	mk = function(nm) {
		mxModel(nm, type = "RAM", manifestVars = c("y1", "y2"),
			mxPath("y1", "y2", arrows = 0, free = TRUE, values = 0.3, labels = "sel12"),
			mxPath(c("y1", "y2"), arrows = 2, free = TRUE, values = 0.9),
			mxData(oc, type = "cov", numObs = 200), mxFitFunctionML())
	}
	mxModel("mgsel", mk("g1"), mk("g2"), mxFitFunctionMultigroup(c("g1", "g2")))
}

test_that("umxSummary shows a Selection block for arrows=0 paths", {
	# Defect: selection lives outside A/S/M (selectionVector), which is all
	# mxStandardizeRAMpaths maps, so umxSummary silently dropped it.
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	m = mxRun(makeSelSingle(), silent = TRUE)
	out = capture.output(inv <- umxSummary(m, std = FALSE, matrixAddresses = TRUE), type = "output")
	expect_true(any(grepl("Selection paths for model", out, fixed = TRUE)))
	expect_true(any(grepl("s12", out, fixed = TRUE)))
	selRows = inv[grepl("(^|\\.)selectionVector$", inv$matrix), ]
	expect_equal(nrow(selRows), 2)
	expect_true(all(is.finite(selRows$Estimate)))
	expect_true(all(is.finite(selRows$SE)))
})

test_that("multigroup shared selection label shows per-group rows", {
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	mg = mxRun(makeSelMulti(), silent = TRUE)
	out = capture.output(inv <- umxSummary(mg, std = FALSE, matrixAddresses = TRUE), type = "output")
	expect_true(any(grepl("Selection paths for model", out, fixed = TRUE)))
	selRows = inv[grepl("(^|\\.)selectionVector$", inv$matrix), ]
	expect_equal(nrow(selRows), 2)
	expect_equal(sum(selRows$name == "sel12", na.rm = TRUE), 2)
})

test_that("selection rows show no standardized values", {
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	m = mxRun(makeSelSingle(), silent = TRUE)
	out = capture.output(inv <- umxSummary(m, std = TRUE, matrixAddresses = TRUE), type = "output")
	selRows = inv[grepl("(^|\\.)selectionVector$", inv$matrix), ]
	expect_equal(nrow(selRows), 2)
	expect_true(all(is.na(selRows$Std.Estimate)))
})

test_that("SIG/NS filters run over selection rows", {
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	m = mxRun(makeSelSingle(), silent = TRUE)
	sigOut = capture.output(umxSummary(m, std = FALSE, filter = "SIG"), type = "output")
	expect_true(any(grepl("s12", sigOut, fixed = TRUE)))
	nsOut = capture.output(umxSummary(m, std = FALSE, filter = "NS"), type = "output")
	expect_false(any(grepl("s12", nsOut, fixed = TRUE)))
	expect_true(any(grepl("Selection paths for model", nsOut, fixed = TRUE)))
})
