library(testthat)
library(umx)
library(OpenMx)

test_that("umxSummary and plot handle uncertainty reporting options correctly", {
	# Fit a simple RAM model on raw data
	data(demoOneFactor)
	manifests = names(demoOneFactor)[1:3] # use subset for speed
	m1 = umxRAM("One Factor Test", data = demoOneFactor[, manifests],
		umxPath("G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)

	# 1. Test umxSummary with uncertainty options
	# uncertainty = "none" (Estimates only, no SE or CI columns)
	sumNone = capture.output({
		tblNone = umxSummary(m1, std = TRUE, uncertainty = "none")
	})
	expect_false("Std.SE" %in% names(tblNone))
	expect_false("CI" %in% names(tblNone))

	# uncertainty = "SE" (Standard errors and Wald CIs)
	sumSE = capture.output({
		tblSE = umxSummary(m1, std = TRUE, uncertainty = "SE")
	})
	expect_true("Std.SE" %in% names(tblSE))
	expect_true("CI" %in% names(tblSE))

	# uncertainty = "RobustSE" (Robust standard errors)
	sumRobust = capture.output({
		tblRobust = umxSummary(m1, std = TRUE, uncertainty = "RobustSE")
	})
	expect_true("Std.SE" %in% names(tblRobust))
	expect_true("CI" %in% names(tblRobust))

	# uncertainty = "CI" (Profile likelihood CIs)
	# Run profile CIs first
	m1CI = umxConfint(m1, parm = "all", run = TRUE)
	sumCI = capture.output({
		tblCI = umxSummary(m1CI, std = TRUE, uncertainty = "CI")
	})
	expect_true("CI" %in% names(tblCI))
	expect_false("Std.SE" %in% names(tblCI))

	# 2. Test plot with uncertainty options
	# uncertainty = "none" (no parentheses/brackets)
	dotNone = plot(m1, uncertainty = "none", file = NA)
	expect_true(any(grepl("G -> x1 \\[label=\"[0-9.]+\"\\]", dotNone)))

	# uncertainty = "SE" (parentheses showing standard errors)
	dotSE = plot(m1, uncertainty = "SE", file = NA)
	expect_true(any(grepl("G -> x1 \\[label=\"[0-9.]+ \\([0-9.]+\\)\"\\]", dotSE)))

	# uncertainty = "RobustSE"
	dotRobust = plot(m1, uncertainty = "RobustSE", file = NA)
	expect_true(any(grepl("G -> x1 \\[label=\"[0-9.]+ \\([0-9.]+\\)\"\\]", dotRobust)))

	# uncertainty = "CI" (brackets showing profile CIs)
	dotCI = plot(m1CI, uncertainty = "CI", file = NA)
	expect_true(any(grepl("G -> x1 \\[label=\"[0-9.]+ \\[[0-9.-]+, [0-9.-]+\\]\"\\]", dotCI)))
})
