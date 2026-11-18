library(testthat)
library(umx)
library(OpenMx)

test_that("unlabeled RAM params get correct types, not Manifest path", {
	# Defect: grouping located rows by label; omxLocateParameters(NA) returns
	# all unlabeled cells, so every unlabeled row took the first match's type.
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = mxModel("unlabeled", type = "RAM",
		manifestVars = v, latentVars = "G",
		mxPath("G", to = v[1], free = FALSE, values = 1),
		mxPath("G", to = v[2:3], free = TRUE, values = 0.8),
		mxPath(v, arrows = 2, free = TRUE, values = 0.5),
		mxPath(v[1], to = v[2], arrows = 2, free = TRUE, values = 0.2),
		mxPath("G", arrows = 2, free = TRUE, values = 1),
		mxPath("one", to = v, free = TRUE, values = 0),
		mxData(demoOneFactor[, v], type = "raw"), mxFitFunctionML())
	m1 = mxRun(m1, silent = TRUE)
	out = capture.output(umxSummary(m1, std = FALSE), type = "output")
	for (tp in c("Factor loading", "Residual", "Mean", "Factor Variance", "Manifest Cov")) {
		expect_true(any(grepl(tp, out, fixed = TRUE)))
	}
	expect_false(any(grepl("Manifest path", out, fixed = TRUE)))
})

test_that("unlabeled multigroup RAM types residuals correctly", {
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	set.seed(5)
	dat = data.frame(y1 = rnorm(150), y2 = rnorm(150))
	mk = function(nm) {
		mxModel(nm, type = "RAM", manifestVars = c("y1", "y2"),
			mxPath("one", c("y1", "y2"), free = TRUE),
			mxPath(c("y1", "y2"), arrows = 2, free = TRUE, values = 1),
			mxPath("y1", to = "y2", arrows = 2, free = TRUE, values = 0.2),
			mxData(dat, type = "raw"), mxFitFunctionML())
	}
	mg = mxModel("mgunlabeled", mk("g1"), mk("g2"), mxFitFunctionMultigroup(c("g1", "g2")))
	mg = mxRun(mg, silent = TRUE)
	out = capture.output(umxSummary(mg, std = FALSE), type = "output")
	expect_true(any(grepl("Residual", out, fixed = TRUE)))
	expect_true(any(grepl("Manifest Cov", out, fixed = TRUE)))
	expect_true(any(grepl("Mean", out, fixed = TRUE)))
})
