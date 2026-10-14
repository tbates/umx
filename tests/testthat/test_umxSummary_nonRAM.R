# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxSummary_nonRAM.R")

require(umx)
umx_set_silent(TRUE)
data(demoOneFactor)
v = names(demoOneFactor)[1:3]
# A one-factor model written with mxMatrix/mxAlgebra (not RAM)
mk = function(name, d, dims = v) {
	mxModel(name,
		mxMatrix("Full", 3, 1, free = TRUE, values = .5, labels = paste0("l", 1:3), name = "L"),
		mxMatrix("Diag", 3, 3, free = TRUE, values = .5, labels = paste0("e", 1:3), name = "E"),
		mxMatrix("Full", 1, 3, free = TRUE, values = 0, labels = paste0("m", 1:3), name = "M"),
		mxAlgebra(L %*% t(L) + E, name = "S"),
		mxExpectationNormal("S", means = "M", dimnames = dims),
		mxFitFunctionML(),
		mxData(d[, v], type = "raw")
	)
}
m = mxRun(mk("mat", demoOneFactor), silent = TRUE)
quiet = function(expr) { out = NULL; invisible(capture.output(suppressMessages(out <- expr))); out }

test_that("umxSummary reports raw estimates for non-RAM models instead of failing", {
	x = quiet(umxSummary(m))
	expect_s3_class(x, "data.frame")
	expect_equal(sort(x$name), sort(names(omxGetParameters(m))))
	expect_message(capture.output(umxSummary(m, std = TRUE)), "standardized estimates need a RAM model")
	expect_message(capture.output(umxSummary(m)), "Model Fit")

	mg = mxRun(mxModel("mg", mk("g1", demoOneFactor[1:250, ]), mk("g2", demoOneFactor[251:500, ]), mxFitFunctionMultigroup(c("g1", "g2"))), silent = TRUE)
	expect_s3_class(quiet(umxSummary(mg)), "data.frame")
})

test_that("umxModify returns the refit non-RAM model, even inside try/tryCatch", {
	y = try(quiet(umxModify(m, update = "e3", value = .2, name = "fix_e3")), silent = TRUE)
	expect_s4_class(y, "MxModel")
	expect_equal(y$name, "fix_e3")
	expect_false("e3" %in% names(omxGetParameters(y)))
	z = tryCatch(quiet(umxModify(m, update = "e3", value = .2, name = "fix_e3", comparison = TRUE)), error = function(e) e)
	expect_s4_class(z, "MxModel")
})

test_that("xmu_safe_run_summary reports a run error without re-throwing it", {
	bad = mk("bad", demoOneFactor, dims = c("x1", "x2", "not_in_data"))
	out = tryCatch(suppressMessages(xmu_safe_run_summary(bad)), error = function(e) e)
	expect_s4_class(out, "MxModel")
	expect_false(umx_has_been_run(out))
	expect_message(xmu_safe_run_summary(bad), "Error incurred trying to run model")
	expect_true(is.na(suppressMessages(xmu_safe_run_summary(bad, returning = "summary"))))
})
