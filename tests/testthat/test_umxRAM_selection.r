library(testthat)
library(umx)
library(OpenMx)

# Feature: umxRAM top-level selection= spec (Pearson-Aitken). umxRAM generates
# the arrows=0 selected-variance path plus an implied-population-variance
# constraint from list(varNames=, popCov=). Bare/multi/unknown/group/WLS/sketch/
# hand-arrows combos error clearly.
test_that("umxRAM selection recovers the population covariance", {
	sel = umx_make_selection_data()$sel
	m = umxRAM("selDemo", data = sel,
		umxPath(v.m. = c("IQ", "C", "EA")),
		umxPath(from = c("IQ", "C"), to = "EA"),
		umxPath("IQ", with = "C"),
		autoRun = TRUE,
		selection = list(varNames = "EA", popCov = 1.0))
	expect_equal(m$output$status$code, 0)
	tab = summary(m)$parameters
	expect_true(abs(tab$Estimate[tab$name == "C_with_IQ"]) < 0.05)
	expect_equal(unname(tab$Estimate[tab$name == "sel_EA"]), unname(var(sel$EA)), tolerance = 0.05)
	expect_true(any(sapply(m$constraints, function(cc) cc$name) == "sel_popVar"))
	expect_equal(as.numeric(mxEval(popVar_sel, m, compute = TRUE)), 1.0, tolerance = 1e-3)
})

test_that("umxRAM selection validates its spec", {
	set.seed(42)
	n = 500
	IQ = rnorm(n)
	C = rnorm(n)
	EA = 0.4 * IQ + 0.2 * C + rnorm(n, sd = sqrt(0.8))
	sel = data.frame(IQ = IQ, C = C, EA = EA)
	sel = sel[sel$EA > quantile(sel$EA, 0.7), ]
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = "EA"), "must be a list")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = c("EA", "IQ"), popCov = 1)), "length")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "nope", popCov = 1)), "nope")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = 1), group = "zyg"), "group")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = 1), type = "WLS"), "WLS")
	expect_error(umxRAM("v", data = c("IQ", "C", "EA"), umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = 1)), "data")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		mxPath(from = "EA", arrows = 0), autoRun = FALSE,
		selection = list(varNames = "EA", popCov = 1)), "arrows=0")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = -1)), "positive")
	base = umxRAM("b", data = sel, umxPath(v.m. = c("IQ", "C", "EA")), autoRun = FALSE)
	expect_error(umxRAM(base, selection = list(varNames = "EA", popCov = 1)), "updating")
	expect_error(umxRAM("EA ~ IQ + C", data = sel, autoRun = FALSE,
		selection = list(varNames = "EA", popCov = 1)), "lavaan")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "EA", popVar = 1)), "varNames")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA")), "needs popCov")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = 3, popCov = 1)), "character")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = character(0), popCov = 1)), "at least one")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = diag(2))), "scalar")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = c(1, 1))), "scalar")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = "x")), "scalar")
	bad1 = matrix(1, 1, 1, dimnames = list("XX", "XX"))
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = bad1)), "dimnames")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		mxMatrix("Full", 1, 1, name = "sel_e"), autoRun = FALSE,
		selection = list(varNames = "EA", popCov = 1)), "already used")
})

test_that("single-var selection algebra prints with caption", {
	set.seed(7)
	sel = data.frame(EA = rnorm(200))
	m = umxRAM("v", data = sel, umxPath(v.m. = "EA"),
		autoRun = FALSE, selection = list(varNames = "EA", popCov = 1.0))
	out = capture.output(xmu_print_algebras(m))
	expect_true(any(grepl("Model-implied population variance of the selection variable", out, fixed = TRUE)))
	f = umxRAM("v", data = sel, umxPath(v.m. = "EA"),
		autoRun = TRUE, selection = list(varNames = "EA", popCov = 1.0))
	out = capture.output(xmu_print_algebras(f))
	expect_true(any(grepl("1, CI95[1, 1]", out, fixed = TRUE)))
})
