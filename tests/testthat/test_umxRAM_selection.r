library(testthat)
library(umx)
library(OpenMx)

# Feature: umxRAM top-level selection= spec (Pearson-Aitken). umxRAM generates
# the arrows=0 selected-variance path plus an implied-population-variance
# constraint from list(var=, popVar=). Bare/multi/unknown/group/WLS/sketch/
# hand-arrows combos error clearly.
test_that("umxRAM selection recovers the population covariance", {
	sel = umx_make_selection_data()$sel
	m = umxRAM("selDemo", data = sel,
		umxPath(v.m. = c("IQ", "C", "EA")),
		umxPath(from = c("IQ", "C"), to = "EA"),
		umxPath("IQ", with = "C"),
		autoRun = TRUE,
		selection = list(var = "EA", popVar = 1.0))
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
		autoRun = FALSE, selection = "EA"), "popVar")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = c("EA", "IQ"), popVar = 1)), "popCov")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "nope", popVar = 1)), "nope")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "EA", popVar = 1), group = "zyg"), "group")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "EA", popVar = 1), type = "WLS"), "WLS")
	expect_error(umxRAM("v", data = c("IQ", "C", "EA"), umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "EA", popVar = 1)), "data")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		mxPath(from = "EA", arrows = 0), autoRun = FALSE,
		selection = list(var = "EA", popVar = 1)), "arrows=0")
	expect_error(umxRAM("v", data = sel, umxPath(v.m. = c("IQ", "C", "EA")),
		autoRun = FALSE, selection = list(var = "EA", popVar = -1)), "positive")
	base = umxRAM("b", data = sel, umxPath(v.m. = c("IQ", "C", "EA")), autoRun = FALSE)
	expect_error(umxRAM(base, selection = list(var = "EA", popVar = 1)), "updating")
	expect_error(umxRAM("EA ~ IQ + C", data = sel, autoRun = FALSE,
		selection = list(var = "EA", popVar = 1)), "lavaan")
})
