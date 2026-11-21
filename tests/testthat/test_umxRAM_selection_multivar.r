library(testthat)
library(umx)
library(OpenMx)

# Feature: umxRAM selection= for several variables (Pearson-Aitken battery).
# selection = list(varNames = c(...), popCov = <k-by-k>) recovers population
# validities when the sample is selected on the whole set. popCov takes a named
# matrix (varNames inferred from dimnames) or a length-k*k vector (varNames required).
# Schmidt & Hunter (1998), Table 1: COG .51, INT .51, BIO .35; COG-INT .30 and
# COG-BIO .50 as reported alongside; INT-BIO .25 is ASSUMED for the sim.
test_that("umxRAM selection recovers battery validities", {
	set.seed(100)
	nPop = 30000
	Rpop = matrix(c(1.00, 0.30, 0.50, 0.51,
	                0.30, 1.00, 0.25, 0.51,
	                0.50, 0.25, 1.00, 0.35,
	                0.51, 0.51, 0.35, 1.00), 4, 4)
	colnames(Rpop) = rownames(Rpop) = c("COG", "INT", "BIO", "PERF")
	pop = as.data.frame(matrix(rnorm(nPop * 4), ncol = 4) %*% chol(Rpop))
	names(pop) = c("COG", "INT", "BIO", "PERF")
	composite = pop$COG + pop$INT + pop$BIO
	sel = pop[composite > quantile(composite, 0.7), ]
	tests = c("COG", "INT", "BIO")
	truth = c(0.51, 0.51, 0.35)

	# Naive validities are attenuated: the correction has real work to do.
	naive = c(cor(sel$COG, sel$PERF), cor(sel$INT, sel$PERF), cor(sel$BIO, sel$PERF))
	for (j in seq_along(truth)) {
		expect_true(naive[j] < truth[j] - 0.10)
	}

	m1 = umxRAM("battery", data = sel, autoRun = TRUE,
		umxPath(v.m. = c(tests, "PERF")),
		umxPath(unique.bivariate = c(tests, "PERF")),
		selection = list(varNames = tests, popCov = Rpop[tests, tests]))
	expect_equal(m1$output$status$code, 0)
	cc = coef(m1)
	v = m1$S$values
	got = c(cc[["COG_with_PERF"]], cc[["INT_with_PERF"]], cc[["BIO_with_PERF"]]) /
		sqrt(c(v["COG", "COG"], v["INT", "INT"], v["BIO", "BIO"]) * v["PERF", "PERF"])
	for (j in seq_along(truth)) {
		expect_equal(got[j], truth[j], tolerance = 0.05)
	}
	expect_true(any(sapply(m1$constraints, function(cc) cc$name) == "sel_popCov"))
	expect_equal(as.numeric(mxEval(impCov_sel, m1, compute = TRUE)),
		as.numeric(Rpop[tests, tests]), tolerance = 1e-3)

	# Vector form fits the identical model: same validities out.
	m3 = umxRAM("batteryVec", data = sel, autoRun = TRUE,
		umxPath(v.m. = c(tests, "PERF")),
		umxPath(unique.bivariate = c(tests, "PERF")),
		selection = list(varNames = tests, popCov = as.numeric(Rpop[tests, tests])))
	expect_equal(m3$output$status$code, 0)
	cc3 = coef(m3)
	v3 = m3$S$values
	got3 = c(cc3[["COG_with_PERF"]], cc3[["INT_with_PERF"]], cc3[["BIO_with_PERF"]]) /
		sqrt(c(v3["COG", "COG"], v3["INT", "INT"], v3["BIO", "BIO"]) * v3["PERF", "PERF"])
	expect_equal(got3, got)
})

test_that("umxRAM selection infers var and reorders popCov", {
	set.seed(100)
	nPop = 30000
	Rpop = matrix(c(1.00, 0.30, 0.50, 0.51,
	                0.30, 1.00, 0.25, 0.51,
	                0.50, 0.25, 1.00, 0.35,
	                0.51, 0.51, 0.35, 1.00), 4, 4)
	colnames(Rpop) = rownames(Rpop) = c("COG", "INT", "BIO", "PERF")
	pop = as.data.frame(matrix(rnorm(nPop * 4), ncol = 4) %*% chol(Rpop))
	names(pop) = c("COG", "INT", "BIO", "PERF")
	composite = pop$COG + pop$INT + pop$BIO
	sel = pop[composite > quantile(composite, 0.7), ]
	tests = c("COG", "INT", "BIO")
	obsSel = cov(sel[, tests])

	# No var: inferred from the popCov dimnames; starts track the selection plan.
	m2 = umxRAM("batteryInfer", data = sel, autoRun = FALSE,
		umxPath(v.m. = c(tests, "PERF")),
		umxPath(unique.bivariate = c(tests, "PERF")),
		selection = list(popCov = Rpop[tests, tests]))
	expect_true(any(sapply(m2$constraints, function(cc) cc$name) == "sel_popCov"))
	wantVech = numeric(0)
	for (j in seq_along(tests)) {
		for (i in seq(j, length(tests))) {
			wantVech = c(wantVech, Rpop[tests[i], tests[j]])
		}
	}
	expect_equal(as.numeric(m2$popCov_sel_vech$values), wantVech)
	sp = m2$expectation$selectionPlan
	for (r in seq_len(nrow(sp))) {
		expect_equal(m2$selectionVector$values[r, 1], obsSel[sp$from[r], sp$to[r]])
	}
	expect_true("sel_COG" %in% names(coef(m2)))
	expect_true("sel_BIO_COG" %in% names(coef(m2)))

	# Dimnames in another order: target is reordered to var order.
	m4 = umxRAM("batteryReorder", data = sel, autoRun = FALSE,
		umxPath(v.m. = c(tests, "PERF")),
		umxPath(unique.bivariate = c(tests, "PERF")),
		selection = list(varNames = tests, popCov = Rpop[rev(tests), rev(tests)]))
	expect_equal(as.numeric(m4$popCov_sel_vech$values), wantVech)

	# No dimnames: var supplies them.
	m5 = umxRAM("batteryAttach", data = sel, autoRun = FALSE,
		umxPath(v.m. = c(tests, "PERF")),
		umxPath(unique.bivariate = c(tests, "PERF")),
		selection = list(varNames = tests, popCov = unname(Rpop[tests, tests])))
	expect_equal(as.numeric(m5$popCov_sel_vech$values), wantVech)
})

test_that("umxRAM selection validates multi-var specs", {
	set.seed(7)
	tiny = data.frame(COG = rnorm(200), INT = rnorm(200), BIO = rnorm(200), PERF = rnorm(200))
	tests = c("COG", "INT", "BIO")
	okCov = matrix(c(1, .3, .5, .3, 1, .25, .5, .25, 1), 3, 3,
		dimnames = list(tests, tests))
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests, popCov = c(1, .3, .3, 1))), "length")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(popCov = c(1, .3, .3, 1))), "needs varNames")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests,
			popCov = { bad = okCov; rownames(bad)[3] = "XXX"; colnames(bad)[3] = "XXX"; bad })), "dimnames")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests[1:2],
			popCov = matrix(c(1, .2, .3, 1), 2, 2, dimnames = list(tests[1:2], tests[1:2])))), "symmetric")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = "COG", popVar = 1, popCov = okCov)), "varNames")
	# Unnamed 1-by-1 routes to the single-var path (sel_popVar constraint).
	oneByOne = umxRAM("o", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = "COG", popCov = matrix(1, 1, 1)))
	expect_true(any(sapply(oneByOne$constraints, function(cc) cc$name) == "sel_popVar"))
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests[1:2], popCov = matrix(1:6, 2, 3))), "square")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests[1:2],
			popCov = matrix(letters[1:4], 2, 2, dimnames = list(tests[1:2], tests[1:2])))), "numeric")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = c("COG", "INT", "NOPE"), popCov = okCov)), "dimnames")
	unknownCov = okCov
	rownames(unknownCov)[3] = "NOPE"
	colnames(unknownCov)[3] = "NOPE"
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = c("COG", "INT", "NOPE"), popCov = unknownCov)), "NOPE")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = c("COG", "COG", "INT"), popCov = okCov)), "distinct")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		mxPath(from = "COG", arrows = 0), autoRun = FALSE,
		selection = list(varNames = tests, popCov = okCov)), "arrows=0")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(popCov = 1.0)), "needs varNames")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests[1:2], popCov = c("a", "b", "c", "d"))), "numeric")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests, popCov = okCov), type = "WLS"), "WLS")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		autoRun = FALSE, selection = list(varNames = tests, popCov = okCov), group = "zyg"), "group")
	expect_error(umxRAM("v", data = tiny, umxPath(v.m. = c(tests, "PERF")),
		mxMatrix("Full", 1, 1, name = "sel_E"), autoRun = FALSE,
		selection = list(varNames = tests, popCov = okCov)), "already used")
})
