# Constraint rows change df without changing the EP column.
library(testthat)
library(umx)

set.seed(1)
n = 80
x1 = rnorm(n)
x2 = 0.6 * x1 + rnorm(n)
dat = data.frame(x1 = x1, x2 = x2)

freeModel = mxModel("free", type = "RAM",
	manifestVars = c("x1", "x2"),
	mxPath(from = "x1", to = "x2", arrows = 1, free = TRUE, values = 0.5, labels = "b"),
	mxPath(from = c("x1", "x2"), arrows = 2, free = TRUE, values = 1, labels = c("v1", "v2")),
	mxPath(from = "one", to = c("x1", "x2"), free = TRUE, values = 0.1, labels = c("m1", "m2")),
	mxData(observed = dat, type = "raw")
)
freeModel = mxRun(freeModel, silent = TRUE)

nestedModel = mxModel("nested", type = "RAM",
	manifestVars = c("x1", "x2"),
	mxPath(from = "x1", to = "x2", arrows = 1, free = FALSE, values = 0, labels = "b"),
	mxPath(from = c("x1", "x2"), arrows = 2, free = TRUE, values = 1, labels = c("v1", "v2")),
	mxPath(from = "one", to = c("x1", "x2"), free = TRUE, values = 0.1, labels = c("m1", "m2")),
	mxData(observed = dat, type = "raw")
)
nestedModel = mxRun(nestedModel, silent = TRUE)

constrainedModel = mxModel(freeModel, name = "constrained",
	mxConstraint(m1 == m2, name = "meansEqual")
)
constrainedModel = mxRun(constrainedModel, silent = TRUE)

constrainedSummary = summary(constrainedModel)
nConstraints = sum(constrainedSummary$constraints)
epLive = constrainedSummary$estimatedParameters
epEffective = epLive - nConstraints
notePattern = paste0(
	"Note: df for 'constrained' reflects ", nConstraints, " constraint",
	if (abs(nConstraints) == 1) "" else "s",
	": ", epLive, " estimated parameters \\(", epEffective, " effective after constraints\\)."
)

test_that("a model with an equality constraint gets one note", {
	expect_true(nConstraints > 0)
	out = umxCompare(freeModel, constrainedModel, silent = TRUE)
	expect_equal(out$EP[out$Model == "constrained"], epLive)
	notes = attr(out, "constraintNotes")
	expect_length(notes, 1)
	expect_match(notes, notePattern)
	expect_false(grepl("free", notes))
})

test_that("constraint-free models produce no note", {
	out = umxCompare(freeModel, nestedModel, silent = TRUE)
	expect_null(attr(out, "constraintNotes"))
	printed = capture.output(umxCompare(freeModel, nestedModel))
	expect_false(any(grepl("reflects", printed)))
})

test_that("markdown, html, and inline all print the note", {
	markdownOut = capture.output(umxCompare(freeModel, constrainedModel, report = "markdown"))
	htmlOut = capture.output(umxCompare(freeModel, constrainedModel, report = "html"))
	inlineOut = capture.output(umxCompare(freeModel, constrainedModel, report = "inline"))
	expect_true(any(grepl(notePattern, markdownOut)))
	expect_true(any(grepl(notePattern, htmlOut)))
	expect_true(any(grepl(notePattern, inlineOut)))
})

test_that("an unrun model is skipped by the note helper", {
	unrunModel = mxModel("unrun", type = "RAM",
		manifestVars = "x1",
		mxPath(from = "x1", arrows = 2, free = TRUE, values = 1),
		mxData(observed = dat[, "x1", drop = FALSE], type = "raw")
	)
	expect_false(umx_has_been_run(unrunModel))
	notes = umx:::xmu_compare_constraint_note(list(unrunModel, constrainedModel))
	expect_length(notes, 1)
	expect_match(notes, "constrained")
})

test_that("CP identification constraints are noted against an IP model", {
	skip_on_cran()
	data(GFF, package = "umx")
	selDVs = c("gff", "fc")
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")
	cpModel = umxCP("CP", selDVs = selDVs, sep = "_T", nFac = 1, dzData = dzData, mzData = mzData, tryHard = "no", autoRun = TRUE)
	ipModel = umxIP("IP", selDVs = selDVs, sep = "_T", nFac = c(a = 1, c = 1, e = 1), dzData = dzData, mzData = mzData, tryHard = "no", autoRun = TRUE)
	cpSummary = summary(cpModel)
	nCp = sum(cpSummary$constraints)
	expect_true(nCp > 0)
	out = umxCompare(ipModel, cpModel, silent = TRUE)
	notes = attr(out, "constraintNotes")
	expect_true(any(grepl(paste0("^Note: df for '", cpModel$name, "'"), notes)))
	cpRow = out[out$Model == cpModel$name, ]
	ipRow = out[out$Model == ipModel$name, ]
	expect_equal(cpRow$EP, cpSummary$estimatedParameters)
	# df_CP - df_IP = nConstraints - EP_CP + EP_IP, so the effective count explains Δ df.
	expect_equal(cpRow$EP - nCp, ipRow$EP - cpRow[["\u0394 df"]])
})
