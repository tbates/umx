# Collate separate umxCompare() tables without recomputing within-call deltas.
library(testthat)
library(umx)

test_that("umx_table_collate stacks matching tables and refuses a mismatch", {
	oldStore = getOption("umx_table_collate")
	on.exit(options(umx_table_collate = oldStore), add = TRUE)
	options(umx_table_collate = NULL)

	expect_message(umx_table_collate("show"), "No comparison table store is active")
	expect_error(umx_table_collate("add", table = data.frame(Model = "m1"), engine = "ML"), "init")
	expect_null(getOption("umx_table_collate"))
	expect_error(umx_table_collate("explode"), "argument must be one of")

	empty = umx_table_collate("init")
	expect_true(is.data.frame(empty))
	expect_equal(nrow(empty), 0)
	expect_false(is.null(getOption("umx_table_collate")))
	expect_message(umx_table_collate("show"), "store is empty")

	one = data.frame(Model = c("m1", "m2"), EP = c(4, 3), stringsAsFactors = FALSE)
	attr(one, "constraintNotes") = "Note: df for 'm2' reflects 1 constraint: 4 estimated parameters (3 effective after constraints)."
	attr(one, "robustScalingFactors") = list(m2 = 1.25)
	two = data.frame(Model = c("m2", "m3"), EP = c(3, 2), stringsAsFactors = FALSE)
	attr(two, "constraintNotes") = attr(one, "constraintNotes")
	attr(two, "robustScalingFactors") = list(m3 = 1.10)

	added = umx_table_collate("add", table = one, engine = "MLR")
	expect_equal(nrow(added), 2)
	added = umx_table_collate("add", table = two, engine = "MLR")
	expect_equal(added$Model, c("m1", "m2", "m2", "m3"))
	expect_equal(added$EP, c(4, 3, 3, 2))
	expect_equal(attr(added, "constraintNotes"), attr(one, "constraintNotes"))
	expect_equal(attr(added, "robustScalingFactors"), list(m2 = 1.25, m3 = 1.10))

	printed = capture.output(shown <- umx_table_collate("show"))
	expect_equal(shown$EP, c(4, 3, 3, 2))
	expect_true(any(grepl("call order", printed, fixed = TRUE)))
	expect_true(any(grepl("c_d = 1.250", printed, fixed = TRUE)))
	expect_true(any(grepl("c_d = 1.100", printed, fixed = TRUE)))
	expect_true(any(grepl("reflects 1 constraint", printed, fixed = TRUE)))
	capture.output(stillThere <- umx_table_collate("show"))
	expect_equal(nrow(stillThere), 4)

	alias = capture.output(printedAgain <- umx_table_collate("print"))
	expect_equal(printedAgain$Model, shown$Model)
	expect_true(length(alias) > 0)

	badColumns = data.frame(Model = "x", Chi = 1, stringsAsFactors = FALSE)
	expect_error(umx_table_collate("add", table = badColumns, engine = "WLS"), "cannot stack")
	capture.output(afterBadColumns <- umx_table_collate("show"))
	expect_equal(nrow(afterBadColumns), 4)

	expect_error(umx_table_collate("add", table = one, engine = "ML"), "storing MLR")
	capture.output(afterBadEngine <- umx_table_collate("show"))
	expect_equal(nrow(afterBadEngine), 4)

	conflict = two
	attr(conflict, "robustScalingFactors") = list(m2 = 9)
	expect_error(umx_table_collate("add", table = conflict, engine = "MLR"), "different robust scaling factor")
	capture.output(afterConflict <- umx_table_collate("show"))
	expect_equal(nrow(afterConflict), 4)

	expect_error(umx_table_collate("add", table = "nope", engine = "MLR"), "data.frame")
	expect_error(umx_table_collate("add", table = one, engine = "bayes"), "must be one of")

	umx_table_collate("init")
	expect_message(umx_table_collate("show"), "store is empty")
	expect_null(umx_table_collate("off"))
	expect_null(getOption("umx_table_collate"))
})

test_that("umxCompare appends each call and keeps that call's delta AIC", {
	oldStore = getOption("umx_table_collate")
	on.exit(options(umx_table_collate = oldStore), add = TRUE)
	options(umx_table_collate = NULL)

	set.seed(1)
	n = 80
	x1 = rnorm(n)
	x2 = 0.6 * x1 + rnorm(n)
	dat = data.frame(x1 = x1, x2 = x2)

	free = mxModel("free", type = "RAM",
		manifestVars = c("x1", "x2"),
		mxPath(from = "x1", to = "x2", arrows = 1, free = TRUE, values = 0.5, labels = "b"),
		mxPath(from = c("x1", "x2"), arrows = 2, free = TRUE, values = 1, labels = c("v1", "v2")),
		mxPath(from = "one", to = c("x1", "x2"), free = TRUE, values = 0.1, labels = c("m1", "m2")),
		mxData(observed = dat, type = "raw")
	)
	free = mxRun(free, silent = TRUE)

	dropB = mxModel("dropB", type = "RAM",
		manifestVars = c("x1", "x2"),
		mxPath(from = "x1", to = "x2", arrows = 1, free = FALSE, values = 0, labels = "b"),
		mxPath(from = c("x1", "x2"), arrows = 2, free = TRUE, values = 1, labels = c("v1", "v2")),
		mxPath(from = "one", to = c("x1", "x2"), free = TRUE, values = 0.1, labels = c("m1", "m2")),
		mxData(observed = dat, type = "raw")
	)
	dropB = mxRun(dropB, silent = TRUE)

	dropMeans = mxModel("dropMeans", type = "RAM",
		manifestVars = c("x1", "x2"),
		mxPath(from = "x1", to = "x2", arrows = 1, free = FALSE, values = 0, labels = "b"),
		mxPath(from = c("x1", "x2"), arrows = 2, free = TRUE, values = 1, labels = c("v1", "v2")),
		mxPath(from = "one", to = c("x1", "x2"), free = FALSE, values = 0, labels = c("m1", "m2")),
		mxData(observed = dat, type = "raw")
	)
	dropMeans = mxRun(dropMeans, silent = TRUE)

	constrained = mxModel(free, name = "constrained",
		mxConstraint(m1 == m2, name = "meansEqual")
	)
	constrained = mxRun(constrained, silent = TRUE)

	call1 = umxCompare(free, dropB, silent = TRUE)
	call2 = umxCompare(dropB, dropMeans, silent = TRUE)
	expect_null(getOption("umx_table_collate"))

	umx_table_collate("init")
	returned = umxCompare(free, dropB, silent = TRUE)
	expect_equal(nrow(returned), nrow(call1))
	expect_equal(returned[["\u0394 AIC"]], call1[["\u0394 AIC"]])
	livePrint = capture.output(umxCompare(dropB, dropMeans))
	expect_true(any(grepl("dropMeans", livePrint, fixed = TRUE)))

	capture.output(stacked <- umx_table_collate("show"))
	expect_equal(stacked$Model, c("free", "dropB", "dropB", "dropMeans"))
	expect_equal(stacked[["\u0394 AIC"]], c(call1[["\u0394 AIC"]], call2[["\u0394 AIC"]]))
	expect_equal(stacked[["\u0394 Fit"]], c(call1[["\u0394 Fit"]], call2[["\u0394 Fit"]]))
	expect_equal(stacked$p, c(call1$p, call2$p))
	expect_true(is.na(stacked[["Compare with Model"]][1]))
	expect_equal(stacked[["Compare with Model"]][2], "free")
	expect_true(is.na(stacked[["Compare with Model"]][3]))
	expect_equal(stacked[["Compare with Model"]][4], "dropB")
	expect_equal(stacked$AIC, c(call1$AIC, call2$AIC))

	capture.output(again <- umx_table_collate("show"))
	expect_equal(again$AIC, stacked$AIC)

	umx_table_collate("init")
	noted1 = umxCompare(free, constrained, silent = TRUE)
	noted2 = umxCompare(constrained, dropB, silent = TRUE)
	notes1 = attr(noted1, "constraintNotes")
	notes2 = attr(noted2, "constraintNotes")
	if (is.null(notes1)) {
		notes1 = character()
	}
	if (is.null(notes2)) {
		notes2 = character()
	}
	notePrint = capture.output(notedStack <- umx_table_collate("show"))
	expect_equal(attr(notedStack, "constraintNotes"), unique(c(notes1, notes2)))
	expect_true(length(attr(notedStack, "constraintNotes")) >= 1)
	expect_true(any(grepl("constrained", notePrint, fixed = TRUE)))

	umx_table_collate("off")
	umxCompare(free, dropB, silent = TRUE)
	expect_null(getOption("umx_table_collate"))
})
