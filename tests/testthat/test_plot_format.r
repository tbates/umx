library(testthat)
library(umx)
library(OpenMx)

test_that("plot honors format for ACE models", {
	data(twinData)
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")
	m1 = umxACE("plotfmt_ace", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
	oldFormat = umx_set_plot_format(silent = TRUE)
	on.exit(umx_set_plot_format(oldFormat), add = TRUE)
	umx_set_plot_format("DiagrammeR")
	d = plot(m1, format = "graphviz", file = NA)
	expect_true(startsWith(d, "digraph"))
	expect_equal(umx_set_plot_format(silent = TRUE), "DiagrammeR")
	expect_error(plot(m1, format = "bogus", file = NA), "should be one of")
})

test_that("plot honors format for RAM models", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("plotfmt_ram", data = demoOneFactor[, v], type = "cov", autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	m1 = mxRun(m1, silent = TRUE)
	oldFormat = umx_set_plot_format(silent = TRUE)
	on.exit(umx_set_plot_format(oldFormat), add = TRUE)
	umx_set_plot_format("DiagrammeR")
	d = plot(m1, format = "graphviz", file = NA)
	expect_true(startsWith(d, "digraph"))
	expect_equal(umx_set_plot_format(silent = TRUE), "DiagrammeR")
	expect_error(plot(m1, format = "bogus", file = NA), "should be one of")
})

test_that("plot passes format through multigroup recursion", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(v.m. = v),
			umxPath(v1m0 = "G")
		)
	}
	mg = umxSuperModel("plotfmt_mg", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]))
	oldFormat = umx_set_plot_format(silent = TRUE)
	on.exit(umx_set_plot_format(oldFormat), add = TRUE)
	umx_set_plot_format("DiagrammeR")
	plot(mg, format = "graphviz", file = NA)
	expect_equal(umx_set_plot_format(silent = TRUE), "DiagrammeR")
})

test_that("plot returns one widget per submodel for multigroup models", {
	# Defect: multigroup branch discarded each submodel widget, so R.app showed the help notes but no diagrams
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(v.m. = v),
			umxPath(v1m0 = "G")
		)
	}
	mg = umxSuperModel("plotmg_widgets", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]))
	oldFormat = umx_set_plot_format(silent = TRUE)
	on.exit(umx_set_plot_format(oldFormat), add = TRUE)
	umx_set_plot_format("DiagrammeR")
	plots = plot(mg, format = "DiagrammeR")
	expect_equal(length(plots), 2)
	for(p in plots){
		expect_true(inherits(p, "htmlwidget"))
	}
})

test_that("multigroup plot emits one note naming plot.MxModel", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(v.m. = v),
			umxPath(v1m0 = "G")
		)
	}
	mg = umxSuperModel("plotmg_note", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]))
	oldFormat = umx_set_plot_format(silent = TRUE)
	on.exit(umx_set_plot_format(oldFormat), add = TRUE)
	umx_set_plot_format("DiagrammeR")
	nNotes = 0
	noteText = ""
	plots = withCallingHandlers(plot(mg, format = "DiagrammeR"),
		message = function(m){ nNotes <<- nNotes + 1; noteText <<- paste0(noteText, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_equal(length(plots), 2)
	expect_equal(nNotes, 1)
	expect_true(grepl("plot.MxModel", noteText, fixed = TRUE))
	expect_true(grepl("plot.MxModel(mg)", noteText, fixed = TRUE))
})

test_that("plot note names plot.MxModel for classes without a dedicated plotter", {
	# MxModelGSEM has no plot.* method, so plot() falls through to plot.MxModel
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(v.m. = v),
			umxPath(v1m0 = "G")
		)
	}
	mg = umxSuperModel("plotmg_gsemnote", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]))
	mg = as(mg, "MxModelGSEM")
	nNotes = 0
	noteText = ""
	plots = withCallingHandlers(plot(mg, format = "graphviz", file = NA),
		message = function(m){ nNotes <<- nNotes + 1; noteText <<- paste0(noteText, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_equal(length(plots), 2)
	expect_equal(nNotes, 1)
	expect_true(grepl("plot.MxModel", noteText, fixed = TRUE))
})

test_that("sketch builders return silently without auto-preview", {
	# Defect: sketch auto-preview plot()ed and discarded the widget (note but no diagram)
	oldAuto = umx_set_auto_plot(TRUE, silent = TRUE)
	on.exit(umx_set_auto_plot(oldAuto, silent = TRUE), add = TRUE)
	notes = character(0)
	m1 = withCallingHandlers(umxRAM("GDP ~ 0.7*N_billionaires"), message = function(m) {
		notes <<- c(notes, conditionMessage(m))
		invokeRestart("muffleMessage")
	})
	m2 = withCallingHandlers(umxRAM("sk", umxPath("G", to = c("x", "y")), umxPath(var = c("x", "y")), data = c("x", "y")), message = function(m) {
		notes <<- c(notes, conditionMessage(m))
		invokeRestart("muffleMessage")
	})
	expect_false(any(grepl("customise options", notes, fixed = TRUE)))
	expect_true(isS4(m1))
	expect_true(isS4(m2))
})
