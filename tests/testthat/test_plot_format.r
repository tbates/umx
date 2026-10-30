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
