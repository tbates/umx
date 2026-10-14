# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_run.R")

require(umx)
data(demoOneFactor)
manifests = names(demoOneFactor)

m1 = umxRAM("One Factor", data = demoOneFactor, autoRun = FALSE,
	umxPath("G", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "G")
)

set.seed(1)
dd = demoOneFactor
dd$age = rnorm(nrow(dd))
m_def = umxRAM("defvar", data = dd, autoRun = FALSE,
	umxPath("G", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "G"),
	umxPath(defn = "def", labels = "data.age"),
	umxPath("def", to = "x1", labels = "b_age")
)

test_that("umx_run picks the run function and runs the model", {
	expect_message(r1 <- umx_run(m1), "with mxRun")
	expect_equal(r1$output$status$code, 0)
	expect_equal(r1$output$fit, mxRun(m1, silent = TRUE)$output$fit, tolerance = 1e-4)

	expect_message(umx_run(m_def), "with mxTryHard")
	expect_message(umx_run(m_def, smart = "off"), "with mxRun")
	expect_message(umx_run(m1, smart = "mxTryHard"), "with mxTryHard")
})

test_that("umx_run does not re-run a model that has been run", {
	r1 = suppressMessages(umx_run(m1))
	expect_silent(r2 <- umx_run(r1))
	expect_identical(r2$output$fit, r1$output$fit)
})
