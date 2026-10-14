# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxCompare_bestAIC.R")

require(umx)
umx_set_silent(TRUE)
data(demoOneFactor)
manifests = names(demoOneFactor)
m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
	umxPath("G", to = manifests),
	umxPath(var = manifests),
	umxPath(var = "G", fixedAt = 1)
)
m2 = umxModify(m1, update = "G_to_x2", name = "drop_x2")
m3 = umxModify(m1, update = "x5_with_x5", value = .1, name = "fix_e5")
quiet = function(expr) { out = NULL; invisible(capture.output(suppressMessages(out <- expr))); out }

test_that("umxCompare returns the best model by AIC when compareWeightedAIC = TRUE", {
	best = quiet(umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE))
	expect_s4_class(best, "MxModel")
	aics = sapply(list(m1, m2, m3), AIC)
	expect_equal(best$name, c(m1$name, m2$name, m3$name)[which.min(aics)])

	# The base itself can be the best model
	expect_equal(quiet(umxCompare(m1, m2, compareWeightedAIC = TRUE))$name, m1$name)

	# Still prints the weights unless silent
	expect_output(umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE), "best fitting model according to AIC")
	expect_silent(best2 <- umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE, silent = TRUE))
	expect_equal(best2$name, best$name)
})

test_that("umxCompare still returns the table by default", {
	tbl = quiet(umxCompare(m1, c(m2, m3)))
	expect_s3_class(tbl, "data.frame")
	expect_equal(nrow(tbl), 3)
})
