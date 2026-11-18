library(testthat)
library(umx)
library(OpenMx)

# Defect: umxDiscTwin legend paired breaks c("Pop","MZ","DZ") with labels
# c("Unselected","DZ within family","MZ within family"), tagging the MZ
# swatch as DZ and vice versa. X-axis order (Pop, DZ, MZ) was correct.
test_that("umxDiscTwin legend tags each swatch with its own group", {
	set.seed(1)
	nFam = 120
	zyg = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	famX = rnorm(nFam)
	SOSeffort_T1 = famX + rnorm(nFam, sd = 0.5)
	SOSeffort_T2 = famX + rnorm(nFam, sd = 0.5)
	IQ_T1 = 0.5 * SOSeffort_T1 + rnorm(nFam)
	IQ_T2 = 0.5 * SOSeffort_T2 + rnorm(nFam)
	data = data.frame(FAMID = 1:nFam, zyg = zyg, SOSeffort_T1 = SOSeffort_T1,
		SOSeffort_T2 = SOSeffort_T2, IQ_T1 = IQ_T1, IQ_T2 = IQ_T2)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	p = withCallingHandlers(umxDiscTwin(x = "SOSeffort", y = "IQ", data = data, out = "plot", sep = "_T", zyg = "zyg"),
		message = function(m) invokeRestart("muffleMessage"))
	expect_s3_class(p, "ggplot")
	expect_equal(levels(p$data$xLevel), c("Pop", "DZ", "MZ"))
	fillScale = p$scales$get_scales("fill")
	expect_equal(length(fillScale$breaks), 3)
	dzLabel = fillScale$labels[fillScale$breaks == "DZ"]
	mzLabel = fillScale$labels[fillScale$breaks == "MZ"]
	expect_true(grepl("DZ", dzLabel, fixed = TRUE))
	expect_true(grepl("MZ", mzLabel, fixed = TRUE))
	expect_false(grepl("MZ", dzLabel, fixed = TRUE))
	expect_false(grepl("DZ", mzLabel, fixed = TRUE))
})
