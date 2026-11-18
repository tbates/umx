library(testthat)
library(umx)
library(OpenMx)

# Defect: umxDiscTwin hardcoded data$zyg and lme(IQ ~ SOSeffort ...) stubs:
# any x != "SOSeffort" errored, any y != "IQ" was silently ignored, and the
# zygosity column could not be renamed.
test_that("umxDiscTwin consumes x and y", {
	msgs = character(0)
	withCallingHandlers({
		set.seed(2)
		nFam = 300
		zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
		famX1 = rnorm(nFam)
		x1_T1 = famX1 + rnorm(nFam, sd = 2)
		x1_T2 = famX1 + rnorm(nFam, sd = 2)
		famX2 = rnorm(nFam)
		x2_T1 = famX2 + rnorm(nFam, sd = 2)
		x2_T2 = famX2 + rnorm(nFam, sd = 2)
		y_T1 = 0.8 * x1_T1 + rnorm(nFam)
		y_T2 = 0.8 * x1_T2 + rnorm(nFam)
		noiseY_T1 = rnorm(nFam)
		noiseY_T2 = rnorm(nFam)
		data = data.frame(FAMID = 1:nFam, zygosity = zygosity, x1_T1 = x1_T1, x1_T2 = x1_T2,
			x2_T1 = x2_T1, x2_T2 = x2_T2, y_T1 = y_T1, y_T2 = y_T2,
			noiseY_T1 = noiseY_T1, noiseY_T2 = noiseY_T2)
		pdf(NULL)
		on.exit(dev.off(), add = TRUE)
		outX1 = umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T")
		expect_equal(as.character(outX1$xLevel), c("Pop", "DZ", "MZ"))
		bX1 = outX1$Bwithin[outX1$xLevel == "MZ"]
		outX2 = umxDiscTwin(x = "x2", y = "y", data = data, out = "table", sep = "_T")
		bX2 = outX2$Bwithin[outX2$xLevel == "MZ"]
		expect_true(bX1 > 0.4)
		expect_true(abs(bX2) < 0.2)
		outNoise = umxDiscTwin(x = "x1", y = "noiseY", data = data, out = "table", sep = "_T")
		bNoise = outNoise$Bwithin[outNoise$xLevel == "MZ"]
		expect_true(abs(bX1 - bNoise) > 0.3)
	}, message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_false(any(grepl("Assuming twin data", msgs, fixed = TRUE)))
})

test_that("umxDiscTwin zygosity column is parameterized", {
	withCallingHandlers({
		set.seed(3)
		nFam = 300
		zyg = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
		famX = rnorm(nFam)
		x1_T1 = famX + rnorm(nFam, sd = 2)
		x1_T2 = famX + rnorm(nFam, sd = 2)
		y_T1 = 0.8 * x1_T1 + rnorm(nFam)
		y_T2 = 0.8 * x1_T2 + rnorm(nFam)
		data = data.frame(FAMID = 1:nFam, zyg = zyg, x1_T1 = x1_T1, x1_T2 = x1_T2,
			y_T1 = y_T1, y_T2 = y_T2)
		pdf(NULL)
		on.exit(dev.off(), add = TRUE)
		outZ = umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", zyg = "zyg")
		expect_equal(as.character(outZ$xLevel), c("Pop", "DZ", "MZ"))
		expect_true(outZ$Bwithin[outZ$xLevel == "MZ"] > 0.4)
		expect_error(umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", zyg = "missing_col"))
	}, message = function(m) invokeRestart("muffleMessage"))
})
