library(testthat)
library(umx)
library(OpenMx)

# Fixes: lboundM targets the betas; starts follow the data; symmetric input
# checks; quiet build; summary labels betas by path, honors digits, returns
# the model, and drops the dead file parameter.
test_that("GxEbiv lboundM targets the beta paths", {
	set.seed(31)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 50, sd = 10)
	ht2 = rnorm(nFam, mean = 50, sd = 10)
	wt1 = rnorm(nFam, mean = 20, sd = 8)
	wt2 = rnorm(nFam, mean = 20, sd = 8)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	expect_true(is.na(m$top$aBeta1$lbound[[1]]))
	m2 = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE, lboundM = -0.5),
		message = function(m) invokeRestart("muffleMessage"))
	for (nm in c("aBeta1", "aBeta2", "cBeta1", "cBeta2", "eBeta1", "eBeta2")) {
		expect_equal(unname(m2$top[[nm]]$lbound[[1]]), -0.5)
	}
	expect_equal(unname(m2$top$a11$lbound[[1]]), 0)
})

test_that("GxEbiv checks var names in both datasets", {
	set.seed(32)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 50, sd = 10)
	ht2 = rnorm(nFam, mean = 50, sd = 10)
	wt1 = rnorm(nFam, mean = 20, sd = 8)
	wt2 = rnorm(nFam, mean = 20, sd = 8)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	mzBad = mz
	mzBad$wt1 = NULL
	expect_error(withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mzBad, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage")), "wt1")
	expect_error(withCallingHandlers(
		umxGxEbiv(selDVs = c("wt1", "wt2"), selDefs = "ht", dzData = dz, mzData = mz, sep = NULL, dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage")), "must be 2")
})

test_that("GxEbiv build is quiet", {
	set.seed(33)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 50, sd = 10)
	ht2 = rnorm(nFam, mean = 50, sd = 10)
	wt1 = rnorm(nFam, mean = 20, sd = 8)
	wt2 = rnorm(nFam, mean = 20, sd = 8)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	msgs = character(0)
	withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_false(any(grepl("selDVs:", msgs, fixed = TRUE)))
})

test_that("GxEbiv starts follow the data", {
	set.seed(34)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 50, sd = 10)
	ht2 = rnorm(nFam, mean = 50, sd = 10)
	wt1 = rnorm(nFam, mean = 20, sd = 8)
	wt2 = rnorm(nFam, mean = 20, sd = 8)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	modVar = var(mz$ht1)
	dvVar = var(mz$wt1)
	modMean = mean(c(mz$ht1, mz$ht2))
	dvMean = mean(c(mz$wt1, mz$wt2))
	expect_equal(unname(m$top$a11$values[[1]]), sqrt(0.8 * modVar), tolerance = 1e-8)
	expect_equal(unname(m$top$c11$values[[1]]), 0)
	expect_equal(unname(m$top$e11$values[[1]]), sqrt(0.6 * modVar), tolerance = 1e-8)
	expect_equal(unname(m$top$a21$values[[1]]), sqrt(0.8 * dvVar), tolerance = 1e-8)
	expect_equal(unname(m$top$a22$values[[1]]), sqrt(0.8 * dvVar), tolerance = 1e-8)
	expect_equal(unname(m$top$e22$values[[1]]), sqrt(0.6 * dvVar), tolerance = 1e-8)
	expect_equal(unname(m$top$aBeta1$values[[1]]), 0)
	expect_equal(unname(m$top$expMean$values[1, ]), c(modMean, dvMean, modMean, dvMean), tolerance = 1e-8)
})

test_that("GxEbiv 1-row build has finite starts", {
	oneMZ = data.frame(ht1 = 1.5, wt1 = 0, ht2 = 1.5, wt2 = 0)
	oneDZ = data.frame(ht1 = 1.5, wt1 = 0, ht2 = 1.5, wt2 = 0)
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = oneDZ, mzData = oneMZ, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	expect_true(all(is.finite(c(m$top$a11$values, m$top$a21$values, m$top$c11$values, m$top$expMean$values))))
})

test_that("GxEbiv summary has no file parameter", {
	expect_false("file" %in% names(formals(umxSummaryGxEbiv)))
})

# Shared fitted model for the summary assertions below (one small fit).
data(twinData)
gxebivDF = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
gxebivMZ = subset(gxebivDF, zygosity %in% c("MZFF", "MZMM"))[1:100, ]
gxebivDZ = subset(gxebivDF, zygosity %in% c("DZFF", "DZMM", "DZOS"))[1:100, ]
fitGxEbiv = withCallingHandlers(
	umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = gxebivDZ, mzData = gxebivMZ, sep = "", dropMissingDef = TRUE, autoRun = TRUE),
	message = function(m) invokeRestart("muffleMessage"))

test_that("GxEbiv summary labels betas by path", {
	msgs = character(0)
	out = capture.output(withCallingHandlers(umxSummaryGxEbiv(fitGxEbiv),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }))
	expect_true(any(grepl("common paths", msgs, fixed = TRUE)))
	expect_true(any(grepl("unique paths", msgs, fixed = TRUE)))
	expect_false(any(grepl("Betas on defVar", msgs, fixed = TRUE)))
})

test_that("GxEbiv summary honors digits", {
	out2 = capture.output(withCallingHandlers(umxSummaryGxEbiv(fitGxEbiv, digits = 2),
		message = function(m) invokeRestart("muffleMessage")))
	out4 = capture.output(withCallingHandlers(umxSummaryGxEbiv(fitGxEbiv, digits = 4),
		message = function(m) invokeRestart("muffleMessage")))
	i2 = grep("a1|a2", out2, fixed = TRUE)
	i4 = grep("a1|a2", out4, fixed = TRUE)
	expect_equal(length(i2), 1)
	expect_equal(length(i4), 1)
	expect_false(identical(out2[i2:(i2 + 3)], out4[i4:(i4 + 3)]))
})

test_that("GxEbiv summary notice omits comparison", {
	msgs = character(0)
	out = capture.output(withCallingHandlers(umxSummaryGxEbiv(fitGxEbiv, std = TRUE),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }))
	notice = msgs[grepl("not yet implemented", msgs, fixed = TRUE)]
	expect_equal(length(notice), 1)
	expect_false(any(grepl("comparison", notice, fixed = TRUE)))
})

test_that("GxEbiv summary returns the model", {
	out = capture.output(res <- withCallingHandlers(umxSummaryGxEbiv(fitGxEbiv),
		message = function(m) invokeRestart("muffleMessage")))
	expect_true(is(res, "MxModelGxEbiv"))
})
