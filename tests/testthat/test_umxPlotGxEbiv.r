library(testthat)
library(umx)
library(OpenMx)

# Defects: umxPlotGxEbiv summed Cholesky paths before squaring (univariate
# formula on a two-path model) and pooled the moderator axis from columns 3:4
# (twin-2 moderator AND twin-2 trait). Tests use unrun models with known
# parameters: no fitting needed.
test_that("GxEbiv moderator values come from the def columns only", {
	set.seed(21)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 100, sd = 1)
	ht2 = rnorm(nFam, mean = 100, sd = 1)
	wt1 = rnorm(nFam)
	wt2 = rnorm(nFam)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	vals = xmu_GxEbiv_moderatorValues(m)
	expect_true(min(vals) > 50)
	expect_true(max(vals) < 150)
})

test_that("GxEbiv variances sum squares per Cholesky path", {
	set.seed(22)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 100, sd = 1)
	ht2 = rnorm(nFam, mean = 100, sd = 1)
	wt1 = rnorm(nFam)
	wt2 = rnorm(nFam)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	labs = c("a21_r1c1", "a22_r1c1", "aBeta1_r1c1", "aBeta2_r1c1",
		"c21_r1c1", "c22_r1c1", "cBeta1_r1c1", "cBeta2_r1c1",
		"e21_r1c1", "e22_r1c1", "eBeta1_r1c1", "eBeta2_r1c1")
	m = omxSetParameters(m, labels = labs,
		values = c(0.5, 0.3, 0.1, 0.2, 0.4, 0.2, 0, 0.1, 0.3, 0.4, 0.2, 0))
	mv = c(-1, 0, 2)
	comp = xmu_GxEbiv_variances(m, mv)
	wantVa = (0.5 + 0.1 * mv)^2 + (0.3 + 0.2 * mv)^2
	wantVc = (0.4 + 0 * mv)^2 + (0.2 + 0.1 * mv)^2
	wantVe = (0.3 + 0.2 * mv)^2 + (0.4 + 0 * mv)^2
	wantVt = wantVa + wantVc + wantVe
	expect_equal(unname(comp$raw[, "Va"]), wantVa, tolerance = 1e-8)
	expect_equal(unname(comp$raw[, "Vc"]), wantVc, tolerance = 1e-8)
	expect_equal(unname(comp$raw[, "Ve"]), wantVe, tolerance = 1e-8)
	expect_equal(unname(comp$raw[, "Vt"]), wantVt, tolerance = 1e-8)
	expect_equal(unname(comp$std[, 1]), wantVa / wantVt, tolerance = 1e-8)
	expect_equal(unname(comp$std[, 2]), wantVc / wantVt, tolerance = 1e-8)
	expect_equal(unname(comp$std[, 3]), wantVe / wantVt, tolerance = 1e-8)
})

test_that("GxEbiv variances match the fitted-model algebras", {
	oneMZ = data.frame(ht1 = 1.5, wt1 = 0, ht2 = 1.5, wt2 = 0)
	oneDZ = data.frame(ht1 = 1.5, wt1 = 0, ht2 = 1.5, wt2 = 0)
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = oneDZ, mzData = oneMZ, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	labs = c("a21_r1c1", "a22_r1c1", "aBeta1_r1c1", "aBeta2_r1c1",
		"c21_r1c1", "c22_r1c1", "cBeta1_r1c1", "cBeta2_r1c1",
		"e21_r1c1", "e22_r1c1", "eBeta1_r1c1", "eBeta2_r1c1")
	m = omxSetParameters(m, labels = labs,
		values = c(0.5, 0.3, 0.1, 0.2, 0.4, 0.2, 0, 0.1, 0.3, 0.4, 0.2, 0))
	m = omxSetParameters(m, labels = names(omxGetParameters(m)), free = FALSE)
	r = mxRun(m, silent = TRUE)
	comp = xmu_GxEbiv_variances(r, 1.5)
	expect_equal(unname(r$MZ$Amz$result[2, 2]), unname(comp$raw[, "Va"]), tolerance = 1e-6)
	expect_equal(unname(r$MZ$C$result[2, 2]), unname(comp$raw[, "Vc"]), tolerance = 1e-6)
	expect_equal(unname(r$MZ$E$result[2, 2]), unname(comp$raw[, "Ve"]), tolerance = 1e-6)
	expect_equal(unname(r$MZ$expCovMZ$result[2, 2]), unname(comp$raw[, "Vt"]), tolerance = 1e-6)
})

test_that("GxEbiv plot runs with no alpha notice", {
	set.seed(23)
	nFam = 60
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	ht1 = rnorm(nFam, mean = 100, sd = 1)
	ht2 = rnorm(nFam, mean = 100, sd = 1)
	wt1 = rnorm(nFam)
	wt2 = rnorm(nFam)
	data = data.frame(zygosity = zygosity, ht1 = ht1, ht2 = ht2, wt1 = wt1, wt2 = wt2)
	mz = subset(data, zygosity %in% c("MZFF", "MZMM"))
	dz = subset(data, zygosity %in% c("DZFF", "DZMM"))
	m = withCallingHandlers(
		umxGxEbiv(selDVs = "wt", selDefs = "ht", dzData = dz, mzData = mz, sep = "", dropMissingDef = TRUE, autoRun = FALSE),
		message = function(m) invokeRestart("muffleMessage"))
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	msgs = character(0)
	withCallingHandlers(umxPlotGxEbiv(m),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_false(any(grepl("early alpha", msgs, fixed = TRUE)))
})
