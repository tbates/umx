library(testthat)
library(umx)
library(OpenMx)

# Validation: umxDiffMZ lifted from pre-alpha. Locks the annotated beta/R2
# against an independent lm, MZ-only plot data, xylim, known-slope recovery,
# and honest input guards.
test_that("umxDiffMZ annotates the MZ diff-score regression", {
	data(twinData)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	msgs = character(0)
	p = withCallingHandlers(
		umxDiffMZ(x = "ht", y = "wt", labxy = c(-0.5, 3), data = twinData, sep = ""),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
	expect_s3_class(p, "ggplot")
	expect_false(any(grepl("pre-alpha", msgs, fixed = TRUE)))
	expect_true(all(p$data[, "zygosity"] %in% c("MZFF", "MZMM")))
	df = umx_scale_wide_twin_data(varsToScale = c("ht", "wt"), sep = "", data = twinData, twins = 1:2)
	df$xDiff = df[, "ht1"] - df[, "ht2"]
	df$yDiff = df[, "wt1"] - df[, "wt2"]
	mz = df[df[, "zygosity"] %in% c("MZFF", "MZMM"), ]
	fit = lm(yDiff ~ xDiff, data = mz)
	wantBeta = round(coef(fit)[["xDiff"]], 2)
	wantR2 = round(summary(fit)$r.squared, 3)
	labels = character(0)
	for (l in p$layers) {
		if (!is.null(l$aes_params$label)) {
			labels = c(labels, as.character(l$aes_params$label))
		}
	}
	expect_true(any(grepl(paste0('"', wantBeta, ' ['), labels, fixed = TRUE)))
	expect_true(any(grepl(paste0("R2 = ", wantR2), labels, fixed = TRUE)))
})

test_that("umxDiffMZ honors xylim", {
	data(twinData)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	p = withCallingHandlers(
		umxDiffMZ(x = "ht", y = "wt", xylim = c(-2, 2), data = twinData, sep = ""),
		message = function(m) invokeRestart("muffleMessage"))
	expect_equal(p$coordinates$limits$x, c(-2, 2))
	expect_equal(p$coordinates$limits$y, c(-2, 2))
})

test_that("umxDiffMZ recovers a known within-pair slope", {
	set.seed(5)
	nFam = 800
	zygosity = rep(c("MZFF", "MZMM"), nFam / 2)
	famX = rnorm(nFam)
	x_T1 = famX + rnorm(nFam, sd = 2)
	x_T2 = famX + rnorm(nFam, sd = 2)
	y_T1 = 0.5 * x_T1 + rnorm(nFam, sd = 0.1)
	y_T2 = 0.5 * x_T2 + rnorm(nFam, sd = 0.1)
	data = data.frame(FAMID = 1:nFam, zygosity = zygosity, x_T1 = x_T1, x_T2 = x_T2,
		y_T1 = y_T1, y_T2 = y_T2)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	p = withCallingHandlers(
		umxDiffMZ(x = "x", y = "y", data = data),
		message = function(m) invokeRestart("muffleMessage"))
	labels = character(0)
	for (l in p$layers) {
		if (!is.null(l$aes_params$label)) {
			labels = c(labels, as.character(l$aes_params$label))
		}
	}
	gotBeta = as.numeric(sub('.*beta == "([0-9.]+) .*', "\\1", labels))
	gotBeta = gotBeta[!is.na(gotBeta)][1]
	sdx = sd(c(x_T1, x_T2))
	sdy = sd(c(y_T1, y_T2))
	expect_true(abs(gotBeta - 0.5 * sdx / sdy) < 0.02)
})

test_that("umxDiffMZ guards inputs", {
	data(twinData)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	expect_error(withCallingHandlers(
		umxDiffMZ(x = "ht", y = "wt", data = twinData, sep = "", zyg = "missing_col"),
		message = function(m) invokeRestart("muffleMessage")), "zygosity")
	dzOnly = twinData[twinData[, "zygosity"] %in% c("DZFF", "DZMM", "DZOS"), ]
	expect_error(withCallingHandlers(
		umxDiffMZ(x = "ht", y = "wt", data = dzOnly, sep = ""),
		message = function(m) invokeRestart("muffleMessage")), "MZ")
})
