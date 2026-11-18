library(testthat)
library(umx)
library(OpenMx)

# Lift: umxDiscTwin prints a markdown results table by default (APA p-values,
# digits-controlled rounding), returns the raw table invisibly, reports real N,
# and drops the pre-alpha notice and dead use parameter.
test_that("umxDiscTwin prints a markdown table by default", {
	set.seed(11)
	nFam = 300
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	famX1 = rnorm(nFam)
	x1_T1 = famX1 + rnorm(nFam, sd = 2)
	x1_T2 = famX1 + rnorm(nFam, sd = 2)
	y_T1 = 0.8 * x1_T1 + rnorm(nFam)
	y_T2 = 0.8 * x1_T2 + rnorm(nFam)
	data = data.frame(FAMID = 1:nFam, zygosity = zygosity, x1_T1 = x1_T1, x1_T2 = x1_T2,
		y_T1 = y_T1, y_T2 = y_T2)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	msgs = character(0)
	out = capture.output(res <- withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T"),
		message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }))
	expect_false(any(grepl("pre-alpha", msgs, fixed = TRUE)))
	expect_true(any(grepl("|", out, fixed = TRUE)))
	expect_true(any(grepl("Bwithin", out, fixed = TRUE)))
	expect_true(any(grepl("< 0.001", out, fixed = TRUE)))
	expect_true(is.numeric(res$Bwithin))
	expect_true(is.numeric(res$pval))
	expect_equal(res$N[res$xLevel == "Pop"], 2 * nFam)
	expect_equal(res$N[res$xLevel == "DZ"], 2 * sum(zygosity %in% c("DZFF", "DZMM")))
	expect_equal(res$N[res$xLevel == "MZ"], 2 * sum(zygosity %in% c("MZFF", "MZMM")))
	out4 = capture.output(res4 <- withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", digits = 4),
		message = function(m) invokeRestart("muffleMessage")))
	want4 = as.character(round(res4$Bwithin[res4$xLevel == "MZ"], 4))
	expect_true(any(grepl(want4, out4, fixed = TRUE)))
})

test_that("umxDiscTwin report=none is silent and use= is gone", {
	set.seed(12)
	nFam = 100
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	famX1 = rnorm(nFam)
	x1_T1 = famX1 + rnorm(nFam, sd = 2)
	x1_T2 = famX1 + rnorm(nFam, sd = 2)
	y_T1 = 0.8 * x1_T1 + rnorm(nFam)
	y_T2 = 0.8 * x1_T2 + rnorm(nFam)
	data = data.frame(FAMID = 1:nFam, zygosity = zygosity, x1_T1 = x1_T1, x1_T2 = x1_T2,
		y_T1 = y_T1, y_T2 = y_T2)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	out = capture.output(res <- withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", report = "none"),
		message = function(m) invokeRestart("muffleMessage")))
	expect_equal(length(out), 0)
	vis = withVisible(withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", report = "none"),
		message = function(m) invokeRestart("muffleMessage")))
	expect_false(vis$visible)
	expect_error(withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", use = "complete.obs"),
		message = function(m) invokeRestart("muffleMessage")))
})

test_that("umxDiscTwin reports df, returns all models, labels y", {
	set.seed(13)
	nFam = 150
	zygosity = sample(c("MZFF", "MZMM", "DZFF", "DZMM"), nFam, replace = TRUE)
	famX1 = rnorm(nFam)
	x1_T1 = famX1 + rnorm(nFam, sd = 2)
	x1_T2 = famX1 + rnorm(nFam, sd = 2)
	y_T1 = 0.8 * x1_T1 + rnorm(nFam)
	y_T2 = 0.8 * x1_T2 + rnorm(nFam)
	data = data.frame(FAMID = 1:nFam, zygosity = zygosity, x1_T1 = x1_T1, x1_T2 = x1_T2,
		y_T1 = y_T1, y_T2 = y_T2)
	pdf(NULL)
	on.exit(dev.off(), add = TRUE)
	tab = withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "table", sep = "_T", report = "none"),
		message = function(m) invokeRestart("muffleMessage"))
	expect_true("df" %in% names(tab))
	expect_true(all(tab$df > 0 & tab$df < tab$N))
	fits = withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "model", sep = "_T"),
		message = function(m) invokeRestart("muffleMessage"))
	expect_equal(names(fits), c("Pop", "DZ", "MZ"))
	for (lvl in c("Pop", "DZ", "MZ")) {
		expect_s3_class(fits[[lvl]], "lme")
		expect_equal(unname(fits[[lvl]]$coefficients$fixed[["x1"]]), tab$Bwithin[tab$xLevel == lvl], tolerance = 1e-6)
	}
	plt = withCallingHandlers(
		umxDiscTwin(x = "x1", y = "y", data = data, out = "plot", sep = "_T"),
		message = function(m) invokeRestart("muffleMessage"))
	expect_equal(plt$labels$y, "Effect (beta)")
})
