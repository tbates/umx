library(umx)
library(testthat)

test_that("umx_make_GxE_data has expected structure and truth attributes", {
	df = umx_make_GxE_data(nMZpairs = 50, nDZpairs = 50, seed = 99)
	expect_equal(nrow(df), 100L)
	expect_equal(ncol(df), 9L)
	expect_true(all(c(
		"zygosity", "outcome_T1", "outcome_T2",
		"outcomeAge_T1", "outcomeAge_T2",
		"mod_T1", "mod_T2", "age_T1", "age_T2"
	) %in% names(df)))
	expect_equal(as.integer(table(df$zygosity)[c("MZ", "DZ")]), c(50L, 50L))
	# Shared family moderator and age
	expect_equal(df$mod_T1, df$mod_T2)
	expect_equal(df$age_T1, df$age_T2)

	truth = attr(df, "truth")
	expect_true(is.list(truth))
	expect_equal(truth$am, 0.15)
	expect_equal(truth$a, 0.5)
	expect_equal(truth$c, 0.3)
	expect_equal(truth$e, 0.6)
	expect_equal(truth$cm, 0)
	expect_equal(truth$em, 0)
	expect_equal(truth$seed, 99L)
	expect_equal(truth$nMZpairs, 50L)
	expect_equal(truth$nDZpairs, 50L)
	expect_true(abs(truth$cor_outcomeAge_age - 0.2) < 0.08)
	# Defaults at larger N still land near target age correlation
	dfLarge = umx_make_GxE_data(nMZpairs = 500, nDZpairs = 500, seed = 1)
	expect_true(abs(attr(dfLarge, "truth")$cor_outcomeAge_age - 0.2) < 0.03)
})

test_that("umxGxE recovers am on synthetic GxE data (not on CRAN)", {
	skip_on_cran()
	# Moderated ACE is low power; use enough pairs for a finite estimate near truth
	df = umx_make_GxE_data(nMZpairs = 400, nDZpairs = 400, seed = 1)
	truth = attr(df, "truth")

	m1 = umxGxE(
		selDVs = "outcome",
		selDefs = "mod",
		sep = "_T",
		data = df,
		mzData = "MZ",
		dzData = "DZ",
		tryHard = "yes",
		autoRun = TRUE
	)
	amHat = as.numeric(m1$top$am$values[1, 1])
	expect_true(is.finite(amHat))
	# Loose tolerance: GxE moderation recovery is noisy
	expect_lt(abs(amHat - truth$am), 0.12)
})
