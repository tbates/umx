# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_LGCM.R")

require(umx)
umx_set_silent(TRUE)
set.seed(42)
nv  = 4
var = paste0("y", 1:nv)
age = paste0("age", 1:nv)

# Families with ages (centered decades) and placeholder scores, for building models to simulate from
mkFam = function(n, nSib) {
	d = data.frame(row.names = seq_len(n))
	for (s in seq_len(nSib)) {
		base = rnorm(n, 0, .3)
		for (w in 1:nv) {
			d[[paste0(age[w], "_T", s)]] = base + (w - 1) * .5 + rnorm(n, 0, .05)
			d[[paste0(var[w], "_T", s)]] = 0
		}
	}
	d
}
setTruth = function(m, T = FALSE) {
	m = omxSetParameters(m, labels = c("A_1_1", "A_2_2", "C_1_1", "C_2_2", "E_1_1", "E_2_2", "es_y", "mu_Int", "mu_Shape"),
		values = c(.5, .2, .2, .05, .3, .1, .4, 1, -.5))
	if (T) m = omxSetParameters(m, labels = c("T_1_1", "T_2_2"), values = c(.3, .1))
	m
}
simulate = function(m) {
	sim = NULL
	invisible(capture.output(sim <- mxGenerateData(m)))
	sim
}

test_that("umx_LGCM recovers simulated twin growth parameters", {
	m0  = umx_LGCM(var = var, age = age, mzData = mkFam(500, 2), dzData = mkFam(500, 2), autoRun = FALSE)
	expect_false(umx_has_been_run(m0))
	sim = simulate(setTruth(m0))
	m1  = umx_LGCM(var = var, age = age, mzData = sim$MZ, dzData = sim$DZ, tryHard = "no")
	expect_equal(m1$output$status$code, 0)
	est = omxGetParameters(m1)
	expect_equal(unname(est[c("A_1_1", "C_1_1", "E_1_1", "es_y", "mu_Int", "mu_Shape")]), c(.5, .2, .3, .4, 1, -.5), tolerance = .25)
})

test_that("umx_LGCM models trios when member 3 is present, and only then estimates T", {
	mz3 = mkFam(50, 3); dz3 = mkFam(50, 3)
	expect_message(m3 <- umx_LGCM(var = var, age = age, mzData = mz3, dzData = dz3, Tvar = TRUE, autoRun = FALSE), "trios")
	expect_equal(length(m3$MZ$expectation$dims), 3 * nv)
	expect_true("T_1_1" %in% names(omxGetParameters(m3)))

	# Twin pairs only: T is confounded with C, so dropped with a message
	expect_message(m2 <- umx_LGCM(var = var, age = age, mzData = mkFam(50, 2), dzData = mkFam(50, 2), Tvar = TRUE, autoRun = FALSE), "NOT be estimated")
	expect_false("T_1_1" %in% names(omxGetParameters(m2)))

	# Pairs plus a non-co-born SIB group: T is estimated
	m4 = umx_LGCM(var = var, age = age, mzData = mkFam(50, 2), dzData = mkFam(50, 2), sibData = mkFam(50, 2), Tvar = TRUE, autoRun = FALSE)
	expect_true(all(c("MZ", "DZ", "SIB") %in% names(m4@submodels)))
	expect_true("T_1_1" %in% names(omxGetParameters(m4)))
})

test_that("umx_LGCM splits data by zyg, by dplyr-style grouping, and from a pipe", {
	all3 = rbind(transform(mkFam(40, 3), zygosity = "mz"), transform(mkFam(40, 3), zygosity = "dz"), transform(mkFam(40, 3), zygosity = "Sib"))
	m1 = umx_LGCM(var = var, age = age, data = all3, autoRun = FALSE)
	expect_true(all(c("MZ", "DZ", "SIB") %in% names(m1@submodels)))
	expect_equal(nrow(m1$MZ$data$observed), 40)

	# Mock of a grouped data frame (as dplyr::group_by makes it), without dplyr
	grp = all3
	grp$zygosity = NULL
	grp$zz = all3$zygosity
	attr(grp, "groups") = data.frame(zz = c("dz", "mz", "Sib"), .rows = I(lapply(c("dz", "mz", "Sib"), function(z) which(all3$zygosity == z))))
	class(grp) = c("grouped_df", "tbl_df", "tbl", "data.frame")
	m2 = grp |> umx_LGCM(var = var, age = age, autoRun = FALSE)
	expect_equal(names(m2@submodels), names(m1@submodels))
	expect_equal(nrow(m2$DZ$data$observed), 40)

	# Ungrouped data piped in, with zyg named
	m3 = all3 |> umx_LGCM(var = var, age = age, zyg = "zygosity", autoRun = FALSE)
	expect_equal(names(m3@submodels), names(m1@submodels))
})

test_that("umx_LGCM handles missing ages, links, quadratic, covariates, and Chol", {
	mz = mkFam(60, 2); dz = mkFam(60, 2)
	mz$Z_T1 = rnorm(60); mz$Z_T2 = rnorm(60); dz$Z_T1 = rnorm(60); dz$Z_T2 = rnorm(60)
	mz$age2_T1[1:5] = NA
	expect_message(m1 <- umx_LGCM(var = var, age = age, mzData = mz, dzData = dz, autoRun = FALSE), "5 observed scores")
	expect_false(anyNA(m1$MZ$data$observed[, paste0(age, "_T1")]))

	for (lk in c("exponential", "logistic", "gompertz")) {
		m = umx_LGCM(var = var, age = age, invariant = "Z", mzData = mz, dzData = dz, link = lk, quadratic = TRUE, autoRun = FALSE)
		expect_true(all(c("b_Int_on_Z1", "b_Shape_on_Z1", "b_Quad_on_Z1") %in% names(omxGetParameters(m))))
		expect_true(is.finite(mxRun(m, useOptimizer = FALSE, silent = TRUE)$output$fit))
	}
	mS = umx_LGCM(var = var, age = age, invariant = "Z", mzData = mz, dzData = dz, autoRun = FALSE)
	mC = umx_LGCM(var = var, age = age, invariant = "Z", mzData = mz, dzData = dz, Chol = TRUE, autoRun = FALSE)
	expect_equal(length(omxGetParameters(mC)), length(omxGetParameters(mS)))
	expect_true("a_1_1" %in% names(omxGetParameters(mC)))
})

test_that("xmu_update_covar handles a third family member", {
	d = data.frame(age_T1 = c(1, NA), age_T2 = c(NA, 2), age_T3 = c(NA, 3), y_T1 = 1:2, y_T2 = 3:4, y_T3 = 5:6)
	out2 = xmu_update_covar(d, covar = "age", pheno = "y")
	expect_true(is.na(out2$age_T3[1]))
	out3 = xmu_update_covar(d, covar = "age", pheno = "y", nSib = 3)
	expect_equal(out3$age_T3, c(99999, 3))
	expect_equal(out3$y_T3, c(NA, 6))
	expect_equal(out3[, c("age_T1", "age_T2", "y_T1", "y_T2")], out2[, c("age_T1", "age_T2", "y_T1", "y_T2")])
})
