# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_tidy_glance_autostart.R")

require(umx)
data(demoOneFactor)
manifests = names(demoOneFactor)

m1 = umxRAM("One Factor", data = demoOneFactor,
	umxPath("G", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "G")
)

# Definition-variable model: x1's mean depends on a covariate
set.seed(1)
dd = demoOneFactor
dd$age = rnorm(nrow(dd))
dd$x1  = dd$x1 + 0.3 * dd$age
dd$fam = rep(seq_len(nrow(dd)/2), each = 2)
m_def = umxRAM("defvar", data = dd, autoRun = FALSE,
	umxPath("G", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "G"),
	umxPath(defn = "def", labels = "data.age"),
	umxPath("def", to = "x1", labels = "b_age")
)

test_that("umx_tidy returns raw and standardized estimates", {
	td = umx_tidy(m1)
	expect_s3_class(td, "data.frame")
	expect_equal(names(td), c("term", "est", "se", "matrix", "row", "col"))
	expect_equal(nrow(td), length(omxGetParameters(m1)))
	expect_equal(td$est, unname(omxGetParameters(m1)[td$term]))

	ts = umx_tidy(m1, std = TRUE)
	expect_equal(names(ts), c("term", "std.est", "std.se", "ci"))
	expect_true(all(abs(ts$std.est) <= 1 + 1e-8))
	expect_true(all(grepl("^-?[0-9.]+ \\[", ts$ci)))
	expect_error(umx_tidy(1:3), "MxModel")
})

test_that("umx_glance returns one row of fit statistics", {
	g = umx_glance(m1)
	expect_equal(nrow(g), 1)
	expect_equal(names(g), c("aic", "bic", "cfi", "tli", "chisq", "chisq.df", "chisq.p",
		"minus2LL", "df", "nobs", "eff_nobs", "rmsea", "rmsea.lower", "rmsea.upper"))
	expect_equal(g$minus2LL, m1$output$Minus2LogLikelihood)
	expect_equal(g$eff_nobs, nrow(demoOneFactor))
	expect_true(g$cfi > .9 && g$cfi <= 1)
})

test_that("umx_autostart handles definition variables, and umx_glance keeps them in refModels", {
	m_start = umx_autostart(m_def)
	expect_false(identical(omxGetParameters(m_start), omxGetParameters(m_def)))
	m_fit = mxRun(m_start, silent = TRUE)
	expect_equal(m_fit$output$status$code, 0)
	expect_equal(unname(omxGetParameters(m_fit)["b_age"]), 0.3, tolerance = .1)

	g = umx_glance(m_fit)
	expect_true(g$chisq >= 0)
	expect_true(g$cfi <= 1)

	# A missing definition variable drops the row from the likelihood
	dd2 = dd
	dd2$age[1:10] = NA
	m_na = mxModel(m_fit, mxData(dd2, type = "raw"))
	expect_equal(xmu_eff_nobs(m_na), nrow(dd2) - 10)

	# Without definition variables umx_autostart is just mxAutoStart
	expect_equal(omxGetParameters(umx_autostart(m1)), omxGetParameters(mxAutoStart(m1)))
})

test_that("umx_tidy cluster-robust SEs", {
	# umxRAM drops unused data columns, so re-attach the data with the cluster ID
	m_fit = mxRun(umx_autostart(mxModel(m_def, mxData(dd, type = "raw"))), silent = TRUE)
	naive = umx_tidy(m_fit)
	robust = umx_tidy(m_fit, cluster = "fam")
	expect_equal(robust$est, naive$est)
	expect_false(isTRUE(all.equal(robust$se, naive$se)))
	expect_true(all(is.finite(robust$se)))
	expect_error(umx_tidy(m_fit, cluster = "nope"), "not found")
})

test_that("umx_tidy and umx_glance handle multigroup models", {
	mA = umxRAM("g1", data = demoOneFactor[1:250, ], autoRun = FALSE,
		umxPath("G", to = manifests), umxPath(v.m. = manifests), umxPath(v1m0 = "G"))
	mB = umxRAM("g2", data = demoOneFactor[251:500, ], autoRun = FALSE,
		umxPath("G", to = manifests), umxPath(v.m. = manifests), umxPath(v1m0 = "G"))
	mg = umxSuperModel("mg", mA, mB)
	expect_equal(nrow(umx_glance(mg)), 1)
	expect_equal(umx_glance(mg)$eff_nobs, 500)
	ts = umx_tidy(mg, std = TRUE)
	expect_gt(nrow(ts), 0)
})
