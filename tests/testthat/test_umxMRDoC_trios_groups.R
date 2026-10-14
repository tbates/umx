# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxMRDoC_trios_groups.R")

require(umx)
umx_set_silent(TRUE)
q = function(x) suppressMessages(suppressWarnings(x))
data(docData)
dd = docData
dd$zygosity = ifelse(grepl("^MZ", dd$zygosity), "MZ", "DZ")
pheno = c("varA1", "varA2")

test_that("umxMRDoC uses grouped data, or a .group column, as zygosity", {
	ref = q(umxMRDoC(data = dd, pheno = pheno, prss = "varB1"))
	g = dd
	attr(g, "groups") = data.frame(zygosity = c("DZ", "MZ"), .rows = I(list(which(dd$zygosity == "DZ"), which(dd$zygosity == "MZ"))))
	class(g) = c("grouped_df", "tbl_df", "tbl", "data.frame")
	m1 = q(g |> umx_mrdoc(pheno = pheno, prss = "varB1"))
	expect_equal(m1$output$fit, ref$output$fit)

	g2 = dd
	names(g2)[names(g2) == "zygosity"] = ".group"
	g2$.group = tolower(g2$.group)
	m2 = q(umxMRDoC(data = g2, pheno = pheno, prss = "varB1"))
	expect_equal(m2$output$fit, ref$output$fit)

	# A sibling level in grouped data turns on sibs
	g3 = dd
	g3$zygosity[1:150] = "SIB"
	g3$zz = g3$zygosity
	attr(g3, "groups") = data.frame(zz = c("DZ", "MZ", "SIB"), .rows = I(lapply(c("DZ", "MZ", "SIB"), function(z) which(g3$zz == z))))
	class(g3) = c("grouped_df", "tbl_df", "tbl", "data.frame")
	expect_message(m3 <- umxMRDoC(data = g3, pheno = pheno, prss = "varB1", autoRun = FALSE), "setting sibs = TRUE")
	expect_true("Sibs" %in% names(m3@submodels))
	expect_true("tx2" %in% names(omxGetParameters(m3)))
})

test_that("umxMRDoC models twin + sibling trios", {
	set.seed(1)
	mkTrio = function(d, vars) { for (v in vars) d[[paste0(v, "_T3")]] = rnorm(nrow(d)); d }
	mz = mkTrio(subset(docData, zygosity %in% c("MZFF", "MZMM")), c("varA1", "varA2", "varB1", "varB2", "age"))
	dz = mkTrio(subset(docData, zygosity %in% c("DZFF", "DZMM")), c("varA1", "varA2", "varB1", "varB2", "age"))
	mz$age_T1 = mz$age_T2 = rnorm(nrow(mz)); dz$age_T1 = rnorm(nrow(dz)); dz$age_T2 = rnorm(nrow(dz))

	# MRDoC: MZ keeps twin 1 and sibling PRS, drops twin 2's
	t1 = q(umxMRDoC(mzData = mz, dzData = dz, pheno = pheno, prss = "varB1", autoRun = FALSE))
	expect_equal(t1$MZ$expectation$dims, c("varA1_T1", "varA2_T1", "varB1_T1", "varA1_T2", "varA2_T2", "varA1_T3", "varA2_T3", "varB1_T3"))
	expect_length(t1$DZ$expectation$dims, 9)
	expect_true(is.finite(mxRun(t1, useOptimizer = FALSE, silent = TRUE)$output$fit))
	expect_false("tx2" %in% names(omxGetParameters(t1)))

	# Recovery of g1 and b1 from data simulated from the trio model, T identified by trios alone
	truth = c(g1 = .3, b1 = .25, ax2 = .4, ay2 = .3, cx2 = .2, cy2 = .2, ex2 = .4, ey2 = .4, tx2 = .2, ty2 = .2)
	t0 = q(umxMRDoC(mzData = mz, dzData = dz, pheno = pheno, prss = "varB1", sibs = TRUE, autoRun = FALSE))
	t0 = omxSetParameters(t0, labels = names(truth), values = truth)
	sim = NULL
	invisible(capture.output(sim <- mxGenerateData(t0)))
	sim$MZ$varB1_T2 = sim$MZ$varB1_T1
	fit = q(umxMRDoC(mzData = sim$MZ, dzData = sim$DZ, pheno = pheno, prss = "varB1", sibs = TRUE, batteries = NULL))
	expect_equal(fit$output$status$code, 0)
	expect_equal(unname(omxGetParameters(fit)[c("g1", "b1")]), c(.3, .25), tolerance = .3)

	# MRDoC2 and covariates with trios
	t2 = q(umxMRDoC(mzData = mz, dzData = dz, pheno = pheno, prss = c("varB1", "varB2"), covar = "age", autoRun = FALSE))
	expect_length(t2$MZ$expectation$dims, 10)
	expect_true(all(c("dCov1", "dCov2", "dCov3") %in% names(t2$MZ@matrices)))
	expect_true(is.finite(mxRun(t2, useOptimizer = FALSE, silent = TRUE)$output$fit))

	# Pair data with sibs = TRUE but no sibling data is an error
	expect_error(q(umxMRDoC(data = dd, pheno = pheno, prss = "varB1", sibs = TRUE)), "needs sibling data")
})
