library(testthat)
library(umx)
library(OpenMx)

data(docData)
pheno = c("varA1", "varA2")

# Trio fixtures: docData pairs plus a random third sibling for each family
set.seed(1)
mzTrio = subset(docData, zygosity %in% c("MZFF", "MZMM"))
dzTrio = subset(docData, zygosity %in% c("DZFF", "DZMM"))
for (v in c("varA1", "varA2", "varB1", "varB2", "age")) {
	mzTrio[[paste0(v, "_T3")]] = rnorm(nrow(mzTrio))
	dzTrio[[paste0(v, "_T3")]] = rnorm(nrow(dzTrio))
}
mzTrio$age_T1 = rnorm(nrow(mzTrio)); mzTrio$age_T2 = rnorm(nrow(mzTrio))
dzTrio$age_T1 = rnorm(nrow(dzTrio)); dzTrio$age_T2 = rnorm(nrow(dzTrio))

test_that("umxMRDoC detects trios and filters twin 2's PRS in MZ families", {
	expect_message(t1 <- umxMRDoC(mzData = mzTrio, dzData = dzTrio, pheno = pheno, prss = "varB1", autoRun = FALSE), "modeling twin \\+ sibling trios")
	# MZ keeps twin 1 and sibling PRS, drops twin 2's (it equals twin 1's)
	expect_equal(t1$MZ$expectation$dims, c("varA1_T1", "varA2_T1", "varB1_T1", "varA1_T2", "varA2_T2", "varA1_T3", "varA2_T3", "varB1_T3"))
	expect_length(t1$DZ$expectation$dims, 9)
	expect_true(is.finite(mxRun(t1, useOptimizer = FALSE, silent = TRUE)$output$fit))
	expect_false("tx2" %in% names(omxGetParameters(t1)))
})

test_that("umxMRDoC recovers g1 and b1 from trio data, T identified by trios alone", {
	expect_message(t0 <- umxMRDoC(mzData = mzTrio, dzData = dzTrio, pheno = pheno, prss = "varB1", sibs = TRUE, autoRun = FALSE), "modeling twin \\+ sibling trios")
	truth = c(g1 = .3, b1 = .25, ax2 = .4, ay2 = .3, cx2 = .2, cy2 = .2, ex2 = .4, ey2 = .4, tx2 = .2, ty2 = .2)
	t0 = omxSetParameters(t0, labels = names(truth), values = truth)
	sim = NULL
	invisible(capture.output(sim <- mxGenerateData(t0)))
	sim$MZ$varB1_T2 = sim$MZ$varB1_T1
	expect_message(fit <- umxMRDoC(mzData = sim$MZ, dzData = sim$DZ, pheno = pheno, prss = "varB1", sibs = TRUE, batteries = NULL, summary = FALSE), "modeling twin \\+ sibling trios")
	expect_equal(fit$output$status$code, 0)
	expect_equal(unname(omxGetParameters(fit)[c("g1", "b1")]), c(.3, .25), tolerance = .3)
})

test_that("umxMRDoC supports MRDoC2 and covariates with trios", {
	expect_message(t2 <- umxMRDoC(mzData = mzTrio, dzData = dzTrio, pheno = pheno, prss = c("varB1", "varB2"), covar = "age", autoRun = FALSE), "modeling twin \\+ sibling trios")
	expect_length(t2$MZ$expectation$dims, 10)
	expect_true(all(c("dCov1", "dCov2", "dCov3") %in% names(t2$MZ@matrices)))
	expect_true(is.finite(mxRun(t2, useOptimizer = FALSE, silent = TRUE)$output$fit))
})

test_that("umxMRDoC trios work via data = (scaling covers member 3)", {
	dd = rbind(mzTrio, dzTrio)
	dd$zygosity = rep(c("MZ", "DZ"), c(nrow(mzTrio), nrow(dzTrio)))
	expect_message(t3 <- umxMRDoC(data = dd, pheno = pheno, prss = "varB1", autoRun = FALSE), "modeling twin \\+ sibling trios")
	expect_length(t3$MZ$expectation$dims, 8)
	expect_true(is.finite(mxRun(t3, useOptimizer = FALSE, silent = TRUE)$output$fit))
})

test_that("umxMRDoC data = matches levels in any case, uses all given levels, drops NA rows", {
	dd = docData
	dd$zygosity = ifelse(grepl("^MZ", dd$zygosity), "mz", "dz")
	ref = umxMRDoC(data = dd, pheno = pheno, prss = "varB1", autoRun = FALSE)
	nMZ = sum(dd$zygosity == "mz")
	expect_equal(nrow(ref$MZ$data$observed), nMZ)

	multi = umxMRDoC(data = docData, pheno = pheno, prss = "varB1", autoRun = FALSE, mzData = c("MZFF", "MZMM"), dzData = c("DZFF", "DZMM"))
	expect_equal(nrow(multi$MZ$data$observed), sum(docData$zygosity %in% c("MZFF", "MZMM")))

	ddNA = dd
	ddNA$zygosity[1:10] = NA
	naMod = umxMRDoC(data = ddNA, pheno = pheno, prss = "varB1", autoRun = FALSE)
	expect_equal(nrow(naMod$MZ$data$observed) + nrow(naMod$DZ$data$observed), nrow(dd) - 10)
})

test_that("umxMRDoC errors informatively without sibling data, and for ordinal trios", {
	dd = docData
	dd$zygosity = ifelse(grepl("^MZ", dd$zygosity), "MZ", "DZ")
	expect_error(umxMRDoC(data = dd, pheno = pheno, prss = "varB1", sibs = TRUE), "needs sibling data")

	mzOrd = mzTrio
	mzOrd$varA1_T1 = ordered(cut(mzOrd$varA1_T1, breaks = 3))
	mzOrd$varA1_T2 = ordered(cut(mzOrd$varA1_T2, breaks = 3))
	mzOrd$varA1_T3 = ordered(cut(mzOrd$varA1_T3, breaks = 3))
	expect_error(suppressMessages(umxMRDoC(mzData = mzOrd, dzData = dzTrio, pheno = pheno, prss = "varB1", autoRun = FALSE)), "not yet supported with trios")
})
