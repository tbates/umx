library(testthat)
library(umx)
library(OpenMx)

# xmuLabel auto-labels selection (arrows=0) paths, so no explicit labels are
# needed: umxPath pairs plus selection paths all share across groups.
test_that("umxRAM maes1998 matches passing goldens", {
	data(maes1998)
	allVars = colnames(maes1998$abd)
	wifeDiagnosis = allVars[startsWith(allVars, "wife")]
	husbandDiagnosis = allVars[startsWith(allVars, "husband")]
	abd = umxRAM("abd", data = mxData(maes1998$abd, type = "cov", numObs = maes1998$nObs[["abd"]]), autoRun = FALSE,
		umxPath(unique.bivariate = wifeDiagnosis, lbound = -0.99, ubound = 0.99),
		umxPath(unique.bivariate = husbandDiagnosis, lbound = -0.99, ubound = 0.99),
		umxPath(wifeDiagnosis, husbandDiagnosis, arrows = 0, connect = "unique.pairs", lbound = -0.99, ubound = 0.99),
		umxPath(var = allVars, fixedAt = 1))
	aft = umxRAM("aft", data = mxData(maes1998$aft, type = "cov", numObs = maes1998$nObs[["aft"]]), autoRun = FALSE,
		umxPath(unique.bivariate = wifeDiagnosis, lbound = -0.99, ubound = 0.99),
		umxPath(unique.bivariate = husbandDiagnosis, lbound = -0.99, ubound = 0.99),
		umxPath(wifeDiagnosis, husbandDiagnosis, arrows = 0, connect = "unique.pairs", lbound = -0.99, ubound = 0.99),
		umxPath(var = allVars, fixedAt = 1))
	fig6 = mxModel("fig6", abd, aft, mxFitFunctionMultigroup(c("abd", "aft")))
	fig6 = mxRun(fig6, silent = TRUE)
	fig6r = mxRefModels(fig6, run = TRUE)
	fig6s = summary(fig6, refModels = fig6r)
	expect_equal(fig6s$ChiDoF, 65)
	expect_equal(fig6s$Chi, 544.0544, 1e-3)
	expect_equal(fig6s$CFI, 0.8756, 1e-4)
})

test_that("xmuLabel labels selection paths", {
	m1 = mxModel("t1", type = "RAM", manifestVars = c("EA", "IQ", "C"),
		umxPath("EA", "EA", arrows = 0),
		umxPath("IQ", "C", arrows = 0))
	m1 = xmuLabel(m1)
	expect_equal(unname(m1$selectionVector$labels[, 1]), c("sel_EA", "sel_C_IQ"))
	m2 = mxModel("t2", type = "RAM", manifestVars = c("EA", "IQ", "C"),
		umxPath("EA", "EA", arrows = 0),
		mxPath("IQ", "C", arrows = 0, labels = "mine"))
	m2 = xmuLabel(m2)
	expect_equal(unname(m2$selectionVector$labels[, 1]), c("sel_EA", "mine"))
	m3 = mxModel("t3", type = "RAM", manifestVars = c("A", "B"),
		mxPath("A", "B", arrows = 0, step = 1),
		mxPath("A", "B", arrows = 0, step = 2))
	m3 = xmuLabel(m3)
	expect_equal(unname(m3$selectionVector$labels[, 1]), c("sel_A_B_s1", "sel_A_B_s2"))
})
