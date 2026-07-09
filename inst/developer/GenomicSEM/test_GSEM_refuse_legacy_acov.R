# Hard refusal of OpenMx type='acov' / MxDataLegacyWLS in umx
library(testthat)
library(OpenMx)
library(umx)

test_that("xmu_make_mxData refuses type='acov'", {
	S = diag(2)
	dimnames(S) = list(c("a", "b"), c("a", "b"))
	suppressWarnings({
		leg = mxData(S, type = "acov", acov = diag(3), fullWeight = diag(3), numObs = 10)
	})
	expect_error(
		xmu_make_mxData(leg, type = "Auto", manifests = c("a", "b")),
		"does not support OpenMx type='acov'"
	)
})

test_that("umxRAM refuses type='acov' data", {
	S = diag(2)
	dimnames(S) = list(c("a", "b"), c("a", "b"))
	suppressWarnings({
		leg = mxData(S, type = "acov", acov = diag(3), fullWeight = diag(3), numObs = 10)
	})
	expect_error(
		umxRAM("bad", data = leg, autoRun = FALSE,
			umxPath(var = c("a", "b")), umxPath("a", with = "b")),
		"does not support OpenMx type='acov'|type='acov'"
	)
})

test_that("xmu_wls_extract_WV refuses legacy acov model data", {
	S = diag(2)
	dimnames(S) = list(c("a", "b"), c("a", "b"))
	suppressWarnings({
		leg = mxData(S, type = "acov", acov = diag(3), fullWeight = diag(3), numObs = 10)
	})
	# Minimal shell model with legacy data
	m = mxModel("shell", type = "RAM",
		manifestVars = c("a", "b"),
		mxPath("a", to = "b", arrows = 2),
		mxPath(c("a", "b"), arrows = 2),
		leg
	)
	expect_error(xmu_wls_extract_WV(m), "type='acov'|not supported|does not support")
})

test_that("modern observedStats still works for extract and GSEM", {
	data(Psych_LDSC, package = "umx")
	m = umxGSEM("g ~= SCZ + BIP + MDD", covstruc = Psych_LDSC, estimation = "DWLS",
	            tryHard = "no", autoRun = FALSE)
	wv = xmu_wls_extract_WV(m)
	expect_true(is.matrix(wv$useWeight))
	expect_true(is.matrix(wv$asymCov))
	expect_true(diag(wv$useWeight)[1] > 1)
	expect_true(diag(wv$asymCov)[1] < 1)
})
