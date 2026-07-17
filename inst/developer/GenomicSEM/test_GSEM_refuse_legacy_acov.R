# Hard refusal of removed OpenMx WLS data API (type='acov' / top-level formals / legacy keys).
# Construction fails in OpenMx; umx also refuses any in-hand legacy-shaped objects forever.
library(testthat)
library(OpenMx)
library(umx)

test_that("OpenMx hard-errors legacy mxData construction", {
	S = diag(2)
	dimnames(S) = list(c("a", "b"), c("a", "b"))
	W = diag(3)
	expect_error(
		mxData(S, type = "acov", acov = W, fullWeight = diag(3), numObs = 10),
		"Top-level acov and fullWeight|has been removed|removed"
	)
	expect_error(
		mxData(numObs = 10, type = "none", observedStats = list(cov = S)),
		"has been removed|removed"
	)
	expect_error(
		mxData(numObs = 10, observedStats = list(cov = S, acov = W)),
		"have been removed|removed"
	)
	expect_error(
		mxData(numObs = 10, observedStats = list(cov = S, fullWeight = W)),
		"have been removed|removed"
	)
})

test_that("umx legacy detectors refuse acov/none and legacy observedStats keys", {
	expect_true(xmu_is_legacy_acov_data(list(type = "acov")))
	expect_true(xmu_is_legacy_acov_data(list(type = "none")))
	expect_true(xmu_is_legacy_acov_data(list(type = "summary", observedStats = list(acov = diag(1)))))
	expect_true(xmu_is_legacy_acov_data(list(type = "summary", observedStats = list(fullWeight = diag(1)))))
	expect_false(xmu_is_legacy_acov_data(list(
		type = "summary",
		observedStats = list(cov = diag(2), useWeight = diag(3), asymCov = diag(3))
	)))
	expect_error(xmu_stop_legacy_acov("test"), "does not support the removed OpenMx WLS data interface")
})

test_that("xmu_make_mxData refuses legacy-shaped data", {
	fake = list(type = "acov", observed = diag(2))
	expect_error(
		xmu_make_mxData(fake, type = "Auto", manifests = c("a", "b")),
		"does not support the removed OpenMx WLS data interface|type='acov'"
	)
})

test_that("modern observedStats still works for extract and GSEM", {
	data(Psych_LDSC, package = "umx")
	m = umxGSEM("g ~= SCZ + BIP + MDD", covstruc = Psych_LDSC, estimation = "DWLS",
	            tryHard = "no", autoRun = FALSE)
	expect_equal(m$data$type, "summary")
	wv = xmu_wls_extract_WV(m)
	expect_true(is.matrix(wv$useWeight))
	expect_true(is.matrix(wv$asymCov))
	expect_true(diag(wv$useWeight)[1] > 1)
	expect_true(diag(wv$asymCov)[1] < 1)
})
