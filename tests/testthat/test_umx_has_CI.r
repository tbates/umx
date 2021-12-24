# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_has_CI.r") 
# test_package("umx")

context("umx umx_has_CIs Functions")

test_that("umx_has_CIs works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
   	
	m1 = umxRAM("check_model_ex", data = demoOneFactor, type = "cov",
		umxPath("G", to = manifests),
		umxPath(var     = manifests),
		umxPath(var     = "G", fixedAt = 1)
	)
	umx_has_CIs(m1) # FALSE: no CIs and no output
	m1 = mxModel(m1, mxCI("G_to_x1"))
	expect_true(umx_has_CIs(m1, check = "intervals")) # intervals set
	expect_false(umx_has_CIs(m1, check = "output"))  # FALSE not yet run
	m1 = mxRun(m1)
	expect_false(umx_has_CIs(m1, check = "output"))  # Still FALSE: Set and Run
	m1 = mxRun(m1, intervals = TRUE)
	expect_true(umx_has_CIs(m1, check = "output"))  # TRUE: Set, and Run with intervals = T

	data(myFADataRaw, package="OpenMx")
	manifests = names(myFADataRaw)
	latents   = c("G")
	m1 = mxModel("m1", type="RAM",
		manifestVars = manifests, latentVars   = latents,
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2), # manifest residuals 
		mxPath(from = latents, arrows = 2, free = FALSE, values = 1), # latents fixed@1
		mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
		mxPath(from = "one", to = manifests, arrows = 1), # manifest means
		mxData(myFADataRaw, type = "raw")
	)
	m1 = mxRun(m1)
	m2 = mxModel(m1, mxCI("A[2,13]"), name="by_bracket1")
	m2 = mxRun(m2, intervals = TRUE)
	expect_true(umx_has_CIs(m2)) # worked

	# ===============================================================
	# = Make a new model, add a label, and request CI on that label =
	# ===============================================================
	m3 = m1; m3$A$labels[2,13] = "myLabel"
	m3 = mxRun(mxModel(m3, mxCI("myLabel"), name = "bylabel"), intervals = TRUE)
	expect_true(umx_has_CIs(m3))


	# =========================
	# = Does it work for cov? =
	# =========================

	m1 = mxModel("m1", type="RAM",
		manifestVars = manifests, latentVars   = latents,
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2), # manifest residuals 
		mxPath(from = latents, arrows = 2, free = FALSE, values = 1), # latents fixed@1
		mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
		mxData(cov(myFADataRaw[,manifests]), type = "cov", numObs = 500)
	)
	m1 = mxRun(m1)
	m2 = mxRun(mxModel(m1, mxCI("A[2,13]"), name="by_bracket"), intervals = TRUE)
	expect_true(umx_has_CIs(m2) )

	m3 = m1; m3$A@labels[2,13] = "myLabel"
	m3 = mxRun(mxModel(m3, mxCI("myLabel"), name = "bylabel"), intervals = TRUE)
	expect_true(umx_has_CIs(m3) )
})
