data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)
latents   = c("G")
m1 <- mxModel("m1", type="RAM",
	manifestVars = manifests, latentVars   = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
	mxPath(from = "one", to = manifests, arrows = 1), # manifest means
	mxData(myFADataRaw, type = "raw")
)
umx_set_optimizer("SLSQP")
# umx_set_optimizer("NPSOL")
# umx_set_optimizer("CSOLNP")
m1 = mxRun(m1)
m2 = mxModel(m1, mxCI("A[2,13]"), name="by_bracket1")
m2 = mxRun(m2, intervals = TRUE)
umx_has_CIs(m2) # worked
testthat::expect_true(umx_has_CIs(m2), "test_CI m2 failed")
# ===============================================================
# = Make a new model, add a label, and request CI on that label =
# ===============================================================
m3 = m1; m3$A$labels[2,13] = "myLabel"
m3 = mxRun(mxModel(m3, mxCI("myLabel"), name = "bylabel"), intervals = TRUE)
testthat::expect_true(umx_has_CIs(m3), "test_CI m3 failed")


# =========================
# = Does it work for cov? =
# =========================

m1 <- mxModel("m1", type="RAM",
	manifestVars = manifests, latentVars   = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
	mxData(cov(myFADataRaw[,manifests]), type = "cov", numObs = 500)
)
m1 = mxRun(m1)
m2 = mxRun(mxModel(m1, mxCI("A[2,13]"), name="by_bracket"), intervals = TRUE)
testthat::expect_true(umx_has_CIs(m2), "test_CI m2 failed")

m3 = m1; m3$A@labels[2,13] = "myLabel"
m3 = mxRun(mxModel(m3, mxCI("myLabel"), name = "bylabel"), intervals = TRUE)
testthat::expect_true(umx_has_CIs(m3), "test_CI m3 failed")
