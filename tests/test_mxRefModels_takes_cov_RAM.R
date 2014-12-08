# ===========================================================
# = test that mxRefModels can be given a cov data RAM model =
# ===========================================================
# 2.0.0.4004 gave this error:
# Error in obsdata[selVars, selVars] : subscript out of bounds

require(OpenMx)
data(demoOneFactor)
mxVersion()
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
        manifestVars = manifests, latentVars = latents,
        mxPath(from = latents, to = manifests),
        mxPath(from = manifests, arrows = 2),
        mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
        mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = mxRun(m1)
m1Ref = mxRefModels(m1)

