# test that mxRefModels can be given a cov data RAM model
require(OpenMx)
data(demoOneFactor)
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
# Error in obsdata[selVars, selVars] : subscript out of bounds
mxVersion() # OpenMx version: 2.0.0.4004
