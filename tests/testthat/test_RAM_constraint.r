# ===================================
# = RAM model with constraint works =
# ===================================
library(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
m1 <- mxModel("OneFactorPath", type = "RAM", 
   manifestVars = manifests, 
   latentVars = latents, 
   mxPath(latents, to = manifests, labels = paste0("l", 1:5)), 
	mxPath(manifests, arrows = 2), 
	mxPath(latents, arrows = 2, free = F, values = 1.0), 
	mxData(cov(demoOneFactor), type = "cov", numObs = 500), 
	mxAlgebra(S[6, 6], name = "GV"), 
   mxConstraint(GV - 1 == 0, name = "pointless")
)
m1 <- mxRun(m1)