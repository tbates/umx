# ===================================
# = RAM model with constraint works =
# ===================================
library(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
m1 <- mxModel("test", type = "RAM", 
   manifestVars = manifests, 
   latentVars = latents, 
   mxPath(latents, to = manifests), 
	mxPath(manifests, arrows = 2), 
	mxPath(latents, arrows = 2, free = F, values = 1), 
	mxData(cov(demoOneFactor), type = "cov", numObs = 500), 
	mxAlgebra(S[6, 6], name = "GV"), 
   mxConstraint(GV - 1 == 0, name = "pointless")
)
m1 <- mxRun(m1)
# Running test with 10 parameters
# Error in runHelper(model, frontendStart, intervals, silent, suppressWarnings,  :
#   SLSQP: Failed due to singular matrix E or C in LSQ subproblem or rank-deficient equality constraint subproblem or positive directional derivative in line search
  