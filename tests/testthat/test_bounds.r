data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)
latents   = c("G")
m1 <- mxModel("m1", type="RAM",
	manifestVars = manifests, latentVars   = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latent fixed@1
	mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
	mxPath(from = "one", to = manifests, arrows = 1), # manifest means
	mxData(myFADataRaw, type = "raw")
)
m1 = mxRun(m1)
summary(m1)$parameters[3,2:6] # 0.803
m1@matrices$A@lbound["x1", "G"] = 0 # lbound G->x1 @ 0 
m1 = mxRun(m1) 
summary(m1)$parameters[3,2:6] # 0.803
m1@matrices$A@lbound["x1","G"] = .9
m1 = mxRun(m1) 
summary(m1)$parameters[3,2:6] # 0.9

