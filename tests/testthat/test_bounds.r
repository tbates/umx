data(myFADataRaw, package="OpenMx")
manifests = paste0("x", 1:6)
latents   = c("G")
m1 <- umxRAM("m1", mxData(myFADataRaw, type = "raw"),
	umxPath(from  = latents, to = manifests),
	umxPath(var   = manifests), # manifest residuals 
	umxPath(means = manifests), # manifest means
	umxPath(var   = latents, fixedAt = 1), # latent fixed@1
	umxPath(means = latents, fixedAt = 0) # manifest means
)
m1 = mxRun(m1)
testthat::expect_equal(omxGetParameters(m1, fetch="values")[["G_to_x1"]], .803,  tolerance = .001, scale = 1)
m1 = mxRun(omxSetParameters(m1, "G_to_x1", lbound=0))
testthat::expect_equal(omxGetParameters(m1, fetch="values")[["G_to_x1"]], .803,  tolerance = .001, scale = 1)

m1 = mxRun(omxSetParameters(m1, "G_to_x1", lbound=.9))
testthat::expect_equal(omxGetParameters(m1, fetch="values")[["G_to_x1"]], .9,  tolerance = .001, scale = 1)
