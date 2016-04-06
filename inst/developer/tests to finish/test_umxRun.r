# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxRun.r") 
# 
# test_package("umx")
# TODO get tests for umxRun!
# 2. [] means
# 1. [] no means
# 3. [] run more than once
# 3. [] timing for hessian?

require(OpenMx)
data(demoOneFactor)
latents  = c("g")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = umxRun(m1, setLabels = T, setValues = T)

test_that("umxRun works", {
	expect_is(m1, "MxModel")
})
