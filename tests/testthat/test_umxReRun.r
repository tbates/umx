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
m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
	umxPath(from = latents, to = manifests),
	umxPath(var = manifests),
	umxPath(var = latents, fixedAt = 1)
)
m1 = mxRun(m1)
m2 = umxReRun(m1, update = "g_to_x1", name = "drop_X1")

test_that("umxRun works", {
	mxCompare(m1, m2)$p[2]
	m2 = umxReRun(m1, update = "g_to_x1", name = "drop_X1", comparison = T)
	expect_is(m1, "MxModel")
})
