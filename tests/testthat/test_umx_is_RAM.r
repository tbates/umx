# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_is_RAM.r") 
# 
# test_package("umx")
require(umx)
data(demoOneFactor) # from OpenMx
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
	umxPath(latents, to = manifests),
	umxPath(var = manifests),
	umxPath(var = latents, fixedAt = 1.0)
)

m2 <- mxModel("One Factor",
	mxMatrix("Full", 5, 1, values = 0.2, free = T, name = "A"), 
	mxMatrix("Symm", 1, 1, values = 1, free = F, name = "L"), 
	mxMatrix("Diag", 5, 5, values = 1, free = T, name = "U"), 
	mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
	mxExpectationNormal("R", dimnames = names(demoOneFactor)),
	mxFitFunctionML(),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m2 = mxRun(m2)

test_that("testing umx_is_RAM", {
	expect_equal(umx_is_RAM(m1), TRUE)
	expect_equal(umx_is_RAM(m2), FALSE)
})
