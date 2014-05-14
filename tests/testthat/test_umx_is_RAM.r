# library(testthat)
# library(umx)
# test_file("~/bin/umx/inst/tests/test_umx_is_RAM.r") 
# 
# test_package("umx")
# TODO test under 1.4 and 2.b
require(OpenMx)
data(demoOneFactor)

latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM", 
	manifestVars = manifests, latentVars = latents, 
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)

m2 <- mxModel("One Factor",
	mxMatrix("Full", 5, 1, values = 0.2, free = T, name = "A"), 
	mxMatrix("Symm", 1, 1, values = 1, free = F, name = "L"), 
	mxMatrix("Diag", 5, 5, values = 1, free = T, name = "U"), 
	mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
	mxMLObjective("R", dimnames = names(demoOneFactor)), 
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m2 = mxRun(m2)

test_that("testing umx_is_RAM", {
	expect_equal(umx_is_RAM(m1), TRUE)
	expect_equal(umx_is_RAM(m2), FALSE)
}
)


