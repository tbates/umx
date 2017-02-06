# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_has_means.r") 
# 
# test_package("umx")

context("umx_has type functions")

require(umx)
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

m2 <- mxModel(m1, 
	mxPath(from = "one", to = manifests),
	mxData(demoOneFactor, type = "raw")
)
m2run = umxRun(m2, setLabels = TRUE, setValues = TRUE)

test_that("umx_has_means works", {
	expect_false(umx_has_means(m1))
	expect_true(umx_has_means(m2))
	expect_true(umx_has_means(m2run))
})
