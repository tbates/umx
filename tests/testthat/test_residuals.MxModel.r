# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_residuals.MxModel.r") 
# test_package("umx")
# TODO get tests for residuals!
# 2. [] need to get the text output test working
# 1. [] need to test supress,
# 3. [] digits
# 3. [] Latents in RAM
# 3. [] Latents non-RAM !

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
residuals(m1)

test_that("residuals.MxModel works", {
	# testthat::expect_output(residuals(m1),"     x1    x2   x3    x4 x5\nx1    .     . 0.01     .  .\nx2    .     . 0.01 -0.01  .\nx3 0.01  0.01    .     .  .\nx4    . -0.01    .     .  .\nx5    .     .    .     .  .\n[1] \"nb: You can zoom in on bad values with suppress, e.g. suppress = .01\""
	)
})
