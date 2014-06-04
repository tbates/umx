# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_set_default_optimizer.r") 
# 
# test_package("umx")
library(OpenMx)
manifests = c("mpg", "disp", "gear")
m1 <- mxModel("ind", type = "RAM",
	manifestVars = manifests,
	mxPath(from = manifests, arrows = 2),
	mxPath(from = "one", to = manifests),
	mxData(mtcars[,manifests], type="raw")
)
mxOption(m1, "Default optimizer", "CSOLNP")
# Default optimizer is a global option and cannot be set on models

mxOption(NULL, "Default optimizer", "CSOLNP")
m1 = mxRun(m1)
test_that("mxOption set Default optimizer in a model and universally", {
	expect_match(m1@runstate$compute$steps[1][[1]]$engine, "CSOLNP")
	# Set optimizer for this model
	m1 = mxRun(mxOption(m1, "Default optimizer", "NPSOL"))
	expect_match(m1@runstate$compute$steps[1][[1]]$engine, "NPSOL")

	# test switching globally
	mxOption(NULL, "Default optimizer", "NPSOL")
	m1 = mxRun(m1)
	expect_match(m1@runstate$compute$steps[1][[1]]$engine, "NPSOL")
	m1 = mxRun(mxOption(m1, "Default optimizer", "CSOLNP"))
	expect_match(m1@runstate$compute$steps[1][[1]]$engine, "CSOLNP")
})

m1 = mxRun(m1 <- mxOption(m1, "Default optimizer", "NPSOL"))