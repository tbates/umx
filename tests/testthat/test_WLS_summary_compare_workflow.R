# "~/bin/umx/tests/testthat/test_WLS_summary_compare_workflow.R"
library(OpenMx)
library(umx)
library(testthat)
library(lavaan)

# 1. Generate the Data 
# Create a true relationship where path b1 = 0.5
set.seed(123)
simData = data.frame(x = rnorm(1000))
simData$y = simData$x * 0.5 + rnorm(1000, mean = 0, sd = 0.8)

test_that("RAM ML summary and comparison same for umx, openmx, and lavaan", {
	lavStr = "y ~ x"
	fitLav = sem(lavStr, data = simData, fixed.x = FALSE, meanstructure = TRUE)
	lavCoef = lavaan::coef(fitLav)

	# Fit the Base OpenMx RAM Model
	fitMx = mxModel("openmx_RAMBase", type = "RAM",
		manifestVars = c("x", "y"),
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "x_to_y"),
		mxPath(from = "x", arrows = 2, free = TRUE, values = 1, labels = "var_x"),
		mxPath(from = "y", arrows = 2, free = TRUE, values = 1, labels = "var_y"),
		mxPath(from = "one", to = "x", free = TRUE, values = 0, labels = "mean_x"),
		mxPath(from = "one", to = "y", free = TRUE, values = 0, labels = "mean_y"),
		mxData(observed = simData, type = "raw")
	)
	fitMx = mxRun(fitMx, silent = TRUE)
	mxCoef = coef(fitMx)

	# Fit the umxRAM Model
	fitUmx = umxRAM("umxBase", data = simData,
		umxPath("x", to = "y"),
		umxPath(var = c("x", "y")),
		umxPath(means = c("x", "y"))
	)
	umxCoef = coef(fitUmx)

	# Assert parameter estimates match between OpenMx, umxRAM, and lavaan
	expect_equivalent(mxCoef["x_to_y"], lavCoef["y~x"], tolerance = 1e-4)
	expect_equivalent(mxCoef["var_x"], lavCoef["x~~x"], tolerance = 1e-4)
	expect_equivalent(mxCoef["var_y"], lavCoef["y~~y"], tolerance = 1e-4)
	expect_equivalent(mxCoef["mean_x"], lavCoef["x~1"], tolerance = 1e-4)
	expect_equivalent(mxCoef["mean_y"], lavCoef["y~1"], tolerance = 1e-4)

	expect_equivalent(umxCoef["x_to_y"], lavCoef["y~x"], tolerance = 1e-4)
	expect_equivalent(umxCoef["x_with_x"], lavCoef["x~~x"], tolerance = 1e-4)
	expect_equivalent(umxCoef["y_with_y"], lavCoef["y~~y"], tolerance = 1e-4)
	expect_equivalent(umxCoef["one_to_x"], lavCoef["x~1"], tolerance = 1e-4)
	expect_equivalent(umxCoef["one_to_y"], lavCoef["y~1"], tolerance = 1e-4)
})

test_that("WLS summary and comparison same for lavaan, openmx, and umx", {
	lavStr = "y ~ x"
	fitLav = sem(lavStr, data = simData, estimator = "WLS", fixed.x = FALSE)
	lavCoef = lavaan::coef(fitLav)

	# Fit the Base OpenMx WLS Model
	fitMx = mxModel("openmx_WLSBase", type = "RAM",
		manifestVars = c("x", "y"),
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "x_to_y"),
		mxPath(from = "x", arrows = 2, free = TRUE, values = 1, labels = "var_x"),
		mxPath(from = "y", arrows = 2, free = TRUE, values = 1, labels = "var_y"),
		mxData(observed = simData, type = "raw"),
		mxFitFunctionWLS()
	)
	fitMx = mxRun(fitMx, silent = TRUE)
	mxCoef = coef(fitMx)

	# Fit the umx WLS Model
	fitUmx = umxRAM("umxWLS", data = simData, type = "WLS",
		umxPath("x", to = "y"),
		umxPath(var = c("x", "y"))
	)
	umxCoef = coef(fitUmx)

	# Assert parameter estimates match
	expect_equivalent(mxCoef["x_to_y"], lavCoef["y~x"], tolerance = 1e-4)
	expect_equivalent(mxCoef["var_x"], lavCoef["x~~x"], tolerance = 1e-4)
	expect_equivalent(mxCoef["var_y"], lavCoef["y~~y"], tolerance = 1e-4)

	expect_equivalent(umxCoef["x_to_y"], lavCoef["y~x"], tolerance = 1e-4)
	expect_equivalent(umxCoef["x_with_x"], lavCoef["x~~x"], tolerance = 1e-4)
	expect_equivalent(umxCoef["y_with_y"], lavCoef["y~~y"], tolerance = 1e-4)
})

test_that("Factor model using HSwls dataset works and converges", {
	data("HSwls", package = "umx")

	# Fit a one-factor model of variables x1 to x9 using DWLS
	fitUmx = suppressWarnings(umxRAM("g =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", data = HSwls, type = "DWLS"))

	# Verify that the model converged successfully (status code 0)
	expect_equal(fitUmx$output$status$code, 0)

	# Verify class
	expect_s4_class(fitUmx, "MxModel")
})