library(testthat)
library(OpenMx)
library(umx)

context("umxCompare WLS Routing and Homogeneity Constraint tests")

# Generate simple data for all models
set.seed(123)
simData = data.frame(x = rnorm(100))
simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8)

# Fit WLS Models
mWls1 = mxModel("WLS_1", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxData(observed = simData, type = "raw"),
  mxFitFunctionWLS()
)
mWls1 = mxRun(mWls1, silent = TRUE)

mWls2 = mxModel("WLS_2", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = FALSE, values = 0.0, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxData(observed = simData, type = "raw"),
  mxFitFunctionWLS()
)
mWls2 = mxRun(mWls2, silent = TRUE)

# Fit ML Models
mMl1 = mxModel("ML_1", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxPath(from = "one", to = c("x", "y"), free = TRUE),
  mxData(observed = simData, type = "raw")
)
mMl1 = mxRun(mMl1, silent = TRUE)

mMl2 = mxModel("ML_2", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = FALSE, values = 0.0, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxPath(from = "one", to = c("x", "y"), free = TRUE),
  mxData(observed = simData, type = "raw")
)
mMl2 = mxRun(mMl2, silent = TRUE)

# Construct Legacy WLS Models (WLS but no Jacobian)
mLegacy1 = mWls1
mLegacy1@output$implied_jacobian = NULL

mLegacy2 = mWls2
mLegacy2@output$implied_jacobian = NULL

test_that("Engine Mismatch Errors (ML vs WLS) are thrown", {
  # WLS Base vs ML Comparison(s)
  expect_error(umxCompare(mWls1, list(mMl1)), "Engine Mismatch: Cannot compare a WLS model with an ML model\\.")
  expect_error(umxCompare(mWls1, list(mWls2, mMl1)), "Engine Mismatch: Cannot compare a WLS model with an ML model\\.")

  # ML Base vs WLS Comparison(s)
  expect_error(umxCompare(mMl1, list(mWls1)), "Engine Mismatch: Cannot compare an ML model with a WLS model\\.")
  expect_error(umxCompare(mMl1, list(mMl2, mWls1)), "Engine Mismatch: Cannot compare an ML model with a WLS model\\.")
})

test_that("Engine Mismatch Errors (Legacy WLS vs GenomicMx WLS) are thrown", {
  # GenomicMx Base vs Legacy WLS Comparison(s)
  expect_error(umxCompare(mWls1, list(mLegacy1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model \\(no Jacobian\\) with a GenomicMx WLS model\\. Both models must use the same engine\\.")
  expect_error(umxCompare(mWls1, list(mWls2, mLegacy1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model \\(no Jacobian\\) with a GenomicMx WLS model\\. Both models must use the same engine\\.")

  # Legacy WLS Base vs GenomicMx WLS Comparison(s)
  expect_error(umxCompare(mLegacy1, list(mWls1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model \\(no Jacobian\\) with a GenomicMx WLS model\\. Both models must use the same engine\\.")
  expect_error(umxCompare(mLegacy1, list(mLegacy2, mWls1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model \\(no Jacobian\\) with a GenomicMx WLS model\\. Both models must use the same engine\\.")
})

test_that("Successful multi-model comparison routing works", {
  # All Modern WLS (GenomicMx)
  expect_message(umxCompare(mWls1, list(mWls2), silent = TRUE), "GenomicMx WLS models detected")
  modernTable = suppressMessages(umxCompare(mWls1, list(mWls2), silent = TRUE))
  expect_s3_class(modernTable, "data.frame")

  # All Legacy WLS
  expect_warning(umxCompare(mLegacy1, list(mLegacy2), silent = TRUE), "Legacy OpenMx WLS engine detected \\(missing Jacobian\\)")
  legacyTable = suppressWarnings(umxCompare(mLegacy1, list(mLegacy2), silent = TRUE))
  expect_s3_class(legacyTable, "data.frame")

  # All ML
  mlTable = umxCompare(mMl1, list(mMl2), silent = TRUE)
  expect_s3_class(mlTable, "data.frame")
})
