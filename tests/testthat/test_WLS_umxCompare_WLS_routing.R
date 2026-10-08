# "~/bin/umx/tests/testthat/test_umxCompare_WLS_routing.R"
library(testthat)
library(umx)

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
  expect_error(umxCompare(mWls1, mMl1), "Engine Mismatch: Cannot compare a WLS model with an ML model")
  expect_error(umxCompare(mWls1, c(mWls2, mMl1)), "Engine Mismatch: Cannot compare a WLS model with an ML model")

  # ML Base vs WLS Comparison(s)
  expect_error(umxCompare(mMl1, mWls1), "Engine Mismatch: Cannot compare an ML model with a WLS model")
  expect_error(umxCompare(mMl1, c(mMl2, mWls1)), "Engine Mismatch: Cannot compare an ML model with a WLS model")
})

test_that("Engine Mismatch Errors (Legacy WLS vs GenomicMx WLS) are thrown", {
  skip_if_not(!is.null(mWls1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

  # GenomicMx Base vs Legacy WLS Comparison(s)
  expect_error(umxCompare(mWls1, mLegacy1), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model with a GenomicMx WLS model")
  expect_error(umxCompare(mWls1, c(mWls2, mLegacy1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model with a GenomicMx WLS model")

  # Legacy WLS Base vs GenomicMx WLS Comparison(s)
  expect_error(umxCompare(mLegacy1, mWls1), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model with a GenomicMx WLS model")
  expect_error(umxCompare(mLegacy1, c(mLegacy2, mWls1)), "Engine Mismatch: Cannot compare a legacy OpenMx WLS model with a GenomicMx WLS model")
})

test_that("Successful multi-model comparison routing works", {
  # All Modern WLS (GenomicMx)
  if (!is.null(mWls1@output$implied_jacobian)) {
    modernTable = suppressWarnings(umxCompare(mWls1, mWls2, silent = TRUE))
    expect_s3_class(modernTable, "data.frame")
  }

  # All Legacy WLS
  options(umx_warned_legacy_wls = FALSE) # Reset to ensure warning fires
  expect_warning(umxCompare(mLegacy1, mLegacy2, silent = TRUE), "Base model missing cached WLS Jacobian")
  legacyTable = suppressWarnings(umxCompare(mLegacy1, mLegacy2, silent = TRUE))
  expect_s3_class(legacyTable, "data.frame")

  # All ML
  mlTable = umxCompare(mMl1, mMl2, silent = TRUE)
  expect_s3_class(mlTable, "data.frame")
})

test_that("a base list longer than one does not ask for listOK", {
  # c(m1, m2) is a list. umx_is_MxModel() on that list warns unless the
  # caller already knows it is the model list.
  mlMessages = capture.output(
    mlTable <- umxCompare(c(mMl1, mMl2), c(mMl2), silent = TRUE),
    type = "message"
  )
  expect_false(any(grepl("listOK", mlMessages)))
  expect_s3_class(mlTable, "data.frame")
  expect_true(nrow(mlTable) > 1)

  wlsMessages = capture.output(
    wlsTable <- umxCompare(c(mWls1, mWls1), c(mWls2), silent = TRUE),
    type = "message"
  )
  expect_false(any(grepl("listOK", wlsMessages)))
  expect_s3_class(wlsTable, "data.frame")
  expect_true(nrow(wlsTable) > 1)
})
