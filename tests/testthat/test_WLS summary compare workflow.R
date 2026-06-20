library(OpenMx)
library(umx)
library(testthat)

test_that("WLS summary and comparison works", {
  # 1. Generate the Data 
  set.seed(123)
  df = data.frame(x = rnorm(1000))
  # Create a true relationship where path b1 = 0.5
  df$y = df$x * 0.5 + rnorm(1000, mean = 0, sd = 0.8) 

  # 2. Fit the Base WLS Model
  mBase = mxModel("WLS_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxData(observed = df, type = "raw"),
    mxFitFunctionWLS()
  )
  mBase = mxRun(mBase, silent = TRUE)

  # 3. Fit the Nested WLS Model (Drop the b1 path to 0)
  mNested = mxModel(mBase, name = "WLS_Nested")
  mNested$A$values["y", "x"] = 0
  mNested$A$free["y", "x"] = FALSE
  mNested = mxRun(mNested, silent = TRUE)

  # 4. Fit an ML Model (Non-WLS)
  mMl = mxModel("ML_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxPath(from = "one", to = c("x", "y"), free = TRUE),
    mxData(observed = df, type = "raw")
  )
  mMl = mxRun(mMl, silent = TRUE)

  # 5. Verify umxCompare routes to xmu_compare_WLS
  res_direct = xmu_compare_WLS(mBase, mNested)
  res_routed = umxCompare(mBase, mNested)
  expect_equal(res_routed, res_direct)

  # 6. Verify error on mixing WLS and ML
  expect_error(umxCompare(mBase, mMl), "Cannot compare a WLS model with an ML model.")
})