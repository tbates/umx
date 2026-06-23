library(testthat)
library(OpenMx)
library(umx)

context("xmu_compare_WLS routing and penalty tests")

# Generate simple WLS models for comparison tests
set.seed(123)
simData = data.frame(x = rnorm(100))
simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8)

mWlsBase = mxModel("WLS_Base", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxData(observed = simData, type = "raw"),
  mxFitFunctionWLS()
)
mWlsBase = mxRun(mWlsBase, silent = TRUE)

mWlsNested = mxModel("WLS_Nested", type="RAM",
  manifestVars = c("x", "y"),
  mxPath(from = "x", to = "y", arrows = 1, free = FALSE, values = 0.0, labels = "b1"),
  mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
  mxData(observed = simData, type = "raw"),
  mxFitFunctionWLS()
)
mWlsNested = mxRun(mWlsNested, silent = TRUE)

test_that("Universal Routing works (Modern models with Jacobians)", {
  res = xmu_compare_WLS(mWlsBase, mWlsNested)
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(2, 9))
  
  # Assert Satorra-Bentler columns are populated
  expect_false(is.na(res$Strict_SB[2]))
  expect_false(is.na(res$delta_ep[2]))
  expect_false(is.na(res$p[2]))
  
  expect_equal(res$delta_ep[2], 1)
  expect_true(res$Strict_SB[2] > 0)
  expect_true(res$p[2] >= 0 && res$p[2] <= 1)
})

test_that("Graceful Degradation works (Legacy WLS models missing Jacobians)", {
  mWlsBaseNoJac = mWlsBase
  mWlsBaseNoJac@output$implied_jacobian = NULL
  
  mWlsNestedNoJac = mWlsNested
  mWlsNestedNoJac@output$implied_jacobian = NULL
  
  expect_message({
    suppressWarnings({
      resNoJac = xmu_compare_WLS(mWlsBaseNoJac, mWlsNestedNoJac)
    })
  }, "lack a cached Jacobian")
  
  expect_s3_class(resNoJac, "data.frame")
  
  # Assert strict SB values degrade to NA
  expect_true(is.na(resNoJac$Strict_SB[2]))
  expect_true(is.na(resNoJac$p[2]))
  
  # Naive parameter count and absolute metrics should still calculate
  expect_equal(resNoJac$delta_ep[2], 1)
  expect_false(is.na(resNoJac$SRMR[2]))
  expect_false(is.na(resNoJac$Pseudo_BIC[2]))
})

test_that("bicPenaltyN routing works", {
  # 1. Genomic N=1000 fallback
  mGenomic = mWlsBase
  class(mGenomic) = "MxModelGSEM"
  
  expect_message({
    resGenomic = xmu_compare_WLS(mGenomic, mWlsNested)
  }, "Massive-N / GSEM model detected")
  
  # Verify that N=1000 was used for BIC
  expectedBicGenomic = round(xmu_pseudo_BIC(chisq = resGenomic$chisq_WLS, k = resGenomic$ep, n = 1000), 2)
  expect_equal(resGenomic$Pseudo_BIC, expectedBicGenomic)
  
  # 2. Standard numObs fallback (actual N = 100)
  resStd = suppressMessages(xmu_compare_WLS(mWlsBase, mWlsNested))
  expectedBicStd = round(xmu_pseudo_BIC(chisq = resStd$chisq_WLS, k = resStd$ep, n = 100), 2)
  expect_equal(resStd$Pseudo_BIC, expectedBicStd)
  
  # 3. User manual override
  capturedMessages = capture_messages({
    resOverride = xmu_compare_WLS(mWlsBase, mWlsNested, bicPenaltyN = 500)
  })
  expect_true(any(grepl("custom penalty benchmark", capturedMessages)))
  expect_true(any(grepl("diverges from the empirical model sample size", capturedMessages)))
  
  expectedBicOverride = round(xmu_pseudo_BIC(chisq = resOverride$chisq_WLS, k = resOverride$ep, n = 500), 2)
  expect_equal(resOverride$Pseudo_BIC, expectedBicOverride)
})

test_that("Triage attribute warning system works", {
  mWlsBaseSmoothed = mWlsBase
  attr(mWlsBaseSmoothed, "gsem_triage") = list(smoothed = TRUE)
  
  expect_message({
    res = xmu_compare_WLS(mWlsBaseSmoothed, mWlsNested)
  }, "estimated using nearPD smoothed covariance matrices")
})
