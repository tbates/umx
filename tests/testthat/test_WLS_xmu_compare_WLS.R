library(testthat)
library(OpenMx)
library(umx)

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
  skip_if_not(!is.null(mWlsBase@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
  res = xmu_compare_WLS(mWlsBase, mWlsNested)
  
  expect_s3_class(res, "data.frame")
  expect_equal(dim(res), c(2, 12))
  
  # Assert Satorra-Bentler columns are populated
  expect_false(is.na(res$diffFit[2]))
  expect_false(is.na(res$delta_df[2]))
  expect_false(is.na(res$p[2]))
  
  expect_equal(res$delta_df[2], 1)
  expect_true(res$diffFit[2] > 0)
  expect_true(res$p[2] >= 0 && res$p[2] <= 1)
  
  # Assert new columns are populated
  expect_false(is.na(res$AIC[2]))
  expect_false(is.na(res$CFI[2]))
  expect_true(is.na(res$delta_CFI[2]))
  expect_false(is.na(res$delta_SRMR[2]))
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
  expect_equal(dim(resNoJac), c(2, 12))
  
  # Assert strict SB values degrade to NA
  expect_true(is.na(resNoJac$diffFit[2]))
  expect_true(is.na(resNoJac$p[2]))
  
  # Naive parameter count and absolute metrics should still calculate
  expect_equal(resNoJac$delta_df[2], 1)
  expect_false(is.na(resNoJac$SRMR[2]))
  expect_false(is.na(resNoJac$AIC[2]))
})

test_that("Standard AIC calculation works and Pseudo_BIC is purged", {
  skip_if_not(!is.null(mWlsBase@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
  res = xmu_compare_WLS(mWlsBase, mWlsNested)
  
  # Verify that Pseudo_BIC column does not exist
  expect_null(res$Pseudo_BIC)
  
  # Verify standard AIC calculation: AIC = Chi + 2 * EP
  expect_equal(res$AIC[1], round(res$Chi[1] + 2 * res$EP[1], 2))
  expect_equal(res$AIC[2], round(res$Chi[2] + 2 * res$EP[2], 2))
})

test_that("Triage attribute warning system works", {
  mWlsBaseSmoothed = mWlsBase
  attr(mWlsBaseSmoothed, "gsem_triage") = list(smoothed = TRUE)
  
  expect_message({
    res = suppressWarnings(xmu_compare_WLS(mWlsBaseSmoothed, mWlsNested))
  }, "estimated using nearPD smoothed covariance matrices")
})

test_that("Genomic Track B routing and natively GSEM scaled difference works", {
  mGenomic = mWlsBase
  class(mGenomic) = "MxModelGSEM"
  mGenomic@output$chi = 10.5
  
  mNestedGenomic = mWlsNested
  class(mNestedGenomic) = "MxModelGSEM"
  mNestedGenomic@output$chi = 14.8
  
  expect_message({
    res = xmu_compare_WLS(mGenomic, mNestedGenomic)
  }, "Genomic GSEM model detected")
  
  expect_equal(res$diffFit[2], 4.3) # 14.8 - 10.5
  expect_equal(res$delta_df[2], 1)
  expect_false(is.na(res$p[2]))
})

test_that("Non-converged/failed nested models do not break the function", {
  # Create a nested model with invalid/NA output or failed state
  mWlsBaseMock = mWlsBase
  mWlsBaseMock@output$implied_jacobian = matrix(1, nrow=3, ncol=3)
  
  mWlsBroken = mWlsNested
  mWlsBroken@output$implied_jacobian = matrix(NA, nrow=3, ncol=2) # invalid jacobian dimensions or NAs
  
  # Should gracefully degrade to NA without throwing an error
  expect_warning({
    res = xmu_compare_WLS(mWlsBaseMock, mWlsBroken)
  }, "Satorra-Bentler calculation failed")
  
  expect_s3_class(res, "data.frame")
  expect_true(is.na(res$diffFit[2]))
  expect_true(is.na(res$p[2]))
  expect_equal(res$delta_df[2], 1)
})
