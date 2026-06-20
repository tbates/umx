library(testthat)
library(OpenMx)

test_that("xmuCalculateSRMR computes exact known standardized residuals", {
  
  # ---------------------------------------------------------
  # 1. The Rigged Deterministic Test
  # ---------------------------------------------------------
  # Create an exact 2x2 observed covariance/correlation matrix
  obsMat = matrix(c(1.0, 0.5, 
                    0.5, 1.0), nrow=2, dimnames=list(c("x","y"), c("x","y")))
  
  # Build a model that FORCES the implied matrix to be wrong by exactly 0.1
  mRigged = mxModel("RiggedSRMR", type="RAM",
                    manifestVars = c("x", "y"),
                    mxPath(from="x", to="y", arrows=2, free=FALSE, values=0.4), # Rigged error
                    mxPath(from=c("x", "y"), arrows=2, free=FALSE, values=1),
                    mxData(observed=obsMat, type="cov", numObs=100)
  )
  mRigged = mxRun(mRigged, silent=TRUE)
  
  # The known SRMR is sqrt(0.01 / 3) = 0.05773503
  expected_srmr = sqrt(0.01 / 3)
  calculated_srmr = xmuCalculateSRMR(mRigged)
  
  expect_equal(calculated_srmr, expected_srmr, tolerance = 1e-5, 
               info = "SRMR math failed the deterministic residual test")
  
  # ---------------------------------------------------------
  # 2. The WLS Data Extraction Test
  # ---------------------------------------------------------
  # Ensure the function can successfully hunt down the observed matrix 
  # when OpenMx hides it inside the WLS summary stats object.
  set.seed(123)
  dat = data.frame(x = rnorm(100), y = rnorm(100))
  
  mWLS = mxModel("WLSTest", type="RAM",
                 manifestVars = c("x", "y"),
                 mxPath(from="x", to="y", arrows=1, free=TRUE, values=0.2),
                 mxPath(from=c("x", "y"), arrows=2, free=TRUE, values=1),
                 mxData(observed=dat, type="raw"),
                 mxFitFunctionWLS()
  )
  mWLS = mxRun(mWLS, silent=TRUE)
  
  wls_srmr = xmuCalculateSRMR(mWLS)
  
  # We don't need a specific value here, we just need to know it didn't crash
  # and returned a valid numeric probability space [0, 1].
  expect_true(is.numeric(wls_srmr), info="WLS SRMR did not return a number")
  expect_true(wls_srmr >= 0 && wls_srmr <= 1, info="WLS SRMR is outside [0,1] bounds")
})