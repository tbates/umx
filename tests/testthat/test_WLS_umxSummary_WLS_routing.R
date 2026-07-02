library(testthat)
library(OpenMx)
library(umx)

# Helper function to run umxSummary and collect all messages and warnings
# Using <- for function definition per AGENTS.md style constraints
runSummaryCollectOutputs <- function(model, refModels = NULL) {
  msgs = character()
  warns = character()

  withCallingHandlers({
    umxSummary(model, refModels = refModels)
  }, message = function(m) {
    # Using = for internal assignments per AGENTS.md style constraints
    msgs <<- c(msgs, m$message)
    invokeRestart("muffleMessage")
  }, warning = function(w) {
    warns <<- c(warns, w$message)
    invokeRestart("muffleWarning")
  })

  list(messages = msgs, warnings = warns)
}

test_that("umxSummary Case 1: Modern WLS routing works", {
  # 1. Generate the Data 
  set.seed(123)
  simData = data.frame(x = rnorm(100))
  simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8) 

  # 2. Fit the Base WLS Model
  mBase = mxModel("WLS_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxData(observed = simData, type = "raw"),
    mxFitFunctionWLS()
  )
  mBase = mxRun(mBase, silent = TRUE)

  # Check if Jacobian is present
  hasJacobian = !is.null(mBase$output$implied_jacobian)
  if (hasJacobian) {
    res = runSummaryCollectOutputs(mBase)
    # Verify messages
    expect_true(any(grepl("Modern WLS model with Jacobian detected\\. Applying SB-2010 robust metrics", res$messages)))
    expect_true(any(grepl("For WLS/DWLS models, conventional fit index cutoffs", res$messages)))
    expect_false(any(grepl("worse than desired", res$messages)))
    expect_equal(length(res$warnings), 0)
  } else {
    options(umx_warned_legacy_wls = FALSE) # Reset to ensure warning fires
    res = runSummaryCollectOutputs(mBase)
    expect_true(any(grepl("Legacy OpenMx WLS engine detected", res$warnings)))
  }
})

test_that("umxSummary Case 2: Legacy WLS routing works", {
  set.seed(123)
  simData = data.frame(x = rnorm(100))
  simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8) 

  mBase = mxModel("WLS_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxData(observed = simData, type = "raw"),
    mxFitFunctionWLS()
  )
  mBase = mxRun(mBase, silent = TRUE)

  # Mock missing Jacobian by setting implied_jacobian to NULL
  mLegacy = mBase
  mLegacy@output$implied_jacobian = NULL

  options(umx_warned_legacy_wls = FALSE) # Reset to ensure warning fires
  # Run summary and check outputs
  res = runSummaryCollectOutputs(mLegacy)
  
  # Verify messages and warning
  expect_false(any(grepl("Modern WLS model with Jacobian detected", res$messages)))
  expect_false(any(grepl("For WLS/DWLS models, conventional fit index cutoffs", res$messages)))
  expect_false(any(grepl("worse than desired", res$messages)))
  expect_true(any(grepl("Legacy OpenMx WLS engine detected \\(missing Jacobian\\)", res$warnings)))
})

test_that("umxSummary Case 3: Standard ML routing works", {
  set.seed(123)
  simData = data.frame(x = rnorm(100))
  simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8) 

  # Fit an ML Model (Non-WLS) with a poor fit
  mMl = mxModel("ML_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = FALSE, values = 0.0, labels = "b1"), # Fixed at 0 to force poor fit
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxPath(from = "one", to = c("x", "y"), free = TRUE),
    mxData(observed = simData, type = "raw")
  )
  mMl = mxRun(mMl, silent = TRUE)

  # Run summary and check outputs
  res = runSummaryCollectOutputs(mMl)
  
  # Verify messages: Should NOT print WLS specific messages
  expect_false(any(grepl("Modern WLS model with Jacobian detected", res$messages)))
  expect_false(any(grepl("For WLS/DWLS models, conventional fit index cutoffs", res$messages)))
  
  # TLI/RMSEA should be evaluated and warnings printed if they exceed desired thresholds
  # (Since TLI/RMSEA will be bad for this restricted model)
  expect_true(any(grepl("TLI is worse than desired", res$messages)) || any(grepl("RMSEA is worse than desired", res$messages)))
  expect_equal(length(res$warnings), 0)
})

test_that("umxSummary Case 4: ML Fallback routing works", {
  set.seed(123)
  simData = data.frame(x = rnorm(100))
  simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8) 

  mMl = mxModel("ML_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxPath(from = "one", to = c("x", "y"), free = TRUE),
    mxData(observed = simData, type = "raw")
  )
  mMl = mxRun(mMl, silent = TRUE)

  # Mock summary.MxModel temporarily to make Chi and ChiDoF NULL, preventing partial matches
  summary.MxModel <- function(object, ...) {
    s = OpenMx:::summary.MxModel(object, ...)
    s$Chi = NULL
    s$ChiDoF = NULL
    s
  }
  registerS3method("summary", "MxModel", summary.MxModel)
  
  # Ensure we restore original method after test finishes
  on.exit({
    registerS3method("summary", "MxModel", OpenMx:::summary.MxModel)
  }, add = TRUE)

  # Run summary with refModels = FALSE to trigger Case 4 (Fallback block)
  res = runSummaryCollectOutputs(mMl, refModels = FALSE)
  
  # Verify messages: Should print basic Model Fit -2LL, df, AIC statistics
  expect_true(any(grepl("Model Fit: -2LL =", res$messages)))
  expect_true(any(grepl("AIC =", res$messages)))
  expect_false(any(grepl("CFI =", res$messages)))
  expect_false(any(grepl("TLI =", res$messages)))
})

test_that("xmu_robust_WLS_fit boundary and error handling works", {
  set.seed(123)
  simData = data.frame(x = rnorm(100))
  simData$y = simData$x * 0.5 + rnorm(100, mean = 0, sd = 0.8) 

  mBase = mxModel("WLS_Base", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxData(observed = simData, type = "raw"),
    mxFitFunctionWLS()
  )
  mBase = mxRun(mBase, silent = TRUE)

  # Check if Jacobian is supported in this environment
  skip_if_not(!is.null(mBase$output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")

  # 1. Missing Jacobian error
  mNoJac = mBase
  mNoJac@output$implied_jacobian = NULL
  expect_error(xmu_robust_WLS_fit(mNoJac), "Target model missing implied_jacobian\\.")

  # 2. Missing Weight matrix error
  mNoWeight = mBase
  mNoWeight$data@observedStats$useWeight = NULL
  expect_error(xmu_robust_WLS_fit(mNoWeight), "Could not locate WLS Weight matrix \\(W\\)\\.")

  # 3. Missing Asymmetric Covariance matrix error
  mNoAcov = mBase
  mNoAcov$data@observedStats$asymCov = NULL
  expect_error(xmu_robust_WLS_fit(mNoAcov), "Could not locate Asymptotic Covariance matrix \\(V\\)\\.")

  # 4. Independent model used for TLI denominator scaling tests and N fallback test
  mInd = mxModel("WLS_Ind", type="RAM",
    manifestVars = c("x", "y"),
    mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
    mxData(observed = simData, type = "raw"),
    mxFitFunctionWLS()
  )
  mInd = mxRun(mInd, silent = TRUE)

  # Fallback for sample size N (NA must be cast to NA_real_ to avoid class union validation error on slot numObs)
  # Uses mInd (where df > 0) to avoid NaN RMSEA
  mNoN = mInd
  mNoN$data$numObs = NA_real_
  resN = xmu_robust_WLS_fit(mNoN)
  expect_false(is.na(resN$RMSEA))

  # 5. TLI division-by-zero fallback (TLI returns NA when denominator is 0)
  resOrig = xmu_robust_WLS_fit(mInd)
  
  # Re-run same independent WLS model but scale asymCov so that scaling factor is exactly equal to chisqIndRaw
  # This makes chisqIndScaled = 1, dfInd = 1, and tliDenom = 0
  asymCov = mInd$data@observedStats$asymCov
  weightMat = mInd$data@observedStats$useWeight
  manifests = mInd@manifestVars
  obsCov = cov(mInd$data$observed, use = "pairwise.complete.obs")

  sVec = rep(0, nrow(asymCov))
  rowNames = rownames(asymCov)
  for (i in 1:length(rowNames)) {
  	name = rowNames[i]
  	parts = strsplit(name, "[ _]")[[1]]
  	parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
  	if (length(parts) == 2) {
  		sVec[i] = obsCov[parts[1], parts[2]]
  	} else if (length(parts) == 1) {
  		sVec[i] = obsCov[parts[1], parts[1]]
  	}
  }
  dInd = sVec
  for (i in 1:length(rowNames)) {
  	name = rowNames[i]
  	isMean = FALSE
  	isVar = FALSE
  	parts = strsplit(name, "[ _]")[[1]]
  	parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
  	if (length(parts) == 2 && parts[1] == parts[2]) {
  		isVar = TRUE
  	} else if (length(parts) == 1) {
  		isVar = TRUE
  	}
  	if (isMean || isVar) {
  		dInd[i] = 0
  	}
  }
  chisqIndRaw = as.numeric(t(dInd) %*% weightMat %*% dInd)

  invertMatrix = function(x) {
  	chol2inv(chol(x))
  }
  getScalingFactor = function(jacMat, asymCovMat, weightMatVal, dfVal) {
  	info = t(jacMat) %*% weightMatVal %*% jacMat
  	infoInv = invertMatrix(info)
  	vw = asymCovMat %*% weightMatVal
  	vwDelta = vw %*% jacMat
  	uMat = vw - vwDelta %*% infoInv %*% t(jacMat) %*% weightMatVal
  	traceVal = sum(diag(uMat))
  	scalingFactor = traceVal / dfVal
  	return(scalingFactor)
  }

  jacInd = xmu_build_independence_jacobian(2)
  rownames(jacInd) = rownames(asymCov)

  cIndOrig = getScalingFactor(jacInd, asymCov, weightMat, 1)

  scaleFactor = chisqIndRaw / cIndOrig

  mScaled = mInd
  mScaled$data@observedStats$asymCov = mInd$data@observedStats$asymCov * scaleFactor

  resScaled = xmu_robust_WLS_fit(mScaled)
  expect_true(is.na(resScaled$TLI))
})
