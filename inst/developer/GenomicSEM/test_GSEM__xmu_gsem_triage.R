library(testthat)
library(umx)

test_that("umxGSEM Triage Logic Direct testing of xmu_gsem_triage works", {
  # Setup simple S and V dimensions
  sMat = diag(c(0.5, 0.5))
  
  # 1. Clean Matrix (Level 0)
  vClean = diag(c(0.01, 0.01, 0.01))
  res0 = umx:::xmu_gsem_triage(vMat = vClean, sMat = sMat)
  expect_equal(res0$triageLevel, 0)
  expect_equal(res0$smoothed, FALSE)
  
  # 2. Silent Ridge Fix (Level 1)
  vRidge = diag(c(0.01, 0.01, -1e-9))
  res1 = umx:::xmu_gsem_triage(vMat = vRidge, sMat = sMat)
  expect_equal(res1$triageLevel, 1)
  expect_equal(res1$smoothed, TRUE)
  # Verify that diagonals were adjusted by 1e-6
  expect_equal(diag(res1$V), diag(vRidge) + 1e-6)
  expect_equal(diag(res1$S), diag(sMat) + 1e-6)
  
  # 3. nearPD Coercion (Level 2)
  vNearPD = diag(c(0.01, 0.01, -0.01))
  expect_warning({
    res2 = umx:::xmu_gsem_triage(vMat = vNearPD, sMat = sMat)
  }, "Applying nearPD coercion")
  expect_equal(res2$triageLevel, 2)
  expect_equal(res2$smoothed, TRUE)
  
  # 4. Fatal Matrix Deficiency (Level 3)
  vFatal = diag(c(0.01, 0.01, -0.1))
  expect_error({
    umx:::xmu_gsem_triage(vMat = vFatal, sMat = sMat)
  }, "Fatal Matrix Deficiency: V matrix is severely non-positive definite")
  
  # 5. Missingness (NA) Catch
  vNA = diag(c(0.01, 0.01, 0.01))
  vNA[1, 2] = NA
  expect_error({
    umx:::xmu_gsem_triage(vMat = vNA, sMat = sMat)
  }, "Fatal Missingness: LDSC matrices contain raw NAs")
  
  # 6. smooth = FALSE Override
  expect_error({
    umx:::xmu_gsem_triage(vMat = vNearPD, sMat = sMat, smooth = FALSE)
  }, "Matrix is non-positive definite.*and smooth = FALSE")
})

test_that("Integration testing in umxGSEM works", {
  traits = c("T1", "T2")
  modelStr = "
  T1 ~~ T1
  T2 ~~ T2
  T1 ~~ T2
  "
  sMat = matrix(c(0.5, 0.2, 0.2, 0.6), nrow = 2, ncol = 2, dimnames = list(traits, traits))
  vechNames = c("T1 T1", "T2 T1", "T2 T2")
  
  # Case 0: Clean PD matrices (autoRun = FALSE to avoid NaN summary warnings)
  vClean = diag(c(0.01, 0.005, 0.012))
  dimnames(vClean) = list(vechNames, vechNames)
  fit0 = umxGSEM(model = modelStr, S = sMat, V = vClean, estimation = "DWLS", autoRun = FALSE)
  
  # Verify class and attributes (uses expect_s3_class because setting multiple class strings converts S4 to S3)
  expect_s3_class(fit0, "MxModelGSEM")
  expect_equal(attr(fit0, "gsem_triage")$triageLevel, 0)
  expect_equal(attr(fit0, "gsem_triage")$smoothed, FALSE)
  
  # Case 1: Microscopic NPD matrices (Silent Ridge)
  vRidge = diag(c(0.01, 0.005, -1e-9))
  dimnames(vRidge) = list(vechNames, vechNames)
  fit1 = umxGSEM(model = modelStr, S = sMat, V = vRidge, estimation = "DWLS", autoRun = FALSE)
  
  expect_s3_class(fit1, "MxModelGSEM")
  expect_equal(attr(fit1, "gsem_triage")$triageLevel, 1)
  expect_equal(attr(fit1, "gsem_triage")$smoothed, TRUE)
  
  # Case 2: Alertable NPD matrices (nearPD Coercion)
  vNearPD = diag(c(0.01, 0.005, -0.01))
  dimnames(vNearPD) = list(vechNames, vechNames)
  
  # Capturing warning from package method
  expect_warning({
    fit2 = umxGSEM(model = modelStr, S = sMat, V = vNearPD, estimation = "DWLS", autoRun = FALSE)
  }, "Applying nearPD coercion")
  
  expect_s3_class(fit2, "MxModelGSEM")
  expect_equal(attr(fit2, "gsem_triage")$triageLevel, 2)
  expect_equal(attr(fit2, "gsem_triage")$smoothed, TRUE)
})
