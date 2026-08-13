library(OpenMx)
library(umx)
library(testthat)

test_that("T1 - unit-diag + XX3 formula invariant", {
  data(HSwls, package = "umx")
  m1 = umxRAM("WLS_HS", data = HSwls, type = "DWLS",
    umxPath(v.m. = c("visual", "verbal", "speed")),
    umxPath("visual", to = c("x1","x2","x3")),
    umxPath("verbal", to = c("x4","x5","x6")),
    umxPath("speed",  to = c("x7","x8","x9")),
    umxPath(unique.bivariate = c("visual","verbal","speed")),
    umxPath(v.m. = paste0("x", 1:9))
  )
  skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
  implWLS = mxGetExpected(m1, "covariance")
  expect_true(any(abs(diag(implWLS) - 1) > 0.05))
  
  mCat = umx:::xmu_catml_eval_model(m1)
  implCat = mxGetExpected(mCat, "covariance")
  expect_equal(as.numeric(diag(implCat)), rep(1, length(m1@manifestVars)), tolerance = 1e-6)
  
  R = m1$data$observedStats$cov[m1@manifestVars, m1@manifestVars]
  Rth = cov2cor(implWLS[m1@manifestVars, m1@manifestVars])
  n = m1$data$numObs
  p = length(m1@manifestVars)
  
  Fml <- function(S, Sig) {
    as.numeric(determinant(Sig, log = TRUE)$modulus) +
      sum(diag(solve(Sig) %*% S)) -
      as.numeric(determinant(S, log = TRUE)$modulus) - p
  }
  
  xx3Formula = n * Fml(R, Rth)
  xx3Wrong   = n * Fml(R, implWLS)
  cm = umx:::xmu_catml_discrepancy_at_WLS(m1)
  expect_equal(cm$fMlTarget, xx3Formula, tolerance = max(1, 0.02 * xx3Formula))
  expect_lt(abs(cm$fMlTarget - xx3Formula), abs(cm$fMlTarget - xx3Wrong))
})

test_that("T2 - indices not garbage on HSwls", {
  data(HSwls, package = "umx")
  m1 = umxRAM("WLS_HS", data = HSwls, type = "DWLS",
    umxPath(v.m. = c("visual", "verbal", "speed")),
    umxPath("visual", to = c("x1","x2","x3")),
    umxPath("verbal", to = c("x4","x5","x6")),
    umxPath("speed",  to = c("x7","x8","x9")),
    umxPath(unique.bivariate = c("visual","verbal","speed")),
    umxPath(v.m. = paste0("x", 1:9))
  )
  skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
  rf = xmu_robust_WLS_fit(m1)
  expect_equal(attr(rf, "correction"), "Savalei2021")
  expect_gt(rf$CFI, 0.85)
  expect_gt(rf$TLI, 0.75)
  expect_lt(rf$RMSEA, 0.16)
  expect_false(rf$CFI < 0.80 && rf$RMSEA > 0.18)
})

test_that("T3 - lavaan ballpark (if installed)", {
  skip_if_not_installed("lavaan")
  library(lavaan)
  data(HSwls, package = "umx")
  
  lavModel = "
    visual =~ x1 + x2 + x3
    verbal =~ x4 + x5 + x6
    speed  =~ x7 + x8 + x9
  "
  lavFit = cfa(lavModel, data = HSwls, ordered = paste0("x", 1:9), estimator = "WLSMV")
  lavCfiRobust = fitMeasures(lavFit, "cfi.robust")
  lavRmseaRobust = fitMeasures(lavFit, "rmsea.robust")
  
  m1 = umxRAM("WLS_HS", data = HSwls, type = "DWLS",
    umxPath(v.m. = c("visual", "verbal", "speed")),
    umxPath("visual", to = c("x1","x2","x3")),
    umxPath("verbal", to = c("x4","x5","x6")),
    umxPath("speed",  to = c("x7","x8","x9")),
    umxPath(unique.bivariate = c("visual","verbal","speed")),
    umxPath(v.m. = paste0("x", 1:9))
  )
  skip_if_not(!is.null(m1@output$implied_jacobian), "Current OpenMx engine does not support WLS Jacobians (Legacy OpenMx)")
  rf = xmu_robust_WLS_fit(m1)
  
  expect_lt(abs(rf$CFI - lavCfiRobust), 0.05)
  expect_lt(abs(rf$RMSEA - lavRmseaRobust), 0.04)
})

test_that("T5 - implied_correlation is a correlation", {
  data(HSwls, package = "umx")
  m1 = umxRAM("WLS_HS", data = HSwls, type = "DWLS",
    umxPath(v.m. = c("visual", "verbal", "speed")),
    umxPath("visual", to = c("x1","x2","x3")),
    umxPath("verbal", to = c("x4","x5","x6")),
    umxPath("speed",  to = c("x7","x8","x9")),
    umxPath(unique.bivariate = c("visual","verbal","speed")),
    umxPath(v.m. = paste0("x", 1:9))
  )
  ic = umx:::xmu_catml_implied_correlation(m1)
  expect_equal(as.numeric(diag(ic)), rep(1, nrow(ic)), tolerance = 1e-6)
  expect_true(all(abs(ic) <= 1 + 1e-8))
})
