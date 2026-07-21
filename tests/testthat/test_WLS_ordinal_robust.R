library(testthat)
library(OpenMx)
library(umx)

test_that("Ordinal WLS Satorra-Bentler robust fit statistics and difference tests work with complete Jacobian", {
  set.seed(12345)
  n  = 500
  f  = rnorm(n)
  x1 = 0.75 * f + rnorm(n, sd = 0.66)
  x2 = 0.80 * f + rnorm(n, sd = 0.60)
  x3 = 0.70 * f + rnorm(n, sd = 0.71)
  x4 = 0.85 * f + rnorm(n, sd = 0.53)

  dat = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)
  dat[] = lapply(dat, function(x) cut(x, breaks = 4, labels = FALSE))
  dat[] = lapply(dat, ordered)

  # Fit base model
  m1 = suppressWarnings(umxRAM("WLS_ordinal_base", data = dat, type = "WLS",
    umxPath("f", to = c("x1", "x2", "x3", "x4") ),
    umxPath(v.m. = "f"),
    umxPath(v.m. = c("x1", "x2", "x3", "x4"))
  ))
  m1 = mxRun(m1, silent = TRUE)
  
  # Run summary and assert it completes successfully
  m1Summary = suppressWarnings(umxSummary(m1))
  expect_true(!is.null(m1Summary))

  # Fit nested model (drop f to x1 path)
  m2 = umxModify(m1, update = "f_to_x1", name = "WLS_ordinal_nested")
  expect_s4_class(m2, "MxModel")
  
  # Compare models and assert Satorra-Bentler results
  comp = suppressWarnings(umxCompare(m1, m2))
  expect_s3_class(comp, "data.frame")
  expect_equal(dim(comp), c(2, 12))
  expect_equal(comp$delta_df[2], 1)
  
  # Check if Jacobian is present to determine if robust metrics should be calculated or NA
  hasJacobian = !is.null(m1$output$implied_jacobian)
  if (hasJacobian) {
    # Assert robust CFI is not NA
    expect_false(is.na(comp$CFI[1]))
    expect_false(is.na(comp$diffFit[2]))
    expect_false(is.na(comp$p[2]))
  } else {
    # Under legacy OpenMx, robust fit metrics return NA
    expect_true(is.na(comp$CFI[1]))
    expect_true(is.na(comp$diffFit[2]))
    expect_true(is.na(comp$p[2]))
  }
})

test_that("Raw WLS binary variables mean handling in umxRAM works", {
  data(HSwls, package = "umx")
  
  # Run umxRAM and capture model
  mDWLS = suppressMessages(umxRAM("One_Factor_DWLS", data = HSwls, type = "DWLS",
       umxPath("g", to = paste0("x", 1:9)),
       umxPath(var = paste0("x", 1:9)),
       umxPath(var = "g", fixedAt = 1)
  ))
  
  # Verify that binary variables x1, x2, x3 have their expected means fixed to 0
  expect_false(mDWLS$M$free[1, "x1"])
  expect_false(mDWLS$M$free[1, "x2"])
  expect_false(mDWLS$M$free[1, "x3"])
  
  expect_equivalent(mDWLS$M$values[1, "x1"], 0)
  expect_equivalent(mDWLS$M$values[1, "x2"], 0)
  expect_equivalent(mDWLS$M$values[1, "x3"], 0)

  # Binary residual variance fixed at 1
  expect_false(mDWLS$S$free["x1", "x1"])
  expect_false(mDWLS$S$free["x2", "x2"])
  expect_false(mDWLS$S$free["x3", "x3"])
  expect_equivalent(mDWLS$S$values["x1", "x1"], 1)
  expect_equivalent(mDWLS$S$values["x3", "x3"], 1)
  
  # Verify that ordinal variables x4 to x9 have free expected means and residuals
  expect_true(mDWLS$M$free[1, "x4"])
  expect_true(mDWLS$M$free[1, "x9"])
  expect_true(mDWLS$S$free["x4", "x4"])
  expect_true(mDWLS$S$free["x9", "x9"])
})

test_that("umxRAM fixes binary ID even when user requests free v.m.", {
  data(HSwls, package = "umx")
  m1 = suppressMessages(umxRAM("vm_bin", data = HSwls[1:300, ], type = "FIML", autoRun = FALSE,
    umxPath("g", to = paste0("x", 1:3)),
    umxPath(v.m. = paste0("x", 1:3)),
    umxPath(var = "g", fixedAt = 1)
  ))
  expect_false(m1$M$free[1, "x1"])
  expect_equivalent(m1$M$values[1, "x1"], 0)
  expect_false(m1$S$free["x1", "x1"])
  expect_equivalent(m1$S$values["x1", "x1"], 1)
})

test_that("xmu_threshold_id_RAM is silent when already identified", {
  data(HSwls, package = "umx")
  m1 = suppressMessages(umxRAM("id_ok", data = HSwls[1:200, ], type = "FIML", autoRun = FALSE,
    umxPath("g", to = paste0("x", 1:3)),
    umxPath(var = paste0("x", 1:3), fixedAt = 1),
    umxPath(means = paste0("x", 1:3), fixedAt = 0),
    umxPath(var = "g", fixedAt = 1)
  ))
  # Re-run identification: should not message when already correct
  msgs = capture.output({
    m2 = xmu_threshold_id_RAM(m1, action = "fix", verbose = TRUE)
  }, type = "message")
  expect_length(msgs, 0)
  expect_false(m2$M$free[1, "x1"])
  expect_false(m2$S$free["x1", "x1"])
})
