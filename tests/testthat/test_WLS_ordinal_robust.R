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
  m1 = umxRAM("WLS_ordinal_base", data = dat, type = "WLS",
    umxPath("f", to = c("x1", "x2", "x3", "x4") ),
    umxPath(v.m. = "f"),
    umxPath(v.m. = c("x1", "x2", "x3", "x4"))
  )
  m1 = mxRun(m1, silent = TRUE)
  
  # Run summary and assert it completes successfully
  m1Summary = umxSummary(m1)
  expect_true(!is.null(m1Summary))

  # Fit nested model (drop f to x1 path)
  m2 = umxModify(m1, update = "f_to_x1", name = "WLS_ordinal_nested")
  expect_s4_class(m2, "MxModel")
  
  # Compare models and assert Satorra-Bentler results
  comp = umxCompare(m1, m2)
  expect_s3_class(comp, "data.frame")
  expect_equal(dim(comp), c(2, 12))
  expect_equal(comp$delta_df[2], 1)
  
  # Assert robust CFI is not NA
  expect_false(is.na(comp$CFI[1]))
  expect_false(is.na(comp$diffFit[2]))
  expect_false(is.na(comp$p[2]))
})
