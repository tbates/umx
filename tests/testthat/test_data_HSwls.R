library(testthat)
library(umx)

test_that("HSwls dataset loads and has correct ordinal structure", {
  # Load the dataset
  data(HSwls, package = "umx")
  
  # 1. Check classes and dimensions
  expect_s3_class(HSwls, "data.frame")
  expect_equal(dim(HSwls), c(301, 15))
  
  # 2. Check column names
  expected_cols = c("id", "sex", "ageyr", "agemo", "school", "grade",
                    "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9")
  expect_equal(names(HSwls), expected_cols)
  
  # 3. Check that x1 to x9 are ordered factors with the correct number of levels
  for (i in 1:9) {
    var_name = paste0("x", i)
    x = HSwls[[var_name]]
    expect_true(is.factor(x), info = paste(var_name, "is not a factor"))
    expect_true(is.ordered(x), info = paste(var_name, "is not an ordered factor"))
    
    expected_levels = if (i <= 3) 2 else if (i <= 6) 3 else 4
    expect_equal(nlevels(x), expected_levels, info = paste(var_name, "has incorrect number of levels"))
    
    # Also verify the OpenMx specific mxFactor attribute is TRUE
    expect_true(attr(x, "mxFactor"), info = paste(var_name, "does not have mxFactor attribute set to TRUE"))
  }
})
