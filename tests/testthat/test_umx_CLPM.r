# library(testthat)
# library(umx)
# test_package("umx")
# test_active_file("~/bin/umx/tests/testthat/test_umx_CLPM.r")

test_that("testing umxCLPM", {
  hamaker <- umxCLPM(waves = 4, name = "mymodel", model = "Hamaker2015")
  is(mxModel(hamaker))
})
