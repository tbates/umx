# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_residuals.MxModel.r") 
# test_package("umx")
# TODO make tests for residuals!
# [] need to get the text output test working
# [] need to test suppress,
# [] digits
# [] Latents in RAM
# [] Latents non-RAM !
context("residuals()")

test_that("residuals.MxModel works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("test", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
		umxPath("g", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "g", fixedAt = 1)
	)

	expect_output(residuals(m1))
	# "
	# |   |x1   |x2    |x3   |x4    |x5 |
	# |:--|:----|:-----|:----|:-----|:--|
	# |x1 |.    |.     |0.01 |.     |.  |
	# |x2 |.    |.     |0.01 |-0.01 |.  |
	# |x3 |0.01 |0.01  |.    |.     |.  |
	# |x4 |.    |-0.01 |.    |.     |.  |
	# |x5 |.    |.     |.    |.     |.  |
	# [1] \"nb: You can zoom in on bad values with, e.g. suppress = .01, which will hide values smaller than this. Use digits = to round\""
})
