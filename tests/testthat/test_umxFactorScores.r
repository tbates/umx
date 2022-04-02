# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_misc.r") 
# 
# testthat::test_package("umx")

context("umxFactorScores")

test_that("umx_scale works for different inputs", {
	data(mttcars)
	m1 = umxEFA(mtcars, factors = 2)
	x = umxFactorScores(m1, type = 'Regression', minManifests = 3)
	# =========================================================================
	# = histogram of F1 and plot of F1 against F2 showing they are orthogonal =
	# =========================================================================
	hist(x$F1)
	plot(F1 ~ F2, data = x)
	m1 = umxEFA(mtcars, factors = 1)
	x = umxFactorScores(m1, type = 'Regression', minManifests = 3)
	x
})
