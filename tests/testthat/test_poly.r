library(umx)
library(testthat)

umx_set_silent(TRUE)

test_that("umx_polychoric work", {
	tmp = mtcars
	tmp$am = umxFactor(mtcars$am)
	tmp$vs = umxFactor(mtcars$vs)
	tmp = umx_scale(tmp)
	x = umx_polychoric(tmp[, c("am", "vs")], tryHard = "yes")
	x$polychorics
	cor(mtcars[, c("am", "vs")])
})
