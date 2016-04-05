# library(testthat)
# library(umx)
# testthat::test_file("~/bin/umx/tests/testthat/test_umx_residualize.r") 
# test_package("umx")
test_that("umx_residualize works for different inputs", {
	tmp = mtcars
	tmp$mpg_T1  = tmp$mpg_T2  = tmp$mpg
	tmp$cyl_T1  = tmp$cyl_T2  = tmp$cyl
	tmp$disp_T1 = tmp$disp_T2 = tmp$disp

	# ===================
	# = test one listed =
	# ===================
	a = umx_residualize("mpg", c("cyl", "disp"), data = tmp)
	b = residuals(lm(mpg ~ cyl + disp, data = tmp, na.action = na.exclude))
	testthat::expect_true(all(a$mpg == b))

	# =========================
	# = formula interface & I =
	# =========================
	a = umx_residualize(mpg ~ cyl + I(cyl^2) + disp, data = tmp)
	b = residuals(lm(mpg ~ cyl + I(cyl^2) + disp, data = tmp, na.action = na.exclude))
	testthat::expect_true(all(a$mpg == b))

	# ========================================================================
	# = Demonstrate ability to residualize WIDE data (i.e. 1 family per row) =
	# ========================================================================
	a = umx_residualize("mpg", c("cyl", "disp"), c("_T1", "_T2"), data = tmp) # [1:5,12:17]
	b = residuals(lm(mpg ~ cyl + disp, data = rbind(tmp, tmp), na.action = na.exclude))
	testthat::expect_equal(a$mpg_T1, as.numeric(b)[1:dim(tmp)[1]])

	# ===================================
	# = Residualise several DVs at once =
	# ===================================
	a = umx_residualize(c("mpg", "hp"), cov = c("cyl", "disp"), data = tmp)
	b = residuals(lm(mpg ~ cyl + disp, data = tmp, na.action = na.exclude))
	c = residuals(lm(hp ~ cyl + disp, data = tmp, na.action = na.exclude))
	testthat::expect_equal(a$mpg, as.numeric(b))
	testthat::expect_equal(a$hp , as.numeric(c))
})