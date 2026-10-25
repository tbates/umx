# library(testthat)
# library(umx)
# testthat::test_file("~/bin/umx/tests/testthat/test_umx_residualize.r") 
# test_package("umx")
test_that("umx_residualize works for different inputs", {
	data(mtcars)
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
test_that("umx_residualize accepts data first, as in a pipe", {
	data(mtcars)
	tmp = mtcars
	tmp$mpg_T1  = tmp$mpg_T2  = tmp$mpg
	tmp$cyl_T1  = tmp$cyl_T2  = tmp$cyl
	tmp$disp_T1 = tmp$disp_T2 = tmp$disp
	ref  = umx_residualize("mpg", c("cyl", "disp"), data = tmp)
	refW = umx_residualize("mpg", c("cyl", "disp"), c("_T1", "_T2"), data = tmp)
	refF = umx_residualize(mpg ~ cyl + I(cyl^2) + disp, data = tmp)

	# base pipe: data lands in var
	expect_equal(tmp |> umx_residualize("mpg", c("cyl", "disp")), ref)
	expect_equal(tmp |> umx_residualize("mpg", c("cyl", "disp"), c("_T1", "_T2")), refW)
	expect_equal(tmp |> umx_residualize(mpg ~ cyl + I(cyl^2) + disp), refF)
	# mixed named and positional
	expect_equal(tmp |> umx_residualize("mpg", covs = c("cyl", "disp")), ref)
	expect_equal(tmp |> umx_residualize(covs = c("cyl", "disp"), "mpg"), ref)
	expect_equal(tmp |> umx_residualize(var = "mpg", c("cyl", "disp"), suffixes = c("_T1", "_T2")), refW)
	# data first with named var and covs, so data lands in suffixes
	expect_equal(umx_residualize(tmp, var = "mpg", covs = c("cyl", "disp")), ref)
	expect_equal(umx_residualize(tmp, var = "mpg", cov = c("cyl", "disp")), ref)
	# several DVs
	expect_equal(tmp |> umx_residualize(c("mpg", "hp"), c("cyl", "disp")),
		umx_residualize(c("mpg", "hp"), c("cyl", "disp"), data = tmp))
	# no data at all
	expect_error(umx_residualize("mpg", c("cyl", "disp")), "needs data")
})

test_that("umx_residualize numeric suffixes and positional data still work", {
	data(twinData)
	a = umx_residualize("bmi", "age", suffixes = 1:2, twinData)
	b = umx_residualize("bmi", "age", suffixes = 1:2, data = twinData)
	expect_equal(a, b)
	expect_equal(twinData |> umx_residualize("bmi", "age", suffixes = 1:2), b)
})
