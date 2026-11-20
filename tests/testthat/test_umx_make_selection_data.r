library(testthat)
library(umx)
library(OpenMx)

# Feature: umx_make_selection_data() generates Pearson-Aitken demo data
# (IQ/C -> EA collider + top-p selection) so no .rda must ship.
test_that("umx_make_selection_data returns pop and sel", {
	d = umx_make_selection_data()
	expect_equal(names(d), c("pop", "sel"))
	expect_equal(names(d$pop), c("IQ", "C", "EA"))
	expect_equal(nrow(d$pop), 6000)
	expect_true(nrow(d$sel) < nrow(d$pop))
	expect_true(min(d$sel$EA) >= quantile(d$pop$EA, 0.7))
	expect_equal(umx_make_selection_data(), d)
})

test_that("umx_make_selection_data validates inputs", {
	expect_error(umx_make_selection_data(N = -5), "positive")
	expect_error(umx_make_selection_data(r_IQ_C = 2), "correlation")
	expect_error(umx_make_selection_data(IQ_on_EA = 0.9, C_on_EA = 0.9), "residual")
	expect_error(umx_make_selection_data(prop = 1.5), "proportion")
})
