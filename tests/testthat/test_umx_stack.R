library(umx)
library(testthat)
context("umx_stack")

test_that("umx_stack reshapes wide to long, passing along variables", {
	df = umx_stack(mtcars, select = c("disp", "hp"), passalong = "mpg")
	expect_equal(nrow(df), 2L * nrow(mtcars))
	expect_equal(names(df), c("mpg", "values", "ind"))
	expect_true(is.factor(df$ind))
	expect_equal(levels(df$ind), c("disp", "hp"))
	expect_equal(df$values, c(mtcars$disp, mtcars$hp))
	expect_equal(df$mpg, rep(mtcars$mpg, times = 2))
})

test_that("umx_stack example plot builds with ggplot2 not attached (R CMD check env)", {
	if (!requireNamespace("ggplot2", quietly = TRUE)) {
		skip("ggplot2 not installed")
	}
	if ("package:ggplot2" %in% search()) {
		skip("ggplot2 attached: cannot simulate R CMD check environment")
	}
	df = umx_stack(mtcars, select = c("disp", "hp"), passalong = "mpg")
	p = ggplot2::ggplot(df, ggplot2::aes(x = mpg, y = values, colour = ind)) + ggplot2::geom_point() + ggplot2::geom_smooth()
	expect_true(inherits(p, "ggplot"))
})

test_that("umx_stack shipped example uses only qualified ggplot2 calls", {
	code = example("umx_stack", package = "umx", character.only = TRUE, give.lines = TRUE)
	bare = grep("(?<![:\\w])(ggplot|aes|geom_point|geom_smooth)\\s*\\(", code, perl = TRUE, value = TRUE)
	expect_equal(length(bare), 0L)
})
