# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxAPA.r")

test_that("umxAPA rejects a model passed as se (pipe double-feed)", {
	require(umx)
	m1 = lm(mpg ~ wt + disp, data = mtcars)
	# base |> rewrites m1 |> umxAPA(m1) to umxAPA(m1, m1): the model lands in se
	expect_error(umxAPA(m1, m1), "twice")
	expect_error(m1 |> umxAPA(m1, std = TRUE), "twice")
	# same guard in the glm branch
	df = mtcars
	df$highMpg = as.integer(df$mpg > 20)
	g1 = glm(highMpg ~ wt + disp, data = df, family = binomial)
	expect_error(umxAPA(g1, g1), "twice")
	# legitimate piping and explicit use keep working
	expect_error(m1 |> umxAPA(std = TRUE), NA)
	expect_error(umxAPA(m1, "wt"), NA)
	expect_error(umxAPA(m1), NA)
})

test_that("umxAPA anova cats the sentence and returns the table invisibly", {
	require(umx)
	m1 = lm(mpg ~ wt + cyl, data = mtcars)
	m2 = lm(mpg ~ wt, data = mtcars)
	an = anova(m2, m1)
	expect_output(umxAPA(an), "Adding the additional predictors significantly improved model fit, F(1, 29) = 13.22, p = 0.001.", fixed = TRUE)
	out = withVisible(umxAPA(an))
	expect_false(out$visible)
	expect_identical(out$value, an)
})
