library(umx)
library(testthat)
context("umxRAM_GLM")

test_that("umxRAM_GLM intercept-only Poisson matches glm", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(91)
	dat = data.frame(y = rpois(60, lambda = exp(0.35)))
	g = glm(y ~ 1, family = poisson(), data = dat)
	m = umxRAM_GLM("poi",
		umxPath("one", to = "y"),
		data = dat,
		families = list(y = poisson()),
		autoRun = TRUE)
	expect_true(is(m$fitfunction, "MxFitFunctionGLM"))
	expect_equal(as.numeric(m$S$values["y", "y"]), 0)
	expect_false(isTRUE(m$S$free["y", "y"]))
	expect_equal(as.numeric(m$output$fit), -2 * as.numeric(logLik(g)), tolerance = 1e-5)
	expect_equal(as.numeric(m$output$estimate[1]), as.numeric(coef(g)[1]), tolerance = 1e-4)
})

test_that("umxRAM_GLM mixed Gaussian + Poisson builds S correctly", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(92)
	dat = data.frame(x = rnorm(40, 0.2, 1), z = rpois(40, 2))
	m = umxRAM_GLM("mix",
		umxPath("one", to = c("x", "z")),
		data = dat,
		families = list(z = poisson()),
		autoRun = FALSE)
	expect_true(is(m$fitfunction, "MxFitFunctionGLM"))
	expect_equal(as.numeric(m$S$values["z", "z"]), 0)
	expect_true(isTRUE(m$S$free["x", "x"]))
	expect_equal(length(m@latentVars), 0)
})

test_that("umxRAM_GLM mxPath x to y matches glm and does not give x residual S", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(93)
	n = 50
	x = rnorm(n)
	dat = data.frame(y = rpois(n, lambda = exp(0.15 + 0.4 * x)), x = x)
	g = glm(y ~ x, family = poisson(), data = dat)
	m = umxRAM_GLM("poiX",
		umxPath("one", to = "y"),
		umxPath("x", to = "y"),
		data = dat,
		families = list(y = poisson()),
		autoRun = TRUE)
	expect_equal(as.numeric(m$S$values["x", "x"]), 0)
	expect_false(isTRUE(m$S$free["x", "x"]))
	expect_equal(as.numeric(m$output$fit), -2 * as.numeric(logLik(g)), tolerance = 1e-4)
	b0 = as.numeric(m$output$estimate[grep("one_to_y", names(m$output$estimate))])
	b1 = as.numeric(m$output$estimate[grep("x_to_y", names(m$output$estimate))])
	expect_equal(b0, as.numeric(coef(g)[1]), tolerance = 1e-3)
	expect_equal(b1, as.numeric(coef(g)[2]), tolerance = 1e-3)
})

test_that("umxRAM_GLM observed predictor with large x matches glm and umxSummary does not call mxRefModels", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(94)
	n = 80
	kms1k = 10 + 4 * runif(n)
	dat = data.frame(y = rpois(n, lambda = exp(4.1 - 0.02 * kms1k)), kms1k = kms1k)
	g = glm(y ~ kms1k, family = poisson(), data = dat)
	m = umxRAM_GLM("poiK",
		umxPath("one", to = "y"),
		umxPath("kms1k", to = "y"),
		data = dat,
		families = list(y = poisson()),
		autoRun = TRUE)
	expect_equal(as.numeric(m$A$values["y", "kms1k"]), as.numeric(coef(g)[2]), tolerance = 0.05)
	expect_false(isTRUE(m$S$free["kms1k", "kms1k"]))
	expect_equal(as.numeric(m$S$values["kms1k", "kms1k"]), 0)
	expect_equal(as.numeric(m$output$fit), -2 * as.numeric(logLik(g)), tolerance = 1e-3)
	b0 = as.numeric(m$output$estimate[grep("one_to_y", names(m$output$estimate))])
	b1 = as.numeric(m$output$estimate[grep("kms1k_to_y", names(m$output$estimate))])
	expect_equal(b0, as.numeric(coef(g)[1]), tolerance = 1e-3)
	expect_equal(b1, as.numeric(coef(g)[2]), tolerance = 1e-3)
	expect_true(is.finite(b0) && abs(b0) < 20)
	expect_true(is.finite(b1) && abs(b1) < 2)
	out = capture.output(umxSummary(m))
	expect_false(grepl("mxRefModels", paste(out, collapse = "\n")))
	expect_message(umxSummary(m), "-2LL")
})
