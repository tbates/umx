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

test_that("umxRAM_GLM chain Petrol -> kms -> Poisson deaths runs", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(95)
	n = 60
	petrol = rnorm(n)
	kms = 0.2 + 0.5 * petrol + rnorm(n, 0, 0.4)
	dat = data.frame(
		DriversKilled = rpois(n, lambda = exp(0.3 + 0.4 * kms)),
		kms1k = kms,
		PetrolPrice = petrol
	)
	m = umxRAM_GLM("chain",
		umxPath("one", to = "DriversKilled"),
		umxPath("PetrolPrice", to = "kms1k"),
		umxPath("kms1k", to = "DriversKilled"),
		data = dat,
		families = list(DriversKilled = poisson()),
		autoRun = TRUE)
	expect_false(isTRUE(m$S$free["PetrolPrice", "PetrolPrice"]))
	expect_equal(as.numeric(m$S$values["PetrolPrice", "PetrolPrice"]), 0)
	expect_true(isTRUE(m$S$free["kms1k", "kms1k"]))
	expect_true(isTRUE(m$M$free[1, "kms1k"]))
	expect_true(isTRUE(m$M$free[1, "DriversKilled"]))
	expect_true(is.finite(as.numeric(m$output$fit)))
	expect_true(as.numeric(m$output$fit) > 0 && as.numeric(m$output$fit) < 1e6)
	expect_true(as.numeric(m$S$values["kms1k", "kms1k"]) > 0)
	expect_true(abs(as.numeric(m$A$values["kms1k", "PetrolPrice"])) < 5)
	expect_true(abs(as.numeric(m$A$values["DriversKilled", "kms1k"])) < 5)
	expect_message(umxSummary(m), "Observed predictors")
})

test_that("umxDummyCodeExogenousFactors matches the Dobson Poisson glm", {
	skip_if_not(exists("mxFamily", mode = "function"))
	counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
	outcome = gl(n = 3, k = 1, length = 9)
	treatment = gl(n = 3, k = 3)
	df = data.frame(counts = counts, outcome = outcome, treatment = treatment)
	glm1 = glm(counts ~ outcome + treatment, family = poisson(), data = df)
	m1 = umxRAM_GLM("rct",
		umxPath(c("outcome", "treatment"), to = "counts"),
		umxPath("one", to = "counts"),
		data = df,
		families = list(counts = poisson()),
		autoRun = FALSE)
	m2 = umxDummyCodeExogenousFactors(m1)
	expect_equal(length(m2@output), 0)
	expect_equal(sort(m2$manifestVars), sort(c("counts", "outcome2", "outcome3", "treatment2", "treatment3")))
	expect_false("outcome" %in% names(m2$data$observed))
	expect_false("treatment" %in% names(m2$data$observed))
	expect_false("outcome" %in% colnames(m2$A$values))
	for (dummyName in c("outcome2", "outcome3", "treatment2", "treatment3")) {
		expect_equal(as.numeric(m2$S$values[dummyName, dummyName]), 0)
		expect_false(isTRUE(m2$S$free[dummyName, dummyName]))
		expect_equal(as.numeric(m2$M$values[1, dummyName]), 0)
		expect_false(isTRUE(m2$M$free[1, dummyName]))
		expect_true(isTRUE(m2$A$free["counts", dummyName]))
	}
	expect_true(is.factor(df$outcome))
	m2 = mxRun(m2)
	b = coef(m2)
	g = coef(glm1)
	expect_equal(as.numeric(b["one_to_counts"]), as.numeric(g["(Intercept)"]), tolerance = 1e-3)
	expect_equal(as.numeric(b["outcome2_to_counts"]), as.numeric(g["outcome2"]), tolerance = 1e-3)
	expect_equal(as.numeric(b["outcome3_to_counts"]), as.numeric(g["outcome3"]), tolerance = 1e-3)
	expect_equal(as.numeric(b["treatment2_to_counts"]), as.numeric(g["treatment2"]), tolerance = 1e-3)
	expect_equal(as.numeric(b["treatment3_to_counts"]), as.numeric(g["treatment3"]), tolerance = 1e-3)
	expect_equal(as.numeric(m2$output$fit), -2 * as.numeric(logLik(glm1)), tolerance = 1e-4)

	glm3 = glm(counts ~ outcome + treatment, family = poisson(), data = df, contrasts = list(outcome = contr.treatment(3, base = 3)))
	m3 = umxDummyCodeExogenousFactors(m1, contrasts = "treatment", reference = c(outcome = "3"))
	expect_false("outcome3" %in% m3$manifestVars)
	expect_true("outcome1" %in% m3$manifestVars)
	expect_true("treatment2" %in% m3$manifestVars)
	m3 = mxRun(m3)
	b3 = coef(m3)
	g3 = coef(glm3)
	expect_equal(as.numeric(b3["one_to_counts"]), as.numeric(g3["(Intercept)"]), tolerance = 1e-3)
	expect_equal(as.numeric(b3["outcome1_to_counts"]), as.numeric(g3["outcome1"]), tolerance = 1e-3)
	expect_equal(as.numeric(b3["outcome2_to_counts"]), as.numeric(g3["outcome2"]), tolerance = 1e-3)
	expect_equal(as.numeric(b3["treatment2_to_counts"]), as.numeric(g3["treatment2"]), tolerance = 1e-3)
	expect_equal(as.numeric(b3["treatment3_to_counts"]), as.numeric(g3["treatment3"]), tolerance = 1e-3)
})

test_that("umxRun returns the fitted GLM model", {
	skip_if_not(exists("mxFamily", mode = "function"))
	counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
	df = data.frame(counts = counts, outcome = gl(3, 1, 9), treatment = gl(3, 3))
	glm1 = glm(counts ~ outcome + treatment, family = poisson(), data = df)
	m1 = umxRAM_GLM("rctRun",
		umxPath(c("outcome", "treatment"), to = "counts"),
		umxPath("one", to = "counts"),
		data = df,
		families = list(counts = poisson()),
		autoRun = FALSE)
	m1 = umxDummyCodeExogenousFactors(m1)
	mRun = umxRun(m1)
	expect_true(umx_has_been_run(mRun))
	expect_equal(as.numeric(mRun$output$fit), -2 * as.numeric(logLik(glm1)), tolerance = 1e-4)
	expect_equal(as.numeric(mRun$A$values["counts", "outcome2"]), as.numeric(coef(glm1)["outcome2"]), tolerance = 1e-3)
	expect_equal(as.numeric(mRun$M$values[1, "counts"]), as.numeric(coef(glm1)["(Intercept)"]), tolerance = 1e-3)
	expect_error(umxSummary(mRun), NA)
})

test_that("umxModify drops GLM predictors whose arrows are fixed at 0", {
	skip_if_not(exists("mxFamily", mode = "function"))
	counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
	df = data.frame(counts = counts, outcome = gl(3, 1, 9), treatment = gl(3, 3))
	glmOutcome = glm(counts ~ outcome, family = poisson(), data = df)
	m1 = umxRAM_GLM("rctMod",
		umxPath(c("outcome", "treatment"), to = "counts"),
		umxPath("one", to = "counts"),
		data = df,
		families = list(counts = poisson()),
		autoRun = FALSE)
	m1 = umxDummyCodeExogenousFactors(m1)
	m1 = mxRun(m1)
	m2 = umxModify(m1, c("treatment2_to_counts", "treatment3_to_counts"))
	expect_true(umx_has_been_run(m2))
	expect_false("treatment2" %in% m2$manifestVars)
	expect_false("treatment3" %in% m2$manifestVars)
	expect_true("outcome2" %in% m2$manifestVars)
	expect_equal(as.numeric(m2$output$fit), -2 * as.numeric(logLik(glmOutcome)), tolerance = 1e-3)
	m3 = umxModify(m1, "treatment2_to_counts", value = 0.2, free = FALSE)
	expect_true("treatment2" %in% m3$manifestVars)
	expect_true(umx_has_been_run(m3))
	expect_equal(as.numeric(m3$A$values["counts", "treatment2"]), 0.2, tolerance = 1e-8)
})

test_that("umxDummyCodeExogenousFactors leaves ordered factors and rejects a destination or a custom label", {
	skip_if_not(exists("mxFamily", mode = "function"))
	dat = data.frame(y = rpois(30, lambda = exp(0.2)), x = rep(c(1, 2), 15))
	m = umxRAM_GLM("ord",
		umxPath("x", to = "y"),
		umxPath("one", to = "y"),
		data = dat,
		families = list(y = poisson()),
		autoRun = FALSE)
	obs = m$data$observed
	obs$x = mxFactor(obs$x, levels = c(1, 2))
	m = mxModel(m, mxData(observed = obs, type = "raw"))
	out = expect_message(umxDummyCodeExogenousFactors(m), "Left ordered factors unchanged")
	expect_true("x" %in% out$manifestVars)
	expect_false(any(grepl("^x[0-9]", setdiff(out$manifestVars, "x"))))

	datG = data.frame(y = rpois(20, lambda = 2), g = rep(1:2, each = 10))
	mTo = umxRAM_GLM("toFactor",
		umxPath("y", to = "g"),
		umxPath("one", to = "y"),
		data = datG,
		families = list(y = poisson()),
		autoRun = FALSE)
	obsG = mTo$data$observed
	obsG$g = factor(obsG$g, levels = c(1, 2))
	mTo = mxModel(mTo, mxData(observed = obsG, type = "raw"))
	expect_error(umxDummyCodeExogenousFactors(mTo), "incoming")

	counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
	df = data.frame(counts = counts, outcome = gl(3, 1, 9), treatment = gl(3, 3))
	mLab = umxRAM_GLM("lab",
		umxPath(c("outcome", "treatment"), to = "counts"),
		umxPath("one", to = "counts"),
		data = df,
		families = list(counts = poisson()),
		autoRun = FALSE)
	mLab$A$labels["counts", "outcome"] = "mySlope"
	expect_error(umxDummyCodeExogenousFactors(mLab), "mySlope")
})
