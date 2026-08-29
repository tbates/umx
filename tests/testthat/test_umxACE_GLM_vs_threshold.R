library(umx)
library(testthat)
context("umxACE_GLM vs liability threshold")

# Liability ACE: y* = b0 + aA + cC + eE, y = 1{y* > 0}.
# Given A,C,E, y is determined. umxACE binary is that model.
# GLM Bernoulli: P(y=1|eta) = logit/probit(eta), eta = b0 + aA + cC + eE.
# That adds leftover unique variance on top of e, so e (and often a) collapse.
# This is why GLM ACE was not used for binary traits: it failed to show
# heritability in known samples. Threshold ACE is the liability model.

simLiabilityPair = function(rA, n, trueA, trueC, trueE, trueB0) {
	A = rnorm(n)
	C = rnorm(n)
	E1 = rnorm(n)
	E2 = rnorm(n)
	A2 = rA * A + sqrt(max(0, 1 - rA * rA)) * rnorm(n)
	ystar1 = trueB0 + trueA * A + trueC * C + trueE * E1
	ystar2 = trueB0 + trueA * A2 + trueC * C + trueE * E2
	data.frame(y_T1 = as.integer(ystar1 > 0), y_T2 = as.integer(ystar2 > 0))
}

aceStd = function(model) {
	a = as.numeric(mxEval(top.a, model)[1, 1])
	c = as.numeric(mxEval(top.c, model)[1, 1])
	e = as.numeric(mxEval(top.e, model)[1, 1])
	V = a * a + c * c + e * e
	list(a = a, c = c, e = e, aStd = a / sqrt(V), cStd = c / sqrt(V), eStd = e / sqrt(V), V = V)
}

glmLoad = function(model) {
	pars = omxGetParameters(model)
	a = as.numeric(pars["a_r1c1"])
	c = as.numeric(pars["c_r1c1"])
	e = if ("e_r1c1" %in% names(pars)) as.numeric(pars["e_r1c1"]) else as.numeric(model$MZ$A$values["y_T1", "e1_T1"])
	b0 = as.numeric(pars[grep("^one_to_y", names(pars))[1]])
	V = a * a + c * c + e * e
	list(a = a, c = c, e = e, b0 = b0, aStd = if (V > 0) (a * a) / V else NA, cStd = if (V > 0) (c * c) / V else NA, eStd = if (V > 0) (e * e) / V else NA)
}

test_that("at n=80, liability umxACE holds A better than GLM Bernoulli ACE on the same liability data", {
	skip_if_not(exists("mxFamily", mode = "function"))
	set.seed(13)
	n = 80
	trueA = 0.5
	trueC = 0.3
	trueE = 0.4
	trueB0 = 0.2
	trueV = trueA * trueA + trueC * trueC + trueE * trueE
	trueAstd = trueA / sqrt(trueV)
	mzRaw = simLiabilityPair(1, n, trueA, trueC, trueE, trueB0)
	dzRaw = simLiabilityPair(0.5, n, trueA, trueC, trueE, trueB0)

	mzFac = mzRaw
	dzFac = dzRaw
	mzFac$y_T1 = mxFactor(mzFac$y_T1, levels = c(0, 1))
	mzFac$y_T2 = mxFactor(mzFac$y_T2, levels = c(0, 1))
	dzFac$y_T1 = mxFactor(dzFac$y_T1, levels = c(0, 1))
	dzFac$y_T2 = mxFactor(dzFac$y_T2, levels = c(0, 1))

	mThr = umxACE(selDVs = "y", mzData = mzFac, dzData = dzFac, sep = "_T", autoRun = TRUE, tryHard = "yes", addCI = FALSE, intervals = FALSE)
	mLogit = umxACE_GLM(selDVs = "y", mzData = mzRaw, dzData = dzRaw, sep = "_T", family = stats::binomial(link = "logit"), autoRun = TRUE, tryHard = "yes")
	mProbit = umxACE_GLM(selDVs = "y", mzData = mzRaw, dzData = dzRaw, sep = "_T", family = stats::binomial(link = "probit"), autoRun = TRUE, tryHard = "yes")

	thr = aceStd(mThr)
	glo = glmLoad(mLogit)
	gpr = glmLoad(mProbit)

	cat("\nLiability DGP n=80 true raw a,c,e,b0 =", trueA, trueC, trueE, trueB0, " true a_std =", round(trueAstd, 3), "\n")
	cat("umxACE threshold  a,c,e raw =", round(thr$a, 3), round(thr$c, 3), round(thr$e, 3), " a_std =", round(thr$aStd, 3), "\n")
	cat("GLM logit          a,c,e    =", round(glo$a, 3), round(glo$c, 3), round(glo$e, 3), " a^2 share =", round(glo$aStd, 3), "\n")
	cat("GLM probit         a,c,e    =", round(gpr$a, 3), round(gpr$c, 3), round(gpr$e, 3), " a^2 share =", round(gpr$aStd, 3), "\n")

	expect_true(is.finite(thr$a) && is.finite(glo$a) && is.finite(gpr$a))
	expect_equal(glo$e, 1)
	expect_equal(gpr$e, 1)
	# Threshold ACE is the liability DGP. GLM e_ii is fixed at 1 (scale).
	expect_true(thr$aStd > 0.15)
})
