library(testthat)
library(umx)
library(OpenMx)

test_that("umx_is_RAM detects RAM, containers, and non-RAM", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("isRAM_ex", data = demoOneFactor[, v], type = "cov", autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	expect_true(umx_is_RAM(m1))
	# A multigroup container over RAM submodels counts as RAM
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], type = "cov", autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(var = v),
			umxPath(var = "G", fixedAt = 1)
		)
	}
	mg = mxModel("isRAM_mg", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]),
		mxFitFunctionMultigroup(c("g1", "g2")))
	expect_true(umx_is_RAM(mg))
	# A matrix/algebra model is not RAM
	mat = mxModel("isRAM_mat",
		mxMatrix("Full", 3, 1, free = TRUE, values = .5, name = "L"),
		mxMatrix("Diag", 3, 3, free = TRUE, values = .5, name = "E"),
		mxMatrix("Full", 1, 3, free = TRUE, values = 0, name = "M"),
		mxAlgebra(L %*% t(L) + E, name = "S"),
		mxExpectationNormal("S", means = "M", dimnames = v),
		mxFitFunctionML(),
		mxData(demoOneFactor[, v], type = "raw"))
	expect_false(umx_is_RAM(mat))
	# A container mixing RAM and non-RAM is not RAM
	mix = mxModel("isRAM_mix", mkGroup("g1", demoOneFactor[1:250, ]), mat,
		mxFitFunctionMultigroup(c("g1", "isRAM_mat")))
	expect_false(umx_is_RAM(mix))
	# Empty models and non-models are not RAM
	expect_false(umx_is_RAM(mxModel("isRAM_empty")))
	expect_false(umx_is_RAM(lm(mpg ~ wt, data = mtcars)))
})

test_that("umxSummary tables raw estimates for matrix models", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mat = mxRun(mxModel("sumMat_ex",
		mxMatrix("Full", 3, 1, free = TRUE, values = .5, labels = paste0("l", 1:3), name = "L"),
		mxMatrix("Diag", 3, 3, free = TRUE, values = .5, labels = paste0("e", 1:3), name = "E"),
		mxMatrix("Full", 1, 3, free = TRUE, values = 0, labels = paste0("m", 1:3), name = "M"),
		mxAlgebra(L %*% t(L) + E, name = "S"),
		mxExpectationNormal("S", means = "M", dimnames = v),
		mxFitFunctionML(),
		mxData(demoOneFactor[, v], type = "raw")), silent = TRUE)
	s = umxSummary(mat, std = FALSE)
	expect_s3_class(s, "data.frame")
	expect_equal(nrow(s), 9)
	expect_true(all(c("name", "Estimate", "SE") %in% names(s)))
	# Raw estimates match OpenMx summary()$parameters
	omxPars = summary(mat)$parameters
	for(i in 1:nrow(s)){
		expect_equal(s$Estimate[i], omxPars$Estimate[omxPars$name == s$name[i]])
	}
	# Asking for std gives raw plus one polite message
	expect_message(s2 <- umxSummary(mat, std = TRUE), "showing raw estimates")
	expect_equal(s2$Estimate, s$Estimate)
})

test_that("umxSummary keeps the RAM path for multigroup RAM", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], type = "cov", autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(var = v),
			umxPath(var = "G", fixedAt = 1)
		)
	}
	mg = mxRun(omxAssignFirstParameters(mxModel("sumMG_ex",
		mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]),
		mxFitFunctionMultigroup(c("g1", "g2")))), silent = TRUE)
	s = umxSummary(mg, std = TRUE)
	expect_true("Std.Estimate" %in% names(s))
})

test_that("xmu_safe_run_summary keeps the model when summary cannot run", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("router_ex", data = demoOneFactor[, v], type = "cov",
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	# summary off + returning summary gives NA, not an object-not-found error
	out = xmu_safe_run_summary(m1, autoRun = FALSE, summary = FALSE, returning = "summary")
	expect_true(is.na(out))
	# a model that fails to run is returned, not lost to a re-signalled error
	broken = mxModel("router_broken",
		mxMatrix("Full", 1, 1, free = TRUE, values = .5, name = "A"),
		mxMatrix("Full", 1, 1, free = FALSE, values = 1, name = "S"),
		mxMatrix("Full", 1, 1, free = FALSE, values = 1, name = "F"),
		mxMatrix("Full", 1, 1, free = FALSE, values = 0, name = "M"),
		mxExpectationRAM("A", "S", "F", "M", dimnames = "x"),
		mxFitFunctionML())
	out = xmu_safe_run_summary(broken, summary = FALSE, returning = "model")
	expect_equal(out$name, "router_broken")
})

test_that("single-model tools still refuse multigroup containers", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	mkGroup = function(name, d) {
		umxRAM(name, data = d[, v], type = "cov", autoRun = FALSE,
			umxPath("G", to = v),
			umxPath(var = v),
			umxPath(var = "G", fixedAt = 1)
		)
	}
	mg = mxModel("guardMG_ex", mkGroup("g1", demoOneFactor[1:250, ]), mkGroup("g2", demoOneFactor[251:500, ]),
		mxFitFunctionMultigroup(c("g1", "g2")))
	expect_error(tmx_show(mg), "only knows how to display")
	expect_error(umx:::xmuLabel_RAM_Model(mg), "must be an OpenMx RAM Model")
	expect_error(umx:::xmuRAM2Ordinal(mg), "only works with RAM models")
	# umxRun on a container skips saturated-fit work instead of crashing on missing data
	out = umxRun(omxAssignFirstParameters(mg), tryHard = "no", summary = FALSE)
	expect_equal(out$name, "guardMG_ex")
	expect_true(umx_has_been_run(out))
})
