library(testthat)
library(umx)

test_that("umxGSEM fits simple bivariate model and subsets correctly", {
	# Setup simple bivariate heritability and correlation data
	traits = c("T1", "T2")
	S = matrix(c(0.50, 0.20, 0.20, 0.60), nrow = 2, ncol = 2, dimnames = list(traits, traits))
	V = diag(c(0.01, 0.005, 0.012))
	vech_names = c("T1 T1", "T2 T1", "T2 T2")
	dimnames(V) = list(vech_names, vech_names)
	
	# Model specification
	m1 = "
	T1 ~~ T1
	T2 ~~ T2
	T1 ~~ T2
	"
	
	# Fit model using DWLS
	fit = umxGSEM(m1, S = S, V = V, estimation = "DWLS", autoRun = TRUE)
	
	expect_s4_class(fit, "MxModelGSEM")
	expect_false(identical(fit$data$type, "acov"))
	expect_false(inherits(fit$data, "MxDataLegacyWLS"))
	os = fit$data$observedStats
	expect_equal(os$cov, S)
	expect_true(!is.null(os$useWeight))
	expect_true(!is.null(os$asymCov))
	expect_equal(diag(os$useWeight), 1 / diag(os$asymCov), tolerance = 1e-10)
	expect_s4_class(fit$fitfunction, "MxFitFunctionWLS")
	expect_equal(fit$fitfunction$type, "DWLS")
	
	# Setup 3-trait data to test subsetting
	traits3 <- c("T1", "T2", "T3")
	S3 <- matrix(c(0.50, 0.20, 0.10,
	               0.20, 0.60, 0.15,
	               0.10, 0.15, 0.70), nrow = 3, ncol = 3, dimnames = list(traits3, traits3))
	V3 <- diag(c(0.01, 0.005, 0.002, 0.012, 0.004, 0.015))
	vech_names3 <- c("T1 T1", "T2 T1", "T3 T1", "T2 T2", "T3 T2", "T3 T3")
	dimnames(V3) <- list(vech_names3, vech_names3)
	
	# Fit bivariate model referencing only T1 and T2 using the 3-trait S3 and V3 data
	fit_subset <- umxGSEM(model = m1, S = S3, V = V3, estimation = "DWLS", autoRun = TRUE)
	
	expect_s4_class(fit_subset, "MxModelGSEM")
	expect_equal(fit_subset$manifestVars, c("T1", "T2"))
	expect_false(identical(fit_subset$data$type, "acov"))
	expect_equal(fit_subset$data$observedStats$cov, S) # 2x2 S of T1 and T2
	expect_equal(nrow(fit_subset$data$observedStats$asymCov), 3) # 3 unique pairs
	
	# Test tmx_show on genomic SEM matrices
	tmx_show(fit, matrices = "data.S", report = "markdown")
	tmx_show(fit, matrices = "data.V", report = "markdown")
	tmx_show(fit, matrices = "V", report = "markdown")
	tmx_show(fit, report = "markdown")
})

test_that("umxGSEM uses modern observedStats (useWeight + asymCov), not type=acov", {
	traits3 <- c("T1", "T2", "T3")
	S3 <- matrix(c(0.50, 0.20, 0.10,
	               0.20, 0.60, 0.15,
	               0.10, 0.15, 0.70), nrow = 3, ncol = 3, dimnames = list(traits3, traits3))
	# 6 unique lower-tri elements for 3x3 S
	V3 <- diag(c(0.01, 0.005, 0.002, 0.012, 0.004, 0.015))
	vech_names3 <- c("T1 T1", "T2 T1", "T3 T1", "T2 T2", "T3 T2", "T3 T3")
	dimnames(V3) <- list(vech_names3, vech_names3)

	model_str <- "
	F1 =~ NA*T1 + T2 + T3
	F1 ~~ 1*F1
	"
	fit <- umxGSEM(model = model_str, S = S3, V = V3, estimation = "DWLS", autoRun = FALSE, name = "g3")
	expect_s4_class(fit, "MxModelGSEM")
	# Modern path: no legacy type='acov'
	expect_false(identical(fit$data$type, "acov"))
	os <- fit$data$observedStats
	expect_true(!is.null(os$cov))
	expect_true(!is.null(os$useWeight))
	expect_true(!is.null(os$asymCov))
	# useWeight should be inverse of asymCov diagonal for DWLS
	expect_equal(diag(os$useWeight), 1 / diag(os$asymCov), tolerance = 1e-10)
	# OpenMx residual dimnames
	expect_true(all(grepl("^(var_|poly_)", colnames(os$asymCov))))
})

test_that("umxSummary and umxCompare handle WLS models correctly", {
	# Setup simple bivariate heritability and correlation data
	traits <- c("T1", "T2")
	S <- matrix(c(0.50, 0.20, 0.20, 0.60), nrow = 2, ncol = 2, dimnames = list(traits, traits))
	V <- diag(c(0.01, 0.005, 0.012))
	vech_names <- c("T1 T1", "T2 T1", "T2 T2")
	dimnames(V) <- list(vech_names, vech_names)
	
	m1 <- "
	T1 ~~ T1
	T2 ~~ T2
	T1 ~~ T2
	"
	fit1 <- umxGSEM(m1, S = S, V = V, estimation = "DWLS", autoRun = TRUE)
	
	# Verify xmu_is_wls detects it
	expect_true(xmu_is_wls(fit1))
	
	# Verify umxSummary output prints the custom robust WLS note
	expect_message(umxSummary(fit1), "Applying robust WLS fit metrics")
	
	# Setup comparison model
	m2 <- "
	T1 ~~ T1
	T2 ~~ T2
	T1 ~~ 0*T2
	"
	fit2 <- umxGSEM(m2, S = S, V = V, estimation = "DWLS", autoRun = TRUE)
	
	# Verify umxSummary on fit2 (which has high RMSEA) prints the custom robust WLS note and does NOT print "worse than desired"
	expect_message(umxSummary(fit2), "Applying robust WLS fit metrics")
	withCallingHandlers(
		umxSummary(fit2),
		message = function(m) {
			if (grepl("worse than desired", m$message)) {
				fail("Should not print conventional worse than desired warning for WLS model")
			}
		}
	)
	
	# Verify umxCompare prints the custom note about discrepancy fit units and cutoffs
	compare_output <- capture.output(umxCompare(fit1, fit2))
	
	# Verify that WLS/GSEM specific note is in the output, and not the default -2LL note
	expect_true(any(grepl("Change in degrees of freedom", compare_output)))
	expect_true(any(grepl("For GSEM models, evaluate absolute fit using SRMR", compare_output)))
	expect_false(any(grepl("change in -2 \u00D7 Log-Likelihood", compare_output)))
})

