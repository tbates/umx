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
	expect_equal(fit$data$type, "acov")
	expect_equal(fit$data$observed, S)
	expect_equal(fit$data$acov, V)
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
	expect_equal(fit_subset$data$type, "acov")
	expect_equal(fit_subset$data$observed, S) # should match 2x2 S of T1 and T2
	expect_equal(fit_subset$data$acov, V) # should match 3x3 V of T1 and T2 pairs
	
	# Test tmx_show on genomic SEM matrices
	tmx_show(fit, matrices = "data.S", report = "markdown")
	tmx_show(fit, matrices = "data.V", report = "markdown")
	tmx_show(fit, matrices = "V", report = "markdown")
	tmx_show(fit, report = "markdown")
})

test_that("umxRAM natively subsets and reorders acov type mxData", {
	# Setup traits and covariance/weight matrices
	traits3 <- c("T1", "T2", "T3")
	S3 <- matrix(c(0.50, 0.20, 0.10,
	               0.20, 0.60, 0.15,
	               0.10, 0.15, 0.70), nrow = 3, ncol = 3, dimnames = list(traits3, traits3))
	V3 <- diag(c(0.01, 0.005, 0.002, 0.012, 0.004, 0.015))
	vech_names3 <- c("T1 T1", "T2 T1", "T3 T1", "T2 T2", "T3 T2", "T3 T3")
	dimnames(V3) <- list(vech_names3, vech_names3)
	
	# Pack into mxData of type acov
	acov_data <- mxData(observed = S3, type = "acov", acov = V3, numObs = 100)
	
	# Model specification for a bivariate model of T2 and T1 (note reverse order)
	model_str <- "
	T2 ~~ T2
	T1 ~~ T1
	T2 ~~ T1
	"
	
	# Call umxRAM directly passing the full acov_data
	fit <- umxRAM(model_str, data = acov_data, type = "DWLS", autoRun = FALSE)
	
	# Check that fit has only T1 and T2 manifestVars (alphabetical ordering)
	expect_equal(fit$manifestVars, c("T1", "T2"))
	
	# Check that data was subsetted and reordered correctly:
	# 1. Observed covariance matrix should be 2x2 and match T1 and T2
	expected_S <- matrix(c(0.50, 0.20, 0.20, 0.60), nrow = 2, ncol = 2, dimnames = list(c("T1", "T2"), c("T1", "T2")))
	expect_equal(fit$data$observed, expected_S)
	
	# 2. Asymptotic covariance matrix should be 3x3 with row/col names:
	# "T1 T1", "T2 T1", "T2 T2"
	expected_vech <- c("T1 T1", "T2 T1", "T2 T2")
	expect_equal(rownames(fit$data$acov), expected_vech)
	expect_equal(colnames(fit$data$acov), expected_vech)
	# Check values are correct subset of V3 (T1 T1 = 0.01, T2 T1 = 0.005, T2 T2 = 0.012)
	expected_V <- diag(c(0.01, 0.005, 0.012))
	dimnames(expected_V) <- list(expected_vech, expected_vech)
	expect_equal(fit$data$acov, expected_V)
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
	expect_message(umxSummary(fit1), "Applying SB-2010 robust metrics")
	
	# Setup comparison model
	m2 <- "
	T1 ~~ T1
	T2 ~~ T2
	T1 ~~ 0*T2
	"
	fit2 <- umxGSEM(m2, S = S, V = V, estimation = "DWLS", autoRun = TRUE)
	
	# Verify umxSummary on fit2 (which has high RMSEA) prints the custom robust WLS note and does NOT print "worse than desired"
	expect_message(umxSummary(fit2), "Applying SB-2010 robust metrics")
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

