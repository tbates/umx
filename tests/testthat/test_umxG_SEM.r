library(testthat)
library(umx)

context("umxG_SEM tests")

test_that("umxG_SEM fits simple bivariate model and subsets correctly", {
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
	fit = umxG_SEM(m1, S = S, V = V, estimation = "DWLS", autoRun = TRUE)
	
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
	fit_subset <- umxG_SEM(model = m1, S = S3, V = V3, estimation = "DWLS", autoRun = TRUE)
	
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
