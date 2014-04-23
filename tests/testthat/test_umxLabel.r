# library(testthat); library(devtools)
# test_file("~/bin/umx/inst/tests/test_umxLabel.r")
# 
# test_package("umx")
test_that("umxLabel works for different inputs", {	
	require(umx)
	require(OpenMx)
	# ===============================
	# = Test on each type of matrix =
	# ===============================
	allTypes = c("Diag", "Full", "Iden", "Lower", "Stand", "Sdiag", "Symm", "Unit", "Zero")

	# 1. Test Diag
		obj = mxMatrix(name = "a", type = "Diag", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
		 "a_r1c1", NA    , NA,
		  NA   , "a_r2c2", NA,
		  NA   , NA    , "a_r3c3"))
		testthat::expect_identical(obj, res)

	# 2. Test Full
		obj = mxMatrix(name = "a", type = "Full", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    "a_r1c1", "a_r1c2", "a_r1c3",
	    "a_r2c1", "a_r2c2", "a_r2c3",
	    "a_r3c1", "a_r3c2", "a_r3c3"))

		testthat::expect_identical(obj, res)

	# 3. Test Iden
		obj = mxMatrix(name = "a", type = "Iden", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		obj
		res =  matrix(nrow = 3, byrow = T, data = c(
	    NA, NA, NA,
	    NA, NA, NA,
	    NA, NA, NA))

		testthat::expect_identical(obj, res)

	# 4. Test Lower Stand Sdiag Symm Unit Zero
		obj = mxMatrix(name = "a", type = "Lower", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		obj
		res =  matrix(nrow = 3, byrow = T, data = c(
	    "a_r1c1",       NA,       NA,
	    "a_r2c1", "a_r2c2",       NA,
	    "a_r3c1", "a_r3c2", "a_r3c3"))

		testthat::expect_identical(obj, res)

	# 5. Test Stand Sdiag Symm Unit Zero
		obj = mxMatrix(name = "a", type = "Stand", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    NA      , "a_r2c1", "a_r3c1",
	    "a_r2c1", NA      , "a_r3c2",
	    "a_r3c1", "a_r3c2",       NA))

		testthat::expect_identical(obj, res)

	# 6. Test Sdiag
		obj = mxMatrix(name = "a", type = "Sdiag", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    NA      , NA      , NA,
	    "a_r2c1", NA      , NA,
	    "a_r3c1", "a_r3c2", NA))

		testthat::expect_identical(obj, res)

	# 7. Test Symm
		obj = mxMatrix(name = "a", type = "Symm", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    "a_r1c1", "a_r2c1", "a_r3c1",
	    "a_r2c1", "a_r2c2", "a_r3c2",
	    "a_r3c1", "a_r3c2", "a_r3c3"))

		testthat::expect_identical(obj, res)

	# 7. Test Unit
		obj = mxMatrix(name = "a", type = "Unit", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    NA, NA, NA,
	    NA, NA, NA,
	    NA, NA, NA))
		testthat::expect_identical(obj, res)

	# 7. Test Zero
		obj = mxMatrix(name = "a", type = "Zero", nrow = 3, ncol = 3); obj = umxLabel(obj); obj = obj$labels
		res =  matrix(nrow = 3, byrow = T, data = c(
	    NA, NA, NA,
	    NA, NA, NA,
	    NA, NA, NA))
		testthat::expect_identical(obj, res)

	# Test RAM labeling
	data(demoOneFactor)
	latents  = c("G"); manifests = names(demoOneFactor)
	m1 <- mxModel("m1", type = "RAM", 
		manifestVars = manifests, latentVars = latents, 
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2),
		mxPath(from = latents, arrows = 2, free = F, values = 1.0),
		mxData(cov(demoOneFactor), type = "cov", numObs = 500)
	)
	m1 = umxLabel(m1)
	names_c <- c("x1", "x2", "x3", "x4", "x5", "G")
	expected_A_labels <- matrix(nrow=6, byrow = TRUE, data = c(
		"x1_to_x1", "x2_to_x1", "x3_to_x1", "x4_to_x1", "x5_to_x1", "G_to_x1", 
		"x1_to_x2", "x2_to_x2", "x3_to_x2", "x4_to_x2", "x5_to_x2", "G_to_x2", 
		"x1_to_x3", "x2_to_x3", "x3_to_x3", "x4_to_x3", "x5_to_x3", "G_to_x3", 
		"x1_to_x4", "x2_to_x4", "x3_to_x4", "x4_to_x4", "x5_to_x4", "G_to_x4", 
		"x1_to_x5", "x2_to_x5", "x3_to_x5", "x4_to_x5", "x5_to_x5", "G_to_x5", 
		"x1_to_G" , "x2_to_G" , "x3_to_G" , "x4_to_G" , "x5_to_G" , "G_to_G")
	)

	expected_S_labels <- matrix(nrow=6, byrow = TRUE, data = c(
		"x1_with_x1", "x1_with_x2", "x1_with_x3", "x1_with_x4", "x1_with_x5", "G_with_x1", 
		"x1_with_x2", "x2_with_x2", "x2_with_x3", "x2_with_x4", "x2_with_x5", "G_with_x2", 
		"x1_with_x3", "x2_with_x3", "x3_with_x3", "x3_with_x4", "x3_with_x5", "G_with_x3", 
		"x1_with_x4", "x2_with_x4", "x3_with_x4", "x4_with_x4", "x4_with_x5", "G_with_x4", 
		"x1_with_x5", "x2_with_x5", "x3_with_x5", "x4_with_x5", "x5_with_x5", "G_with_x5", 
		"G_with_x1" , "G_with_x2" , "G_with_x3" , "G_with_x4" , "G_with_x5" , "G_with_G")
	)
	rownames(expected_A_labels) <- names_c
	colnames(expected_A_labels) <- names_c
	rownames(expected_S_labels) <- names_c
	colnames(expected_S_labels) <- names_c

	testthat::expect_equal(m1@matrices$A@labels, expected_A_labels)
	testthat::expect_equal(m1@matrices$S@labels, expected_S_labels)


	# ======================================================
	# = Check we are rejecting things we don't understand	 =
	# ======================================================
	testthat::expect_error(umxLabel(1), regexp = "I can only label OpenMx models and mxMatrix types. You gave me a double")	

})
