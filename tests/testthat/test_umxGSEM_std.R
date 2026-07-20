test_that("umxGSEM_std works correctly with Delta-method scaling", {
	require(umx)
	
	# 1. Set up a 2-trait covariance structure
	# S matrix
	sMat = matrix(c(4.0, 1.5, 1.5, 9.0), nrow = 2, ncol = 2)
	colnames(sMat) = c("X1", "X2")
	rownames(sMat) = c("X1", "X2")
	
	# V matrix (vech order: var_X1, poly_X2_X1, var_X2)
	vMat = diag(c(0.2, 0.3, 0.4))
	namesV = c("var_X1", "poly_X2_X1", "var_X2")
	colnames(vMat) = namesV
	rownames(vMat) = namesV
	
	# I matrix
	iMat = matrix(c(1.0, 0.5, 0.5, 1.2), nrow = 2, ncol = 2)
	colnames(iMat) = c("X1", "X2")
	rownames(iMat) = c("X1", "X2")
	
	covStruc = list(S = sMat, V = vMat, I = iMat)
	
	# 2. Run standardization
	stdCov = umxGSEM_std(covStruc)
	
	# 3. Verify S (correlation matrix)
	expect_equal(stdCov$S[1, 1], 1.0)
	expect_equal(stdCov$S[2, 2], 1.0)
	expect_equal(stdCov$S[1, 2], 0.25)
	
	# 4. Verify I (standardized intercepts)
	# I_11 = 1.0 / S_11 = 1.0 / 4.0 = 0.25
	# I_22 = 1.2 / S_22 = 1.2 / 9.0 = 0.1333333
	# I_12 = 0.5 / sqrt(S_11 * S_22) = 0.5 / 6.0 = 0.08333333
	expect_equal(stdCov$I[1, 1], 0.25)
	expect_equal(stdCov$I[2, 2], 1.2 / 9.0)
	expect_equal(stdCov$I[1, 2], 0.5 / 6.0)
	
	# 5. Verify V_R diagonal variances (variances of constant 1.0 are 0)
	expect_equal(stdCov$V["var_X1", "var_X1"], 0.0)
	expect_equal(stdCov$V["var_X2", "var_X2"], 0.0)
	
	# 6. Verify off-diagonal correlation sampling variance via hand-calculated Delta method:
	# expected_var = (-0.03125)^2 * 0.2 + (1/6.0)^2 * 0.3 + (-0.25 / 18)^2 * 0.4
	expectedVal = ((-0.03125)^2 * 0.2) + ((1 / 6.0)^2 * 0.3) + (((-0.25 / 18)^2) * 0.4)
	expect_equal(stdCov$V["poly_X2_X1", "poly_X2_X1"], expectedVal, tolerance = 1e-6)
})

test_that("umxGSEM commonfactor model standardized loading parity test", {
	skip_on_cran()
	require(umx)
	data(Psych_LDSC, package = "umx")
	psychLDSCLabeled = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	
	# Subset to 3 traits for a just-identified model (df = 0) where fit is perfect
	traits = c("SCZ", "BIP", "MDD")
	psychLDSCSub = umx:::xmu_gsem_subset_covstruc(psychLDSCLabeled, traits)
	
	# 1. Fit unstandardized GSEM model
	m1 = umxGSEM("g =~ SCZ + BIP + MDD", covstruc = psychLDSCSub, estimation = "DWLS", name = "m1")
	
	# 2. Standardize S/V and fit standardized GSEM model
	psychLDSCStd = umxGSEM_std(psychLDSCSub)
	m2 = umxGSEM("g =~ SCZ + BIP + MDD", covstruc = psychLDSCStd, estimation = "DWLS", name = "m2")
	
	# 3. Retrieve estimates
	c1 = coef(m1)
	c2 = coef(m2)
	sdS = sqrt(diag(psychLDSCSub$S))
	
	# Standardized loading from unstandardized model should match the raw loading from the standardized model
	expect_equal(
		abs(c2[c("p1_", "p2_", "p3_")]),
		abs(c1[c("p1_", "p2_", "p3_")]) / sdS[c("SCZ", "BIP", "MDD")],
		tolerance = 1e-5
	)
})

