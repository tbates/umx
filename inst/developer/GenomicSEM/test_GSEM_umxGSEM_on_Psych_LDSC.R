# "~/bin/umx/tests/testthat/test_umxGSEM_on_Psych_LDSC.R"
library(testthat)
library(umx)

test_that("umxGSEM Psych_LDSC 5-trait common factor (user syntax)", {
	data("Psych_LDSC", package = "umx")
	# umx path-style ~= accepted; unit-variance ID; DWLS
	m1 = umxGSEM(model = "g ~= SCZ + BIP + MDD + EA + INSOM",
	             covstruc = Psych_LDSC, estimation = "DWLS", name = "GSEM_g")
	expect_s4_class(m1, "MxModelGSEM")
	expect_equal(m1$output$status$code, 0)
	# DWLS optimum (OpenMx residual order + correct useWeight): SCZ/BIP dominate
	cf = coef(m1)
	expect_equal(as.numeric(cf[1]), 0.431, tolerance = 0.02) # SCZ loading
	expect_equal(as.numeric(cf[2]), 0.424, tolerance = 0.02) # BIP loading
	expect_equal(as.numeric(m1$output$fit), 407.32, tolerance = 1)
	resSummary = umxSummary(m1)
	expect_true(!is.null(resSummary))
})

test_that("umxGSEM Psych_LDSC workshop path model + compare", {
	data("Psych_LDSC", package = "umx")
	lavStr = "
		F1 =~ NA*SCZ + BIP + MDD
		F1 ~~ 1*F1
		INSOM ~ F1
		EA ~ INSOM_to_EA*INSOM
	"
	m1 = umxGSEM(model = lavStr, covstruc = Psych_LDSC, name = "GSEM_Base", std.lv = FALSE)
	expect_s4_class(m1, "MxModelGSEM")
	expect_equal(m1$output$status$code, 0)
	# EA ~ INSOM should be strongly negative on this genetic cov structure
	expect_true(as.numeric(coef(m1)["INSOM_to_EA"]) < -0.3)

	resSummary = umxSummary(m1)
	expect_true(!is.null(resSummary))

	m2 = umxModify(m1, update = "INSOM_to_EA", name = "GSEM_No_EA")
	expect_s4_class(m2, "MxModelGSEM")

	expect_message({
	  comp = umxCompare(m1, m2)
	}, "Genomic GSEM model detected")

	expect_s3_class(comp, "data.frame")
	expect_equal(comp$delta_df[2], 1)
	expect_false(is.na(comp$diffFit[2]))
	expect_false(is.na(comp$p[2]))
})