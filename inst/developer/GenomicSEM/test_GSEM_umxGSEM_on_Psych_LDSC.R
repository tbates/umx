# "~/bin/umx/tests/testthat/test_umxGSEM_on_Psych_LDSC.R"
library(testthat)
library(umx)

test_that("Direct testing of umxGSEM on built-in Psych_LDSC ldsc data", {

	# 1. Load the built-in LDSC data
	data("Psych_LDSC", package = "umx")

	# 2. Define the Base Model in lavaan for now
	lavStr = "
		F1 =~ NA*SCZ + BIP + MDD
		F1 ~~ 1*F1
		INSOM ~ F1
		EA ~ INSOM_to_EA*INSOM
	"
	# 3. Push it through the new GSEM engine
	# This will trigger the triage firewall. Watch the console to see if 
	# Psych_LDSC requires nearPD smoothing (Level 2) or passes clean.
	m1 = umxGSEM(model = lavStr, covstruc = Psych_LDSC, name = "GSEM_Base")
	expect_s4_class(m1, "MxModelGSEM")

	# Check the summary to ensure paths and SEs populated correctly
	resSummary = umxSummary(m1)
	expect_true(!is.null(resSummary))

	# 4. Test the new Comparison Switchboard
	# Drop the EA ~ INSOM path (parameter label "p6_") to create a nested comparison model
	m2 = umxModify(m1, update = "INSOM_to_EA", name = "GSEM_No_EA")
	expect_s4_class(m2, "MxModelGSEM")

	# Run the comparison. This should trigger the "Genomic GSEM model detected" message.
	expect_message({
	  comp = umxCompare(m1, m2)
	}, "Genomic GSEM model detected")

	expect_s3_class(comp, "data.frame")
	expect_equal(dim(comp), c(2, 12))
	expect_equal(comp$delta_df[2], 1)
	expect_false(is.na(comp$diffFit[2]))
	expect_false(is.na(comp$p[2]))
	expect_false(is.na(comp$SRMR[2]))
	expect_false(is.na(comp$delta_SRMR[2]))
	expect_false(is.na(comp$CFI[2]))
	expect_false(is.na(comp$delta_CFI[2]))
	expect_false(is.na(comp$AIC[2]))
	expect_null(comp$Pseudo_BIC)
})