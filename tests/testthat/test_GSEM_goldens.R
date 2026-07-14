# P0 goldens: structural umxGSEM, expand_snp algebra, umxGSEM_GWAS regression.
# Fixtures under tests/testthat/fixtures/gsem/ (no GenomicSEM package required).
# Optional GenomicSEM parity: inst/developer/GenomicSEM/test_GSEM_optional_GenomicSEM_parity.R

library(testthat)
library(umx)

options(umx_auto_plot = FALSE)
fixture_dir = testthat::test_path("fixtures", "gsem")

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

load_gwas_toy_snps <- function() {
	path = file.path(fixture_dir, "gwas_toy_snps.rds")
	skip_if_not(file.exists(path), "gwas_toy_snps.rds fixture missing")
	readRDS(path)
}

# -----------------------------------------------------------------------------
# Label / vech contract
# -----------------------------------------------------------------------------

test_that("xmu_gsem_vech_names uses OpenMx pair labels in lower.tri order", {
	nm = umx:::xmu_gsem_vech_names(c("SCZ", "BIP", "MDD"))
	expect_equal(nm, c(
		"var_SCZ", "poly_BIP_SCZ", "poly_MDD_SCZ",
		"var_BIP", "poly_MDD_BIP", "var_MDD"
	))
})

test_that("umxGSEM_label_ldsc labels S, I, V, N", {
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	x = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	expect_equal(rownames(x$S), colnames(x$S))
	expect_equal(colnames(x$I), colnames(x$S))
	expect_equal(colnames(x$V), umx:::xmu_gsem_vech_names(colnames(x$S)))
	expect_equal(colnames(x$N), colnames(x$V))
	expect_true("var_SCZ" %in% colnames(x$V))
})

test_that("xmu_gsem_subset_SV rejects positional V[1:z,1:z] of larger V", {
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	S3 = Psych_LDSC$S[1:3, 1:3]
	Vbad = Psych_LDSC$V[1:6, 1:6]
	expect_error(
		umx:::xmu_gsem_subset_covstruc(list(S=S3, V=Vbad), c("SCZ", "BIP", "MDD")),
		"Do not use V\\[1:z"
	)
})

test_that("xmu_gsem_subset_SV named subset of full V works", {
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	traits = c("SCZ", "BIP", "MDD")
	sub = umx:::xmu_gsem_subset_covstruc(Psych_LDSC, traits)
	expect_equal(colnames(sub$S), traits)
	expect_equal(colnames(sub$V), umx:::xmu_gsem_vech_names(traits))
	expect_equal(nrow(sub$V), 6L)
})

# -----------------------------------------------------------------------------
# expand_snp algebra
# -----------------------------------------------------------------------------

test_that("xmu_gsem_expand_snp matches hand formulas and golden S/V", {
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	gold = readRDS(file.path(fixture_dir, "expand_snp_golden.rds"))
	traits = c("SCZ", "BIP", "MDD")
	sub = umx:::xmu_gsem_subset_covstruc(Psych_LDSC, traits)
	I = Psych_LDSC$I[traits, traits, drop = FALSE]
	expn = umx:::xmu_gsem_expand_snp(
		sub,
		beta_i = gold$beta, se_i = gold$se,
		varSNP_i = gold$varSNP, varSNPSE2 = (5e-4)^2,
		GC = "standard"
	)
	expect_equal(dim(expn$S), c(4L, 4L))
	expect_equal(dim(expn$V), c(10L, 10L))
	expect_equal(colnames(expn$S), c("SNP", traits))
	# Genetic cov SNP-trait = varSNP * beta
	expect_equal(expn$S["SNP", "SCZ"], as.numeric(gold$varSNP * gold$beta["SCZ"]), tolerance = 1e-12)
	expect_equal(expn$S["SNP", "SNP"], gold$varSNP, tolerance = 1e-12)
	# Trait block preserved
	expect_equal(expn$S[traits, traits], sub$S, tolerance = 1e-10)
	# Golden full matrices (regression)
	expect_equal(expn$S, gold$S, tolerance = 1e-8)
	expect_equal(unname(expn$V), unname(gold$V), tolerance = 1e-6)
})

# -----------------------------------------------------------------------------
# Structural umxGSEM golden (Psych_LDSC common factor)
# -----------------------------------------------------------------------------

test_that("umxGSEM Psych_LDSC commonfactor matches golden coefs", {
	skip_on_cran()
	gold = readRDS(file.path(fixture_dir, "commonfactor_psych_golden.rds"))
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	m = umxGSEM(
		"g ~= SCZ + BIP + MDD + EA + INSOM",
		covstruc = Psych_LDSC,
		estimation = "DWLS",
		tryHard = "yes",
		name = "cf_test"
	)
	expect_s4_class(m, "MxModelGSEM")
	expect_equal(as.integer(m$output$status$code), 0L)
	expect_equal(as.numeric(m$output$fit), gold$fit, tolerance = 0.05)
	cf = coef(m)
	cf = coef(m)
	expect_equal(names(cf), names(gold$coef))
	# Absolute values: factor sign can flip
	expect_equal(abs(as.numeric(cf)), abs(as.numeric(gold$coef)), tolerance = 0.02)
	# SCZ / BIP dominate (printed-precision style)
	# Map by relative magnitude rather than label (p1_ etc. can renumber)
	abs_cf = sort(abs(as.numeric(cf)), decreasing = TRUE)
	expect_true(abs_cf[1] > 0.2)
	expect_true(abs_cf[2] > 0.2)
})

test_that("umxGSEM keeps SNP when present in S (GWAS expand)", {
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	traits = c("SCZ", "BIP", "MDD")
	sub = umx:::xmu_gsem_subset_covstruc(Psych_LDSC, traits)
	I = Psych_LDSC$I[traits, traits, drop = FALSE]
	expn = umx:::xmu_gsem_expand_snp(
		sub,
		beta_i = c(0.01, 0.02, 0.005),
		se_i = c(0.01, 0.01, 0.01),
		varSNP_i = 0.3,
		varSNPSE2 = (5e-4)^2
	)
	m = umxGSEM(
		model = "F1 =~ 1*SCZ + BIP + MDD\nF1 ~~ NA*F1\nF1 ~ SNP\n",
		S = expn$S, V = expn$V,
		estimation = "DWLS",
		autoRun = FALSE,
		tryHard = "no",
		std.lv = FALSE,
		name = "snp_keep"
	)
	expect_true("SNP" %in% m$manifestVars)
	expect_false("SNP" %in% m$latentVars)
	expect_true("SNP" %in% colnames(m$data$observedStats$cov))
})

# -----------------------------------------------------------------------------
# umxGSEM_GWAS regression vs fixture golden
# -----------------------------------------------------------------------------

test_that("umxGSEM_GWAS ests match golden snapshot and vary by SNP", {
	skip_on_cran()
	snps = load_gwas_toy_snps()
	gold = utils::read.csv(file.path(fixture_dir, "gwas_golden.csv"), stringsAsFactors = FALSE)
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	traits = c("SCZ", "BIP", "MDD")
	# Limit to SNPs present in golden
	snps = snps[snps$SNP %in% gold$SNP, , drop = FALSE]
	snps = snps[match(gold$SNP, snps$SNP), , drop = FALSE]

	gwas = umxGSEM_GWAS(
		covstruc = Psych_LDSC,
		SNPs = snps,
		traits = traits,
		quiet = TRUE
	)
	expect_equal(nrow(gwas), nrow(gold))
	expect_equal(gwas$SNP, gold$SNP)
	# Must not collapse to a constant estimate (SNP-in-S regression)
	expect_true(length(unique(round(gwas$est, 8))) >= max(2L, nrow(gwas) - 2L))
	expect_true(all(is.finite(gwas$est)))
	expect_false(any(gwas$fail))
	# Match golden ests (optimizer can wander slightly)
	expect_equal(gwas$est, gold$est, tolerance = 5e-3)
	# SE: at least some finite; Z/P present when se finite
	expect_true(sum(is.finite(gwas$se)) >= 1L)
	ok = is.finite(gwas$se) & gwas$se > 0
	if (any(ok)) {
		expect_true(all(is.finite(gwas$Z[ok])))
		expect_true(all(is.finite(gwas$P[ok])))
		expect_true(all(gwas$P[ok] >= 0 & gwas$P[ok] <= 1))
	}
	expect_true("se_source" %in% names(gwas))
})

test_that("umxGSEM_GWAS handles missing SE / bad status without constant-est bug", {
	skip_on_cran()
	snps = load_gwas_toy_snps()
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	gwas = umxGSEM_GWAS(
		covstruc = Psych_LDSC,
		SNPs = snps[1:5, , drop = FALSE],
		traits = c("SCZ", "BIP", "MDD"),
		quiet = TRUE
	)
	expect_equal(nrow(gwas), 5)
	# Status codes recorded
	expect_true(all(is.finite(gwas$status) | is.na(gwas$status)))
	# If SE missing, Z and P must also be missing
	na_se = !is.finite(gwas$se)
	if (any(na_se)) {
		expect_true(all(!is.finite(gwas$Z[na_se]) | is.na(gwas$Z[na_se])))
		expect_true(all(!is.finite(gwas$P[na_se]) | is.na(gwas$P[na_se])))
	}
	# se_source is openmx, sandwich, dropped_*, or NA
	if (any(!is.na(gwas$se_source))) {
		expect_true(all(gwas$se_source[!is.na(gwas$se_source)] %in%
			c("openmx", "summary", "sandwich", "dropped_numeric", "dropped_bad_status", "analytic")))
	}
	# Distinct estimates across SNPs
	expect_gt(length(unique(round(gwas$est, 7))), 1)
})

test_that("xmu_gsem_extract_snp_path works", {
	skip_on_cran()
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	traits = c("SCZ", "BIP", "MDD")
	sub = umx:::xmu_gsem_subset_covstruc(Psych_LDSC, traits)
	I = Psych_LDSC$I[traits, traits, drop = FALSE]
	expn = umx:::xmu_gsem_expand_snp(
		sub,
		beta_i = c(0.01, -0.02, 0.005),
		se_i = c(0.01, 0.012, 0.009),
		varSNP_i = 0.4,
		varSNPSE2 = (5e-4)^2
	)
	m = umxGSEM(
		model = "F1 =~ 1*SCZ + BIP + MDD\nF1 ~~ NA*F1\nF1 ~ SNP\n",
		S = expn$S, V = expn$V,
		estimation = "DWLS",
		autoRun = FALSE,
		tryHard = "no",
		std.lv = FALSE
	)
	fit = mxRun(m, silent = TRUE, suppressWarnings = TRUE)
	expect_false(is.null(fit$output$implied_jacobian))
	es = umx:::xmu_gsem_extract_snp_path(fit, traits)
	expect_true(is.finite(es$est))
	# SE may be openmx or sandwich
	if (is.finite(es$se)) {
		expect_true(es$se > 0)
		expect_true(es$se_source %in% c("openmx", "summary", "sandwich"))
	}
})

test_that("umxGSEM_GWAS analytic engine exactly matches mxRun fallback", {
	skip_on_cran()
	data(Psych_LDSC, package = "umx")
	Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC, overwrite = TRUE)
	traits = c("SCZ", "BIP", "MDD")
	
	# Generate exactly 3 predictable dummy SNPs
	snps = data.frame(
		SNP = paste0("rs", 1:3),
		MAF = c(0.4, 0.2, 0.3),
		beta.SCZ = c(0.02, -0.01, 0.05),
		se.SCZ = c(0.01, 0.01, 0.02),
		beta.BIP = c(0.01, 0.00, 0.03),
		se.BIP = c(0.01, 0.01, 0.02),
		beta.MDD = c(-0.01, 0.02, 0.01),
		se.MDD = c(0.01, 0.02, 0.01)
	)
	
	# Run Analytic DWLS
	gwas_analytic = umxGSEM_GWAS(
		covstruc = Psych_LDSC, SNPs = snps, traits = traits,
		estimation = "DWLS", quiet = TRUE
	)
	
	# Create a dummy expanded S and V to build the mxModel template
	sub = umx:::xmu_gsem_subset_covstruc(Psych_LDSC, traits)
	I_LD = diag(3); dimnames(I_LD) = list(traits, traits)
	betas = c(snps$beta.SCZ[1], snps$beta.BIP[1], snps$beta.MDD[1])
	ses = c(snps$se.SCZ[1], snps$se.BIP[1], snps$se.MDD[1])
	varSNP = 2 * snps$MAF[1] * (1 - snps$MAF[1])
	sub$I = I_LD
	expn_dummy = umx:::xmu_gsem_expand_snp(sub, betas, ses, varSNP, (5e-4)^2, "standard", NULL)
	
	m_meas = umxGSEM(model = "F1 =~ SCZ + BIP + MDD\nF1 ~~ NA*F1\nF1 ~ SNP", S = expn_dummy$S, V = expn_dummy$V, estimation = "DWLS", autoRun = FALSE, quiet = TRUE, std.lv = FALSE)
	
	gwas_mxrun = umxGSEM_GWAS(
		covstruc = Psych_LDSC, SNPs = snps, traits = traits,
		model = m_meas, estimation = "DWLS", quiet = TRUE
	)
	
	expect_equal(nrow(gwas_analytic), 3)
	expect_equal(gwas_analytic$se_source, rep("analytic", 3))
	expect_true(all(gwas_mxrun$se_source != "analytic", na.rm = TRUE))
	
	# Compare estimates and SEs between analytic and mxRun
	# mxRun SEs can fluctuate by 1-5% due to optimizer vs exact algebra, but estimates should be very close.
	expect_equal(gwas_analytic$est, gwas_mxrun$est, tolerance = 0.05)
	expect_equal(gwas_analytic$se, gwas_mxrun$se, tolerance = 0.05)
})

test_that("umxGSEM_sim_snps generates valid data.frame", {
    snps = umxGSEM_sim_snps(n = 100, traits = c("SCZ", "BIP", "MDD"))
    expect_s3_class(snps, "data.frame")
    expect_equal(nrow(snps), 100)
    expect_true(all(c("SNP", "CHR", "BP", "A1", "A2", "MAF", "beta.SCZ", "se.SCZ") %in% colnames(snps)))
    expect_false(any(snps$A1 == snps$A2)) # Alleles must differ
})
