# Smoke tests: umxGSEM MxModel input, sumstats, GWAS on workshop SNP subsets
library(testthat)
library(umx)

dir_gsem = system.file("developer", "GenomicSEM", package = "umx")
if (!nzchar(dir_gsem) || !file.exists(file.path(dir_gsem, "SCZ_subset.txt"))) {
	dir_gsem = "inst/developer/GenomicSEM"
}
skip_if_not(file.exists(file.path(dir_gsem, "SCZ_subset.txt")), "SNP subset files not installed")

test_that("umxGSEM accepts an existing umxRAM model", {
	data(Psych_LDSC, package = "umx")
	S = as.matrix(Psych_LDSC$S)
	rownames(S) = colnames(S)
	traits = c("SCZ", "BIP", "MDD")
	S3 = S[traits, traits]
	V = as.matrix(Psych_LDSC$V)
	colnames(V) = rownames(V) = xmu_gsem_vech_names(colnames(S))
	V3 = V[xmu_gsem_vech_names(traits), xmu_gsem_vech_names(traits)]

	m0 = umxRAM("tmp", data = mxData(S3, type = "cov", numObs = 2), type = "cov",
		autoRun = FALSE, std.lv = TRUE,
		umxPath("g", to = traits),
		umxPath(var = traits),
		umxPath(var = "g", fixedAt = 1)
	)
	m1 = umxGSEM(m0, S = S3, V = V3, estimation = "DWLS", tryHard = "no", autoRun = TRUE)
	expect_s4_class(m1, "MxModelGSEM")
	expect_true(any(grepl("SCZ", names(coef(m1))) | grepl("g_to_SCZ|p1_", names(coef(m1)))))
})

test_that("umxGSEM_sumstats merges tutorial subsets", {
	files = file.path(dir_gsem, c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt"))
	ref = file.path(dir_gsem, "reference.1000G.subset.txt")
	snps = umxGSEM_sumstats(files, ref, trait.names = c("SCZ", "BIP", "MDD"), se.logit = TRUE)
	expect_true(nrow(snps) > 10)
	expect_true(all(c("SNP", "MAF", "beta.SCZ", "se.SCZ", "beta.BIP", "se.MDD") %in% colnames(snps)))
	expect_true(all(is.finite(snps$beta.SCZ)))
	expect_true(all(snps$se.SCZ > 0))
})

test_that("umxGSEM_GWAS runs on a handful of SNPs", {
	data(Psych_LDSC, package = "umx")
	S = as.matrix(Psych_LDSC$S)
	rownames(S) = colnames(S)
	traits = c("SCZ", "BIP", "MDD")
	S3 = S[traits, traits]
	V = as.matrix(Psych_LDSC$V)
	colnames(V) = rownames(V) = xmu_gsem_vech_names(colnames(S))
	V3 = V[xmu_gsem_vech_names(traits), xmu_gsem_vech_names(traits)]
	I3 = as.matrix(Psych_LDSC$I)
	dimnames(I3) = list(colnames(S), colnames(S))
	I3 = I3[traits, traits]

	files = file.path(dir_gsem, c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt"))
	ref = file.path(dir_gsem, "reference.1000G.subset.txt")
	snps = umxGSEM_sumstats(files, ref, trait.names = traits, se.logit = TRUE)

	gwas = umxGSEM_GWAS(
		covstruc = list(S = S3, V = V3, I = I3),
		SNPs = snps,
		maxSNPs = 5,
		quiet = TRUE
	)
	expect_equal(nrow(gwas), 5)
	expect_true(all(c("SNP", "est", "se", "Z", "P", "fail") %in% colnames(gwas)))
	expect_true(all(is.finite(gwas$est)))
	expect_false(any(gwas$fail))
})
