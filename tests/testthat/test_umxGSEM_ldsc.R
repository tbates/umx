# umxGSEM_ldsc: imxLDSC wrap, OpenMx V names, umxGSEM can consume the covstruc.

library(testthat)
library(umx)

skip_if(is.null(get0("imxLDSC", envir = asNamespace("OpenMx"), inherits = FALSE)), "imxLDSC not in this OpenMx")
skip_if_not(xmu_has_summary_mxData(), "OpenMx fork with type='summary' support is required")

test_that("umxGSEM_ldsc labels V and umxGSEM accepts the covstruc", {
	testDir = file.path(tempdir(), paste0("umx_ldsc_", as.integer(Sys.time())))
	on.exit(unlink(testDir, recursive = TRUE), add = TRUE)
	dir.create(testDir)
	oldWd = getwd()
	on.exit(setwd(oldWd), add = TRUE)
	setwd(testDir)

	nSnp = 80L
	snpIds = paste0("rs", seq_len(nSnp))
	a1Ref = rep(c("A", "C"), length.out = nSnp)
	a2Ref = rep(c("G", "T"), length.out = nSnp)
	refFile = file.path(testDir, "w_hm3.snplist")
	write.table(data.frame(SNP = snpIds, A1 = a1Ref, A2 = a2Ref, stringsAsFactors = FALSE),
		refFile, row.names = FALSE, col.names = TRUE, sep = " ", quote = FALSE, eol = "\n")

	g1 = file.path(testDir, "t1.txt")
	write.table(data.frame(SNP = snpIds, A1 = a1Ref, A2 = a2Ref, P = rep(0.05, nSnp),
		effect = rep(0.1, nSnp), N = rep(1000, nSnp), INFO = 0.95, MAF = 0.2, stringsAsFactors = FALSE),
		g1, row.names = FALSE, col.names = TRUE, sep = " ", quote = FALSE, eol = "\n")
	g2 = file.path(testDir, "t2.txt")
	write.table(data.frame(SNP = snpIds, A1 = a1Ref, A2 = a2Ref, P = rep(0.10, nSnp),
		effect = rep(0.05, nSnp), N = rep(1200, nSnp), INFO = 0.98, MAF = 0.25, stringsAsFactors = FALSE),
		g2, row.names = FALSE, col.names = TRUE, sep = " ", quote = FALSE, eol = "\n")

	ss = umxGSEM_munge(files = c(g1, g2), hm3 = refFile, Ns = c(1000, 1200),
		trait.names = c("T1", "T2"), output_dir = testDir, overwrite = TRUE)
	expect_true(all(file.exists(ss)))

	ldDir = file.path(testDir, "ld")
	dir.create(ldDir)
	writeLines("100", file.path(ldDir, "1.l2.M_5_50"))
	set.seed(42)
	con = gzfile(file.path(ldDir, "1.l2.ldscore.gz"), "w")
	write.table(data.frame(CHR = 1, SNP = snpIds, BP = seq_len(nSnp), L2 = runif(nSnp, 1, 2),
		stringsAsFactors = FALSE), con, row.names = FALSE, col.names = TRUE, sep = "\t", quote = FALSE)
	close(con)

	covstruc = umxGSEM_ldsc(
		traits = ss,
		sample.prev = c(NA, NA),
		population.prev = c(NA, NA),
		ld = ldDir,
		wld = ldDir,
		trait.names = c("T1", "T2"),
		n.blocks = 5,
		stand = TRUE,
		select = 1
	)
	expect_true(all(c("S", "V", "I", "N", "m") %in% names(covstruc)))
	expect_equal(colnames(covstruc$S), c("T1", "T2"))
	expect_equal(colnames(covstruc$V), umx:::xmu_gsem_vech_names(c("T1", "T2")))
	expect_equal(dim(covstruc$V), c(3L, 3L))
	expect_true(all(is.finite(covstruc$S)))
	expect_true(all(diag(covstruc$V) >= 0))
	expect_silent(umx:::xmu_gsem_check_format(covstruc))

	m = umxGSEM("F1 ~= T1 + T2", covstruc = covstruc, estimation = "DWLS",
		autoRun = FALSE, tryHard = "no", name = "ldsc_cf")
	expect_s4_class(m, "MxModelGSEM")
	expect_setequal(m$manifestVars, c("T1", "T2"))
	expect_setequal(colnames(m$data$observedStats$cov), c("T1", "T2"))
	expect_true(!is.null(m$data$observedStats$useWeight))
	expect_true(!is.null(m$data$observedStats$asymCov))
})
