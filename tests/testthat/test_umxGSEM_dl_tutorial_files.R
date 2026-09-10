# umxGSEM_dl_tutorial_files: copy shipped subsets; skip GitHub when LD scores exist.

library(testthat)
library(umx)

test_that("umxGSEM_dl_tutorial_files skips download when ld and subsets already present", {
	tmpDir = file.path(tempdir(), paste0("gsem_tut_", as.integer(Sys.time())))
	on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
	ldDir = file.path(tmpDir, "eur_w_ld_chr")
	dir.create(ldDir, recursive = TRUE)
	writeLines("SNP A1 A2", file.path(ldDir, "w_hm3.snplist"))
	writeLines("100", file.path(ldDir, "1.l2.M_5_50"))
	writeBin(raw(8), file.path(ldDir, "1.l2.ldscore.gz"))
	writeLines("SNP\tA1\tA2\tP\tbeta", file.path(tmpDir, "SCZ_subset.txt"))
	writeLines("SNP\tA1\tA2\tP\tbeta", file.path(tmpDir, "BIP_subset.txt"))
	writeLines("SNP\tA1\tA2\tP\tbeta", file.path(tmpDir, "MDD_subset.txt"))
	writeLines("SNP\tCHR\tBP\tMAF\tA1\tA2", file.path(tmpDir, "reference.1000G.subset.txt"))
	badUrl = "https://invalid.example/umxGSEM_tutorial.tar.gz"
	tut = umxGSEM_dl_tutorial_files(path = tmpDir, overwrite = FALSE, url = badUrl)
	expect_type(tut, "list")
	expect_equal(names(tut), c("dir", "hm3", "ld", "sumstats", "ref"))
	expect_equal(normalizePath(tut$dir), normalizePath(tmpDir))
	expect_true(file.exists(tut$hm3))
	expect_equal(basename(tut$sumstats), c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt"))
	expect_true(all(file.exists(tut$sumstats)))
	expect_true(file.exists(tut$ref))
	expect_error(
		umxGSEM_dl_tutorial_files(path = tmpDir, overwrite = TRUE, url = badUrl),
		"Could not download"
	)
})

test_that("umxGSEM_dl_tutorial_files errors on missing pack and bad url", {
	skip_on_cran()
	tmpDir = file.path(tempdir(), paste0("gsem_tut_miss_", as.integer(Sys.time())))
	on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
	expect_error(
		umxGSEM_dl_tutorial_files(path = tmpDir, overwrite = FALSE, url = "https://invalid.example/umxGSEM_tutorial.tar.gz"),
		"Could not download"
	)
	expect_true(dir.exists(tmpDir))
})

test_that("umxGSEM_dl_tutorial_files unpacks a local tarball", {
	localTar = "/tmp/umxGSEM_tutorial.tar.gz"
	skip_if_not(file.exists(localTar), "local tutorial tarball not built")
	tmpDir = file.path(tempdir(), paste0("gsem_tut_tar_", as.integer(Sys.time())))
	on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
	tut = umxGSEM_dl_tutorial_files(path = tmpDir, overwrite = TRUE, url = localTar)
	expect_true(file.exists(tut$hm3))
	expect_true(file.exists(file.path(tut$ld, "1.l2.ldscore.gz")))
	expect_equal(length(tut$sumstats), 3L)
	hdr = readLines(tut$hm3, n = 1L)
	expect_true(grepl("SNP", hdr, fixed = TRUE))
})

test_that("umxGSEM_munge maps headers per file (BIP-style snpid/pval vs SNP/P)", {
	skip_if(is.null(get0("imxMunge", envir = asNamespace("OpenMx"), inherits = FALSE)), "imxMunge not in this OpenMx")
	tmpDir = file.path(tempdir(), paste0("munge_hdr_", as.integer(Sys.time())))
	on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
	dir.create(tmpDir)
	hm3 = file.path(tmpDir, "w_hm3.snplist")
	write.table(data.frame(SNP = c("rs1", "rs2"), A1 = c("A", "C"), A2 = c("G", "T"), stringsAsFactors = FALSE),
		hm3, row.names = FALSE, quote = FALSE, sep = " ", eol = "\n")
	fSnp = file.path(tmpDir, "scz.txt")
	write.table(data.frame(SNP = c("rs1", "rs2"), A1 = c("A", "C"), A2 = c("G", "T"), P = c(0.05, 0.10), beta = c(0.10, -0.20), stringsAsFactors = FALSE),
		fSnp, row.names = FALSE, quote = FALSE, sep = "\t", eol = "\n")
	fBip = file.path(tmpDir, "bip.txt")
	write.table(data.frame(snpid = c("rs1", "rs2"), a1 = c("A", "C"), a2 = c("G", "T"), pval = c(0.05, 0.10), or = c(0.10, -0.20), stringsAsFactors = FALSE),
		fBip, row.names = FALSE, quote = FALSE, sep = "\t", eol = "\n")
	outs = umxGSEM_munge(
		files = c(fSnp, fBip),
		hm3 = hm3,
		Ns = c(1000, 2000),
		trait.names = c("SCZ", "BIP"),
		output_dir = tmpDir,
		overwrite = TRUE
	)
	expect_true(all(file.exists(outs)))
	scz = read.table(gzfile(outs[1]), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	bip = read.table(gzfile(outs[2]), header = TRUE, sep = "\t", stringsAsFactors = FALSE)
	expect_equal(nrow(scz), 2L)
	expect_equal(nrow(bip), 2L)
	expect_equal(colnames(scz), c("SNP", "N", "Z", "A1", "A2"))
	expect_equal(scz$N, c(1000, 1000))
	expect_equal(bip$N, c(2000, 2000))
})

