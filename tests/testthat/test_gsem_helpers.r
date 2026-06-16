library(testthat)
library(umx)

context("Genomic SEM helper tests")

test_that("umxGSEM_dl_RefList works correctly", {
	# Setup temp directory for tests
	tmp_dir <- file.path(tempdir(), "gsem_ref_list_test")
	on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
	
	# 1. Check directory creation and download failure handling
	# Since tmp_dir does not exist, calling the function should create it,
	# and then fail to download from the invalid URL.
	expect_error(
		umxGSEM_dl_RefList(project_path = tmp_dir, path2snplist = "https://invalid.url/nonexistent_file.snplist")
	)
	expect_true(dir.exists(tmp_dir))
	
	# 2. Check cache mechanism
	# Create a dummy w_hm3.snplist file
	dest_file <- file.path(tmp_dir, "w_hm3.snplist")
	writeLines("dummy snplist content", dest_file)
	
	# Calling it with overwrite = FALSE should hit cache and NOT try to download,
	# so it won't throw an error even with an invalid URL.
	path <- umxGSEM_dl_RefList(project_path = tmp_dir, path2snplist = "https://invalid.url/nonexistent_file.snplist", overwrite = FALSE)
	expect_equal(normalizePath(path), normalizePath(dest_file))
	
	# 3. Check overwrite = TRUE bypasses cache and tries to download (and fails on invalid URL)
	expect_error(
		umxGSEM_dl_RefList(project_path = tmp_dir, path2snplist = "https://invalid.url/nonexistent_file.snplist", overwrite = TRUE)
	)
})

test_that("umxGSEM_munge works correctly", {
	# Setup temp directory for tests
	tmp_dir <- file.path(tempdir(), "gsem_munge_test")
	on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
	
	# 1. Check null files error
	expect_error(umxGSEM_munge(files = NULL), "Provide a character vector of your summary statistics files")
	
	# 2. Setup mock data files
	dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)
	
	# Mock hm3 snplist
	mock_hm3 <- file.path(tmp_dir, "mock_hm3.snplist")
	ref_data <- data.frame(SNP = c("rs1", "rs2"), A1 = c("A", "C"), A2 = c("T", "G"))
	write.table(ref_data, mock_hm3, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
	
	# Mock sumstat file with custom column names that need auto-detection
	mock_sumstats <- file.path(tmp_dir, "mock_sumstats.txt")
	sumstats <- data.frame(
		rsid = c("rs1", "rs2"), 
		A1 = c("A", "C"), 
		A2 = c("T", "G"), 
		N = c(100, 100), 
		pval = c(0.05, 0.01), 
		or = c(1.5, 0.8),
		info = c(0.95, 0.99),
		maf = c(0.12, 0.25)
	)
	write.table(sumstats, mock_sumstats, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t")
	
	# 3. Run munge
	# Ns is passed as a single number (should be replicated)
	# column.names is empty (should trigger auto-detection)
	output_files <- umxGSEM_munge(
		files = mock_sumstats, 
		hm3 = mock_hm3, 
		Ns = 10000, 
		info.filter = 0.9, 
		maf.filter = 0.01, 
		output_dir = tmp_dir, 
		overwrite = TRUE
	)
	
	# Check output file is returned and exists
	expect_equal(length(output_files), 1)
	expect_equal(basename(output_files), "mock_sumstats.sumstats.gz")
	expect_true(file.exists(output_files))
})

test_that("Anthro_LDSC dataset loads and has correct structure", {
	data("Anthro_LDSC", package = "umx")
	expect_true(exists("Anthro_LDSC"))
	expect_type(Anthro_LDSC, "list")
	expect_equal(names(Anthro_LDSC), c("V", "S", "I", "N", "m"))
	expect_equal(colnames(Anthro_LDSC$S), c("BMI", "WHR", "CO", "Waist", "Hip", "Height", "IHC", "BL", "BW"))
	expect_equal(dim(Anthro_LDSC$V), c(45, 45))
	expect_equal(dim(Anthro_LDSC$S), c(9, 9))
	expect_equal(dim(Anthro_LDSC$I), c(9, 9))
	expect_equal(dim(Anthro_LDSC$N), c(1, 45))
	expect_equal(Anthro_LDSC$m, 1173569)
})

test_that("PSYCH_COV dataset loads and has correct structure", {
	data("PSYCH_COV", package = "umx")
	expect_true(exists("PSYCH_COV"))
	expect_type(PSYCH_COV, "list")
	expect_equal(names(PSYCH_COV), c("V", "S", "I", "N", "m"))
	expect_equal(colnames(PSYCH_COV$S), c("SCZ", "BIP", "MDD", "EA", "INSOM"))
	expect_equal(dim(PSYCH_COV$V), c(15, 15))
	expect_equal(dim(PSYCH_COV$S), c(5, 5))
	expect_equal(dim(PSYCH_COV$I), c(5, 5))
	expect_equal(dim(PSYCH_COV$N), c(1, 15))
})


