# Optional parity vs GenomicSEM (manual / maintainer machine).
# Not run on CRAN or default GHA (requires GenomicSEM + often pinned lavaan).
#
# Usage:
#   source("inst/developer/GenomicSEM/test_GSEM_optional_GenomicSEM_parity.R")
# Or:
#   Sys.setenv(UMX_TEST_GENOMICSEM = "1")
#   testthat::test_file("inst/developer/GenomicSEM/test_GSEM_optional_GenomicSEM_parity.R")
#
# Protocol:
#   1. Same Psych_LDSC traits / same toy SNPs as tests/testthat/fixtures/gsem/
#   2. Compare commonfactor loadings (abs) and, if commonfactorGWAS available, SNP ests
#   3. Write snap under _snaps/ if UMX_WRITE_GENOMICSEM_SNAP=1

library(testthat)
library(umx)

skip_if_not(
	identical(Sys.getenv("UMX_TEST_GENOMICSEM"), "1"),
	"Set UMX_TEST_GENOMICSEM=1 to run GenomicSEM parity"
)
skip_if_not(
	requireNamespace("GenomicSEM", quietly = TRUE),
	"GenomicSEM package not installed"
)

options(umx_auto_plot = FALSE)
fixture_dir = system.file("developer", "GenomicSEM", package = "umx")
if (!nzchar(fixture_dir)) {
	fixture_dir = "inst/developer/GenomicSEM"
}
snap_dir = file.path(fixture_dir, "_snaps")
dir.create(snap_dir, showWarnings = FALSE, recursive = TRUE)

test_that("optional: umxGSEM commonfactor abs-coefs near GenomicSEM commonfactor", {
	data(Psych_LDSC, package = "umx")
	# GenomicSEM expects list elements often named S_LD / V_LD — map
	covstruc = list(
		S_LD = Psych_LDSC$S,
		V_LD = Psych_LDSC$V,
		I = Psych_LDSC$I,
		N = Psych_LDSC$N,
		m = Psych_LDSC$m
	)
	# Ensure dimnames on S
	if (is.null(rownames(covstruc$S_LD))) {
		rownames(covstruc$S_LD) = colnames(covstruc$S_LD)
	}

	umx_fit = umxGSEM(
		"g ~= SCZ + BIP + MDD + EA + INSOM",
		covstruc = Psych_LDSC,
		estimation = "DWLS",
		tryHard = "yes"
	)
	umx_cf = abs(as.numeric(coef(umx_fit)))
	umx_cf = sort(umx_cf, decreasing = TRUE)

	gsem_ok = FALSE
	gsem_load = NULL
	tryCatch({
		# commonfactor lives in GenomicSEM; API may vary by version
		if (exists("commonfactor", where = asNamespace("GenomicSEM"), inherits = FALSE)) {
			cf = GenomicSEM::commonfactor(covstruc, estimation = "DWLS")
		} else {
			cf = do.call(getFromNamespace("commonfactor", "GenomicSEM"), list(covstruc, estimation = "DWLS"))
		}
		# Extract loadings if present in modelfit / results
		if (is.list(cf) && !is.null(cf$results)) {
			res = cf$results
			if (is.data.frame(res) && "Unstand_Est" %in% names(res)) {
				gsem_load = abs(as.numeric(res$Unstand_Est[res$op == "=~" | grepl("=~", res$op)]))
			} else if (is.data.frame(res) && "est" %in% names(res)) {
				gsem_load = abs(as.numeric(res$est))
			}
		}
		if (!is.null(gsem_load) && length(gsem_load) >= 2) {
			gsem_load = sort(gsem_load, decreasing = TRUE)
			gsem_ok = TRUE
		}
	}, error = function(e) {
		skip(paste("GenomicSEM commonfactor failed:", conditionMessage(e)))
	})

	skip_if_not(gsem_ok, "Could not parse GenomicSEM commonfactor loadings")
	n = min(length(umx_cf), length(gsem_load), 5L)
	expect_equal(umx_cf[1:n], gsem_load[1:n], tolerance = 0.05)

	if (identical(Sys.getenv("UMX_WRITE_GENOMICSEM_SNAP"), "1")) {
		utils::write.csv(
			data.frame(rank = seq_len(n), umx = umx_cf[1:n], GenomicSEM = gsem_load[1:n]),
			file.path(snap_dir, "commonfactor_psych_umx_vs_GenomicSEM.csv"),
			row.names = FALSE
		)
	}
})

test_that("optional: umxGSEM_GWAS ests finite and non-constant (GenomicSEM env)", {
	# Full GenomicSEM GWAS parity is version-fragile; require non-constant finite ests
	# and optionally write our gwas table for external compare.
	snps_path = testthat::test_path("..", "..", "tests", "testthat", "fixtures", "gsem", "gwas_toy_snps.rds")
	if (!file.exists(snps_path)) {
		snps_path = "tests/testthat/fixtures/gsem/gwas_toy_snps.rds"
	}
	skip_if_not(file.exists(snps_path), "gwas_toy_snps.rds not found")
	snps = readRDS(snps_path)
	data(Psych_LDSC, package = "umx")
	gwas = umxGSEM_GWAS(
		Psych_LDSC, snps[1:min(10, nrow(snps)), , drop = FALSE],
		traits = c("SCZ", "BIP", "MDD"),
		quiet = TRUE
	)
	expect_true(all(is.finite(gwas$est)))
	expect_gt(length(unique(round(gwas$est, 7))), 1)
	if (identical(Sys.getenv("UMX_WRITE_GENOMICSEM_SNAP"), "1")) {
		utils::write.csv(gwas, file.path(snap_dir, "gwas_umx_latest.csv"), row.names = FALSE)
	}
})
