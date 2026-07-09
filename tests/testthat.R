# CRAN: skip the entire suite (historical CRAN time/noise complaints).
# GitHub Actions / other CI: run tests (NOT_CRAN=true and CI=true from r-lib/GHA).
# Local interactive: run tests for development.
#
# GenomicSEM / pinned-lavaan parity checks are NOT in this suite. They live under
# inst/developer/GenomicSEM/ and are manual-only (not CRAN, not GHA).

library(testthat)
library(umx)

runTests = identical(Sys.getenv("NOT_CRAN"), "true") ||
	nzchar(Sys.getenv("CI")) ||
	interactive()

if (runTests) {
	test_check("umx")
} else {
	message("Skipping tests outside CI/interactive (e.g. CRAN).")
}
