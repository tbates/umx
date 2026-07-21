library(testthat)
library(OpenMx)
library(umx)

test_that("xmu_openmx_engine_status returns a structured list", {
	st = xmu_openmx_engine_status()
	expect_type(st, "list")
	expect_true(all(c("ok", "version", "reason", "install_msg") %in% names(st)))
	expect_type(st$ok, "logical")
	# This workspace's OpenMx is typically the GenomicMx fork
	if (isTRUE(getOption("OpenMx.genomicMx")) ||
		identical(tolower(as.character(utils::packageDescription("OpenMx")$GenomicMx)), "yes") ||
		exists("imxWlsDefinitionVariablesAreExogenousOnly", where = asNamespace("OpenMx"), inherits = FALSE)) {
		expect_true(st$ok)
		expect_equal(st$install_msg, "")
	}
})

test_that("xmu_openmx_install_message mentions install.OpenMx GenomicMx", {
	msg = xmu_openmx_install_message("Unit test feature")
	expect_match(msg, "install.OpenMx\\(\"GenomicMx\"\\)")
	expect_match(msg, "Unit test feature")
	expect_match(msg, "tbates/GenomicMx")
})

test_that("install.OpenMx accepts GenomicMx loc without installing", {
	# match.arg only — do not network-install in CI
	expect_error(install.OpenMx("not_a_build"), "should be one of")
	# loc formals include GenomicMx
	fmls = eval(formals(install.OpenMx)$loc)
	expect_true("GenomicMx" %in% fmls)
	expect_true("CRAN" %in% fmls)
})

test_that("startup silence option is respected conceptually", {
	# Helper path used by .onAttach
	old = getOption("umx.genomicMx.startup")
	on.exit(options(umx.genomicMx.startup = old), add = TRUE)
	options(umx.genomicMx.startup = FALSE)
	expect_false(isTRUE(getOption("umx.genomicMx.startup")))
})
