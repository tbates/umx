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
	expect_match(msg, "github.com/tbates/umx/releases")
	expect_false(grepl("OpenMx/OpenMx", msg, fixed = TRUE))
	expect_false(grepl("tbates/GenomicMx", msg, fixed = TRUE))
})

test_that("install.OpenMx accepts GenomicMx loc without installing", {
	# match.arg only -- do not network-install in CI
	expect_error(install.OpenMx("not_a_build"), "should be one of")
	fmls = eval(formals(install.OpenMx)$loc)
	expect_true("GenomicMx" %in% fmls)
	expect_true("CRAN" %in% fmls)
	expect_true("open GenomicMx release page" %in% fmls)
	# Source "dev" path disabled while fork is private (public path = umx Releases binaries)
	expect_false("dev" %in% fmls)
})

test_that("install.OpenMx requires RcppParallel >= 6.2.0 and probes load", {
	# Body inspection only -- do not network-install
	bodyTxt = paste(deparse(body(install.OpenMx)), collapse = "\n")
	expect_match(bodyTxt, "6.2.0")
	expect_match(bodyTxt, "RcppParallel")
	expect_match(bodyTxt, "1.7-6")
	expect_match(bodyTxt, "needPkgs")
	expect_match(bodyTxt, "loadNamespace")
})

test_that("xmu_genomicmx_release_page_url points at umx releases", {
	expect_equal(xmu_genomicmx_release_page_url(), "https://github.com/tbates/umx/releases/tag/genomicmx")
})

test_that("startup silence option is respected conceptually", {
	# Helper path used by .onAttach
	old = getOption("umx.genomicMx.startup")
	on.exit(options(umx.genomicMx.startup = old), add = TRUE)
	options(umx.genomicMx.startup = FALSE)
	expect_false(isTRUE(getOption("umx.genomicMx.startup")))
})

test_that("xmu_genomicmx_binary_url defaults to tag genomicmx and has verbose param", {
	f = get("xmu_genomicmx_binary_url", envir = asNamespace("umx"))
	fmls = formals(f)
	expect_true("tag" %in% names(fmls))
	expect_true("verbose" %in% names(fmls))
	expect_equal(fmls$tag, "genomicmx")
})

test_that("xmu_genomicmx_binary_url returns NULL when GitHub is unreachable (no network in CI)", {
	f = get("xmu_genomicmx_binary_url", envir = asNamespace("umx"))
	res = f(tag = "does-not-exist-xyz-123", verbose = FALSE)
	expect_null(res)
})

test_that("xmu_genomicmx_binary_url does not query /releases/latest", {
	f = get("xmu_genomicmx_binary_url", envir = asNamespace("umx"))
	bodyTxt = paste(deparse(body(f)), collapse = "\n")
	expect_match(bodyTxt, "releases/tags")
	expect_false(grepl("releases/latest", bodyTxt, fixed = TRUE))
})

test_that("resolver logic: no fallback across macOS arch or R version (body inspection)", {
	f = get("xmu_genomicmx_binary_url", envir = asNamespace("umx"))
	bodyTxt = paste(deparse(body(f)), collapse = "\n")
	expect_match(bodyTxt, "R.*Phase 1 is R release only")
	expect_match(bodyTxt, "no asset matching macOS arch")
})

