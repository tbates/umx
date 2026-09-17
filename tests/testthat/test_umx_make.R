library(umx)
library(testthat)
context("umx_make sitrep and deps_install")

test_that("deps_install installs hard dependencies only by default", {
	skip_if_not_installed("pak")
	fakeRoot = tempfile()
	dir.create(fakeRoot)
	writeLines(c("Package: fakepkg", "Version: 0.1"), file.path(fakeRoot, "DESCRIPTION"))
	seenDeps = list()
	testthat::with_mocked_bindings(
		local_install_dev_deps = function(root, ...) {
			seenDeps$root <<- root
			seenDeps$dots <<- list(...)
			invisible(NULL)
		},
		.package = "pak",
		umx_make("deps_install", pkg = fakeRoot)
	)
	expect_identical(seenDeps$dots$dependencies, NA)
})

test_that("deps_install forwards dependencies = TRUE for full setup", {
	skip_if_not_installed("pak")
	fakeRoot = tempfile()
	dir.create(fakeRoot)
	writeLines(c("Package: fakepkg", "Version: 0.1"), file.path(fakeRoot, "DESCRIPTION"))
	seenDeps = list()
	testthat::with_mocked_bindings(
		local_install_dev_deps = function(root, ...) {
			seenDeps$root <<- root
			seenDeps$dots <<- list(...)
			invisible(NULL)
		},
		.package = "pak",
		umx_make("deps_install", pkg = fakeRoot, dependencies = TRUE)
	)
	expect_identical(seenDeps$dots$dependencies, TRUE)
})

test_that("sitrep reports hard deps, ignores Suggests, points at deps_install", {
	fakeRoot = tempfile()
	dir.create(fakeRoot)
	writeLines(c("Package: fakepkg", "Version: 0.1", "Depends: R (>= 4.1.0), MASS", "Imports: utils", "Suggests: knitr"),
		file.path(fakeRoot, "DESCRIPTION"))
	fakeOld = matrix(c("MASS", .libPaths()[1L], "1.0", "4.6.0", "2.0", "CRAN"), nrow = 1L,
		dimnames = list(c("MASS"), c("Package", "LibPath", "Installed", "Built", "ReposVer", "Repository")))
	testthat::with_mocked_bindings(
		old.packages = function(...) fakeOld,
		.package = "utils",
		{
			expect_message(umx_make("sitrep", pkg = fakeRoot), "MASS out of date")
			expect_message(umx_make("sitrep", pkg = fakeRoot), "umx_make[(]\"deps_install\"[)]")
			gotMsgs = character()
			withCallingHandlers(umx_make("sitrep", pkg = fakeRoot),
				message = function(m) {
					gotMsgs <<- c(gotMsgs, conditionMessage(m))
					invokeRestart("muffleMessage")
				}
			)
			expect_false(any(grepl("knitr", gotMsgs, fixed = TRUE)))
		}
	)
})

test_that("sitrep still reminds when CRAN is unreachable", {
	fakeRoot = tempfile()
	dir.create(fakeRoot)
	writeLines(c("Package: fakepkg", "Version: 0.1", "Depends: R (>= 4.1.0), MASS", "Imports: utils"),
		file.path(fakeRoot, "DESCRIPTION"))
	testthat::with_mocked_bindings(
		old.packages = function(...) stop("trying to use CRAN without setting a mirror"),
		.package = "utils",
		{
			expect_message(umx_make("sitrep", pkg = fakeRoot), "CRAN outdated-check skipped")
			expect_message(umx_make("sitrep", pkg = fakeRoot), "umx_make[(]\"deps_install\"[)]")
		}
	)
})
