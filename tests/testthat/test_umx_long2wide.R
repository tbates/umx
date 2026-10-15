# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_long2wide.R")

require(umx)
data(twinData)
tmp = twinData[, -2]
tmp$twinID1 = 1; tmp$twinID2 = 2
long = umx_wide2long(data = tmp, sep = "")
args = list(famID = "fam", twinID = "twinID", zygosity = "zygosity", vars2keep = c("bmi", "wt"), passalong = "cohort")
ref = suppressMessages(do.call(umx_long2wide, c(list(data = long), args)))

test_that("umx_long2wide works on a data.frame", {
	expect_s3_class(ref, "data.frame")
	expect_equal(nrow(ref), length(unique(long$fam)))
	expect_true(all(c("bmi_T1", "bmi_T2", "wt_T1", "wt_T2", "cohort") %in% names(ref)))
})

test_that("umx_long2wide accepts data.tables", {
	skip_if_not_installed("data.table")
	dt = data.table::as.data.table(long)
	wide = suppressMessages(do.call(umx_long2wide, c(list(data = dt), args)))
	expect_equal(wide, ref)
})

test_that("umx_long2wide realizes lazy inputs via their as.data.frame method", {
	# Stand-in for a lazy dtplyr/dbplyr pipeline: unusable until as.data.frame() realizes it
	lazy = structure(list(source = long), class = "umx_test_lazy")
	as.data.frame.umx_test_lazy <- function(x, ...) x$source
	registerS3method("as.data.frame", "umx_test_lazy", as.data.frame.umx_test_lazy)
	wide = suppressMessages(do.call(umx_long2wide, c(list(data = lazy), args)))
	expect_equal(wide, ref)
})
