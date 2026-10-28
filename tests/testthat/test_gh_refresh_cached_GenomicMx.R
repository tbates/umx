library(testthat)
library(umx)

test_that("gh_refresh_cached_GenomicMx check reports staleness", {
	skip_on_cran()
	skip_if(Sys.which("gh") == "", "gh CLI not installed")
	out = gh_refresh_cached_GenomicMx(check = TRUE)
	expect_true(all(c("cached", "local", "stale") %in% names(out)))
	expect_s3_class(out$cached, "data.frame")
	expect_true(all(c("hash", "built") %in% names(out$cached)))
	expect_equal(length(out$stale), 1)
	expect_true(is.logical(out$stale))
})
