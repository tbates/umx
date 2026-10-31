library(testthat)
library(umx)

test_that("gh_refresh_cached_GenomicMx check reports staleness", {
	skip_on_cran()
	skip_if(Sys.which("gh") == "", "gh CLI not installed")
	ghAuthed = suppressWarnings(system2("gh", c("auth", "status"), stdout = FALSE, stderr = FALSE)) == 0
	skip_if(!ghAuthed, "gh CLI not authenticated")
	skip_if(!dir.exists(path.expand("~/bin/OpenMx")), "no local OpenMx checkout")
	out = gh_refresh_cached_GenomicMx(check = TRUE)
	expect_true(all(c("cached", "local", "stale") %in% names(out)))
	expect_s3_class(out$cached, "data.frame")
	expect_true(all(c("hash", "built") %in% names(out$cached)))
	expect_equal(length(out$stale), 1)
	expect_true(is.logical(out$stale))
})
