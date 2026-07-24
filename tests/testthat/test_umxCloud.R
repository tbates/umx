library(umx)
library(testthat)

test_that("umxCloud formals and offline guards", {
	f = formals(umxCloud)
	expect_false("data" %in% names(f))
	expect_equal(eval(f$serverType), "cpx31")
	expect_equal(eval(f$dockerImage), "rocker/r-ver:4.4.2")
	expect_true(isTRUE(eval(f$autoDestroy)))

	# Missing token must fail before network (empty env)
	old = Sys.getenv("HETZNER_API_TOKEN", unset = NA_character_)
	on.exit({
		if (is.na(old)) {
			Sys.unsetenv("HETZNER_API_TOKEN")
		} else {
			Sys.setenv(HETZNER_API_TOKEN = old)
		}
	}, add = TRUE)
	Sys.unsetenv("HETZNER_API_TOKEN")

	# Minimal MxModel shell without calling Hetzner (token check first)
	# Use a real tiny model object if OpenMx available
	m = mxModel("tiny", mxMatrix("Full", 1, 1, free = FALSE, values = 1, name = "A"))
	expect_error(
		umxCloud(m, apiToken = "", verbose = FALSE),
		regexp = "token"
	)
	expect_error(
		umxCloud(list(a = 1), apiToken = "dummy", verbose = FALSE),
		regexp = "MxModel"
	)
})
