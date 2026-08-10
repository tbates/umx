library(umx)
library(testthat)

test_that("umxRAM weight is stored and influences fit", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02

	# Unweighted reference
	m_unw = umxRAM("unw", data = tmp,
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	expect_true(inherits(m_unw, "MxModel"))
	expect_true(is.finite(m_unw$output$Minus2LogLikelihood))
	# No weight should be set when not requested (NULL, NA or empty)
	expect_true(is.null(m_unw$data$weight) || length(m_unw$data$weight)==0 || all(is.na(m_unw$data$weight)))
	expect_false(identical(m_unw$data$weight, "cyl"))

	# Weighted with cyl (heterogeneous frequency-like weights)
	m_w = umxRAM("w", data = tmp, weight = "cyl",
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	expect_true(inherits(m_w, "MxModel"))
	expect_true(is.finite(m_w$output$Minus2LogLikelihood))
	# Weight is stored as column name in mxData
	expect_equal(m_w$data$weight, "cyl")
	# Weight column retained in observed data (even though not a manifest)
	expect_true("cyl" %in% colnames(m_w$data$observed))
	# Weighted fit differs from unweighted (weights are variable)
	expect_false(isTRUE(all.equal(m_w$output$Minus2LogLikelihood, m_unw$output$Minus2LogLikelihood)))
	# Parameter estimates should shift with heterogeneous weights
	par_unw = parameters(m_unw)$Estimate
	par_w   = parameters(m_w)$Estimate
	expect_false(isTRUE(all.equal(par_unw, par_w)))
	# Weight column is not treated as manifest: manifestVars should be same as unweighted
	expect_equal(sort(m_w$manifestVars), sort(m_unw$manifestVars))
	expect_false("cyl" %in% m_w$manifestVars)
})

test_that("umxRAM weight=1 equals no weight and weight=2 equals duplicated data", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02

	# weight = 1 for all should equal no weight
	tmp$w1 = 1
	m1 = umxRAM("m1", data = tmp, weight = "w1",
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	m2 = umxRAM("m2", data = tmp,
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	expect_equal(m1$output$Minus2LogLikelihood, m2$output$Minus2LogLikelihood, tolerance = 1e-6)
	expect_equal(parameters(m1)$Estimate, parameters(m2)$Estimate, tolerance = 1e-6)

	# Frequency weight w=2 vs duplicated rows
	tmp$w2 = 2
	m_w2 = umxRAM("w2", data = tmp, weight = "w2",
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	tmpDup = tmp[rep(seq_len(nrow(tmp)), 2), ]
	m_dup = umxRAM("dup", data = tmpDup,
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	# Frequency weighting should give identical parameters and -2LL as duplicated data
	expect_equal(m_w2$output$Minus2LogLikelihood, m_dup$output$Minus2LogLikelihood, tolerance = 1e-6)
	expect_equal(parameters(m_w2)$Estimate, parameters(m_dup)$Estimate, tolerance = 1e-6)
	# Also -2LL should be exactly 2 * unweighted -2LL for w=2
	expect_equal(m_w2$output$Minus2LogLikelihood, 2 * m2$output$Minus2LogLikelihood, tolerance = 1e-6)
})

test_that("umxRAM weight column missing or invalid errors gracefully", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	expect_error(
		umxRAM("bad", data = tmp, weight = "notACol",
			umxPath(c("wt", "litres"), to = "mpg"),
			umxPath(v.m. = c("wt", "litres", "mpg")),
			autoRun = FALSE
		)
	)
})

test_that("umxRAM weight works with latent variable", {
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	tmp$w = rep(c(1,2), length.out = nrow(tmp))
	m_lat_w = umxRAM("lat_w", data = tmp, weight = "w",
		umxPath("F", to = c("wt", "litres", "mpg")),
		umxPath(v1m0 = "F"),
		umxPath(v.m. = c("wt", "litres", "mpg")),
		autoRun = TRUE, tryHard = "no"
	)
	expect_true(is.finite(m_lat_w$output$Minus2LogLikelihood))
	expect_true("w" %in% colnames(m_lat_w$data$observed))
	expect_false("w" %in% m_lat_w$manifestVars)
})

test_that("umxRAM weight long-standing buglet: user example syntax is valid", {
	# This is the exact example from the issue (with typo fixed)
	data(mtcars)
	tmp = mtcars
	tmp$litres = tmp$disp / 61.02
	# The issue had a typo "um  umxPath" – test the intended code runs without error
	m1 = umxRAM("testWeight", data = tmp, weight = "cyl",
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg"))
	)
	expect_true(inherits(m1, "MxModel"))
	m2 = umxRAM("noWeight", data = tmp,
		umxPath(c("wt", "litres"), to = "mpg"),
		umxPath("wt", with = "litres"),
		umxPath(v.m. = c("wt", "litres", "mpg"))
	)
	expect_true(inherits(m2, "MxModel"))
})
