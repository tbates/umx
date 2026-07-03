library(OpenMx)
library(lavaan)
library(umx)
library(testthat)

test_that("WLS in lavaan and umxRAM identical", {
	# 1. Generate the Data 
	set.seed(12345)
	n  = 100
	f  = rnorm(n)
	x1 = 0.75 * f + rnorm(n, sd = 0.66)
	x2 = 0.80 * f + rnorm(n, sd = 0.60)
	x3 = 0.70 * f + rnorm(n, sd = 0.71)
	x4 = 0.85 * f + rnorm(n, sd = 0.53)

	dat = data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4)

	# Turn into ordered categorical (4 categories)
	dat[] = lapply(dat, function(x) cut(x, breaks = 4, labels = FALSE))
	dat[] = lapply(dat, ordered)

	# 1. Model in umxRAM
	# TODO: Note in WLS vignette if not clear: Data can be raw, or mxData()
	# TODO: implement m.v. as a synonym for v.m.?
	m1 = umxRAM("WLS_umx", data = dat, type = "WLS", allContinuousMethod = "cumulants",   # or "marginals"
    	umxPath("f", to = c("x1", "x2", "x3", "x4") ),
		umxPath(v1m0 = "f"),
		umxPath(v.m. = c("x1", "x2", "x3", "x4"))
	)

	# 2. Model in lavaan
	model = ' f =~ x1 + x2 + x3 + x4 '
	lav1  = cfa(model, data = dat, ordered = names(dat), estimator = "WLSMV")

	# TODO compare results!
	
	# 3. Fit the Nested WLS Model (Drop the b1 path to 0)
	m2 umxModify(m1, name = "WLS_Nested")

	# 4. Fit an ML Model (Non-WLS)
	m3 = umxRAM("ML_Base", mxData(observed = df, type = "raw"),
		umxPath(from = "x", to = "y"),
		umxPath("x", with= "y"),
		umxPath(v.m.= c("x", "y")),
		
	)

	# 5. Verify umxCompare routes to xmu_compare_WLS
	res_direct = xmu_compare_WLS(m1, m2)
	res_routed = umxCompare(m1, m2)
	expect_equal(res_routed, res_direct)

})