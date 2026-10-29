library(testthat)
library(umx)
library(OpenMx)

test_that("umxAutoStart matches mxAutoStart without definition variables", {
	data(demoOneFactor)
	v = names(demoOneFactor)[1:3]
	m1 = umxRAM("autostart_plain", data = demoOneFactor[, v], autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(v.m. = v),
		umxPath(v1m0 = "G")
	)
	s1 = omxGetParameters(umxAutoStart(m1))
	s2 = omxGetParameters(mxAutoStart(m1))
	expect_equal(s1, s2)
})

test_that("umxAutoStart handles definition variables", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	mDef = umxRAM("autostart_def", data = dd, autoRun = FALSE,
		umxPath("G", to = "x1"),
		umxPath(v.m. = "x1"),
		umxPath(v1m0 = "G"),
		umxPath(defn = "def", labels = "data.age"),
		umxPath("def", to = "x1")
	)
	expect_error(mxAutoStart(mDef), "Definition variables found")
	s = umxAutoStart(mDef)
	starts = omxGetParameters(s)
	expect_true(all(is.finite(starts)))
	r1 = mxRun(s, silent = TRUE)
	expect_equal(r1$output$status$code, 0)
})

test_that("umxAutoStart works with factor columns in the data", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	dd$group = factor(rep(c("a", "b"), length.out = nrow(dd)))
	mDef = umxRAM("autostart_fac", data = dd, autoRun = FALSE,
		umxPath("G", to = "x1"),
		umxPath(v.m. = "x1"),
		umxPath(v1m0 = "G"),
		umxPath(defn = "def", labels = "data.age"),
		umxPath("def", to = "x1")
	)
	s = umxAutoStart(mDef)
	expect_true(all(is.finite(omxGetParameters(s))))
})

test_that("umxAutoStart placeholders row1, mean, and zero all run", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	placeholders = c("row1", "mean", "zero")
	for (ph in placeholders) {
		mDef = umxRAM(paste0("autostart_", ph), data = dd, autoRun = FALSE,
			umxPath("G", to = "x1"),
			umxPath(v.m. = "x1"),
			umxPath(v1m0 = "G"),
			umxPath(defn = "def", labels = "data.age"),
			umxPath("def", to = "x1")
		)
		s = umxAutoStart(mDef, placeholder = ph)
		expect_true(all(is.finite(omxGetParameters(s))))
	}
})

test_that("umxAutoStart handles multigroup definition variable models", {
	set.seed(1)
	data(demoOneFactor)
	dd = demoOneFactor
	dd$age = rnorm(nrow(dd))
	mkGroup = function(name, d) {
		umxRAM(name, data = d, autoRun = FALSE,
			umxPath("G", to = "x1"),
			umxPath(v.m. = "x1"),
			umxPath(v1m0 = "G"),
			umxPath(defn = "def", labels = "data.age"),
			umxPath("def", to = "x1")
		)
	}
	mg = mxModel("autostart_mg", mkGroup("g1", dd[1:250, ]), mkGroup("g2", dd[251:500, ]),
		mxFitFunctionMultigroup(c("g1", "g2")))
	# equated labels need one start each before any autostart (mxAutoStart asks the same)
	mg = omxAssignFirstParameters(mg)
	s = umxAutoStart(mg)
	expect_true(all(is.finite(omxGetParameters(s))))
	# same optimum as default starts (this 1-indicator toy is Hessian-singular either way)
	r0 = mxRun(mg, silent = TRUE)
	r1 = mxRun(s, silent = TRUE)
	expect_equal(r1$output$fit, r0$output$fit)
})
