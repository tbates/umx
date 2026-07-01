# "~/bin/umx/tests/testthat/test_WLS_summary_compare_workflow.R"
library(OpenMx)
library(umx)
library(testthat)

# 1. Generate the Data 
# Create a true relationship where path b1 = 0.5
set.seed(123)
df = data.frame(x = rnorm(1000))
df$y = df$x * 0.5 + rnorm(1000, mean = 0, sd = 0.8) 

test_that("RAM ML summary and comparison same for umx and lavaan", {
	lavStr = "x ~ y"
	lav1 = lavaan(lavStr, data=df, auto.var=TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE)
	tmp = summary(lav1, fit.measures=TRUE)
	tmp$fit["cfi"]

	# 2. Fit the Base RAM Model
	mxm1 = mxRun(mxModel("openmx_RAMBase",type="RAM", mxData(observed = df, type = "raw"),
		manifestVars = c("x", "y")
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
		mxPath(from = c("x", "y")  , arrows = 2, free = TRUE, values = 1),
		mxFitFunctionWLS()
	))

	umx1 = umxRAM("umxBase", data = mxData(observed = df, type = "raw"),
		umxPath(from = "x", to = "y"),
		umxPath(var = c("x", "y"))
	)

}

test_that("WLS summary and comparison same for lavaan and umx", {

	lavStr = "x ~ y"
	lav1 = lavaan(lavStr, data=df, ordered = TRUE, auto.fix.first=TRUE, auto.cov.lv.x=TRUE)
	tmp = summary(lav1, fit.measures=TRUE)
	tmp$fit["cfi"]

	# 2. Fit the Base RAM Model
	mxm1 = mxRun(mxModel("openmx_RAMBase",type="RAM", mxData(observed = df, type = "raw"),
		manifestVars = c("x", "y")
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
		mxPath(from = c("x", "y")  , arrows = 2, free = TRUE, values = 1),
		mxFitFunctionWLS()
	))

	umx1 = umxRAM("umxBase", type="DWLS", data = mxData(observed = df, type = "raw"),
		umxPath(from = "x", to = "y"),
		umxPath(var = c("x", "y"))
	)

	# 3. Fit the Nested WLS Model (Drop the b1 path to 0)
	m2 = umxModify(m1, update = "x_to_y")

	# 4. Fit an ML Model (Non-WLS)
	mMl = mxModel("ML_Base", type="RAM",
		manifestVars = c("x", "y"),
		mxPath(from = "x", to = "y", arrows = 1, free = TRUE, values = 0.2, labels = "b1"),
		mxPath(from = c("x", "y"), arrows = 2, free = TRUE, values = 1),
		mxPath(from = "one", to = c("x", "y"), free = TRUE),
		mxData(observed = df, type = "raw")
	)
	mMl = mxRun(mMl, silent = TRUE)

})