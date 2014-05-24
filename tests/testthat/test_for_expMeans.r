# Here the ‘BivariateHeterogeneity_PathRaw.R’ script, made piecewise:
library(OpenMx); library(MASS);

selVars <- c("X","Y")
set.seed(200)
rs = .5; df1 <- MASS::mvrnorm(1000, c(0, 0), matrix(c(1, rs, rs, 1), 2, 2))
rs = .4; df2 <- MASS::mvrnorm(1000, c(0, 0), matrix(c(1, rs, rs, 1), 2, 2))
dimnames(df1) <- list(NULL, selVars)
dimnames(df2) <- list(NULL, selVars)

model1 <- mxModel("group1", type = "RAM",
	manifestVars = selVars,
	mxPath(selVars, arrows = 2, free = T, values = 1, lbound = .01, labels = c("vX1", "vY1") ),
	mxPath("X", to = "Y", arrows = 2, free = T, values = .2, lbound = .01, labels = "cXY1"),
	mxPath("one", to = selVars, values = c(0.1, -0.1), ubound = c(NA, 0), lbound = c(0, NA), labels = c("mX1", "mY1") ),
	mxData(df1, type = "raw")
)

model2 <- mxModel("group2", type = "RAM", 
	manifestVars = selVars, 
	mxPath(selVars, arrows = 2, free = T, values = 1, lbound = .01, labels = c("vX2", "vY2")),
	mxPath("X", to = "Y", arrows = 2, free = T, values = .2, lbound = .01, labels = "cXY2"),
	mxPath("one", to = selVars, arrows = 1, free = T, values = c(0.1, -0.1), ubound = c(NA, 0), lbound = c(0, NA), labels = c("mX2", "mY2")),
	mxData(df2, type = "raw")
)

m1 <- mxModel("bivariate Heterogeneity RAM", model1, model2,
	mxAlgebra( group1.fitfunction + group2.fitfunction, name = "h12" ),
	mxFitFunctionAlgebra("h12")
)

m1 = mxRun(m1) # 10909.32
testthat::expect_less_than(abs(mxEval(fitfunction, m1)- 10909.32), .01)

m2 <- mxModel("Bivar_Heterogeneity_RAM", model1, model2,
	mxFitFunctionMultigroup(c("group1.fitfunction", "group2.fitfunction"))
)
m2 <- mxRun(m2)
testthat::expect_less_than(abs(mxEval(fitfunction, m2)- 10909.32), .01)