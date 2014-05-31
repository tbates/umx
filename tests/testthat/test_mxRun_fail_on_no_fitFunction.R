# test: require mxFitFunctionML

library(OpenMx)
# A couple of helpful label strings
v1_6 = paste0("v",1:6) # [1] "v1" "v2" "v3" "v4" "v5" "v6"
x1_2 = c("x1", "x2")

covData <- matrix(nrow=6, ncol=6, byrow=TRUE, dimnames=list(v1_6, v1_6),
	data = c(0.9223099, 0.1862938, 0.4374359, 0.8959973, 0.9928430, 0.5320662,
            0.1862938, 0.2889364, 0.3927790, 0.3321639, 0.3371594, 0.4476898,
            0.4374359, 0.3927790, 1.0069552, 0.6918755, 0.7482155, 0.9013952,
            0.8959973, 0.3321639, 0.6918755, 1.8059956, 1.6142005, 0.8040448,
            0.9928430, 0.3371594, 0.7482155, 1.6142005, 1.9223567, 0.8777786,
            0.5320662, 0.4476898, 0.9013952, 0.8040448, 0.8777786, 1.3997558))

# Create the model, fit it, and print a summary.

m1 <- mxModel(model = "exampleModel", 
	# Create LISREL matrices
	mxMatrix(name="TD", "Diag", nrow=6, ncol=6, values = rep(.2, 6) , free=T, dimnames = list(v1_6, v1_6)),
	mxMatrix(name="PH", "Symm", nrow=2, ncol=2, values = c(1, .3, 1), free=c(F, T, F), dimnames = list(x1_2, x1_2)),
	mxMatrix(name="LX", "Full", nrow=6, ncol=2, values = c(.5, .6, .8, rep(0,6), .4, .7, .5), free = c(T,T,T, rep(F, 6),T,T,T), dimnames = list(v1_6, x1_2)),
	# Create a LISREL objective with LX, TD, and PH matrix names
	mxExpectationLISREL(LX = "LX", TD = "TD", PH = "PH"),
	# mxFitFunctionML(),
	mxData(covData, type = "cov", numObs = 100)
)
# this mxRun should fail...
m1 <- mxRun(m1)
# if mxRun doesn't fail, summary will have no fit statistics
# make sure Minus2LogLikelihood is not NA
testthat::expect_false(is.na(summary(m1)$Minus2LogLikelihood))
# test it is the correct value
expectedValue = 292.9183
obtainedValue = summary(m1)$Minus2LogLikelihood
testthat::expect_equal(obtainedValue,  expected = expectedValue, tolerance = 0.002, scale = expectedValue)
