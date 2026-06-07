library(testthat)
library(umx)
library(OpenMx)

test_that("plot.MxLISRELModel works and generates dot files correctly", {
	# Set up data
	vNames <- paste("v", as.character(1:6), sep="")
	dimList <- list(vNames, vNames)
	covData <- matrix(
	  c(0.9223099, 0.1862938, 0.4374359, 0.8959973, 0.9928430, 0.5320662,
		0.1862938, 0.2889364, 0.3927790, 0.3321639, 0.3371594, 0.4476898,
		0.4374359, 0.3927790, 1.0069552, 0.6918755, 0.7482155, 0.9013952,
		0.8959973, 0.3321639, 0.6918755, 1.8059956, 1.6142005, 0.8040448,
		0.9928430, 0.3371594, 0.7482155, 1.6142005, 1.9223567, 0.8777786,
		0.5320662, 0.4476898, 0.9013952, 0.8040448, 0.8777786, 1.3997558
		), nrow=6, ncol=6, byrow=TRUE, dimnames=dimList)

	# Create LISREL matrices
	mLX <- mxMatrix("Full", values=c(.5, .6, .8, rep(0, 6), .4, .7, .5),
			  name="LX", nrow=6, ncol=2,
			  free=c(TRUE,TRUE,TRUE,rep(FALSE, 6),TRUE,TRUE,TRUE),
			  dimnames=list(vNames, c("x1","x2")))
	mTD <- mxMatrix("Diag", values=c(rep(.2, 6)), 
			  name="TD", nrow=6, ncol=6, free=TRUE,
			  dimnames=dimList)
	mPH <- mxMatrix("Symm", values=c(1, .3, 1), 
			  name="PH", nrow=2, ncol=2, free=c(FALSE, TRUE, FALSE),
			  dimnames=list(c("x1","x2"),c("x1","x2")))

	expFunction <- mxExpectationLISREL(LX="LX", TD="TD", PH="PH")
	tmpData <- mxData(observed=covData, type="cov", numObs=100)
	fitFunction <- mxFitFunctionML()
	tmpModel <- mxModel(model="exampleModel",
						mLX, mTD, mPH, expFunction, fitFunction, tmpData)
	tmpModelOut <- mxRun(tmpModel, silent=TRUE)

	# Try standardising it directly
	stdModel <- umx_standardize(tmpModelOut)
	expect_s4_class(stdModel, "MxModel")

	# Try plotting it without standardisation
	expect_error(plot(tmpModelOut, file=NA), NA)

	# Try plotting it with standardisation
	expect_error(plot(tmpModelOut, std=TRUE, file=NA), NA)

	# Try plotting it with different residual options
	expect_error(plot(tmpModelOut, resid="line", file=NA), NA)
	expect_error(plot(tmpModelOut, resid="none", file=NA), NA)

	# Create a LISREL model with means
	mTX <- mxMatrix("Full", values=c(rep(0.1, 6)), name="TX", nrow=6, ncol=1, free=TRUE, dimnames=list(vNames, NULL))
	mKA <- mxMatrix("Full", values=c(0, 0), name="KA", nrow=2, ncol=1, free=FALSE, dimnames=list(c("x1","x2"), NULL))
	expFunctionMeans <- mxExpectationLISREL(LX="LX", TD="TD", PH="PH", TX="TX", KA="KA")
	meanData <- c(0.1, 0.2, 0.3, 0.1, 0.2, 0.3)
	names(meanData) <- vNames
	tmpDataMeans <- mxData(observed=covData, type="cov", numObs=100, means=meanData)
	tmpModelMeans <- mxModel(model="exampleModelMeans",
							mLX, mTD, mPH, mTX, mKA, expFunctionMeans, fitFunction, tmpDataMeans)
	tmpModelMeansOut <- mxRun(tmpModelMeans, silent=TRUE)

	# Plot model with means
	expect_error(plot(tmpModelMeansOut, file=NA), NA)
	expect_error(plot(tmpModelMeansOut, std=TRUE, file=NA), NA)
})
