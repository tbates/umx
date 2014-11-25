library(OpenMx)
data(multiData1)
manifests <- c("x1", "y")
multiData1Cov <- cov(multiData1[,c(1,5)])
uniRegModel <- mxModel("Univariate Regression of y on x1", type="RAM",
   manifestVars=manifests,
   mxPath("x1", to="y", free=TRUE, values=.2, labels="b1"),
   mxPath(manifests, arrows=2, free=TRUE, values=-.8, labels=c("VarX1", "VarE")),
   mxData(multiData1Cov, type="cov", numObs=500)
)
mxRun(uniRegModel)
ign <- omxCheckWarning(try(mxRun(uniRegModel)),
	paste("In model 'Univariate Regression of y on x1' Optimizer returned a non-zero status code 10.",
    "Starting values are not feasible. Consider mxTryHard()")
)

3996

Running Univariate Regression of y on x1
Error: The job for model 'Univariate Regression of y on x1' exited abnormally with the error message: MxComputeGradientDescent: fitfunction Univariate Regression of y on x1.fitfunction is not finite (Expected covariance matrix is non-positive-definite)
							 >