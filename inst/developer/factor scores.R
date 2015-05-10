exampleData <- data.frame(x1=rnorm(100), x2=rnorm(100), x3=rnorm(100), x4=rnorm(100), x5=rnorm(100))

# matrix specification of common factor model

factorModel <- mxModel("Initial Model",
	mxData(exampleData, "raw"),
	mxMatrix(type="Full", nrow=1, ncol=5,
		free=TRUE, values=0, name="alpha"),
	mxMatrix("Full", 1, 1, FALSE, 0,   name="mu"),
	mxMatrix("Full", 1, 5, TRUE,  0.8, name="lambda"),
	mxMatrix("Symm", 1, 1, FALSE, 1,   name="phi"),
	mxMatrix("Diag", 5, 5, TRUE,  0.6, name="theta"),
	mxAlgebra(t(lambda) %*% phi %*% lambda + theta, name="cov"),
	mxAlgebra(alpha + mu %*% lambda, name="mean"),
	mxExpectationNormal("cov", "mean", names(exampleData)),
	mxFitFunctionML()
)
	
factorResults <- mxRun(factorModel)

fsModel <- mxModel(factorResults, name="FactorScore")

fsModel$alpha$free[,] <- FALSE
fsModel$lambda$free[,] <- FALSE
fsModel$theta$free[,] <- FALSE

fsModel$mu$free[,] <- TRUE

# change objective function
fsModel$fitfunction <- mxFitFunctionML(vector=TRUE)

# change data
fsModel$data <- mxData(exampleData[1,], "raw")

# make the weighted factor score model
fullModel <- mxModel("Weighted Factor Score Model",
	fsModel,
	mxAlgebra(1 / (sqrt(2*pi)) * sqrt(det(FactorScore.phi))) *
		exp(-.5*(FactorScore.mu %&% solve(FactorScore.phi))),
		name="weight"),
	mxAlgebra(-2*log(weight %x% FactorScore.fitfunction), name="alg"),
	mxFitFunctionAlgebra("alg")
)

patterns <- unique(factorResults$data$observed)

# run the weighted factor score model
fsResults <- mxRun(fullModel)

