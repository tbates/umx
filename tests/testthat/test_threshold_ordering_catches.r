# Load libraries
require(OpenMx)
require(MASS)
isIdentified <- function(nVariables, nFactors){
	as.logical(1 + sign((nVariables * (nVariables - 1)/2) -  nVariables * nFactors + nFactors * (nFactors - 1) / 2))
}

# Set up simulation parameters: nVariables>=3, nThresholds>=1, nSubjects>=nVariables*nThresholds 
# and model should be identified
nVariables   <- 5
nFactors     <- 1
nThresholds  <- 3
nSubjects    <- 500
isIdentified(nVariables, nFactors) # if this function returns FALSE then model is not identified, otherwise it is.

loadings     <- matrix(.7,nrow=nVariables,ncol=nFactors)
residuals    <- 1 - (loadings * loadings)
sigma        <- loadings %*% t(loadings) + vec2diag(residuals)
mu           <- matrix(0,nrow=nVariables,ncol=1)

# Simulate multivariate normal data
set.seed(1234)
continuousData <- mvrnorm(n = nSubjects, mu, sigma)
head(continuousData)

# Chop continuous variables into ordinal data with nThresholds+1 approximately equal categories, based on 1st variable
quants       <- quantile(continuousData[,1],  probs = c((1:nThresholds)/(nThresholds+1)))
ordinalData  <- matrix(0, nrow=nSubjects, ncol=nVariables)
for(i in 1:nVariables) {
	ordinalData[,i]  <- cut(as.vector(continuousData[,i]),c(-Inf,quants,Inf))
}
# Make the ordinal variables into R factors and Name them
ordinalData <- as.data.frame(ordinalData)
ordinalData <- mxFactor(ordinalData,levels=c(1:(nThresholds+1)))
varNames    <- paste0("banana", 1:nVariables)
names(ordinalData) <- varNames
str(ordinalData)

# Create Factor Model with Raw Ordinal Data and Matrices Input
m1 <- mxModel("oneFactorThresholdModel",
	mxMatrix(name="facLoadings", type="Full", nrow=nVariables, ncol=nFactors, free=TRUE, values=0.2, lbound=-.99, ubound=.99),
	mxMatrix(name="unitVector", type="Unit", nrow=nVariables, ncol=1),
	mxMatrix(name="expMeans", type="Zero", nrow=1, ncol=nVariables),
	mxMatrix(name="deviations", type="Full", nrow=nThresholds, ncol=nVariables, free=TRUE, values=.2,
		lbound = rep(c(-Inf, rep(.01,(nThresholds-1))) , nVariables),
		dimnames=list(c(), varNames)),
	mxMatrix(name="unitLower", type="Lower", nrow=nThresholds, ncol=nThresholds, free=FALSE, values=1),
	mxAlgebra(unitVector -  (diag2vec(facLoadings %*% t(facLoadings))), name="resVariances"),
	mxAlgebra(facLoadings %*% t(facLoadings) + vec2diag(resVariances), name="expCovariances"),
	mxAlgebra(unitLower %*% deviations, name="expThresholds" ),
	mxExpectationNormal("expCovariances", means="expMeans", dimnames=varNames, thresholds="expThresholds" ),
	mxFitFunctionML(),
	mxData(ordinalData, type='raw')
)

# Fit the model
m1 <- mxRun(m1, suppressWarnings = TRUE)
summary(m1)