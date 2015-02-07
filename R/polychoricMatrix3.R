#
# Copyright 2007-2009 The OpenMx Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

#
# Define function for computing polychoric/polyserial/pearson correlations
#
polychoricMatrix <- function(data, useDeviations=TRUE) {
	nvar         <- dim(data)[[2]]
	ncontinuous  = nordinal = 0
	nthresh      <- vector(mode="integer",nvar)
	isord        <- vector(mode="logical",nvar)
	nameList     <- names(data)
	ordnameList  <- vector(mode="character",nvar)
	contnameList <- vector(mode="character",nvar)

	# Figure out which variables are ordinal factors
	correlationLabels <- matrix(NA,nrow=nvar,ncol=nvar)
	for (i in 1:nvar) {
	    if (is.factor(data[,i])){
	        nordinal <- nordinal + 1
	        nthresh[nordinal] <- length(table(data[,i]))-1
	        ordnameList[nordinal] <- nameList[i]
	        isord[i] <- TRUE
	    } else {
	        ncontinuous <- ncontinuous + 1
	        nthresh[i] <- 0
	        contnameList[ncontinuous] <- nameList[i]
	        isord[i] <- FALSE
	    }
		# Label correlation parameters
	    for (k in 1:nvar) {
	        if (i > k) {
				correlationLabels[i,k] <- paste("r",i,k)
				correlationLabels[k,i] <- paste("r",i,k)
	        }
	   }
	}
	if (nordinal>0) {
		ordnameList<-ordnameList[1:nordinal]
	} else {
		ordnameList <- NULL
	}
	if (ncontinuous>0) {
		contnameList<-contnameList[1:ncontinuous]
	} else {
		contnameList <- NULL
	}

	# Find largest number of thresholds of all the ordinal variables
	maxnthresh <- max(nthresh)

	# Populate matrix with threshold deviations, starting threshold 1 at -1 and putting maximum at +1
	# for efficiency, we could take a better guess to start with
	minthresh <- -.5
	maxthresh <- .5

	# Construct either threshold deviation matrix or threshold direct estimate matrix - as long as there's at least one ordinal variable
	if (nordinal > 0){
	    if (useDeviations) {
	            thresholdDeviationValues     <- matrix(0,nrow=maxnthresh, ncol=nordinal)
	            thresholdDeviationValues[1,] <- minthresh
	            thresholdDeviationLbounds    <- matrix(nrow=maxnthresh, ncol=nordinal)
	            thresholdDeviationLabels     <- matrix(nrow=maxnthresh, ncol=nordinal)
	            thresholdDeviationLabels[1,] <- paste("ThresholdDeviation ", 1, 1:nordinal)
	            thresholdDeviationFree       <- matrix(F,nrow=maxnthresh, ncol=nordinal)
	            thresholdDeviationFree[1,]   <- TRUE
	            iordvar                      <- 0
				for (i in 1:nvar) { 
	            	if (isord[i]) {
	                    iordvar <- iordvar + 1
	                    if(nthresh[iordvar]>1) {
	                        for (j in 2:nthresh[iordvar]) {
	                                thresholdDeviationValues[j,iordvar] <- (maxthresh - minthresh) / nthresh[iordvar]
	                                thresholdDeviationLbounds[j,iordvar] <- .001
	                                thresholdDeviationLabels[j,iordvar] <- paste("ThresholdDeviation ", j, iordvar)
	                                thresholdDeviationFree[j,iordvar] <- TRUE
	                            }
	                    }
	                }
				}
	        } else {
				# no deviations
	            thresholdDirectEstimatesValues     <- matrix(0,nrow=maxnthresh, ncol=nordinal)
	            thresholdDirectEstimatesLbounds    <- matrix(-Inf,nrow=maxnthresh, ncol=nordinal)
	            thresholdDirectEstimatesLabels     <- matrix(nrow=maxnthresh, ncol=nordinal)
	            thresholdDirectEstimatesFree       <- matrix(F,nrow=maxnthresh, ncol=nordinal)
	            thresholdDirectEstimatesValues[1,] <- minthresh
	            thresholdDirectEstimatesLabels[1,] <- paste("ThresholdDirectEstimates ", 1, 1:nordinal)
	            thresholdDirectEstimatesFree[1,]   <- TRUE
	            iordvar <- 0
	            for (i in 1:nvar) { 
	                    if (isord[i]) {
	                        iordvar <- iordvar + 1
	                        if(nthresh[iordvar]>1){
	                                for (j in 2:nthresh[iordvar]){
	                                        thresholdDirectEstimatesValues[j,iordvar] <- minthresh + (j-1) * ((maxthresh - minthresh) / nthresh[iordvar])
	                                        thresholdDirectEstimatesLabels[j,iordvar] <- paste("ThresholdDirectEstimate ", j, iordvar)
	                                        thresholdDirectEstimatesFree[j,iordvar] <- TRUE
	                                }
	                        }
	                  }
	            }
	     }
	}
	nameList <- names(data)
	tnames <- paste("Threshold",1:maxnthresh,sep='')

	# Define the model
	model <- mxModel('model')
	model <- mxModel(model, mxMatrix("Stand", name = "R", nrow = nvar, ncol = nvar, free=TRUE, labels=correlationLabels, lbound=-.999999999, ubound=.999999999, dimnames=list(nameList, nameList)))
	model <- mxModel(model, mxMatrix("Full", name = "M", nrow = 1, ncol = nvar, free=!isord, dimnames = list('Mean', nameList)))
	model <- mxModel(model, mxMatrix("Diag", name = "StdDev", nrow = nvar, ncol = nvar, free=!isord, values=1, lbound=.01, dimnames=list(nameList, nameList)))
	model$expCov <- mxAlgebra(StdDev %&% R, dimnames=list(nameList,nameList))

	# Algebra to compute Threshold matrix
	if (nordinal > 0){
	    if (useDeviations) {
	        # For Multiplication
	        model <- mxModel(model, mxMatrix("Lower", name="UnitLower", nrow = maxnthresh, ncol = maxnthresh, free=F, values=1))
	        # Threshold differences:
	        model <- mxModel(model, mxMatrix("Full", name="thresholdDeviations", nrow = maxnthresh, ncol = nordinal, free=thresholdDeviationFree,   values=thresholdDeviationValues, lbound=thresholdDeviationLbounds, labels = thresholdDeviationLabels))
	        model <- mxModel(model, mxAlgebra(UnitLower %*% thresholdDeviations, dimnames=list(tnames,ordnameList), name="thresholds"))
	        } else {
	        model <- mxModel(model, mxMatrix("Full", name="thresholds", ncol = nordinal, nrow = maxnthresh, free=thresholdDirectEstimatesFree, values=thresholdDirectEstimatesValues, lbound=thresholdDirectEstimatesLbounds, labels = thresholdDirectEstimatesLabels))
	        dimnames(model$thresholds)=list(tnames,ordnameList)
	    }
	}

	# Define the objective function
	if (nordinal > 0){
	    objective <- mxFIMLObjective(covariance="expCov", means="M", thresholds="thresholds", threshnames=ordnameList)
	}else{
	    objective <- mxFIMLObjective(covariance="expCov", means="M")
	}

	# Define the observed covariance matrix
	dataMatrix <- mxData(data, type='raw')

	# Add the objective function and the data to the model
	model <- mxModel(model, objective, dataMatrix)

	# Run the job
	model <- mxRun(model, unsafe=T)

	# Populate seMatrix for return
	seMatrix <- matrix(NA,nvar,nvar)
	k<-0
	for (i in 1:nvar){
	    for (j in i:nvar){
	        if(i != j) {
	            k <- k+1
	            seMatrix[i,j] <- model@output$standardErrors[k]
	            seMatrix[j,i] <- model@output$standardErrors[k]
	        }
	    }
	}
	# Add dimnames to thresholds, which oddly are not in model$thresholds' output
	if(nordinal > 0) {
	    if(useDeviations){
	        thresholds <- matrix(model@output$algebras$model.thresholds, nrow=maxnthresh, ncol=nordinal, dimnames=list(tnames,ordnameList))     
	    } else{
	        thresholds <- matrix(model@output$matrices$model.thresholds, nrow=maxnthresh, ncol=nordinal, dimnames=list(tnames,ordnameList))     
	    }
	}else{
	    thresholds <- NULL
	}
	# Return results      
	return(list(polychorics=model$expCov@result, thresholds=thresholds, polychoricStandardErrors=seMatrix, Minus2LogLikelihood=model@output$Minus2LogLikelihood, Hessian=model@output$calculatedHessian, estHessian=model@output$estimatedHessian,estimatedModel=model))
}


# Pairwise wrapper function
polypairwise <- function (data, useDeviations=TRUE, printFit=FALSE, use="any") {
    nvar <- dim(data)[[2]]
    ncor <- nvar*(nvar-1)/2
    pairCorrelationMatrix <- matrix(diag(,nvar),nvar,nvar,dimnames=list(names(data),names(data)))
    pairErrorMatrix <- matrix(diag(,nvar),nvar,nvar,dimnames=list(names(data),names(data)))
    pairErrors <- matrix(0,ncor,1)
    pairCount <- 0
    namelist <- NULL
    for (var1 in 1:(nvar-1)) {
        for (var2 in (var1+1):(nvar)) {
            pairCount <- pairCount + 1
            cat(c("\n\n",pairCount,names(data)[var1],names(data)[var2]))
            if (use=="complete.obs")
            {
                tempData <- data[complete.cases(data[,c(var1,var2)]),c(var1,var2)]
            }
            else
            {
                tempData <- data[,c(var1,var2)]
            }
            tempResult <- polychoricMatrix(tempData, useDeviations)
            pairCorrelationMatrix[var1,var2] <- tempResult$polychorics[2,1]
            pairCorrelationMatrix[var2,var1] <- pairCorrelationMatrix[var1,var2]
            pairErrors[pairCount] <- tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var1,var2] <- tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var2,var1] <- pairErrorMatrix[var1,var2]
            namelist <- c(namelist,paste(names(data[var1]),names(data[var2]),sep="-"))
            # If the variables are both ordinal, figure out -2lnL for all proportions
            if (is.factor(data[,var1]) && is.factor(data[,var2]))
            {
                tabmatrix <- as.matrix(table(data[,c(var1,var2)],useNA='no'))
                proportions <- tabmatrix/sum(tabmatrix)
                logliks <- (log(proportions)*tabmatrix)
                if(printFit){
                cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik <- -2*sum(logliks[!is.na(logliks)])))
                sumres <- summary(tempResult$estimatedModel)
                cat(paste("\n -2 times fitted log-likelihood", sumres$Minus2LogLikelihood))
                cat(paste("\n Difference in -2lnL units", diffchi <- sumres$Minus2LogLikelihood - minus2SatLogLik))
                cat(paste("\n Number of parameters of fitted model",sumres$estimatedParameters))
                cat(paste("\n Number of cells of contingency table =",nCells <- length(table(data[,c(var1,var2)]))))
                cat(paste("\n Effective number of degrees of freedom", (df <- nCells-sumres$estimatedParameters-1)))
                cat(paste("\n p-value", pchisq(diffchi, df, lower.tail=F)))
                cat(paste("\n N = ", sum(tabmatrix)))
                cat("\n\n")
            }
            }
        }
    }
    dimnames(pairErrors) <- list(namelist,"est(SE)")
    return(list(R=pairCorrelationMatrix,SE=pairErrors,SEMatrix=pairErrorMatrix))
}

# Pairwise wrapper function
polytriowise <- function (data, useDeviations=TRUE, printFit=FALSE, use="any") {
    nvar <- dim(data)[[2]]
    if(nvar < 3) stop("Must have at least three variables for trio-wise polychorics")
    ncor <- nvar*(nvar-1)/2
    pairCorrelationMatrix <- matrix(NA,nvar,nvar,dimnames=list(names(data),names(data)))
    diag(pairCorrelationMatrix) <- 1
    pairErrorMatrix <- matrix(diag(,nvar),nvar,nvar,dimnames=list(names(data),names(data)))
    pairErrors <- matrix(0,ncor,1)
    pairCount <- 0
    namelist <- NULL
    for (var1 in 1:(nvar2-1)) {
        for (var2 in (var1+1):(nvar2)) {
            pairCount <- pairCount + 1
            cat(c("\n\n",pairCount,names(data)[var1],names(data)[var2]))
            if (use=="complete.obs")
            {
                tempData <- cbind(mustHaveData,data[complete.cases(data[,c(var1,var2)]),c(var1,var2)])
            }
            else
            {
                tempData <- cbind(mustHaveData,data[,c(var1,var2)])
            }
            tempResult <- polychoricMatrix(tempData, useDeviations)
            pairCorrelationMatrix[var1,var2] <- tempResult$polychorics[2,1]
            pairCorrelationMatrix[var2,var1] <- pairCorrelationMatrix[var1,var2]
            pairErrors[pairCount] <- tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var1,var2] <- tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var2,var1] <- pairErrorMatrix[var1,var2]
            namelist <- c(namelist,paste(names(data[var1]),names(data[var2]),sep="-"))
            # If the variables are both ordinal, figure out -2lnL for all proportions
            if (is.factor(data[,var1]) && is.factor(data[,var2]))
            {
                tabmatrix <- as.matrix(table(data[,c(var1,var2)],useNA='no'))
                proportions <- tabmatrix/sum(tabmatrix)
                logliks <- (log(proportions)*tabmatrix)
                if(printFit){
                cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik <- -2*sum(logliks[!is.na(logliks)])))
                sumres <- summary(tempResult$estimatedModel)
                cat(paste("\n -2 times fitted log-likelihood", sumres$Minus2LogLikelihood))
                cat(paste("\n Difference in -2lnL units", diffchi <- sumres$Minus2LogLikelihood - minus2SatLogLik))
                cat(paste("\n Number of parameters of fitted model",sumres$estimatedParameters))
                cat(paste("\n Number of cells of contingency table =",nCells <- length(table(data[,c(var1,var2)]))))
                cat(paste("\n Effective number of degrees of freedom", (df <- nCells-sumres$estimatedParameters-1)))
                cat(paste("\n p-value", pchisq(diffchi, df, lower.tail=F)))
                cat(paste("\n N = ", sum(tabmatrix)))
                cat("\n\n")
            }
            }
        }
    }
	# dimnames(pairErrors) <- list(namelist,"est(SE)")
    return(list(R=pairCorrelationMatrix,SE=pairErrors,SEMatrix=pairErrorMatrix))
}
