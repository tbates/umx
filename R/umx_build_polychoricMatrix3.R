#' FIML-based polychoric, polyserial, and pearson correlations
#'
#' Compute polychoric/polyserial/pearson correlations with FIML in OpenMx
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @return - matrix of correlations
#' @export
#' @family Data Functions
#' @references - \url{https://doi.org/10.3389/fpsyg.2016.00528}
#' @examples
#' # TODO: Needs examples
umx_polychoric <- function(data, useDeviations = TRUE) {
	nVar         = dim(data)[[2]]
	nContinuous  = nOrdinal = 0
	nThresh      = vector(mode = "integer", nVar)
	isOrd        = vector(mode = "logical", nVar)
	nameList     = names(data)
	ordnameList  = vector(mode = "character", nVar)
	contnameList = vector(mode = "character", nVar)

	# Figure out which variables are ordinal factors
	correlationLabels = matrix(NA, nrow = nVar, ncol = nVar)
	for (i in 1:nVar) {
	    if (is.factor(data[,i])){
	        nOrdinal = nOrdinal + 1
	        nThresh[nOrdinal] = length(table(data[, i])) - 1
	        ordnameList[nOrdinal] = nameList[i]
	        isOrd[i] = TRUE
	    } else {
	        nContinuous = nContinuous + 1
	        nThresh[i] = 0
	        contnameList[nContinuous] = nameList[i]
	        isOrd[i] = FALSE
	    }
		# Label correlation parameters
	    for (k in 1:nVar) {
	        if (i > k) {
				correlationLabels[i, k] = paste("r", i, k)
				correlationLabels[k, i] = paste("r", i, k)
	        }
	   }
	}
	if (nOrdinal > 0) {
		ordnameList = ordnameList[1:nOrdinal]
	} else {
		ordnameList = NULL
	}
	if (nContinuous > 0) {
		contnameList = contnameList[1:nContinuous]
	} else {
		contnameList = NULL
	}

	# Find largest number of thresholds of all the ordinal variables
	maxnThresh = max(nThresh)

	# Populate matrix with threshold deviations, starting threshold 1 at -1 and putting maximum at +1
	# for efficiency, we could take a better guess to start with
	minThresh = (-.5)
	maxthresh =   .5

	# Construct either threshold deviation matrix or threshold direct estimate matrix - as long as there's at least one ordinal variable
	if (nOrdinal > 0){
	    if (useDeviations) {
	            thresholdDeviationValues     = matrix(0, nrow = maxnThresh, ncol = nOrdinal)
	            thresholdDeviationValues[1,] = minThresh
	            thresholdDeviationLbounds    = matrix(nrow = maxnThresh, ncol = nOrdinal)
	            thresholdDeviationLabels     = matrix(nrow = maxnThresh, ncol = nOrdinal)
	            thresholdDeviationLabels[1,] = paste("ThresholdDeviation ", 1, 1:nOrdinal)
	            thresholdDeviationFree       = matrix(F, nrow = maxnThresh, ncol = nOrdinal)
	            thresholdDeviationFree[1,]   = TRUE
	            iordvar                      = 0
				for (i in 1:nVar) { 
	            	if (isOrd[i]) {
	                    iordvar = iordvar + 1
	                    if(nThresh[iordvar]>1) {
	                        for (j in 2:nThresh[iordvar]) {
	                                thresholdDeviationValues[j,iordvar] = (maxthresh - minThresh) / nThresh[iordvar]
	                                thresholdDeviationLbounds[j,iordvar] = .001
	                                thresholdDeviationLabels[j,iordvar] = paste("ThresholdDeviation ", j, iordvar)
	                                thresholdDeviationFree[j,iordvar] = TRUE
	                            }
	                    }
	                }
				}
	        } else {
				# no deviations
	            thresholdDirectEstimatesValues     = matrix(0,nrow=maxnThresh, ncol=nOrdinal)
	            thresholdDirectEstimatesLbounds    = matrix(-Inf,nrow=maxnThresh, ncol=nOrdinal)
	            thresholdDirectEstimatesLabels     = matrix(nrow=maxnThresh, ncol=nOrdinal)
	            thresholdDirectEstimatesFree       = matrix(F,nrow=maxnThresh, ncol=nOrdinal)
	            thresholdDirectEstimatesValues[1,] = minThresh
	            thresholdDirectEstimatesLabels[1,] = paste("ThresholdDirectEstimates ", 1, 1:nOrdinal)
	            thresholdDirectEstimatesFree[1,]   = TRUE
	            iordvar = 0
	            for (i in 1:nVar) { 
	                    if (isOrd[i]) {
	                        iordvar = iordvar + 1
	                        if(nThresh[iordvar]>1){
	                                for (j in 2:nThresh[iordvar]){
	                                        thresholdDirectEstimatesValues[j,iordvar] = minThresh + (j-1) * ((maxthresh - minThresh) / nThresh[iordvar])
	                                        thresholdDirectEstimatesLabels[j,iordvar] = paste("ThresholdDirectEstimate ", j, iordvar)
	                                        thresholdDirectEstimatesFree[j,iordvar] = TRUE
	                                }
	                        }
	                  }
	            }
	     }
	}
	nameList = names(data)
	tnames = paste("Threshold",1:maxnThresh,sep='')

	# Define the model
	model = mxModel('model')
	model = mxModel(model, mxMatrix("Stand", name = "R", nrow = nVar, ncol = nVar, free=TRUE, labels=correlationLabels, lbound=-.999999999, ubound=.999999999, dimnames=list(nameList, nameList)))
	model = mxModel(model, mxMatrix("Full", name = "M", nrow = 1, ncol = nVar, free=!isOrd, dimnames = list('Mean', nameList)))
	model = mxModel(model, mxMatrix("Diag", name = "StdDev", nrow = nVar, ncol = nVar, free=!isOrd, values=1, lbound=.01, dimnames=list(nameList, nameList)))
	model$expCov = mxAlgebra(StdDev %&% R, dimnames=list(nameList,nameList))

	# Algebra to compute Threshold matrix
	if (nOrdinal > 0){
	    if (useDeviations) {
	        # For Multiplication
	        model = mxModel(model, mxMatrix("Lower", name="UnitLower", nrow = maxnThresh, ncol = maxnThresh, free=F, values=1))
	        # Threshold differences:
	        model = mxModel(model, mxMatrix("Full", name="thresholdDeviations", nrow = maxnThresh, ncol = nOrdinal, free=thresholdDeviationFree,   values=thresholdDeviationValues, lbound=thresholdDeviationLbounds, labels = thresholdDeviationLabels))
	        model = mxModel(model, mxAlgebra(UnitLower %*% thresholdDeviations, dimnames=list(tnames,ordnameList), name="thresholds"))
	        } else {
	        model = mxModel(model, mxMatrix("Full", name="thresholds", ncol = nOrdinal, nrow = maxnThresh, free=thresholdDirectEstimatesFree, values=thresholdDirectEstimatesValues, lbound=thresholdDirectEstimatesLbounds, labels = thresholdDirectEstimatesLabels))
	        dimnames(model$thresholds)=list(tnames,ordnameList)
	    }
	}

	# Define the objective function
	if (nOrdinal > 0){
	    objective = mxFIMLObjective(covariance="expCov", means="M", thresholds="thresholds", threshnames=ordnameList)
	}else{
	    objective = mxFIMLObjective(covariance="expCov", means="M")
	}

	# Define the observed covariance matrix
	dataMatrix = mxData(data, type='raw')

	# Add the objective function and the data to the model
	model = mxModel(model, objective, dataMatrix)

	# Run the job
	model = mxRun(model, unsafe=T)

	# Populate seMatrix for return
	seMatrix = matrix(NA,nVar,nVar)
	k=0
	for (i in 1:nVar){
	    for (j in i:nVar){
	        if(i != j) {
	            k = k+1
	            seMatrix[i,j] = model@output$standardErrors[k]
	            seMatrix[j,i] = model@output$standardErrors[k]
	        }
	    }
	}
	# Add dimnames to thresholds, which oddly are not in model$thresholds' output
	if(nOrdinal > 0) {
	    if(useDeviations){
	        thresholds = matrix(model@output$algebras$model.thresholds, nrow=maxnThresh, ncol=nOrdinal, dimnames=list(tnames,ordnameList))     
	    } else{
	        thresholds = matrix(model@output$matrices$model.thresholds, nrow=maxnThresh, ncol=nOrdinal, dimnames=list(tnames,ordnameList))     
	    }
	}else{
	    thresholds = NULL
	}
	# Return results      
	return(list(polychorics=model$expCov@result, thresholds=thresholds, polychoricStandardErrors=seMatrix, Minus2LogLikelihood=model@output$Minus2LogLikelihood, Hessian=model@output$calculatedHessian, estHessian=model@output$estimatedHessian,estimatedModel=model))
}


#' FIML-based Pairwise polychoric, polyserial, and pearson correlations
#'
#' Compute polychoric/polyserial/pearson correlations with FIML in OpenMx
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @param printFit Whether to print information about the fit achievedd (default = FALSE)
#' @param use parameter (default = "any")
#' @return - matrix of correlations
#' @export
#' @family Data Functions
#' @references - \url{https://doi.org/10.3389/fpsyg.2016.00528}
#' @examples
#' # TODO: Needs examples
umx_polypairwise <- function (data, useDeviations=TRUE, printFit=FALSE, use="any") {
    nVar = dim(data)[[2]]
    ncor = nVar*(nVar-1)/2
    pairCorrelationMatrix = matrix(diag(,nVar),nVar,nVar,dimnames=list(names(data),names(data)))
    pairErrorMatrix = matrix(diag(,nVar),nVar,nVar,dimnames=list(names(data),names(data)))
    pairErrors = matrix(0,ncor,1)
    pairCount = 0
    namelist = NULL
    for (var1 in 1:(nVar-1)) {
        for (var2 in (var1+1):(nVar)) {
            pairCount = pairCount + 1
            cat(c("\n\n",pairCount,names(data)[var1],names(data)[var2]))
            if (use=="complete.obs")
            {
                tempData = data[stats::complete.cases(data[,c(var1,var2)]),c(var1,var2)]
            }
            else
            {
                tempData = data[,c(var1,var2)]
            }
            tempResult = umx_polychoric(tempData, useDeviations)
            pairCorrelationMatrix[var1,var2] = tempResult$polychorics[2,1]
            pairCorrelationMatrix[var2,var1] = pairCorrelationMatrix[var1,var2]
            pairErrors[pairCount] = tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var1,var2] = tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var2,var1] = pairErrorMatrix[var1,var2]
            namelist = c(namelist,paste(names(data[var1]),names(data[var2]),sep="-"))
            # If the variables are both ordinal, figure out -2lnL for all proportions
            if (is.factor(data[,var1]) && is.factor(data[,var2]))
            {
                tabmatrix = as.matrix(table(data[,c(var1,var2)],useNA='no'))
                proportions = tabmatrix/sum(tabmatrix)
                logliks = (log(proportions)*tabmatrix)
                if(printFit){
                cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik = -2*sum(logliks[!is.na(logliks)])))
                sumres = summary(tempResult$estimatedModel)
                cat(paste("\n -2 times fitted log-likelihood", sumres$Minus2LogLikelihood))
                cat(paste("\n Difference in -2lnL units", diffchi = sumres$Minus2LogLikelihood - minus2SatLogLik))
                cat(paste("\n Number of parameters of fitted model",sumres$estimatedParameters))
                cat(paste("\n Number of cells of contingency table =",nCells = length(table(data[,c(var1,var2)]))))
                cat(paste("\n Effective number of degrees of freedom", (df = nCells-sumres$estimatedParameters-1)))
                cat(paste("\n p-value", stats::pchisq(diffchi, df, lower.tail=F)))
                cat(paste("\n N = ", sum(tabmatrix)))
                cat("\n\n")
            }
            }
        }
    }
    dimnames(pairErrors) = list(namelist,"est(SE)")
    return(list(R=pairCorrelationMatrix,SE=pairErrors,SEMatrix=pairErrorMatrix))
}

#' FIML-based trio-based polychoric, polyserial, and pearson correlations
#'
#' Compute polychoric/polyserial/pearson correlations with FIML in OpenMx.
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @param printFit Whether to print information about the fit achievedd (default = FALSE)
#' @param use parameter (default = "any")
#' @return - matrix of correlations
#' @export
#' @family Data Functions
#' @references - \url{https://doi.org/10.3389/fpsyg.2016.00528}
#' @examples
#' # TODO: Needs examples
umx_polytriowise <- function (data, useDeviations = TRUE, printFit = FALSE, use = "any") {
    nVar = dim(data)[[2]]
    if(nVar < 3) {
		stop("Must have at least three variables for trio-wise polychorics")
	}
    ncor = nVar * (nVar - 1) / 2
    pairCorrelationMatrix = matrix(NA, nVar, nVar, dimnames = list(names(data), names(data)))
    diag(pairCorrelationMatrix) = 1
    pairErrorMatrix = matrix(diag(,nVar),nVar,nVar,dimnames=list(names(data),names(data)))
    pairErrors = matrix(0,ncor,1)
    pairCount = 0
    namelist = NULL
    for (var1 in 1:(nVar2-1)) {
        for (var2 in (var1+1):(nVar2)) {
            pairCount = pairCount + 1
            cat(c("\n\n", pairCount, names(data)[var1], names(data)[var2]))
            if (use == "complete.obs"){
				# TODO used to say, but no visible binding for mustHaveData
				# tempData = cbind(mustHaveData, data[stats::complete.cases(data[,c(var1,var2)]),c(var1,var2)])
                tempData = data[stats::complete.cases(data[,c(var1, var2)]), c(var1,var2)]
            } else {
                tempData = data[,c(var1,var2)]
            }
            tempResult = umx_polychoric(tempData, useDeviations)
            pairCorrelationMatrix[var1,var2] = tempResult$polychorics[2,1]
            pairCorrelationMatrix[var2,var1] = pairCorrelationMatrix[var1,var2]
            pairErrors[pairCount] = tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var1,var2] = tempResult$polychoricStandardErrors[2,1]
            pairErrorMatrix[var2,var1] = pairErrorMatrix[var1,var2]
            namelist = c(namelist,paste(names(data[var1]),names(data[var2]),sep="-"))
            # If the variables are both ordinal, figure out -2lnL for all proportions
            if (is.factor(data[,var1]) && is.factor(data[,var2])) {
                tabmatrix = as.matrix(table(data[,c(var1,var2)],useNA='no'))
                proportions = tabmatrix/sum(tabmatrix)
                logliks = (log(proportions)*tabmatrix)
                if(printFit){
	                cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik = -2*sum(logliks[!is.na(logliks)])))
	                sumres = summary(tempResult$estimatedModel)
	                cat(paste("\n -2 times fitted log-likelihood", sumres$Minus2LogLikelihood))
	                cat(paste("\n Difference in -2lnL units", diffchi = sumres$Minus2LogLikelihood - minus2SatLogLik))
	                cat(paste("\n Number of parameters of fitted model",sumres$estimatedParameters))
	                cat(paste("\n Number of cells of contingency table =",nCells = length(table(data[,c(var1,var2)]))))
	                cat(paste("\n Effective number of degrees of freedom", (df = nCells-sumres$estimatedParameters-1)))
	                cat(paste("\n p-value", stats::pchisq(diffchi, df, lower.tail=F)))
	                cat(paste("\n N = ", sum(tabmatrix)))
	                cat("\n\n")
            	}
            }
        }
    }
	# dimnames(pairErrors) = list(namelist,"est(SE)")
    return(list(R = pairCorrelationMatrix, SE = pairErrors, SEMatrix = pairErrorMatrix))
}
