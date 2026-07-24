#' FIML-based polychoric, polyserial, and Pearson correlations
#'
#' Compute polychoric/polyserial/Pearson correlations with FIML.
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @param tryHard 'no' uses normal mxRun (default), "yes" uses mxTryHard, and others used named versions: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @return - list of output and diagnostics. matrix of correlations = $polychorics
#' @export
#' @family Data Functions
#' @references - Barendse, M. T., Ligtvoet, R., Timmerman, M. E., & Oort, F. J. (2016). Model Fit after Pairwise Maximum Likelihood. *Frontiers in Psychology*, **7**, 528. \doi{10.3389/fpsyg.2016.00528}.
#' @examples
#' \dontrun{
#' tmp = mtcars
#' tmp$am = umxFactor(mtcars$am)
#' tmp$vs = umxFactor(mtcars$vs)
#' tmp = umx_scale(tmp)
#' x = umx_polychoric(tmp[, c("am", "vs")], tryHard = "yes")
#' x$polychorics
#' cor(mtcars[, c("am", "vs")])
#' 
#' }
#' 
umx_polychoric <- function(data, useDeviations = TRUE, tryHard = c("no", "yes", "ordinal", "search")) {
	tryHard      = match.arg(tryHard)
	if (!is.data.frame(data)) {
		data = as.data.frame(data)
	}
	nVar         = ncol(data)
	nameList     = names(data)
	if (is.null(nameList) || any(!nzchar(nameList))) {
		nameList = paste0("V", seq_len(nVar))
		names(data) = nameList
	}
	nThresh      = integer(nVar)
	isOrd        = logical(nVar)
	ordnameList  = character(0)
	contnameList = character(0)

	# Figure out which variables are ordinal factors (index nThresh by column i, not ordinal count)
	correlationLabels = matrix(NA_character_, nrow = nVar, ncol = nVar)
	for (i in 1:nVar) {
		if (is.factor(data[, i])) {
			if (!is.ordered(data[, i])) {
				data[, i] = mxFactor(data[, i], levels = levels(data[, i]))
			}
			nThresh[i] = nlevels(data[, i]) - 1L
			if (nThresh[i] < 1L) {
				stop("umx_polychoric: factor ", omxQuotes(nameList[i]), " has fewer than 2 levels.")
			}
			ordnameList = c(ordnameList, nameList[i])
			isOrd[i] = TRUE
		} else {
			nThresh[i] = 0L
			contnameList = c(contnameList, nameList[i])
			isOrd[i] = FALSE
		}
		for (k in 1:nVar) {
			if (i > k) {
				correlationLabels[i, k] = paste0("r", i, k)
				correlationLabels[k, i] = paste0("r", i, k)
			}
		}
	}
	nOrdinal = sum(isOrd)
	nContinuous = sum(!isOrd)
	if (nOrdinal == 0) {
		ordnameList = NULL
	}
	if (nContinuous == 0) {
		contnameList = NULL
	}

	# Empirical starts: continuous means/SDs; ordinal thresholds from marginal proportions
	meanStarts = numeric(nVar)
	sdStarts = rep(1, nVar)
	for (i in 1:nVar) {
		if (!isOrd[i]) {
			meanStarts[i] = mean(as.numeric(data[, i]), na.rm = TRUE)
			sdi = stats::sd(as.numeric(data[, i]), na.rm = TRUE)
			sdStarts[i] = if (is.finite(sdi) && sdi > 0.01) sdi else 1
		}
	}

	# Largest number of thresholds among ordinal variables
	maxnThresh = if (nOrdinal > 0) max(nThresh) else 0L

	# Threshold starts from cumulative proportions (avoids log(-Inf) integration crashes on sparse pairs)
	xmu_thresh_starts_from_data <- function(x) {
		tab = table(x, useNA = "no")
		if (sum(tab) < 1) {
			return(rep(0, max(1, nlevels(x) - 1L)))
		}
		cum = cumsum(as.numeric(tab)) / sum(tab)
		# Drop final 1.0; map empty tails away from +/- Inf
		z = stats::qnorm(pmin(pmax(cum[-length(cum)], 1e-6), 1 - 1e-6))
		z[!is.finite(z)] = 0
		z
	}

	if (nOrdinal > 0) {
		if (useDeviations) {
			thresholdDeviationValues  = matrix(0, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDeviationLbounds = matrix(NA_real_, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDeviationLabels  = matrix(NA_character_, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDeviationFree    = matrix(FALSE, nrow = maxnThresh, ncol = nOrdinal)
			iordvar = 0L
			for (i in 1:nVar) {
				if (isOrd[i]) {
					iordvar = iordvar + 1L
					nt = nThresh[i]
					zStarts = xmu_thresh_starts_from_data(data[, i])
					if (length(zStarts) < nt) {
						zStarts = c(zStarts, seq_len(nt - length(zStarts)) * 0.2)
					}
					# Convert absolute thresholds to positive deviations
					thresholdDeviationValues[1, iordvar] = zStarts[1]
					thresholdDeviationFree[1, iordvar] = TRUE
					thresholdDeviationLabels[1, iordvar] = paste("ThresholdDeviation", 1, iordvar)
					if (nt > 1L) {
						for (j in 2:nt) {
							dev = zStarts[j] - zStarts[j - 1]
							if (!is.finite(dev) || dev < 0.001) {
								dev = 0.2
							}
							thresholdDeviationValues[j, iordvar] = dev
							thresholdDeviationLbounds[j, iordvar] = 0.001
							thresholdDeviationLabels[j, iordvar] = paste("ThresholdDeviation", j, iordvar)
							thresholdDeviationFree[j, iordvar] = TRUE
						}
					}
				}
			}
		} else {
			thresholdDirectEstimatesValues  = matrix(0, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDirectEstimatesLbounds = matrix(-Inf, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDirectEstimatesLabels  = matrix(NA_character_, nrow = maxnThresh, ncol = nOrdinal)
			thresholdDirectEstimatesFree    = matrix(FALSE, nrow = maxnThresh, ncol = nOrdinal)
			iordvar = 0L
			for (i in 1:nVar) {
				if (isOrd[i]) {
					iordvar = iordvar + 1L
					nt = nThresh[i]
					zStarts = xmu_thresh_starts_from_data(data[, i])
					if (length(zStarts) < nt) {
						zStarts = c(zStarts, seq_len(nt - length(zStarts)) * 0.2)
					}
					for (j in 1:nt) {
						thresholdDirectEstimatesValues[j, iordvar] = zStarts[j]
						thresholdDirectEstimatesLabels[j, iordvar] = paste("ThresholdDirectEstimate", j, iordvar)
						thresholdDirectEstimatesFree[j, iordvar] = TRUE
					}
				}
			}
		}
	}
	tnames = if (maxnThresh > 0) paste0("Threshold", 1:maxnThresh) else character(0)

	# Define the model: ordinal/binary latent mean@0 residual@1; continuous free mean/SD
	model = mxModel('model',
		mxMatrix("Stand", name = "R", nrow = nVar, ncol = nVar, free = TRUE, labels = correlationLabels, values = 0, lbound = -.999999, ubound = .999999, dimnames = list(nameList, nameList)),
		mxMatrix("Full", name = "M", nrow = 1, ncol = nVar, free = !isOrd, values = meanStarts, dimnames = list('Mean', nameList)),
		mxMatrix("Diag", name = "StdDev", nrow = nVar, ncol = nVar, free = !isOrd, values = sdStarts, lbound = .01, dimnames = list(nameList, nameList))
	)
	model$expCov = mxAlgebra(StdDev %&% R, dimnames = list(nameList, nameList), name = "expCov")

	# Algebra to compute Threshold matrix
	if (nOrdinal > 0) {
		if (useDeviations) {
			model = mxModel(model,
				mxMatrix("Lower", name = "UnitLower", nrow = maxnThresh, ncol = maxnThresh, free = FALSE, values = 1),
				mxMatrix("Full", name = "thresholdDeviations", nrow = maxnThresh, ncol = nOrdinal, free = thresholdDeviationFree, values = thresholdDeviationValues, lbound = thresholdDeviationLbounds, labels = thresholdDeviationLabels),
				mxAlgebra(UnitLower %*% thresholdDeviations, dimnames = list(tnames, ordnameList), name = "thresholds")
			)
		} else {
			model = mxModel(model, mxMatrix("Full", name = "thresholds", ncol = nOrdinal, nrow = maxnThresh, free = thresholdDirectEstimatesFree, values = thresholdDirectEstimatesValues, lbound = thresholdDirectEstimatesLbounds, labels = thresholdDirectEstimatesLabels))
			dimnames(model$thresholds) = list(tnames, ordnameList)
		}
	}

	if (nOrdinal > 0) {
		expectation = mxExpectationNormal(covariance = "expCov", means = "M", thresholds = "thresholds", threshnames = ordnameList)
	} else {
		expectation = mxExpectationNormal(covariance = "expCov", means = "M")
	}

	model = mxModel(model,
		expectation,
		mxFitFunctionML(),
		mxData(data, type = "raw")
	)

	# Run (tryHard already optimizes; only mxRun when tryHard == "no")
	varMsg = paste(nameList, collapse = ", ")
	safeRun <- function(model, how) {
		out = tryCatch({
			if (how == "no") {
				mxRun(model, silent = TRUE)
			} else if (how == "yes") {
				mxTryHard(model, silent = TRUE, bestInitsOutput = FALSE)
			} else if (how == "ordinal") {
				mxTryHardOrdinal(model, silent = TRUE, bestInitsOutput = FALSE)
			} else if (how == "search") {
				mxTryHardWideSearch(model, silent = TRUE, bestInitsOutput = FALSE)
			} else {
				stop("tryHard = ", omxQuotes(how), " not known: use no, yes, ordinal, or search")
			}
		}, error = function(e) e)
		out
	}
	fit = safeRun(model, tryHard)
	# Fallback chain if first strategy fails (esp. mixed continuous/binary on stock OpenMx)
	if (!inherits(fit, "MxModel") || is.null(fit$output) || !isTRUE(fit$output$status$code %in% c(0L, 1L))) {
		for (how in setdiff(c("ordinal", "yes", "search", "no"), tryHard)) {
			fit2 = safeRun(model, how)
			if (inherits(fit2, "MxModel") && !is.null(fit2$output) && isTRUE(fit2$output$status$code %in% c(0L, 1L))) {
				fit = fit2
				break
			}
			if (inherits(fit2, "MxModel") && umx_has_been_run(fit2)) {
				fit = fit2
			}
		}
	}
	if (!inherits(fit, "MxModel")) {
		stop("umx_polychoric: failed to fit variables ", omxQuotes(varMsg),
			". Ensure ordinal columns are ordered factors (umxFactor) and continuous columns have variance.", call. = FALSE)
	}
	if (!umx_has_been_run(fit)) {
		fit = tryCatch(mxRun(fit, silent = TRUE), error = function(e) {
			stop("umx_polychoric: failed to fit variables ", omxQuotes(varMsg), ": ", conditionMessage(e), call. = FALSE)
		})
	}
	model = fit

	# Populate seMatrix for return
	seMatrix = matrix(NA_real_, nVar, nVar)
	if (!is.null(model@output$standardErrors)) {
		k = 0L
		for (i in 1:nVar) {
			for (j in i:nVar) {
				if (i != j) {
					k = k + 1L
					if (k <= length(model@output$standardErrors)) {
						seMatrix[i, j] = model@output$standardErrors[k]
						seMatrix[j, i] = model@output$standardErrors[k]
					}
				}
			}
		}
	}
	if (nOrdinal > 0) {
		if (useDeviations) {
			thresholds = matrix(model@output$algebras$model.thresholds, nrow = maxnThresh, ncol = nOrdinal, dimnames = list(tnames, ordnameList))
		} else {
			thresholds = matrix(model@output$matrices$model.thresholds, nrow = maxnThresh, ncol = nOrdinal, dimnames = list(tnames, ordnameList))
		}
	} else {
		thresholds = NULL
	}
	expCov = tryCatch(model$expCov$result, error = function(e) NULL)
	if (is.null(expCov)) {
		expCov = tryCatch(mxEval(expCov, model), error = function(e) matrix(NA_real_, nVar, nVar, dimnames = list(nameList, nameList)))
	}
	return(list(
		polychorics = expCov,
		thresholds = thresholds,
		polychoricStandardErrors = seMatrix,
		Minus2LogLikelihood = model@output$Minus2LogLikelihood,
		Hessian = model@output$calculatedHessian,
		estHessian = model@output$estimatedHessian,
		estimatedModel = model
	))
}


#' FIML-based Pairwise polychoric, polyserial, and Pearson correlations
#'
#' Compute polychoric/polyserial/Pearson correlations with FIML in OpenMx, but pair by pair, not across the whole dataset at once.
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @param printFit Whether to print information about the fit achieved (default = FALSE)
#' @param use parameter (default = "any")
#' @param tryHard 'no' uses normal mxRun (default), "yes" uses mxTryHard, and others used named versions: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @return - matrix of correlations
#' @export
#' @family Data Functions
#' @references - Barendse, M. T., Ligtvoet, R., Timmerman, M. E., & Oort, F. J. (2016). Model Fit after Pairwise Maximum Likelihood. *Frontiers in Psychology*, **7**, 528. \doi{10.3389/fpsyg.2016.00528}.
#' @examples
#' tmp = mtcars
#' tmp$am = umxFactor(mtcars$am)
#' tmp$vs = umxFactor(mtcars$vs)
#' # Scale continuous only (leave ordered factors alone)
#' tmp[, c("hp", "mpg")] = umx_scale(tmp[, c("hp", "mpg")])
#' x = umx_polypairwise(tmp[, c("hp", "mpg", "am", "vs")], tryHard = "ordinal")
#' x$R
#' cor(mtcars[, c("hp", "mpg", "am", "vs")])
umx_polypairwise <- function (data, useDeviations= TRUE, printFit= FALSE, use= "any", tryHard = c("no", "yes", "ordinal", "search")) {
	tryHard = match.arg(tryHard)

    nVar = dim(data)[[2]]
    ncor = nVar*(nVar-1)/2
    pairCorrelationMatrix = matrix(diag(,nVar), nVar, nVar, dimnames= list(names(data), names(data)))
    pairErrorMatrix = matrix(diag(,nVar), nVar, nVar, dimnames= list(names(data), names(data)))
    pairErrors = matrix(0,ncor,1)
    pairCount = 0
    namelist = NULL
	 for (var1 in 1:(nVar-1)) {
		 for (var2 in (var1+1):(nVar)) {
			pairCount = pairCount + 1
			cat(c("\n\n",pairCount,names(data)[var1],names(data)[var2]))
			if (use=="complete.obs"){
				 tempData = data[stats::complete.cases(data[,c(var1,var2)]),c(var1,var2)]
			}else{
			    tempData = data[,c(var1,var2)]
			}
			tempResult = umx_polychoric(tempData, useDeviations, tryHard = tryHard)
			pairCorrelationMatrix[var1,var2] = tempResult$polychorics[2,1]
			pairCorrelationMatrix[var2,var1] = pairCorrelationMatrix[var1,var2]
			pairErrors[pairCount] = tempResult$polychoricStandardErrors[2,1]
			pairErrorMatrix[var1,var2] = tempResult$polychoricStandardErrors[2,1]
			pairErrorMatrix[var2,var1] = pairErrorMatrix[var1,var2]
			namelist = c(namelist,paste(names(data[var1]),names(data[var2]),sep="-"))
			# If the variables are both ordinal, figure out -2lnL for all proportions
         if (is.factor(data[,var1]) && is.factor(data[,var2])) {
             tabmatrix = as.matrix(table(data[,c(var1, var2)], useNA= 'no'))
             proportions = tabmatrix/sum(tabmatrix)
             logliks = (log(proportions)*tabmatrix)
             if(printFit){
                cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik = -2*sum(logliks[!is.na(logliks)])))
                sumres = summary(tempResult$estimatedModel)
                cat(paste("\n -2 times fitted log-likelihood", sumres$Minus2LogLikelihood))
                cat(paste("\n Difference in -2lnL units", diffchi = sumres$Minus2LogLikelihood - minus2SatLogLik))
                cat(paste("\n Number of parameters of fitted model", sumres$estimatedParameters))
                cat(paste("\n Number of cells of contingency table =", nCells = length(table(data[,c(var1,var2)]))))
                cat(paste("\n Effective number of degrees of freedom", (df = nCells-sumres$estimatedParameters-1)))
                cat(paste("\n p-value", stats::pchisq(diffchi, df, lower.tail= FALSE)))
                cat(paste("\n N = ", sum(tabmatrix)))
                cat("\n\n")
			 	}
			}
		}
	}
	dimnames(pairErrors) = list(namelist,"est(SE)")
	return(list(R= pairCorrelationMatrix, SE= pairErrors, SEMatrix= pairErrorMatrix))
}

#' FIML-based trio-based polychoric, polyserial, and Pearson correlations
#'
#' Compute polychoric/polyserial/Pearson correlations with FIML in OpenMx.
#'
#' @param data Dataframe
#' @param useDeviations Whether to code the mode using deviation thresholds (default = TRUE)
#' @param printFit Whether to print information about the fit achieved (default = FALSE)
#' @param use parameter (default = "any")
#' @param tryHard 'no' uses normal mxRun (default), "yes" uses mxTryHard, and others used named versions: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @return - matrix of correlations
#' @export
#' @family Data Functions
#' @references - \doi{10.3389/fpsyg.2016.00528}
#' @examples
#' \dontrun{
#' tmp = mtcars
#' tmp$am = umxFactor(mtcars$am)
#' tmp$vs = umxFactor(mtcars$vs)
#' tmp[, c("hp", "mpg")] = umx_scale(tmp[, c("hp", "mpg")])
#' x = umx_polytriowise(tmp[, c("hp", "mpg", "am", "vs")], tryHard = "ordinal")
#' x$R
#' cor(mtcars[, c("hp", "mpg", "am", "vs")])
#' }
#'
umx_polytriowise <- function (data, useDeviations = TRUE, printFit = FALSE, use = "any", tryHard = c("no", "yes", "ordinal", "search")) {
	tryHard = match.arg(tryHard)
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
	for (var1 in 1:(nVar-1)) {
		for (var2 in (var1+1):(nVar)) {
			pairCount = pairCount + 1
			cat(c("\n\n", pairCount, names(data)[var1], names(data)[var2]))
			if (use == "complete.obs"){
			    tempData = data[stats::complete.cases(data[,c(var1, var2)]), c(var1,var2)]
			} else {
			    tempData = data[,c(var1,var2)]
			}
			tempResult = umx_polychoric(tempData, useDeviations, tryHard=tryHard)
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
					sumres = summary(tempResult$estimatedModel)
					cat(paste("\n -2 times saturated log-likelihood", minus2SatLogLik = -2*sum(logliks[!is.na(logliks)])))
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
