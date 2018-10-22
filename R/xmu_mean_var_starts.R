#' Helper for boilerplate means and variance start values for twin models
#'
#' @description
#' umx_mean_var_starts can handle several common/boilerplate situations in which means and variance start values
#' are used in twin models.
#'
#' @param mzData Data for MZ pairs.
#' @param dzData Data for DZ pairs.
#' @param selVars Variable names: If sep = NULL, then treated as full names for both sibs.
#' @param sep All the variables full names.
#' @param equateMeans (NULL)
#' @param nSib How many people in a family.
#' @param varForm: "Cholesky".
#' @param SD = TRUE (FALSE = variance, not SD).
#' @param divideBy = 3 (A,C,E) 1/3rd each. Use 1 to do this yourself post-hoc.
#' @return - varStarts and meanStarts
#' @export
#' @family xmu internal not for end user
#' @examples
#' data(twinData)
#' selDVs = c("wt", "ht")
#' mzData = twinData[twinData$zygosity %in%  "MZFF", ] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF", ]
#' round(sqrt(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3),3)
#' tmp = xmu_mean_var_starts(mzData, dzData, selVars = selDVs, sep= "", equateMeans = TRUE, varForm = "Cholesky")
#' tmp
#' round(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3,3)
#' tmp = xmu_mean_var_starts(mzData, dzData, selVars = selDVs, sep= "", equateMeans = TRUE, varForm = "Cholesky", SD=FALSE)
#' tmp
#' # one variable
#' tmp = xmu_mean_var_starts(mzData, dzData, selVars = "wt", sep= "", equateMeans = TRUE, varForm = "Cholesky", SD=FALSE)
xmu_mean_var_starts <- function(mzData, dzData, selVars = selVars, sep = NULL, equateMeans= NULL, nSib = 2, varForm = c("Cholesky"), SD= TRUE, divideBy = 3) {
	# Make mxData, dropping any unused columns
	if(!is.null(sep)){
		# sep = ""; nSib = 2; selVars = c("wt", "ht")
		selVars = umx_paste_names(selVars, sep = sep, suffixes = 1:nSib)
	}

	nVar = length(selVars)/nSib
	dataType = umx_is_cov(dzData, boolean = FALSE)
	if(dataType == "raw") {
		if(is.null(equateMeans)){
			stop("you have to tell xmu_mean_var_starts whether to equate the means or not")
		}
		allData = rbind(mzData, dzData)[,selVars]
		T1 = allData[, 1:nVar, drop = FALSE]
		T2 = allData[, (nVar+1):(nVar*2), drop = FALSE];
		names(T2) = names(T1)
		longData = rbind(T1, T2)[, selVars[1:nVar], drop = FALSE]
		# Mean starts (used across all raw solutions
		meanStarts = umx_means(longData, ordVar = 0, na.rm = TRUE)
		# Make wide again
		meanStarts = c(meanStarts, meanStarts)
		if(equateMeans){
			meanLabels = paste0("mean", selVars[1:nVar]) # Names recycled for twin 2
			meanLabels = c(meanLabels, meanLabels)
		} else {
			meanLabels = paste0("mean", selVars)
		}
		varStarts = umx_var(longData, format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	} else if(dataType %in% c("cov", "cor")){
		meanStarts = NA # Not used for summary data
		meanLabels = NA
		het_mz = umx_reorder(mzData, selVars)		
		het_dz = umx_reorder(dzData, selVars)
		varStarts = diag(het_mz)[1:nVar]
	}
	# Covariance matrix, 1/3 allocated to each of A=C=E.
	varStarts = varStarts/divideBy
	if(varForm == "Cholesky"){
		if(SD){
			# sqrt() to switch from var to path coefficient scale
			# Sqrt to switch from var to path coefficient scale
			varStarts = t(chol(diag(varStarts, length(varStarts))))
			varStarts = matrix(varStarts, nVar, nVar)
		} else {
			varStarts = diag(varStarts, nVar, nVar)
		}
	} else {
		stop("Not sure how to handle varForm = ", omxQuotes(varForm))
	}
	return(list(varStarts=varStarts, meanStarts= meanStarts, meanLabels= meanLabels))
}

