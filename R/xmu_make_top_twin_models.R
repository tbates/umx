#' Add a means model to a twin model
#'
#' @description
#' Add simple or definition based means model to a twin model (i.e., which contains top MZ and DZ models)
#'
#' @param model The model we are modifying (must have MZ DZ and top submodels)
#' @param defVars the names of definition variables
#' @param sep How twin variable names are expanded (default "_T")
#' @return - model with means model added.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [xmuTwinMeanModelParts_top()], [xmuTwinMeans_MZDZ()]
#' @md
#' @examples
#' \dontrun{
#' defVars = c("VETSA1_age","VETSA2_age", "VETSA3_age")
#' umxTwinAddMeansModel(m1, defVars = defVars, sep = "_T")
#' xmuDefBetasInTop(defVars = defVars)
#' xmuDefMeanInDataGroup(defVars = defVars, expMeanAlgName = "expMean")
#' m1 = umxTwinAddMeansModel(m1, defVars = c("a", "b"))
#' }
#'
umxTwinAddMeansModel <- function(model, defVars = NULL, sep = "_T"){
	# need to check the def vars are still in the dataset at this point...?
	umx_check(all(c("MZ", "DZ", "top") %in% names(model)), "stop", message="need a model with top, MZ and DZ submodels")	
	if(!is.null(defVars)){
		# 1. Stick means and betas into top and Stick alg and def matrices into MZ and DZ
		newTop = mxModel(model$top, xmuTwinMeanModelParts_top(defVars = defVars, sep= sep))
		newMZ  = mxModel(model$MZ , xmuTwinMeans_MZDZ(defVars = defVars, sep=sep))
		newDZ  = mxModel(model$DZ , xmuTwinMeans_MZDZ(defVars = defVars, sep=sep))
		model  = mxModel(model, newTop, newMZ, newDZ)
	} else {
		# no need to change MZ DZ models... all in top.
		# ...
	}
	return(model)
}

#' Not for end-users: Create matrices and algebra for means in twin models
#'
#' @description
#' Returns two def matrices and the means algebra which live in `model$MZ`
#'
#' @param defVars the names of definition variables
#' @param sep How twin variable names are expanded (default "_T")
#' @param expMeanAlgName The name we expect for the algebra (leave set to "expMean")
#' @return - two def matrices and the means algebra which live in `model$MZ`
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmuTwinMeans_MZDZ(defVars = c("a", "b"))
#'
xmuTwinMeans_MZDZ <- function(defVars = NULL, sep="_T", expMeanAlgName = "expMean") {
	# top.Mean top.betaDef Def_T1 top.Mean top.betaDef Def_T2 need to be added to 
	nVar = length(defVars)
	T1defLabels = paste0("data.", defVars, sep, 1)
	T2defLabels = paste0("data.", defVars, sep, 2)
	a = mxMatrix(name="Def_T1", type="Full", nrow=1, ncol=nVar, free=FALSE, labels=T1defLabels)
	b = mxMatrix(name="Def_T2", type="Full", nrow=1, ncol=nVar, free=FALSE, labels=T2defLabels)
	c = mxAlgebra(name=expMeanAlgName, cbind(top.Mean + top.betaDef*Def_T1, top.Mean + top.betaDef*Def_T2))
	return(list(a, b, c))
}

#' Not for end-users: Create matrices "Mean" and "betaDef" in twin models
#'
#' @description
#' Returns two matrices "Mean" and "betaDef" which live in `model$top`
#'
#' @param nVar how many variables you have
#' @param defVars the names of definition variables
#' @param sep The twin number separator.
#' @return - two matrices "Mean" and "betaDef"
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmuTwinMeanModelParts_top(nVar=3, defVars = c("a", "b"), sep="_T")
#'
xmuTwinMeanModelParts_top <- function(nVar, defVars = NULL, sep="_T") {
	betaLabels = paste0("beta_", defVars, (1:length(defVars)))
	a = mxMatrix(name="Mean"   , type="Zero", nrow=1, ncol=nVar)
	b = mxMatrix(name="betaDef", type="Full", nrow=1, ncol=nVar, free=TRUE, labels= betaLabels, values = 0, lbound=-2,ubound=2)
	return(list(a, b))
}


#' Helper providing boilerplate start values for means and variance in twin models 
#'
#' @description
#' `xmu_starts` can handle several common/boilerplate situations in which means and variance start values
#' are used in twin models.
#'
#' @param mzData Data for MZ pairs.
#' @param dzData Data for DZ pairs.
#' @param selVars Variable names: If sep = NULL, then treated as full names for both sibs.
#' @param sep All the variables full names.
#' @param equateMeans (NULL)
#' @param nSib How many subjects in a family.
#' @param varForm currently just "Cholesky" style.
#' @param SD = TRUE (FALSE = variance, not SD).
#' @param divideBy = 3 (A,C,E) 1/3rd each. Use 1 to do this yourself post-hoc.
#' @return - varStarts and meanStarts
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' data(twinData)
#' selDVs = c("wt", "ht")
#' mzData = twinData[twinData$zygosity %in%  "MZFF", ] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF", ]
#' 
#' round(sqrt(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3),3)
#' 
#' tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky")
#' tmp
#' 
#' round(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3,3)
#' tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#' 
#' tmp
#' 
#' # one variable
#' tmp = xmu_starts(mzData, dzData, selVars = "wt", sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#' 
#' # Ordinal/continuous mix
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
#' # Cut BMI column to form ordinal obesity variables
#' obLevels = c('normal', 'overweight', 'obese')
#' cuts     = quantile(twinData[, "bmi1"], probs = c(.5, .8), na.rm = TRUE)
#' twinData$obese1= cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' twinData$obese2= cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' # Make the ordinal variables into mxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' tmp = xmu_starts(mzData, dzData, selVars = c("wt","obese"), sep= "", 
#'	 nSib= 2, equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#' 
#' tmp = xmu_starts(mxData(mzData, type="raw"), mxData(mzData, type="raw"), 
#'    selVars = c("wt","obese"), sep= "", nSib= 2, equateMeans = TRUE, 
#'	  varForm = "Cholesky", SD= FALSE)
#'
xmu_starts <- function(mzData, dzData, selVars = selVars, sep = NULL, equateMeans= NULL, nSib = 2, varForm = c("Cholesky"), SD= TRUE, divideBy = 3) {
	# Make mxData, dropping any unused columns
	if(!is.null(sep)){
		# sep = ""; nSib = 2; selVars = c("wt", "ht")
		selVars = umx_paste_names(selVars, sep = sep, suffixes = 1:nSib)
	}
	nVar = length(selVars)/nSib
	dataType = umx_is_cov(dzData, boolean = FALSE)
	if(dataType == "raw") {
		if(is.null(equateMeans)){
			stop("you have to tell xmu_starts whether to equate the means or not")
		}
		if(umx_is_MxData(mzData)){
			allData = rbind(mzData$observed, dzData$observed)[,selVars]
		}else{
			allData = rbind(mzData, dzData)[,selVars]
		}

		T1 = allData[, 1:nVar, drop = FALSE]
		T2 = allData[, (nVar+1):(nVar*2), drop = FALSE];
		names(T2) = names(T1)
		longData = rbind(T1, T2)[, selVars[1:nVar], drop = FALSE]
		# Mean starts (used across all raw solutions
		meanStarts = umx_means(longData, ordVar = 0, na.rm = TRUE)

		# Make wide again
		meanStarts = c(meanStarts, meanStarts)
		if(equateMeans){
			meanLabels = paste0("expMean_", selVars[1:nVar]) # Names recycled for twin 2
			meanLabels = c(meanLabels, meanLabels)
		} else {
			meanLabels = paste0("expMean_", selVars)
		}
		varStarts = umx_var(longData, format= "diag", ordVar = 1, use = "pairwise.complete.obs", strict = FALSE)
	} else if(dataType %in% c("cov", "cor")){
		meanStarts = NA # Not used for summary data
		meanLabels = NA
		if(umx_is_MxData(mzData)){
			het_mz = umx_reorder(mzData$observed, selVars)		
			het_dz = umx_reorder(dzData$observed, selVars)
		}else{
			het_mz = umx_reorder(mzData, selVars)		
			het_dz = umx_reorder(dzData, selVars)
		}
		varStarts = (diag(het_mz)[1:nVar]+ diag(het_dz)[1:nVar])/2
	}else{
		stop("xmu_starts can only handle raw and cov/cor data types. You gave me ", omxQuotes(dataType))
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

#' Get on or more columns from mzData or regular data.frame
#'
#' @description
#' same effect as `df[, col]` but works for [mxData()] and check the names are present
#'
#' @param data mxData or data.frame
#' @param col the name(s) of the column(s) to extract
#' @param drop whether to drop the structure of the data.frame when extracting one column
#' @return - column of data
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmu_extract_column(mtcars, "wt")
#' xmu_extract_column(mxData(mtcars, type = "raw"), "wt")
#' xmu_extract_column(mxData(mtcars, type = "raw"), "wt", drop=TRUE)
#' xmu_extract_column(mxData(mtcars, type = "raw"), c("wt", "mpg"))
xmu_extract_column <- function(data, col, drop= FALSE) {
	umx_check_names(col, data)
	if(umx_is_MxData(data)){
		return(data$observed[ , col, drop = drop])
	}else{
		return(data[ , col, drop = drop])
	}
}

#' Add weight matrices to twin models.
#'
#' @description
#' Add weight models (MZw, DZw) with matrices (e.g. mzWeightMatrix) to a twin model, and 
#' update `mxFitFunctionMultigroup`. This yileds a weighted model with vector objective.
#' 
#' To weight objective functions in OpenMx, you specify a container model that applies the weights
#' m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
#' This option makes FIML return individual likelihoods for each row of the data (rather than a single 
#' -2LL value for the model)
#' You then optimize weighted versions of these likelihoods by building additional models containing 
#' weight data and an algebra that multiplies the likelihoods from the first model by the weight vector.
#' 
#' @param model umx-style twin model
#' @param mzWeights data for MZ weights matrix
#' @param dzWeights data for DZ weights matrix
#' @return - model
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' tmp = umx_make_twin_data_nice(data=twinData, sep="", zygosity="zygosity", numbering= 1:2)
#' m1  = umxACE(selDVs = "wt", data = tmp, dzData = "DZFF", mzData = "MZFF", autoRun= FALSE)
#' m1$MZ$fitfunction$vector= TRUE
#' 
#' tmp = xmu_twin_add_WeightMatrices(m1,
#'		mzWeights= rnorm(nrow(m1$MZ$data$observed)), 
#'		dzWeights= rnorm(nrow(m1$DZ$data$observed))
#'  )
#' 
xmu_twin_add_WeightMatrices <- function(model, mzWeights = NULL, dzWeights = NULL) {

	umx_check(model$MZ$fitfunction$vector, "stop", "xmu_twin_add_WeightMatrix: You need to set the fitFunction to vector mode... but it appears I haven't")
	
	if(umx_is_MxMatrix(mzWeights)){
		# just check the name
		if(mzWeights$name != "mzWeightMatrix"){ stop(
			paste0("xmu_twin_add_WeightMatrices expects name of mzWeights to be 'mzWeightMatrix'.\n", "It was ", omxQuotes(mzWeights$name))
		)}
	} else {
		# make incoming raw data into matrices
		# convert from dataframe if necessary
		if(!is.null(dim(mzWeights)) ){ mzWeights = mzWeights[,1] }
		if(!is.null(dim(dzWeights)) ){ dzWeights = dzWeights[,1] }
		mzWeights = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = length(mzWeights), ncol = 1, free = FALSE, values = mzWeights)
		dzWeights = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = length(dzWeights), ncol = 1, free = FALSE, values = dzWeights)
	}

	model = mxModel(model,
		mxModel("MZw", mzWeights,
			mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
			mxFitFunctionAlgebra("mzWeightedCov")
		),
		mxModel("DZw", dzWeights,
			mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
			mxFitFunctionAlgebra("dzWeightedCov")
		),
		mxFitFunctionMultigroup(c("MZw", "DZw"))
	)
	return(model)
}

