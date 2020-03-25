#' Add a means model to a twin model
#'
#' @description
#' Add simple or definition based means model to a twin model (i.e., which contains top MZ and DZ models)
#'
#' @param model The model we are modifying (myst have MZ DZ and top submodels)
#' @param defVars the names of definition variables
#' @param sep How twin variable names are expanded (default "_T")
#' @return - model with means model added.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [xmuDefBetas()], [xmuDefMean()]
#' @md
#' @examples
#' \dontrun{
#' defVars = c("VETSA1_age","VETSA2_age", "VETSA3_age")
#' umxAddMeansModel(m1, defVars = defVars)
#' xmuDefBetas(defVars = defVars)
#' xmuDefMean(defVars = defVars, expMeanAlgName = "expMean")
#' m1 = umxTwinAddMeansModel(m1, defVars = c("a", "b"))
#' }
#'
umxTwinAddMeansModel <- function(model, defVars = NULL, sep = "_T"){
	# need to check the def vars are still in the dataset at this point...?
	umx_check(all(c("MZ", "DZ", "top") %in% names(model)), message="need a model with top, MZ and DZ submodels")	
	if(!is.null(defVars)){
		# 1. Stick means and betas into top and Stick alg and def matrices into MZ and DZ
		newTop = mxModel(model$top, xmuDefBetas(defVars = defVars, sep= sep))
		newMZ  = mxModel(model$MZ , xmuDefMean(defVars = defVars, sep=sep))
		newDZ  = mxModel(model$DZ , xmuDefMean(defVars = defVars, sep=sep))
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
#' @seealso - [xmuDefBetas()], [umxTwinAddMeansModel()]
#' @md
#' @examples
#' xmuDefMean(defVars = c("a", "b"))
#'
xmuDefMean <- function(defVars = NULL, sep="_T", expMeanAlgName = "expMean") {
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
#' @return - two matrices "Mean" and "betaDef"
#' @export
#' @family xmu internal not for end user
#' @seealso - [xmuDefMean()], [umxTwinAddMeansModel()]
#' @md
#' @examples
#' xmuDefBetas(nVar=3, defVars = c("a", "b"), sep="_T")
#'
xmuDefBetas <- function(nVar, defVars = NULL, sep="_T") {
	betaLabels = paste0("beta_", defVars, (1:length(defVars)))
	a = mxMatrix(name="Mean"   , type="Zero", nrow=1, ncol=nVar)
	b = mxMatrix(name="betaDef", type="Full", nrow=1, ncol=nVar, free=TRUE, labels= betaLabels, values = 0, lbound=-2,ubound=2)
	return(list(a, b))
}