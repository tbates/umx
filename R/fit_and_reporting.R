#   Copyright 2007-2020 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# =====================
# = Model Diagnostics =
# =====================

#' Diagnose problems in a model - this is a work in progress.
#'
#' The goal of this function **WILL BE** (not currently functional) to diagnose problems in
#' a model and return suggestions to the user.
#' It is a work in progress, and of no use as yet.
#'
#' Best diagnostics are:
#' 
#' 1. Observed data variances and means
#' 2. Expected variances and means
#' 3. Difference of these?
#' 	
#' Try
#' 	* diagonalizeExpCov diagonal
#' 	* [umx_is_ordered()]
#'
#' 	more tricky - we should really report the variances and the standardized thresholds.
#' The guidance would be to try starting with unit variances and thresholds that are within
#'  +/- 2 SD of the mean. [bivariate outliers %p option](https://openmx.ssri.psu.edu/thread/3899)
#' @param model an [mxModel()] to diagnose
#' @param tryHard whether I should try and fix it? (defaults to FALSE)
#' @param diagonalizeExpCov Whether to diagonalize the ExpCov
#' @return - helpful messages and perhaps a modified model
#' @export
#' @family Teaching and Testing functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' 
#' m1 = umxRAM("OneFactor", data = demoOneFactor, type= "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m1 = mxRun(m1)
#' umxSummary(m1, std = TRUE)
#' umxDiagnose(m1)
umxDiagnose <- function(model, tryHard = FALSE, diagonalizeExpCov = FALSE){
	# 1. First thing to check is whether the covariance matrix is positive definite.
	minEigen = min(eigen(umxExpCov(model))$values)
	if(minEigen<0){
		message("The expected covariance matrix is not positive definite")
		# now what?
	}
}

# =============================
# = Fit and Reporting Helpers =
# =============================

#' AIC weight-based conditional probabilities.
#'
#' @description
#' Returns the best model by AIC, and computes the probabilities 
#' according to AIC weight-based conditional probabilities (Wagenmakers & Farrell, 2004). 
#'
#' @param models a list of models to compare.
#' @param digits (default 2)
#' @return - Best model
#' @export
#' @family Miscellaneous Stats Functions
#' @seealso - [AIC()]
#' @references - Wagenmakers E.J., Farrell S. (2004), 192-196. AIC model selection using Akaike weights. *Psychonomic Bulletin and Review*. **11**, 192-196. \url{https://pubmed.ncbi.nlm.nih.gov/15117008/}
#' @md
#' @examples
#' l1 = lm(mpg~ wt + disp, data=mtcars)
#' l2 = lm(mpg~ wt, data=mtcars)
#' umxWeightedAIC(models = list(l1, l2))
umxWeightedAIC <- function(models, digits= 2) {
	if(class(models[[1]])== "numeric"){
		stop("Please input the list of models to compare as a list, i.e. models = list(model1, model2)")
	}
	AIClist = c()
	for (i in models) {
		AIClist = c(AIClist, AIC(i))
	}
	whichBest = which.min(AIClist)
	bestModel = models[[whichBest]]
	aic.weights = round(MuMIn::Weights(AIClist), 2)
	if(isS4(models[[1]]) & is(models[[1]], "MxModel")){
		message("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
		# Probabilities according to AIC Weights (Wagenmakers et al https://pubmed.ncbi.nlm.nih.gov/15117008/ )
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model for ", 
			omxQuotes(namez(models)), " respectively are: ",
			omxQuotes(aic.weights), " Using MuMIn::Weights(AIC()).")		
	}else{
		if("call" %in% names(bestModel)){
			# ID = paste0("Model ", omxQuotes(bestModel$call))
			ID = paste0("Model ", whichBest)
		} else {
			ID = paste0("Model ", whichBest)
		}
		message(ID, " is the best fitting model according to AIC.")
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model are (for each model you gave me): ",
			omxQuotes(aic.weights), " Using MuMIn::Weights(AIC()).")		
		
	}
	invisible(bestModel)
}

#' Reduce models, and report the results.
#'
#' @description
#' Given a `umx` model (currently `umxACE` and `umxGxE` are supported - ask for more!)
#' `umxReduce` will conduct a formalised reduction process. It will also report
#' Akaike weights are also reported showing relative support across models.
#'
#' Specialized functions are called for different type of input:
#' 1. **GxE model reduction** For [umxGxE()] models [umxReduceGxE()] is called.
#' 2. **ACE model reduction** For [umxACE()] models,[umxReduceACE()] is called.
#' 
#' `umxReduce` reports the results in a table. Set the format of the table with
#' [umx_set_table_format()], or set `report= "html"` to open a
#' table for pasting into a word processor.
#' 
#' `umxReduce` is a work in progress, with more automatic reductions coming as demand emerges.
#' I am thinking for RAM models to drop NS paths, and report that test.
#'
#' @param model The [mxModel()] which will be reduced.
#' @param report How to report the results. "html" = open in browser
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param baseFileName (optional) custom filename for html output (defaults to "tmp")
#' @param tryHard Default = "yes"
#' @param silent Default = FALSE
#' @param ... Other parameters to control model summary
#' @family Model Summary and Comparison
#' @family Twin Modeling Functions
#' @seealso [umxReduceGxE()], [umxReduceACE()]
#' @references - Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights.
#'  *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}
#' @export
#' @md
umxReduce <- function(model, report = c("markdown", "inline", "html"), intervals = TRUE, baseFileName = "tmp", tryHard = "yes", silent=FALSE, ...){
	UseMethod("umxReduce", model)
}

#' @export
umxReduce.default <- function(model, report = c("markdown", "inline", "html"), intervals = FALSE, baseFileName = "tmp", tryHard = "yes", silent=FALSE, ...){
	stop("umxReduce is not defined for objects of class:", class(model))
}

#' Reduce a GxE model.
#'
#' @description
#' This function can perform model reduction for [umxGxE()] models, 
#' testing dropping a`,c` & e`, as well as c & c`, a & a` etc.
#'
#' It reports the results in a table. Set the format of the table with
#' [umx_set_table_format()]. Or set `report = "html"` to open a
#' table for pasting into a word processor.
#' 
#' In addition to printing a table, the function returns the preferred model.
#' 
#' @param model A [umxGxE()] to reduce.
#' @param report How to report the results. default = "markdown". "html" = open in browser.
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param baseFileName (optional) custom filename for html output (default = "tmp").
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param silent Default (FALSE)
#' @param ... Other parameters to control model summary.
#' @return best model
#' @export
#' @family Twin Modeling Functions
#' @seealso [umxReduce()], [umxReduceACE()]
#' @references - Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights.
#' *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}.
#' @md
#' @examples
#' \dontrun{
#' model = umxReduce(model)
#' }
umxReduceGxE <- function(model, report = c("markdown", "inline", "html", "report"), intervals = TRUE, baseFileName = "tmp_gxe", tryHard = c("yes", "no", "ordinal", "search"), silent = FALSE, ...) {
	report = match.arg(report)
	umx_is_MxModel(model)
	if(class(model) == "MxModelGxE"){		
		# Reduce GxE Model

		noAmod = umxModify(model, update = "am_r1c1", name = "No_mod_on_A", tryHard= tryHard)
		noCmod = umxModify(model, update = "cm_r1c1", name = "No_mod_on_C", tryHard= tryHard)
		noEmod = umxModify(model, update = "em_r1c1", name = "No_mod_on_E", tryHard= tryHard)

		noACEmod     = umxModify(model, regex  = "[ace]m_r1c1" , name = "No_moderation", tryHard= tryHard)

		no_a_no_am  = umxModify(noAmod     , update = "a_r1c1" , name = "No_A_no_mod_on_A"  , tryHard= tryHard)
		no_c_no_cm  = umxModify(noCmod     , update = "c_r1c1" , name = "No_C_no_mod_on_C"  , tryHard= tryHard)
		no_c_no_cem = umxModify(no_c_no_cm , update = "em_r1c1", name = "No_c_no_ce_mod"    , tryHard= tryHard)
		no_c_no_mod = umxModify(no_c_no_cem, update = "am_r1c1", name = "No_c_no_moderation", tryHard= tryHard)

		# if("testFor quad11 in parameters"){
		# 	no_sq_mean  = umxModify(noACEmod, update = "quad11" , name = "No_mod_no_quad_mean", tryHard= tryHard)
		# 	no_lin_mean = umxModify(noACEmod, update = "lin11"  , name = "No_mod_no_lin_mean" , tryHard= tryHard)
		# 	nomeans     = umxModify(noACEmod, regex = "lin|quad", name = "No_mod_no_means_mod", tryHard= tryHard)
		# } else {
		# 	# likely has non-equal moderators don't bother trying to drop
		# 	# betaSelf and betaCoTwin they are almost certainly necessary
		# }

		comparisons = c(
			noAmod, noCmod, noEmod, noACEmod,
			no_a_no_am, no_c_no_cm, no_c_no_cem,
			no_c_no_mod
		)

		# ====================
		# = everything table =
		# ====================
		
		umxCompare(model, comparisons, all = TRUE, report = report, file = paste0(baseFileName, "1.html"))
		# umxCompare(no_c_no_cem, no_c_no_moderation, all = TRUE, report = report, file = paste0(baseFileName, "2.html"))
		modelList = c(model, comparisons)
		
		# get list of AICs
		AIClist = c()
		for (i in modelList) {
			AIClist = c(AIClist, AIC(i))
		}
		whichBest = which.min(AIClist)
		bestModel = modelList[[whichBest]]
		message("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
		# Probabilities according to AIC MuMIn::Weights (Wagenmakers et al https://pubmed.ncbi.nlm.nih.gov/15117008/ )
		aic.weights = round(Weights(AIClist), 2)
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model for ", 
			omxQuotes(namez(modelList)), " respectively are: ",
			omxQuotes(aic.weights), " Using MuMIn::Weights(AIC())."
		)
		if(intervals){
			bestModel = mxRun(bestModel, intervals = intervals)
		}
		invisible(bestModel)
	} else {
		stop("This function is for GxE. Feel free to let me know what you want...")
	}
}
#' @export
umxReduce.MxModelGxE <- umxReduceGxE

#' Reduce an ACE model.
#'
#' This function can perform model reduction on [umxACE()] models,
#' testing dropping A and C, as well as an ADE or ACE model, displaying the results
#' in a table, and returning the best model.
#'
#' It is designed for testing univariate models. You can offer up either the ACE or ADE base model.
#'
#' Suggestions for more sophisticated automation welcomed!
#'
#' @param model an ACE or ADE [mxModel()] to reduce
#' @param report How to report the results. "html" = open in browser
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param baseFileName (optional) custom filename for html output (defaults to "tmp")
#' @param tryHard (default = "yes")
#' @param silent Don't print the ACE models (default = FALSE)
#' @param digits rounding in printout (default = 2)
#' @param ... Other parameters to control model summary
#' @return Best fitting model
#' @export
#' @family Twin Modeling Functions
#' @seealso [umxReduceGxE()], [umxReduce()]
#' @references - Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights. *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}
#' @md
#' @examples
#' \dontrun{
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#'
#' # ===========================================================================
#' # = Table of parameters + fit comparisons, ready too copy to word processor =
#' # ===========================================================================
#' umxReduce(m1, silent=TRUE, digits=2, repo="h")
#'
#' # ==========================================
#' # = Function captures the preferred model =
#' # ==========================================
#' m2 = umxReduce(m1)
#' umxSummary(m2)
#' 
#' # works for ADE input also
#' m1 = umxACE(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "", dzCr = .25)
#' 
#' }
umxReduceACE <- function(model, report = c("markdown", "inline", "html", "report"), intervals = TRUE, baseFileName = "tmp", tryHard = c("yes", "no", "ordinal", "search"), silent=FALSE, digits = 2, ...) {
	report  = match.arg(report)
	tryHard = match.arg(tryHard)
	if(silent){
		oldSilent = umx_set_silent(TRUE)
	}else{
		oldSilent = FALSE
	}
	oldAutoPlot = umx_set_auto_plot(FALSE, silent = TRUE)
	if(model$top$dzCr$values == 1){
		message("You gave me an ACE model")		
		ACE = model
		ADE = umxModify(model, 'dzCr_r1c1', value = .25, name = "ADE", tryHard = tryHard)
		if(-2*logLik(ACE) > -2*logLik(ADE)){
			CE = umxModify(ADE, regex = "a_r[0-9]+c[0-9]+" , name = "DE", tryHard = tryHard)
			AE = umxModify(ADE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
			 E = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
			message("A dominance model is preferred, set dzCr = 0.25")
		}else{
			CE = umxModify(ACE, regex = "a_r[0-9]+c[0-9]+" , name = "CE", tryHard = tryHard)
			AE = umxModify(ACE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
			 E = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
		}
	}else if(model$top$dzCr$values == .25){
		if(model$name=="ACE"){
			message("You gave me an ADE model, but it was called 'ACE'. I have renamed it ADE for the purposes of clarity in model reduction.")
			model = mxRename(model, newname = "ADE", oldname = "ACE")
		} else {
			message("You gave me an ADE model.")
		}
		ADE = model
		ACE = umxModify(ADE, 'dzCr_r1c1', value = 1, name = "ACE", tryHard = tryHard)
		AE  = umxModify(ADE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
		 E  = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
		if(-2*logLik(ADE) > -2*logLik(ACE)){
			CE = umxModify(ACE, regex = "a_r[0-9]+c[0-9]+" , name = "CE", tryHard=tryHard)
			message("An ACE model is preferred, set dzCr = 1.0")
		}else{
			CE = umxModify(ADE, regex = "a_r[0-9]+c[0-9]+" , name = "DE", tryHard=tryHard)
		}
	}else{
		stop(model$top$dzCr$values, " is an odd number for dzCr, isn't it? I was expecting 1 (C) or .25 (D)",
		"\nPerhaps you're a John Loehlin (requiescat in pace :-( ) fan, and are doing an assortative mating test? e-mail me to get this added here.")
		# TODO umxReduceACE handle odd values of dzCr as assortative mating etc.?
		bestModel = model
	}
	# = Show fit table =
	tmp = data.frame(matrix(nrow=5,ncol=4))
	names(tmp) = c("a"              , "c"                 , "e"                 , "d")
	tmp[1,] = c(ACE$top$a_std$result, ACE$top$c_std$result, ACE$top$e_std$result, NA)
	tmp[2,] = c(ADE$top$a_std$result, NA                  , ADE$top$e_std$result, ADE$top$c_std$result)
	tmp[3,] = c( NA                 ,  CE$top$c_std$result,  CE$top$e_std$result, NA)
	tmp[4,] = c( AE$top$a_std$result, NA                  ,  AE$top$e_std$result, NA)
	tmp[5,] = c( NA                 , NA                  ,   E$top$e_std$result, NA)

	biggles = umxCompare(ACE, c(ADE, CE, AE, E), all = TRUE, report = report, silent=TRUE)
	tmp2 = cbind(biggles[, 1, drop = FALSE], tmp, biggles[, 2:dim(biggles)[2] ] )
	umx_print(tmp2, digits = digits, report = report)
	
	whichBest = which.min(AIC(ACE, ADE, CE, AE)[,"AIC"])[1]
	bestModel = list(ACE, ADE, CE, AE)[[whichBest]]
	message("Among ACE, ADE, CE, and AE models ", omxQuotes(bestModel$name), " fit best according to AIC.")
	# Probabilities according to AIC MuMIn::Weights (Wagenmakers et al https://pubmed.ncbi.nlm.nih.gov/15117008/ )
	aic.weights = round(Weights(AIC(ACE, ADE, CE, AE)[,"AIC"]), 2)
	aic.names   = namez(c(ACE, ADE, CE, AE))
	message("Conditional AIC probability {Wagenmakers, 2004, 192-196}  indicates relative model support as", 
		omxQuotes(aic.names), " respectively are: ", 
		omxQuotes(aic.weights), " Using MuMIn::Weights(AIC()).")
		message(paste0(aic.names," (", aic.weights, "%)"))
	if(intervals){
		bestModel = mxRun(bestModel, intervals = intervals)
	}
	umx_set_auto_plot(oldAutoPlot, silent = TRUE)
	umx_set_silent(oldSilent)
	invisible(bestModel)
}
#' @export
umxReduce.MxModelACE <- umxReduceACE

#' Get residuals from an MxModel
#'
#' Return the [residuals()] from an OpenMx RAM model. You can format these (with digits), and suppress small values.
#'
#' @rdname residuals.MxModel
#' @param object An fitted [mxModel()] from which to get residuals
#' @param digits round to how many digits (default = 2)
#' @param suppress smallest deviation to print out (default = NULL = show all)
#' @param reorder optionally reorder the variables in the residuals matrix to show patterns
#' @param ... Optional parameters
#' @return - matrix of residuals
#' @export
#' @family Reporting functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#'
#'# ===================================
#'# = Show the residuals of the model =
#'# ===================================
#' residuals(m1)
#' # |   |x1   |x2    |x3   |x4    |x5 |
#' # |:--|:----|:-----|:----|:-----|:--|
#' # |x1 |.    |.     |0.01 |.     |.  |
#' # |x2 |.    |.     |0.01 |-0.01 |.  |
#' # |x3 |0.01 |0.01  |.    |.     |.  |
#' # |x4 |.    |-0.01 |.    |.     |.  |
#' # |x5 |.    |.     |.    |.     |.  |
#' # [1] "nb: You can zoom in on bad values with, e.g. suppress = .01, which
#' #      will hide values smaller than this. Use digits = to round"
#'
#' residuals(m1, digits = 3)
#' residuals(m1, digits = 3, suppress = .005)
#' # residuals are returned as an invisible object you can capture in a variable
#' a = residuals(m1); a
residuals.MxModel <- function(object, digits = 2, suppress = NULL, reorder=NULL, ...){
	umx_check_model(object, type = NULL, hasData = TRUE)
	expCov = umxExpCov(object, latents = FALSE)
	if(object$data$type == "raw"){
		obsCov = umxHetCor(object$data$observed)
	} else {
		obsCov = object$data$observed
	}
	resid = cov2cor(obsCov) - cov2cor(expCov)
	if(!is.null(reorder)){
		resid = umx_reorder(resid, newOrder = reorder, force = TRUE)
	}
	umx_print(data.frame(resid), digits = digits, zero.print = ".", suppress = suppress)
	if(is.null(suppress)){
		print("nb: Zoom in on bad values with, e.g. suppress = .1, which will hide smaller values. Use digits = to round")
	}
	invisible(resid)
}

# define generic loadings...
#' loadings
#' Generic loadings function to extract factor loadings from exploratory or confirmatory
#' factor analyses.
#'
#' See \code{\link[umx]{loadings.MxModel}} to access the loadings of OpenMx EFA models.
#' 
#' Base \code{\link[stats]{loadings}} handles [factanal()] objects. 
#'
#' @param x an object from which to get loadings 
#' @param ... additional parameters
#' @return - matrix of loadings
#' @export
#' @family Reporting functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
loadings <- function(x, ...) UseMethod("loadings")
#' @export
loadings.default <- function(x, ...) stats::loadings(x, ...) 

# TODO: alternative approach would be to use setGeneric("loadings")

#' Extract factor loadings from an EFA (factor analysis).
#'
#' loadings extracts the factor loadings from an EFA (factor analysis) model.
#' It behaves equivalently to stats::loadings, returning the loadings from an 
#' EFA (factor analysis). However it does not store the rotation matrix.
#'
#' @param x A RAM model from which to get loadings.
#' @param ... Other parameters (currently unused)
#' @return - loadings matrix
#' @export
#' @family Miscellaneous Functions
#' @seealso - [factanal()], [loadings()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' myVars = c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(name = "test", factors = 2, data = mtcars[, myVars])
#' loadings(m1)
#'
loadings.MxModel <- function(x, ...) {
	x$A$values[x@manifestVars, x@latentVars, drop = FALSE]
}


#' Get confidence intervals from a umx model
#'
#' Implements confidence interval function for umx models.
#' 
#' Note: By default, requesting new CIs wipes the existing ones.
#' To keep these, set wipeExistingRequests = FALSE.
#' 
#' Because CIs can take time to run, by default only already-computed CIs will be reported. To run new CIs, set run = TRUE .
#'
#' @details *Note*: [confint()] is an OpenMx function which will return SE-based CIs.
#' 
#' If `parm` is empty, and `run = FALSE`, a message will alert you to set `run = TRUE`. 
#'
#' @param object An [mxModel()], possibly already containing [mxCI()]s that have been [mxRun()] with intervals = TRUE))
#' @param parm	Which parameters to get confidence intervals for. Can be "existing", "all", or one or more parameter names.
#' @param level The confidence level required (default = .95)
#' @param run Whether to run the model (defaults to FALSE)
#' @param wipeExistingRequests Whether to remove existing CIs when adding new ones (ignored if parm = 'existing').
#' @param optimizer For difficult CIs, trying other optimizers can help!
#' @param showErrorCodes (default = FALSE)
#' @export
#' @return - [mxModel()]
#' @family Reporting functions
#' @seealso - [stats::confint()], [OpenMx::mxSE()], [umxCI()], [OpenMx::mxCI()]
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' 
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
#' 	umxPath(from = "G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' 
#' m1 = umxConfint(m1, run = TRUE) # There are no existing CI requests...
#' 
#' \dontrun{
#' # Add a CI request for "G_to_x1", run, and report. Save with this CI computed
#' m2 = umxConfint(m1, parm = "G_to_x1", run = TRUE) 
#' 
#' # Just print out any existing CIs
#' umxConfint(m2)
#' 
#' # CI requests added for free matrix parameters. User prompted to set run = TRUE
#' m3 = umxConfint(m1, "all")
#' 
#' # Run the requested CIs
#' m3 = umxConfint(m3, run = TRUE) 
#' 
#' # Run CIs for free one-headed (asymmetric) paths in RAM model. 
#' #   note: Deletes other existing requests,
#' tmp = umxConfint(m1, parm = "A", run = TRUE)
#' 
#' # Wipe existing CIs, add G_to_x1
#' tmp = umxConfint(m1, parm = "G_to_x1", run = TRUE, wipeExistingRequests = TRUE) 
#' 
#' # For some twin models, a "smart" mode is implemented
#' # note: only implemented for umxCP so far
#' m2 =  umxConfint(m1, "smart")
#' }
#'
umxConfint <- function(object, parm = c("existing", "all", "or one or more labels", "smart"), wipeExistingRequests = TRUE, level = 0.95, run = FALSE, showErrorCodes = FALSE, optimizer= c("SLSQP", "NPSOL", "CSOLNP", "current")) {
	optimizer = match.arg(optimizer)
	if(optimizer == "current"){
		# optimizer is sent to omxRunCI so set "current" to the actual name
		optimizer = umx_set_optimizer(silent = TRUE)
	}
	parm = xmu_match.arg(parm, c("existing", "all", "or one or more labels", "smart"), check = FALSE)

	# Upgrade "all" to "smart" for CP
	if(class(object)[[1]] == "MxModelCP" && parm == "all"){
		parm = "smart"
	}

	# 1. remove existing CIs if requested to
	if(wipeExistingRequests && (parm != "existing")){
		if(length(object$intervals)){
			object = mxModel(object, remove = TRUE, object$intervals)
			message("Removed existing CIs")
		}
	}
	
	# 2. Add CIs if requested
	if (length(parm) >1){
		# Add requested CIs to model
		# TODO umxConfint: Check that these are valid and not duplicates
		object = mxModel(object, mxCI(parm, interval = level))
	} else if (parm == "all") {
		CIs_to_set = names(omxGetParameters(object, free = TRUE))
		object = mxModel(object, mxCI(CIs_to_set, interval = level))
	} else if (parm == "smart"){
		if(class(object)[[1]] == "MxModelCP"){
			# Add individual smart (only free cell) mxCI requests
			# For CP model, these are the free cells in
			# 	top.as_std, top.cs_std, top.es_std
			# object = m1
			this = object$top$as$free
			template = umxMatrix("A", "Full", dim(this)[1], dim(this)[2])$labels
			patt = "^.*_r([0-9]+)c([0-9]+)$"
			as_free = gsub(pattern = patt, replacement= "top.as_std[\\1,\\2]", template)[which(object$top$as$free)]
			cs_free = gsub(pattern = patt, replacement= "top.cs_std[\\1,\\2]", template)[which(object$top$cs$free)]
			es_free = gsub(pattern = patt, replacement= "top.es_std[\\1,\\2]", template)[which(object$top$es$free)]

			# Get labels for free cells in top.cp_loadings_std
			this = object$top$cp_loadings$free
			template = umxMatrix("A", "Full", dim(this)[1], dim(this)[2])$labels
			cp_loadings_free = gsub(pattern = patt, replacement= "top.cp_loadings_std[\\1,\\2]", template)[which(this)]

			# top.a_cp, top.c_cp, top.e_cp
			this = object$top$a_cp$free
			template  = umxMatrix("A", "Full", dim(this)[1], dim(this)[2])$labels
			a_cp_free = gsub(pattern = patt, replacement= "top.a_cp[\\1,\\2]", template)[which(object$top$a_cp$free)]
			c_cp_free = gsub(pattern = patt, replacement= "top.c_cp[\\1,\\2]", template)[which(object$top$c_cp$free)]
			e_cp_free = gsub(pattern = patt, replacement= "top.e_cp[\\1,\\2]", template)[which(object$top$e_cp$free)]

			CIs2Add = c(a_cp_free, c_cp_free, e_cp_free, cp_loadings_free, as_free, cs_free, es_free)
			object = mxModel(object, mxCI(CIs2Add, interval = level))
			message("added ", length(CIs2Add), " CIs")
		} else {
			stop("I only know how to add smart CIs for CP models so far. Sorry")
		}
	} else if (parm == "existing"){
		# nothing to do
	} else {
		# User requesting 1 new CI
		# TODO umxConfint: Check that these are valid and not duplicates
		object = mxModel(object, mxCI(parm, interval = level))
	}

	# 3. Run CIs if requested
	if(run) {
		# Check there are some in existence
		if(!umx_has_CIs(object, "intervals")) {
			message("Polite note: This model has no CIs yet. Perhaps you wanted to use parm = 'all' to request CIs on all free parameters? Or list some path labels?")
		}else{
			# object = mxRun(object, intervals = TRUE)
			object = omxRunCI(object, optimizer = optimizer)
		}
	}
	# 4. Report CIs
	if(!umx_has_CIs(object, "both")) {
		if(run == FALSE){
			message("Polite note: Some CIs have been requested but not run. Add ", omxQuotes("run = TRUE"), " to your umxConfint() call to run them.\n",
			"To store the model capture it from umxConfint like this:\n",
			"m1 = umxConfint(m1, run = TRUE)")
		} else if(length(object$intervals)==0){
			message("No CIs requested...")
		} else{
			message("Polite note: hmmm... You wanted it run, but I don't see any computed CIs despite there being ", length(object$intervals), " requested...",
			"\nThat's a bug. Please report it to timothy.c.bates@gmail.com")
		}
	} else {
		# model has CIs and they have been run
		# 1. Summarize model
		model_summary = summary(object, verbose = TRUE)
		# 2. Extract CIs and details, and arrange for merging
		CIdetail = model_summary$CIdetail
		CIdetail = CIdetail[, c("parameter", "value", "side", "diagnostic", "statusCode")]
		CIdetail$diagnostic = as.character(CIdetail$diagnostic)
		CIdetail$statusCode = as.character(CIdetail$statusCode)
		CIdetail$diagnostic = namez(CIdetail$diagnostic, pattern = "alpha level not reached"         , replacement = "alpha hi")
		CIdetail$statusCode = namez(CIdetail$statusCode, pattern = "infeasible non-linear constraint", replacement = "constrained")
		CIdetail$statusCode = namez(CIdetail$statusCode, pattern = "iteration limit/blue"            , replacement = "blue")

		CIs = model_summary$CI
		CIs$parameter = row.names(CIs)
		row.names(CIs) = NULL
		CIs = CIs[, c("parameter", "estimate", "lbound", "ubound", "note")]
		intersect(names(CIdetail), names(CIs))
		tmp = merge(CIs, CIdetail[CIdetail$side == "lower", ], by = "parameter", all.x = TRUE)
		tmp = merge(tmp, CIdetail[CIdetail$side == "upper", ], by = "parameter", all.x = TRUE, suffixes = c(".lower",".upper"))
		tmp$side.lower = NULL
		tmp$side.upper = NULL

		# 3. Format CIs
		model_CIs   = round(CIs[,c("lbound", "estimate", "ubound")], 3)
		model_CI_OK = object$output$confidenceIntervalCodes
		colnames(model_CI_OK) = c("lbound Code", "ubound Code")
		model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
		print(model_CIs)
		npsolMessages = list(
		'1' = 'The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN).',
		'2' = 'The linear constraints and bounds could not be satisfied. The problem has no feasible solution.',
		'3' = 'The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.',
		'4' = 'The major iteration limit was reached (Mx status BLUE).',
		'5' = 'not used',
		'6' = 'The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)',
		'7' = 'The function derivatives returned by funcon or funobj appear to be incorrect.',
		'8' = 'not used',
		'9' = 'An input parameter was invalid')
		if(!is.null(model_CI_OK) && any(model_CI_OK !=0) && showErrorCodes){
			codeList = c(model_CI_OK[,"lbound Code"], model_CI_OK[,"ubound Code"])
			relevantCodes = unique(codeList); relevantCodes = relevantCodes[relevantCodes !=0]
			for(i in relevantCodes) {
			   print(paste0(i, ": ", npsolMessages[i][[1]]))
			}
		}
	}
	invisible(object)
}

# 1776(liberty), 1789(liberty+terror), 1815 (liberty+inequality)

#' Add (and, optionally, run) confidence intervals to a structural model.
#'
#' `umxCI` adds [OpenMx::mxCI()] calls for requested (default all) parameters in a model, 
#' runs these CIs if necessary, and reports them in a neat summary.
#'
#' @details 
#' `umxCI` also reports if any problems were encountered. The codes are standard OpenMx errors and warnings
#' \itemize{
#' \item 1: The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN)
#' \item 2: The linear constraints and bounds could not be satisfied. The problem has no feasible solution.
#' \item 3: The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.
#' \item 4: The major iteration limit was reached (Mx status BLUE).
#' \item 6: The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)
#' \item 7: The function derivatives returned by funcon or funobj appear to be incorrect.
#' \item 9: An input parameter was invalid.
#' }
#' If `run = "no"`, the function simply adds the CI requests, but returns the model without running them.
#' 
#' @param model The [mxModel()] you wish to report [mxCI()]s on
#' @param which What CIs to add: c("ALL", NA, "list of your making")
#' @param remove = FALSE (if set, removes existing specified CIs from the model)
#' @param run Whether or not to compute the CIs. Valid values = "no" (default), "yes", "if necessary".
#' 'show' means print the intervals if computed, or list their names if not.
#' @param interval The interval for newly added CIs (defaults to 0.95)
#' @param type The type of CI (defaults to "both", options are "lower" and  "upper")
#' @param regex Add CIs for labels matching this regular expression (over-rides which)
#' @param showErrorCodes Whether to show errors (default == TRUE)
#' @return - [mxModel()]
#' @family Reporting functions
#' @seealso - [stats::confint()], [umxConfint()], [umxCI()], [umxModify()]
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m1$intervals # none yet - empty list()
#' m1 = umxCI(m1)
#' m1$intervals # $G_to_x1...
#' m1 = umxCI(m1, remove = TRUE) # remove CIs from the model and return it
#' m1$intervals # none again
#'
#' # Add CIs by name
#' parameters(m1, patt="_with_")
#' m1 = umxCI(m1, which = "x1_with_x1")
#' m1 = umxCI(m1, which = c("x1_with_x1", "x2_with_x2"))
#' m1 = umxCI(m1, regex = "x1_with_", run= "yes")
#' #           lbound estimate ubound lbound Code ubound Code
#' # x1_with_x1  0.036    0.041  0.047           0           0
#' 
#' # ========================
#' # = A twin model example =
#' # ========================
#' data(twinData) 
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = c("bmi1","bmi2"), dzData = dzData, mzData = mzData)
#' \dontrun{
#' umxCI(m1, run = "show") # show what will be requested
#' umxCI(m1, run = "yes") # actually compute the CIs
#' # Don't force update of CIs, but if they were just added, then calculate them
#' umxCI(m1, run = "if necessary")
#' m1 = umxCI(m1, remove = TRUE) # remove them all
#' m1$intervals # none!
#' # Show what parameters are available to get CIs on
#' umxParameters(m1) 
#' # Request a CI by label:
#' m1 = umxCI(m1, which = "a_r1c1", run = "yes")
#' }
umxCI <- function(model = NULL, which = c("ALL", NA, "list of your making"), remove = FALSE, run = c("no", "yes", "if necessary", "show"), interval = 0.95, type = c("both", "lower", "upper"), regex = NULL, showErrorCodes = TRUE) {
	# Note: OpenMx now overloads confint, returning SE-based intervals.
	run = match.arg(run)
	which = xmu_match.arg(which, c("ALL", NA, "list of your making"), check = FALSE)
	if(remove){
		if(!is.null(regex)){
			CIs = namez(model$intervals, pattern = regex)
		} else if(any(which == "ALL")){
			CIs = names(model$intervals)
		} else {
			CIs = which 
		}
		if(length(names(model$intervals)) > 0){
			model = mxModel(model, mxCI(CIs), remove = TRUE)
		} else {
			message("model has no intervals to remove")
		}
		invisible(model)
	} else {
		# Adding CIs
		# TODO Avoid duplicating existing CIs
		# TODO Add each CI individually
		# TODO Break them out into separate models and reassemble if on cluster?
		if(any(is.na(which)) && is.null(regex)){
			# nothing to add
		} else {
			if(!is.null(regex)){
				CIs = umxGetParameters(model, regex = regex, free = TRUE)
			} else if(any(which == "ALL")){
				CIs = names(omxGetParameters(model, free = TRUE))
			} else {
				CIs = which 
			}
			model = mxModel(model, mxCI(CIs, interval = interval, type = type))
		}
	}
	if(run == "yes" | (!umx_has_CIs(model) & run == "if necessary")) {
		model = mxRun(model, intervals = TRUE)
	} else {
		message("Not running CIs, run == ", run)
	}

	if(run == "show") {
		print("CI requests in the model:")
		print(names(model$intervals))
	}
	if(umx_has_CIs(model)){
		umxConfint(model, showErrorCodes = showErrorCodes)
		message("Table: Computed CIs in model ", model$name)
	}
	invisible(model)
}

#' Shows a compact, publication-style, summary of umx models
#'
#' @description
#' Report the fit of a OpenMx model or specialized model class (such as ACE, CP etc.)
#' in a compact form suitable for reporting in a journal.
#'
#' See documentation for RAM models summary here: [umxSummary.MxModel()].
#' 
#' View documentation on the ACE model subclass here: [umxSummaryACE()].
#' 
#' View documentation on the ACEv model subclass here: [umxSummaryACEv()].
#' 
#' View documentation on the IP model subclass here: [umxSummaryIP()].
#' 
#' View documentation on the CP model subclass here: [umxSummaryCP()].
#' 
#' View documentation on the GxE model subclass here: [umxSummaryGxE()].
#'
#' @param model The [mxModel()] whose fit will be reported
#' @param ... Other parameters to control model summary
#' @family Model Summary and Comparison
#' @md
#' @export
umxSummary <- function(model, ...){
	UseMethod("umxSummary", model)
}

#' @export
umxSummary.default <- function(model, ...){
	stop("umxSummary is not defined for objects of class:", class(model))
}

#' Shows a compact, publication-style, summary of a RAM model
#'
#' Report the fit of a model in a compact form suitable for a journal. 
#' It reports parameters in a markdown or html table (optionally standardized), and fit indices
#' RMSEA (an absolute fit index, comparing the model to a perfect model) and CFI and TLI (incremental fit indices comparing model a model with the worst fit).
#' 
#' `umxSummary` alerts you when model fit is worse than accepted criterion (TLI >= .95 and RMSEA <= .06; (Hu & Bentler, 1999; Yu, 2002).
#' 
#' Note: For some (multi-group) models, you will need to fall back on [summary()]
#' 
#' **CIs and Identification**
#' This function uses the standard errors reported by OpenMx to produce the CIs you see in umxSummary
#' These are used to derive confidence intervals based on the formula 95%CI = estimate +/- 1.96*SE)
#' 
#' Sometimes SEs appear NA. This may reflect a model which is not identified (see <http://davidakenny.net/cm/identify.htm>).
#' This can include empirical under-identification - for instance two factors
#' that are essentially identical in structure. use [mxCheckIdentification()] to check identification.
#' 
#' Solutions: If there are paths estimated at or close to zero suggests that fixing one or two of 
#' these to zero may fix the standard error calculation.
#' 
#' If factor loadings can flip sign and provide identical fit, this creates another form of 
#' under-identification and can break confidence interval estimation.
#' *Solution*: Fixing a factor loading to 1 and estimating factor variances can help here.
#'
#' @aliases umxSummary.MxModel umxSummary.MxRAMModel
#' @param model The [mxModel()] whose fit will be reported
#' @param std If TRUE, model is standardized (Default FALSE, NULL means "don't show").
#' @param digits How many decimal places to report (Default 2)
#' @param report If "html", then show results in browser (default = "markdown")
#' @param SE Whether to compute SEs... defaults to TRUE. In rare cases, you might need to turn off to avoid errors.
#' @param means Whether to include means in the summary (TRUE)
#' @param residuals Whether to include residuals in the summary (TRUE)
#' @param filter whether to show significant paths (SIG) or NS paths (NS) or all paths (ALL)
#' @param RMSEA_CI Whether to compute the CI on RMSEA (Defaults to FALSE)
#' @param refModels Saturated models if needed for fit indices (see example below:
#' 	If NULL will be computed on demand. If FALSE will not be computed.
#' @param ... Other parameters to control model summary
#' @param matrixAddresses Whether to show "matrix address" columns (Default = FALSE)
#' @family Summary functions
#' @seealso - [umxRAM()]
#' @references - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance 
#'  structure analysis: Conventional criteria versus new alternatives. *Structural Equation Modeling*, **6**, 1-55. 
#'
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models
#'  with binary and continuous outcomes. University of California, Los Angeles, Los Angeles.
#'  Retrieved from <https://www.statmodel.com/download/Yudissertation.pdf>
#' 
#' <https://tbates.github.io>
#' 
#' @export
#' @import OpenMx
#' @return - parameterTable returned invisibly, if estimates requested
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' umxSummary(m1, std = TRUE)
#' # output as latex
#' umx_set_table_format("latex")
#' umxSummary(m1, std = TRUE)
#' umx_set_table_format("markdown")
#' # output as raw
#' umxSummary(m1, std = FALSE)
#' 
#' # switch to a raw data model
#' m1 = umxRAM("One Factor", data = demoOneFactor[1:100, ],
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G")
#' )
#' umxSummary(m1, std = TRUE, filter = "NS")
umxSummary.MxModel <- function(model, refModels = NULL, std = FALSE, digits = 2, report = c("markdown", "html"), means= TRUE, residuals= TRUE, SE = TRUE, filter = c("ALL", "NS", "SIG"), RMSEA_CI = FALSE, ..., matrixAddresses = FALSE){
	# TODO make table take lists of models...
	commaSep = paste0(umx_set_separator(silent = TRUE), " ")
	report   = match.arg(report)
	filter   = match.arg(filter)
	
	message("?umxSummary std=T|F', digits, report= 'html', filter= 'NS' & more")
	
	# If the filter is not default, user must want something: Assume it's what would have been the default...
	if( filter != "ALL" & is.null(std) ) {
		std = FALSE
	}else if(!is.null(std)){
		 if(SE == FALSE){
			 # message("SE must be TRUE to show std, overriding to set SE = TRUE")
			 SE = TRUE
		 }
	}
	umx_has_been_run(model, stop = TRUE)

	if(is.null(refModels)) {
		# SaturatedModels not passed in from outside, so get them from the model
		# TODO umxSummary Improve efficiency: Compute summary only once by detecting when SaturatedLikelihood is missing
		# m1$output$SaturatedLikelihood
		modelSummary = summary(model)
		if(is.na(modelSummary$SaturatedLikelihood)){
			# no SaturatedLikelihood, compute refModels
			refModels = tryCatch({
			    refModels = mxRefModels(model, run = TRUE)
			}, warning = function(x) {
			    print("Warning calling mxRefModels: mxRefModels can't handle all designs, including twin, and WLS https://github.com/OpenMx/OpenMx/issues/184")
			}, error = function(x) {
			    print("Error calling mxRefModels: mxRefModels can't handle all designs, including twin, and WLS https://github.com/OpenMx/OpenMx/issues/184")
			}, finally={
			    # print("cleanup-code")
			})

			if(!class(refModels)=="list"){
				modelSummary = summary(model)
			} else {
				modelSummary = summary(model, refModels = refModels)
			}
		}
		if(is.null(model$data)){
			# TODO model with no data - no saturated solution?
			# message("Top model doesn't contain data. You may get extra information from summary() rather than umxSummary()")
		}
	} else if (refModels == FALSE){
		modelSummary = summary(model) # Don't use or generate refModels		
	}else{
		modelSummary = summary(model, refModels = refModels) # Using refModels supplied by user
	}

	# DisplayColumns show
	if(!is.null(std)){
		# nb: mxStandardizeRAMpaths returns the raw paths as well, so two birds, one stone.
		parameterTable = mxStandardizeRAMpaths(model, SE = SE) # Compute standard errors
		nSubModels = length(model$submodels)
		if(nSubModels > 0){
			tmp = parameterTable
			parameterTable = tmp[[1]]
			if(nSubModels > 1){
				for (i in 2:nSubModels) {
					parameterTable = rbind(parameterTable, tmp[[i]])
					# TODO: vertical merge, or show only model 1?
				}			
			}
		}
		#          name    label  matrix   row         col    Raw.Value  Raw.SE   Std.Value    Std.SE
		# 1  Dep.A[6,1]    age    A        mean_sdrr   age   -0.37       0.0284   -0.372350    .028
		# Raw.SE is new
		names(parameterTable) = c("label", "name", "matrix", "row", "col", "Estimate", "SE", "Std.Estimate", "Std.SE")

		if(matrixAddresses){
			naming = c("name", "matrix", "row", "col")
		} else {
			naming = c("name")
		}
		# TODO: umxSummary add p value, perhaps CI?
		# TODO: umxSummary block table into latents/resid/means etc.
		
		if(std == TRUE){
			# TODO: should CI be here?
			namesToShow = c(naming, "Std.Estimate", "Std.SE", "CI")
		}else{ # must be raw
			namesToShow = c(naming, "Estimate", "SE")					
		}

		if("CI" %in% namesToShow){
			parameterTable$sig = TRUE
			parameterTable$CI  = ""
			for(i in 1:dim(parameterTable)[1]) {
				# TODO we only show SE-based CI for std estimates so far
				est   = parameterTable[i, "Std.Estimate"]
				CI95  = parameterTable[i, "Std.SE"] * 1.96
				bounds = c(est - CI95, est + CI95)

				if(any(is.na(bounds))) {
					# protect cases with SE == NA from evaluation for significance
				} else {
					if (any(bounds <= 0) & any(bounds >= 0)){
						parameterTable[i, "sig"] = FALSE
					}
					if(est < 0){
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), commaSep, round(est + CI95, digits), "]")
					} else {
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), commaSep, round(est + CI95, digits), "]")
					}
				}
			}
		}
		if(filter == "NS") {
			toShow = parameterTable[parameterTable$sig == FALSE, namesToShow]
		} else if(filter == "SIG") {
			toShow = parameterTable[parameterTable$sig == TRUE, namesToShow]
		} else {
			toShow = parameterTable[, namesToShow]
		}
		toShow = xmu_summary_RAM_group_parameters(model, toShow,  means= means, residuals = residuals)
		umx_print(toShow, digits = digits, report = report, caption = paste0("Parameter loadings for model ", omxQuotes(model$name)), na.print = "", zero.print = "0", justify = "none")
	}
	with(modelSummary, {
		if(!is.finite(TLI)){
			TLI_OK = "OK"
		} else {
			if(TLI > .95) {
				TLI_OK = "OK"
			} else {
				TLI_OK = "bad"
			}
		}
		if(!is.finite(RMSEA)) {
			RMSEA_OK = "OK"
		} else {
			if(RMSEA < .06){
			RMSEA_OK = "OK"
			} else {
				RMSEA_OK = "bad"
			}
		}
		if(report == "table"){
			x = data.frame(cbind(model$name, round(Chi,2), formatC(p, format="g"), round(CFI,3), round(TLI,3), round(RMSEA, 3)))
			names(x) = c("model","\u03C7","p","CFI", "TLI","RMSEA") # \u03A7 is unicode for chi
			print(x)
		} else {
			if(RMSEA_CI){
				RMSEA_CI = RMSEA(modelSummary)$txt
			} else {
				RMSEA_CI = paste0("RMSEA = ", round(RMSEA, 3))
			}
			fitMsg = paste0("\nModel Fit: \u03C7\u00B2(", ChiDoF, ") = ", round(Chi, 2), # was A7
				# "Chi2(", ChiDoF, ") = ", round(Chi, 2), # was A7
				", p "      , umx_APA_pval(p, .001, 3, addComparison = TRUE),
				"; CFI = "  , round(CFI, 3),
				"; TLI = "  , round(TLI, 3),
				"; ", RMSEA_CI
			)
			message(fitMsg)
			if(TLI_OK   != "OK"){ message("TLI is worse than desired (>.95)") }
			if(RMSEA_OK != "OK"){ message("RMSEA is worse than desired (<.06)")}
		}
	})
	# TODO: umxSummary.MxRAMModel integrate interval printing into summary table
	if(!is.null(model$output$confidenceIntervals)){
		print(model$output$confidenceIntervals)
	}
	
	xmu_print_algebras(model)
	
	if(!is.null(std)){ # return these as  invisible for the user to filter, sort etc.
		if(filter == "NS"){
			invisible(parameterTable[parameterTable$sig == FALSE, namesToShow])
		}else if(filter == "SIG"){
			invisible(parameterTable[parameterTable$sig == TRUE, namesToShow])
		}else{
			invisible(parameterTable[,namesToShow])
		}
	}
}

#' @export
umxSummary.MxRAMModel <- umxSummary.MxModel


#' Shows a compact, publication-style, summary of a umx Cholesky ACE model
#'
#' Summarize a fitted Cholesky model returned by [umxACE()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelACE
#' @param model an [mxModel()] to summarize.
#' @param digits round to how many digits (default = 2).
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output.
#' @param comparison you can run mxCompare on a comparison model (NULL).
#' @param std Whether to standardize the output (default = TRUE).
#' @param showRg = whether to show the genetic correlations (FALSE).
#' @param CIs Whether to show Confidence intervals if they exist (TRUE).
#' @param returnStd Whether to return the standardized form of the model (default = FALSE).
#' @param report If "html", then open an html table of the results.
#' @param extended how much to report (FALSE).
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @param show std, raw etc. Not implemented for umxACE yet.
#' @return - optional [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACE()], [plot.MxModelACE()], [umxModify()]
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' \dontrun{
#' umxSummaryACE(m1, file = NA);
#' umxSummaryACE(m1, file = "name", std = TRUE)
#' stdFit = umxSummaryACE(m1, returnStd = TRUE);
#' }
umxSummaryACE <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", show, ...) {
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, trim= TRUE, twinOneOnly= TRUE)
		nVar   = length(selDVs)

		# TODO umxSummaryACE these already exist if a_std exists..
		# TODO replace all this with xmu_standardizeACE
		# Calculate standardized variance components
		a = mxEval(top.a, model) # Path coefficients
		c = mxEval(top.c, model)
		e = mxEval(top.e, model)
		A = mxEval(top.A, model) # Variances
		C = mxEval(top.C, model)
		E = mxEval(top.E, model)
		
		if(std){
			caption = paste0("Standardized parameter estimates from a ", dim(a)[2], "-factor Cholesky ACE model. ")
			Vtot = A + C + E;            # Total variance
			I    = diag(nVar);           # nVar Identity matrix
			SD   = solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations
			# (same as "(\sqrt(I.Vtot))~"

			# Standardized _path_ coefficients ready to be stacked together
			a_std  = SD %*% a; # Standardized path coefficients
			c_std  = SD %*% c;
			e_std  = SD %*% e;
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			caption = paste0("Raw parameter estimates from a ", dim(a)[2], "-factor Cholesky ACE model. ")
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs, stringsAsFactors = FALSE);

		if(model$top$dzCr$values == .25){
			colNames = c("a", "d", "e")
			caption = paste0(caption, "A: additive genetic; D: dominance effects; E: unique environment.")
		} else {
			colNames = c("a", "c", "e")
			caption = paste0(caption, "A: additive genetic; C: common environment; E: unique environment.")
		}
		names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		umx_print(Estimates, digits = digits, caption = caption, report = report, zero.print = zero.print)
		xmu_twin_print_means(model = model, report = report)

		if(extended == TRUE) {
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)] = NA
			cClean[upper.tri(cClean)] = NA
			eClean[upper.tri(eClean)] = NA
			unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs);
			names(unStandardizedEstimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			umx_print(unStandardizedEstimates, caption = "Unstandardized Cholesky ACE model path coefficients", digits = digits, zero.print = zero.print)
		}

		if(showRg) {
			# Pre & post multiply covariance matrix by inverse of standard deviations
			NAmatrix = matrix(NA, nVar, nVar);
			rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # E correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)] = NA
			rCClean[upper.tri(rCClean)] = NA
			rEClean[upper.tri(rEClean)] = NA
			genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = selDVs);
			names(genetic_correlations) = selDVs
		 	# Make a nice table.
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			umx_print(genetic_correlations, caption = "Genetic correlations", digits = digits, zero.print = zero.print)
		}
		hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO umxACE CI code: Need to refactor into some function calls...
			# TODO and then add to umxSummaryIP and CP
			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
			# Discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			# THIS IS NOT NEEDED: confidenceIntervals come with estimate in the middle now...
			# These can be names ("top.a_std[1,1]") or labels ("a_r1c1")
			# imxEvalByName finds them both
			# outList = c();
			# for(aName in row.names(CIlist)) {
			# 	outList = append(outList, imxEvalByName(aName, model))
			# }
			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			# CIlist = CIlist[, c("lbound", "estimate", "ubound")]
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the CI results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_CI = c_CI = e_CI = matrix(NA, rows, cols)

			# Iterate over each CI
			labelList = imxGenerateLabels(model)	
			rowCount  = dim(CIlist)[1]
			# return(CIlist)
			for(n in 1:rowCount) { # n = 1
				thisName = row.names(CIlist)[n] # thisName = "a11"
					# Convert labels to [bracket] style
					if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString = paste0(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]")

				if(grepl("^a", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
				}
			}
			# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
			# TODO umxSummaryACE: Add option to use mxSE
			Estimates = data.frame(cbind(a_CI, c_CI, e_CI), row.names = selDVs, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			umx_print(Estimates, digits = digits, zero.print = zero.print,  report=report, file = "tmpCI.html")
			xmu_twin_print_means(model, digits = digits, report = report)
			CI_Fit = model
			CI_Fit$top$a$values = a_CI
			CI_Fit$top$c$values = c_CI
			CI_Fit$top$e$values = e_CI
		} # end Use CIs
	} # end list catcher?

	if(!is.na(file)) {
		# message("making dot file")
		if(hasCIs & CIs){
			umxPlotACE(CI_Fit, file = file, std = FALSE)
		} else {
			umxPlotACE(model, file = file, std = std)
		}
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		xmu_standardize_ACE(model)
	}else{
		invisible(Estimates)
	}
}

#' @export
umxSummary.MxModelACE <- umxSummaryACE

#' Present results of a twin ACE-model with covariates in table and graphical forms.
#'
#' Summarize a Cholesky model with covariates, as returned by [umxACEcov()]
#'
#' @aliases umxSummary.MxModelACEcov
#' @param model a [umxACEcov()] model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param extended how much to report (FALSE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param std = whether to show the standardized model (TRUE)
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param CIs Whether to show Confidence intervals if they exist (TRUE)
#' @param zero.print How to show zeros (".")
#' @param report If "html", then open an html table of the results.
#' @param ... Other parameters to control model summary
#' @return - optional [mxModel()]
#' @export
#' @family Summary functions
#' @seealso - [umxACEcov()] 
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' \dontrun{
#' umxSummaryACE(m1, file = NA)
#' umxSummaryACE(m1, file = "name", std = TRUE)
#' stdFit = umxSummaryACE(m1, returnStd = TRUE)
#' }
umxSummaryACEcov <- function(model, digits = 2, showRg = FALSE, std = TRUE, comparison = NULL, CIs = TRUE, zero.print = ".", report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, extended = FALSE, ...) {
	report   = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	
	# Depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACEcov(thisFit, digits = digits, file = file, returnStd = returnStd, extended = extended, showRg = showRg, std = std, comparison = comparison, CIs = CIs, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, source = "expCovMZ", trim = TRUE, twinOneOnly = TRUE)
		nDV    = length(selDVs)

		# Calculate standardized variance components
		a    = mxEval(top.a, model); # Path coefficients
		c    = mxEval(top.c, model);
		e    = mxEval(top.e, model);
		A    = mxEval(top.A, model); # Variances
		C    = mxEval(top.C, model);
		E    = mxEval(top.E, model);
		Vtot = A + C + E; # Total variance
		Iden = diag(nDV); # nDV Identity matrix
		SD   = solve(sqrt(Iden * Vtot)) # Inverse of diagonal matrix of standard deviations
		# (same as "(\sqrt(Iden.Vtot))~"

		# Standardized _path_ coefficients ready to be stacked together
		a_std = SD %*% a; # Standardized path coefficients
		c_std = SD %*% c;
		e_std = SD %*% e;

		if(std){
			caption = "Standardized solution"
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			caption = "Raw solution"
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		rowNames = sub("(_T)?1$", "", selDVs)
		Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);

		names(Estimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));

		umx_print(Estimates, digits = digits, zero.print = zero.print, report=report, caption= caption)
		xmu_twin_print_means(model, digits = digits, report = report)
		
		if(extended == TRUE) {
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)] = NA
			cClean[upper.tri(cClean)] = NA
			eClean[upper.tri(eClean)] = NA
			unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);
			names(unStandardizedEstimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));
			umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print, caption = "Unstandardized path coefficients")
		}

		# Pre & post multiply covariance matrix by inverse of standard deviations
		if(showRg) {
			NAmatrix = matrix(NA, nDV, nDV);
			rA = tryCatch(solve(sqrt(Iden * A)) %*% A %*% solve(sqrt(Iden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(Iden * C)) %*% C %*% solve(sqrt(Iden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(Iden * E)) %*% E %*% solve(sqrt(Iden * E)), error = function(err) return(NAmatrix)); # E correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)] = NA
			rCClean[upper.tri(rCClean)] = NA
			rEClean[upper.tri(rEClean)] = NA
			genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames)
			names(genetic_correlations) <- rowNames
		 	# Make a nice-ish table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each= nDV), rep(1:nDV))
			umx_print(genetic_correlations, digits=digits, zero.print = zero.print, caption = "Genetic correlations")
		}
		stdFit = model
		hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO Need to refactor this into some function calls...
			# TODO and then add to umxSummaryIP and CP
			message("Creating CI-based report!")
			# CIs exist, get the lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0), ]
			# discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]

			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			CIlist = CIlist[, c("lbound", "estimate", "ubound")] 
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the standardized results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_std = c_std = e_std = matrix(NA, rows, cols)

			# iterate over each CI
			labelList = imxGenerateLabels(model)			
			rowCount = dim(CIlist)[1]
			# return(CIlist)
			for(n in 1:rowCount) { # n = 1
				thisName = row.names(CIlist)[n] # thisName = "a11"
				# convert labels to [bracket] style
					if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts = round(CIlist[n, c("estimate", "lbound", "ubound")], 2)
				thisString = paste(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]", sep="")
				# print(list(CIlist, labelList, rowCount, fullName, thisMatrixName))

				if(grepl("^a", thisMatrixName)) {
					a_std[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_std[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_std[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
				}
			}
			print(a_std)
			print(c_std)
			print(e_std)
		}
	} # Use CIs
	stdFit$top$a$values = a_std
	stdFit$top$c$values = c_std
	stdFit$top$e$values = e_std
	if(!is.na(file)) {
		message("making dot file")
		plot(model, file, std = std)
	}
	if(returnStd) {
		if(CIs){
			message("Returned model won't work if you asked for CIs...")
		}
		return(stdFit)
	}
}
#' @export
umxSummary.MxModelACEcov <- umxSummaryACEcov


#' Present the results of a Common-pathway twin model in table and graphical form
#'
#' Summarizes a Common-Pathway model, as returned by [umxCP()]
#'
#' @aliases umxSummary.MxModelCP
#' @param model A fitted [umxCP()] model to summarize
#' @param digits Round to how many digits (default = 2)
#' @param std Whether to show the standardized model (TRUE) (ignored: used extended = TRUE to get unstandardized)
#' @param CIs Confidence intervals (default FALSE)
#' @param showRg Whether to show the genetic correlations (default FALSE)
#' @param comparison Run mxCompare on a comparison model (default NULL)
#' @param report Print tables to the console (as 'markdown'), or open in browser ('html')
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [mxModel()]
#' @export
#' @family Summary functions
#' @seealso - \code{\link{umxCP}()}, [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
# # Help optimizer by putting wt on a similar scale to ht
#' twinData$wt1 = twinData$wt1/10
#' twinData$wt2 = twinData$wt2/10
#' selDVs = c("ht", "wt")
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' umx_set_auto_plot(FALSE) # turn off autoplotting for CRAN
#' m1 = umxCP(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "", optimizer = "SLSQP")
#' umxSummaryCP(m1, file = NA) # Suppress plot creation with file
#' umxSummary(m1, file = NA)   # Generic summary is the same
#' stdFit = umxSummaryCP(m1, digits = 2, std = TRUE, file = NA, returnStd = TRUE);
#' umxSummary(m1, std = FALSE, showRg = TRUE, file = NA);
#' umxSummary(m1, std = FALSE, file = NA)
#'
#' # =================
#' # = Print example =
#' # =================
#' umxSummary(m1, file = "Figure 3", std = TRUE)
#'
#' # =================
#' # = Confint example =
#' # =================
#' m1 = umxConfint(m1, "smart", run = FALSE);
#' m1 = umxConfint(m1, "smart", run = TRUE);
#' umxSummary(m1, CIs = TRUE, file = NA);
#' }
#'
umxSummaryCP <- function(model, digits = 2, std = TRUE, CIs = FALSE, showRg = FALSE, comparison = NULL, report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, ...) {
	# TODO: Detect value of DZ covariance, and if .25 set "C" to "D" in tables
	report = match.arg(report)

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message(paste("Output for Model: ", omxQuotes(thisFit$name), "\n"))
			umxSummaryCP(thisFit, digits = digits, file = file, returnStd = returnStd, showRg = showRg, comparison = comparison, std = std, CIs = CIs)
		}
	} else {
		umx_check_model(model, "MxModelCP", beenRun = TRUE, callingFn = "umxSummaryCP")
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, source = "expCovMZ", trim = TRUE, twinOneOnly = TRUE)
		nVar   = length(selDVs)
		nFac   = dim(model$top$matrices$a_cp)[[1]]	

		if(CIs){
			oldModel = model # Cache this in case we need it (CI stash model has string where values should be).
			model = xmu_CI_stash(model, digits = digits, dropZeros = TRUE, stdAlg2mat = TRUE)
		} else if(any(c(std, returnStd))) {
			model = xmu_standardize_CP(model) # Make a standardized copy of model
		}

		# 1. Create Tables of Common Factor Paths
		a_cp = model$top$a_cp$values # nFac * nFac matrix of path coefficients flowing into cp_loadings
		c_cp = model$top$c_cp$values
		e_cp = model$top$e_cp$values

		# Common factor ACE inputs are std to 1
		# Bind diag of a_cp, c and e columns into nFac-row matrix
		commonACE = cbind(diag(a_cp), diag(c_cp), diag(e_cp)) 
		commonACE = data.frame(commonACE, row.names = paste("Common.factor", 1:nFac, sep = "."), stringsAsFactors = FALSE);
		names(commonACE) = c ("A", "C", "E")
		umx_print(commonACE, digits = digits, zero.print = ".", report = report, caption = "Common Factor Paths")
		
		if(class(model$top$matrices$a_cp)[1] == "LowerMatrix"){
			message("You used correlated genetic inputs to the common factor. This is the a_cp matrix")
			print(a_cp)
		}
		
		# 2. Create Table of standardized loadings on Common factors
		cp_loadings = model$top$cp_loadings$values # nVar * nFac matrix
		cp_loadings = data.frame(cp_loadings, row.names = selDVs, stringsAsFactors = FALSE);
		names(cp_loadings) = paste0("CP", 1:length(names(cp_loadings)))
		umx_print(cp_loadings, digits = digits, zero.print = ".", report =report, caption = "Loadings of each trait on the Common Factors")

		# 3. Create Tables of Specific-factor loadings
		# Stack specific path coefficients together
		as = model$top$as$values # Specific factor path coefficients
		cs = model$top$cs$values
		es = model$top$es$values

		specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')), stringsAsFactors = FALSE,
			rbind(diag(as), 
				  diag(cs),
				  diag(es))
		)
		names(specifics) = selDVs
		umx_print(specifics, digits = digits, report = report, caption = "Specific-factor loadings", zero.print = ".")
		xmu_twin_print_means(model, digits = digits, report = report)

		if(showRg) {
			# Make Table of Genetic Correlations
			# Pre & post multiply covariance matrix by inverse of standard deviations
			A = model$top$A$values # Variances
			C = model$top$C$values
			E = model$top$E$values
			Vtot = A + C + E; # Total variance
			nVarIden = diag(nVar)
			NAmatrix = matrix(NA, nVar, nVar);

			rA = tryCatch(solve(sqrt(nVarIden * A)) %*% A %*% solve(sqrt(nVarIden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(nVarIden * C)) %*% C %*% solve(sqrt(nVarIden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(nVarIden * E)) %*% E %*% solve(sqrt(nVarIden * E)), error = function(err) return(NAmatrix)); # E correlations
			genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = selDVs);
			# Make a table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			umx_print(genetic_correlations, digits = digits, zero.print = ".", report = report, caption = "Genetic Correlations")
		}
		if(!is.na(file)){
			umxPlotCP(model, file = file, digits = digits, std = FALSE, means = FALSE)
		}
		if(returnStd) {
			invisible(model)
		}
	}
}

#' @export
umxSummary.MxModelCP <- umxSummaryCP

#' Present the results of an independent-pathway twin model in table and graphical form
#'
#' Summarize a Independent Pathway model, as returned by [umxIP()]
#'
#' @aliases umxSummary.MxModelIP
#' @param model A fitted [umxIP()] model to summarize
#' @param digits round to how many digits (default = 2)
#' @param std = Whether to show the standardized model (TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param comparison Whether to run mxCompare on a comparison model (NULL)
#' @param CIs Confidence intervals (F)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param report how to display the results ("html" will open in browser as table)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [mxModel()]
#' @family Summary functions
#' @export
#' @seealso - \code{\link{umxIP}()}, [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(GFF) # family function and well-being data
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("hap", "sat", "AD") # These will be expanded into "hap_T1" "hap_T2" etc.
#' m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' umxSummaryIP(m1)
#' plot(m1)
#' \dontrun{
#' umxSummaryIP(m1, digits = 2, file = "Figure3", showRg = FALSE, CIs = TRUE);
#' }
umxSummaryIP <- function(model, digits = 2, file = getOption("umx_auto_plot"), std = TRUE, showRg = FALSE, comparison = NULL, CIs = FALSE, returnStd = FALSE, report = c("markdown", "html"), ...) {
	umx_check_model(model, "MxModelIP", beenRun = TRUE, callingFn = "umxSummaryIP")
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)

	selDVs = dimnames(model$top.expCovMZ)[[1]]
	stdFit = model; # If we want to output a model with the standardized values (perhaps for drawing a path diagram)
	nVar   = length(selDVs)/2;
	# how to detect how many factors are present?
	# Calculate standardized variance components
	ai = mxEval(top.ai, model); # Column of independent path coefficients (nVar * nFac) 
	ci = mxEval(top.ci, model);
	ei = mxEval(top.ei, model);

	as = mxEval(top.as, model); # nVar * nVar matrix of specific path coefficients (Just diagonal, or possibly Cholesky lower for E)
	cs = mxEval(top.cs, model);
	es = mxEval(top.es, model);

	A  = mxEval(top.A , model); # Totaled Variance components (ai + as etc.)
	C  = mxEval(top.C , model);
	E  = mxEval(top.E , model);

	nFac     = c(a = dim(ai)[2], c = dim(ci)[2], e = dim(ei)[2]);

	Vtot     = A+C+E; # total variance
	nVarIden = diag(nVar); # Make up a little nVar Identity matrix using the behavior of diag to make an nVar*nVar Identity matrix
	SD       = solve(sqrt(nVarIden*Vtot)) # inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	ai_std   = SD %*% ai ; # Standardized path coefficients (independent general factors )
	ci_std   = SD %*% ci ; # Standardized path coefficients (independent general factors )
	ei_std   = SD %*% ei ; # Standardized path coefficients (independent general factors )

	stdFit@submodels$top$ai@values = ai_std
	stdFit@submodels$top$ci@values = ci_std
	stdFit@submodels$top$ei@values = ei_std

	rowNames = sub("(_T)?1$", "", selDVs[1:nVar])
	std_Estimates = data.frame(cbind(ai_std, ci_std, ei_std), row.names = rowNames, stringsAsFactors = FALSE);

	# 1. Make Table of General IP path loadings
	x = sapply(FUN = seq_len, nFac)
	names(std_Estimates) = c(paste0("ai", 1:nFac["a"]), paste0("ci", 1:nFac["c"]), paste0("ei", 1:nFac["e"]))
	umx_print(std_Estimates, digits = digits, zero.print = ".", report = report, caption = "General Independent Pathway path loadings")

	# 2. Make Table of Standard specific path coefficients ready to be stacked together
	as_std = SD %*% as; # Standardized path coefficients (nVar specific factors matrices)
	cs_std = SD %*% cs;
	es_std = SD %*% es;
	stdFit@submodels$top$as@values = as_std
	stdFit@submodels$top$cs@values = cs_std
	stdFit@submodels$top$es@values = es_std

	std_Specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')),
		rbind(
			diag(as_std), 
			diag(cs_std),
			diag(es_std)
		)
	)
	names(std_Specifics) = rowNames;
	umx_print(round(std_Specifics, digits), digits = digits, zero.print = ".", report = report, caption = "Specific factor loadings")
	xmu_twin_print_means(model, digits = digits, report = report)
	
	if(showRg) {
		# Pre & post multiply covariance matrix by inverse of standard deviations
		NAmatrix = matrix(NA, nVar, nVar);  
		rA = tryCatch(solve(sqrt(nVarIden*A)) %*% A %*% solve(sqrt(nVarIden*A)), error=function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(nVarIden*C)) %*% C %*% solve(sqrt(nVarIden*C)), error=function(err) return(NAmatrix)); # shared environmental correlations
		rE = tryCatch(solve(sqrt(nVarIden*E)) %*% E %*% solve(sqrt(nVarIden*E)), error=function(err) return(NAmatrix)); # Unique environmental correlations
		genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
		# Make a table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits = digits, zero.print = ".", report = report, caption = "Genetic Correlations")
	}
	if(CIs){
		message("Showing CIs in output not implemented yet. In the mean time, use summary(model) to view them.")
	}
	if(!is.na(file)){
		umxPlotIP(x = stdFit, file = file, digits = digits, std = FALSE)
	}
	if(returnStd) {
		return(stdFit)
	}
}

#' @export
umxSummary.MxModelIP <- umxSummaryIP

#' Summarize a GxE model
#'
#' Summarize a genetic moderation model, as returned by [umxGxE()]. Prints graphs of A, C, and E, standardized and raw.
#'
#' Note: see also [umxReduce()] which knows how to reduce a GxE model.
#'
#' @aliases umxSummary.MxModelGxE
#' @param model A fitted [umxGxE()] model to summarize
#' @param reduce  Whether run and tabulate a complete model reduction...(Defaults to FALSE)
#' @param separateGraphs If TRUE, both std and raw plots in one figure (default FALSE)
#' @param report "markdown" or "html" = open a browser for copyable tables
#' @param gg Whether to use ggplot to create the graphs (default TRUE)
#' @param std Whether to show the standardized model (not implemented! TRUE)
#' @param CIs Confidence intervals (FALSE)
#' @param xlab label for the x-axis of plot
#' @param digits round to how many digits (default = 2)
#' @param location default = "topleft"
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param show not doing anything yet (required for all summary functions)
#' @param ... Optional additional parameters
#' @return - optional [mxModel()]
#' @family Summary functions
#' @export
#' @seealso - [umxGxE()], [umxReduce()], [plot()], [umxSummary)] all work for IP, CP, GxE, and ACE models.
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' # The total sample has been subdivided into a young cohort, 
#' # aged 18-30 years, and an older cohort aged 31 and above.
#' # Cohort 1 Zygosity is coded as follows 1 == MZ females 2 == MZ males 
#' # 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
#  # use ?twinData to learn about this data set.
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, zygosity == "MZFF", selVars)
#' dzData  = subset(twinData, zygosity == "DZMM", selVars)
#' # Exclude cases with missing Def
#' mzData = mzData[!is.na(mzData[selDefs[1]]) & !is.na(mzData[selDefs[2]]),]
#' dzData = dzData[!is.na(dzData[selDefs[1]]) & !is.na(dzData[selDefs[2]]),]
#' \dontrun{
#' m1 = umxGxE(selDVs = "bmi", selDefs = "age", sep="", dzData = dzData, mzData = mzData)
#' # Plot Moderation
#' umxSummaryGxE(m1)
#' umxSummaryGxE(m1, location = "topright")
#' umxSummaryGxE(m1, separateGraphs = FALSE)
#' }
umxSummaryGxE <- function(model = NULL, digits = 2, xlab = NA, location = "topleft", separateGraphs = FALSE, gg=TRUE, file = getOption("umx_auto_plot"), returnStd = NULL, std = NULL, reduce = FALSE, CIs = NULL, report = c("markdown", "html"), show= NULL, ...) {
	report = match.arg(report)
	# if(!is.null(show){
	# 	if(show == "std"){
	# 		std = TRUE
	# 	} else {
	# 		std = FALSE
	# 	}
	# }
	
	umx_has_been_run(model, stop = TRUE)
	
	if(any(!is.null(c(returnStd, std, CIs) ))){
		message("For GxE, returnStd, std, comparison or CIs are not implemented... The plot will include standardized outcomes, but raw output should be emphasized. SEs are shown rather the CIs, at present")
	}

	if(is.null(model)){
		message("umxSummaryGxE calls plot.MxModelGxE for a twin moderation plot. A use example is:\n umxSummaryGxE(model, location = \"topright\")")
		stop();
	}
	tablePub = summary(model)$parameters[, c("name", "Estimate", "Std.Error")]
	if(report == "html"){
		print(xtable::xtable(tablePub), type = "HTML", file = file, sanitize.text.function = function(x){x}, digits=3)
		umx_open(file)
	} else {
		# markdown
		umx_print(tablePub, digits = digits)
	}

	umxPlotGxE(model, xlab = xlab, location = location, separateGraphs = separateGraphs, gg = gg)

	if(reduce){
		umxReduce(model = model, report = report)
	}
}

#' @export
umxSummary.MxModelGxE <- umxSummaryGxE


#' Print a comparison table of one or more [mxModel()]s, formatted nicely.
#'
#' @description
#' umxCompare compares two or more [mxModel()]s. It has several nice features:
#' 
#' 1. It supports direct control of rounding, and reports p-values rounded to APA style.
#' 2. It reports the table in your preferred format (default is markdown, options include latex).
#' 3. Table columns are arranged to make for easy comparison for readers.
#' 4. report = 'inline', will provide an English sentence suitable for a paper.
#' 5. report = "html" opens a web table in your browser to paste into a word processor.
#' 
#' *Note*: If you leave comparison blank, it will just give fit info for the base model
#'
#' @param base The base [mxModel()] for comparison
#' @param comparison The model (or list of models) which will be compared for fit with the base model (can be empty)
#' @param all Whether to make all possible comparisons if there is more than one base model (defaults to T)
#' @param digits rounding for p-values etc.
#' @param report "markdown" (default), "inline" (a sentence suitable for inclusion in a paper), or "html".
#' create a web table and open your default browser.
#' (handy for getting tables into Word, and other text systems!)
#' @param file file to write html too if report = "html" (defaults to "tmp.html")
#' @param compareWeightedAIC Show the Wagenmakers AIC weighted comparison (default = FALSE)
#' @param silent (don't print, just return the table as a dataframe (default = FALSE)
#' @family Model Summary and Comparison
#' @seealso - [umxSummary()], [umxRAM()],[mxCompare()]
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
#' \dontrun{
#' umxCompare(m1, m2, report = "html") # Open table in browser
#' }
#'
#' # Two comparison models
#' m3 = umxModify(m2, update = "G_to_x3", name = "drop_path_2_x2_and_3")
#' 
#' umxCompare(m1, c(m2, m3))
#' umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE)
#' umxCompare(c(m1, m2), c(m2, m3), all = TRUE)
#'
#' \dontrun{
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("WLS", data = demoOneFactor, type = "DWLS",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
#' umxCompare(m1, m2, report = "html") # Open table in browser
#' }
umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, digits = 3, report = c("markdown", "html", "inline"), compareWeightedAIC = FALSE, silent = FALSE, file = "tmp.html") {
	report = match.arg(report)
	if(umx_is_MxModel(all)){
		stop("Provide all comparison models as a c() (You provided a model as input to 'all', and I'm guessing that's a mistake)")
	}
	if(is.null(comparison)){
		comparison = base
	} else if (is.null(base)) {
		stop("You must provide at least a base model for umxCompare")
	}
	if(length(base) == 1) {
		if(typeof(base) == "list"){
			base = base[[1]]
		}
		if(!umx_has_been_run(base)){
			warning("Base model not run yet!")		
		}
	}
	if(length(comparison) == 1) {
		if(typeof(comparison) == "list"){
			comparison = comparison[[1]]
		}
		if(!umx_has_been_run(comparison)){
			warning("Comparison model has not been run!")		
		}
	}
	tableOut = mxCompare(base = base, comparison = comparison, all = all)
	tableOut = as.data.frame(tableOut)

	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf |p     |fit       |fitUnits |diffFit |chisq     |SBchisq |
	# |:--------|:--------------|---:|:---------|:----|---------:|:---------|:-------|:-----|:---------|:--------|:-------|:---------|:-------|
	# |DWLS     |               |  6 |          |0    | 12.00000 |          |        |      |0         |r'Wr     |        |0         |        |
	# |DWLS     |drop_l2mpg     |  5 |          |1    | 14.49542 |          |1       |      |4.4954186 |r'Wr     |        |4.4954186 |        |
	
	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf | p    |
	#    1            2           3     4          5     6          7          8        9     
	# | twinSat | <NA>          | 13 | 333.0781 | 149 | 35.07809 | NA       | NA     | NA   |
	# | twinSat | betaSetToZero | 10 | 351.6486 | 152 | 47.64858 | 18.57049 | 3      | 0.01 |

	# Pre Feb 2021 version 2.18.1.233
	if(packageVersion("OpenMx") < "2.18.1.233"){
		# old format mxCompare
		tablePub = tableOut[, c("comparison", "ep", "diffLL" , "diffdf", "p", "AIC", "base")]
		names(tablePub)     = c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "base")
		tablePub$fitUnits = ""
	} else {
		# new format mxCompare
		tablePub = tableOut[, c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "base", "fitUnits")]
		# str(tmp@results)
		# 'data.frame':	2 obs. of  13 variables:
		#  $ base      : chr  "tim" "tim"
		#  $ comparison: chr  NA "tim"
		#  $ ep        : num  9 9
		#  $ df        : num  87 87
		#  $ diffdf    : num  NA 0
		#  $ fit       : num  330 330
		#  $ fitUnits  : chr  "-2lnL" "-2lnL"
		#  $ diffFit   : num  NA 0
		#  $ AIC       : num  348 348
		#  $ p         : num  NA NA
		#  $ minus2LL  : num  330 330
		#  $ diffLL    : num  NA 0
		#  $ SBchisq   : num  NA NA
	}

	# Subtract row-1 AIC from all values and place the resulting deltaAIC column after AIC 
	tablePub$deltaAIC = tablePub[, "AIC"] - tablePub[1, "AIC"]
	tablePub = tablePub[,c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "deltaAIC", "base", "fitUnits")]

	# c("1: Comparison", "2: Base", "3: EP", "4: AIC", "5: &Delta; -2LL", "6: &Delta; df", "7: p")
	# U+2206 = math delta
	# Fix problem where base model has compare set to its own name, and name set to NA
	nRows = dim(tablePub)[1]
	for (i in 1:nRows) {
		if(is.na(tablePub[i, "comparison"])){
			tablePub[i, "comparison"] = tablePub[i, "base"]
			tablePub[i, "base"] = NA
		}
	}
	tablePub[, "p"] = umx_APA_pval(tablePub[, "p"], min = (1/ 10^3), digits = digits, addComparison = NA)
	if(report == "inline"){
		n_rows = dim(tablePub)[1]
		for (i in 1:n_rows) {
			thisPValue = tableOut[i, "p"]
			if(!is.na(thisPValue) && !is.nan(thisPValue)){
				if(tableOut[i, "p"] < .05){
					this = ". This caused a significant loss of fit "
				} else {
					this = ". This did not lower fit significantly "
				}
				inlineMsg = paste0("The hypothesis that ", omxQuotes(tablePub[i, "comparison"]), 
				" was tested by dropping ", tablePub[i, "comparison"],
				" from ", omxQuotes(tablePub[i, "base"]), 
				this, "(\u03C7\u00B2(", tablePub[i, "diffdf"], ") = ", round(tablePub[i, "diffFit"], 2), # \u03A7 = Chi \u00B2 = superscript 2
				", p = ", tablePub[i, "p"], ": AIC = ", round(tablePub[i, "AIC"], digits), " change in AIC = ", round(tablePub[i, "deltaAIC"], digits), ")."
				)
				if(!silent){
					cat(inlineMsg)
				}
			}
		}
	}
	
	# Rename for printing to console
	names(tablePub) = c("Model", "EP", "\u0394 Fit" , "\u0394 df" , "p", "AIC", "\u0394 AIC", "Compare with Model", "Fit units")

	if(report == "inline"){ report= "markdown"}
	if(!silent){
		umx_print(tablePub, digits = digits, zero.print = "0", caption = "Table of Model Comparisons", report = report)
	}
	# htmlNames       = c("Model", "EP", "&Delta; -2LL", "&Delta; df", "p", "AIC", "&Delta AIC", "Compare with Model")
	# if(report == "html"){
	# 	tableHTML = tablePub
	# 	names(tableHTML) = htmlNames
	# 	print(xtable::xtable(tableHTML), type = "HTML", file = file, sanitize.text.function = function(x){x})
	# 	umx_open(file)
	# }
	if(compareWeightedAIC){
		modelList = c(base, comparison)
		# get list of AICs
		AIClist = c()
		for (i in modelList) {
			AIClist = c(AIClist, AIC(i))
		}
		whichBest = which.min(AIClist)
		bestModel = modelList[[whichBest]]
		# Probabilities according to AIC MuMIn::Weights (Wagenmakers et al https://pubmed.ncbi.nlm.nih.gov/15117008/ )
		aic.weights = round(Weights(AIClist), 2)
		if(!silent){
			cat("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
			cat("AIC weight-based  {Wagenmakers, 2004, 192-196} conditional probabilities of being the best model for ", 
				omxQuotes(namez(modelList)), " respectively are: ", 
				omxQuotes(aic.weights), " Using MuMIn::Weights(AIC()).")	
		}
		
	}
	invisible(tablePub)
}

#' umxCI_boot
#'
#' Compute boot-strapped Confidence Intervals for parameters in an [mxModel()]
#' The function creates a sampling distribution for parameters by repeatedly drawing samples
#' with replacement from your data and then computing the statistic for each redrawn sample.
#' @param model is an optimized mxModel
#' @param rawData is the raw data matrix used to estimate model
#' @param type is the kind of bootstrap you want to run. "par.expected" and "par.observed" 
#' use parametric Monte Carlo bootstrapping based on your expected and observed covariance matrices, respectively.
#' "empirical" uses empirical bootstrapping based on rawData.
#' @param std specifies whether you want CIs for unstandardized or standardized parameters (default: std = TRUE)
#' @param rep is the number of bootstrap samples to compute (default = 1000).
#' @param conf is the confidence value (default = 95)
#' @param dat specifies whether you want to store the bootstrapped data in the output (useful for multiple analyses, such as mediation analysis)
#' @param digits rounding precision
#' @return - expected covariance matrix
#' @export
#' @references - \url{https://openmx.ssri.psu.edu/thread/2598}
#' Original written by \url{https://openmx.ssri.psu.edu/users/bwiernik}
#' @seealso - [umxExpMeans()], [umxExpCov()]
#' @family Reporting functions
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#'
#' umxCI_boot(m1, type = "par.expected")
#'}
umxCI_boot <- function(model, rawData = NULL, type = c("par.expected", "par.observed", "empirical"), std = TRUE, rep = 1000, conf = 95, dat = FALSE, digits = 3) {
	# depends on MASS::mvrnorm
	type = match.arg(type, c("par.expected", "par.observed", "empirical"))
	if(type == "par.expected") {
		exp = umxExpCov(model, latents = FALSE)
	} else if(type == "par.observed") {
		if(model$data$type == "raw") {
			exp = var(mxEval(data, model))
		} else { 
			if(model$data$type == "sscp") {
				exp = mxEval(data, model) / (model$data$numObs - 1)
			} else {
				exp = mxEval(data, model)
			}
		}
	}
	N = round(model$data$numObs)
	pard = t(data.frame("mod" = summary(model)$parameters[, 5 + 2 * std], row.names = summary(model)$parameters[, 1]))
	pb   = txtProgressBar(min = 0, max = rep, label = "Computing confidence intervals", style = 3)

	if(type == "empirical") {
		if(length(rawData) == 0) {
			if(model$data$type == "raw"){
				rawData = mxEval(data, model)
			} else {
				stop("No raw data supplied for empirical bootstrap.")	
			}
		}
		for(i in 1:rep){
			bsample.i = sample.int(N, size = N, replace = TRUE)
			bsample   = var(rawData[bsample.i, ])
			mod       = mxRun(mxModel(model, mxData(observed = bsample, type = "cov", numObs = N)), silent = TRUE)
			pard      = rbind(pard, summary(mod)$parameters[, 5 + 2*std])
			rownames(pard)[nrow(pard)] = i
			utils::setTxtProgressBar(pb, i)
		}
	} else {
		for(i in 1:rep){
			bsample = var(MASS::mvrnorm(N, rep(0, nrow(exp)), exp))
			mod     = mxRun(mxModel(model, mxData(observed = bsample, type = "cov", numObs = N)), silent = TRUE)
			pard    = rbind(pard, summary(mod)$parameters[, 5 + 2 * std])
			rownames(pard)[nrow(pard)] = i
			utils::setTxtProgressBar(pb, i)
		}
	}
	low = (1-conf/100)/2
	upp = ((1-conf/100)/2) + (conf/100)
	LL  = apply(pard, 2, FUN = quantile, probs = low) # lower limit of confidence interval
	UL  = apply(pard, 2, FUN = quantile, probs = upp) # upper quantile for confidence interval
	LL4 = round(LL, 4)
	UL4 = round(UL, 4)
	ci  = cbind(LL4, UL4)
	colnames(ci) = c(paste((low*100), "%", sep = ""), paste((upp*100), "%", sep = ""))
	p         = summary(model)$parameters[, c(1, 2, 3, 4, c(5:6 + 2*std))]
	cols      = sapply(p, is.numeric)
	p[, cols] = round(p[,cols], digits) 
	
	if(dat) {
		return(list("Type" = type, "bootdat" = data.frame(pard), "CI" = cbind(p, ci)))
	} else {
		return(list("CI" = cbind(p, ci)))
	}
}


# ============
# = Graphics =
# ============

#' Create and display a graphical path diagram for a LISREL model.
#'
#' `plot.MxLISRELModel` produces SEM diagrams using [DiagrammeR::DiagrammeR()] (or a graphviz application) to create the image. 
#' 
#' \emph{Note:} By default, plots open in your browser (or plot pane if using RStudio).
#' 
#' **Opening in an external editor/app**
#' 
#' The underlying format is graphviz.
#' If you use `umx_set_plot_format("graphviz")`, figures will open in a graphviz helper app (if installed).
#' If you use graphviz, we try and use that app, but YOU HAVE TO INSTALL IT!
#' 
#' On MacOS, you may need to associate the \sQuote{.gv} extension with your graphviz app.
#' Find the .gv file made by plot, get info (cmd-I), then choose \dQuote{open with}, 
#' select graphviz.app (or OmniGraffle professional),
#' then set \dQuote{change all}.
#' 
#' 
#' The commercial application \dQuote{OmniGraffle} is great for editing these images.
#'
#' @rdname plot.MxLISRELModel
#' @param x A LISREL [mxModel()] from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE).
#' @param fixed Whether to show fixed paths (defaults to TRUE)
#' @param means Whether to show means or not (default = TRUE)
#' @param digits The number of decimal places to add to the path coefficients
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param labels Whether to show labels on the paths. both will show both the parameter and the label. ("both", "none" or "labels")
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional parameters
#' @export
#' @seealso - [umx_set_plot_format()], [umx_set_auto_plot()], [umx_set_plot_format()], [plot.MxModel()], [umxPlotACE()], [umxPlotCP()], [umxPlotIP()], [umxPlotGxE()]
#' @family umx S3 functions
#' @family Plotting functions
#' @references - <https://github.com/tbates/umx>, <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
#' @md
#' @examples
#' # plot()
#' # TODO get LISREL example model
#' # Figure out how to map its matrices to plot. Don't do without establishing demand.
plot.MxLISRELModel <- function(x = NA, std = FALSE, fixed = TRUE, means = TRUE, digits = 2, file = "name", labels = c("none", "labels", "both"), resid = c("circle", "line", "none"), strip_zero = TRUE, ...) {
	stop("Sorry, plot doesn't yet support LISREL models. I'd advise using umxRAM instead.")
}

#' Create and display a graphical path diagram for a model.
#'
#' [plot()] produces SEM diagrams in graphviz format, and relies on [DiagrammeR()] (or a 
#' graphviz application) to create the image. 
#' \emph{Note:} DiagrammeR is supported out of the box.  By default, plots open in your browser. 
#' 
#' If you use umx_set_plot_format("graphviz"), they will open in a graphviz helper app (if installed).
#' The commercial application \dQuote{OmniGraffle} is great for editing these images.
#' On unix and windows, [plot()] will create a pdf and open it in your default pdf reader.
#' 
#' If you use graphviz, we try and use that app, but YOU HAVE TO INSTALL IT!
#' 
#' *MacOS note*: On Mac, we will try and open an app: you may need to associate the \sQuote{.gv}
#' extension with the graphviz app.
#' Find the .gv file made by plot, get info (cmd-I), then choose \dQuote{open with}, 
#' select graphviz.app (or OmniGraffle professional),
#' then set \dQuote{change all}.
#'
#' @aliases plot umxPlot
#' @rdname plot.MxModel
#' @param x An [mxModel()] from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE).
#' @param fixed Whether to show fixed paths (defaults to TRUE)
#' @param means Whether to show means or not (default = TRUE)
#' @param digits The number of decimal places to add to the path coefficients
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param labels Whether to show labels on the paths. "none", "labels", or "both" (parameter + label).
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = FALSE)
#' @param splines Whether to allow lines to curve: defaults to "TRUE" (nb: some models look better with "FALSE")
#' @param min optional list of objects to group at the top of the plot. Default (NULL) chooses automatically.
#' @param same optional list of objects to group at the same rank in the plot. Default (NULL) chooses automatically.
#' @param max optional list of objects to group at the bottom of the plot. Default (NULL) chooses automatically.
#' @param ... Optional parameters
#' @export
#' @seealso - [umx_set_plot_format()], [plot.MxModel()], [umxPlotACE()], [umxPlotCP()], [umxPlotIP()], [umxPlotGxE()]
#' @family Plotting functions
#' @references - <https://github.com/tbates/umx>, <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' plot(m1)
#' plot(m1, std = TRUE, resid = "line", digits = 3, strip_zero = FALSE)
#' 
#' # ============================================================
#' # = With a growth model, demonstrate splines= false to force =
#' # = straight lines, and move "rank" of intercept object      =
#' # ============================================================
#' 
#' m1 = umxRAM("grow", data = myGrowthMixtureData,
#'		umxPath(var = manifests, free = TRUE), 
#'		umxPath(means = manifests, fixedAt = 0), 
#'		umxPath(v.m. = c("int","slope")),
#'		umxPath("int", with = "slope"),
#'		umxPath("int", to = manifests, fixedAt = 1), 
#'		umxPath("slope", to = manifests, arrows = 1, fixedAt = c(0,1,2,3,4))
#' )
#'
#' plot(m1, means=FALSE, strip=TRUE, splines="FALSE", max="int")
#' } # end dontrun
#'
plot.MxModel <- function(x = NA, std = FALSE, fixed = TRUE, means = TRUE, digits = 2, file = "name", labels = c("none", "labels", "both"), resid = c("circle", "line", "none"), strip_zero = FALSE, splines = c("TRUE", "FALSE", "compound", "ortho", "polyline"), min= NULL, same= NULL, max= NULL, ...) {
	if(is.logical(splines)){ splines = ifelse(splines, "TRUE", "FALSE")}
	splines = match.arg(splines)

	# loop over submodels
	if(length(x@submodels)){
		n = 1
		for (sub in x@submodels) {
			if(file == "name"){
				thisFile = file
			} else {
				thisFile = paste0(file, "_group_", n)
			}
			plot.MxModel(sub, std = std, fixed = fixed, means = means, digits = digits, file = file, labels = labels, resid = resid, strip_zero = strip_zero, splines = splines, min= min, same= same, max= max, ...)
			n = n + 1
		}
	} else {
		# ==========
		# = Setup  =
		# ==========
		model   = x # just to be clear that x is a model
		resid   = match.arg(resid)
		labels  = match.arg(labels)
		latents = model@latentVars # 'vis', 'math', and 'text' 
		selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
	
		# Update values using compute = T to capture labels with [] references.
		# TODO: !!! Needs more work to sync with confidence intervals and SEs
		model$S$values = mxEval(S, model, compute = TRUE)
		model$A$values = mxEval(A, model, compute = TRUE)
		if(!is.null(model$M)){
			model$M$values = mxEval(M, model, compute = TRUE)
		}
	
		if(std){ model = xmu_standardize_RAM(model, return = "model") }

		# ========================
		# = Get Symmetric & Asymmetric Paths =
		# ========================
		out = "";
		out = xmu_dot_make_paths(model$matrices$A, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "Single arrow paths", digits = digits)
		if(resid == "circle"){
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "Covariances", digits = digits)
		} else if(resid == "line"){
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = TRUE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits)
		}else{
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits)		
		}
		# TODO should xmu_dot_make_residuals handle fixed or not necessary?
		tmp = xmu_dot_make_residuals(model$matrices$S, latents = latents, digits = digits, resid = resid)
		variances     = tmp$variances     # either "var_var textbox" or "var -> var port circles"
		varianceNames = tmp$varianceNames # names of residuals/variances. EMPTY if using circles 
		# =================
		# = Define shapes =
		# =================
		preOut = paste0('\tsplines="', splines , '";\n\t# Latents\n')

		for(var in latents) {
		   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}

		preOut = paste0(preOut, "\n\t# Manifests\n")
		for(var in selDVs) {
		   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
		}

	
		# ================
		# = handle means =
		# ================
		if(umx_has_means(model) & means){
			out = paste0(out, "\n\t# Means paths\n")
			# Add a triangle to the list of shapes
			preOut = paste0(preOut, "\t one [shape = triangle];\n")
			mxMat = model$matrices$M
			mxMat_vals   = mxMat$values
			mxMat_free   = mxMat$free
			mxMat_labels = mxMat$labels
			meanVars = colnames(mxMat$values)
			for(to in meanVars) {
				thisPathLabel = mxMat_labels[1, to]
				thisPathFree  = mxMat_free[1, to]
				thisPathVal   = round(mxMat_vals[1, to], digits)
				if(thisPathFree){
					labelStart = ' [label="' 
				} else {
					labelStart = ' [label="@'
				}

				# TODO plot.MxModel: Find way to show means fixed @0
				if(thisPathFree || fixed ) {
					# if(thisPathFree | (fixed & thisPathVal != 0) ) {
					out = paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n')
				}else{
					# cat(paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n'))
					# return(thisPathVal != 0)
				}
			}
		}

		# ===========================
		# = Make the variance lines =
		# ===========================
		# x1_var [label="0.21", shape = plaintext];
		# or (circles)
		# x1 -> x1 [label="0.21", direction = both];
		preOut = paste0(preOut, "\n\t#Variances/residuals\n")
		for(var in variances) {
		   preOut = paste0(preOut, "\t", var, ";\n")
		}
		# ======================
		# = Set the ranks e.g. =
		# ======================
		# {rank=same; x1 x2 x3 x4 x5 };
		# TODO more intelligence possible in plot() perhaps hints like "MIMIC" or "ACE"
		if(umx_has_means(model)){ append(varianceNames, "one")}

		# min = latents; same = selDVs; max = varianceNames
		x = xmu_dot_move_ranks(max = max, min = min, same=same, old_min = latents, old_same = selDVs, old_max = varianceNames)
		rankVariables = xmu_dot_rank_str(min = x$min, same = x$same, max = x$max)

		# ===================
		# = Selection paths =
		# ===================
		if(!is.null(model$expectation$selectionPlan)){
			# Draw each found "from-to" pair as a headless arrow on the diagram.
			
			selPaths = cbind(model$expectation$selectionPlan, value = model[["selectionVector"]]$values)
		  #   step from to     values
		  # 1    1   V1 V2 -0.2373752

			for(thisPath in 1:nrow(selPaths)) {
		 	    step  = selPaths[thisPath, "step"] # color
				color = c("red", "green", "blue")[step]
				from  = selPaths[thisPath, "from"]
				to    = selPaths[thisPath, "to"]
				val   = round(selPaths[thisPath, "value"], digits)
				newPath = paste0("\t", from, " ->", to, "[label='", val, "' color='", color, "' dir='none'];\n")
				out   = paste0(out, newPath)
			}			
		}
		
		# ===================================
		# = Assemble full text to write out =
		# ===================================
		label = model$name
		digraph = paste0(
			"digraph G {\n    ", 
			'label="', label, '";\n',
			preOut, "\n",
			out   , "\n",
			rankVariables, "\n}"
		)
		message("\n?plot.MxModel options: std, means, digits, strip_zero, file, splines=T/F/ortho,..., min=, max =, same = , fixed, resid= 'circle|line|none'")
		# cat(out)
		xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	}
} # end plot.MxModel

#' @export
plot.MxRAMModel <- plot.MxModel

#' Make a graphical display of an ACE model
#'
#' plot method for [umxACE()] models. Make a graphical display of an ACE model
#'
#' @aliases plot.MxModelACE
#' @param x [mxModel()] to plot (created by umxACE in order to inherit the MxModelACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param means Whether to show means paths (default is FALSE)
#' @param std Whether to standardize the model (default is TRUE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param showFixed Whether too draw fixed parameters.
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxACE()]
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE("plotACE example", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#' plot(m1, std = FALSE) # don't standardize
umxPlotACE <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, showFixed = FALSE, ...) {
	model = x # just to be clear that x is a model
	if(std){model = xmu_standardize_ACE(model)}

	nVar   = dim(model$top$a$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = sub("(_T)?[0-9]$", "", selDVs[1:(nVar)]) # trim "_Tn" from end
	out    = list(str = "", latents = c(), manifests = c())

	# 2. Factor correlations on the lower
	out = xmu_dot_mat2dot(model$top$a, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$c, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$e, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = xmu_dot_mat2dot(model$top$intercept , cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, p = out)
		out = xmu_dot_mat2dot(model$top$meansBetas, cells = "any" , toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, p = out)
	}

	# TODO: could add self-referential @1 loops to a, c, and e
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = out$manifests)

	same    = xmu_dot_rank(out$manifests, "."    , rank = "same")
	top     = xmu_dot_rank(out$latents  , "^a"   , rank = "min")
	bottom  = xmu_dot_rank(out$latents  , "^[ce]", rank = "max")

	label   = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, out$str, same, top, bottom, "\n}"
	)

	message("\n?umxPlotACE options: std=T/F, means=T/F, digits=n, strip_zero=T/F, file=, min=, max =")
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}
 # end umxPlotACE

#' @export
plot.MxModelACE <- umxPlotACE

#' Make a graphical display of an ACE model with covariates.
#'
#' Make a graphical display of an ACE model with covariates.
#'
#' @aliases plot.MxModelACEcov
#' @param x [mxModel()] to plot (created by umxACE in order to inherit the MxModelACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param means Whether to show means paths (default is FALSE)
#' @param std Whether to standardize the model (default is TRUE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE) 
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxACE()]
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' # BMI ?twinData from Australian twins. 
#' # Cohort 1 Zygosity 1 == MZ females 3 == DZ females
#' data(twinData)
#' 
#' # Pick the variables. We will use base names (i.e., "bmi") and set suffix.
#' selDVs  = c("bmi")
#' selCovs = c("ht")
#' selVars = umx_paste_names(c(selDVs, selCovs), sep = "", suffixes= 1:2)
#' # Just top few pairs so example runs quickly
#' mzData = subset(twinData, zygosity == "MZFF", selVars)[1:100, ]
#' dzData = subset(twinData, zygosity == "DZFF", selVars)[1:100, ]
#' m1 = umxACEcov(selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep= "")
#' plot(m1)
#' plot(m1, std = FALSE) # don't standardize
umxPlotACEcov <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, ...) {
	model = x # just to be clear that x is a model

	if(!class(x) == "MxModelACEcov"){
		stop("The first parameter of umxPlotACEcov must be an ACEcov model, you gave me a ", class(x))
	}

	if(std){model = umx_standardize(model)}

	# Relies on 'a' not having its dimnames stripped off...
	if(model$MZ$data$type == "raw"){
		selDVs = dimnames(model$top$a)[[1]]
		# selDVs = dimnames(model$MZ$data$observed)[[2]]
		selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end
	}else{
		stop("ACEcov has to have raw data...")
	}

	nVar    = length(selDVs)/2;
	selDVs  = selDVs[1:(nVar)]
	out     = "" ;
	latents = c();

	varCount = length(selDVs)
	parameterKeyList = omxGetParameters(model);
	for(thisParam in names(parameterKeyList) ) {
		value = parameterKeyList[thisParam]
		if(class(value) == "numeric") {
			value = round(value, digits)
		}
		if (grepl("^[ace]_r[0-9]+c[0-9]+", thisParam)) { # a c e
			show    = TRUE
			search  = '([ace])_r([0-9]+)c([0-9]+)'
			from    = sub(search, '\\1\\3', thisParam, perl = T); # a c or e
			target  = as.numeric(sub(search, '\\2', thisParam, perl = T)); # pull the row
			target  = selDVs[target]
			latents = append(latents, from)
		} else { # means probably
			if(means){
				show = TRUE
			} else {
				show = FALSE
			}
			selDVs
			from   = thisParam; # "one"
			target = sub('r([0-9])c([0-9])', 'var\\2', thisParam, perl=T)
		}
		if(show){
			out = paste0(out, from, " -> ", target, " [label = \"", value, "\"]", ";\n")
		}
	}
	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}

	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(var in selDVs[1:varCount]) {
	   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
	}
	rankVariables = paste("\t{rank = same; ", paste(selDVs[1:varCount], collapse = "; "), "};\n") # {rank = same; v1T1; v2T1;}
	# grep('a', latents, value=T)
	rankA   = paste("\t{rank = min; ", paste(grep('a'   , latents, value = T), collapse = "; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[ce]', latents, value = T), collapse = "; "), "};\n") # {rank=min; c1; e1}
	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut,
		out,
		rankVariables,
		rankA,
		rankCE, "\n}", sep=""
	)
	message("\n?umxPlotACEcov options: std=, means=, digits=, strip_zero=, file=, min=, max =")
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
} # end umxPlotACEcov

#' @export
plot.MxModelACEcov <- umxPlotACEcov

#' Plot the results of a GxE univariate test for moderation of ACE components.
#'
#' Plot GxE results (univariate environmental moderation of ACE components).
#' Options include plotting the raw and standardized graphs separately, or in a combined panel.
#' You can also set the label for the x axis (xlab), and choose the location of the legend.
#' 
#' *note*: If `gg=TRUE`, the plots are drawn in ggplot, and also returned as a `list(raw, std)` so you can edit them.
#'
#' @aliases plot.MxModelGxE
#' @param x A fitted [umxGxE()] model to plot
#' @param xlab String to use for the x label (default = NA, which will use the variable name)
#' @param location Where to plot the legend (default = "topleft")
#' see ?legend for alternatives like bottomright
#' @param separateGraphs (default = FALSE)
#' @param acergb Colors to use for plot c(a = "red", c = "green", e = "blue", tot = "black")
#' @param gg Use ggplot2 (default = TRUE)
#' @param moderatorValues If you want to pass in your own list of moderator values instead of the real ones in the data (Default = NULL)
#' @param ... Optional additional parameters
#' @return None 
#' @family Plotting functions
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxGxE()]
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' twinData$age1 = twinData$age2 = twinData$age
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1= umxGxE(selDVs= "bmi", selDefs= "age", dzData= dzData, mzData= mzData, sep="", tryHard="yes")
#' plot(m1)
#' # Directly call umxPlotGxE
#' umxPlotGxE(m1, xlab = "Age", separateGraphs = TRUE, gg = FALSE)
#' umxPlotGxE(m1, moderatorValues=18:67)
#' 
#' }
umxPlotGxE <- function(x, xlab = NA, location = "topleft", separateGraphs = FALSE, acergb = c("red", "green", "blue", "black"), gg = TRUE, moderatorValues= NULL, ...) {
	if(!class(x)[[1]] == "MxModelGxE"){
		stop("The first parameter of umxPlotGxE must be a GxE model, you gave me a ", class(x)[[1]])
	}
	model = x # to remind us that x has to be a umxGxE model
	mzData = model$MZ$data$observed
	dzData = model$DZ$data$observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = sub("(_T)?[0-9]$", "", selDefs[1])
	}
	# Get unique values of moderator
	mz1 = as.vector(mzData[,selDefs[1]])
	mz2 = as.vector(mzData[,selDefs[2]])
	dz1 = as.vector(dzData[,selDefs[1]])
	dz2 = as.vector(dzData[,selDefs[2]])

	if(is.null(moderatorValues)){
		defVarValues = sort(unique(c(mz1,mz2,dz1,dz2)))
	} else {
		defVarValues = moderatorValues
	}

	a   = model$top$matrices$a$values
	c   = model$top$matrices$c$values
	e   = model$top$matrices$e$values
	am  = model$top$matrices$am$values
	cm  = model$top$matrices$cm$values
	em  = model$top$matrices$em$values

	tmp = summary(model)$parameters[, c("name", "Estimate", "Std.Error")]
	aSE = tmp[tmp$name=="a_r1c1", "Std.Error"]
	cSE = tmp[tmp$name=="c_r1c1", "Std.Error"]
	eSE = tmp[tmp$name=="e_r1c1", "Std.Error"]

	amSE = tmp[tmp$name=="am_r1c1", "Std.Error"]
	cmSE = tmp[tmp$name=="cm_r1c1", "Std.Error"]
	emSE = tmp[tmp$name=="em_r1c1", "Std.Error"]

	Va  = (c(a) + c(am) * defVarValues)^2
	Vc  = (c(c) + c(cm) * defVarValues)^2
	Ve  = (c(e) + c(em) * defVarValues)^2

	VaUpper  = (c(a + 1.96*aSE) + c(am + 1.96*amSE) * defVarValues)^2
	VcUpper  = (c(c + 1.96*cSE) + c(cm + 1.96*cmSE) * defVarValues)^2
	VeUpper  = (c(e + 1.96*eSE) + c(em + 1.96*emSE) * defVarValues)^2
	VaLower  = (c(a - 1.96*aSE) + c(am - 1.96*amSE) * defVarValues)^2
	VcLower  = (c(c - 1.96*cSE) + c(cm - 1.96*cmSE) * defVarValues)^2
	VeLower  = (c(e - 1.96*eSE) + c(em - 1.96*emSE) * defVarValues)^2

	Vt  = Va + Vc + Ve
	out    = as.matrix(cbind(Va, Vc, Ve, Vt, VaUpper, VaLower, VcUpper, VcLower, VeUpper, VeLower))
	outStd = as.matrix(cbind(Va = Va/Vt, Vc = Vc/Vt, Ve = Ve/Vt))

	if(gg){
		tmp = data.frame(cbind(defVarValues, out))
		p = ggplot(data = tmp) 
		p = p + geom_ribbon(aes(x = defVarValues, ymin = VeLower, ymax = VeUpper), alpha = .2, fill = "blue" , show.legend= FALSE, linetype= 0)
		p = p + geom_ribbon(aes(x = defVarValues, ymin = VcLower, ymax = VcUpper), alpha = .2, fill = "green", show.legend= FALSE, linetype= 0)
		p = p + geom_ribbon(aes(x = defVarValues, ymin = VaLower, ymax = VaUpper), alpha = .2, fill = "red"  , show.legend= FALSE, linetype= 0)
		# p = p + geom_line(aes(x=defVarValues, y = Vt, group = 4, colour = 'Vtot'))
		p = p + geom_line(aes(x=defVarValues, y = Va, group = 1, colour = 'Va'))
		p = p + geom_line(aes(x=defVarValues, y = Vc, group = 2, colour = 'Vc'))
		p = p + geom_line(aes(x=defVarValues, y = Ve, group = 3, colour = 'Ve'))
		p = p + labs(x = xlab, y = 'Raw Variance', title = "Raw Moderation effects")
		raw = p + theme(legend.title = element_blank() ) + theme(legend.background = element_blank() ) + theme(legend.position = c(.1, .9))


		tmp = data.frame(cbind(defVarValues, outStd))
		p = ggplot(data = tmp, ylim = c(0,1)) 
		p = p + geom_line(aes(x=defVarValues, y = Va, group = 1, colour = 'Va'))
		p = p + geom_line(aes(x=defVarValues, y = Vc, group = 2, colour = 'Vc'))
		p = p + geom_line(aes(x=defVarValues, y = Ve, group = 3, colour = 'Ve'))
		p = p + labs(x = xlab, y = 'Standardized Variance', title = "Standardized Moderation effects")
		std = p + theme(legend.title = element_blank() ) + theme(legend.background = element_blank() ) + theme(legend.position = c(.1, .9))

		tmp = list(std, raw)
		if(separateGraphs){
			print(ggdraw(std))
			print(ggdraw(raw))
		}else{
			print(plot_grid(plotlist=tmp))
		}
		invisible(tmp)
		
	} else {
		if(!separateGraphs){
			graphics::par(mfrow = c(1, 2)) # one row * two columns to hold raw and std plots
		}
		graphics::matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = acergb, xlab = xlab, ylab = "Variance", main= "Raw Moderation Effects")
		graphics::legend(location, legend = c("genetic", "shared", "unique", "total"), lty = 1:4, col = acergb, bty = "n")
	
		graphics::matplot(defVarValues, outStd, type = "l", lty = 1:4, col = acergb, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main= "Standardized Moderation Effects")
		graphics::legend(location, legend = c("genetic", "shared", "unique"), lty = 1:4, col = acergb, bty = "n")
		graphics::par(mfrow = c(1, 1)) # back to black
	
	}
	
}

#' @export
plot.MxModelGxE <- umxPlotGxE

# TODO: umxPlotCPnew Add SEstyle code from plotCP
# TODO: umxPlotCPnew Add new label trimming code if necessary
#' Draw and display a graphical figure of Common Pathway model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' @aliases plot.MxModelCP
#' @param x The Common Pathway [mxModel()] to display graphically
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param showFixed Whether to graph paths that are fixed but != 0 (default = TRUE)
#' @param SEstyle report "b (se)" instead of "b \[lower, upper\]" when CIs are found (Default FALSE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxCP()]
#' @family Plotting functions
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' umx_set_optimizer("SLSQP")
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
# # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP("new", selDVs = selDVs, sep = "_T", 
#' 	dzData = dzData, mzData = mzData, nFac = 3
#' )
#' # m1 = mxTryHardOrdinal(m1)
#' umxPlotCP(m1)
#' plot(m1) # No need to remember a special name: plot works fine!
#' }
umxPlotCP <- function(x = NA, means = FALSE, std = TRUE, digits = 2, showFixed = TRUE, file = "name", format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	format = match.arg(format)
	model  = x # just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelCP", callingFn = "umxPlotCP")

	if(std){ model = xmu_standardize_CP(model) }

	nFac   = dim(model$top$a_cp$labels)[[1]]
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())
	# Process x_cp matrices
	# 1. Collect latents on the diag
	# from = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())
	out = xmu_dot_mat2dot(model$top$a_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$c_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$e_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out)

	# 2. Factor correlations on the lower
	# from = "<name><rowNum>"; target = "<name><colNum>"
	out = xmu_dot_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out)

	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = xmu_dot_mat2dot(model$top$cp_loadings, cells= "any", toLabel= selDVs, from= "cols", fromLabel= "common", fromType= "latent", showFixed = showFixed, p= out)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)

	# Process "as" matrix
	out = xmu_dot_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out)
	}
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace]_cp", "min")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s[0-9]+$", "max")
	
	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, 
		top, 
		bottom, 
		out$str, "\n}"
	)
	
	if(format != "current"){ umx_set_plot_format(format) }
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	# TODO umxPlotCP could tabulate thresholds?
	# Process "_dev" (where are these?)
	# cat(out$str)
}

#' @export
plot.MxModelCP <- umxPlotCP

#' Draw a graphical figure for a Independent Pathway model
#'
#' Options include digits (rounding), showing means or not, standardization, and which output format is desired.
#'
#' @aliases plot.MxModelIP
#' @param x The [umxIP()] model to plot
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param showFixed Whether to graph paths that are fixed but != 0 (default = TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR")
#' @param SEstyle Report "b (se)" instead of "b \[lower, upper\]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - optionally return the dot code
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxIP()]
#' @family Plotting functions
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 =    umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' plot(model)
#' umxPlotIP(model, file = NA)
#' }
umxPlotIP <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, showFixed = TRUE, format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	format = match.arg(format)
	model = x # Just to emphasize that x has to be a model 
	umx_check_model(model, "MxModelIP", callingFn = "umxPlotIP")
	
	if(std){
		model = xmu_standardize_IP(model)
	}

	# 1. get vars from rows of as matrix
	nFac   = dim(model$top$ai$labels)[[2]] # added! (but won't work, right? can have different numbers of a c, e )
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())

	# TODO Check I am handling nFac > 1 properly!!

	# from = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())

	# 1. Collect ai (the independent latent factors)
	out = xmu_dot_mat2dot(model$top$ai, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$ci, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$ei, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)

	# 2 collect as (the specific latent factors)
	out = xmu_dot_mat2dot(model$top$as, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$cs, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$es, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)


	# Process "expMean" 1 * nVar matrix e.g. "expMean_gff_T1"
	if(means){
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out)
	}
	
	# TODO: Could extract thresholds? "_dev[0-9]+$"

	# TODO Add CIs to parameter values
	# this code picks out the CIs if available... Now would need to be embedded in xmu_dot_mat2dot() now?
	# CIstr = xmu_get_CI(model, label = thisParam, prefix = "top.", suffix = "_std", digits = digits, SEstyle = SEstyle, verbose = FALSE)
	# CIstr = xmu_get_CI(model= tmp, label = "S[1,1]", prefix = "Holzinger_and_Swineford_1939.", SEstyle = TRUE, digits = 3)

	# ==============
	# = up to here =
	# ==============

	preOut = "\t# Latents\n"
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace]i", "min")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s", "max")

	label = model$name
	splines = "FALSE"

	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, 
		top, 
		bottom, 
		out$str, "\n}"
	)

	if(format != "current"){ umx_set_plot_format(format) }
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

#' @export
plot.MxModelIP <- umxPlotIP

#' Report modifications which would improve fit.
#'
#' This function uses the mechanical modification-indices approach to detect single paths which, if added
#' or dropped, would improve fit.
#' 
#' Notes:
#' 1. Runs much faster with full = FALSE (but this does not allow the model to re-fit around the newly-
#' freed parameter).
#' 2. Compared to mxMI, this function returns top changes, and also suppresses the run message.
#' 3. Finally, of course: see the requirements for (legitimate) post-hoc modeling in [mxMI()]
#' You are almost certainly doing better science when testing competing models rather than modifying a model to fit.
#' @param model An [mxModel()] for which to report modification indices
#' @param matrices which matrices to test. The default (NA) will test A & S for RAM models
#' @param full Change in fit allowing all parameters to move. If FALSE only the parameter under test can move.
#' @param numInd How many modifications to report. Use -1 for all. Default (NA) will report all over 6.63 (p = .01)
#' @param typeToShow Whether to shown additions or deletions (default = "both")
#' @param decreasing How to sort (default = TRUE, decreasing)
#' @seealso - [mxMI()]
#' @family Model Summary and Comparison
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' # umxMI(m1, full=FALSE)
umxMI <- function(model = NA, matrices = NA, full = TRUE, numInd = NA, typeToShow = "both", decreasing = TRUE) {
	if(typeToShow != "both"){
		message("Only showing both add and remove is supported so far")
	}
	if(is.na(matrices)){
		if(umx_is_RAM(model)){
			matrices = c("A", "S")
		}else{
			message("You need to tell me which matrices to test (this is not a RAM model, so I don't know.)")
		}
	}
	suppressMessages({MI = mxMI(model = model, matrices = matrices, full = full)})
	if(full){
		MIlist = MI$MI.Full
	} else {
		MIlist = MI$MI
	}
	if(is.na(numInd)){
		thresh = qchisq(p = (1 - 0.01), df = 1) # 6.63
		# check how many
		nSig = length(MIlist[MIlist > thresh])
		if(nSig < 1){
			# nothing significant, display top 3 or so
			mostPossible = length(MIlist)
			numInd = min(3, mostPossible)
			suggestions = sort(MIlist, decreasing = decreasing)[1:numInd]
		} else {
			suggestions = sort(MIlist[MIlist > thresh], decreasing = decreasing)
		}		
	} else {
		suggestions = sort(MIlist, decreasing = decreasing)[1:numInd]
	}
	print(suggestions)
	invisible(MI)
}

# ======================
# = Path tracing rules =
# ======================
#' umxUnexplainedCausalNexus
#'
#' umxUnexplainedCausalNexus report the effect of a change (delta) in a variable (from) on an output (to)
#'
#' @param from A variable in the model for which you want to compute the effect of a change.
#' @param delta A the amount to simulate changing \sQuote{from} by. 
#' @param to The dependent variable that you want to watch changing.
#' @param model The model containing variables from and to.
#' @seealso - [mxCheckIdentification()], [mxCompare()]
#' @family Advanced Model Building Functions
#' @references - https://github.com/tbates/umx/
#' @export
#' @md
#' @examples
#' \dontrun{
#' umxUnexplainedCausalNexus(from="yrsEd", delta = .5, to = "income35", model)
#' }
umxUnexplainedCausalNexus <- function(from, delta, to, model= NULL) {
	umx_check_model(model, type = "RAM")
	
	manifests = model@manifestVars
	partialDataRow = matrix(0, 1, length(manifests))  # add dimnames to allow access by name
	dimnames(partialDataRow) = list("val", manifests)
	partialDataRow[1, from]  = delta # delta is in raw "from" units
	partialDataRow[1, to]    = NA
	completedRow = umxConditionalsFromModel(model, partialDataRow, meanOffsets = TRUE)
	# by default, meanOffsets = FALSE, and the results take expected means into account
	return(completedRow[1, to])
}

umxConditionalsFromModel <- function(model, newData = NULL, returnCovs = FALSE, meanOffsets = FALSE) {
	# Usage: umxConditionalsFromModel(model, newData)
	# Original author: [Timothy Brick](https://github.com/tbates/umx/users/tbrick)
	# [history](https://github.com/tbates/umx/thread/2076)
	# Called by: umxUnexplainedCausalNexus
	# TODO: Special case for latent variables
	expectation = model$objective
	A = NULL
	S = NULL
	M = NULL
	
	# Handle missing data
	if(is.null(newData)) {
		data = model$data
		if(data$type != "raw") {
			stop("Conditionals requires either new data or a model with raw data.")
		}
		newData = data$observed
	}
	
	# New fit-function style
	eCov  = model$fitfunction$info$expCov
	eMean = model$fitfunction$info$expMean
	expectation = model$expectation
	if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
		A = eval(substitute(model$X$values, list(X=expectation$A)))
		S = eval(substitute(model$X$values, list(X=expectation$S)))
		if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation$M)) {
			M = eval(substitute(model$X$values, list(X=expectation$M)))
		}
	}

	if(!is.null(A)) {
		# RAM model: calculate total expectation
		I = diag(nrow(A))
		Z = solve(I-A)
		eCov = Z %*% S %*% t(Z)
		if(!is.null(M)) {
			eMean = Z %*% t(M)
		}
		latents = model@latentVars
		newData = data.frame(newData, matrix(NA, ncol=length(latents), dimnames=list(NULL, latents)))
	}
	
	# No means
	if(meanOffsets || !dim(eMean)[1]) {
		eMean = matrix(0.0, 1, ncol(eCov), dimnames=list(NULL, colnames(eCov)))
	}
	
	# TODO: Sort by pattern of missingness, lapply over patterns
	nRows = nrow(newData)
	outs = omxApply(newData, 1, umxComputeConditionals, sigma=eCov, mu=eMean, onlyMean=!returnCovs)
	if(returnCovs) {
		means = matrix(NA, nrow(newData), ncol(eCov))
		covs = rep(list(matrix(NA, nrow(eCov), ncol(eCov))), nRows)
		for(i in 1:nRows) {
			means[i,] = outs[[i]]$mu
			covs[[i]] = outs[[i]]$sigma
		}
		return(list(mean = means, cov = covs))
	}
	return(t(outs))
}

umxComputeConditionals <- function(sigma, mu, current, onlyMean = FALSE) {
	# Usage: umxComputeConditionals(model, newData)
	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	# original author: [Timothy Brick](https://github.com/tbates/umx/users/tbrick)
	# [history](https://github.com/tbates/umx/thread/2076)
	# called by umxConditionalsFromModel()
	if(dim(mu)[1] > dim(mu)[2] ) {
		mu = t(mu)
	}

	nVar = length(mu)
	vars = colnames(sigma)

	if(!is.matrix(current)) {
		current = matrix(current, 1, length(current), dimnames=list(NULL, names(current)))
	}
	
	# Check inputs
	if(dim(sigma)[1] != nVar || dim(sigma)[2] != nVar) {
		stop("Non-conformable sigma and mu matrices in conditional computation.")
	}
	
	if(is.null(vars)) {
		vars = rownames(sigma)
		if(is.null(vars)) {
			vars = colnames(mu)
			if(is.null(vars)) {
				vars = names(current)
				if(is.null(vars)) {
					vars = paste("X", 1:dim(sigma)[1], sep = "")
					names(current) = vars
				}
				names(mu) = vars
			}
			dimnames(sigma) = list(vars, vars)
		}
		rownames(sigma) = vars
	}
	
	if(is.null(colnames(sigma))) {
		colnames(sigma) = vars
	}
	
	if(is.null(rownames(sigma))) {
		rownames(sigma) = colnames(sigma)
	}

	if(!setequal(rownames(sigma), colnames(sigma))) {
		stop("Rows and columns of sigma do not match in conditional computation.")
	}
	
	if(!setequal(rownames(sigma), vars) || !setequal(colnames(sigma), vars)) {
		stop("Names of covariance and means in conditional computation fails.")
	}
	
	if(length(current) == 0) {
		if(onlyMean) {
			return(mu)
		}
		return(list(sigma=covMat, mu=current))
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			print(paste("Got data vector of length ", ncol(current), " and names of length ", length(vars)))
			stop("Length and names of current values mismatched in conditional computation.")
		}
		names(current) = vars[1:ncol(current)]
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			if(length(vars) == 0 || ncol(current) != length(vars)) {
				print(paste("Got mean vector of length ", ncol(current), " and names of length ", length(vars)))
				stop("Length and names of mean values mismatched in conditional computation.")
			}
		}
		names(mu) = vars
	}
	
	# Get Missing and Non-missing sets
	if(!setequal(names(current), vars)) {
		newSet          = setdiff(vars, names(current))
		current[newSet] = NA
		current         = current[vars]
	}
	
	# Compute Schur Complement
	# Calculate parts:
	missing    = names(current[is.na(current)])
	nonmissing = setdiff(vars, missing)
	ordering   = c(missing, nonmissing)
	
	totalCondCov = NULL

	# Handle all-missing and none-missing cases
	if(length(missing) == 0) {
		totalMean        = current
		names(totalMean) = names(current)
		totalCondCov     = sigma
	} 

	if(length(nonmissing) == 0) {
		totalMean = mu
		names(totalMean) = names(mu)
		totalCondCov = sigma
	}

	# Compute Conditional expectations
	if(is.null(totalCondCov)) {
		
		covMat   = sigma[ordering, ordering]
		missMean = mu[, missing]
		haveMean = mu[, nonmissing]

		haves    = current[nonmissing]
		haveNots = current[missing]

		missCov = sigma[missing, missing]
		haveCov = sigma[nonmissing, nonmissing]
		relCov  = sigma[missing, nonmissing]
		relCov  = matrix(relCov, length(missing), length(nonmissing))

		invHaveCov = solve(haveCov)
		condMean   = missMean + relCov %*% invHaveCov %*% (haves - haveMean)

		totalMean             = current * 0.0
		names(totalMean)      = vars
		totalMean[missing]    = condMean
		totalMean[nonmissing] = current[nonmissing]
	}

	if(onlyMean) {
		return(totalMean)
	}
	
	if(is.null(totalCondCov)) {
		condCov = missCov - relCov %*% invHaveCov %*% t(relCov)
	
		totalCondCov = sigma * 0.0
		totalCondCov[nonmissing, nonmissing] = haveCov
		totalCondCov[missing, missing] = condCov
	}	
	return(list(sigma = totalCondCov, mu = totalMean))
	
}

# =========================
# = Pull model components =
# =========================

#' Display path estimates from a model, filtering by name and value.
#'
#' @description
#' Often you want to see the estimates from a model, and often you don't want all of them.
#' [umxParameters()] helps in this case, allowing you to select parameters matching a name filter,
#' and also to only show parameters above or below a certain value.
#' 
#' If pattern is a vector, each regular expression is matched, and all unique matches to the whole vector are returned.
#'
#' @details
#' It is on my TODO list to implement filtering by significance, and to add standardizing.
#'
#' @param x an [mxModel()] or model summary from which to report parameter estimates.
#' @param thresh optional: Filter out estimates 'below' or 'above' a certain value (default = "all").
#' @param b Combine with thresh to set a minimum or maximum for which estimates to show.
#' @param pattern Optional string to match in the parameter names. Default '.*' matches all. [regex()] allowed!
#' @param std Standardize output: NOT IMPLEMENTED YET
#' @param digits Round to how many digits (2 = default).
#' @return - list of matching parameters, filtered by name and value
#' @export
#' @family Reporting Functions
#' @seealso - [umxGetParameters()], [umxSummary()], [namez()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("OneFactor", data = demoOneFactor,
#' 	umxPath(from = "G", to = manifests), # factor loadings
#' 	umxPath(v.m. = manifests),           # residual variance
#' 	umxPath(v1m0 = "G")                  # standardized latent
#' )
#' # Parameters with values below .1
#' umxParameters(m1, "below", .1)
#' # Parameters with values above .5
#' umxParameters(m1, "above", .5)
#' # Parameters with values below .1 and containing "_to_" in their label
#' umxParameters(m1, "below", .1, "_to_")
umxParameters <- function(x, thresh = c("all", "above", "below", ">", "<", "NS", "sig"), b = NULL, pattern = ".*", std = FALSE, digits = 2) {
	# TODO clarify when to use parameters vs. umxGetParameters
	# TODO Add filtering by significance (based on SEs)
	# TODO Offer a method to handle sub-models
	# 	model$aSubmodel$matrices$aMatrix$labels
	# 	model$MZ$matrices
	
	# x = cp4
	if(class(thresh) == "numeric"){
		stop("You might not have specified the parameter value (b) by name. e.g.:\n
	parameters(cp4, pattern = '_cp_', thresh = 'below', b = .1)\n
or specify all arguments:\n
	parameters(cp4, 'below', .1, '_cp_')
		")
	}
	thresh = match.arg(thresh)

	if(!is.null(b) && (thresh == "all")){
		message("Ignoring b (cutoff) and thresh = all. Set above or below to pick a beta to cut on.")
	}

	if(std){
		x = umx_standardize(x)
	}

	if(class(x) != "summary.mxmodel"){
		if(!umx_has_been_run(x)){
			# message("Just a note: Model has not been run. That might not matter for you")
		}
		# model that may or may not have been run: make up a similar dataframe to what we get from summary$parameters
		x = omxGetParameters(x)
		x = data.frame(name = names(x), Estimate = as.numeric(x), stringsAsFactors = FALSE)
	} else {
		x = x$parameters
	}

	# Handle 1 or more regular expressions.
	parList = c()
	for (i in 1:length(pattern)) {
		parList = c(parList, umx_names(x$name, pattern = pattern[i]))
	}
	parList = unique(parList)
	
	if(thresh %in%  c("above", ">") ){
		filter = x$name %in% parList & abs(x$Estimate) > b
	} else if(thresh %in% c("below", "<")){
		filter = x$name %in% parList & abs(x$Estimate) < b
	} else if(thresh == "all"){
		filter = x$name %in% parList
	} else if(thresh == "NS"){
		stop("NS not yet implemented in 'parameters' : email maintainer('umx') to get this done.")
	} else if(thresh == "sig"){
		stop("sig not yet implemented in 'parameters': email maintainer('umx') to get this done.")
	}

	if(sum(filter) == 0){
		message(paste0("Nothing found matching pattern ", omxQuotes(pattern), " and minimum absolute value ", thresh, " ", b, "."))
		
		paste0("Might try flipping the from and to elements of the name, or look in these closest matches for what you intended: ",
			omxQuotes(agrep(pattern = pattern, x = x$name, max.distance = 4, value = TRUE))
		)
	} else {
		umx_round(x[filter, c("name", "Estimate")], digits = digits)
	}
}

#' @rdname umxParameters
#' @export
parameters <- umxParameters

#' Get parameters from a model, with support for pattern matching!
#'
#' umxGetParameters retrieves parameter labels from a model, like [omxGetParameters()].
#' However, it is supercharged with regular expressions, so you can get labels that match a pattern.
#' 
#' In addition, if regex contains a vector, this is treated as a list of raw labels to search for, 
#' and return if all are found.
#' \emph{note}: To return all labels, just leave regex as is.
#'
#' @param inputTarget An object to get parameters from: could be a RAM [mxModel()]
#' @param regex A regular expression to filter the labels. Default (NA) returns all labels. If vector, treated as raw labels to find.
#' @param free  A Boolean determining whether to return only free parameters.
#' @param fetch What to return: "labels" (default) or "values", "free", "lbound", "ubound", or "all"
#' @param verbose How much feedback to give
#' @export
#' @seealso [omxGetParameters()], [parameters()]
#' @family Reporting Functions
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' 
#' # Show all parameters
#' umxGetParameters(m1)
#' umxGetParameters(m1, free = TRUE)  # Only free parameters
#' umxGetParameters(m1, free = FALSE) # Only fixed parameters
#' # Complex regex pattern
#' umxGetParameters(m1, regex = "x[1-3]_with_x[2-5]", free = TRUE)
#' 
umxGetParameters <- function(inputTarget, regex = NA, free = NA, fetch = c("labels", "values", "free", "lbound", "ubound", "all"), verbose = FALSE) {
	# TODO
	# 1. Be nice to offer a method to handle sub-models
	# 	model$aSubmodel$matrices$aMatrix$labels
	# 	model$MZ$matrices
	# 2. Simplify handling
		# allow umxGetParameters to function like omxGetParameters()[name filter]
	# 3. Allow user to request values, free, etc. (already done with umx_parameters)
	fetch = match.arg(fetch)
	umx_check(fetch=="labels", action="stop",message="polite stop: You asked for ", omxQuotes(fetch), ". umxGetParameters only supports fetching labels at present: Make an issue on github to support fetching these, or try parameters() ")
	if(umx_is_MxModel(inputTarget)) {
		topLabels = names(omxGetParameters(inputTarget, indep = FALSE, free = free))
	} else if(methods::is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget$labels
		} else {
			topLabels = inputTarget$labels[inputTarget$free==free]
		}
	}else{
		stop("I am sorry Dave, umxGetParameters needs either a model or an mxMatrix: you offered a ", class(inputTarget)[1])
	}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( length(regex) > 1 || !is.na(regex) ) {
		if(length(regex) > 1){
			# Assume regex is a list of labels
			theLabels = theLabels[theLabels %in% regex]
			if(length(regex) != length(theLabels)){
				msg = "Not all labels found! Missing were:\n"
				stop(msg, regex[!(regex %in% theLabels)]);
			}
		} else {
			# It's a grep string
			theLabels = grep(regex, theLabels, perl = FALSE, value = TRUE) # Return more detail
		}
		if(length(theLabels) == 0){
			msg = paste0("Found no labels matching", omxQuotes(regex), "!\n")
			if(umx_is_MxModel(inputTarget)){
				msg = paste0(msg, "\nUse umxGetParameters(", deparse(substitute(inputTarget)), ") to see all parameters in the model")
			}else{
				msg = paste0(msg, "\nUse umxGetParameters() without a pattern to see all parameters in the model")
			}
			stop(msg);
		}
	}
	return(theLabels)
}


#' Extract AIC from MxModel
#'
#' Returns the AIC for an OpenMx model.
#' Original Author: Brandmaier
#'
#' @method extractAIC MxModel
#' @rdname extractAIC.MxModel
#' @export
#' @param fit an fitted [mxModel()] from which to get the AIC
#' @param scale not used
#' @param k not used
#' @param ... any other parameters (not used)
#' @return - AIC value
#' @seealso - [AIC()], [umxCompare()], [logLik()]
#' @family Reporting functions
#' @references - \url{https://openmx.ssri.psu.edu/thread/931#comment-4858}
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' extractAIC(m1)
#' # -2.615998
#' AIC(m1)
#' }
extractAIC.MxModel <- function(fit, scale, k, ...) {
	a = mxCompare(fit)
	return(a[1, "AIC"])
}

#' Get the expected vcov matrix
#'
#' Extract the expected covariance matrix from an [mxModel()]
#'
#' @aliases vcov.MxModel
#' @param object an [mxModel()] to get the covariance matrix from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. NULL (Default) = no rounding.
#' @param ... extra parameters (to match [vcov()])
#' @return - expected covariance matrix
#' @export
#' @family Reporting functions
#' @references - \url{https://openmx.ssri.psu.edu/thread/2598}
#' Original written by \url{https://openmx.ssri.psu.edu/users/bwiernik}
#' @seealso - [umxRun()], [umxCI_boot()]
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )#'
#' vcov(m1) # supplied by OpenMx
#' umxExpCov(m1, digits = 3)
umxExpCov <- function(object, latents = FALSE, manifests = TRUE, digits = NULL, ...){
	# umx_has_been_run(m1)
	# TODO integrate with mxGetExpected(model, "covariance")
	# mxGetExpected(m1, component= c("means", "covariance", "standVector") )
	if(object$data$type == "raw"){
		manifestNames = names(object$data$observed)
	} else {
		manifestNames = dimnames(object$data$observed)[[1]]
	}
	if(umx_is_RAM(object)){
		if(manifests & !latents){
			thisFit = paste0(object$name, ".fitfunction")
			expCov  = attr(object$output$algebras[[thisFit]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		} else {
			A = mxEval(A, object)
			S = mxEval(S, object)
			I = diag(1, nrow(A))
			E = solve(I - A)
			expCov = E %&% S # The model-implied covariance matrix
			mV = NULL
			if(latents) {
				mV = object@latentVars 
			}
			if(manifests) {
				mV = c(mV, object@manifestVars)
			}
			expCov = expCov[mV, mV]
		}
	} else {
		if(latents){
			stop("I don't know how to reliably get the latents for non-RAM objects... Sorry :-(")
		} else {
			expCov = attr(object$output$algebras[[paste0(object$name, ".fitfunction")]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		}
	}
	if(!is.null(digits)){
		expCov = round(expCov, digits)
	}
	return(expCov) 
}

# #' @export
# vcov.MxModel <- umxExpCov


#' Extract the expected means matrix from an [mxModel()]
#'
#' Extract the expected means matrix from an [mxModel()]
#'
#' @param model an [mxModel()] to get the means from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Default (NULL) will not round at all.
#' @return - expected means
#' @export
#' @family Reporting functions
#' @references - <https://openmx.ssri.psu.edu/thread/2598>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor,
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' umxExpMeans(m1)
#' umxExpMeans(m1, digits = 3)
umxExpMeans <- function(model, manifests = TRUE, latents = NULL, digits = NULL){
	umx_check_model(model, beenRun = TRUE)
	if(!umx_has_means(model)){
		stop("Model has no means expectation to get: Are there any means in the data? (type='raw', or type = 'cov' with means?)")
	}
	
	if(umx_is_RAM(model)){
		# TODO something nice to do here?
	}
	if(!is.null(latents)){
		# TODO should a function called expMeans get expected means for latents... why not.
		stop("Haven't thought about getting means for latents yet... Bug me about it :-)")
	}
	expMean = attr(model$output$algebras[[paste0(model$name, ".fitfunction")]], "expMean")
	
	if(model$data$type == "raw"){
		manifestNames = names(model$data$observed)
	} else {
		manifestNames = dimnames(model$data$observed)[[1]]
	}
	dimnames(expMean) = list("mean", manifestNames)
	if(!is.null(digits)){
		expMean = round(expMean, digits)
	}
	return(expMean)
}


# Define generic RMSEA...
#' Generic RMSEA function
#'
#' See [RMSEA.MxModel()] to access the RMSEA of MxModels
#'
#' @param x an object from which to get the RMSEA 
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show
#' @return - RMSEA object containing value (and perhaps a CI)
#' @md
#' @export
#' @family Reporting functions
RMSEA <- function(x, ci.lower, ci.upper, digits) UseMethod("RMSEA", x)

#' RMSEA function for MxModels
#'
#' Return RMSEA and its confidence interval on a model.
#' RMSEA(tmp, silent=TRUE)$RMSEA
#'
#' @rdname RMSEA.MxModel
#' @param x an [mxModel()] from which to get RMSEA
#' @param ci.lower the lower CI to compute (only .05 supported)
#' @param ci.upper the upper CI to compute (only .95 supported)
#' @param digits digits to show (default = 3)
#' @return - object containing the RMSEA, lower and upper bounds, and p-close
#' @export
#' @family Reporting functions
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' RMSEA(m1)
#' 
#' x = RMSEA(m1)
#' x$RMSEA # 0.0309761
#'
#' # Raw: needs to be run by umx to get RMSEA
#' m2 = umxRAM("One Factor", data = demoOneFactor,
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' RMSEA(m2)
#' }
RMSEA.MxModel <- function(x, ci.lower = .05, ci.upper = .95, digits = 3) { 
	model = x
	if(is.null(model$output$SaturatedLikelihood)){
		# no ref models in summary... compute them
		# no SaturatedLikelihood, compute refModels
		refModels = tryCatch({
		    refModels = mxRefModels(model, run = TRUE)
		}, warning = function(x) {
		    print("warning in RMSEA.MxModel calling mxRefModels: mxRefModels can't handle all designs, including twin, and WLS https://github.com/OpenMx/OpenMx/issues/184")
		}, error = function(x) {
		    print("error RMSEA.MxModel calling mxRefModels: mxRefModels can't handle all designs, including twin, and WLS https://github.com/OpenMx/OpenMx/issues/184")
		}, finally={
		    # print("cleanup-code")
		})
		if(!class(refModels)=="list"){
			modelSummary = summary(model)
		} else {
			modelSummary = summary(model, refModels = refModels)
		}
	}else{
		modelSummary = summary(model)		
	}
	RMSEA.summary.mxmodel(x= modelSummary, ci.lower = ci.lower, ci.upper = ci.upper, digits = digits)
}

#' RMSEA function for MxModels
#'
#' Compute the confidence interval on RMSEA and print it out. 
#' *note*: If your goal is to extract the RMSEA from a model, use `RMSEA(m1)$RMSEA`
#'
#' @param x an [mxModel()] summary from which to get RMSEA
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show (defaults to 3)
#' @return - object containing the RMSEA and lower and upper bounds
#' @rdname RMSEA.summary.mxmodel
#' @export
#' @family Reporting functions
#' @references - <https://github.com/simsem/semTools/wiki/Functions>, <https://github.com/tbates/umx>
#' @md
#' @examples
#' 
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor[1:100,], type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' tmp = summary(m1)
#' RMSEA(tmp)
#' }
RMSEA.summary.mxmodel <- function(x, ci.lower = .05, ci.upper = .95, digits = 3){
	summary = x # x is a model summary
	if(ci.lower != .05 | ci.upper != .95){
		stop("only 95% CI on RMSEA supported as yet...")
	}
	if(is.na(summary$RMSEA) || is.null(summary$RMSEA)){
		message("model summary has no RMSEA, sorry - you might need to run ref models?")
		return(NA)
	} else {
		output = list(RMSEA = summary$RMSEA, CI.lower = summary$RMSEACI["lower"], CI.upper = summary$RMSEACI["upper"], RMSEA.pvalue = summary$RMSEAClose)
		class(output) = "RMSEA"
		attr(output, 'digits')   = digits
		return(output)
	}
}

#' Print a RMSEA object
#'
#' Print method for "RMSEA" objects: e.g. [umx::RMSEA()]. 
#'
#' @param x RMSEA object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::RMSEA()], [print()]
#' @md
#' @method print RMSEA
#' @export
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type= "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' tmp = summary(m1)
#' RMSEA(tmp)
#' }
#'
print.RMSEA <- function(x, ...) {
	# x is an RMSEA object
	if(!is.null(attr(x, 'digits')) ){
		digits = attr(x, 'digits')
	}

	txt = paste0("RMSEA = ", round(x$RMSEA, digits))
	txt = paste0(txt, " CI[")
	txt = paste0(txt, round(x$CI.lower, digits), ", ")
	txt = paste0(txt, round(x$CI.upper, digits), "], ")
	txt = paste0(txt, "Prob(RMSEA <= 0.05) = ", umx_APA_pval(x$RMSEA.pvalue))
	cat(txt)
}

# ===================================
# = Summary Stats and table helpers =
# ===================================

#' Fishers Method of combining p-values.
#'
#' @description
#' `FishersMethod` implements R.A. Fisher's (1925) method for creating a meta-analytic p-value by combining a 
#' set of p-values from tests of the same hypothesis in independent samples. See also Stouffer's method 
#' for combining Z scores, which allows weighting.
#' @param pvalues A vector of p-values, e.g. c(.041, .183)
#' @param ... More p-values if you want to offer them up one by one instead of wrapping in a vector for `pvalues`
#' @return - A meta-analytic p-value
#' @export
#' @family Miscellaneous Stats Functions
#' @references - Fisher, R.A. (1925). *Statistical Methods for Research Workers*. Oliver and Boyd (Edinburgh). ISBN 0-05-002170-2.
#' * Fisher, R. A (1948). "Questions and answers #14". *The American Statistician*. **2**: 30–31. doi: https://doi.org/10.2307/2681650.
#' * Stouffer, S. A. and Suchman, E. A. and DeVinney, L. C. and Star, S. A. and Williams, R. M. Jr. (1949) The American Soldier, 
#' Vol. 1 - Adjustment during Army Life. Princeton, Princeton University Press.
#' @md
#' @examples
#' FishersMethod(c(.041, .378))
FishersMethod <- function(pvalues, ...){
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	pvalues = c(pvalues, unlist(dot.items))
	pchisq( -2 * sum(log(pvalues)), df= (2 * length(pvalues)), lower.tail = FALSE)
}

#' Geometric Mean
#'
#' @description
#' The Geometric mean is the nth-root of the product of `n` input values.
#' Common uses include computing economic utility.  That is to say, the utility of 
#' `c(1, 2, 10)` is \deqn{(1 * 2 * 10)/3} = 6.6 not 6.3 (the arithmetic mean).
#' 
#' @param x A vector of values.
#' @param na.rm remove NAs by default.
#' @return - Geometric mean of x
#' @export
#' @family Miscellaneous Stats Functions
#' @references - <https://en.wikipedia.org/wiki/Geometric_mean>
#' @md
#' @examples
#' geometric_mean(c(50, 100))
#'
#' # For a given sum, geometric mean is maximised with equality
#' geometric_mean(c(75,75))
#'
#' v = c(1, 149); c(sum(v), geometric_mean(v), mean(v), median(v))
#' # 150.00000  12.20656  75.00000  75.00000
#' 
#' # Underlying logic
#' sqrt(50 * 100)
#' 
#' # Alternate form using logs
#' exp(mean(log(c(50 *100))))
#' 
#' # Reciprocal duality
#' 1/geometric_mean(c(100, 50))
#' geometric_mean(c(1/100, 1/50))
geometric_mean = function(x, na.rm = c(TRUE, FALSE)){
	na.rm = xmu_match.arg(na.rm, option_list= c(TRUE, FALSE), check = TRUE)
	exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

#' Harmonic Mean
#'
#' @description
#' The harmonic mean is the reciprocal of the arithmetic mean of the reciprocals of the input values.
#' Common uses include computing the mean of ratios, for instance the average P/E ratio in a portfolio. 
#' Also it is the correct mean for averaging speeds weighted for distance.
#' 
#' @param x A vector of values to take the harmonic mean for
#' @param weights Optional vector of weights.
#' @param na.rm remove NAs (default = TRUE).
#' @return - Harmonic mean of x
#' @export
#' @family Miscellaneous Stats Functions
#' @references - <https://en.wikipedia.org/wiki/Harmonic_mean>
#' @md
#' @examples
#' # Harmonic means are suitable for ratios
#' tmp = c(33/1, 23/1)
#' harmonic_mean(tmp)
#' 
#' geometric_mean(tmp)
#' mean(tmp)
#'
#' # Example with weights
#' harmonic_mean(c(33/1, 23/1), weights= c(.2, .8))
#' # If Jack travels outbound at 1 mph, and returns at 10 miles an hour, what is his average speed?
#' harmonic_mean(c(1,10)) # 1.81 mph
#' 
harmonic_mean = function(x, weights = NULL, na.rm = c(TRUE, FALSE)){
	na.rm = xmu_match.arg(na.rm, option_list= c(TRUE, FALSE), check = TRUE)
	if(na.rm){
		x = x[!is.na(x)]
		weights = weights[!is.na(weights)]
	}
	if(is.null(weights)){
		# reciprocal of the arithmetic mean of the reciprocals of the input values
		h = 1/mean(1/x)
	} else {
		h = sum(weights)/sum(weights/x)
	}
	return(h)
}

#' Summarizing functions used in umx_aggregate and for umxAPA
#'
#' Miscellaneous functions that are handy in summary and other tasks where you might otherwise have
#' to craft a custom nameless functions. e.g.
#' 
#' * [umx_fun_mean_sd()]: returns "mean (SD)" of x.
#'
#' *note*: if a factor is given, then the mode is returned instead of the mean and SD.
#' @param x input
#' @param na.rm How to handle missing (default = TRUE = remove)
#' @param digits Rounding (default = 2)
#' @return - function result
#' @export
#' @family xmu internal not for end user
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' umxAPA(mtcars[,1:3]) # uses umx_fun_mean_sd
umx_fun_mean_sd = function(x, na.rm = TRUE, digits = 2){
	if(!is.numeric(x)){
		paste0("mode = ", names(which.max(table(x))))
	} else {
		paste0(
			round(mean(x, na.rm = na.rm), digits), " ",
			"(", round(sd(x, na.rm = na.rm),digits = digits), ")"
	)
	}
}

#' Convenient formula-based cross-tabs & built-in summary functions
#'
#' @description
#' A common task is preparing summary tables, aggregating over some grouping factor.
#' Like mean and sd of age, by sex. R's [aggregate()] function is useful and powerful, allowing
#' xtabs based on a formula.
#' 
#' umx_aggregate makes using it a bit easier. In particular, it has some common functions 
#' for summarizing data built-in, like "mean (sd)" (the default).
#' 
#' \code{umx_aggregate(mpg ~ cyl, data = mtcars, what = "mean_sd")}
#' 
#' \tabular{ll}{
#' cyl        \tab mpg\cr
#' 4 (n = 11) \tab 26.66 (4.51)\cr
#' 6 (n = 7)  \tab 19.74 (1.45)\cr
#' 8 (n = 14) \tab 15.1 (2.56)\cr
#' }
#'
#' @param formula The aggregation formula. e.g., DV ~ condition.
#' @param data frame to aggregate (defaults to df for common case)
#' @param what function to use. Default reports "mean (sd)".
#' @param digits to round results to.
#' @param report Format for the table: Default is markdown.
#' @return - table
#' @export
#' @family Reporting Functions
#' @seealso - [umx_apply()], [aggregate()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' # =====================================
#' # = Basic use, compare with aggregate =
#' # =====================================
#' aggregate(mpg ~ cyl, FUN = mean, na.rm = TRUE, data = mtcars)
#' umx_aggregate(mpg ~ cyl, data = mtcars)
#' 
#' # =============================================
#' # = Use different (or user-defined) functions =
#' # =============================================
#' umx_aggregate(mpg ~ cyl, data = mtcars, what = "n")
#' umx_aggregate(mpg ~ cyl, data = mtcars, what = function(x){sum(!is.na(x))})
#' 
#' # turn off markdown
#' umx_aggregate(mpg ~ cyl, data = mtcars, report = "txt")
#' 
#' # ============================================
#' # = More than one item on the left hand side =
#' # ============================================
#' umx_aggregate(cbind(mpg, qsec) ~ cyl, data = mtcars, digits = 3)
#' # Transpose table
#' t(umx_aggregate(cbind(mpg, qsec) ~ cyl, data = mtcars))
#' 
#' \dontrun{
#' umx_aggregate(cbind(moodAvg, mood) ~ condition, data = study1)
#' }
umx_aggregate <- function(formula = DV ~ condition, data = df, what = c("mean_sd", "n"), digits = 2, report = c("markdown", "html", "txt")) {
	report = match.arg(report)
	what = xmu_match.arg(what, c("mean_sd", "n"), check = FALSE)
	# TODO: add summaryBy ability to handle more than var on the left hand side
	# 	output odds or odds ratios for binary?
	# doBy::summaryBy(Sex_T1 + Sex_T2 ~ zyg, data = twinData, FUN = function(x) { round(c(
	# 	n    = length(x),
	# 	mean = mean(x, na.rm = T),
	# 	sd   = sd(x, na.rm = T)), 2)
	# })
	# TODO: add "sep" to umx_aggregate to make wide data long for summary as in genEpi_TwinDescriptives
	# genEpi_TwinDescriptives(mzData = twinData, dzData = NULL, selDVs = selDVs, groupBy = c("Sex_T1", "Sex_T2"), graph = F)
	# genEpi_twinDescribe(twinData, varsToSummarize="Age", groupBy="Sex", suffix="_T")

	mean_sd = function(x){
		if(is.numeric(x)){
			paste0(round(mean(x, na.rm = TRUE), digits = digits), " (",
				   round(sd(x, na.rm = TRUE), digits = digits), ")"
			)
		} else {
			paste0(names(table(x))," ", table(x), collapse = "; ")
		}
	}
	x_n = function(x){sum(!is.na(x))}

	if(class(what)=="function"){
		FUN = what
	} else if(class(what) != "character"){
		stop("umx_aggregate what should be a built-in name like 'mean_sd', or a function, you gave me a", class(what))
	} else if(what == "mean_sd"){
		FUN = mean_sd
	} else if(what == "n"){
		FUN = x_n
	}
	tmp = aggregate(formula, FUN = FUN, data = data)
	n_s = aggregate(formula, FUN = x_n, data = data)
	tmp = data.frame(tmp)
	tmp[, 1] = paste0(as.character(tmp[, 1]), " (n = ", n_s[, 2], ")")
	if(report == "html"){
		umx_print(tmp, digits = digits, file = "tmp.html")
	} else if(report == "markdown"){
		return(kable(tmp, format="pipe"))
	}else{
		return(tmp)
	}
}

#' Round p-values according to APA guidelines
#'
#' @description
#' `umx_APA_pval` formats p-values, rounded in APA style. So you get '< .001' instead of .000000002 or 1.00E-09.
#' 
#' You probably would be better off using [umxAPA()], which handles many more object types.
#' 
#' You set the precision with digits. Optionally, you can add '=' '<' etc. The default for addComparison (NA) adds these when needed.
#'
#' @param p The p-value to round
#' @param min Values below min will be reported as "< min"
#' @param digits Number of decimals to which to round (default = 3)
#' @param addComparison Whether to add '=' '<' etc. (NA adds when needed)
#' @family xmu internal not for end user
#' @return - p-value formatted in APA style
#' @export
#' @seealso - [umxAPA()], [round()]
#' @md
#' @examples
#' umx_APA_pval(.052347)
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(1.23E-4)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = TRUE)
umx_APA_pval <- function(p, min = .001, digits = 3, addComparison = NA) {
	# Typically use umxAPA?
	if(length(p) > 1){
		o = rep(NA, length(p))
		for(i in seq_along(p)) {
		   o[i] = umx_APA_pval(p[i], min = min, digits = digits, addComparison = addComparison)
		}
		return(o)
	} else {
		if(is.nan(p) | is.na(p)){
			if(is.na(addComparison)){
				return(p)
			}else if(addComparison){
				return(paste0("= ", p))
			} else {
				return(p)
			}
		}
		if(p < min){
			if(is.na(addComparison)){
				return(paste0("< ", min))
			}else if(addComparison){
				return(paste0("< ", min))
			} else {
				return(min)
			}
		} else {
			if(is.na(addComparison)){
				return(format(round(p, digits), scientific = FALSE, nsmall = digits))
			}else if(addComparison){				
				return(paste0("= ", format(round(p, digits), scientific = FALSE, nsmall = digits)))
			} else {
				return(round(p, digits))
			}
		}	
	}
}

#' Creates nicely formatted journal-style summaries of models, p-values, data-frames and much more.
#'
#' @description
#' `umxAPA` creates APA-style reports from a range of statistical models, or to summarize data.
#' 
#' 1. Given an [stats::lm()] model, `umxAPA` will return a formatted effect, including 95% CI. 
#' e.g.: `umxAPA(lm(mpg~wt, data=mtcars), "wt")` yields: \eqn{\beta} = -5.34 \[-6.48, -4.20\], p < 0.001. here "wt" 
#' restricts the output to just the named effect.
#' 2. `umxAPA` also supports [t.test()], [stats::glm()], [cor.test()], and others as I need them.
#' 3. Get a CI from `obj=beta` and se=se : `umxAPA(-0.30, .03)` returns \eqn{\beta} = -0.3 \[-0.36, -0.24\]
#' 4. Back out an SE from \eqn{\beta} and CI: `umxAPA(-0.030, c(-0.073, 0.013))` returns \eqn{\beta} = -0.03, se = 0.02
#' 5. Given only a number as obj, will be treated as a p-value, and returned in APA format.
#' 6. Given a dataframe, `umxAPA` will return a table of correlations with means and SDs in the last row. e.g.:
#' `umxAPA(mtcars[,c("cyl", "wt", "mpg", )]` yields:
#'   \tabular{lccc}{
#'            \tab cyl         \tab  wt          \tab mpg          \cr
#'    cyl     \tab 1           \tab  0.78        \tab -0.85        \cr
#'    wt      \tab 0.78        \tab  1           \tab -0.87        \cr
#'    mpg     \tab -0.85       \tab  -0.87       \tab 1            \cr
#'    mean_sd \tab 6.19 (1.79) \tab  3.22 (0.98) \tab 20.09 (6.03)
#'   }
#'
#' @aliases summaryAPA
#' @param obj A model (e.g. [lm()], [lme()], [glm()], [t.test()]), beta-value, or [data.frame]
#' @param se If obj is a beta, se treated as standard-error (returning a CI). 
#' If obj is a model, used to select effect of interest (blank for all effects). 
#' Finally, set se to the CI c(lower, upper), to back out the SE.
#' @param p If obj is a beta, use p-value to compute SE (returning a CI).
#' @param std Whether to report std betas (re-runs model on standardized data).
#' @param digits How many digits to round output.
#' @param use If obj is a data.frame, how to handle NAs (default = "complete")
#' @param min For a p-value, the smallest value to report numerically (default .001)
#' @param addComparison For a p-value, whether to add "</=" default (NA) adds "<" if necessary
#' @param report What to return (default = 'markdown'). Use 'html' to open a web table.
#' @param lower Whether to not show the lower triangle of correlations for a data.frame (Default TRUE)
#' @param SEs Whether or not to show correlations with their SE (Default TRUE)
#' @param means Whether or not to show means in a correlation table (Default TRUE)
#' @param test If obj is a glm, which test to use to generate p-values options = "Chisq", "LRT", "Rao", "F", "Cp"
#' @return - string
#' @export
#' @seealso [SE_from_p()]
#' @family Reporting Functions
#' @references - <https://stats.idre.ucla.edu/r/dae/logit-regression>, <https://github.com/tbates/umx>, <https://www.shengdongzhao.com/?p=1501>
#' @md
#' @examples
#' 
#' # ========================================
#' # = Report lm (regression/anova) results =
#' # ========================================
#' umxAPA(lm(mpg ~ wt + disp, mtcars)) # Report all parameters
#' umxAPA(lm(mpg ~ wt + disp, mtcars), "wt") # Just effect of weight
#' umxAPA(lm(mpg ~ wt + disp, mtcars), std = TRUE) # Standardize model!
#' 
#' ###############
#' # GLM example #
#' ###############
#'
#' df = mtcars
#' df$mpg_thresh = 0
#' df$mpg_thresh[df$mpg > 16] = 1
#' m1 = glm(mpg_thresh ~ wt + gear,data = df, family = binomial)
#' umxAPA(m1)
#' 
#' ###############
#' # A t-Test    #
#' ###############
#'
#' umxAPA(t.test(x = 1:10, y = c(7:20)))
#' umxAPA(t.test(extra ~ group, data = sleep))
#' 
#' # ======================================================
#' # = Summarize DATA FRAME: Correlations + Means and SDs =
#' # ======================================================
#' umxAPA(mtcars[,1:3])
#' umxAPA(mtcars[,1:3], digits = 3)
#' umxAPA(mtcars[,1:3], lower = FALSE)
#' \dontrun{
#' umxAPA(mtcars[,1:3], report = "html")
#' }
#' 
#' # ==========================================
#' # = CONFIDENCE INTERVAL from effect and se =
#' # ==========================================
#' umxAPA(.4, .3) # parameter 2 interpreted as SE
#' 
#' # Input beta and CI, and back out the SE
#' umxAPA(-0.030, c(-0.073, 0.013), digits = 3)
#' 
#' # ====================
#' # = Format a p-value =
#' # ====================
#' umxAPA(.0182613)   #   0.02
#' umxAPA(.00018261) # < 0.001
#' umxAPA(.00018261, addComparison = FALSE) # 0.001
#' 
#' # ========================
#' # = Report a correlation =
#' # ========================
#' data(twinData)
#' tmp = subset(twinData, zygosity %in% c("MZFF", "MZMM"))
#' m1 = cor.test(~ wt1 + wt2, data = tmp)
#' umxAPA(m1)
#'
umxAPA <- function(obj = .Last.value, se = NULL, p = NULL, std = FALSE, digits = 2, use = "complete", min = .001, addComparison = NA, report = c("markdown", "html"), lower = TRUE, test = c("Chisq", "LRT", "Rao", "F", "Cp"), SEs = TRUE, means = TRUE) {
	report = match.arg(report)
	test = match.arg(test)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	if(std){
		betaSymbol = " \u03B2 = "
	} else {
		betaSymbol = " B = "
	}
	if("htest" == class(obj)[[1]]){
		# t.test
		if(obj$method ==  "Pearson's product-moment correlation"){
			# cor.test
			o = paste0("r = ", round(obj$estimate, digits), " [", round(obj$conf.int[1], digits), commaSep, round(obj$conf.int[2], digits), "]")
			o = paste0(o, ", t(", obj$parameter, ") = ", round(obj$statistic, digits),  ", p = ", umxAPA(obj$p.value))
		} else {
			o = paste0("Means were ", omxQuotes(obj$estimate),
			". CI[", round(obj$conf.int[1], 2), ", ", round(obj$conf.int[2], 2), "]. ",
			"t(", round(obj$parameter, 2), ") = ", round(obj$statistic, 2), ", p = ", umxAPA(obj$p.value))
		}
		cat(o)
		invisible(o)
	}else if("data.frame" == class(obj)[[1]]){
		# Generate a summary of correlation and means
		# TODO umxAPA could upgrade strings to factors here (instead of stopping)...
		cor_table = umxHetCor(obj, ML = FALSE, use = use, treatAllAsFactor = FALSE, verbose = FALSE, std.err = SEs, return = "hetcor object")
		# cor_table = x; digits = 2
		# cor_table = umx_apply(FUN= round, of = cor_table, digits = digits) # round correlations
		correlations = round(cor_table$correlations, digits)
		if(SEs){
			std.errors = round(cor_table$std.errors, digits)
			correlations[] = paste0(as.character(correlations), " (", as.character(std.errors), ")")
		}
		cor_table = correlations

		if(lower){
			cor_table[upper.tri(cor_table)] = ""
		}

		if(means){
			mean_sd = umx_apply(umx_fun_mean_sd, of = obj)
			output  = data.frame(rbind(cor_table, mean_sd), stringsAsFactors = FALSE)
			rownames(output)[length(rownames(output))] = "Mean (SD)"
		} else {
			output  = data.frame(cor_table, stringsAsFactors = FALSE)
		}
		if(report == "html"){
			umx_print(output, digits = digits, file = "tmp.html")
		} else {
			umx_print(output, digits = digits)
		}
		if(anyNA(obj)){
			message("Some rows in dataframe had missing values.")
		}
	} else if("matrix" == class(obj)[[1]]) {
		# Assume these are correlations or similar numbers
		cor_table = umx_apply(round, obj, digits = digits) # round correlations
		output = data.frame(cor_table)
		if(report == "html"){
			umx_print(output, digits = digits, file = "tmp.html")
		} else {
			umx_print(output, digits = digits)
		}
	} else if("lm" == class(obj)[[1]]) {
		# Report lm summary table
		if(std){
			obj = update(obj, data = umx_scale(obj$model))
		}
		sumry = summary(obj)
		conf  = confint(obj)
		if(is.null(se)){
			se = dimnames(sumry$coefficients)[[1]]
		}
		for (i in se) {
			lower   = conf[i, 1]
			upper   = conf[i, 2]
			b_and_p = sumry$coefficients[i, ]
			b       = b_and_p["Estimate"]
			tval    = b_and_p["t value"]
			pval    = b_and_p["Pr(>|t|)"]
			cat(paste0(i, betaSymbol, round(b, digits), 
				" ["  , round(lower, digits), commaSep, round(upper, digits), "], ",
				"t = ", round(tval , digits), ", p ", umx_APA_pval(pval, addComparison = TRUE), "\n"
			))
		}
		cat(paste0("R\u00B2 = ", round(sumry$r.squared, 3), " (adj = ", round(sumry$adj.r.squared, 3), ")"))
		invisible(obj)
	} else if("glm" == class(obj)[[1]]) {
		# report glm summary table
		if(std){
			message("TODO: not sure how to not scale the DV in this glm model: Don't trust this")
			obj = update(obj, data = umx_scale(obj$model))
		}
		# TODO pick test based on family
		# Chisq = "binomial" "Poisson" (Chisq same as "LRT")
		# F = gaussian, quasibinomial, quasipoisson
		# Cp similar to AIC
		# see ?anova.glm 
		cat("Change in the log odds of the outcome for a one unit increase in the predictor variable:\n")
		model_coefficients = summary(obj)$coefficients
		conf = confint(obj)
		if(is.null(se)){
			se = dimnames(model_coefficients)[[1]]
		}
		for (i in se) {
			lower   = conf[i, 1]
			upper   = conf[i, 2]
			b_and_p = model_coefficients[i, ]
			b       = b_and_p["Estimate"]
			testStat    = b_and_p["z value"]
			pval    = b_and_p["Pr(>|z|)"]
			cat(paste0(i, " log(odds) = ", round(b, digits), 
			   " [", round(lower, digits), commaSep, round(upper, digits), "], ",
			   "z = ", round(testStat, digits), ", p ", umx_APA_pval(pval, addComparison = TRUE), "\n"
			))
		}
		if(obj$family$family == "binomial"){
			# https://stats.idre.ucla.edu/r/dae/logit-regression/
			cat("\nAs ORs (odds ratios, rather than log(odds)):\n")


			model_ORs = exp(coef(obj)) # Odds Ratios OR
			confOR    = exp(conf)
			for (i in 1:length(model_ORs)) {
				lower    = confOR[i, 1]
				upper    = confOR[i, 2]
				OR       = model_ORs[i]
				testStat = model_coefficients[i, "z value"]
				pval     = model_coefficients[i, "Pr(>|z|)"]
				cat(paste0(se[i], " OR = ", round(OR, digits), " [", round(lower, digits), commaSep, round(upper, digits), "], ",
 			    "z = ", round(testStat, digits), ", p ", umx_APA_pval(pval, addComparison = TRUE), "\n"))
			}

			cat("\nAnd as probabilities...\n")
			for (i in 1:length(model_ORs)) {
				OR = model_ORs[i]
				cat(paste0(se[i], " probability = ", round(OR/(1+OR), digits), "\n"))
			}
		}
		cat(paste0("\nAIC = ", round(AIC(obj), 3) ))
	} else if( "lme" == class(obj)[[1]]) {
		# report lm summary table
		if(std){
			obj = update(obj, data = umx_scale(obj$data))
		}
		model_coefficients = summary(obj)$tTable
		conf = intervals(obj, which = "fixed")[[1]]
		if(is.null(se)){
			se = dimnames(model_coefficients)[[1]]
		}
		for (i in se) {
			# umx_msg(i)
			lower   = conf[i, "lower"]
			upper   = conf[i, "upper"]
			b       = conf[i, "est."]
			tval    = model_coefficients[i, "t-value"]
			numDF   = model_coefficients[i, "DF"]
			pval    = model_coefficients[i, "p-value"]
			cat(paste0(i, betaSymbol, round(b, digits), 
			   " [", round(lower, digits), commaSep, round(upper, digits), "], ",
			   "t(", numDF, ") = ", round(tval, digits), ", p ", umx_APA_pval(pval, addComparison = TRUE),"\n"
			))
		}
	} else {
		if(is.null(se)){
			if(is.null(p)){
				# obj is likely a p value (p not provided separately which is what SE_from_p expects...)
				return(umx_APA_pval(obj, min = min, digits = digits, addComparison = addComparison))
			} else {
				# p-value provided but not SE
				se  = SE_from_p(beta = obj, p = p)
				str = paste0("\u03B2 = ", round(obj, digits), " [", round(obj - (1.96 * se), digits), commaSep, round(obj + (1.96 * se), digits), "]")
				cat(str)
				invisible(str)
			}
		} else if(length(se) == 2){
			# beta and CI
			# lower = b - (1.96 * se)
			# upper = b + (1.96 * se)
			str= paste0("\u03B2 = ", round(obj, digits), "SE = ", round((se[2] - se[1])/(1.96 * 2), digits))
			cat(str)
			invisible(str)
		} else {
			# obj = beta and SE
			str = paste0("\u03B2 = ", round(obj, digits), " [", round(obj - (1.96 * se), digits), commaSep, round(obj + (1.96 * se), digits), "]")
			cat(str)
			invisible(str)
		}

	}
}

#' @export
summaryAPA <- umxAPA

#' Summarize twin data
#'
#' @description
#' Produce a summary of wide-format twin data, showing the number of individuals, the mean and SD for each trait, and the correlation for each twin-type.
#'
#' Set MZ and DZ to summarize the two-group case.
#' 
#' @param data The twin data.
#' @param selVars Collection of variables to report on, e.g. c("wt", "ht").
#' @param sep  The separator string that will turn a variable name into a twin variable name, default= "_T" for wt_T1 and wt_T2.
#' @param age The age column in the dataset (default "age")
#' @param zyg  The zygosity column in the dataset (default "zygosity").
#' @param MZ Set level in zyg corresponding to MZ for two group case (defaults to using 5-group case).
#' @param DZ Set level in zyg corresponding to DZ for two group case (defaults to using 5-group case).
#' @param MZFF The level of zyg corresponding to MZ FF pairs: default= "MZFF".
#' @param DZFF The level of zyg corresponding to DZ FF pairs: default= "DZFF".
#' @param MZMM The level of zyg corresponding to MZ MM pairs: default= "MZMM".
#' @param DZMM The level of zyg corresponding to DZ MM pairs: default= "DZMM".
#' @param DZOS The level of zyg corresponding to DZ OS pairs: default= "DZOS".
#' @param digits Rounding precision of the report (default 2).
#' @param report What to return (default = 'markdown'). Use 'html' to open a web table.
#' @return - formatted table, e.g. in markdown.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxAPA()]
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' data(twinData)
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"))
#' MZs = c("MZMM", "MZFF"); DZs = c("DZFF","DZMM", "DZOS")
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"), MZ = MZs, DZ = DZs)
umxSummarizeTwinData <- function(data = NULL, selVars = NULL, sep = "_T", zyg = "zygosity", age = "age", MZ = NULL, DZ = NULL, MZFF= "MZFF", DZFF= "DZFF", MZMM= "MZMM", DZMM= "DZMM", DZOS= "DZOS", digits = 2, report = c("markdown", "html")) {
	report = match.arg(report)
	# TODO cope with two group case.
	# data = twinData; selVars = c("wt", "ht"); zyg = "zygosity"; sep = ""; digits = 2
	if(umx_check_names(age, data= data, die=FALSE)){
		ageCol = data[, age]
	} else if(umx_check_names(paste0(age, sep, 1), data= data, die=FALSE)){
		ageCol = data[, paste0(age, sep, 1)]
	}else{
		stop("Sorry: I can't find an age column called ", omxQuotes(age), " or ", omxQuotes(paste0(age, sep, 1)), " Set age= <name of your age column>")
	}
	cat(paste0("mean age ", round(mean(ageCol, na.rm = TRUE), 2), " (SD= ", round(sd(ageCol, na.rm = TRUE), 2), ")"))
	
	
	selDVs = tvars(selVars, sep)
	umx_check_names(selDVs, data = data, die = TRUE)
	long = umx_wide2long(data= data[,selDVs], sep =sep)
	blob = rep(NA, length(selVars))	
	if(is.null(MZ)){
		df = data.frame(Var = blob, Mean = blob, SD = blob, rMZFF = blob, rMZMM = blob, rDZFF = blob, rDZMM = blob, rDZOS = blob, stringsAsFactors = FALSE)
		n = 1
		for (varName in selVars){
			# varName = "ht"
			df[n, "Var"]  = varName
			df[n, "Mean"] = round(mean(long[,varName], na.rm = TRUE), digits)
			df[n, "SD"]   = round(sd(long[,varName], na.rm = TRUE), digits)
			rMZFF = cor.test(data = data[data[,zyg] %in% MZFF,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			rMZMM = cor.test(data = data[data[,zyg] %in% MZMM,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			rDZFF = cor.test(data = data[data[,zyg] %in% DZFF,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			rDZMM = cor.test(data = data[data[,zyg] %in% DZMM,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			rDZOS = cor.test(data = data[data[,zyg] %in% DZOS,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))

			df[n, "rMZFF"] = paste0(round(rMZFF$estimate, digits), " (", round((rMZFF$conf.int[2] - rMZFF$conf.int[1])/(1.96 * 2), digits), ")")
			df[n, "rMZMM"] = paste0(round(rMZMM$estimate, digits), " (", round((rMZMM$conf.int[2] - rMZMM$conf.int[1])/(1.96 * 2), digits), ")")
			df[n, "rDZFF"] = paste0(round(rDZFF$estimate, digits), " (", round((rDZFF$conf.int[2] - rDZFF$conf.int[1])/(1.96 * 2), digits), ")")
			df[n, "rDZMM"] = paste0(round(rDZMM$estimate, digits), " (", round((rDZMM$conf.int[2] - rDZMM$conf.int[1])/(1.96 * 2), digits), ")")
			df[n, "rDZOS"] = paste0(round(rDZOS$estimate, digits), " (", round((rDZOS$conf.int[2] - rDZOS$conf.int[1])/(1.96 * 2), digits), ")")
			n = n+1
		}
		nPerZyg = table(data[, zyg])
		names(df) = namez(df, "(rMZFF)", paste0("\\1 (", nPerZyg["MZFF"],")"))
		names(df) = namez(df, "(rDZFF)", paste0("\\1 (", nPerZyg["DZFF"],")"))
		names(df) = namez(df, "(rMZMM)", paste0("\\1 (", nPerZyg["MZMM"],")"))
		names(df) = namez(df, "(rDZMM)", paste0("\\1 (", nPerZyg["DZMM"],")"))
		names(df) = namez(df, "(rDZOS)", paste0("\\1 (", nPerZyg["DZOS"],")"))
	}else{
		df = data.frame(Var = blob, Mean = blob, SD = blob, rMZ = blob, rDZ = blob, stringsAsFactors = FALSE)		
		n = 1
		for (varName in selVars){
			# varName = "ht"
			df[n, "Var"]  = varName
			df[n, "Mean"] = round(mean(long[,varName], na.rm = TRUE), digits)
			df[n, "SD"]   = round(sd(long[,varName], na.rm = TRUE), digits)
			rMZ = cor.test(data = data[data[,zyg] %in% MZ,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			rDZ = cor.test(data = data[data[,zyg] %in% DZ,], as.formula(paste0("~ ", varName, sep, 1, "+", varName, sep, 2)))
			df[n, "rMZ"] = paste0(round(rMZ$estimate, digits), " (", round((rMZ$conf.int[2] - rMZ$conf.int[1])/(1.96 * 2), digits), ")")
			df[n, "rDZ"] = paste0(round(rDZ$estimate, digits), " (", round((rDZ$conf.int[2] - rDZ$conf.int[1])/(1.96 * 2), digits), ")")
			n = n+1
		}
		nPerZyg = data.frame(table(data[, zyg]))
		names(df) = namez(df, "(rMZ)", paste0("\\1 (", sum(nPerZyg[nPerZyg$Var1 %in% MZ,"Freq"]),")"))
		names(df) = namez(df, "(rDZ)", paste0("\\1 (", sum(nPerZyg[nPerZyg$Var1 %in% DZ,"Freq"]),")"))
	}
	if(report == "html"){
		umx_print(df, digits=digits, file = "tmp.html")
	} else {
		umx_print(df, digits=digits)
	}
	
	# return(df)
	# Calculate Mean Age and SD for men and women
	# umx_aggregate(value ~ Sex, data = longformat, what = "mean_sd")
	
	# Calculate correlations, means and sd
	# umxAPA(mzData[, allItemNames], use ="pairwise.complete.obs")
	# umxAPA(dzData[, allItemNames], use ="pairwise.complete.obs")
}


#' Test the difference between correlations for significance.
#'
#' @description
#' `umx_r_test` is a wrapper around the cocor test of difference between correlations.
#'
#' @details
#' **Non-overlapping (no variable in common) correlations in the same dataset.**
#' If 4 variables are provided in `vars`, `umx_r_test` conducts a test of
#' the correlation of var 1 & 2 differs in magnitude from the correlation of var 3 with var 4.
#' (`r.jk` and `r.hm` in cocor speak).
#' 
#' **Overlapping (1 variable in common) correlations in the same dataset.**
#' If 3 variables are provided in `vars`, `umx_r_test` conducts a test of whether
#' the correlation of var 1 & 2 differs in magnitude from the correlation of var 1 with var 3.
#' (`r.jk` and `r.jh` in cocor speak).
#' 
#' In the future it will be expanded to handle other correlations, and to take correlations as input.
#'
#' @param data The dataset.
#' @param vars Three or 4 variables forming the two pairs of columns.
#' @param alternative A two (default) or one-sided (greater less) test.
#' @return cocor result.
#' @export
#' @family Miscellaneous Stats Functions
#' @md
#' @examples
#' # Is the correlation of mpg with cylinder count different from that 
#' # obtaining between disp and hp?
#' vars = c("mpg", "cyl", "disp", "hp")
#' umx_r_test(mtcars, vars)
#' umx_r_test(mtcars, c("mpg", "disp", "hp"))
umx_r_test <- function(data = NULL, vars = vars, alternative = c("two.sided", "greater", "less")) {
	alternative = match.arg(alternative)
	alpha        = 0.05
	conf.level   = 0.95
	null.value   = 0
	data.name    = NULL
	var.labels   = NULL
	return.htest = FALSE
	n    = nrow(data)	
	jkhm = data[, vars]
	cors = cor(jkhm)
	# jkhm = 1234
	if(length(vars)==3){
		r.jk = as.numeric(cors[vars[1], vars[2]])
		r.jh = as.numeric(cors[vars[1], vars[3]])
		r.kh = as.numeric(cors[vars[2], vars[3]])
		# tests = (pearson1898, hotelling1940, hendrickson1970, williams1959, olkin1967, dunn1969, steiger1980, meng1992, hittner2003, or zou2007).
		cocor::cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, alternative = alternative, test = "hittner2003", alpha = alpha, 
			conf.level = conf.level, null.value = null.value, data.name = data.name, var.labels = var.labels, return.htest = return.htest)
	} else {
		r.jk = as.numeric(cors[vars[1], vars[2]])
		r.hm = as.numeric(cors[vars[3], vars[4]])
		r.jh = as.numeric(cors[vars[1], vars[3]])
		r.jm = as.numeric(cors[vars[1], vars[4]])
		r.kh = as.numeric(cors[vars[2], vars[3]])
		r.km = as.numeric(cors[vars[2], vars[4]])
		# test         = "silver2004"
	 	# tests = (pearson1898, dunn1969, steiger1980, raghunathan1996, silver2004, or zou2007).
 		cocor::cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative = alternative, test = "silver2004", alpha = alpha, 
			conf.level = conf.level, null.value = null.value, data.name = data.name, var.labels = var.labels, return.htest = return.htest)
	}

}
