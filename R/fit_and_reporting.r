# library(devtools)
# document("~/bin/umx"); install("~/bin/umx");
# setwd("~/bin/umx"); 
# build("~/bin/umx")
# check("~/bin/umx")
# release("~/bin/umx")
# load_all("~/bin/umx")
# show_news("~/bin/umx")
# install_github("tbates/umx"); library(umx);
# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools

# =====================
# = MOdel Diagnostics =
# =====================

#' mxDiagnostic
#'
#' Diagnose problems in a model
#'
#' @param model an \code{\link{mxModel}} to diagnose
#' @param tryHard whether I should try and fix it? (defaults to FALSE)
#' @param diagonalizeExpCov Whether to diagonalize the ExpCov
#' @return - helpful messages and perhaps a modified model
#' @export
#' @family umx build functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' myData = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' m1 <- umxRAM("OneFactor", data = myData,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' m1 = mxRun(m1)
#' umxSummary(m1, show = "std")
#' umxDiagnose(m1)
umxDiagnose <- function(model, tryHard = FALSE, diagonalizeExpCov = FALSE){
	# 1. First thing to check is whether the covariance matrix is positive definite.
	minEigen = min(eigen(umxExpCov(model))$values)
	if(minEigen<0){
		message("The expected covariance matrix is not positive definite")
		# now what?
	}
  # Best diagnostics are
  # 1. observed data variances and means
  # 2. expected variances and means
  # 3 Difference of these?
  # try
  # diagonalizeExpCov diagonal.
  # umx_any_ordinal()
  # more tricky - we should really report the variances and the standardized thresholds.
  # The guidance would be to try starting with unit variances and thresholds that are within +/- 2SD of the mean.
  # [bivariate outliers %p](http://openmx.psyc.virginia.edu/thread/3899)
}

# =============================
# = Fit and Reporting Helpers =
# =============================

#' umx_drop_ok
#'
#' Print a meaningful sentence about a model comparison. SHould be merged with umxCompare
#'
#' @param model1 the base code{\link{mxModel}}
#' @param model2 the nested code{\link{mxModel}}
#' @param text name of the thing being tested, i.e., "Extraversion" or "variances"
#' @return - 
#' @export
#' @family umx reporting functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' \dontrun{
#' model = umx_drop_ok(model)
#' }
umx_drop_ok <- function(model1, model2, text = "parameter") {
	a = mxCompare(model1, model2)
	if(a$diffdf[2] > 1){
		are = "are"
	}else{
		are = "is"
	}
	if(a$p[2] < .05){
		if(!is.null(text)){ print(paste0("The ", text, " ", are, " significant and should be kept (p = ", umx_APA_pval(a$p[2]), ")")) }
		return(FALSE)
	} else {
		if(!is.null(text)){ print(paste0("The ", text, " ", are, " non-significant and can be dropped (p = ", umx_APA_pval(a$p[2]), ")")) }
		return(TRUE)
	}
}

#' residuals.MxModel
#'
#' Return the \code{\link{residuals}} from an OpenMx RAM model
#'
#' @rdname residuals.MxModel
#' @param model a (run) \code{\link{mxModel}} to get residuals from
#' @param digits rounding (default = 2)
#' @param suppress smallest deviation to print out (default = NULL = show all)
#' @param ... Optional parameters
#' @return - residual correlation matrix
#' @export
#' @export residuals.MxModel
#' @family umx reporting functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("g")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' residuals(m1)
#' residuals(m1, digits = 3)
#' residuals(m1, digits = 3, suppress = .005)
#' # residuals are returned as an invisible object you can capture in a variable
#' a = residuals(m1); a
residuals.MxModel <- function(model, digits = 2, suppress = NULL, ...){
	umx_check_model(model, type = NULL, hasData = TRUE)
	expCov = umxExpCov(model, latents = FALSE)
	if(model@data@type == "raw"){
		obsCov = umxHetCor(model@data@observed)
	} else {
		obsCov = model@data@observed
	}
	resid = cov2cor(obsCov) - cov2cor(expCov)
	umx_print(data.frame(resid), digits = digits, zero.print = ".", suppress = suppress)
	if(is.null(suppress)){
		print("nb: You can zoom in on bad values with, e.g. suppress = .01, which will hide values smaller than this. Use digits = to round")
	}
	invisible(resid)
}

#' umxStandardizeModel
#'
#' umxStandardizeModel takes a RAM-style model, and returns standardized version.
#'
#' @param model The \code{\link{mxModel}} you wish to standardise
#' @param return What to return. Valid options: "parameters", "matrices", or "model"
#' @param Amatrix Optionally tell the function what the name of the asymmetric matrix is (defaults to RAM standard A)
#' @param Smatrix Optionally tell the function what the name of the symmetric matrix is (defaults to RAM standard S)
#' @param Mmatrix Optionally tell the function what the name of the means matrix is (defaults to RAM standard M)
#' @return - a \code{\link{mxModel}} or else parameters or matrices if you request those
#' @family umx reporting functions
#' @references - \url{http://github.com/tbates/umx}
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m1 = umxStandardizeModel(m1, return = "model")
#' summary(m1)
umxStandardizeModel <- function(model, return = "parameters", Amatrix = NA, Smatrix = NA, Mmatrix = NA) {
	if (!(return == "parameters"|return == "matrices"|return == "model")) stop("Invalid 'return' parameter. Do you want do get back parameters, matrices or model?")
	suppliedNames = all(!is.na(c(Amatrix,Smatrix)))
	# if the objective function isn't RAMObjective, you need to supply Amatrix and Smatrix

	if (!umx_is_RAM(model) & !suppliedNames ){
		stop("I need either type = RAM model or the names of the equivalent of the A and S matrices.")
	}
	output <- model@output
	# Stop if there is no objective function
	if (is.null(output))stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
	# Stop if there is no output
	if (length(output) < 1){
		message("Model has not been run yet")
		return(model)
	}
	# Get the names of the A, S and M matrices 
	if("expectation" %in% slotNames(model)){
		# openMx 2
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model@expectation@A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model@expectation@S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model@expectation@M}
	} else {
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model@objective@A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model@objective@S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model@objective@M}
	}
	# Get the A and S matrices, and make an identity matrix
	A <- model[[nameA]]
	S <- model[[nameS]]
	I <- diag(nrow(S@values))
	
	# this can fail (non-invertable etc. so we wrap it in try-catch)
	tryCatch({	
		# Calculate the expected covariance matrix
		IA <- solve(I - A@values)
		expCov <- IA %*% S@values %*% t(IA)
		# Return 1/SD to a diagonal matrix
		invSDs <- 1/sqrt(diag(expCov))
		# Give the inverse SDs names, because mxSummary treats column names as characters
		names(invSDs) <- as.character(1:length(invSDs))
		if (!is.null(dimnames(A@values))){names(invSDs) <- as.vector(dimnames(S@values)[[2]])}
		# Put the inverse SDs into a diagonal matrix (might as well recycle my I matrix from above)
		diag(I) <- invSDs
		# Standardize the A, S and M matrices
		#  A paths are value*sd(from)/sd(to) = I %*% A %*% solve(I)
		#  S paths are value/(sd(from*sd(to))) = I %*% S %*% I
		stdA <- I %*% A@values %*% solve(I)
		stdS <- I %*% S@values %*% I
		# Populate the model
		model[[nameA]]@values[,] <- stdA
		model[[nameS]]@values[,] <- stdS
		if (!is.na(nameM)){model[[nameM]]@values[,] <- rep(0, length(invSDs))}
	}, warning = function(cond) {
	    # warning-handler-code
        message(cond)
	}, error = function(cond) {
	    cat("The model could not be standardized")
        message(cond)
	}, finally = {
	    # cleanup-code
	})

	# Return the model, if asked
	if(return=="model"){
		return(model)
	}else if(return=="matrices"){
		# return the matrices, if asked
		matrices <- list(model[[nameA]], model[[nameS]])
		names(matrices) <- c("A", "S")
		return(matrices)
	}else if(return == "parameters"){
		# return the parameters
		#recalculate summary based on standardised matrices
		p <- summary(model)$parameters
		p <- p[(p[,2] == nameA)|(p[,2] == nameS),]
		## get the rescaling factor
		# this is for the A matrix
		rescale <- invSDs[p$row] * 1/invSDs[p$col]
		# this is for the S matrix
		rescaleS <- invSDs[p$row] * invSDs[p$col]
		# put the A and the S together
		rescale[p$matrix == "S"] <- rescaleS[p$matrix == "S"]
		# rescale
		p[,5] <- p[,5] * rescale
		p[,6] <- p[,6] * rescale
		# rename the columns
		# names(p)[5:6] <- c("Std. Estimate", "Std.Std.Error")
		return(p)		
	}
}

#' confint.MxModel
#'
#' Implements confidence interval function for OpenMx models.
#' Note: Currently requested CIs are added to existing CIs, and all are run, 
#' even if they alrady exist in the output. This might change in the future.
#'
#' @details Unlike \code{\link{confint}}, if parm is missing, all CIs requested will be added to the model, 
#' but (because these can take time to run) by default only CIs already computed will be reported.
#' 
#' CIs will be run only if run is TRUE, allowing this function to be used to add
#' CIs without automatically having to run them.
#' If parm is empty, and run = FALSE, a message will alert you to add run = TRUE. 
#' Even a few CIs can take too long to make running the default.
#'
#' @aliases umxConfint
#' @rdname confint.MxModel
#' @param object An \code{\link{mxModel}}, possibly already containing \code{\link{mxCI}}s that have been \code{\link{mxRun}} with intervals = TRUE))
#' @param parm	A specification of which parameters are to be given confidence intervals. Can be "existing", "all", or a vector of names.
#' @param level	The confidence level required (default = .95)
#' @param run Whether to run the model (defaults to FALSE)
#' @param showErrorcodes (default = FALSE)
#' @param ... Additional argument(s) for methods.
#' @export
#' @export confint.MxModel
#' @return - \code{\link{mxModel}}
#' @family umx reporting
#' @seealso - \code{\link[stats]{confint}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = confint(m1) # default: CIs added, but user prompted to set run = TRUE
#' m2 = confint(m2, run = TRUE) # CIs run and reported
#' # Add CIs for asymmetric paths in RAM model, report them, save m1 with this CI added
#' m1 = confint(m1, parm = "G_to_x1", run = TRUE) 
#' # Add CIs for asymmetric paths in RAM model, report them, save m1 with mxCIs added
#' m1 = confint(m1, parm = "A", run = TRUE)
#' confint(m1, parm = "existing") # request existing CIs (none added yet...)
confint.MxModel <- function(object, parm = list("existing", c("vector", "of", "names"), "default = add all"), level = 0.95, run = FALSE, showErrorcodes = FALSE, ...) {
	defaultParmString = list("existing", c("vector", "of", "names"), "default = add all")
	# 1. Add CIs if needed
	if (isTRUE(all.equal(parm, defaultParmString))) {
		if(umx_has_CIs(object, "intervals")) {
			# TODO add a count for the user
			message(length(object@intervals), " CIs found")
		} else {
			message("Adding CIs for all free parameters")
			CIs_to_set = names(omxGetParameters(object, free = TRUE))
			object = mxModel(object, mxCI(CIs_to_set, interval = level))
		}
	} else if(parm == "existing") {
		# check there are some in existence
		if(!umx_has_CIs(object, "intervals")) {
			message("This model has no CIs yet. Perhaps you wanted just confint(model, run = TRUE) to add and run CIs on all free parameters? Or set parm to a list of labels you'd like CIs? Also see help(mxCI)")
		}
	} else {
		# add requested CIs to model
		# TODO check that these exist
		object = mxModel(object, mxCI(parm, interval = level))
	}
	# 2. Run CIs if requested
	if(run) {
		object = mxRun(object, intervals = TRUE)
	}
	# 3. Report CIs if found in output
	if(!umx_has_CIs(object, "both")) {
		if(run == FALSE){
			message("Some CIs have been requested, but have not yet been run. Add ", omxQuotes("run = TRUE"), " to your confint() call to run them.\n",
			"To store the model run capture it from confint like this:\n",
			"m1 = confint(m1, run = TRUE)")
		} else {
			message("hmmm... you wanted it run, but I don't see any computed CIs despite there being ", length(m2$intervals), " requested...",
			"\nThat's a bug. Please report it to timothy.c.bates@gmail.com")
		}
	} else {
		model_summary = summary(object)
		model_CIs   = round(model_summary$CI[,c("lbound", "estimate", "ubound")], 3)
		model_CI_OK = object@output$confidenceIntervalCodes
		colnames(model_CI_OK) <- c("lbound Code", "ubound Code")
		model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
		print(model_CIs)
		npsolMessages <- list(
		'1' = 'The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN).',
		'2' = 'The linear constraints and bounds could not be satisfied. The problem has no feasible solution.',
		'3' = 'The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.',
		'4' = 'The major iteration limit was reached (Mx status BLUE).',
		'5' = 'not used',
		'6' = 'The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)',
		'7' = 'The function derivates returned by funcon or funobj appear to be incorrect.',
		'8' = 'not used',
		'9' = 'An input parameter was invalid')
		if(any(model_CI_OK !=0) & showErrorcodes){
			codeList = c(model_CI_OK[,"lbound Code"], model_CI_OK[,"ubound Code"])
			relevantCodes = unique(codeList); relevantCodes = relevantCodes[relevantCodes !=0]
			for(i in relevantCodes) {
			   print(paste0(i, ": ", npsolMessages[i][[1]]))
			}
		}
	}
	invisible(object)
}

#' umxSummary
#'
#' Report the fit of a model in a compact form suitable for a journal. Emits a "warning" 
#' when model fit is worse than accepted criterion (TLI >= .95 and RMSEA <= .06; (Hu & Bentler, 1999; Yu, 2002).
#' 
#' notes on CIs and Identification
#' Note, the conventional standard errors reported by OpenMx are used to produce the CIs you see in umxSummary
#' These are used to derive confidence intervals based on the formula 95%CI = estimate +/- 1.96*SE)
#' 
#' Sometimes they appear NA. This often indicates a model which is not identified (see\url{http://davidakenny.net/cm/identify.htm}).
#' This can include empirical under-identification - for instance two factors
#' that are essentially identical in structure.
#' 
#' A signature of this would be paths estimated at or close to
#' zero. Fixing one or two of these to zero may fix the standard error calculation, 
#' and alleviate the need to estimate likelihood-based or bootstrap CIs
#' 
#' If factor loadings can flip sign and provide identical fit, this creates another form of 
#' under-identification and can break confidence interval estimation, but I think
#' Fixing a factor loading to 1 and estimating factor variances can help here
#'
#' @param model The \code{\link{mxModel}} whose fit will be reported
#' @param saturatedModels Saturated models if needed for fit indices (see example below: Only needed for raw data, and then not if you've run umxRun)
#' @param report The format for the output line or table (default is "line")
#' @param showEstimates What estimates to show. Options are c("none", "raw", "std", "both", "list of column names"). 
#' Default  is "none" (just shows the fit indices)
#' @param digits How many decimal places to report to (default = 2)
#' @param RMSEA_CI Whether to compute the CI on RMSEA (Defaults to F)
#' @param matrixAddresses Whether to show "matrix address" columns (Default = FALSE)
#' @param filter whether to show significant paths (SIG) or NS paths (NS) or all paths (ALL)
#' @family umx reporting
#' @seealso - \code{\link{mxCI}}, \code{\link{umxCI_boot}}, \code{\link{umxRun}}
#' @references - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance 
#'  structure analysis: Coventional criteria versus new alternatives. Structural Equation Modeling, 6, 1-55. 
#'
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models
#'  with binary and continuous outcomes. University of California, Los Angeles, Los Angeles.
#'  Retrieved from \url{http://www.statmodel.com/download/Yudissertation.pdf}
#' \url{http://www.github.com/tbates/umx}
#' @export
#' @import OpenMx
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxSummary(m1, show = "std")
#' umxSummary(m1, show = "std", digits = 1)
#' \dontrun{
#' # umxSummary(m1, report = "table") # not yet implemented
#' # umxSummary(m1, saturatedModels = umxSaturated(m1))
#' }
umxSummary <- function(model, saturatedModels = NULL, report = "line", showEstimates = c("none", "raw", "std", "both", "list of column names"), digits = 2, RMSEA_CI = FALSE, matrixAddresses = FALSE, filter = c("ALL", "NS", "SIG")){
	validValuesForshowEstimates = c("none", "raw", "std", "both", "list of column names")
	showEstimates = umx_default_option(showEstimates, validValuesForshowEstimates, check = FALSE) # to allow a user specified list
	# showEstimates = match.arg(showEstimates)
	filter = match.arg(filter)
	# if the filter is off default, the user must want something...
	if( filter != "ALL" & showEstimates == "none") {
		showEstimates = "std"
	}
	# TODO make table take lists of models...
	report = match.arg(report)
	umx_has_been_run(model, stop = TRUE)
	# saturatedModels = NULL
	if(is.null(saturatedModels)) {
		# saturatedModels not passed in from outside, so get them from the model
		modelSummary = OpenMx::summary(model)		
		if(is.null(model@data)){
			# # TODO model with no data - no saturated solution?
		} else if(is.na(modelSummary$SaturatedLikelihood)){
			message("There is no saturated likelihood: computing that now...")
			saturatedModels = umxSaturated(model)
			modelSummary = OpenMx::summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
		}
	} else {
		modelSummary = OpenMx::summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
	}

	# DisplayColumns
	if(showEstimates != "none"){
		parameterTable = mxStandardizeRAMpaths(model, SE = TRUE) # compute standard errors
		#                 name    label  matrix   row         col    Raw.Value  Raw.SE   Std.Value    Std.SE
		# 1  no_HRV_Dep.A[6,1]    age    A        mean_sdrr   age   -0.37       0.0284   -0.372350    .028
		# Raw.SE is new
		names(parameterTable) <- c("label", "name", "matrix", "row", "col", "Estimate", "SE", "Std.Estimate", "Std.SE")

		if(matrixAddresses){
			nameing = c("name", "matrix", "row", "col")
		} else {
			nameing = c("name")
		}
		if("Std.Estimate" %in%  names(parameterTable)){
			if(length(showEstimates) > 1) {
				namesToShow = showEstimates # user-specified list
			}else if(showEstimates == "both") {
				namesToShow = c(nameing, "Estimate", "SE", "Std.Estimate", "Std.SE")
			} else if(showEstimates == "std"){
				namesToShow = c(nameing, "Std.Estimate", "Std.SE", "CI")
			}else{ # must be raw
				namesToShow = c(nameing, "Estimate", "SE")					
			}
		} else {
			namesToShow = c(nameing, "Estimate", "SE")
		}
		if("CI" %in% namesToShow){
			parameterTable$sig = TRUE
			parameterTable$CI  = ""
			for(i in 1:dim(parameterTable)[1]) {
				# i = 1
				# x = summary(m1)$parameters
				# digits = 2
				# # TODO we only show se-based CI for std estimates so far
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
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), ", ", round(est + CI95, digits), "]")
					} else {
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), ", ", round(est + CI95, digits), "]")
					}
				}
			}
		}
		if(filter == "NS"){
			print(parameterTable[parameterTable$sig == FALSE, namesToShow], digits = digits, na.print = "", zero.print = "0", justify = "none")			
		}else if(filter == "SIG"){
			print(parameterTable[parameterTable$sig == TRUE, namesToShow], digits = digits, na.print = "", zero.print = "0", justify = "none")
		}else{
			print(parameterTable[,namesToShow], digits = digits, na.print = "", zero.print = "0", justify = "none")			
		}
	} else {
		message("For estimates, add showEstimates = 'raw' 'std' or 'both")
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
				x = data.frame(cbind(model@name, round(Chi,2), formatC(p, format="g"), round(CFI,3), round(TLI,3), round(RMSEA, 3)))
				names(x) = c("model","\u03C7","p","CFI", "TLI","RMSEA") # \u03A7 is unicode for chi
				print(x)
			} else {
				if(RMSEA_CI){
					RMSEA_CI = RMSEA(model)$txt
				} else {
					RMSEA_CI = paste0("RMSEA = ", round(RMSEA, 3))
				}
				x = paste0(
					"\u03C7\u00B2(", degreesOfFreedom, ") = ", round(Chi, 2), # was A7
					", p "      , umx_APA_pval(p, .001, 3),
					"; CFI = "  , round(CFI, 3),
					"; TLI = "  , round(TLI, 3),
					"; ", RMSEA_CI
					)
				print(x)
				if(TLI_OK != "OK"){
					message("TLI is worse than desired")
				}
				if(RMSEA_OK != "OK"){
					message("RMSEA is worse than desired")
				}
			}
	})
	
	if(!is.null(model@output$confidenceIntervals)){
		print(model@output$confidenceIntervals)
	}
}

#' umxSummaryACE
#'
#' Summarise a Cholesky model, as returned by umxACE
#'
#' @param fit an \code{\link{mxModel}} to summarize
#' @param digits rounding (default = 2)
#' @param dotFilename The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = F)
#' @param extended how much to report (F)
#' @param showRg = whether to show the genetic correlations (F)
#' @param showStd = whether to show the standardized model (T)
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param zero.print How to show zeros (".")
#' @param report If 3, then open an html table of the results
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family umx.twin model report
#' @seealso - \code{\link{umxACE}} 
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' selDVs = c("bmi1", "bmi2")
#' mzData <- subset(twinData, ZYG == "MZFF", selDVs)
#' dzData <- subset(twinData, ZYG == "DZFF", selDVs)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' umxSummaryACE(m1)
#' \dontrun{
#' umxSummaryACE(m1, dotFilename = NA);
#' umxSummaryACE(m1, dotFilename = "name", showStd = T)
#' stdFit = umxSummaryACE(m1, returnStd = T);
#' }
umxSummaryACE <- function(fit, digits = 2, dotFilename = NULL, returnStd = F, extended = F, showRg = F, showStd = T, comparison = NULL, CIs = T, zero.print = ".", report = 1) {
	if(typeof(fit) == "list"){ # call self recursively
		for(thisFit in fit) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(fit, stop = T)
	if(is.null(comparison)){
		message("-2 \u00d7 log(Likelihood)") # \u00d7 = times sign
		print(-2 * logLik(fit));			
	} else {
		message("Comparison of fit with parent model:")
		print(umx::umxCompare(comparison, fit, digits = 3))
	}
	selDVs = dimnames(fit$top.expCovMZ)[[1]]
	# genEpi_TableFitStatistics(fit, extended=extended)
	nVar <- length(selDVs)/2;
	# Calculate standardised variance components
	a  <- mxEval(top.a, fit); # Path coefficients
	c  <- mxEval(top.c, fit);
	e  <- mxEval(top.e, fit);

	A  <- mxEval(top.A, fit); # Variances
	C  <- mxEval(top.C, fit);
	E  <- mxEval(top.E, fit);
	Vtot = A + C + E;         # Total variance
	I  <- diag(nVar);         # nVar Identity matrix
	SD <- solve(sqrt(I*Vtot)) # Inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"

	# Standardized _path_ coefficients ready to be stacked together
	a_std <- SD %*% a; # Standardized path coefficients
	c_std <- SD %*% c;
	e_std <- SD %*% e;

	if(showStd){
		message("Standardized solution")
		aClean = a_std
		cClean = c_std
		eClean = e_std
	} else {
		message("Raw solution")
		aClean = a
		cClean = c
		eClean = e
	}

	aClean[upper.tri(aClean)] = NA
	cClean[upper.tri(cClean)] = NA
	eClean[upper.tri(eClean)] = NA
	Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs[1:nVar]);

	names(Estimates) = paste0(rep(c("a", "c", "e"), each = nVar), rep(1:nVar));

	Estimates = umx::umx_print(Estimates, digits = digits, zero.print = ".")
	if(report == 3){
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = F, sortableDF = T); system(paste0("open ", "tmp.html"))
	}else{
		
	}

	if(extended == TRUE) {
		message("Unstandardized path coefficients")
		aClean = a
		cClean = c
		eClean = e
		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs[1:nVar]);
		names(unStandardizedEstimates) = paste(rep(c("a", "c", "e"), each = nVar), rep(1:nVar), sep = "");
		umx_print(unStandardizedEstimates, digits = digits, zero.print = ".")
	}

	# Pre & post multiply covariance matrix by inverse of standard deviations
	if(showRg) {
		message("Genetic correlations")
		NAmatrix <- matrix(NA, nVar, nVar);
		rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # shared environmental correlations
		rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # Unique environmental correlations
		rAClean = rA
		rCClean = rC
		rEClean = rE
		rAClean[upper.tri(rAClean)] = NA
		rCClean[upper.tri(rCClean)] = NA
		rEClean[upper.tri(rEClean)] = NA
		genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = selDVs[1:nVar] );
		names(genetic_correlations) <- selDVs[1:nVar]
	 	# Make a nice-ish table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each=nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits=digits, zero.print = ".")
	}
	stdFit = fit
	hasCIs = umx_has_CIs(fit)
	if(hasCIs & CIs) {
		# TODO Need to refactor this into some function calls...
		# TODO and then add to umxSummaryIP and CP
		message("Creating CI-based report!")
		# CIs exist, get the lower and uppper CIs as a dataframe
		CIlist = data.frame(fit@output$confidenceIntervals)
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
		# These can be names ("top.a_std[1,1]") or labels ("a11")
		# imxEvalByName finds them both
		outList = c();
		for(aName in row.names(CIlist)) {
			outList <- append(outList, imxEvalByName(aName, fit))
		}
		# Add estimates into the CIlist
		CIlist$estimate = outList
		# reorder to match summary
		CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
		CIlist$fullName = row.names(CIlist)
		# Initialise empty matrices for the standardized results
		rows = dim(fit@submodels$top@matrices$a@labels)[1]
		cols = dim(fit@submodels$top@matrices$a@labels)[2]
		a_std = c_std = e_std = matrix(NA, rows, cols)

		# iterate over each CI
		labelList = imxGenerateLabels(fit)			
		rowCount = dim(CIlist)[1]

		for(n in 1:rowCount) { # n = 1
			thisName = row.names(CIlist)[n] # thisName = "a11"
			# convert labels to [bracket] style
				if(!umx::umx_has_square_brackets(thisName)) {
				nameParts = labelList[which(row.names(labelList) == thisName),]
				CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
			}
			fullName = CIlist$fullName[n]

			thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
			thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
			thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
			CIparts = round(CIlist[n, c("estimate", "lbound", "ubound")], 2)
			thisString = paste(CIparts[1], " (",CIparts[2], ":",CIparts[3], ")", sep="")
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
	stdFit@submodels$top@matrices$a@values = a_std
	stdFit@submodels$top@matrices$c@values = c_std
	stdFit@submodels$top@matrices$e@values = e_std
	if(!is.null(dotFilename)) {
		message("making dot file")
		umxPlotACE(fit, dotFilename, std = showStd)
	}
	if(returnStd) {
		return(stdFit)
	}
}

#' umxCompare
#'
#' umxCompare compares two or more \code{\link{mxModel}}s. If you leave comparison blank, it will just give fit info for the base model
#'
#' @param base The base \code{\link{mxModel}} for comparison
#' @param comparison The model (or list of models) which will be compared for fit with the base model (can be empty)
#' @param all Whether to make all possible comparisons if there is more than one base model (defaults to T)
#' @param digits rounding for p etc.
#' @param report Optionally add sentences for inclusion inline in a paper (report= 2)
#' and output to an html table which will open your default browser (report = 3).
#' (This is handy for getting tables into Word, markdown, and other text systems!)
#' @family umx reporting
#' @seealso - \code{\link{mxCompare}}, \code{\link{umxSummary}}, \code{\link{umxRun}},
#' @references - \url{http://www.github.com/tbates/umx/}
#' @family umx reporting
#' @export
#' @import OpenMx
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = umxReRun(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' mxCompare(m1, m2) # what OpenMx gives by default
#' umxCompare(m1, m2, report = 2) # Add English-sentence descriptions
#' umxCompare(m1, m2, report = 3) # Open table in browser
#' m3 = umxReRun(m2, update = "G_to_x3", name = "drop_path_2_x2_and_3")
#' umxCompare(m1, c(m2, m3))
#' umxCompare(c(m1, m2), c(m2, m3), all = TRUE)
umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, digits = 3, report = 1) {
	if(is.null(comparison)){
		comparison <- base
	} else if (is.null(base)) {
		stop("You must provide at least a base model for umxCompare")
	}
	tableOut  = OpenMx::mxCompare(base = base, comparison = comparison, all = all)

	# | 1       |    2          | 3  | 4        | 5   | 6        | 7        | 8      | 9    |
	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf | p    |
	# | twinSat | <NA>          | 13 | 333.0781 | 149 | 35.07809 | NA       | NA     | NA   |
	# | twinSat | betaSetToZero | 10 | 351.6486 | 152 | 47.64858 | 18.57049 | 3      | 0.01 |

	tablePub  = tableOut[, c("comparison", "ep", "diffLL"      , "diffdf"    , "p", "AIC", "base")]
	names(tablePub)     <- c("Model"     , "EP", "&Delta; -2LL", "&Delta; df", "p", "AIC", "Compare with Model")
	# Fix problem where base model has compare set to its own name, and name set to NA
	nRows = dim(tablePub)[1]
	for (i in 1:nRows) {
		if(is.na(tablePub[i, "Model"])){
			tablePub[i, "Model"] = tablePub[i, "Compare with Model"] 
			tablePub[i, "Compare with Model"] = NA
		}
	}
	tablePub[,"p"] = umx_APA_pval(tablePub[, "p"], min = (1/ 10^digits), rounding = digits, addComparison = NA)
	# c("1: Comparison", "2: Base", "3: EP", "4: AIC", "5: &Delta; -2LL", "6: &Delta; df", "7: p")
	if(report > 1){
		n_rows = dim(tablePub)[1]
		for (i in 1:n_rows) {
			if(!is.na(tablePub[i, "p"])){
				if(tableOut[i, 9] < .05){
					did_didnot = ". This caused a significant loss of fit "
				} else {
					did_didnot = ". This did not lower fit significantly"
				}
				message(
				"The hypothesis that ", tablePub[i,"Model"], 
				" was tested by dropping ", tablePub[i,"Model"],
				" from ", tablePub[i,"Compare with Model"], 
				did_didnot, 
				"(\u03A7\u00B2(", tablePub[i, 4], ") = ", round(tablePub[i, 3], 2), # \u03A7 = Chi \u00B2 = superscript 2
				", p = ", tablePub[i,"p"], ")."
				)
			}
		}
	}
	
	if(report == 3){
		R2HTML::HTML(tablePub, file = "tmp.html", Border = 0, append = FALSE, sortableDF = TRUE); system(paste0("open ", "tmp.html"))
	} else {
		umx_print(tablePub)
		# R2HTML::print(tableOut, output = output, rowlabel = "")
	}
	invisible(tablePub)
	
	# " em \u2013 dash"
   # Delta (U+0394)
   # &chi;
 	# "Chi \u03A7"
	# "chi \u03C7"
	# if(export){
	# 	fName= "Model.Fitting.xls"
	# 	write.table(tableOut,fName, row.names = FALSE,sep = "\t", fileEncoding="UTF-8") # macroman UTF-8 UTF-16LE
	# 	system(paste("open", fName));
	# }
}

#' umxCI
#'
#' umxCI adds mxCI() calls for all free parameters in a model, 
#' runs the CIs, and reports a neat summary.
#'
#' This function also reports any problems computing a CI. The codes are standard OpenMx errors and warnings
#' \itemize{
#' \item 1: The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN)
#' \item 2: The linear constraints and bounds could not be satisfied. The problem has no feasible solution.
#' \item 3: The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.
#' \item 4: The major iteration limit was reached (Mx status BLUE).
#' \item 6: The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)
#' \item 7: The function derivates returned by funcon or funobj appear to be incorrect.
#' \item 9: An input parameter was invalid
#' }
#' 
#' @param model The \code{\link{mxModel}} you wish to report \code{\link{mxCI}}s on
#' @param add Whether or not to add mxCIs if none are found (defaults to TRUE)
#' @param run Whether or not to compute the CIs. Valid values = "no" (default), "yes", "if necessary".                                                  
#' @param showErrors Whether to show errors (default == TRUE)
#' @details If runCIs is FALSE, the function simply adds CIs to be computed and returns the model.
#' @return - \code{\link{mxModel}}
#' @family umx reporting
#' @seealso - \code{\link{mxCI}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - http://www.github.com/tbates/umx/
#' @export
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m1$intervals # none yet list()
#' m1 = umxCI(m1)
#' m1$intervals # $G_to_x1
#' m1 = umxCI(m1, add = TRUE) # Add CIs for all free parameters, and return model
#' \dontrun{
#' umxCI(model, run = "yes") # force update of CIs
#' # Don't force update of CIs, but if they were just added, then calculate them
#' umxCI(model, run = "if necessary")
#' }
umxCI <- function(model = NULL, add = TRUE, run = c("no", "yes", "if necessary"), showErrors = TRUE) {
	# TODO add code to not-run CIs
	# TODO superceed this with confint? just need parameters to hold the 95% etc...
	run = umx_default_option(run, c("no", "yes", "if necessary"), check = TRUE)
	run = tolower(run)
	if(add){
		# TODO remove existing CIs to avoid duplicates?
		# TODO ensure each CI is added individually
		# TODO support breaking these out into separate models and reassembling them
		CIs   = names(omxGetParameters(model, free = TRUE))
		model = mxModel(model, mxCI(CIs))
	}
    
	if(run == "yes" | (!umx_has_CIs(model) & run == "if necessary")) {
		model = mxRun(model, intervals = TRUE)
	}else{
		message("Not running CIs, run==", run)
	}

	if(umx_has_CIs(model)){
		message("### CIs for model ", model@name)
		confint(model, showErrorcodes = showErrorcodes)
		# model_summary = summary(model)
		# model_CIs = round(model_summary$CI, 3)
		# model_CI_OK = model@output$confidenceIntervalCodes
		# colnames(model_CI_OK) <- c("lbound Code", "ubound Code")
		# model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
		# print(model_CIs)
		# npsolMessages <- list(
		# '1' = 'The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN).',
		# '2' = 'The linear constraints and bounds could not be satisfied. The problem has no feasible solution.',
		# '3' = 'The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.',
		# '4' = 'The major iteration limit was reached (Mx status BLUE).',
		# '5' = 'not used',
		# '6' = 'The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)',
		# '7' = 'The function derivates returned by funcon or funobj appear to be incorrect.',
		# '8' = 'not used',
		# '9' = 'An input parameter was invalid')
		# if(any(model_CI_OK !=0) & showErrorcodes){
		# 	codeList = c(model_CI_OK[,"lbound Code"], model_CI_OK[,"ubound Code"])
		# 	relevantCodes = unique(codeList); relevantCodes = relevantCodes[relevantCodes !=0]
		# 	for(i in relevantCodes) {
		# 	   print(paste0(i, ": ", npsolMessages[i][[1]]))
		# 	}
		# }
	}
	invisible(model)
}

#' umxCI_boot
#'
#' Compute boot-strapped Confidence Intervals for parameters in an \code{\link{mxModel}}
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
#' @examples
#' \dontrun{
#' 	require(OpenMx)
#' 	data(demoOneFactor)
#' 	latents  = c("G")
#' 	manifests = names(demoOneFactor)
#' 	m1 <- mxModel("One Factor", type = "RAM", 
#' 		manifestVars = manifests, latentVars = latents, 
#' 		mxPath(from = latents, to = manifests),
#' 		mxPath(from = manifests, arrows = 2),
#' 		mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 		mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' 	)
#' 	m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' 	umxCI_boot(m1, type = "par.expected")
#'}
#' @references - \url{http://openmx.psyc.virginia.edu/thread/2598}
#' Original written by \url{http://openmx.psyc.virginia.edu/users/bwiernik}
#' @seealso - \code{\link{umxExpMeans}}, \code{\link{umxExpCov}}
#' @family umx reporting
umxCI_boot <- function(model, rawData = NULL, type = c("par.expected", "par.observed", "empirical"), std = TRUE, rep = 1000, conf = 95, dat = FALSE, digits = 3) {
	require(MASS); require(OpenMx); require(umx)
	type = umx_default_option(type, c("par.expected", "par.observed", "empirical"))
	if(type == "par.expected") {
		exp = umxExpCov(model, latents = FALSE)
	} else if(type == "par.observed") {
		if(model$data@type == "raw") {
			exp = var(mxEval(data, model))
		} else { 
			if(model$data@type == "sscp") {
				exp = mxEval(data, model) / (model$data@numObs - 1)
			} else {
				exp = mxEval(data, model)
			}
		}
	}
	N = round(model@data@numObs)
	pard = t(data.frame("mod" = summary(model)$parameters[, 5 + 2 * std], row.names = summary(model)$parameters[, 1]))
	pb   = txtProgressBar(min = 0, max = rep, label = "Computing confidence intervals", style = 3)
	#####
	if(type == "empirical") {
		if(length(rawData) == 0) {
			if(model$data@type == "raw"){
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
			setTxtProgressBar(pb, i)
		}
	} else {
		for(i in 1:rep){
			bsample = var(MASS::mvrnorm(N, rep(0, nrow(exp)), exp))
			mod     = mxRun(mxModel(model, mxData(observed = bsample, type = "cov", numObs = N)), silent = TRUE)
			pard    = rbind(pard, summary(mod)$parameters[, 5 + 2 * std])
			rownames(pard)[nrow(pard)] = i
			setTxtProgressBar(pb, i)
		}
	}
	low = (1-conf/100)/2
	upp = ((1-conf/100)/2) + (conf/100)
	LL  = apply(pard, 2, FUN = quantile, probs = low) #lower limit of confidence interval
	UL  = apply(pard, 2, FUN = quantile, probs = upp) #upper quantile for confidence interval
	LL4 = round(LL, 4)
	UL4 = round(UL, 4)
	ci  = cbind(LL4, UL4)
	colnames(ci) = c(paste((low*100), "%", sep = ""), paste((upp*100), "%", sep = ""))
	p = summary(model)$parameters[, c(1, 2, 3, 4, c(5:6 + 2*std))]
	cols <- sapply(p, is.numeric)
	p[, cols] <- round(p[,cols], digits) 
	
	if(dat) {
		return(list("Type" = type, "bootdat" = data.frame(pard), "CI" = cbind(p, ci)))
	} else {
		return(list("CI" = cbind(p, ci)))
	}
}

#' umxSaturated
#'
#' Computes the saturated and independence forms of a RAM model (needed to return most 
#' fit statistics when using raw data). umxRun calls this automagically.
#'
#' @param model an \code{\link{mxModel}} to get independence and saturated fits to
#' @param evaluate FALSE
#' @param verbose How much feedback to give.
#' @return - A list of the saturated and independence models, from which fits can be extracted
#' @export
#' @family umx build functions, umx reporting functions
#' @seealso - \code{\link{umxSummary}}, \code{\link{umxRun}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model_sat = umxSaturated(model)
#' summary(model, SaturatedLikelihood = model_sat$Sat, IndependenceLikelihood = model_sat$Ind)
#' }
umxSaturated <- function(model, evaluate = TRUE, verbose = TRUE) {
	# TODO: Update to omxSaturated() and omxIndependenceModel()
	# TODO: Update IndependenceModel to analytic form
	if (!(isS4(model) && is(model, "MxModel"))) {
		stop("'model' must be an mxModel")
	}

	if (length(model@submodels)>0) {
		stop("Cannot yet handle submodels")
	}
	if(! model@data@type == "raw"){
		stop("You don't need to run me for cov or cor data - only raw")
	}
	theData = model@data@observed
	if (is.null(theData)) {
		stop("'model' does not contain any data")
	}
	manifests           = model@manifestVars
	nVar                = length(manifests)
	dataMeans           = colMeans(theData, na.rm = TRUE)
	meansLabels         = paste("mean", 1:nVar, sep = "")
	covData             = cov(theData, use = "pairwise.complete.obs")
	factorLoadingStarts = t(chol(covData))
	independenceStarts  = diag(covData)
	loadingsLabels      = paste0("F", 1:nVar, "loading")

	# Set latents to a new set of 1 per manifest
	# Set S matrix to an Identity matrix (i.e., variance fixed@1)
	# Set A matrix to a Cholesky, manifests by manifests in size, free to be estimated 
	# TODO: start the cholesky at the cov values
	m2 <- mxModel("sat",
    	# variances set at 1
		# mxMatrix(name = "factorVariances", type="Iden" , nrow = nVar, ncol = nVar), # Bunch of Ones on the diagonal
	    # Bunch of Zeros
		mxMatrix(name = "factorMeans"   , type = "Zero" , nrow = 1   , ncol = nVar), 
	    mxMatrix(name = "factorLoadings", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = factorLoadingStarts), 
		# labels = loadingsLabels),
	    mxAlgebra(name = "expCov", expression = factorLoadings %*% t(factorLoadings)),

	    mxMatrix(name = "expMean", type = "Full", nrow = 1, ncol = nVar, values = dataMeans, free = TRUE, labels = meansLabels),
	    mxFIMLObjective(covariance = "expCov", means = "expMean", dimnames = manifests),
	    mxData(theData, type = "raw")
	)
	m3 <- mxModel("independence",
	    # TODO: slightly inefficient, as this has an analytic solution
	    mxMatrix(name = "variableLoadings" , type="Diag", nrow = nVar, ncol = nVar, free = TRUE, values = independenceStarts), 
		# labels = loadingsLabels),
	    mxAlgebra(name = "expCov", expression = variableLoadings %*% t(variableLoadings)),
	    mxMatrix(name  = "expMean", type = "Full", nrow = 1, ncol = nVar, values = dataMeans, free = TRUE, labels = meansLabels),
	    mxFIMLObjective(covariance = "expCov", means = "expMean", dimnames = manifests),
	    mxData(theData, type = "raw")
	)
	m2 <- mxOption(m2, "Calculate Hessian", "No")
	m2 <- mxOption(m2, "Standard Errors"  , "No")
	m3 <- mxOption(m3, "Calculate Hessian", "No")
	m3 <- mxOption(m3, "Standard Errors"  , "No")
	if(evaluate) {
		m2 = mxRun(m2)
		m3 = mxRun(m3)
	}
	if(verbose) {
		message("note: umxRun() will compute saturated for you...")
	}
	return(list(Sat = m2, Ind = m3))
}

#' umxStandardizeACE
#'
#' standardize an ACE model
#'
#' @param fit an \code{\link{mxModel}} to standardize
#' @return - standardized ACE \code{\link{mxModel}}
#' @export
#' @family umx.twin model report
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' fit = umxStandardizeACE(fit)
#' }
umxStandardizeACE <- function(fit) {
	if(typeof(fit) == "list"){ # call self recursively
		for(thisFit in fit) {
			message("Output for Model: ",thisFit@name)
			umxStandardizeACE(thisFit)
		}
	} else {
		if(!umx_has_been_run(fit)){
			stop("I can only standardize ACE models that have been run. Just do\n",
			"yourModel = mxRun(yourModel)")
		}
		selDVs = dimnames(fit$top.expCovMZ)[[1]]
		nVar <- length(selDVs)/2;
		# Calculate standardised variance components
		a  <- mxEval(top.a, fit);   # Path coefficients
		c  <- mxEval(top.c, fit);
		e  <- mxEval(top.e, fit);

		A  <- mxEval(top.A, fit);   # Variances
		C  <- mxEval(top.C, fit);
		E  <- mxEval(top.E, fit);
		Vtot = A+C+E;               # Total variance
		I  <- diag(nVar);           # nVar Identity matrix
		SD <- solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	
		# Standardized _path_ coefficients ready to be stacked together
		fit@submodels$top@matrices$a@values = SD %*% a; # Standardized path coefficients
		fit@submodels$top@matrices$c@values = SD %*% c;
		fit@submodels$top@matrices$e@values = SD %*% e;
		return(fit)
	}
}

# ============
# = Graphics =
# ============
#' plot.MxModel
#'
#' Create graphical path diagrams from your OpenMx models!
#'
#' @aliases umxPlot
#' @rdname plot.MxModel
#' @param model an \code{\link{mxModel}} to make a path diagram from
#' @param std Whether to standardize the model.
#' @param digits The number of decimal places to add to the path coefficients
#' @param dotFilename A file to write the path model to. if you leave it at the default "name", then the model's internal name will be used
#' @param pathLabels Whether to show labels on the paths. both will show both the parameter and the label. ("both", "none" or "labels")
#' @param showFixed Whether to show fixed paths (defaults to FALSE)
#' @param showMeans Whether to show means
#' @param showError Whether to show errors
#' @param ... Optional parameters
#' @export
#' @export plot.MxModel
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @family umx reporting
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' plot(m1)
#' }
plot.MxModel <- function(model = NA, std = TRUE, digits = 2, dotFilename = "name", pathLabels = c("none", "labels", "both"), showFixed = FALSE, showMeans = TRUE, showError = TRUE, ...) {
	# ==========
	# = Setup  =
	# ==========
	valid_PathLabels = c("none", "labels", "both")
	pathLabels = umx_default_option(pathLabels, valid_PathLabels, check = TRUE)

	latents = model@latentVars   # 'vis', 'math', and 'text' 
	selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
	if(std){ model = umxStandardizeModel(model, return = "model") }

	# ========================
	# = Get Symmetric & Asymmetric Paths =
	# ========================
	out = "";
	out = xmu_dot_make_paths(mxMat = model@matrices$A, stringIn = out, heads = 1, showFixed = showFixed, pathLabels = pathLabels, comment = "Single arrow paths", digits = digits)
	out = xmu_dot_make_paths(mxMat = model@matrices$S, stringIn = out, heads = 2, showFixed = showFixed, pathLabels = pathLabels, comment = "Variances", digits = digits)
	tmp = xmu_dot_make_residuals(model@matrices$S, style = NULL, showFixed = TRUE, digits = digits)
	variances     = tmp$variances
	varianceNames = tmp$varianceNames
	# ============================
	# = Make the manifest shapes =
	# ============================
	# x1 [label="E", shape = square];
	preOut = "";
	for(var in selDVs) {
	   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
	}
	# ================
	# = handle means =
	# ================
	if(umx_has_means(model) & showMeans){
		out = paste0(out, "\n\t# Means paths\n")
		# Add a triangle to the list of shapes
		preOut = paste0(preOut, "\t one [shape = triangle];\n")
		mxMat = model@matrices$M
		mxMat_vals   = mxMat@values
		mxMat_free   = mxMat@free
		mxMat_labels = mxMat@labels
		meanVars = colnames(mxMat@values)
		for(target in meanVars) {
			thisPathLabel = mxMat_labels[1, target]
			thisPathFree  = mxMat_free[1, target]
			thisPathVal   = round(mxMat_vals[1, target], digits)
			if(thisPathFree){ labelStart = ' [label="' } else { labelStart = ' [label="@' }

			# TODO find a way of showing means fixed at zero?
			if(thisPathFree | showFixed ) {
			# if(thisPathFree | (showFixed & thisPathVal != 0) ) {
				out = paste0(out, "\tone -> ", target, labelStart, thisPathVal, '"];\n')
			}else{
				# cat(paste0(out, "\tone -> ", target, labelStart, thisPathVal, '"];\n'))
				# return(thisPathVal != 0)
			}
		}
	}
	# ===========================
	# = Make the variance lines =
	# ===========================
	# x1_var [label="0.21", shape = plaintext];
	for(var in variances) {
	   preOut = paste0(preOut, "\t", var, ";\n")
	}
	# ======================
	# = Set the ranks e.g. =
	# ======================
	# {rank=same; x1 x2 x3 x4 x5 };
	# TODO more intelligence possible in plot() perhaps hints like "MIMIC" or "ACE"
	rankVariables = paste0("\t{rank=min ; ", paste(latents, collapse = "; "), "};\n")
	rankVariables = paste0(rankVariables, "\t{rank=same; ", paste(selDVs, collapse = " "), "};\n")
	if(umx_has_means(model)){ append(varianceNames, "one")}

	rankVariables = paste0(rankVariables, "\t{rank=max ; ", paste(varianceNames, collapse = " "), "};\n")

	# ===================================
	# = Assemble full text to write out =
	# ===================================
	digraph = paste("digraph G {\n", preOut, out, rankVariables, "\n}", sep = "\n");

	print("nb: see ?plot.MxModel for options - std, digits, dotFilename, pathLabels, showFixed, showMeans, showError")
	if(!is.na(dotFilename)){
		if(dotFilename == "name"){
			dotFilename = paste0(model@name, ".dot")
		}
		cat(digraph, file = dotFilename) # write to file
		system(paste("open", shQuote(dotFilename)));
		# invisible(cat(digraph))
	} else {
		return (cat(digraph));
	}
}

#' umxPlotACE
#'
#' Make a graphical display of an ACE model
#'
#' @aliases plot plot.MxModel.ACE
#' @param model \code{\link{mxModel}} to plot (created by umxACE in order to inherit the MxModel.ACE class)
#' @param dotFilename the name of the file that is created (use "name" to create the file using the model's name parameter)
#' @param digits How many decimals to include in path loadings (2)
#' @param showMeans Whether to show means paths (F)
#' @param std whether to standardize the model (T)
#' @return - optionally return the dot code
#' @export
#' @family umx reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' selDVs = c("bmi1","bmi2")
#' mzData <- subset(twinData, ZYG == "MZFF", selDVs)
#' dzData <- subset(twinData, ZYG == "DZFF", selDVs)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' \dontrun{
#' plot(model)
#' umxPlotACE(model, dotFilename = NA)
#' umxPlotACE(model, std = F)
#' }
umxPlotACE <- function(model = NA, dotFilename = "name", digits = 2, showMeans = F, std = T) {
	if(std){
		model = umxStandardizeACE(model)
	}
	out = "";
	latents  = c();

	if(m1@submodels$MZ@data$type == "raw"){
		selDVs = names(model@submodels$MZ@data@observed)
	}else{
		selDVs = dimnames(model@submodels$MZ@data@observed)[[1]]
	}

	varCount = length(selDVs)/2;
	parameterKeyList = omxGetParameters(model);
	for(thisParam in names(parameterKeyList) ) {
		value = parameterKeyList[thisParam]
		if(class(value) == "numeric") {
			value = round(value, digits)
		}
		if (grepl("^[ace]_r[0-9]+c[0-9]+", thisParam)) { # a c e
			from    = sub('([ace])_r([0-9]+)c([0-9]+)', '\\1\\3', thisParam, perl = T);  # a c or e
			target  = as.numeric(sub('([ace])_r([0-9]+)c([0-9]+)', '\\2', thisParam, perl = T));
			target  = selDVs[as.numeric(target)]
			latents = append(latents, from)
			show = T
		} else { # means probably
			if(showMeans){
				show = T
			} else {
				show = F
			}
			from   = thisParam;
			target = sub('r([0-9])c([0-9])', 'var\\2', thisParam, perl=T) 
		}
		if(show){
			out = paste0(out, from, " -> ", target, " [label = \"", value, "\"]", ";\n")
		}
	}
	preOut = "";
	for(var in selDVs[1:varCount]) {
	   preOut = paste0(preOut, "\n", var, " [shape = box];\n")
	}

	latents = unique(latents)
	rankVariables = paste("\t{rank = same; ", paste(selDVs[1:varCount], collapse = "; "), "};\n") # {rank = same; v1T1; v2T1;}
	# grep('a', latents, value=T)
	rankA   = paste("\t{rank = min; ", paste(grep('a'   , latents, value=T), collapse="; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[ce]', latents, value=T), collapse="; "), "};\n") # {rank=min; c1; e1}
	digraph = paste("digraph G {\n\tsplines = \"FALSE\";\n", preOut, out, rankVariables, rankA, rankCE, "\n}", sep="");
	# cat(digraph);
	# return (out)
	if(!is.na(dotFilename)){
		if(dotFilename=="name"){
			dotFilename = paste0(model@name, ".dot");
		}
		cat(digraph, file = dotFilename)
		system(paste0("open '", dotFilename, "'"));
		# return(invisible(cat(digraph)))
	} else {
		return (cat(digraph));
	}
} # end umxPlotACE

plot.MxModel.ACE <- umxPlotACE

#' umxMI
#'
#' Report modifications which would improve fit.
#'
#' @param model An \code{\link{mxModel}} for which to report modification indices
#' @param numInd How many modifications to report
#' @param typeToShow Whether to shown additions or deletions (default = "both")
#' @param decreasing How to sort (default = TRUE, decreasing)
#' @param cache = Future function to cache these time-consuming results
#' @seealso - \code{\link{umxAdd1}}, \code{\link{umxDrop1}}, \code{\link{umxRun}}, \code{\link{umxSummary}}
#' @family umx modify model, umx reporting
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' \dontrun{
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxMI(model)
#' umxMI(model, numInd=5, typeToShow="add") # valid options are "both|add|delete"
#' }

umxMI <- function(model = NA, numInd = 10, typeToShow = "both", decreasing = TRUE, cache = TRUE) {
	# depends on xmuMI(model)
	if(typeof(model) == "list"){
		mi.df = model
	} else {
		mi = xmuMI(model, vector = TRUE)
		mi.df = data.frame(path= as.character(attributes(mi$mi)$names), value=mi$mi);
		row.names(mi.df) = 1:nrow(mi.df);
		# TODO: could be a helper: choose direction
		mi.df$from = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\1", mi.df$path)
		mi.df$to   = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\3", mi.df$path)
		mi.df$arrows = 1
		mi.df$arrows[grepl("<->", mi.df$path)]= 2		

		mi.df$action = NA 
		mi.df  = mi.df[order(abs(mi.df[,2]), decreasing = decreasing),] 
		mi.df$copy = 1:nrow(mi.df)
		for(n in 1:(nrow(mi.df)-1)) {
			if(grepl(" <- ", mi.df$path[n])){
				tmp = mi.df$from[n]; mi.df$from[n] = mi.df$to[n]; mi.df$to[n] = tmp 
			}
			from = mi.df$from[n]
			to   = mi.df$to[n]
			a = (model@matrices$S@free[to,from] |model@matrices$A@free[to,from])
			b = (model@matrices$S@values[to,from]!=0 |model@matrices$A@values[to,from] !=0)
			if(a|b){
				mi.df$action[n]="delete"
			} else {
				mi.df$action[n]="add"
			}
			inc= min(4,nrow(mi.df)-(n))
			for (i in 1:inc) {
				if((mi.df$copy[(n)])!=n){
					# already dirty
				}else{
					# could be a helper: swap two 
					from1 = mi.df[n,"from"]     ; to1   = mi.df[n,"to"]
					from2 = mi.df[(n+i),"from"] ; to2   = mi.df[(n+i),'to']
					if((from1==from2 & to1==to2) | (from1==to2 & to1==from2)){
						mi.df$copy[(n+i)]<-n
					}
				}		
			}
		}
	}
	mi.df = mi.df[unique(mi.df$copy),] # c("copy")
	if(typeToShow != "both"){
		mi.df = mi.df[mi.df$action == typeToShow,]
	}
	print(mi.df[1:numInd, !(names(mi.df) %in% c("path","copy"))])
	invisible(mi.df)		
}

# ======================
# = Path tracing rules =
# ======================
#' umxUnexplainedCausalNexus
#'
#' umxUnexplainedCausalNexus report the effect of a change (delta) in a variable (from) on an output (to)
#'
#' @param from A variable in the model that you want to imput the effect of a change
#' @param delta A the amount to simulate changing \"from\" by. 
#' @param to The dependent variable that you want to watch changing
#' @param model The model containing from and to
#' @seealso - \code{\link{umxRun}}, \code{\link{mxCompare}}
#' @family umx modification functions
#' @references - http://www.github.com/tbates/umx/
#' @export
#' @examples
#' \dontrun{
#' umxUnexplainedCausalNexus(from="yrsEd", delta = .5, to = "income35", model)
#' }

umxUnexplainedCausalNexus <- function(from, delta, to, model) {
	manifests = model@manifestVars
	partialDataRow <- matrix(0, 1, length(manifests))  # add dimnames to support string varnames 
	dimnames(partialDataRow) = list("val", manifests)
	partialDataRow[1, from] <- delta # delta is in raw "from" units
	partialDataRow[1, to]   <- NA
	completedRow <- umxConditionalsFromModel(model, partialDataRow, meanOffsets = TRUE)
	# by default, meanOffsets = FALSE, and the results take expected means into account
	return(completedRow[1, to])
}

umxConditionalsFromModel <- function(model, newData = NULL, returnCovs = FALSE, meanOffsets = FALSE) {
	# original author: [Timothy Brick](http://www.github.com/tbates/umx/users/tbrick)
	# [history](http://www.github.com/tbates/umx/thread/2076)
	# Called by: umxUnexplainedCausalNexus
	# TODO:  Special case for latent variables
	# FIXME: Update for fitfunction/expectation
	expectation <- model$objective
	A <- NULL
	S <- NULL
	M <- NULL
	
	# Handle missing data
	if(is.null(newData)) {
		data <- model$data
		if(data@type != "raw") {
			stop("Conditionals requires either new data or a model with raw data.")
		}
		newData <- data@observed
	}
	
	if(is.list(expectation)) {  # New fit-function style
		eCov  <- model$fitfunction@info$expCov
		eMean <- model$fitfunction@info$expMean
		expectation <- model$expectation
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X@values, list(X=expectation@A)))
			S <- eval(substitute(model$X@values, list(X=expectation@S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation@M)) {
				M <- eval(substitute(model$X@values, list(X=expectation@M)))
			}
		}
	} else { # Old objective-style
		eCov <- model$objective@info$expCov
		eMean <- model$objective@info$expMean
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X@values, list(X=expectation@A)))
			S <- eval(substitute(model$X@values, list(X=expectation@S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation@M)) {
				M <- eval(substitute(model$X@values, list(X=expectation@M)))
			}
		}
	}

	if(!is.null(A)) {
		# RAM model: calculate total expectation
		I <- diag(nrow(A))
		Z <- solve(I-A)
		eCov <- Z %*% S %*% t(Z)
		if(!is.null(M)) {
			eMean <- Z %*% t(M)
		}
		latents <- model@latentVars
		newData <- data.frame(newData, matrix(NA, ncol=length(latents), dimnames=list(NULL, latents)))
	}
	
	# No means
	if(meanOffsets || !dim(eMean)[1]) {
		eMean <- matrix(0.0, 1, ncol(eCov), dimnames=list(NULL, colnames(eCov)))
	}
	
	# TODO: Sort by pattern of missingness, lapply over patterns
	nRows = nrow(newData)
	outs <- omxApply(newData, 1, umxComputeConditionals, sigma=eCov, mu=eMean, onlyMean=!returnCovs)
	if(returnCovs) {
		means <- matrix(NA, nrow(newData), ncol(eCov))
		covs <- rep(list(matrix(NA, nrow(eCov), ncol(eCov))), nRows)
		for(i in 1:nRows) {
			means[i,] <- outs[[i]]$mu
			covs[[i]] <- outs[[i]]$sigma
		}
		return(list(mean = means, cov = covs))
	}
	return(t(outs))
}

umxComputeConditionals <- function(sigma, mu, current, onlyMean = FALSE) {
	# Usage: umxComputeConditionals(model, newData)
	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	# original author: [Timothy Brick](http://www.github.com/tbates/umx/users/tbrick)
	# [history](http://www.github.com/tbates/umx/thread/2076)
	# called by umxConditionalsFromModel()
	if(dim(mu)[1] > dim(mu)[2] ) {
		mu <- t(mu)
	}

	nVar <- length(mu)
	vars <- colnames(sigma)

	if(!is.matrix(current)) {
		current <- matrix(current, 1, length(current), dimnames=list(NULL, names(current)))
	}
	
	# Check inputs
	if(dim(sigma)[1] != nVar || dim(sigma)[2] != nVar) {
		stop("Non-conformable sigma and mu matrices in conditional computation.")
	}
	
	if(is.null(vars)) {
		vars <- rownames(sigma)
		if(is.null(vars)) {
			vars <- colnames(mu)
			if(is.null(vars)) {
				vars <- names(current)
				if(is.null(vars)) {
					vars <- paste("X", 1:dim(sigma)[1], sep="")
					names(current) <- vars
				}
				names(mu) <- vars
			}
			dimnames(sigma) <- list(vars, vars)
		}
		rownames(sigma) <- vars
	}
	
	if(is.null(colnames(sigma))) {
		colnames(sigma) <- vars
	}
	
	if(is.null(rownames(sigma))) {
		rownames(sigma) <- colnames(sigma)
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
		names(current) <- vars[1:ncol(current)]
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			if(length(vars) == 0 || ncol(current) != length(vars)) {
				print(paste("Got mean vector of length ", ncol(current), " and names of length ", length(vars)))
				stop("Length and names of mean values mismatched in conditional computation.")
			}
		}
		names(mu) <- vars
	}
	
	# Get Missing and Non-missing sets
	if(!setequal(names(current), vars)) {
		newSet <- setdiff(vars, names(current))
		current[newSet] <- NA
		current <- current[vars]
	}
	
	# Compute Schur Complement
	# Calculate parts:
	missing <- names(current[is.na(current)])
	nonmissing <- setdiff(vars, missing)
	ordering <- c(missing, nonmissing)
	
	totalCondCov <- NULL

	# Handle all-missing and none-missing cases
	if(length(missing) == 0) {
		totalMean = current
		names(totalMean) <- names(current)
		totalCondCov = sigma
	} 

	if(length(nonmissing) == 0) {
		totalMean = mu
		names(totalMean) <- names(mu)
		totalCondCov = sigma
	}

	# Compute Conditional expectations
	if(is.null(totalCondCov)) {
		
		covMat <- sigma[ordering, ordering]
		missMean <- mu[, missing]
		haveMean <- mu[, nonmissing]

		haves <- current[nonmissing]
		haveNots <- current[missing]

		missCov <- sigma[missing, missing]
		haveCov <- sigma[nonmissing, nonmissing]
		relCov <- sigma[missing, nonmissing]
		relCov <- matrix(relCov, length(missing), length(nonmissing))

		invHaveCov <- solve(haveCov)
		condMean <- missMean + relCov %*% invHaveCov %*% (haves - haveMean)

		totalMean <- current * 0.0
		names(totalMean) <- vars
		totalMean[missing] <- condMean
		totalMean[nonmissing] <- current[nonmissing]
	}

	if(onlyMean) {
		return(totalMean)
	}
	
	if(is.null(totalCondCov)) {
		condCov <- missCov - relCov %*% invHaveCov %*% t(relCov)
	
		totalCondCov <- sigma * 0.0
		totalCondCov[nonmissing, nonmissing] <- haveCov
		totalCondCov[missing, missing] <- condCov
	}	
	return(list(sigma=totalCondCov, mu=totalMean))
	
}


# =========================
# = Pull model components =
# =========================

#' extractAIC.MxModel
#'
#' Returns the AIC for an OpenMx model
#' helper function for \code{\link{logLik.MxModel}} (which enables AIC(model); logLik(model); BIC(model)
#' Original Author: brandmaier
#'
#' @method extractAIC MxModel
#' @rdname extractAIC.MxModel
#' @export
#' @param model an \code{\link{mxModel}} to get the AIC from
#' @return - AIC value
#' @seealso - \code{\link{AIC}}, \code{\link{umxCompare}}, \code{\link{logLik.MxModel}}
#' @family umx reporting functions
#' @references - \url{http://www.github.com/tbates/umx/thread/931#comment-4858}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' extractAIC(m1)
#' # -2.615998
#' AIC(m1)
extractAIC.MxModel <- function(model) {
	require(umx)
	a = umx::umxCompare(model)
	return(a[1, "AIC"])
}

#' coef.MxModel
#'
#' Returns the coeficients from an OpenMx RAM model
#'
#' @method coef MxModel
#' @rdname coef.MxModel
#' @export
#' @param model an \code{\link{mxModel}} to get the AIC from
#' @param ... Optional parameters
#' @return - coefficients
#' @family umx reporting functions
#' @references - 
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' coef(m1)
#' # -2.615998
coef.MxModel <- function(model, ...) {
	# TODO implement this
	message("Not implemented")
	message("TODO implement this using wrapper around mxStandardizeRAMpaths")
}

#' umxExpCov
#'
#' extract the expected covariance matrix from an \code{\link{mxModel}}
#'
#' @param model an \code{\link{mxModel}} to get the covariance matrix from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Leave NULL to do no rounding.
#' @return - expected covariance matrix
#' @export
#' @family umx reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/2598}
#' Original written by \url{http://openmx.psyc.virginia.edu/users/bwiernik}
#' @seealso - \code{\link{umxRun}}, \code{\link{umxCI_boot}}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxExpCov(m1)
#' umxExpCov(m1, digits = 3)
umxExpCov <- function(model, latents = FALSE, manifests = TRUE, digits = NULL){
	# umx_has_been_run(m1)
	if(model@data@type == "raw"){
		manifestNames = names(model$data@observed)
	} else {
		manifestNames = dimnames(model$data@observed)[[1]]
	}
	if(umx_is_RAM(model)){
		if(manifests & !latents){
			# expCov = attr(model$objective[[2]]$result, "expCov")
			thisFit = paste0(model$name, ".fitfunction")
			expCov <- attr(model@output$algebras[[thisFit]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		} else {
			A <- mxEval(A, model)
			S <- mxEval(S, model)
			I <- diag(1, nrow(A))
			E <- solve(I - A)
			expCov <- E %&% S # The model-implied covariance matrix
			mV <- NULL
			if(latents) {
				mV <- model@latentVars 
			}
			if(manifests) {
				mV <- c(mV, model@manifestVars)
			}
			expCov = expCov[mV, mV]
		}
	} else {
		if(latents){
			stop("I don't know how to reliably get the latents for non-RAM models... Sorry :-(")
		} else {
			expCov <- attr(model@output$algebras[[paste0(model$name, ".fitfunction")]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		}
	}
	if(!is.null(digits)){
		expCov = round(expCov, digits)
	}
	return(expCov) 
}
#' umxExpMean
#'
#' Extract the expected means matrix from an \code{\link{mxModel}}
#'
#' @param model an \code{\link{mxModel}} to get the means from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Leave NULL to do no rounding.
#' @return - expected means
#' @export
#' @family umx reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/2598}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(demoOneFactor, type = "raw")
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxExpMeans(model = m1)
#' umxExpMeans(m1, digits = 3)
umxExpMeans <- function(model, manifests = TRUE, latents = NULL, digits = NULL){
	# TODO # what does umxExpMeans do under 1.4?
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
	expMean <- attr(model@output$algebras[[paste0(model$name, ".fitfunction")]], "expMean")
	
	if(model@data@type == "raw"){
		manifestNames = names(model$data@observed)
	} else {
		manifestNames = dimnames(model$data@observed)[[1]]
	}
	dimnames(expMean) = list("mean", manifestNames)
	if(!is.null(digits)){
		expMean = round(expMean, digits)
	}
	return(expMean)
}

#' logLik.MxModel
#'
#' Returns the log likelihood for an OpenMx model. This helper also 
#' enables \code{\link{AIC}}(model); \code{\link{BIC}}(model).
#'
#' hat-tip Andreas Brandmaier
#'
#' @method logLik MxModel
#' @rdname  logLik
#' @export
#' @param model the \code{\link{mxModel}} from which to get the log likelihood
#' @param ... Optional parameters
#' @return - the log likelihood
#' @seealso - \code{\link{AIC}}, \code{\link{umxCompare}}
#' @family umx reporting
#' @references - \url{http://openmx.psyc.virginia.edu/thread/931#comment-4858}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' logLik(m1)
#' AIC(m1)
logLik.MxModel <- function(model, ...) {
	Minus2LogLikelihood <- NA
	if (!is.null(model@output) & !is.null(model@output$Minus2LogLikelihood)){
		Minus2LogLikelihood <- (-0.5) * model@output$Minus2LogLikelihood		
	}
	if (!is.null(model@data)){
		attr(Minus2LogLikelihood,"nobs") <- model@data@numObs
	}else{ 
		attr(Minus2LogLikelihood,"nobs") <- NA
	}
	if (!is.null(model@output)){
		attr(Minus2LogLikelihood,"df") <- length(model@output$estimate)	
	} else {
		attr(Minus2LogLikelihood, "df") <- NA
	}
	class(Minus2LogLikelihood) <- "logLik"
	return(Minus2LogLikelihood);
}

#' umxFitIndices
#'
#' A list of fit indices
#'
#' @param model the \code{\link{mxModel}} you want fit indices for
#' @param indepfit an (optional) saturated \code{\link{mxModel}}
#' @return \code{NULL}
#' @export
#' @family umx reporting
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' \dontrun{
#' umxFitIndices(m1)
#' }
umxFitIndices <- function(model, indepfit) {
	# TODO fix umxFitIndices requirement for indepence if model doesn't need it!
    # TODO use means and compute independence model here for example...
	options(scipen = 3)
	indepSummary     <- summary(indepfit)
	modelSummary <- summary(model)
	N         <- modelSummary$numObs
	N.parms   <- modelSummary$estimatedParameters
	N.manifest <- length(model@manifestVars)
	deviance  <- modelSummary$Minus2LogLikelihood
	Chi       <- modelSummary$Chi
	df        <- modelSummary$degreesOfFreedom
	p.Chi     <- 1 - pchisq(Chi, df)
	Chi.df    <- Chi/df
	indep.chi <- indepSummary$Chi
	indep.df  <- indepSummary$degreesOfFreedom
	q <- (N.manifest*(N.manifest+1))/2
	N.latent     <- length(model@latentVars)
	observed.cov <- model@data@observed
	observed.cor <- cov2cor(observed.cov)

	A <- model@matrices$A@values
	S <- model@matrices$S@values
	F <- model@matrices$F@values
	I <- diag(N.manifest+N.latent)
	estimate.cov <- F %*% (qr.solve(I-A)) %*% S %*% (t(qr.solve(I-A))) %*% t(F)
	estimate.cor <- cov2cor(estimate.cov)
	Id.manifest  <- diag(N.manifest)
	residual.cov <- observed.cov-estimate.cov
	residual.cor <- observed.cor-estimate.cor
	F0       <- max((Chi-df)/(N-1),0)
	NFI      <- (indep.chi-Chi)/indep.chi
	NNFI.TLI <- (indep.chi-indep.df/df*Chi)/(indep.chi-indep.df)
	PNFI     <- (df/indep.df)*NFI
	RFI      <- 1 - (Chi/df) / (indep.chi/indep.df)
	IFI      <- (indep.chi-Chi)/(indep.chi-df)
	CFI      <- min(1.0-(Chi-df)/(indep.chi-indep.df),1)
	PRATIO   <- df/indep.df
	PCFI     <- PRATIO*CFI
	NCP      <- max((Chi-df),0)
	RMSEA    <- sqrt(F0/df) # need confidence intervals
	MFI      <- exp(-0.5*(Chi-df)/N)
	GH       <- N.manifest / (N.manifest+2*((Chi-df)/(N-1)))
	GFI      <- 1 - (
		 sum(diag(((solve(estimate.cor) %*% observed.cor)-Id.manifest) %*% ((solve(estimate.cor) %*% observed.cor) - Id.manifest))) /
	    sum(diag((solve(estimate.cor) %*% observed.cor) %*% (solve(estimate.cor) %*% observed.cor)))
	)
	AGFI     <- 1 - (q/df)*(1-GFI)
	PGFI     <- GFI * df/q
	AICchi   <- Chi+2*N.parms
	AICdev   <- deviance+2*N.parms
	BCCchi   <- Chi + 2*N.parms/(N-N.manifest-2)
	BCCdev   <- deviance + 2*N.parms/(N-N.manifest-2)
	BICchi   <- Chi+N.parms*log(N)
	BICdev   <- deviance+N.parms*log(N)
	CAICchi  <- Chi+N.parms*(log(N)+1)
	CAICdev  <- deviance+N.parms*(log(N)+1)
	ECVIchi  <- 1/N*AICchi
	ECVIdev  <- 1/N*AICdev
	MECVIchi <- 1/BCCchi
	MECVIdev <- 1/BCCdev
	RMR      <- sqrt((2*sum(residual.cov^2))/(2*q))
	SRMR     <- sqrt((2*sum(residual.cor^2))/(2*q))
	indices  <-
	rbind(N,deviance,N.parms,Chi,df,p.Chi,Chi.df,
		AICchi,AICdev,
		BCCchi,BCCdev,
		BICchi,BICdev,
		CAICchi,CAICdev,
		RMSEA,SRMR,RMR,
		GFI,AGFI,PGFI,
		NFI,RFI,IFI,
		NNFI.TLI,CFI,
		PRATIO,PNFI,PCFI,NCP,
		ECVIchi,ECVIdev,MECVIchi,MECVIdev,MFI,GH
	)
	return(indices)
}

# define generic RMSEA...
#' Generic RMSEA function
#'
#' See \code{\link[umx]{RMSEA.MxModel}} to access the RMSEA of MxModels
#'
#' @param x an object to get the RMSEA for
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show
#' @return - RMSEA object containing value (and perhaps a CI)
#' @export
#' @family umx reporting functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
RMSEA <- function(x, ci.lower, ci.upper, digits) UseMethod("RMSEA", x)

#' RMSEA function for MxModels
#'
#' Compute the confidence interval on RMSEA
#'
#' @param x an \code{\link{mxModel}} to get CIs on RMSEA for
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show (defaults to 3)
#' @return - object containing the RMSEA and lower and upper bounds
#' @rdname RMSEA.MxModel
#' @export
#' @family umx reporting
#' @references - \url{https://github.com/simsem/semTools/wiki/Functions}, \url{https://github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' RMSEA(m1)
#' # RMSEA(summary(m1))
RMSEA.MxModel <- function(x, ci.lower = .05, ci.upper = .95, digits = 3) { 
	# "MxRAMModel"
	if(ci.lower != .05 | ci.upper != .95){
		stop("Setting CI on RMSEA not supported for mxModels as yet...")
	}
	sm <- summary(x)
	# return(sm)
	RMSEA_Obj = omxRMSEA(x, ci.lower, ci.upper)
	# lower.rmsea   est.rmsea upper.rmsea
	#  0.00000000  0.03088043  0.07449389
	rmsea.est    = RMSEA_Obj["est.rmsea"]
	rmsea.lower  = RMSEA_Obj["rmsea.lower"]
	rmsea.upper  = RMSEA_Obj["rmsea.upper"]
	rmsea.pvalue = "Not computed yet in OpenMx 2"
	# TODO get pvalue
	txt = paste0("RMSEA = ", round(rmsea.est, digits), " CI", sub("^0?\\.", replacement = "", ci.upper), 
		"[", round(rmsea.lower, digits), ", ", round(rmsea.upper, digits), "], p = ", rmsea.pvalue) # umx_APA_pval(rmsea.pvalue)
	print(txt)
	invisible(list(RMSEA = rmsea.est, RMSEA.lower = rmsea.lower, RMSEA.upper = rmsea.upper, CI.lower = ci.lower, CI.upper = ci.upper, RMSEA.pvalue = rmsea.pvalue, txt = txt))

	# if (is.na(sm$Chi)){
	# 	return(NA);
	# }
	# X2 <- sm$Chi
	# df <- sm$degreesOfFreedom
	# N  <- sm$numObs
	#     N.RMSEA <- max(N, X2 * 4)
	#     G <- max(length(model@submodels), 1)
	#
	#     if (is.na(X2) || is.na(df)) {
	#         RMSEA <- as.numeric(NA)
	#     } else if (df > 0) {
	#         GG <- 0
	#         RMSEA <- sqrt(max(c((X2/N)/df - 1/(N - GG), 0))) *  sqrt(G)
	#     } else {
	#         RMSEA <- 0
	#     }
	#
	# lower.lambda <- function(lambda) {
	# 	pchisq(X2, df = df, ncp = lambda) - ci.upper
	# }
	# upper.lambda <- function(lambda) {
	# 	(pchisq(X2, df = df, ncp = lambda) - ci.lower)
	# }
	#
	# if (is.na(X2) || is.na(df)) {
	# 	rmsea.lower <- NA
	# } else if (df < 1 || lower.lambda(0) < 0) {
	# 	rmsea.lower <- 0
	# } else {
	# 	lambda.l <- try(uniroot(f = lower.lambda, lower = 0, upper = X2)$root)
	# 	if (inherits(lambda.l, "try-error")) {
	# 		lambda.l <- NA
	# 	}
	# 	GG <- 0
	# 	rmsea.lower <- sqrt(lambda.l/((N - GG) * df)) * sqrt(G)
	# }
	# if (is.na(X2) || is.na(df)) {
	# 	rmsea.upper <- NA
	# } else if (df < 1 || upper.lambda(N.RMSEA) > 0 || upper.lambda(0) < 0) {
	# 	rmsea.upper <- 0
	# } else {
	# 	lambda.u <- try(uniroot(f = upper.lambda, lower = 0, upper = N.RMSEA)$root)
	# 	if (inherits(lambda.u, "try-error")) {
	# 		lambda.u <- NA
	# 	}
	# 	GG <- 0
	# 	rmsea.upper <- sqrt(lambda.u/((N - GG) * df)) * sqrt(G)
	# }
	# # compute p-value
	# if (is.na(X2) || is.na(df)) {
	# 	rmsea.pvalue <- as.numeric(NA)
	# } else if (df > 0) {
	# 	GG <- 0
	# 	ncp <- (N - GG) * df * 0.05^2/G
	# 	rmsea.pvalue <- (1 - pchisq(X2, df = df, ncp = ncp))
	# } else {
	# 	rmsea.pvalue <- 1
	# }
}

RMSEA.summary.mxmodel <- function(m_summary, ci.lower = .05, ci.upper = .95, digits = 3){
	message("TODO reverse these: so summary does all the work...")
	# TODO reverse these: so summary does all the work...
	# m_summary
	# RMSEA.MxModel(model, ci.lower = ci.lower, ci.upper = ci.upper, digits = digits)
}

#' umxDescriptives
#'
#' Summarize data for an APA style subjects table
#'
#' @param data          data.frame to compute descriptive statistics for
#' @param measurevar    The data column to summarise
#' @param groupvars     A list of columns to group the data by
#' @param na.rm         whether to remove NA from the data
#' @param conf.interval The size of the CI you request - 95 by default
#' @param .drop         Whether to drop TODO
#' @family umx reporting
#' @export
#' @references - \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data}
#' @examples
#' \dontrun{
#' umxDescriptives(data)
#' }

umxDescriptives <- function(data = NULL, measurevar, groupvars = NULL, na.rm = FALSE, conf.interval = .95, .drop = TRUE) {
    require(plyr)
    # New version of length which can handle NA's: if na.rm == T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm){
			sum(!is.na(x))        	
        } else { 
            length(x)
		}
    }

    # The summary; it's not easy to understand...
    datac <- plyr::ddply(data, groupvars, .drop = .drop,
           .fun = function(xx, col, na.rm) {
                   c( N    = length2(xx[,col], na.rm=na.rm),
                      mean = mean   (xx[,col], na.rm=na.rm),
                      sd   = sd     (xx[,col], na.rm=na.rm)
                      )
                  },
            measurevar,
            na.rm
    )
    # Rename the "mean" column
    datac    <- umx_rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
}
