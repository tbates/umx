# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::check("~/bin/umx")
# =====================
# = Model Diagnostics =
# =====================

#' mxDiagnostic
#'
#' Diagnose problems in a model - this is a work in progress.
#'
#' @param model an \code{\link{mxModel}} to diagnose
#' @param tryHard whether I should try and fix it? (defaults to FALSE)
#' @param diagonalizeExpCov Whether to diagonalize the ExpCov
#' @return - helpful messages and perhaps a modified model
#' @export
#' @family Model Building Functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
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

#' umxReduce
#'
#' Reduce a model - this is a work in progress.
#'
#' @param m1 an \code{\link{mxModel}} to reduce
#' @param report How to report the results. "html" = open in browser
#' @param baseFileName file (I add the html) to use when report = "html" (defaults to "tmp")
#' @return - 
#' @export
#' @family umx core functions
#' @references - \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' model = umxReduce(model)
#' }
umxReduce <- function(m1, report = "html", baseFileName = "tmp") {
	umx_is_MxModel(m1)
	if(class(m1) == "MxModel.GxE"){
		# Reduce GxE Model
		no_c   = umxModify(m1, "c_r1c1" , name = "no_c"   )
		no_a   = umxModify(m1, "a_r1c1" , name = "no_a"   )
		no_em  = umxModify(m1, "em_r1c1", name = "no_em"  )
		no_cm  = umxModify(m1, "cm_r1c1", name = "no_cm"  )
		no_am  = umxModify(m1, "am_r1c1", name = "no_am"  )
		no_lin = umxModify(m1, "lin11"  , name = "no_lin" )  # big linear effect of ses on brain size
		no_sq  = umxModify(m1, "quad11" , name = "no_quad")  # no ^2 effect of ses on brain size
		# good to drop the means if possible? I think not. Better to model their most likely value, not lock it too zerp

		no_c_cm   = umxModify(no_c    , "cm_r1c1", name = "no_c_no_cm")
		no_c_cem  = umxModify(no_c_cm , "em_r1c1", name = "no_c_no_em")
		no_c_acem = umxModify(no_c_cem, "am_r1c1", name = "no_a_c_or_em")
		umxCompare(m1, c(no_c, no_a, no_em, no_cm, no_am, no_lin, no_sq), report = "1")
		umxCompare(m1, c(no_c, no_a, no_em, no_cm, no_am, no_lin, no_sq), report = report, file = paste0(baseFileName, ".html"))
		umxCompare(no_c, c(no_c_cm, no_c_cem, no_c_acem), report = "1")
		umxCompare(no_c, c(no_c_cm, no_c_cem, no_c_acem), report = report, file = paste0(baseFileName, 2, ".html"))
		# return(result)
	} else {
		stop("Only GxE implemented so far. Feel free to let me know what you want...")
		# TODO if we get an MxModel.ACE, lets 
		# 1. make umxCP, and umxIP
		# 2. also relaxed CP/IP?
		# 3. report fit table
	}
}

#' umx_drop_ok
#'
#' Print a meaningful sentence about a model comparison. If you use this, please email me and ask to have it
#' merged with \code{\link{umxCompare}}() :-)
#'
#' @param model1 the base code{\link{mxModel}}
#' @param model2 the nested code{\link{mxModel}}
#' @param text name of the thing being tested, i.e., "Extraversion" or "variances"
#' @return - 
#' @export
#' @family Reporting functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents   = c("g")
#' manifests = names(demoOneFactor)
#' myData    = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' m1 <- umxRAM("OneFactor", data = myData,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' m2 = umxModify(m1, update = "g_to_x1", name = "no effect on x1")
#' umx_drop_ok(m1, m2, text = "the path to x1")
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

#' Get residuals from an MxModel
#'
#' Return the \code{\link{residuals}} from an OpenMx RAM model
#'
#' @rdname residuals.MxModel
#' @param object An fitted \code{\link{mxModel}} from which to get residuals
#' @param digits round to how many digits (default = 2)
#' @param suppress smallest deviation to print out (default = NULL = show all)
#' @param ... Optional parameters
#' @return - matrix of residuals
#' @export
#' @family Reporting functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
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
residuals.MxModel <- function(object, digits = 2, suppress = NULL, ...){
	umx_check_model(object, type = NULL, hasData = TRUE)
	expCov = umxExpCov(object, latents = FALSE)
	if(object$data$type == "raw"){
		obsCov = umxHetCor(object$data$observed)
	} else {
		obsCov = object$data$observed
	}
	resid = cov2cor(obsCov) - cov2cor(expCov)
	umx_print(data.frame(resid), digits = digits, zero.print = ".", suppress = suppress)
	if(is.null(suppress)){
		print("nb: You can zoom in on bad values with, e.g. suppress = .01, which will hide values smaller than this. Use digits = to round")
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
#' Base \code{\link[stats]{loadings}} handles \code{\link{factanal}} objects. 
#'
#' @param x an object from which to get loadings 
#' @param ... additional parameters
#' @return - matrix of loadings
#' @export
#' @family Reporting functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
loadings <- function(x, ...) UseMethod("loadings")
#' @export
loadings.default <- function(x, ...) stats::loadings(x, ...) 

# TODO: alternative approach would be to use setGeneric("loadings")

#' loadings.MxModel
#'
#' loadings extracts the factor loadings from an OpenMx EFA (factor analysis) model. 
#' It behaves equivalently to stats::loadings in returning the loadings from an 
#' EFA (factor analysis), but doesn't store the rotation matrix.
#'
#' @param x A RAM model to get which to get loadings 
#' @param ... Other parameters (currently unused)
#' @return - loadings matrix
#' @export
#' @family Reporting Functions
#' @seealso - \code{\link{factanal}}, \code{\link{loadings}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' myVars <- c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(name = "test", factors = 2, data = mtcars[, myVars])
#' loadings(m1)
loadings.MxModel <- function(x, ...) {
	x$A$values[x@manifestVars, x@latentVars, drop = FALSE]
}

#' umx_standardize_RAM
#'
#' umx_standardize_RAM takes a RAM-style model, and returns standardized version.
#'
#' @param model The \code{\link{mxModel}} you wish to standardise
#' @param return What to return. Valid options: "parameters", "matrices", or "model"
#' @param Amatrix Optionally tell the function what the name of the asymmetric matrix is (defaults to RAM standard A)
#' @param Smatrix Optionally tell the function what the name of the symmetric matrix is (defaults to RAM standard S)
#' @param Mmatrix Optionally tell the function what the name of the means matrix is (defaults to RAM standard M)
#' @return - a \code{\link{mxModel}} or else parameters or matrices if you request those
#' @family Reporting functions
#' @references - \url{http://github.com/tbates/umx}
#' @export
#' @examples
#' require(umx)
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
#' m1 = umx_standardize_RAM(m1, return = "model")
#' summary(m1)
umx_standardize_RAM <- function(model, return = "parameters", Amatrix = NA, Smatrix = NA, Mmatrix = NA) {
	if (!(return == "parameters"|return == "matrices"|return == "model")) stop("Invalid 'return' parameter. Do you want do get back parameters, matrices or model?")
	suppliedNames = all(!is.na(c(Amatrix,Smatrix)))
	# if the objective function isn't RAMObjective, you need to supply Amatrix and Smatrix

	if (!umx_is_RAM(model) & !suppliedNames ){
		stop("I need either type = RAM model or the names of the equivalent of the A and S matrices.")
	}
	output <- model$output
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
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model$expectation$A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model$expectation$S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model$expectation$M}
	} else {
		if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model$objective$A}
		if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model$objective$S}
		if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model$objective$M}
	}
	# Get the A and S matrices, and make an identity matrix
	A <- model[[nameA]]
	S <- model[[nameS]]
	I <- diag(nrow(S$values))
	
	# this can fail (non-invertable etc. so we wrap it in try-catch)
	tryCatch({	
		# Calculate the expected covariance matrix
		IA <- solve(I - A$values)
		expCov <- IA %*% S$values %*% t(IA)
		# Return 1/SD to a diagonal matrix
		invSDs <- 1/sqrt(diag(expCov))
		# Give the inverse SDs names, because mxSummary treats column names as characters
		names(invSDs) <- as.character(1:length(invSDs))
		if (!is.null(dimnames(A$values))){names(invSDs) <- as.vector(dimnames(S$values)[[2]])}
		# Put the inverse SDs into a diagonal matrix (might as well recycle my I matrix from above)
		diag(I) <- invSDs
		# Standardize the A, S and M matrices
		#  A paths are value*sd(from)/sd(to) = I %*% A %*% solve(I)
		#  S paths are value/(sd(from*sd(to))) = I %*% S %*% I
		stdA <- I %*% A$values %*% solve(I)
		stdS <- I %*% S$values %*% I
		# Populate the model
		model[[nameA]]$values[,] <- stdA
		model[[nameS]]$values[,] <- stdS
		if (!is.na(nameM)){model[[nameM]]$values[,] <- rep(0, length(invSDs))}
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

#' Get confidence intervals from an MxModel
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
#' @rdname confint.MxModel
#' @param object An \code{\link{mxModel}}, possibly already containing \code{\link{mxCI}}s that have been \code{\link{mxRun}} with intervals = TRUE))
#' @param parm	A specification of which parameters are to be given confidence intervals. Can be "existing", "all", or a vector of names.
#' @param level	The confidence level required (default = .95)
#' @param run Whether to run the model (defaults to FALSE)
#' @param showErrorCodes (default = FALSE)
#' @param ... Additional argument(s) for umxConfint.
#' @export
#' @return - \code{\link{mxModel}}
#' @family Reporting functions
#' @seealso - \code{\link[stats]{confint}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
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
confint.MxModel <- function(object, parm = list("existing", c("vector", "of", "names"), "default = add all"), level = 0.95, run = FALSE, showErrorCodes = FALSE, ...) {
	defaultParmString = list("existing", c("vector", "of", "names"), "default = add all")
	# 1. Add CIs if needed
	if (isTRUE(all.equal(parm, defaultParmString))) {
		if(umx_has_CIs(object, "intervals")) {
			# TODO add a count for the user
			message(length(object$intervals), " CIs found")
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
			message("hmmm... you wanted it run, but I don't see any computed CIs despite there being ", length(object$intervals), " requested...",
			"\nThat's a bug. Please report it to timothy.c.bates at gmail.com")
		}
	} else {
		# model has CIs and they have been run
		model_summary = summary(object)
		CIs = model_summary$CI
		model_CIs   = round(CIs[,c("lbound", "estimate", "ubound")], 3)
		model_CI_OK = object$output$confidenceIntervalCodes
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
		if(any(model_CI_OK !=0) && showErrorCodes){
			codeList = c(model_CI_OK[,"lbound Code"], model_CI_OK[,"ubound Code"])
			relevantCodes = unique(codeList); relevantCodes = relevantCodes[relevantCodes !=0]
			for(i in relevantCodes) {
			   print(paste0(i, ": ", npsolMessages[i][[1]]))
			}
		}
	}
	invisible(object)
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
#' @param which What CIs to add: c("ALL", NA, "list of your making")
#' @param remove = FALSE (if set, removes existing specified CIs from the model)
#' @param run Whether or not to compute the CIs. Valid values = "no" (default), "yes", "if necessary". 
#' @param showErrorCodes Whether to show errors (default == TRUE)
#' @details If runCIs is FALSE, the function simply adds CIs to be computed and returns the model.
#' @return - \code{\link{mxModel}}
#' @family Reporting functions
#' @seealso - \code{\link{mxCI}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - http://www.github.com/tbates/umx/
#' @export
#' @examples
#' require(umx)
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
#' m1 = umxCI(m1, remove = TRUE) # Add CIs for all free parameters, and return model
#' \dontrun{
#' umxCI(model, run = "yes") # force update of CIs
#' # Don't force update of CIs, but if they were just added, then calculate them
#' umxCI(model, run = "if necessary")
#' }
umxCI <- function(model = NULL, which = c("ALL", NA, "list of your making"), remove = FALSE, run = c("no", "yes", "if necessary"), showErrorCodes = TRUE) {
	# TODO superceed this with confint? just need parameters to hold the 95% etc...
	run = match.arg(run)
	which = umx_default_option(which, c("ALL", NA, "list of your making"), check = FALSE)
	if(remove){
		if(which == "ALL"){
			CIs = names(model$intervals)
		} else {
			CIs = which 
		}
		if(length(names(model$intervals))>0){
			model = mxModel(model, mxCI(CIs), remove = TRUE)
		} else {
			message("model has no intervals to remove")
		}
	} else {
		# TODO Avoid duplicating existing CIs?
		# TODO Add each CI individually
		# TODO Break them out into separate models and reassemble if on cluster?
		if(which == "ALL"){
			CIs = names(omxGetParameters(model, free = TRUE))
		} else {
			CIs = which 
		}
		model = mxModel(model, mxCI(CIs))
	}
    
	if(run == "yes" | (!umx_has_CIs(model) & run == "if necessary")) {
		model = mxRun(model, intervals = TRUE)
	} else {
		message("Not running CIs, run == ", run)
	}

	if(umx_has_CIs(model)){
		message("### CIs for model ", model$name)
		confint(model, showErrorCodes = showErrorCodes)
	}
	invisible(model)
}

#' umxSummary.default
#'
#' @description
#' Report the fit of a OpenMx model or specialized model class (such as ACE, CP etc.)
#' in a compact form suitable for a journal. Because this same function is needed in umx, 
#' it gets defined twice currently.
#'
#' See documentation for RAM models summary here: \code{\link{umxSummary.MxModel}}.
#' 
#' View documentation on the IP model subclass here: \code{\link{umxSummary.MxModel.IP}}.
#' 
#' View documentation on the CP model subclass here: \code{\link{umxSummary.MxModel.CP}}.
#' 
#' View documentation on the GxE model subclass here: \code{\link{umxSummary.MxModel.GxE}}.
#' 
#' View documentation on the twin model subclass here: \code{\link{umxSummary.MxModel.ACE}}.
#'
#' @param model The \code{\link{mxModel}} whose fit will be reported
#' @param ... Other parameters to control model summary
#' @family Reporting Functions
#' \url{http://www.github.com/tbates/umx}
#' @export
umxSummary <- function(model, ...){
	UseMethod("umxSummary", model)
}

#' @export
umxSummary.default <- function(model, ...){
	stop("umxSummary is not defined for objects of class:", class(model))
}

#' umxSummary.MxModel
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
#' @aliases umxSummary.MxModel
#' @param model The \code{\link{mxModel}} whose fit will be reported
#' @param refModels Saturated models if needed for fit indices (see example below:
#' 	Only needed for raw data. nb: \code{\link{umxRun}} takes care of this for you)
#' @param showEstimates What estimates to show. Options are c("none", "std", "raw", "both", "list of column names"). 
#' Default  is "none" (just shows the fit indices)
#' @param digits How many decimal places to report (default = 2)
#' @param report If "html", then show results in browser ("1", "table", "html")
#' @param filter whether to show significant paths (SIG) or NS paths (NS) or all paths (ALL)
#' @param SE Whether to compute SEs... defaults to TRUE. In rare cases, you might need to turn off to avoid errors.
#' @param RMSEA_CI Whether to compute the CI on RMSEA (Defaults to FALSE)
#' @param matrixAddresses Whether to show "matrix address" columns (Default = FALSE)
#' @param ... Other parameters to control model summary
#' @family Reporting functions
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
#' @return - parameterTable returned invisibly, if estimates requested
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor",
#' 	data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' umxSummary(m1, show = "std")
#' # output as latex
#' umx_set_table_format("latex")
#' umxSummary(m1, show = "std")
#' umx_set_table_format("markdown")
#' # output as raw
#' umxSummary(m1, show = "raw")
#' m1 <- mxModel(m1,
#'   mxData(demoOneFactor[1:100,], type = "raw"),
#'   umxPath(mean = manifests),
#'   umxPath(mean = latents, fixedAt = 0)
#' )
#' m1 <- mxRun(m1)
#' umxSummary(m1, show = "std", filter = "NS")
umxSummary.MxModel <- function(model, refModels = NULL, showEstimates = c("none", "raw", "std", "both", "list of column names"), digits = 2, report = c("1", "table", "html"), filter = c("ALL", "NS", "SIG"), SE = TRUE, RMSEA_CI = FALSE, matrixAddresses = FALSE, ...){
	# TODO make table take lists of models...
	report = match.arg(report)
	filter = match.arg(filter)
	# showEstimates = match.arg(showEstimates)
	validValuesForshowEstimates = c("none", "raw", "std", "both", "list of column names")
	showEstimates = umx_default_option(showEstimates, validValuesForshowEstimates, check = FALSE) # to allow a user specified list
	# if the filter is off default, the user must want something, let's assume it's std ...
	if( filter != "ALL" & showEstimates == "none") {
		showEstimates = "std"
	}else if(showEstimates =="std" && SE == FALSE){
		message("SE must be TRUE to show std, overriding to set SE =TRUE")
		SE = TRUE
	}
	umx_has_been_run(model, stop = TRUE)
	if(is.null(refModels)) {
		# saturatedModels not passed in from outside, so get them from the model
		# TODO improve efficiency here: compute summary only once by detecting when SaturatedLikelihood is missing
		modelSummary = summary(model)		
		if(is.null(model$data)){
			# # TODO model with no data - no saturated solution?
		} else if(is.na(modelSummary$SaturatedLikelihood)){
			# no SaturatedLikelihood, compute refModels
			refModels = mxRefModels(model, run = TRUE)
			modelSummary = summary(model, refModels = refModels)
		}
	} else {
		modelSummary = summary(model, refModels = refModels) # use user-supplied refModels		
	}

	# DisplayColumns
	if(showEstimates != "none"){
		parameterTable = mxStandardizeRAMpaths(model, SE = SE) # compute standard errors
		#                 name    label  matrix   row         col    Raw.Value  Raw.SE   Std.Value    Std.SE
		# 1  no_HRV_Dep.A[6,1]    age    A        mean_sdrr   age   -0.37       0.0284   -0.372350    .028
		# Raw.SE is new
		names(parameterTable) <- c("label", "name", "matrix", "row", "col", "Estimate", "SE", "Std.Estimate", "Std.SE")

		if(matrixAddresses){
			nameing = c("name", "matrix", "row", "col")
		} else {
			nameing = c("name")
		}
		if(length(showEstimates) > 1) {
			namesToShow = showEstimates # user-specified list
		}else if(showEstimates == "both") {
			namesToShow = c(nameing, "Estimate", "SE", "Std.Estimate", "Std.SE")
		} else if(showEstimates == "std"){
			namesToShow = c(nameing, "Std.Estimate", "Std.SE", "CI")
		}else{ # must be raw
			namesToShow = c(nameing, "Estimate", "SE")					
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
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), ", ", round(est + CI95, digits), "]")
					} else {
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), ", ", round(est + CI95, digits), "]")
					}
				}
			}
		}
		if(filter == "NS") {
			toShow = parameterTable[parameterTable$sig == FALSE, namesToShow]
		} else if(filter == "SIG") {
			toShow = parameterTable[parameterTable$sig == TRUE, namesToShow]
		} else {
			toShow = parameterTable[,namesToShow]
		}
		if(report == "html"){
			umx_print(toShow, digits = digits, file = "tmp.html");
		} else {
			umx_print(toShow, digits = digits, na.print = "", zero.print = "0", justify = "none")
		}
	} else {
		message("For estimates, umxSummary(..., showEstimates = 'std', 'raw', or 'both')")
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
				x = paste0(
					"\u03C7\u00B2(", degreesOfFreedom, ") = ", round(Chi, 2), # was A7
					", p "      , umx_APA_pval(p, .001, 3, addComparison = TRUE),
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
	
	if(!is.null(model$output$confidenceIntervals)){
		print(model$output$confidenceIntervals)
	}
	if(showEstimates != "none"){ # return these as  invisible for the user to filer, sort etc.
		if(filter == "NS"){
			invisible(parameterTable[parameterTable$sig == FALSE, namesToShow])
		}else if(filter == "SIG"){
			invisible(parameterTable[parameterTable$sig == TRUE, namesToShow])
		}else{
			invisible(parameterTable[,namesToShow])
		}
	}
}

#' umxSummaryACE
#'
#' Summarise a Cholesky model returned by \\code{\link{umxACE}}.
#'
#' @aliases umxSummary.MxModel.ACE
#' @param model an \code{\link{mxModel}} to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param showStd Whether to standardize the output (defualt = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If 3, then open an html table of the results
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - \code{\link{umxACE}} 
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
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
#' umxSummaryACE(m1, file = NA);
#' umxSummaryACE(m1, file = "name", showStd = TRUE)
#' stdFit = umxSummaryACE(m1, returnStd = TRUE);
#' }
umxSummaryACE <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, showStd = TRUE, showRg = FALSE, CIs = TRUE, report = c("1", "2", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	report = match.arg(report)
	# depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, file = file, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	if(is.null(comparison)){
		message("-2 \u00d7 log(Likelihood)") # \u00d7 = times sign
		print(-2 * logLik(model));			
	} else {
		message("Comparison of model with parent model:")
		umxCompare(comparison, model, digits = 3)
	}
	selDVs = dimnames(model$top.expCovMZ)[[1]]
	# genEpi_TableFitStatistics(model, extended=extended)
	nVar <- length(selDVs)/2;
	# Calculate standardised variance components
	a  <- mxEval(top.a, model); # Path coefficients
	c  <- mxEval(top.c, model);
	e  <- mxEval(top.e, model);

	A  <- mxEval(top.A, model); # Variances
	C  <- mxEval(top.C, model);
	E  <- mxEval(top.E, model);
	Vtot = A + C + E;         # Total variance
	I  <- diag(nVar);         # nVar Identity matrix
	SD <- solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations
	# (same as "(\sqrt(I.Vtot))~"

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
	rowNames = sub("_.1$", "", selDVs[1:nVar])
	Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);

	names(Estimates) = paste0(rep(c("a", "c", "e"), each = nVar), rep(1:nVar));

	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		# depends on R2HTML::HTML
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = F, sortableDF = T); 
		umx_open("tmp.html")
	}
	
	if(extended == TRUE) {
		message("Unstandardized path coefficients")
		aClean = a
		cClean = c
		eClean = e
		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);
		names(unStandardizedEstimates) = paste0(rep(c("a", "c", "e"), each = nVar), rep(1:nVar));
		umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print)
	}

	# Pre & post multiply covariance matrix by inverse of standard deviations
	if(showRg) {
		message("Genetic correlations")
		NAmatrix <- matrix(NA, nVar, nVar);
		rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # C correlations
		rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # E correlations
		rAClean = rA
		rCClean = rC
		rEClean = rE
		rAClean[upper.tri(rAClean)] = NA
		rCClean[upper.tri(rCClean)] = NA
		rEClean[upper.tri(rEClean)] = NA
		genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames);
		names(genetic_correlations) <- rowNames
	 	# Make a nice-ish table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each=nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits=digits, zero.print = zero.print)
	}
	stdFit = model
	hasCIs = umx_has_CIs(model)
	if(hasCIs & CIs) {
		# TODO Need to refactor this into some function calls...
		# TODO and then add to umxSummaryIP and CP
		message("Creating CI-based report!")
		# CIs exist, get the lower and uppper CIs as a dataframe
		CIlist = data.frame(model$output$confidenceIntervals)
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
		# discard rows named NA
		CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]

		# These can be names ("top.a_std[1,1]") or labels ("a11")
		# imxEvalByName finds them both
		# outList = c();
		# for(aName in row.names(CIlist)) {
		# 	outList <- append(outList, imxEvalByName(aName, model))
		# }
		# # Add estimates into the CIlist
		# CIlist$estimate = outList
		# reorder to match summary
		CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
		CIlist$fullName = row.names(CIlist)
		# Initialise empty matrices for the standardized results
		rows = dim(model$submodels$top$matrices$a$labels)[1]
		cols = dim(model$submodels$top$matrices$a$labels)[2]
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
			thisString = paste(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]", sep="")
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
		# message("making dot file")
		umxPlotACE(stdFit, file = file, std = showStd)
	}
	if(returnStd) {
		if(CIs){
			message("Returned model won't work if you asked for CIs...")
		}
		return(stdFit)
	}
}

#' @export
umxSummary.MxModel.ACE <- umxSummaryACE

#' umxSummaryACEcov
#'
#' Summarise a Cholesky model as returned by umxACEcov
#'
#' @aliases umxSummary.MxModel.ACEcov
#' @param model a \code{\link{umxACEcov}} model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = F)
#' @param extended how much to report (F)
#' @param showRg = whether to show the genetic correlations (F)
#' @param showStd = whether to show the standardized model (T)
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param zero.print How to show zeros (".")
#' @param report If 3, then open an html table of the results
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxACEcov}} 
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
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
#' umxSummaryACE(m1, file = NA);
#' umxSummaryACE(m1, file = "name", showStd = TRUE)
#' stdFit = umxSummaryACE(m1, returnStd = TRUE);
#' }
umxSummaryACEcov <- function(model, digits = 2, file = getOption("umx_auto_plot"), returnStd = FALSE, extended = FALSE, showRg = FALSE, showStd = TRUE, comparison = NULL, CIs = TRUE, zero.print = ".", report = c("1", "2", "html"), ...) {
	report = match.arg(report)
	# depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACEcov(thisFit, digits = digits, file = file, returnStd = returnStd, extended = extended, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	if(is.null(comparison)){
		message("-2 \u00d7 log(Likelihood)") # \u00d7 = times sign
		print(-2 * logLik(model));			
	} else {
		message("Comparison of model with parent model:")
		umxCompare(comparison, model, digits = 3)
	}
	selDVs = dimnames(model$top$a)[[1]]
	# selDVs = dimnames(model$top.expCovMZ)[[1]]
	nDV <- length(selDVs);
	# Calculate standardised variance components
	a  <- mxEval(top.a, model); # Path coefficients
	c  <- mxEval(top.c, model);
	e  <- mxEval(top.e, model);

	A  <- mxEval(top.A, model); # Variances
	C  <- mxEval(top.C, model);
	E  <- mxEval(top.E, model);
	Vtot = A + C + E; # Total variance
	Iden  <- diag(nDV);  # nDV Identity matrix
	SD <- solve(sqrt(Iden * Vtot)) # Inverse of diagonal matrix of standard deviations
	# (same as "(\sqrt(Iden.Vtot))~"

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
	rowNames = sub("_.1$", "", selDVs[1:nDV])
	Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);

	names(Estimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));

	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		# depends on R2HTML::HTML
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = F, sortableDF = T); 
		umx_open("tmp.html")
	}

	if(extended == TRUE) {
		message("Unstandardized path coefficients")
		aClean = a
		cClean = c
		eClean = e
		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);
		names(unStandardizedEstimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));
		umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print)
	}

	# Pre & post multiply covariance matrix by inverse of standard deviations
	if(showRg) {
		message("Genetic correlations")
		NAmatrix <- matrix(NA, nDV, nDV);
		rA = tryCatch(solve(sqrt(Iden * A)) %*% A %*% solve(sqrt(Iden * A)), error = function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(Iden * C)) %*% C %*% solve(sqrt(Iden * C)), error = function(err) return(NAmatrix)); # C correlations
		rE = tryCatch(solve(sqrt(Iden * E)) %*% E %*% solve(sqrt(Iden * E)), error = function(err) return(NAmatrix)); # E correlations
		rAClean = rA
		rCClean = rC
		rEClean = rE
		rAClean[upper.tri(rAClean)] = NA
		rCClean[upper.tri(rCClean)] = NA
		rEClean[upper.tri(rEClean)] = NA
		genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames);
		names(genetic_correlations) <- rowNames
	 	# Make a nice-ish table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each=nDV), rep(1:nDV));
		umx_print(genetic_correlations, digits=digits, zero.print = zero.print)
	}
	stdFit = model
	hasCIs = umx_has_CIs(model)
	if(hasCIs & CIs) {
		# TODO Need to refactor this into some function calls...
		# TODO and then add to umxSummaryIP and CP
		message("Creating CI-based report!")
		# CIs exist, get the lower and uppper CIs as a dataframe
		CIlist = data.frame(model$output$confidenceIntervals)
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
		# discard rows named NA
		CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]

		# # Add estimates into the CIlist
		# CIlist$estimate = outList
		# reorder to match summary
		CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
		CIlist$fullName = row.names(CIlist)
		# Initialise empty matrices for the standardized results
		rows = dim(model$submodels$top$matrices$a$labels)[1]
		cols = dim(model$submodels$top$matrices$a$labels)[2]
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
			thisString = paste(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]", sep="")
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
		plot(model, file, std = showStd)
	}
	if(returnStd) {
		if(CIs){
			message("Returned model won't work if you asked for CIs...")
		}
		return(stdFit)
	}
}
#' @export
umxSummary.MxModel.ACEcov <- umxSummaryACEcov


#' umxSummaryCP
#'
#' Summarise a Commmon Pathway model, as returned by \code{\link{umxCP}}
#'
#' @aliases umxSummary.MxModel.CP
#' @param model A fitted \code{\link{umxCP}} model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param extended how much to report (FALSE)
#' @param showRg Whether to show the genetic correlations (FALSE) (Not implemented!)
#' @param showStd Whether to show the standardized model (TRUE) (Not implemented!)
#' @param comparison Whether to run mxCompare on a comparison model (NULL)
#' @param CIs Confidence intervals (F)
#' @param ... Optional additional parameters
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxCP}()}, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://www.github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' require(umx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' selDVs = c("ht", "wt") # will be expanded into "ht1", "wt1", "ht2", "wt2"
#' mzData <- subset(twinData, ZYG == "MZFF")
#' dzData <- subset(twinData, ZYG == "DZFF")
#' m1 = umxCP(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = "")
#' m1 = mxRun(m1)
#' umxSummaryCP(m1, file = NA) # suppress plot creation with file
#' umxSummary(m1, file = NA) # generic summary is the same
#' stdFit = umxSummaryCP(m1, digits = 2, file = NA, returnStd = FALSE, 
#' 		extended = FALSE, showRg = TRUE, showStd = TRUE, CIs = TRUE);
#' \dontrun{
#' examples which will create graphical output
#' umxSummaryCP(fit);
#' umxSummaryCP(fit, file = "Figure 3", showStd = TRUE)
#' }
umxSummaryCP <- function(model, digits = 2, file = getOption("umx_auto_plot"), returnStd = FALSE, 
    extended = FALSE, showRg = TRUE, comparison = NULL, showStd = TRUE, CIs = FALSE, ...) {
	# TODO: detect value of DZ covariance, and if .25 set "C" to "D"
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message(paste("Output for Model: ", thisFit$name))
			umxSummaryCP(thisFit, digits = digits, file = file, returnStd = returnStd, extended = extended, showRg = showRg, comparison = comparison, CIs = CIs)
		}
	} else {
		if(class(model)[1] != "MxModel.CP"){
			stop("You used umxSummaryCP on model of class ", class(model)[1], "not 'MxModel.CP'")
		}
		umx_has_been_run(model, stop = TRUE)
		if(is.null(comparison)){
			message("-2 \u00d7 log(Likelihood)") # x
			print(-2 * logLik(model))
		}else{
			message("Comparison of model with parent model:")
			print(mxCompare(comparison, model))
		}
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar   = length(selDVs)/2;
		nFac   = dim(model$submodels$top$matrices$a_cp)[[1]]	
		# MZc = mxEval(MZ.expCov,  model); # Same effect as expCovMZ$matrices$twinACEFit
		# DZc = mxEval(DZ.expCov,  model);
		# M   = mxEval(MZ.expMean, model);
		# Calculate standardised variance components
		a_cp  = mxEval(top.a_cp, model); # nFac * nFac matrix of path coefficients flowing into the cp_loadings array
		c_cp  = mxEval(top.c_cp, model);
		e_cp  = mxEval(top.e_cp, model);
		as = mxEval(top.as, model); # Specific factor path coefficients
		cs = mxEval(top.cs, model);
		es = mxEval(top.es, model);
		cp_loadings = mxEval(top.cp_loadings, model); # nVar * nFac matrix
		A  = mxEval(top.A, model);  # Variances
		C  = mxEval(top.C, model);
		E  = mxEval(top.E, model);
		Vtot = A + C + E; # Total variance
		nVarIden = diag(nVar)
		SD       = solve(sqrt(nVarIden*Vtot)); # inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
		# print(SD)  # nVar*nVar diagonal matrix

		# Common factor ACE inputs are already std to 1: Just print out
		commonACE = cbind(diag(a_cp), diag(c_cp), diag(e_cp)) # bind columns of a, c and e into nFac-rows * 3 matrix
		commonACE = data.frame(commonACE, row.names = paste("Common.factor", 1:nFac, sep = "."));
		names(commonACE) = c ("A", "C", "E")
		message("Common Factor paths")
		umx_print(commonACE, digits = digits, zero.print = ".")
		if(class(model$submodels$top$matrices$a_cp)[1] =="LowerMatrix"){
			message("You used correlated genetic inputs to the common factor. This is the a_cp matrix")
			print(a_cp)
		}
		stdFit = umx_standardize_CP(model) # Make a copy of model to hold the standardized results
		# Standardize loadings on Common factors
		std_cp_loadings = mxEval(top.cp_loadings, stdFit); # Standardized path coefficients (general factor(s))		
		rowNames = sub("_.1$", "", selDVs[1:nVar])
		std_CommonEstimate = data.frame(std_cp_loadings, row.names = rowNames);
		names(std_CommonEstimate) = paste0("CP", 1:length(names(std_CommonEstimate)))
		message("Loading of each trait on the Common Factors")
		umx_print(std_CommonEstimate, digits = digits, zero.print = ".")

		# Standard specific path coefficients ready to be stacked together
		asClean <- mxEval(top.as, stdFit);
		csClean <- mxEval(top.cs, stdFit);
		esClean <- mxEval(top.es, stdFit);
		# TODO not needed now?
		asClean[upper.tri(asClean)] = NA
		csClean[upper.tri(csClean)] = NA
		esClean[upper.tri(esClean)] = NA
		standardized_specifics = data.frame(cbind(asClean, csClean, esClean), row.names=rowNames);
		names(standardized_specifics) = paste0(rep(c("As", "Cs", "Es"), each = nVar), rep(1:nVar));
		message("Standardized Specific Loadings")
		umx_print(standardized_specifics, digits = digits, zero.print = ".")
		if(extended == TRUE) {
			cat("\nUnstandardized path coefficients\n") # factor loadings
			print(round(commonACE, digits)); # Loadings on Common factor
			print(round(data.frame(common_loadings, row.names = rowNames), digits));
			# specifics
			asClean = as
			csClean = cs
			esClean = es
			asClean[upper.tri(asClean)] = NA
			csClean[upper.tri(csClean)] = NA
			esClean[upper.tri(esClean)] = NA
			unStandardized_specifics = data.frame(cbind(asClean, csClean, esClean), row.names = rowNames);
			names(unStandardized_specifics) = paste(rep(c("as", "cs", "es"), each = nVar), rep(1:nVar), sep = "");
			umx_print(unStandardized_specifics, digits = digits, zero.print = ".")
		}
		if(showRg) {
			message("Genetic Correlations")
			# Pre & post multiply covariance matrix by inverse of standard deviations
			NAmatrix <- matrix(NA,nVar,nVar);
			rA = tryCatch(solve(sqrt(nVarIden*A)) %*% A %*% solve(sqrt(nVarIden*A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(nVarIden*C)) %*% C %*% solve(sqrt(nVarIden*C)), error = function(err) return(NAmatrix)); # shared environmental correlations
			rE = tryCatch(solve(sqrt(nVarIden*E)) %*% E %*% solve(sqrt(nVarIden*E)), error = function(err) return(NAmatrix)); # Unique environmental correlations
			genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
			# Make a table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			umx_print(genetic_correlations, digits = digits, zero.print = ".")
		}
		if(CIs){
			message("showing CIs in output not implemented yet: use summary(model) to view them in the mean time")
		}
		if(!is.na(file)){
			umxPlotCP(model = stdFit, file = file, digits = digits, showMeans = FALSE, std = FALSE)
		}
		if(returnStd) {
			return(stdFit)
		}
	}
}

#' @export
umxSummary.MxModel.CP <- umxSummaryCP

#' umxSummaryIP
#'
#' Summarise a Independent Pathway model, as returned by \code{\link{umxIP}}
#'
#' @aliases umxSummary.MxModel.IP
#' @param model A fitted \code{\link{umxIP}} model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = F)
#' @param showRg = whether to show the genetic correlations (F)
#' @param showStd = Whether to show the standardized model (TRUE)
#' @param comparison Whether to run mxCompare on a comparison model (NULL)
#' @param CIs Confidence intervals (F)
#' @param ... Optional additional parameters
#' @return - optional \code{\link{mxModel}}
#' @family Twin Modeling Functions
#' @export
#' @seealso - \code{\link{umxIP}()}, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' require(umx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' selDVs = c("ht1", "wt1", "ht2", "wt2")
#' mzData <- subset(twinData, ZYG == "MZFF")
#' dzData <- subset(twinData, ZYG == "DZFF")
#' m1 = umxIP(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' umxSummaryIP(m1, file = NA) # suppress graphing with NA for CRAN
#' \dontrun{
#' umxSummaryIP(m1, digits = 2, file = "Figure3", showRg = FALSE, CIs = TRUE);
#' }
umxSummaryIP <- function(model, digits = 2, file = getOption("umx_auto_plot"), 
    returnStd = FALSE, showStd = FALSE, showRg = TRUE, comparison = NULL, CIs = FALSE, ...) {
	if(class(model)[1] != "MxModel.IP"){
		stop("You used umxSummaryIP on model of class ", class(model)[1], "not 'MxModel.IP'")
	}

	umx_has_been_run(model, stop = TRUE)
	selDVs = dimnames(model$top.expCovMZ)[[1]]
	notImplemented = c(showStd)
	if(any(notImplemented)){
		message("Some parameters not implemented yet. Don't use showStd")
	}

	if(is.null(comparison)){
		message("-2 \u00d7 log(Likelihood)") # \u00d7 = times sign
		print(-2 * logLik(model));			
	}else{
		message("Comparison of model with parent model:")
		print(mxCompare(comparison, model))
	}

	stdFit = model; # If we want to output a model with the standardised values (perhaps for drawing a path diagram)
	nVar   = length(selDVs)/2;
	nFac   = 1;
	# Calculate standardised variance components
	ai = mxEval(top.ai, model); # Column of independent path coefficients (nVar* nFac = 1) 
	ci = mxEval(top.ci, model);
	ei = mxEval(top.ei, model);

	as = mxEval(top.as, model); # nVar*nVar matrix of specific path coefficients (Just diagonal, or possibly Choleksy lower for E)
	cs = mxEval(top.cs, model);
	es = mxEval(top.es, model);

	A  = mxEval(top.A , model); # Totaled Variance components (ai + as etc.)
	C  = mxEval(top.C , model);
	E  = mxEval(top.E , model);

	Vtot     = A+C+E; # total variance
	nVarIden = diag(nVar); # Make up a little nvar Identity matrix using the clever behavior of diag to make an nVar*nVar Identity matrix
	SD       = solve(sqrt(nVarIden*Vtot))   # inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	ai_std   = SD %*% ai ; # Standardized path coefficients (independent general factors )
	ci_std   = SD %*% ci ; # Standardized path coefficients (independent general factors )
	ei_std   = SD %*% ei ; # Standardized path coefficients (independent general factors )

	stdFit@submodels$top$ai@values = ai_std
	stdFit@submodels$top$ci@values = ci_std
	stdFit@submodels$top$ei@values = ei_std

	rowNames = sub("_.1$", "", selDVs[1:nVar])

	std_Estimates = data.frame(cbind(ai_std,ci_std,ei_std), row.names=rowNames);
	message("General IP path loadings")
	names(std_Estimates) = paste0(rep(c("Ai", "Ci", "Ei"), each = nFac));
	umx_print(std_Estimates, digits = digits, zero.print = ".")

	# Standard specific path coefficients ready to be stacked together
	as_std = SD %*% as; # Standardized path coefficients (nVar specific factors matrices)
	cs_std = SD %*% cs;
	es_std = SD %*% es;
	stdFit@submodels$top$as@values = as_std
	stdFit@submodels$top$cs@values = cs_std
	stdFit@submodels$top$es@values = es_std

	asClean = as_std
	csClean = cs_std
	esClean = es_std
	# need to add a check here that the cells are fixed and 0
	asClean[upper.tri(asClean)]=NA
	csClean[upper.tri(csClean)]=NA
	esClean[upper.tri(esClean)]=NA
	std_Specifics = data.frame(cbind(asClean,csClean,esClean), row.names = rowNames);
	names(std_Specifics) = paste0(rep(c("As", "Cs", "Es"), each = nVar), rep(1:nVar));
	message("Specific factor loadings")
	umx_print(round(std_Specifics, digits), digits = digits, zero.print = ".")

	if(showRg) {
		# Pre & post multiply covariance matrix by inverse of standard deviations
		NAmatrix <- matrix(NA, nVar, nVar);  
		rA = tryCatch(solve(sqrt(nVarIden*A)) %*% A %*% solve(sqrt(nVarIden*A)), error=function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(nVarIden*C)) %*% C %*% solve(sqrt(nVarIden*C)), error=function(err) return(NAmatrix)); # shared environmental correlations
		rE = tryCatch(solve(sqrt(nVarIden*E)) %*% E %*% solve(sqrt(nVarIden*E)), error=function(err) return(NAmatrix)); # Unique environmental correlations
		genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
		# Make a table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits = digits, zero.print = ".")
	}
	if(CIs){
		message("showing CIs in output not implemented yet: use summary(model) to view them in the mean time")
	}
	if(!is.na(file)){
		umxPlotIP(model = stdFit, file = file, digits = 2, std = FALSE)
	}
	if(returnStd) {
		return(stdFit)
	}
}

#' @export
umxSummary.MxModel.IP <- umxSummaryIP

#' umxSummaryGxE
#'
#' Summarise a Moderation model, as returned by \code{\link{umxGxE}}
#'
#' @aliases umxSummary.MxModel.GxE
#' @param model A fitted \code{\link{umxGxE}} model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param showStd Whether to show the standardized model (not implemented! TRUE)
#' @param CIs Confidence intervals (FALSE)
#' @param xlab label for the x-axis of plot
#' @param location default = "topleft"
#' @param reduce  Whether run and tabulate a complete model reduction...(Defaults to FALSE)
#' @param separateGraphs default = F
#' @param report "1" = regular, "2" = add descriptive sentences; "html" = open a browser and copyable tables
#' @param ... Optional additional parameters
#' @return - optional \code{\link{mxModel}}
#' @family Twin Modeling Functions
#' @export
#' @seealso - \code{\link{umxGxE}()}, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, and ACE models.
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' # The total sample has been subdivided into a young cohort, 
#' # aged 18-30 years, and an older cohort aged 31 and above.
#' # Cohort 1 Zygosity is coded as follows 1 == MZ females 2 == MZ males 
#' # 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
#  # use ?twinData to learn about this data set.
#' require(umx)
#' data(twinData) 
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, ZYG == "MZFF", selVars)
#' dzData  = subset(twinData, ZYG == "DZMM", selVars)
#' # Exclude cases with missing Def
#' mzData <- mzData[!is.na(mzData[selDefs[1]]) & !is.na(mzData[selDefs[2]]),]
#' dzData <- dzData[!is.na(dzData[selDefs[1]]) & !is.na(dzData[selDefs[2]]),]
#' m1 = umxGxE(selDVs = selDVs, selDefs = selDefs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' # Plot Moderation
#' umxSummaryGxE(m1)
#' umxSummaryGxE(m1, location = "topright")
#' umxSummaryGxE(m1, separateGraphs = FALSE)
umxSummaryGxE <- function(model = NULL, digits = 2, xlab = NA, location = "topleft", separateGraphs = FALSE, file = getOption("umx_auto_plot"), returnStd = NULL, showStd = NULL, reduce = FALSE, CIs = NULL, report = c("1", "2", "html"), ...) {
	report = match.arg(report)
	umx_has_been_run(model, stop=TRUE)
	
	if(any(!is.null(c(file, returnStd, showStd, CIs, ...) ))){
		message("I haven't implemented file, returnStd, extended, showStd, comparison or CIs yet...")
	}

	if(is.null(model)){
		message("umxSummaryGxE calls plot.MxModel.GxE for a twin moderation plot. A use example is:\n umxSummaryGxE(model, location = \"topright\")")
		stop();
	}
	umxPlotGxE(model, xlab = xlab, location = location, separateGraphs = separateGraphs)

	if(reduce){
		modelnomeans  = umxModify(model , update="lin|quad", regex= TRUE, name = "no_moderation_of_means")

		noACEmod     = umxModify(model, update = "[ace]m"       , regex = TRUE, name = "no_moderation")
		noA          = umxModify(model, update = "a_r1c1"       , regex = TRUE, name = "dropA")
		noC          = umxModify(model, update = "c_r1c1"       , regex = TRUE, name = "dropC")
		noE          = umxModify(model, update = "e_r1c1"       , regex = TRUE, name = "dropE")
		noAmod       = umxModify(model, update = "^am"          , regex = TRUE, name = "no_mod_on_A")
		noCmod       = umxModify(model, update = "^cm"          , regex = TRUE, name = "no_mod_on_C")
		noEmod       = umxModify(model, update = "^em"          , regex = TRUE, name = "no_mod_on_E")
		noA_noAmod   = umxModify(model, update = "^(a|am)_r1c1" , regex = TRUE, name = "no_A_no_mod_on_A")
		noC_noCmod   = umxModify(model, update = "^(c|cm)_r1c1" , regex = TRUE, name = "no_C_no_mod_on_C")
		noC_noCEmod  = umxModify(model, update = "^(c|[ce]m)_r" , regex = TRUE, name = "no_C_no_mod_on_C_or_E")
		noC_noACEmod = umxModify(model, update = "^c|([ace]m)_r", regex = TRUE, name = "no_C_no_mod_on_A_C_or_E")

		comparisons = c(
			noACEmod,
			noA, noC,
			noAmod     , noCmod, noEmod,
			noA_noAmod , noC_noCmod,
			noC_noCEmod, noC_noACEmod
		)

		mxCompare(model, modelnomeans)
		umx_drop_ok(model, modelnomeans, text = NULL)

		format(mxCompare(c(model, noC_noCEmod), comparisons, all = T), scientific = FALSE, digits = 5)
		umxCompare(c(model, noC_noCEmod), comparisons, all = TRUE, report = report)
		umxCompare(c(noC_noCmod), noC_noCEmod, all = TRUE, report = report)
	}
}

#' @export
umxSummary.MxModel.GxE <- umxSummaryGxE


#' umxCompare
#'
#' umxCompare compares two or more \code{\link{mxModel}}s. It has several nice features:
#' 1. It supports direct control of rounding, and reports p-values rounded to APA style.
#' 2. It reports the table in your preferred markdown format (relies on knitr)
#' 3. The table columns are arranged in a method suitable for easy comparison for readers.
#' 4. By default, it also reports the output as an ENglish sentence suitable for a paper.
#' 5. It can open tabular output in a browser (report = "html")
#' 
#' note: If you leave comparison blank, it will just give fit info for the base model
#'
#' @param base The base \code{\link{mxModel}} for comparison
#' @param comparison The model (or list of models) which will be compared for fit with the base model (can be empty)
#' @param all Whether to make all possible comparisons if there is more than one base model (defaults to T)
#' @param digits rounding for p etc.
#' @param report Optionally (report = "2") add sentences for inclusion in a paper, or ("html")
#' create a web table and open your default browser.
#' (handy for getting tables into Word, and other text systems!)
#' @param file file to write html too if report=3 (defaults to "tmp.html")
#' @family Reporting functions
#' @seealso - \code{\link{mxCompare}}, \code{\link{umxSummary}}, \code{\link{umxRAM}},
#' @references - \url{http://www.github.com/tbates/umx/}
#' @export
#' @examples
#' require(umx)
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
#' m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' mxCompare(m1, m2) # what OpenMx gives by default
#' umxCompare(m1, m2, report = "2") # Add English-sentence descriptions
#' \dontrun{
#' umxCompare(m1, m2, report = "html") # Open table in browser
#' }
#' m3 = umxModify(m2, update = "G_to_x3", name = "drop_path_2_x2_and_3")
#' umxCompare(m1, c(m2, m3))
#' umxCompare(c(m1, m2), c(m2, m3), all = TRUE)
umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, digits = 3, report = c("2", "1", "html"), file = "tmp.html") {
	report = match.arg(report)
	if(is.null(comparison)){
		comparison <- base
	} else if (is.null(base)) {
		stop("You must provide at least a base model for umxCompare")
	}
	if(length(base) == 1) {
		if(typeof(base) == "list"){
			base = base[[1]]
		}
		if(!umx_has_been_run(base)){
			warning("base model not run yet!")		
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

	# | 1       |    2          | 3  | 4        | 5   | 6        | 7        | 8      | 9    |
	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf | p    |
	# | twinSat | <NA>          | 13 | 333.0781 | 149 | 35.07809 | NA       | NA     | NA   |
	# | twinSat | betaSetToZero | 10 | 351.6486 | 152 | 47.64858 | 18.57049 | 3      | 0.01 |

	tablePub = tableOut[, c("comparison", "ep", "diffLL"      , "diffdf"    , "p", "AIC", "base")]
	# names(tablePub)     <- c("Model"     , "EP", "&Delta; -2LL", "&Delta; df", "p", "AIC", "Compare with Model")
	names(tablePub)     <- c("Model"     , "EP", "\u2206 -2LL", "\u2206 df", "p", "AIC", "Compare with Model")
	# U+2206 = math delta
	# Fix problem where base model has compare set to its own name, and name set to NA
	nRows = dim(tablePub)[1]
	for (i in 1:nRows) {
		if(is.na(tablePub[i, "Model"])){
			tablePub[i, "Model"] = tablePub[i, "Compare with Model"] 
			tablePub[i, "Compare with Model"] = NA
		}
	}
	tablePub[,"p"] = umx_APA_pval(tablePub[, "p"], min = (1/ 10^digits), digits = digits, addComparison = NA)
	# c("1: Comparison", "2: Base", "3: EP", "4: AIC", "5: &Delta; -2LL", "6: &Delta; df", "7: p")
	if(report != "1"){
		n_rows = dim(tablePub)[1]
		for (i in 1:n_rows) {
			thisPValue = tableOut[i, 9]
			if(!is.na(thisPValue) && !is.nan(thisPValue)){
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
				"(\u03C7\u00B2(", tablePub[i, 4], ") = ", round(tablePub[i, 3], 2), # \u03A7 = Chi \u00B2 = superscript 2
				", p = ", tablePub[i,"p"], ")."
				)
			}
		}
	}
	
	if(report == "html"){
		R2HTML::HTML(tablePub, file = file, Border = 0, append = FALSE, sortableDF = TRUE);
		umx_open(file)
	} else {
		umx_print(tablePub)
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
#' 	require(umx)
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
#' @family Reporting functions
umxCI_boot <- function(model, rawData = NULL, type = c("par.expected", "par.observed", "empirical"), std = TRUE, rep = 1000, conf = 95, dat = FALSE, digits = 3) {
	# depemds on MASS::mvrnorm
	type = umx_default_option(type, c("par.expected", "par.observed", "empirical"))
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
	#####
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


# ============
# = Graphics =
# ============

#' Create a graphical figure from an MxModel
#'
#' Create graphical path diagrams from your OpenMx models!
#' 
#' plot() produces SEM diagrams in graphviz format, and relies on DiagrammeR (or a 
#' graphviz application) to create the image. 
#' The commercial application Omnigraffle is great for editing these images.
#' 
#'
#' On unix and windows, plot() will create a pdf and open it in your default pdf reader.
#' 
#' If you use umx_set_plot_format("graphviz"), files will open in with a graphviz helper.
#' 
#' \emph{Note:} DiagrammeR is supported out of the box. If you use graphviz, we try and use that app, but YOU HAVE TO INSTALL IT!
#' On OS X we try and open an app: you may need to associate the \sQuote{.gv}
#' extension with the graphviz app.
#' Find the .gv file made by plot, get info (cmd-I), then choose \dQuote{open with}, 
#' select Graphviz.app (or OmniGraffle professional),
#' then set \dQuote{change all}.
#'
#' @aliases umxPlot
#' @rdname plot.MxModel
#' @param x An \code{\link{mxModel}} from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE).
#' @param digits The number of decimal places to add to the path coefficients
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param pathLabels Whether to show labels on the paths. both will show both the parameter and the label. ("both", "none" or "labels")
#' @param showFixed Whether to show fixed paths (defaults to TRUE)
#' @param showMeans Whether to show means
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param ... Optional parameters
#' @export
#' @seealso - \code{\link{umx_set_plot_format}}
#' @family Reporting functions
#' @references - \url{http://www.github.com/tbates/umx}, \url{https://en.wikipedia.org/wiki/DOT_(graph_description_language)}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' plot(m1)
plot.MxModel <- function(x = NA, std = FALSE, digits = 2, file = "name", pathLabels = c("none", "labels", "both"), showFixed = TRUE, showMeans = TRUE, resid = c("circle", "line", "none"), ...) {
	# ==========
	# = Setup  =
	# ==========
	resid = match.arg(resid)
	model = x # just to be clear that x is a model

	pathLabels = match.arg(pathLabels)
	latents = model@latentVars   # 'vis', 'math', and 'text' 
	selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
	if(std){ model = umx_standardize_RAM(model, return = "model") }

	# ========================
	# = Get Symmetric & Asymmetric Paths =
	# ========================
	out = "";
	out = xmu_dot_make_paths(model$matrices$A, stringIn = out, heads = 1, showFixed = showFixed, pathLabels = pathLabels, comment = "Single arrow paths", digits = digits)
	if(resid == "circle"){
		out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE, showFixed = showFixed, pathLabels = pathLabels, comment = "Covariances", digits = digits)
	} else if(resid == "line"){
		out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = TRUE , showFixed = showFixed, pathLabels = pathLabels, comment = "Covariances & residuals", digits = digits)
	}else{
		out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE , showFixed = showFixed, pathLabels = pathLabels, comment = "Covariances & residuals", digits = digits)		
	}
	# TODO should xmu_dot_make_residuals handle showFixed or not necessary?
	tmp = xmu_dot_make_residuals(model$matrices$S, latents = latents, digits = digits, resid = resid)
	variances     = tmp$variances  #either "var_var textbox" or "var -> var port circles"
	varianceNames = tmp$varianceNames # names of residuals/variances. EMPTY if using circles 
	# ============================
	# = Make the manifest shapes =
	# ============================
	preOut = "\t# Latents\n"
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
	if(umx_has_means(model) & showMeans){
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

			# TODO find a way of showing means fixed at zero?
			if(thisPathFree || showFixed ) {
				# if(thisPathFree | (showFixed & thisPathVal != 0) ) {
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
	rankVariables = paste0("\t{rank=min ; ", paste(latents, collapse = "; "), "};\n")
	rankVariables = paste0(rankVariables, "\t{rank=same; ", paste(selDVs, collapse = " "), "};\n")
	
	if(umx_has_means(model)){ append(varianceNames, "one")}

	if(length(varianceNames) > 0){
		rankVariables = paste0(rankVariables, "\t{rank=max ; ", paste(varianceNames, collapse = " "), "};\n")
	}

	# ===================================
	# = Assemble full text to write out =
	# ===================================
	digraph = paste("digraph G {\n", preOut, out, rankVariables, "\n}", sep = "\n");

	print("nb: see ?plot.MxModel for options - std, digits, file, showFixed, showMeans, resid= 'circle|line|none', pathLabels")
	xmu_dot_maker(model, file, digraph)
} # end plot.MxModel

#' umxPlotACE
#'
#' Make a graphical display of an ACE model
#'
#' @aliases plot plot.MxModel.ACE
#' @param x \code{\link{mxModel}} to plot (created by umxACE in order to inherit the MxModel.ACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param showMeans Whether to show means paths (default is FALSE)
#' @param std Whether to standardize the model (default is TRUE)
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Reporting functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' selDVs = c("bmi1", "bmi2")
#' mzData <- subset(twinData, ZYG == "MZFF", selDVs)
#' dzData <- subset(twinData, ZYG == "DZFF", selDVs)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' plot(m1)
#' plot(m1, std = FALSE) # don't standardize
umxPlotACE <- function(x = NA, file = "name", digits = 2, showMeans = FALSE, std = TRUE, ...) {
	model = x # just to be clear that x is a model
	if(std){
		model = umx_standardize_ACE(model)
	}
	out = "";
	latents  = c();
	if(model$submodels$MZ$data$type == "raw"){
		selDVs = names(model$submodels$MZ$data$observed)
	}else{
		selDVs = dimnames(model$submodels$MZ$data$observed)[[1]]
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
	rankA   = paste("\t{rank = min; ", paste(grep('a'   , latents, value=T), collapse="; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[ce]', latents, value=T), collapse="; "), "};\n") # {rank=min; c1; e1}
	digraph = paste("digraph G {\n\tsplines = \"FALSE\";\n", preOut, out, rankVariables, rankA, rankCE, "\n}", sep="");
	xmu_dot_maker(model, file, digraph)
} # end umxPlotACE

#' @export
plot.MxModel.ACE <- umxPlotACE

#' umxPlotACEcov
#'
#' Make a graphical display of an ACE model
#'
#' @aliases plot.MxModel.ACEcov
#' @param x \code{\link{mxModel}} to plot (created by umxACE in order to inherit the MxModel.ACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param showMeans Whether to show means paths (default is FALSE)
#' @param std Whether to standardize the model (default is TRUE)
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Reporting functions
#' @references - \url{http://tbates.github.io}
#' @examples
#' require(umx)
#' # BMI ?twinData from Australian twins. 
#' # Cohort 1 Zygosity 1 == MZ females 3 == DZ females
#' data(twinData)
#' # Pick the variables. We will use base names (i.e., "bmi") and set suffix.
#' selDVs  = c("bmi")
#' selCovs = c("age")
#' selVars = umx_paste_names(c(selDVs, selCovs), textConstant = "", suffixes= 1:2)
#' # just top few pairs so example runs quickly
#' mzData = subset(twinData, zyg == 1, selVars)[1:100, ]
#' dzData = subset(twinData, zyg == 3, selVars)[1:100, ]
#' # TODO update for new dataset variable zygosity
#' # mzData = subset(twinData, zygosity == "MZFF", selVars)[1:200, ]
#' # dzData = subset(twinData, zygosity == "DZFF", selVars)[1:200, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, 
#' 	 suffix = "", autoRun = TRUE)
#' plot(m1)
#' plot(m1, std = FALSE) # don't standardize
umxPlotACEcov <- function(x = NA, file = "name", digits = 2, showMeans = FALSE, std = TRUE, ...) {
	model = x # just to be clear that x is a model
	# relies on 'a' not having its dimnames stripped off...
	if(model$MZ$data$type == "raw"){
		selDVs = dimnames(model$top$a)[[1]]
		# selDVs = names(model$MZ$data$observed)
	}else{
		stop("ACEcov has to have raw data...")
		# selDVs = dimnames(model$MZ$data$observed)[[1]]
	}
	if(std){
		model = umx_standardize_ACEcov(model)
	}
	out = "";
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
			if(showMeans){
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
	digraph = paste("digraph G {\n\tsplines = \"FALSE\";\n", preOut, out, rankVariables, rankA, rankCE, "\n}", sep="");
	xmu_dot_maker(model, file, digraph)
} # end umxPlotACEcov

#' @export
plot.MxModel.ACEcov <- umxPlotACEcov

#' umxPlotGxE
#'
#' Plot GxE results (univariate environmental moderation of ACE components)
#'
#' @aliases plot.MxModel.GxE
#' @param x A fitted \code{\link{umxGxE}} model to plot
#' @param xlab String to use for the x label (default = NA, which will use the variable name)
#' @param location Where to plot the legend (default = "topleft")
#' see ?legend for alternatives like bottomright
#' @param separateGraphs (default = FALSE)
#' @param ... Optional additional parameters
#' @return - 
#' @family Twin Modeling Functions
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - \code{\link{umxGxE}}
#' @references - \url{http://tbates.github.io}
#' @examples
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, zyg == 1, selVars)
#' dzData  = subset(twinData, zyg == 3, selVars)
#' m1 = umxGxE(selDVs = selDVs, selDefs = selDefs, 
#'  	dzData = dzData, mzData = mzData, dropMissing = TRUE)
#' plot(m1)
#' umxPlotGxE(x = m1, xlab = "SES", separateGraphs = TRUE, location = "topleft")
umxPlotGxE <- function(x, xlab = NA, location = "topleft", separateGraphs = FALSE, ...) {
	model = x # to emphasise that x has to be a umxGxE model
	# get unique values of moderator
	mzData = model$submodels$MZ$data$observed
	dzData = model$submodels$DZ$data$observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = selDefs[1]
	}
	mz1 = as.vector(mzData[,selDefs[1]])
	mz2 = as.vector(mzData[,selDefs[2]])
	dz1 = as.vector(dzData[,selDefs[1]])
	dz2 = as.vector(dzData[,selDefs[2]])
	allValuesOfDefVar= c(mz1,mz2,dz1,dz2)
	defVarValues = sort(unique(allValuesOfDefVar))
	a   = model$submodels$top$matrices$a$values
	c   = model$submodels$top$matrices$c$values
	e   = model$submodels$top$matrices$e$values
	am  = model$submodels$top$matrices$am$values
	cm  = model$submodels$top$matrices$cm$values
	em  = model$submodels$top$matrices$em$values
	Va  = (a + am * defVarValues)^2
	Vc  = (c + cm * defVarValues)^2
	Ve  = (e + em * defVarValues)^2
	Vt  = Va + Vc + Ve
	out    = as.matrix(cbind(Va, Vc, Ve, Vt))
	outStd = as.matrix(cbind(Va/Vt, Vc/Vt, Ve/Vt))
	
	if(separateGraphs){
		print("Outputting two graphs")
	}else{
		graphics::par(mfrow = c(1, 2)) # one row, two columns for raw and std variance
		# par(mfrow = c(2, 1)) # two rows, one column for raw and std variance
	}
	graphics::matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = 1:4, xlab = xlab, ylab = "Variance", main= "Raw Moderation Effects")
	graphics::legend(location, legend = c("genetic", "shared", "unique", "total"), lty = 1:4, col = 1:4)
	# legend(location, legend= c("Va", "Vc", "Ve", "Vt"), lty = 1:4, col = 1:4)
	graphics::matplot(defVarValues, outStd, type = "l", lty = 1:4, col = 1:4, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main= "Standardized Moderation Effects")
	# legend(location, legend= c("Va", "Vc", "Ve"), lty = 1:4, col = 1:4)
	graphics::legend(location, legend = c("genetic", "shared", "unique"), lty = 1:4, col = 1:4)
	graphics::par(mfrow = c(1, 1)) # back to black
}

#' @export
plot.MxModel.GxE <- umxPlotGxE

#' umxPlotCP
#'
#' Draw a graphical figure for a Common Pathway model
#'
#' @aliases plot.MxModel.CP
#' @param x The Common Pathway \code{\link{mxModel}} to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param showMeans Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @family Twin Modeling Functions
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' plot(yourCP_Model) # no need to remember a special name: plot works fine!
#' }
umxPlotCP <- function(x = NA, file = "name", digits = 2, showMeans = FALSE, std = TRUE, ...) {
	model = x # just to emphasise that x has to be a model 
	if(std){
		model = umx_standardize_CP(model)
	}
	# TODO Check I am handling nFac > 1 properly!!
	facCount = dim(model$submodels$top$a_cp$labels)[[1]]
	varCount = dim(model$submodels$top$as$values)[[1]]
	selDVs   = dimnames(model$submodels$MZ$data$observed)[[2]]
	selDVs   = selDVs[1:(varCount)]
	parameterKeyList = omxGetParameters(model)
	out = "";
	cSpecifics = c();
	latents = c();
	for(thisParam in names(parameterKeyList) ) {
		if( grepl("^[ace]_cp_r[0-9]", thisParam)) { # top level a c e
			# top level a c e
			# a_cp_r1c1 note: r1 = factor1
			from = sub("^([ace]_cp)_r([0-9])", '\\1\\2', thisParam, perl=T);
			# "a_cp_r1c1" # row = common factor number
			target = sub("^([ace]_cp)_r([0-9]).*", 'common\\2', thisParam, perl=T);
			latents = append(latents,from);
		} else if (grepl("^cp_loadings_r[0-9]", thisParam)) {
			# common loading cp_loadings_r1c1
			from    = sub("^cp_loadings_r([0-9])c([0-9])", "common\\2", thisParam, perl=TRUE);
			# from    = "common";
			thisVar = as.numeric(sub('cp_loadings_r([0-9])c([0-9])', '\\1', thisParam, perl = TRUE));
			target  = selDVs[as.numeric(thisVar)]
			latents = append(latents,from);
		} else if (grepl("^[ace]s_r[0-9]", thisParam)) {
			# specific
			grepStr = '([ace]s)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1\\3', thisParam, perl=T);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl=T));
			target  = selDVs[as.numeric(targetindex)]			
			latents = append(latents,from);
			cSpecifics = append(cSpecifics,from);
		} else if (grepl("^expMean", thisParam)) { # means probably expMean_r1c1
			grepStr = '(^.*)_r([0-9]+)c([0-9]+)'
			from    = "one";
			targetindex = as.numeric(sub(grepStr, '\\3', thisParam, perl=T));
			target  = selDVs[as.numeric(targetindex)];
		} else {
			stop("I don't know what a ", thisParam, " is")
		}
		if(from == "one" & !showMeans ){
			# not adding means...
		} else {
			CIstr = umx_APA_model_CI(model, cellLabel = thisParam, prefix = "top.", suffix = "_std", digits = digits)
			if(is.na(CIstr)){
				val = round(parameterKeyList[thisParam], digits)
			}else{
				val = CIstr
			}
			out = paste0(out, ";\n", from, " -> ", target, " [label=\"", val, "\"]")
		}
	}
	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}
	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(n in c(1:varCount)) {
	   preOut = paste0(preOut, "\n", selDVs[n], " [shape=square];\n")
	}
	
	ranks = paste(cSpecifics, collapse = "; ");
	ranks = paste0("{rank=sink; ", ranks, "}");
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, ranks, out, "\n}");
	xmu_dot_maker(model, file, digraph)
}

#' @export
plot.MxModel.CP <- umxPlotCP

#' umxPlotIP
#'
#' Make a graphical display of an Independent Pathway model
#'
#' @aliases plot.MxModel.IP
#' @param x The \code{\link{umxIP}} model to plot
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param showMeans Whether to show means paths (defaults to FALSE)
#' @param std whether to standardize the model (defaults to TRUE)
#' @param ... Optional additional parameters
#' 
#' @return - optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()}
#' @references - \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' plot(model)
#' umxPlotIP(model, file = NA)
#' }
umxPlotIP  <- function(x = NA, file = "name", digits = 2, showMeans = FALSE, std = TRUE, ...) {
	model = x # to emphasise that x has to be an umxIP model
	if(std){
		model = umx_standardize_IP(model)
	}
	# TODO Check I am handling nFac > 1 properly!!
	facCount = dim(model$submodels$top$a_cp$labels)[[1]]
	varCount = dim(model$submodels$top$ai$values)[[1]]
	selDVs   = dimnames(model$submodels$MZ$data$observed)[[2]]
	selDVs   = selDVs[1:(varCount)]
	parameterKeyList = omxGetParameters(model, free = TRUE);
	out = "";
	cSpecifics = c();
	latents = c()
	for(thisParam in names(parameterKeyList) ) {
		if( grepl("^[ace]i_r[0-9]", thisParam)) {
			# top level a c e
			# "ai_r1c1" note: c1 = factor1, r1 = variable 1
			# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin");
			grepStr = '^([ace]i)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1_\\3', thisParam, perl = TRUE);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl=T));
			target  = selDVs[as.numeric(targetindex)]
			latents = append(latents,from);
		} else if (grepl("^[ace]s_r[0-9]", thisParam)) { # specific
			grepStr = '([ace]s)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1\\3', thisParam, perl = T);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl = T));
			target  = selDVs[as.numeric(targetindex)]
			cSpecifics = append(cSpecifics,from);
			latents = append(latents,from);
		} else if (grepl("^expMean", thisParam)) { # means probably expMean_r1c1
			grepStr = '(^.*)_r([0-9]+)c([0-9]+)'
			from    = "one";
			targetindex = as.numeric(sub(grepStr, '\\3', thisParam, perl=T));
			target  = selDVs[as.numeric(targetindex)];
		} else {
			stop("I don't know what a ", thisParam, " is")
		}

		if(!showMeans & from == "one"){
			# not adding means...
		} else {
			CIstr = umx_APA_model_CI(model, cellLabel = thisParam, prefix = "top.", suffix = "_std", digits = digits, verbose = F)
			if(is.na(CIstr)){
				val = round(parameterKeyList[thisParam], digits)
			}else{
				val = CIstr
			}
			out = paste0(out, ";\n", from, " -> ", target, " [label=\"", val, "\"]")
		}
		# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin");
	}

	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}
	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(n in c(1:varCount)) {
	   preOut = paste0(preOut, "\n", selDVs[n], " [shape=square];\n")
	}

	ranks = paste(cSpecifics, collapse = "; ");
	ranks = paste0("{rank=sink; ", ranks, "}");
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, ranks, out, "\n}");
	xmu_dot_maker(model, file, digraph)
}
#' @export
plot.MxModel.IP <- umxPlotIP

#' umxMI
#'
#' Report modifications which would improve fit.
#' Notes:
#' 1. Runs much fast with full = FALSE (but this doesn't allow the model to re-fit around the newly-
#' freed parameter).
#' 2. Compared to mxMI, this function returns top changes, and also suppresses the run message.
#' 3. Finally, of course: see the requirements for (legitimate) post-hoc modeling in \code{\link{mxMI}}
#' You are almost certainly doing better science when testing competing models rather than modifying a model to fit.
#' @param model An \code{\link{mxModel}} for which to report modification indices
#' @param matrices which matrices to test. The default (NA) will test A & S for RAM models
#' @param full Change in fit allowing all parameters to move. If FALSE only the parameter under test can move.
#' @param numInd How many modifications to report. Use -1 for all. Default (NA) will report all over 6.63 (p = .01)
#' @param typeToShow Whether to shown additions or deletions (default = "both")
#' @param decreasing How to sort (default = TRUE, decreasing)
#' @seealso - \code{\link{mxMI}}
#' @family Modify or Compare Models
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)[1:3]
#' df = mxData(cov(demoOneFactor[,manifests]), type = "cov", numObs = 500)
#' m1 <- umxRAM("One Factor", data = df,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' umxMI(m1, full=FALSE)
umxMI <- function(model = NA, matrices = NA, full = TRUE, numInd = NA, typeToShow = "both", decreasing = TRUE) {
	if(typeToShow != "both"){
		message("Only showing both add and remove is supported so far")
	}
	if(is.na(matrices)){
		if(umx_is_RAM(model)){
			matrices = c("A", "S")
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
			suggestions = sort(MIlist, decreasing = TRUE)[1:numInd]
		} else {
			suggestions = sort(MIlist[MIlist > thresh], decreasing = TRUE)
		}		
	} else {
		suggestions = sort(MIlist, decreasing = TRUE)[1:numInd]
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
#' @param from A variable in the model that you want to imput the effect of a change
#' @param delta A the amount to simulate changing \"from\" by. 
#' @param to The dependent variable that you want to watch changing
#' @param model The model containing from and to
#' @seealso - \code{\link{umxRun}}, \code{\link{mxCompare}}
#' @family Modify or Compare Models
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
		if(data$type != "raw") {
			stop("Conditionals requires either new data or a model with raw data.")
		}
		newData <- data$observed
	}
	
	if(is.list(expectation)) {  # New fit-function style
		eCov  <- model$fitfunction$info$expCov
		eMean <- model$fitfunction$info$expMean
		expectation <- model$expectation
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X$values, list(X=expectation$A)))
			S <- eval(substitute(model$X$values, list(X=expectation$S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation$M)) {
				M <- eval(substitute(model$X$values, list(X=expectation$M)))
			}
		}
	} else { # Old objective-style
		eCov <- model$objective$info$expCov
		eMean <- model$objective$info$expMean
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X$values, list(X=expectation$A)))
			S <- eval(substitute(model$X$values, list(X=expectation$S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation$M)) {
				M <- eval(substitute(model$X$values, list(X=expectation$M)))
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

#' extractAIC from MxModel
#'
#' Returns the AIC for an OpenMx model
#' helper function for \code{\link{logLik.MxModel}} (which enables AIC(model); logLik(model); BIC(model)
#' Original Author: brandmaier
#'
#' @method extractAIC MxModel
#' @rdname extractAIC.MxModel
#' @export
#' @param fit an fitted \code{\link{mxModel}} from which to get the AIC
#' @param scale not used
#' @param k not used
#' @param ... any other parameters (not used)
#' @return - AIC value
#' @seealso - \code{\link{AIC}}, \code{\link{umxCompare}}, \code{\link{logLik.MxModel}}
#' @family Reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/931#comment-4858}
#' @examples
#' require(umx)
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
extractAIC.MxModel <- function(fit, scale, k, ...) {
	a = umxCompare(fit)
	return(a[1, "AIC"])
}

#' umxExpCov
#'
#' Extract the expected covariance matrix from an \code{\link{mxModel}}
#'
#' @param object an \code{\link{mxModel}} to get the covariance matrix from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Deafult (NULL) is not not round at all.
#' @param ... extra parameters (to match \code{\link{vcov}})
#' @return - expected covariance matrix
#' @export
#' @family Reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/2598}
#' Original written by \url{http://openmx.psyc.virginia.edu/users/bwiernik}
#' @seealso - \code{\link{umxRun}}, \code{\link{umxCI_boot}}
#' @examples
#' require(umx)
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
umxExpCov <- function(object, latents = FALSE, manifests = TRUE, digits = NULL, ...){
	# umx_has_been_run(m1)
	if(object$data$type == "raw"){
		manifestNames = names(object$data$observed)
	} else {
		manifestNames = dimnames(object$data$observed)[[1]]
	}
	if(umx_is_RAM(object)){
		if(manifests & !latents){
			# expCov = attr(object$objective[[2]]$result, "expCov")
			thisFit = paste0(object$name, ".fitfunction")
			expCov <- attr(object$output$algebras[[thisFit]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		} else {
			A <- mxEval(A, object)
			S <- mxEval(S, object)
			I <- diag(1, nrow(A))
			E <- solve(I - A)
			expCov <- E %&% S # The model-implied covariance matrix
			mV <- NULL
			if(latents) {
				mV <- object@latentVars 
			}
			if(manifests) {
				mV <- c(mV, object@manifestVars)
			}
			expCov = expCov[mV, mV]
		}
	} else {
		if(latents){
			stop("I don't know how to reliably get the latents for non-RAM objects... Sorry :-(")
		} else {
			expCov <- attr(object$output$algebras[[paste0(object$name, ".fitfunction")]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		}
	}
	if(!is.null(digits)){
		expCov = round(expCov, digits)
	}
	return(expCov) 
}

#' @rdname umxExpCov
#' @export
vcov.MxModel <- umxExpCov

#' umxExpMean
#'
#' Extract the expected means matrix from an \code{\link{mxModel}}
#'
#' @param model an \code{\link{mxModel}} to get the means from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Default (NULL) will not round at all.
#' @return - expected means
#' @export
#' @family Reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/2598}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(demoOneFactor[1:100,], type = "raw")
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
	expMean <- attr(model$output$algebras[[paste0(model$name, ".fitfunction")]], "expMean")
	
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

#' logLik.MxModel
#'
#' Returns the log likelihood for an OpenMx model. This helper also 
#' enables \code{\link{AIC}}(model); \code{\link{BIC}}(model).
#'
#' hat-tip Andreas Brandmaier
#'
#' @method logLik MxModel
#' @rdname logLik
#' @export
#' @param object the \code{\link{mxModel}} from which to get the log likelihood
#' @param ... Optional parameters
#' @return - the log likelihood
#' @seealso - \code{\link{AIC}}, \code{\link{umxCompare}}
#' @family Reporting functions
#' @references - \url{http://openmx.psyc.virginia.edu/thread/931#comment-4858}
#' @examples
#' require(umx)
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
logLik.MxModel <- function(object, ...) {
	model = object # just to be clear that object is a model
	Minus2LogLikelihood <- NA
	if (!is.null(model$output) & !is.null(model$output$Minus2LogLikelihood)){
		Minus2LogLikelihood <- (-0.5) * model$output$Minus2LogLikelihood		
	}
	if (!is.null(model$data)){
		attr(Minus2LogLikelihood,"nobs") <- model$data$numObs
	}else{ 
		attr(Minus2LogLikelihood,"nobs") <- NA
	}
	if (!is.null(model$output)){
		attr(Minus2LogLikelihood,"df") <- length(model$output$estimate)	
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
#' @family Reporting functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
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
	observed.cov <- model$data$observed
	observed.cor <- cov2cor(observed.cov)

	A <- model$matrices$A$values
	S <- model$matrices$S$values
	F <- model$matrices$F$values
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
#' @param x an object from which to get the RMSEA 
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show
#' @return - RMSEA object containing value (and perhaps a CI)
#' @export
#' @family Reporting functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}, \url{http://openmx.psyc.virginia.edu}
RMSEA <- function(x, ci.lower, ci.upper, digits) UseMethod("RMSEA", x)

#' RMSEA function for MxModels
#'
#' Compute the confidence interval on RMSEA
#'
#' @param x an \code{\link{mxModel}} from which to get RMSEA
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show (defaults to 3)
#' @return - object containing the RMSEA and lower and upper bounds
#' @rdname RMSEA.MxModel
#' @export
#' @family Reporting functions
#' @references - \url{https://github.com/simsem/semTools/wiki/Functions}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' RMSEA(m1)
RMSEA.MxModel <- function(x, ci.lower = .05, ci.upper = .95, digits = 3) { 
	sm <- summary(x)
	RMSEA.summary.mxmodel(x= sm, ci.lower = ci.lower, ci.upper = ci.upper, digits = digits)
}

#' RMSEA function for MxModels
#'
#' Compute the confidence interval on RMSEA
#'
#' @param x an \code{\link{mxModel}} summary from which to get RMSEA
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show (defaults to 3)
#' @return - object containing the RMSEA and lower and upper bounds
#' @rdname RMSEA.summary.mxmodel
#' @export
#' @family Reporting functions
#' @references - \url{https://github.com/simsem/semTools/wiki/Functions}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' RMSEA(m1)
RMSEA.summary.mxmodel <- function(x, ci.lower = .05, ci.upper = .95, digits = 3){
	if(ci.lower != .05 | ci.upper != .95){
		stop("only 95% CI on RMSEA supported as yet...")
	}
	txt = paste0("RMSEA = ", round(x$RMSEA, digits))
	txt = paste0(txt, " CI", sub("^0?\\.", replacement = "", ci.upper))
	txt = paste0(txt, "[", round(x$RMSEACI["lower"], digits), ", ")
	txt = paste0(txt, round(x$RMSEACI["upper"], digits), "], ")
	txt = paste0(txt, "Prob(RMSEA <= 0.05) = ", umx_APA_pval(x$RMSEAClose))
	print(txt)
	invisible(list(RMSEA = x$RMSEA, CI.lower = x$RMSEACI["lower"], 
		CI.upper = x$RMSEACI["upper"], RMSEA.pvalue = x$RMSEAClose, txt = txt)
	)
}

# ===================================
# = summary Stats and table helpers =
# ===================================

#' umx_fun
#'
#' Misellaneous functions that are handy in summary and other tasks where you might otherwise have
#' to craft a custom nameless funtions. e.g.
#' 
#' \itemize{
#'   \item \code{\link{umx_fun_mean_sd}}: returns "mean (SD)" of x.
#'   \item Second item
#' }
#'
#' @param x input
#' @param na.rm How to handle missing (default = TRUE = remove)
#' @param digits Rounding (default = 2)
#' @return - function result
#' @export
#' @family Miscellaneous Stats Helpers
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' summaryAPA(mtcars[,1:3]) # uses umx_fun_mean_sd
umx_fun_mean_sd = function(x, na.rm = TRUE, digits = 2){
	paste0(
		round(mean(x, na.rm = na.rm),2), " ",
		"(", round(sd(x, na.rm=TRUE),digits = digits), ")"
	)
}

#' umx_aggregate
#'
#' R's built-in \code{\link{aggregate}} function is extremely useful and powerful, allowing
#' xtabs based on a formula. umx_aggregate just tries to make using it a bit easier.
#' In particular, it has some handy base functions that simplify the task of summarising data
#' aggregating over some grouping factor. A common use is preparing summary tables.
#'
#' @param formula The aggregation formula. e.g., DV ~ condition
#' @param data frame to aggregate with
#' @param what function to use. Defaults to a built-in "smart" mean (sd)
#' @return - table
#' @export
#' @family Reporting Functions
#' @seealso - \code{\link{aggregate}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' aggregate(mpg ~ cyl, FUN = mean, na.rm = TRUE, data = mtcars)
#' umx_aggregate(mpg ~ cyl, data = mtcars)
#' umx_aggregate(cbind(mpg, qsec) ~ cyl, data = mtcars)
#' t(umx_aggregate(cbind(mpg, qsec) ~ cyl, data = mtcars))
#' \dontrun{
#' umx_aggregate(cbind(moodAvg, mood) ~ condition, data = study1)
#' }
umx_aggregate <- function(formula = DV ~ condition, data, what = c("mean_sd", "n")) {
	# TODO N doesn't seem needed here?
	# TODO other handy aggregating functions?
	mean_sd = function(x){
		paste0(round(mean(x, na.rm=TRUE),2), " (",
			   round(sd(x, na.rm=TRUE),2), ")"
		)
	}
	x_n = function(x){sum(!is.na(x))}

	what = umx_default_option(what, c("mean_sd", "n"), check=FALSE)
	if(what == "mean_sd"){
		FUN = mean_sd
	} else if(what == "n"){
		FUN = x_n
	}else{
		FUN = what
	}
	tmp = aggregate(formula, FUN = FUN, data = data)
	n_s = aggregate(formula, FUN = x_n, data = data)

	# old way
	# row.names(tmp) = paste0(as.character(tmp[,1]), " (n = ", n_s[,2], ")")
	# # tmp = data.frame(tmp)
	# tmp = tmp[,-1, drop = FALSE]
	# return(tmp)

	# new way
	tmp = data.frame(tmp)
	tmp[,1] = paste0(as.character(tmp[,1]), " (n = ", n_s[,2], ")")
	return(tmp)	
}

#' umx_APA_pval
#'
#' round a p value so you get < .001 instead of .000000002 or 1.00E-09
#'
#' @param p The p-value to round
#' @param min Values below min will be reported as "< min"
#' @param digits Number of decimals to which to round (default = 3)
#' @param addComparison Whether to add '=' '<' etc. (NA adds when needed)
#' @param rounding deprecated - please replace 'rounding' with 'digits'
#' @family Reporting Functions
#' @return - p-value formatted in APA style
#' @export
#' @seealso - \code{\link{round}}
#' @examples
#' umx_APA_pval(.052347)
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(1.23E-4)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = TRUE)
umx_APA_pval <- function(p, min = .001, digits = 3, addComparison = NA, rounding = NULL) {
	if(!is.null(rounding)){
		message("rounding deprecated - please replace 'rounding' with 'digits'")
		digits = rounding
	}
	# leave addComparison as NA to add only when needed
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

#' umxAPA
#'
#' @description
#' This function creates object summaries used in reporting models, effects, and summarizing data.
#' 1. Given an lm, will return a formated effect, including 95\% CI 
#' in square brackets, for one of the effects (specified by name in se). e.g.:
#' 
#' \code{\link{summaryAPA}}(m1, "wt")
#' \eqn{\beta} = -5.344 [-6.486, -4.203], p< 0.001
#' 
#' 2. Given a dataframe, summaryAPA will return a table of correlations, with
#' the mean and SD of each variable as the last row.
#' 
#' 3. Given obj and se will return a CI based on 1.96 times the se.
#' 
#' 4. Given only obj, will be treated as a p value as returned in APA format.
#' 
#' @aliases summaryAPA
#' @param obj Either a model (\link{lm}), a beta-value, or a data.frame
#' @param se If b is a model, then name of the parameter of interest, else the SE (standard-error)
#' @param std If obj is an lm, whether to re-run the model on standardized data and report std betas
#' @param digits Round numbers to how many values
#' @param use If obj is a data.frame, how to handle NA (default = "complete")
#' @param min = .001 for a p-value, the smallest value to report numerically
#' @param addComparison for a p-value, whether to add "</=" default (NA) adds "<" if necessary
#' @param report what to return (default = markdown table). Use "html" to open a web page table
#' @return - string
#' @export
#' @family Reporting Functions
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' # Generate a formatted string convey the effects in a model:  
#' umxAPA(lm(mpg ~ wt + disp, mtcars))
#' umxAPA(lm(mpg ~ wt + disp, mtcars), "disp")
#' # Generate a summary table of correlations + Mean and SD:
#' umxAPA(mtcars[,1:3])
#' # Generate a CI string based on effect and se
#' umxAPA(.4, .3)
#' # format p-value
#' umxAPA(.0182613)
#' umxAPA(.000182613)
umxAPA <- function(obj, se = NULL, std = FALSE, digits = 2, use = "complete", min = .001, addComparison = NA, report = c("table", "html")) {
	report = match.arg(report)
	if(class(obj)=="data.frame"){
		# generate a summary of correlation and means
		cor_table = umxHetCor(obj, ML = FALSE, use = use, treatAllAsFactor = FALSE, verbose = FALSE)
		cor_table = umx_apply(round, cor_table, digits = digits) # round corelations
		mean_sd = umx_apply(umx_fun_mean_sd, obj)
		output = data.frame(rbind(cor_table, mean_sd), stringsAsFactors = FALSE)
		if(report == "html"){
			umx_print(output, digits = digits, file = "tmp.html")
		} else {
			umx_print(output, digits = digits)
		}
		if(anyNA(obj)){
			message("Some rows in dataframe had missing values.")
		}
	}else if( "lm" == class(obj)){
		# report lm summary table
		if(std){
			obj = update(obj, data = umx_scale(obj$model))
		}
		model_coefficients = summary(obj)$coefficients
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
			tval    = b_and_p["t value"]
			pval    = b_and_p["Pr(>|t|)"]
			print(paste0(i, " \u03B2 = ", round(b, digits), 
			   " [", round(lower, digits), ", ", round(upper, digits), "], ",
			   "t = ", round(tval, digits), ", p ", umx_APA_pval(pval, addComparison = TRUE)
			))		
		}
	} else {
		if(is.null(se)){
			# p-value
			umx_APA_pval(obj, min = min, digits = digits, addComparison = addComparison)
		} else {
			# beta and SE
			print(paste0("\u03B2 = ", round(obj, digits), " [", round(obj - (1.96 * se), digits), ", ", round(obj + (1.96 * se), digits), "]"))
		}
	}
}

#' @export
summaryAPA <- umxAPA

#' umx_APA_model_CI
#'
#' Look up CIs for free parameters in a model, and return as APA-formatted text string
#'
#' @param model an \code{\link{mxModel}} to get CIs from
#' @param cellLabel the label of the cell to interogate for a CI, e.g. "ai_r1c1"
#' @param prefix This submodel to look in (i.e. "top.")
#' @param suffix The suffix for algebras ("_std")
#' @param digits = 2
#' @param verbose = FALSE
#' @return - the CI string, e.g. ".73 [-.2, .98]"
#' @export
#' @family Misc
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umx_APA_model_CI(fit_IP, cellLabel = "ai_r1c1", prefix = "top.", suffix = "_std")
#' }
umx_APA_model_CI <- function(model, cellLabel, prefix = "top.", suffix = "_std", digits = 2, verbose= FALSE){
	if(!umx_has_CIs(model)){
		if(verbose){
			message("no CIs")
		}
		return(NA)
	} else {
		# we want "top.ai_std[1,1]" from "ai_r1c1"
		result = tryCatch({
			grepStr = '^(.*)_r([0-9]+)c([0-9]+)$' # 1 = matrix names, 2 = row, 3 = column
			mat = sub(grepStr, '\\1', cellLabel, perl = TRUE);
			row = sub(grepStr, '\\2', cellLabel, perl = TRUE);
			col = sub(grepStr, '\\3', cellLabel, perl = TRUE);
		
			z = model$output$confidenceIntervals
			dimIndex = paste0(prefix, mat, suffix, "[", row, ",", col, "]")

			intervalNames = dimnames(z)[[1]]
			
			
			APAstr = paste0(
				umx_APA_pval(z[dimIndex, "estimate"], min = -1, digits = digits),
				" [",
				umx_APA_pval(z[dimIndex, "lbound"], min = -1, digits = digits),
				", ",
				umx_APA_pval(z[dimIndex, "ubound"], min = -1, digits = digits),
				"]"
			)
		    return(APAstr) 
		}, warning = function(cond) {
			if(verbose){
				message(paste0("warning ", cond, " for CI ", omxQuotes(cellLabel)))
			}
		    return(NA) 
		}, error = function(cond) {
			if(verbose){
				message(paste0("error: ", cond, " for CI ", omxQuotes(cellLabel), "\n",
				"dimIndex = ", dimIndex))
				print(intervalNames)
			}
		    return(NA) 
		}, finally = {
		    # cleanup-code
		})
		return(result)
	}
	# if estimate differs...
}