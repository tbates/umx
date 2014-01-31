# https://r-forge.r-project.org/project/admin/?group_id=1745
# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); 
# 
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx"); 
# require(OpenMx); require(umx); ?umx
# devtools::build("~/bin/umx")
# devtools::check("~/bin/umx")
# devtools::load_all("~/bin/umx")
# devtools::dev_help("umxX")
# devtools::show_news("~/bin/umx")

# ========================================
# = Model building and modifying helpers =
# ========================================

#' umxStart
#'
#' umxStart will set start values for the free parameters in RAM and Matrix \code{\link{mxModel}}s, or even mxMatrices.
#' It will try and be smart in guessing these from the values in your data, and the model type.
#'
#' @param obj The RAM or matrix \code{\link{mxModel}}, or \code{\link{mxMatrix}} that you want to set start values for.
#' @param sd Optional Standard Deviation for start values
#' @param n  Optional Mean for start values
#' @param onlyTouchZeros Don't start things that appear to have already been started (useful for speeding \code{\link{umxReRun}})
#' @return - \code{\link{mxModel}} with updated start values
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @export
#' @examples
#' \dontrun{
#' model = umxStart(model)
#' }
umxStart <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = F) {
	if(is.numeric(obj) ) {
		xmuStart_value_list(x = obj, sd = NA, n = 1)
	} else {
		# This is an MxRAM Model: Set sane starting values
		# TODO: Start values in the A matrix...
		# Done: Start values in the S at variance on diag, bit less than cov off diag
		# Done: Start amnifest means in means model
		# TODO: Start latent means?...		
		if (!umx_is_RAM(obj) ) {
			stop("'obj' must be a RAM model (or a simple number)")
		}
		if (length(obj@submodels) > 0) {
			stop("Cannot yet handle submodels")
		}
		theData = obj@data@observed
		if (is.null(theData)) {
			stop("'model' does not contain any data")
		}
		manifests = obj@manifestVars
		nVar      = length(manifests)
		if(obj@data@type == "raw"){
			# = Set the means =
			if(is.null(obj@matrices$M)){
				msg("You are using raw data, but have not yet added paths for the means\n")
				stop("You do this with mxPath(from = 'one', to = 'var')")
			} else {			
				dataMeans = colMeans(theData[,manifests], na.rm = T)
				freeManifestMeans = (obj@matrices$M@free[1, manifests] == T)
				obj@matrices$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
				covData = cov(theData, use = "pairwise.complete.obs")
			}
		} else {
			covData = theData
		}
		dataVariances = diag(covData)
		# ==========================================================
		# = Fill the free symetrical matrix with good start values =
		# ==========================================================
		# The diagonal is variances
		if(onlyTouchZeros) {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE) & obj@matrices$S@values[1:nVar, 1:nVar] == 0
		} else {
			freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE)			
		}
		obj@matrices$S@values[1:nVar, 1:nVar][freePaths] = covData[freePaths]
		return(obj)
	}
}

#' umxLabel
#'
#' umxLabel adds labels to things, be it an: \code{\link{mxModel}} (RAM or matrix based), an \code{\link{mxPath}}, or an \code{\link{mxMatrix}}
#' This is a core function in umx: Adding labels to paths opens the door to \code{\link{umxEquate}}, as well as \code{\link{omxSetParameters}}
#'
#' @param obj An \code{\link{mxModel}} (RAM or matrix based), \code{\link{mxPath}}, or \code{\link{mxMatrix}}
#' @param suffix String to append to each label (might be used to distinguish, say male and female submodels in a model)
#' @param baseName String to prepend to labels. Defaults to empty
#' @param setfree Whether to also 
#' @param drop The value to fix "drop" paths to (defaults to 0)
#' @param jiggle How much to jiggle values in a matrix or list of path values
#' @param boundDiag Whether to bound the diagonal of a matrix
#' @param verbose How much feedback to give the user (default = F)

#' @return - \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxGetParameters}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @export
#' @examples
#' \dontrun{
#'  model = umxLabel(model)
#'  umxLabel(mxMatrix("Full", 3,3, values = 1:9, name = "a"))
#' }
umxLabel <- function(obj, suffix = "", baseName = NA, setfree = F, drop = 0, labelFixedCells = T, jiggle = NA, boundDiag = NA, verbose = F, overRideExisting = F) {	
	# TODO !!!! labelling rule for bivariate perhaps should be sort alphabetically, makes it unambiguous...
	# TODO !!!! implications for umxAdd1 finding the right labels...
	if (is(obj, "MxMatrix") ) { 
		# label an mxMatrix
		xmuLabel_Matrix(obj, baseName, setfree, drop, jiggle, boundDiag, suffix)
	} else if (umx_is_RAM(obj)) { 
		# label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(obj, suffix, labelFixedCells = labelFixedCells, overRideExisting = overRideExisting))
	} else if (umx_is_MxModel(obj) ) {
		# label a non-RAM matrix model
		return(xmuLabel_MATRIX_Model(obj, suffix))
	} else {
		stop("I can only label OpenMx models and mxMatrix types. You gave me a ", typeof(obj))
	}
}

# =================================
# = Run Helpers =
# =================================

#' umxRun
#'
#' umxRun is a version of \code{\link{mxRun}} which can run also set start values, labels, and run multiple times
#' It can also calculate the saturated and independence likelihoods necessary for most fit indices.
#'
#' @param model The \code{\link{mxModel}} you wish to run.
#' @param n The maximum number of times you want to run the model trying to get a code green run (defaults to 1)
#' @param calc_SE Whether to calculate standard errors (not used when n = 1)
#' for the summary (they are not very accurate, so if you use \code{\link{mxCI}} or \code{\link{umxCI}}, you can turn this off)
#' @param calc_sat Whether to calculate the saturated and independence models (for raw \code{\link{mxData}} \code{\link{mxModel}}s) (defaults to TRUE - why would you want anything else?)
#' @param setStarts Whether to set the start values (defaults to F)
#' @param setLabels Whether to set the labels (defaults to F)
#' @param comparison Whether to run umxCompare() after umxRun
#' @return - \code{\link{mxModel}}
#' @seealso - \code{\link{mxRun}}, \code{\link{umxLabel}}, \code{\link{umxStart}}, \code{\link{mxModel}}, \code{\link{umxCIboot}}
#' @references - \url{http://openmx.psyc.virginia.edu}
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setStarts = T)
#' umxSummary(m1, show = "std")
#' \dontrun{
#' model = umxRun(model) # just run: will create saturated model if needed
#' model = umxRun(model, setStarts = T, setLabels = T) # set start values and label all parameters
#' model = umxRun(model, n=10) # run, but also re-run if not green the first run
#' }

umxRun <- function(model, n = 1, calc_SE = T, calc_sat = T, setStarts = F, setLabels = F, comparison = NULL){
	# TODO: return change in -2LL for models being re-run
	# TODO: stash saturated model for re-use
	# TODO: Optimise for speed
	if(setLabels){
		model = umxLabel(model)
	}
	if(setStarts){
		model = umxStart(model)
	}
	if(n == 1){
		model = mxRun(model);
	} else {
		model = mxOption(model, "Calculate Hessian", "No")
		model = mxOption(model, "Standard Errors", "No")
		# make an initial run
		model = mxRun(model);
		n = (n - 1); tries = 0
		# carry on if we failed
		while(model@output$status[[1]] == 6 && n > 2 ) {
			print(paste("Run", tries+1, "status Red(6): Trying hard...", n, "more times."))
			model <- mxRun(model)
			n <- (n - 1)
			tries = (tries + 1)
		}
		if(tries == 0){ 
			# print("Ran fine first time!")	
		}
		# get the SEs for summary (if requested)
		if(calc_SE){
			# print("Calculating Hessian & SEs")
			model = mxOption(model, "Calculate Hessian", "Yes")
			model = mxOption(model, "Standard Errors", "Yes")
			model = mxRun(model)
		}
	}
	if( umx_is_RAM(model)){
		if(model@data@type == "raw"){
			# If we have a RAM model with raw data, compute the satuated and indpendence models
			# TODO: Update to omxSaturated() and omxIndependenceModel()
			# message("computing saturated and independence models so you have access to absoute fit indices for this raw-data model")
			model_sat = umxSaturated(model, evaluate = T, verbose = F)
			model@output$IndependenceLikelihood = as.numeric(-2*logLik(model_sat$Ind))
			model@output$SaturatedLikelihood    = as.numeric(-2*logLik(model_sat$Sat))
		}
	}
	if(!is.null(comparison)){ 
		umxCompare(comparison, model) 
	}
	return(model)
}

#' umxReRun
#' 
#' umxReRun Is a convenience function to re-run an \code{\link{mxModel}}, optionally dropping parameters
#' The main value for umxReRun is compactness. So this one-liner drops a path labelled "Cs", and returns the updated model:
#' fit2 = umxReRun(fit1, dropList = "Cs", name = "newModelName")
#' 
#' If you're a beginner, stick to 
#' fit2 = omxSetParameters(fit1, labels = "Cs", values = 0, free = F, name = "newModelName")
#' fit2 = mxRun(fit2)
#' 
#' @param lastFit  The \code{\link{mxModel}} you wish to update and run.
#' @param dropList A list of strings. If not NA, then the labels listed here will be dropped (or set to the value and free state you specify)
#' @param regex    A regular expression. If not NA, then all labels matching this expression will be dropped (or set to the value and free state you specify)
#' @param free     The state to set "free" to for the parameters whose labels you specify (defaults to free = FALSE, i.e., fixed)
#' @param value    The value to set the parameters whose labels you specify too (defaults to 0)
#' @param freeToStart Whether to update parameters based on their current free-state. free = c(TRUE, FALSE, NA), (defaults to NA - i.e, not checked)
#' @param name      The name for the new model
#' @param verbose   How much feedback to give
#' @param intervals Whether to run confidence intervals (see \code{\link{mxRun}})
#' @param comparison Whether to run umxCompare() after umxRun
#' @return - \code{\link{mxModel}}
#' @seealso - \code{\link{mxRun}}, \code{\link{umxLabel}}, \code{\link{omxGetParameters}}
#' @references - http://openmx.psyc.virginia.edu/
#' @export
#' @examples
#' \dontrun{
#' fit2 = umxReRun(fit1, dropList = "E_to_heartRate", name = "drop_cs")
#' fit2 = umxReRun(fit1, regex = "^E.*rate", name = "drop_hr")
#' fit2 = umxReRun(fit1, regex = "^E", free=T, value=.2, name = "free_E")
#' fit2 = umxReRun(fit1, regex = "Cs", name="AEip", compare = T)
#' }

umxReRun <- function(lastFit, dropList = NA, regex = NA, free = F, value = 0, freeToStart = NA, name = NA, verbose = F, intervals = F, compare = F) {
	if(is.na(name)){
		name = lastFit@name
	}
	if(is.na(regex)) {
		if(any(is.na(dropList))) {
			stop("Both dropList and regex cannot be empty!")
		} else {
			theLabels = dropList
		}
	} else {
		theLabels = umxGetParameters(lastFit, regex = regex, free = freeToStart, verbose = verbose)
	}
	x = omxSetParameters(lastFit, labels = theLabels, free = free, value = value, name = name)
	# x = umxStart(x)
	x = mxRun(x, intervals = intervals)
	if(compare){
		print(umxCompare(lastFit, c(x)))
	}
	return(x)
}

# Parallel helpers to be added here


#' umxStandardizeModel
#'
#' umxStandardizeModel takes a RAM-style model, and returns standardized version.
#'
#' @param model The \code{\link{mxModel}} you wish to standardise
#' @param return Whether you want a standardize model \code{\link{mxModel}}, 
#' just the parameters (default) or matrices: valid options: "parameters", "matrices", or "model"
#' @param Amatrix Optionally tell the function what the name of the asymmetric matrix is (defaults to RAM standard A)
#' @param Smatrix Optionally tell the function what the name of the symmetric matrix is (defaults to RAM standard S)
#' @param Mmatrix Optionally tell the function what the name of the means matrix is (defaults to RAM standard M)
#' @return - a \code{\link{mxModel}} or else parameters or matrices if you request those
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - http://openmx.psyc.virginia.edu/
#' @export
#' @examples
#' \dontrun{
#'  model = umxStandardizeModel(model, return = "model")
#' }
umxStandardizeModel <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {
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
	if (length(output) < 1)stop("Provided model has no output. I can only standardize models that have been run!")
	# Get the names of the A, S and M matrices 
	if("expectation" %in% slotNames(m1)){
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
	
	# Calculate the expected covariance matrix
	IA <- solve(I-A@values)
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
	# Return the model, if asked
	if(return=="model"){
		return(model)
	}else if(return=="matrices"){
		# return the matrices, if asked
		matrices <- list(model[[nameA]], model[[nameS]])
		names(matrices) <- c("A", "S")
		return(matrices)
	}else if(return=="parameters"){
		# return the parameters
		#recalculate summary based on standardised matrices
		p <- summary(model)$parameters
		p <- p[(p[,2]==nameA)|(p[,2]==nameS),]
		## get the rescaling factor
		# this is for the A matrix
		rescale <- invSDs[p$row] * 1/invSDs[p$col]
		# this is for the S matrix
		rescaleS <- invSDs[p$row] * invSDs[p$col]
		# put the A and the S together
		rescale[p$matrix=="S"] <- rescaleS[p$matrix=="S"]
		# rescale
		p[,5] <- p[,5] * rescale
		p[,6] <- p[,6] * rescale
		# rename the columns
		# names(p)[5:6] <- c("Std. Estimate", "Std.Std.Error")
		return(p)		
	}
}

# ==============================
# = Label and equate functions =
# ==============================

#' umxGetParameters
#'
#' Get the parameter labels from a model. Like \code{\link{omxGetParameters}},
#' but supercharged with regular expressions for more power and ease!
#'
#' @param inputTarget An object to get parameters from: could be a RAM \code{\link{mxModel}}
#' @param regex A regular expression to filter the labels defaults to NA - just returns all labels)
#' @param free  A Boolean determining whether to return only free parameters.
#' @param verbose How much feedback to give
#' @export
#' @seealso - \code{\link{omxGetParameters}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umxGetParameters(model)
#' umxGetParameters(mxEval("as", model1)) # all labels of matrix "as" in model "model1"
#' umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T) # get all columns in row 2 of matrix "as"
#' }

umxGetParameters <- function(inputTarget, regex = NA, free = NA, verbose = F) {
	if(class(inputTarget)[1] %in% c("MxRAMModel","MxModel")) {
		topLabels = names(omxGetParameters(inputTarget, indep = FALSE, free = free))
	} else if(is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget@labels
		} else {
			topLabels = inputTarget@labels[inputTarget@free==free]
		}
		}else{
			stop("I am sorry Dave, umxGetLabels needs either a model or a matrix: you offered a ", class(inputTarget)[1])
		}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( !is.na(regex) ) {
		if(length(grep("[\\.\\*\\[\\(\\+\\|^]+", regex) )<1){ # no grep found: add some anchors for safety
			regex = paste("^", regex, "[0-9]*$", sep=""); # anchor to the start of the string
			anchored = T
			if(verbose == T) {
				message("note: anchored regex to beginning of string and allowed only numeric follow\n");
			}
		}else{
			anchored=F
		}
		theLabels = grep(regex, theLabels, perl = F, value = T) # return more detail
		if(length(theLabels) == 0){
			if(anchored == T){
				stop("Found no matching labels! note: anchored regex to beginning of string and allowed only numeric follow\n", regex);
			} else {
				stop("Found no matching labels!");
			}
		}
	}
	# TODO Be nice to offer a method to handle submodels
	# model@submodels$aSubmodel@matrices$aMatrix@labels
	# model@submodels$MZ@matrices
	return(theLabels)
}

#' umxEquate
#'
#' umxEquate equates slave labels to a set of master labels
#'
#' @param model an \code{\link{mxModel}} within which to equate chosen parameters
#' @param master A list of labels with which slave labels will be equated
#' @param slave A list of labels which will be updated to match master labels, thus equating the parameters
#' @param free Boolean determining what the initial state is of master and slave parameters. 
#' Allows stepping over parameters if they are in a particular state
#' @param verbose How much feedback will be given
#' @param name Optional new name for the returned model
#' @return - \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#'  model = umxEquate(model)
#' }
umxEquate <- function(model, master, slave, free = T, verbose = T, name = NULL) {
	# Purpose: to equate parameters by setting of labels (the slave set) = to the labels in a master set
	# umxEquate(model1, master="am", slave="af", free=T|NA|F")
	if(!(class(model)[1] == "MxModel" | class(model)[1] == "MxRAMModel")){
		message("ERROR in umxEquate: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxEquate(model, master=\"a_to_b\", slave=\"a_to_c\", name=\"model2\") # equate paths a->b and a->c, in a new model called \"model2\"")
		stop()
	}
	if(length(grep("[\\^\\.\\*\\[\\(\\+\\|]+", master) )<1){ # no grep found: add some anchors for safety
		master = paste("^", master, "[0-9]*$", sep=""); # anchor to the start of the string
		slave  = paste("^", slave,  "[0-9]*$", sep="");
		if(verbose==T){
			cat("note: anchored regex to beginning of string and allowed only numeric follow\n");
		}
	}
	masterLabels = names(omxGetParameters(model, indep=FALSE, free=free))
	masterLabels = masterLabels[which(!is.na(masterLabels) )]      # exclude NAs
	masterLabels = grep(master, masterLabels, perl = F, value=T)
	# return(masterLabels)
	slaveLabels = names(omxGetParameters(model, indep=F, free=free))
	slaveLabels = slaveLabels[which(!is.na(slaveLabels))] # exclude NAs
	slaveLabels = grep(slave, slaveLabels, perl = F, value=T)
	if( length(slaveLabels) != length(masterLabels)) {
		print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
		stop("ERROR in umxEquate: master and slave labels not the same length!")
	}
	if( length(slaveLabels)==0 ) {
		legal = names(omxGetParameters(model, indep=FALSE, free=free))
		legal = legal[which(!is.na(legal))]
		message("Labels available in model are: ",legal)
		stop("ERROR in umxEquate: no matching labels found!")
	}
	print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
	model = omxSetParameters(model = model, labels = slaveLabels, newlabels = masterLabels, name = name)
	model = omxAssignFirstParameters(model, indep = F)
	return(model)
}

#' umxDrop1
#'
#' Drops each free parameter (selected via regex), returning an \code{\link{mxCompare}}
#' table comparing the effects. A great way to quickly determine which of several 
#' parameters can be dropped without excessive cost
#'
#' @param model An \code{\link{mxModel}} to drop parameters from 
#' @param regex A string to select parameters to drop. leave empty to try all.
#' This is regular expression enabled. i.e., "^a_" will drop parameters beginning with "a_"
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of model comparisons
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umxDrop1(fit3) # try dropping each free parameters (default)  
#' # drop "a_r1c1" and "a_r1c2" and see which matters more.
#' umxDrop1(model, regex="a_r1c1|a_r1c2")
#' }
umxDrop1 <- function(model, regex = NULL, maxP = 1) {
	if(is.null(regex)) {
		toDrop = umxGetParameters(model, free = T)
	} else if (length(regex) > 1) {
		toDrop = regex
	} else {
		toDrop = grep(regex, umxGetParameters(model, free = T), value = T, ignore.case = T)
	}
	message("Will drop each of ", length(toDrop), " parameters: ", paste(toDrop, collapse = ", "), ".\nThis might take some time...")
	out = list(rep(NA, length(toDrop)))
	for(i in seq_along(toDrop)){
		tryCatch({
			message("item ", i, " of ", length(toDrop))
        	out[i] = umxReRun(model, name = paste0("drop_", toDrop[i]), regex = toDrop[i])
		}, warning = function(w) {
			message("Warning incurred trying to drop ", toDrop[i])
			message(w)
		}, error = function(e) {
			message("Error occurred trying to drop ", toDrop[i])
			message(e)
		})
	}
	out = data.frame(umxCompare(model, out))
	out[out=="NA"] = NA
	suppressWarnings({
		out$p   = as.numeric(out$p) 
		out$AIC = as.numeric(out$AIC)
	})
	n_row = dim(out)[1] # 2 9
	sortedOrder = order(out$p[2:n_row])+1
	out[2:n_row, ] <- out[sortedOrder, ]
	good_rows = out$p < maxP
	good_rows[1] = T
	message(sum(good_rows)-1, "of ", length(out$p)-1, " items were beneath your p-threshold of ", maxP)
	return(out[good_rows, ])
}

#' umxAdd1
#'
#' Add each of a set of paths you provide to the model, returning a table of theire effect on fit
#'
#' @param model an \code{\link{mxModel}} to alter
#' @param pathList1 a list of variables to generate a set of paths
#' @param pathList2 an optional second list: IF set paths will be from pathList1 to members of this list
#' @param arrows Make paths with one or two arrows
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of fit changes
#' @export
#' @seealso - \code{\link{umxDrop1}}, \code{\link{mxModel}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' model = umxAdd1(model)
#' }
umxAdd1 <- function(model, pathList1 = NULL, pathList2 = NULL, arrows = 2, maxP = 1) {
	# DONE: RAM paths
	# TODO add non-RAM
	if ( is.null(model@output) ) stop("Provided model hasn't been run: use mxRun(model) first")
	# stop if there is no output
	if ( length(model@output) < 1 ) stop("Provided model has no output. use mxRun() first!")

	if(arrows == 2){
		if(!is.null(pathList2)){
			a = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			b = xmuMakeTwoHeadedPathsFromPathList(pathList2)
			a_to_b = xmuMakeTwoHeadedPathsFromPathList(c(pathList1, pathList2))
			toAdd = a_to_b[!(a_to_b %in% c(a,b))]
		}else{
			if(is.null(pathList1)){
				stop("best to set pathList1!")
				# toAdd = umxGetParameters(model, free = F)
			} else {
				toAdd = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			}
		}
	} else if(arrows == 1){
		if(is.null(pathList2)){
			stop("pathList2 must not be empty for arrows = 1: it forms the target of each path")
		} else {
			toAdd = xmuMakeOneHeadedPathsFromPathList(pathList1, pathList2)
		}
	}else{
		stop("You idiot :-) : arrows must be either 1 or 2, you tried", arrows)
	}
	# TODO fix count? or drop giving it?
	message("You gave me ", length(pathList1), "source variables. I made ", length(toAdd), " paths from these.")

	# Just keep the ones that are not already free... (if any)
	toAdd2 = toAdd[toAdd %in% umxGetParameters(model, free = F)]
	if(length(toAdd2) == 0){
		if(length(toAdd[toAdd %in% umxGetParameters(model, free = NA)] == 0)){
			message("I couldn't find any of those paths in this model.",
				"The most common cause of this error is submitting the wrong model")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}else{
			message("I found (at least some) of those paths in this model, but they were already free")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}		
		stop()
	}else{
		toAdd = toAdd2
	}
	message("Of these ", length(toAdd), " were currently fixed, and I will try adding them")
	message(paste(toAdd, collapse = ", "))

	message("This might take some time...")
	flush.console()
	# out = data.frame(Base = "test", ep = 1, AIC = 1.0, p = 1.0); 
	row1Cols = c("Base", "ep", "AIC", "p")
	out = data.frame(umxCompare(model)[1, row1Cols])
	for(i in seq_along(toAdd)){
		# model = fit1 ; toAdd = c("x2_with_x1"); i=1
		message("item ", i, " of ", length(toAdd))
		tmp = omxSetParameters(model, labels = toAdd[i], free = T, value = .01, name = paste0("add_", toAdd[i]))
		tmp = mxRun(tmp)
		mxc = umxCompare(tmp, model)
		newRow = mxc[2, row1Cols]
		newRow$AIC = mxc[1, "AIC"]
		out = rbind(out, newRow)
	}

	out[out=="NA"] = NA
	out$p   = round(as.numeric(out$p), 3)
	out$AIC = round(as.numeric(out$AIC), 3)
	out <- out[order(out$p),]
	if(maxP==1){
		return(out)
	} else {
		good_rows = out$p < maxP
		message(sum(good_rows, na.rm = T), "of ", length(out$p), " items were beneath your p-threshold of ", maxP)
		message(sum(is.na(good_rows)), " was/were NA")
		good_rows[is.na(good_rows)] = T
		return(out[good_rows, ])
	}
}

# ===============
# = RAM Helpers =
# ===============

# =========================
# = path-oriented helpers =
# =========================

#' umxLatent
#'
#' Helper to ease the creation of latent variables, including formative variables (where the manifests define the latent, and need wiring up behind, and variances setting)
#' The following figures show how a reflective:
#' \figure{reflective.png}
#' 
#' 
#' and a formative variable are created:
#' 
#' formative\figure{formative.png}
#'
#' @param latent the name of the latent variable (string)
#' @param formedBy the list of variables forming this latent
#' @param forms the list of variables which this latent forms (leave blank for formedBy)
#' @param data the dataframe being used in this model
#' @param type = \"exogenous|endogenous\"
#' @param model.name = NULL
#' @param labelSuffix a string to add to the end of each label
#' @param verbose = T
#' @param endogenous This is now deprecated. use type= \"exogenous|endogenous\"
#' @return - path list
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
#'
#' library(OpenMx)
#' library(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor) # x1-5
#' theData = cov(demoOneFactor)
#' m1 = mxModel("reflective", type = "RAM",
#'     manifestVars = manifests,
#'     latentVars   = latents,
#'     # Factor loadings
#'     umxLatent("G", forms = manifests, type = "exogenous", data = theData),
#' 	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m1 = umxRun(m1, setStarts = T, setLabels = T); umxSummary(m1, show="std")
#' umxPlot(m1)
#' 
#' m2 = mxModel("formative", type = "RAM",
#'     manifestVars = manifests,
#'     latentVars   = latents,
#'     # Factor loadings
#'     umxLatent("G", formedBy = manifests, data = theData),
#' 	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m2 = umxRun(m2, setStarts = T, setLabels = T); umxSummary(m2, show="std")
#' umxPlot(m2)
#' }
umxLatent <- function(latent = NULL, formedBy = NULL, forms = NULL, data = NULL, type = NULL,  model.name = NULL, labelSuffix = "", verbose = T, endogenous = "deprecated") {
	# Purpose: make a latent variable formed/or formed by some manifests
	# Use: umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
	# TODO: delete manifestVariance
	# Check both forms and formedBy are not defined
	if(!endogenous == "deprecated"){
		if(endogenous){
			stop("Error in mxLatent: Use of endogenous=T is deprecated. Remove it and replace with type = \"endogenous\"") 
		} else {
			stop("Error in mxLatent: Use of endogenous=F is deprecated. Remove it and replace with type = \"exogenous\"") 
		}
	}
	if(is.null(latent)) { stop("Error in mxLatent: you have to provide the name of the latent variable you want to create") }
	if( is.null(formedBy) &&  is.null(forms)) { stop("Error in mxLatent: Must define one of forms or formedBy") }
	if(!is.null(formedBy) && !is.null(forms)) { stop("Error in mxLatent: Only one of forms or formedBy can be set") }
	if(is.null(data)) { stop("Error in mxLatent: you have to provide the data that will be used in the model") }
	# ==========================================================
	# = NB: If any vars are ordinal, a call to umxMakeThresholdsMatrices
	# = will fix the mean and variance of ordinal vars to 0 and 1
	# ==========================================================
	# Warning("If you use this with a dataframe containing ordinal variables, don't forget to call umxAutoThreshRAMObjective(df)")
	isCov = umx_is_cov(data, boolean = T)
	if( any(!is.null(forms))) {
		manifests <- forms
	}else{
		manifests <- formedBy
	}
	if(isCov) {
		variances = diag(data[manifests, manifests])
	} else {
		manifestOrdVars = umxIsOrdinalVar(data[,manifests])
		if(any(manifestOrdVars)) {
			means         = rep(0, times = length(manifests))
			variances     = rep(1, times = length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm = T)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use = "complete"))
			if( any(!is.null(forms)) ) {
				contVariances = contVariances * .1 # hopefully residuals are modest
			}
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			if(verbose){
				message("No ordinal variables")
			}
			means     = colMeans(data[, manifests], na.rm = T)
			variances = diag(cov(data[, manifests], use = "complete"))
		}
	}

	if( any(!is.null(forms)) ) {
		# Handle forms case
		# p1 = Residual variance on manifests
		# p2 = Fix latent variance @ 1
		# p3 = Add paths from latent to manifests
		p1 = mxPath(from = manifests, arrows = 2, free = T, values = variances)
		if(is.null(type)){ stop("Error in mxLatent: You must set type to either exogenous or endogenous when creating a latent variable with an outgoing path") }
		if(type == "endogenous"){
			# Free latent variance so it can do more than just redirect what comes in
			if(verbose){
				message(paste("latent '", latent, "' is free (treated as a source of variance)", sep=""))
			}
			p2 = mxPath(from=latent, connect="single", arrows=2, free=T, values=.5)
		} else {
			# fix variance at 1 - no inputs
			if(verbose){
				message(paste("latent '", latent, "' has variance fixed @ 1"))
			}
			p2 = mxPath(from=latent, connect="single", arrows=2, free=F, values=1)
		}
		p3 = mxPath(from = latent, to = manifests, connect = "single", free = T, values = variances)
		if(isCov) {
			# Nothing to do: covariance data don't need means...
			paths = list(p1, p2, p3)
		}else{
			# Add means: fix latent mean @0, and add freely estimated means to manifests
			p4 = mxPath(from = "one", to = latent   , arrows = 1, free = F, values = 0)
			p5 = mxPath(from = "one", to = manifests, arrows = 1, free = T, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	} else {
		# Handle formedBy case
		# Add paths from manifests to the latent
		p1 = mxPath(from = manifests, to = latent, connect = "single", free = T, values = umxStart(.6, n=manifests)) 
		# In general, manifest variance should be left free...
		# TODO If the data were correlations... we can inspect for that, and fix the variance to 1
		p2 = mxPath(from = manifests, connect = "single", arrows = 2, free = T, values = variances)
		# Allow manifests to intercorrelate
		p3 = mxPath(from = manifests, connect = "unique.bivariate", arrows = 2, free = T, values = umxStart(.3, n = manifests))
		if(isCov) {
			paths = list(p1, p2, p3)
		}else{
			# Fix latent mean at 0, and freely estimate manifest means
			p4 = mxPath(from="one", to=latent   , free = F, values = 0)
			p5 = mxPath(from="one", to=manifests, free = T, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	}
	if(!is.null(model.name)) {
		m1 <- mxModel(model.name, type="RAM", manifestVars = manifests, latentVars = latent, paths)
		if(isCov){
			m1 <- mxModel(m1, mxData(cov(df), type="cov", numObs = 100))
			message("\n\nIMPORTANT: you need to set numObs in the mxData() statement\n\n\n")
		} else {
			if(any(manifestOrdVars)){
				m1 <- mxModel(m1, umxThresholdRAMObjective(data, deviationBased = T, droplevels = T, verbose = T))
			} else {
				m1 <- mxModel(m1, mxData(data, type = "raw"))
			}
		}
		return(m1)
	} else {
		return(paths)
	}
	# TODO	shift this to a test file
	# readMeasures = paste("test", 1:3, sep="")
	# bad usages
	# mxLatent("Read") # no too defined
	# mxLatent("Read", forms=manifestsRead, formedBy=manifestsRead) #both defined
	# m1 = mxLatent("Read", formedBy = manifestsRead, model.name="base"); umxPlot(m1, std=F, dotFilename="name")
	# m2 = mxLatent("Read", forms = manifestsRead, as.model="base"); 
	# m2 <- mxModel(m2, mxData(cov(df), type="cov", numObs=100))
	# umxPlot(m2, std=F, dotFilename="name")
	# mxLatent("Read", forms = manifestsRead)

}

umxConnect <- function(x) {
	# TODO handle endogenous	
}

umxSingleIndicators <- function(manifests, data, labelSuffix = "", verbose = T){
	# use case
	# mxSingleIndicators(manifests, data)
	if( nrow(data) == ncol(data) & all(data[lower.tri(data)] == t(data)[lower.tri(t(data))]) ) {
		isCov = T
		if(verbose){
			message("treating data as cov")
		}
	} else {
		isCov = F
		if(verbose){
			message("treating data as raw")
		}
	}
	if(isCov){
		variances = diag(data[manifests,manifests])
		# Add variance to the single manfests
		p1 = mxPath(from=manifests, arrows=2, value=variances)
		return(p1)
	} else {
		manifestOrdVars = umxIsOrdinalVar(data[,manifests])
		if(any(manifestOrdVars)){
			means         = rep(0, times=length(manifests))
			variances     = rep(1, times=length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm=T)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use="complete"))
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			means     = colMeans(data[,manifests], na.rm = T)
			variances = diag(cov(data[,manifests], use = "complete"))
		}
		# Add variance to the single manfests
		p1 = mxPath(from = manifests, arrows = 2, value = variances) # labels = mxLabel(manifests, suffix = paste0("unique", labelSuffix))
		# Add means for the single manfests
		p2 = mxPath(from="one", to=manifests, values=means) # labels = mxLabel("one", manifests, suffix = labelSuffix)
		return(list(p1, p2))
	}
}

umxCheckModel <- function(obj, type = "RAM", hasData = NA) {
	# TODO hasSubmodels = F
	if (!isS4(obj) & is(obj, "MxModel")	) {
		stop("'model' must be an mxModel")
	}
	if (!(class(obj$objective)[1] == "MxRAMObjective" | class(obj$expectation)[1] == "MxExpectationRAM")	) {
		stop("'model' must be an RAMModel")
	}
	if (length(obj@submodels) > 0) {
		stop("Cannot yet handle submodels")
	}
	theData = obj@data@observed
	if (is.null(theData)) {
		stop("'model' does not contain any data")
	}	
}


# ===========================
# = matrix-oriented helpers =
# ===========================

# ===================
# = Ordinal helpers =
# ===================

# umxThresholdRAMObjective can set the means and variance of the latents to 0 & 1, and build an appropriate thresholds matrix
# It uses umxIsOrdinalVar, umxMakeThresholdMatrix as helpers

umxThresholdRAMObjective <- function(df,  deviationBased=T, droplevels = T, verbose=F) {
	# Purpose: add means@0 and variance@1 to each ordinal variable, 
	# Use case: umxThresholdRAMObjective(df)
	# TODO: means = zero & VAR = 1 for ordinal variables
	# (this is a nice place to do it, as we have the df present...)
	if(!any(umxIsOrdinalVar(df))){
		stop("No ordinal variables in dataframe: no need to call umxThresholdRAMObjective")
	} 
	pt1 = mxPath(from = "one", to = umxIsOrdinalVar(df,names = T), connect="single", free=F, values = 0)
	pt2 = mxPath(from = umxIsOrdinalVar(df,names = T), connect = "single", arrows = 2, free = F, values = 1)
	return(list(pt1, pt2, umxMakeThresholdMatrix(df, deviationBased = T, droplevels = T, verbose = F)))
}

umxMakeThresholdMatrix <- function(df, deviationBased = T, droplevels = F, verbose = F) {	
	# Purpose: return a mxRAMObjective
	# mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")
	# use case:  umxMakeThresholdMatrix(df, verbose = T)
	# note, called by umxThresholdRAMObjective()
	# TODO: Let the user know if there are any levels dropped...
	if(droplevels){
		df = droplevels(df)
	}
	if(deviationBased){
		return(xmuMakeDeviationThresholdsMatrices(df, droplevels, verbose))
	} else {
		return(xmuMakeThresholdsMatrices(df, droplevels, verbose))
	}
}

# ==================================
# = Borrowed for tutorial purposes =
# ==================================

summaryACEFit <- function(fit, accuracy = 2, dotFilename = NULL, returnStd = F, extended = F, showRg = F, showStd = T, parentModel = NA, CIs = F, zero.print = ".") {
	# Purpose: summarise a Cholesky model, as returned by makeACE_2Group
	# use case: summaryACEFit(fit, dotFilename=NA);
	# summaryACEFit(safeFit, dotFilename = "name", showStd = T)
	# stdFit = summaryACEFit(fit, accuracy=2, dotFilename="name", returnStd=F, extended=F, showRg=T, showStd=T,parentModel=NA, CIs=T);
	if(length(fit)>1){ # call self recursively
		for(thisFit in fit) {
			message("Output for Model: ",thisFit@name)
			summaryACEFit(thisFit, accuracy=accuracy, dotFilename=dotFilename, returnStd=returnStd, extended=extended, showRg=showRg, showStd=showStd, parentModel=NA, CIs=CIs)
		}
	} else {
		if(!class(parentModel)=="logical"){
			message("Comparison of fit")
			print(mxCompare(parentModel, fit))
		}
		logLikelihood = mxEval(objective, fit); 
		message("-2 \u00d7 log(Likelihood)") # 00d7 is unicode for non-ascii multiplication sign
		print(logLikelihood[1,1]);
		selDVs = dimnames(fit$top.mzCov)[[1]]
		# genEpi_TableFitStatistics(fit, extended=extended)
		nVar <- length(selDVs)/2;
		# Calculate standardised variance components
		a  <- mxEval(top.a, fit); # Path coefficients
		c  <- mxEval(top.c, fit);
		e  <- mxEval(top.e, fit);
		A  <- mxEval(top.A, fit); # Variances
		C  <- mxEval(top.C, fit);
		E  <- mxEval(top.E, fit);
		Vtot = A+C+E;             # Total variance
		I  <- diag(nVar); # nVar Identity matrix
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
		aClean[upper.tri(aClean)]=NA
		cClean[upper.tri(cClean)]=NA
		eClean[upper.tri(eClean)]=NA
		Estimates = data.frame(cbind(aClean,cClean,eClean), row.names=selDVs[1:nVar]);
		names(Estimates) = paste(rep(c("a", "c", "e"), each = nVar), rep(1:nVar), sep = "");
		print.dataframe(Estimates, digits = accuracy, zero.print = ".") # this function is created in genEpi.lib
		if(extended==TRUE) {
			message("Unstandardized path coefficients")
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)]=NA
			cClean[upper.tri(cClean)]=NA
			eClean[upper.tri(eClean)]=NA
			unStandardizedEstimates = data.frame(cbind(aClean,cClean,eClean), row.names=selDVs[1:nVar]);
			names(unStandardizedEstimates) = paste(rep(c("a", "c", "e"), each=nVar), rep(1:nVar), sep="");
			print.dataframe(unStandardizedEstimates, digits=accuracy, zero.print = ".")
		}

		# Pre & post multiply covariance matrix by inverse of standard deviations
		if(showRg) {
			message("Genetic correlations")
			NAmatrix <- matrix(NA, nVar, nVar);
			rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error=function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error=function(err) return(NAmatrix)); # shared environmental correlations
			rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error=function(err) return(NAmatrix)); # Unique environmental correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)]=NA
			rCClean[upper.tri(rCClean)]=NA
			rEClean[upper.tri(rEClean)]=NA
			genetic_correlations  = data.frame(cbind(rAClean, rCClean, rEClean), row.names=selDVs[1:nVar] );
			names(genetic_correlations)<-selDVs[1:nVar]
		 	# Make a nice-ish table
			names(genetic_correlations)= paste(rep(c("rA", "rC", "rE"), each=nVar), rep(1:nVar), sep="");
			print.dataframe(genetic_correlations, digits=accuracy, zero.print = ".")
		}
		stdFit = fit
		if(CIs) {
			# TODO Need to refactor this into some function calls...
			if(all(dim(fit@output$confidenceIntervals) == c(0,2))){
				message("You requested me to print out CIs, but there are none...\n
Perhaps you'd like to add 'addStd = T' to your makeACE_2Group() call?")
			} else {
				message("Computing CI-based diagram!")
				# get the lower and uppper CIs as a dataframe
				CIlist = data.frame(fit@output$confidenceIntervals)
				# Drop rows fixed to zero
				CIlist = CIlist[(CIlist$lbound!=0 & CIlist$ubound!=0),]

				# These can be names ("top.a_std[1,1]") or labels ("a11")
				# imxEvalByName finds them both
				outList = c();
				for(aName in row.names(CIlist)) {
					outList <- append(outList, imxEvalByName(aName,fit))
				}
				# Add estimates into the CIlist
				CIlist$estimate = outList
				# reorder to match summary
				CIlist <- CIlist[,c("lbound","estimate", "ubound")] 
				CIlist$fullName = row.names(CIlist)
				# Initialise empty matrices for the standardized results
				rows = dim(fit@submodels$top@matrices$a@labels)[1]
				cols = dim(fit@submodels$top@matrices$a@labels)[2]
				a_std = c_std = e_std= matrix(NA, rows, cols)

				# iterate over each CI
				labelList = imxGenerateLabels(fit)			
				rowCount = dim(CIlist)[1]

				for(n in 1:rowCount) { # n=1
					thisName = row.names(CIlist)[n] # thisName = "a11"
					if(!hasSquareBrackets(thisName)) {
						# upregulate to a bracket name
						nameParts = labelList[which(row.names(labelList)==thisName),]
						CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep="")
					}
					fullName = CIlist$fullName[n]

					thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
					thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
					thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
					CIparts = round(CIlist[n, c("estimate", "lbound", "ubound")], 2)
					thisString = paste(CIparts[1], " (",CIparts[2], ":",CIparts[3], ")", sep="")
					# print(list(CIlist,labelList,rowCount,fullName,thisMatrixName))

					if(grepl("^a", thisMatrixName)) {
						a_std[thisMatrixRow, thisMatrixCol] = thisString
					} else if(grepl("^c", thisMatrixName)){
						c_std[thisMatrixRow, thisMatrixCol] = thisString
					} else if(grepl("^e", thisMatrixName)){
						e_std[thisMatrixRow, thisMatrixCol] = thisString
					} else{
						stop(paste("illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
					}
				}
				print(a_std)
				print(c_std)
				print(e_std)
			}
		} #use CIs
		stdFit@submodels$top@matrices$a@values = a_std
		stdFit@submodels$top@matrices$c@values = c_std
		stdFit@submodels$top@matrices$e@values = e_std
		if(!is.null(dotFilename)) {
			message("making dot file")
			if(showStd){
				# TODO import this function...
				graphViz_Cholesky(stdFit, selDVs, dotFilename)
			}else{
				graphViz_Cholesky(fit, selDVs, dotFilename)
			}
		}
		if(returnStd) {
			return(stdFit)
		}

		# MZc = mxEval(MZ.expCov,  fit);
		# DZc = mxEval(DZ.expCov,  fit);
		# M   = mxEval(MZ.expMean, fit);
	}
}

# ===========
# = Utility =
# ===========
#' umxJiggle
#'
#' umxJiggle takes values in a matrix and jiggles them
#'
#' @param matrixIn an \code{\link{mxMatrix}} to jiggle the values of
#' @param mean the mean value to add to each value
#' @param sd the sd of the jiggle noise
#' @param dontTouch A value, which, if found, will be left as-is (defaults to 0)
#' @return - \code{\link{mxMatrix}}
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @export
#' @examples
#' \dontrun{
#' mat1 = umxJiggle(mat1)
#' }

umxJiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {
	mask = (matrixIn != dontTouch);
	newValues = mask;
	matrixIn[mask == TRUE] = matrixIn[mask == TRUE] + rnorm(length(mask[mask == TRUE]), mean = mean, sd = sd);
	return (matrixIn);
}


umxCheck <- function(fit1){
	# are all the manifests in paths?
	# do the manifests have residuals?
	if(any(duplicated(fit1@manifestVars))){
		stop(paste("manifestVars contains duplicates:", duplicated(fit1@manifestVars)))
	}
	if(length(fit1@latentVars) == 0){
		# Check none are duplicates, none in manifests
		if(any(duplicated(fit1@latentVars))){
			stop(paste("latentVars contains duplicates:", duplicated(fit1@latentVars)))
		}
		if(any(duplicated(c(fit1@manifestVars,fit1@latentVars)))){
			stop(
				paste("manifest and latent lists contain clashing names:", duplicated(c(fit1@manifestVars,fit1@latentVars)))
			)
		}
	}
	# Check manifests in dataframe
}


# ====================
# = Parallel Helpers =
# ====================

eddie_AddCIbyNumber <- function(model, labelRegex = "") {
	# eddie_AddCIbyNumber(model, labelRegex="[ace][1-9]")
	args     = commandArgs(trailingOnly=TRUE)
	CInumber = as.numeric(args[1]); # get the 1st argument from the cmdline arguments (this is called from a script)
	CIlist   = umxGetLabels(model ,regex= "[ace][0-9]", verbose=F)
	thisCI   = CIlist[CInumber]
	model    = mxModel(model, mxCI(thisCI) )
	return (model)
}