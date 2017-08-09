# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# ========================================
# = Not used directly by users =
# ========================================

xmu_safe_summary <- function(model1, model2, summary = TRUE) {
	# model = mxRun(model)
	tryCatch({
		umxSummary(model1)
		umxCompare(model1, model2)
	}, warning = function(w) {
		message("Warning incurred trying to run summary ")
		message(w)
	}, error = function(e) {
		message("Error incurred trying to run summary ")
		message(e)
	})
}

#' xmu_check_levels_identical
#'
#' Just checks that the factor levels for twins 1 and 2 are the same
#'
#' @param df data.frame containing the data
#' @param selDVs base names of variables (without suffixes)
#' @param sep text-constant separating base variable names the twin index (1:2)
#' @param action if unequal levels found:  c("stop", "ignore")
#' @return - 
#' @export
#' @family xmu internal not for end user
#' @examples
#' require(umx)
#' data(twinData)
#' baseNames = c("bmi")
#' selDVs = umx_paste_names(baseNames, "", 1:2)
#' tmp = twinData[, selDVs]
#' tmp$bmi1[tmp$bmi1 <= 22] = 22
#' tmp$bmi2[tmp$bmi2 <= 22] = 22
#' xmu_check_levels_identical(umxFactor(tmp, sep = ""), selDVs = baseNames, sep = "")
#' \dontrun{
#' xmu_check_levels_identical(umxFactor(tmp), selDVs = baseNames, sep = "")
#' }
xmu_check_levels_identical <- function(df, selDVs, sep, action = c("stop", "ignore")){
	n = umx_explode_twin_names(df, sep)
	baseNames   = n$baseNames
	sep         = n$sep
	twinIndexes = n$twinIndexes
	selVars     = umx_paste_names(selDVs, sep = sep, suffixes = twinIndexes)
	umx_check_names(selVars, data = df, die = TRUE)
	nSib = length(twinIndexes)
	if(nSib != 2){
		stop("Sorry, Ask tim to implement handling more than two sibs")
	}
	allIdentical = TRUE
	for (thisVar in selDVs) {
		a = levels(df[,paste0(thisVar, sep, twinIndexes[1])])
		b = levels(df[,paste0(thisVar, sep, twinIndexes[2])])
		if(!identical(a, b)){
			if(action =="stop"){
				allIdentical = FALSE
				stop("levels of ", thisVar, " not identical for twin 1 and twin 2")
			} else {
				# alt.expr
			}
		}
	}
	return(allIdentical)
}

#' xmuLabel_MATRIX_Model (not a user function)
#'
#' This function will label all the free parameters in a (non-RAM) OpenMx \code{\link{mxModel}}
#' nb: We don't assume what each matrix is for. Instead, the function just sticks labels like "a_r1c1" into each cell
#' i.e., matrixname _ r rowNumber c colNumber
#' 
#' End users should just call \code{\link{umxLabel}}
#' 
#'
#' @param model a matrix-style mxModel to label
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @return - The labeled \code{\link{mxModel}}
#' @family xmu internal not for end user
#' @export
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' m2 <- mxModel("One Factor",
#' 	mxMatrix("Full", 5, 1, values = 0.2, free = TRUE, name = "A"), 
#' 	mxMatrix("Symm", 1, 1, values = 1, free = FALSE, name = "L"), 
#' 	mxMatrix("Diag", 5, 5, values = 1, free = TRUE, name = "U"), 
#' 	mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
#' 	mxExpectationNormal("R", dimnames = names(demoOneFactor)),
#' 	mxFitFunctionML(),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m3 = umx:::xmuLabel_MATRIX_Model(m2)
#' m4 = umx:::xmuLabel_MATRIX_Model(m2, suffix = "male")
#' # explore these with omxGetParameters(m4)
xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = TRUE) {
	if(!umx_is_MxModel(model) ){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (umx_is_RAM(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "", verbose = verbose)
	return(model)
}

#' xmuLabel_RAM_Model (not a user function)
#'
#' This function will label all the free parameters in a RAM \code{\link{mxModel}}
#' 
#' End users should just call \code{\link{umxLabel}}
#'
#' @param model a RAM mxModel to label
#' @param suffix a string to append to each label
#' @param labelFixedCells Whether to labelFixedCells (Default TRUE)
#' @param overRideExisting Whether to overRideExisting (Default FALSE)
#' @param verbose how much feedback to give
#' @param name Add optional name parameter to rename returned model (default = leave it along)
#' @return - The labeled \code{\link{mxModel}}
#' @family xmu internal not for end user
#' @export
xmuLabel_RAM_Model <- function(model, suffix = "", labelFixedCells = TRUE, overRideExisting = FALSE, verbose = FALSE, name = NULL) {
	if (!umx_is_RAM(model)) {
		stop("'model' must be an OpenMx RAM Model")
	}
	freeA  = model$A$free
	namesA = dimnames(freeA)[[1]]

	freeS  = model$S$free
	namesS = dimnames(freeS)[[1]]

	# =========================
	# = Add asymmetric labels =
	# =========================
	theseNames = namesA
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeA[toRow, fromCol]){
			   thisLabel = paste0(theseNames[fromCol], "_to_", theseNames[toRow], suffix)			 	
				if(overRideExisting | is.na(model$A$labels[toRow,fromCol])){
					model$A$labels[toRow,fromCol] = thisLabel
			 	}
			}
		}
	}

	# =========================
	# = Add Symmetric labels =
	# =========================
	# Bivariate names are sorted alphabetically, makes it unambiguous...
	theseNames = namesS
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeS[toRow, fromCol]) {
			   orderedNames = sort(c(theseNames[fromCol], theseNames[toRow]))
			   thisLabel = paste0(orderedNames[1], "_with_", orderedNames[2], suffix)
 				if(overRideExisting | is.na(model$S$labels[toRow,fromCol])){
			   	 model$S$labels[toRow,fromCol] = thisLabel
 			 	}
			}
		}
	}
	model$S$labels[lower.tri(model$S$labels)] = t(model$S$labels[upper.tri(t(model$S$labels))])
	toGet = model$S$labels
	transpose_toGet = t(toGet)
	model$S$labels[lower.tri(toGet)] = transpose_toGet[lower.tri(transpose_toGet)]

	# ==============================
	# = Add means labels if needed =
	# ==============================
	# TODO add a test case with raw data but no means...
	if(!is.null(model$data)){
		if(model$data$type == "raw" & is.null(model$M)) {
			message("You are using raw data, but have not yet added paths for the means\n")
			message("Do this with umxPath(means = 'var')")
		}
	}
	if(!is.null(model$M)){
		meanLabels = paste0("one_to_", colnames(model$M$values), suffix)
		if(overRideExisting){
			model$M$labels[] = meanLabels
	 	}else{
			model$M$labels[is.na(model$M$labels)] = meanLabels[is.na(model$M$labels)]
	 	}
	}
	if(!is.null(name)){
		model = mxModel(model, name= name)
	}
	return(model)
}

#' xmuLabel_Matrix (not a user function)
#'
#' This function will label all the free parameters in an \code{\link{mxMatrix}}
#' 
#' End users should just call \code{\link{umxLabel}}
#'
#' Purpose: label the cells of an mxMatrix
#' Detail: Defaults to the handy "matrixname_r1c1" where 1 is the row or column
#' Use case: You should not use this: call umxLabel
#' umx:::xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 3, 3, values = 1, name = "a", byrow = TRUE));
#' umx:::xmuLabel_Matrix(mxMatrix("Symm" , 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"));
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"), overRideExisting = TRUE);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "test"), overRideExisting = TRUE);
#' See also: fit2 = omxSetParameters(fit1, labels = "a_r1c1", free = FALSE, value = 0, name = "drop_a_row1_c1")
#' 
#' @param mx_matrix an mxMatrix
#' @param baseName A base name for the labels NA
#' @param setfree Whether to set free cells FALSE
#' @param drop What values to drop 0
#' @param jiggle = whether to jiggle start values
#' @param boundDiag set diagonal element lbounds to this numeric value (default = NA = ignore) 
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @param labelFixedCells = FALSE
#' @param overRideExisting Whether to overRideExisting (Default FALSE)
#' @return - The labeled \code{\link{mxMatrix}}
#' @family xmu internal not for end user
#' @export
xmuLabel_Matrix <- function(mx_matrix = NA, baseName = NA, setfree = FALSE, drop = 0, jiggle = NA, boundDiag = NA, suffix = "", verbose = TRUE, labelFixedCells = FALSE, overRideExisting = FALSE) {
	if (!is(mx_matrix, "MxMatrix")){ # label a mxMatrix
		stop("I'm sorry Dave... xmuLabel_Matrix works on mxMatrix. You passed an ", class(mx_matrix), ". And why are you calling xmuLabel_Matrix() anyhow? You want umxLabel()")
	}
	type = class(mx_matrix)[1]; # Diag Full  Lower Stand Sdiag Symm Iden Unit Zero
	nrows = nrow(mx_matrix);
	ncols = ncol(mx_matrix);
	newLabels    = mx_matrix$labels;
	mirrorLabels = newLabels
	if(is.na(baseName)) { 
		baseName = mx_matrix$name
	}
	if(suffix != "") {
		baseName = paste(baseName, suffix, sep = "_")
	}

	if(any(grep("^data\\.", newLabels)) ) {
		if(verbose){
			message("matrix ", mx_matrix$name, " contains definition variables in the labels already... I'm leaving them alone")
		}
	}

	# Make a matrix of labels in the form "baseName_rRcC"	
	for (r in 1:nrows) {
		for (c in 1:ncols) {		
			if(grepl("^data\\.", newLabels[r, c])){
				# definition variable, leave it alone
			} else if (overRideExisting | is.na(newLabels[r, c])){
				newLabels[r, c] = paste0(baseName, "_r", r, "c", c)
				if(nrows == ncols) {
					# Used below if needed.
					# Should include all square forms type == "StandMatrix" | type == "SymmMatrix"
					mirrorLabels[c,r] = paste0(baseName, "_r", r, "c", c)
				}
			}			
		}
	}

	if(type == "DiagMatrix"){
		newLabels[lower.tri(newLabels, diag = FALSE)] = NA
		newLabels[upper.tri(newLabels, diag = FALSE)] = NA
	} else if(type == "FullMatrix"){
		# newLabels = newLabels
	} else if(type == "LowerMatrix"){
		newLabels[upper.tri(newLabels, diag = FALSE)] = NA 
	} else if(type == "SdiagMatrix"){
		newLabels[upper.tri(newLabels, diag = TRUE)] = NA
	} else if(type == "SymmMatrix"){
		newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = FALSE)] <- mirrorLabels[upper.tri(mirrorLabels, diag = FALSE)]
	} else if(type == "StandMatrix") {
		newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = FALSE)] <- mirrorLabels[upper.tri(mirrorLabels, diag = FALSE)]
		diag(newLabels) <- NA
	} else if(type == "IdenMatrix" | type == "UnitMatrix" | type == "ZeroMatrix") {
		# message("umxLabel Ignored ", type, " matrix ", mx_matrix$name, " - it has no free values!")
		return(mx_matrix)
	} else {
		return(paste0("You tried to set type ", "to ", omxQuotes(type)));
	}
	# Set labels
	mx_matrix$labels <- newLabels;
	if(setfree == FALSE) {
		# return("Matrix Specification not used: leave free as set in mx_matrix") 
	} else {
		newFree = mx_matrix$free
		# return(newFree)
		newFree[mx_matrix$values == drop] = FALSE;
		newFree[mx_matrix$values != drop] = TRUE;
		if(type=="StandMatrix") {
			newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
			newLabels[upper.tri(newLabels, diag = FALSE)] <- lower.labels;
		} else {
			mx_matrix$free <- newFree
		}
		# newFree[is.na(newLabels)]=NA; # (validated by mxMatrix???)
	}
	if(!is.na(jiggle)){
		mx_matrix$values <- umxJiggle(mx_matrix$values, mean = 0, sd = jiggle, dontTouch = drop) # Expecting sd
	}
	# TODO this might want something to equate values after jiggling around equal labels?
	if(!is.na(boundDiag)){
		diag(mx_matrix$lbound) <- boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

#' xmuMakeThresholdsMatrices (not a user function)
#'
#' You should not be calling this directly.
#' This is not as reliable a strategy and likely to be superceeded...
#'
#' @param df a \code{\link{data.frame}} containing the data for your \code{\link{mxData}} statement
#' @param droplevels a binary asking if empty levels should be dropped (defaults to FALSE)
#' @param verbose how much feedback to give (defaults to FALSE)
#' @return - a list containing an \code{\link{mxMatrix}} called "thresh", 
#' an \code{\link{mxRAMObjective}} object, and an \code{\link{mxData}} object
#' @references - \url{http://tbates.github.io}
#' @examples
#' # x = mtcars
#' # x$cyl = mxFactor(x$cyl, levels = c(4,6,8))
#' # umx:::xmuMakeThresholdsMatrices(df = x, droplevels=FALSE, verbose= TRUE)
xmuMakeThresholdsMatrices <- function(df, droplevels = FALSE, verbose = FALSE) {
	# TODO delete this function??
	isOrdinalVariable = umx_is_ordered(df) 
	if(sum(isOrdinalVariable) == 0){
		stop("no ordinal variables found")
	}
	ordinalColumns = df[,isOrdinalVariable, drop = FALSE]
	nOrdinal = ncol(ordinalColumns);
	ordNameList = names(ordinalColumns);
	levelList   = 1:nOrdinal
	for(n in 1:nOrdinal){
		levelList[n] = nlevels(ordinalColumns[,n])
	}
	maxThreshMinus1 = max(levelList)-1
	threshValues = c() # initialise values 
	for(n in 1:nOrdinal){
		thisLen = levelList[n] -1
		lim = 1.5 # (thisLen/2)
		newValues = seq(from = (-lim), to = (lim), length = thisLen)
		if(thisLen < maxThreshMinus1){
			newValues = c(newValues, rep(NA,times=maxThreshMinus1-thisLen))
		}
		threshValues = c(threshValues, newValues)
		# threshLbounds[j] <- .001
	}

	threshNames = paste0("Threshold", 1:maxThreshMinus1)
	thresh = mxMatrix("Full", name = "thresh", nrow = maxThreshMinus1, ncol = nOrdinal, byrow = FALSE, free = TRUE, values = threshValues, dimnames = list(threshNames,ordNameList))

	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(
		thresh,
		mxRAMObjective(A = "A", S = "S", F = "F", M = "M", thresholds = "thresh"), 
		mxData(df, type = "raw")
		)
	)
}

#' Make deviation threshold matrics
#'
#' Purpose: return a mxRAMObjective(A = "A", S = "S", F = "F", M = "M", thresholds = "thresh"), mxData(df, type = "raw")
#' usecase see: umxMakeThresholdMatrix
#'
#' @param df a dataframe
#' @param droplevels whether to droplevels or not
#' @param verbose how verbose to be
#' @return - list of matrices
#' @export
#' @family xmu internal not for end user
xmuMakeDeviationThresholdsMatrices <- function(df, droplevels, verbose) {
	# TODO delete this function??
	isOrdinalVariable = umx_is_ordered(df) 
	if(sum(isOrdinalVariable) == 0){
		stop("no ordinal variables found")
	}
	ordinalColumns = df[,isOrdinalVariable, drop = FALSE]
	nOrdinal = ncol(ordinalColumns);
	ordNameList = names(ordinalColumns);
	levelList = rep(NA, nOrdinal)
	for(n in 1:nOrdinal) {
		levelList[n] = nlevels(ordinalColumns[, n])
	}
	maxThreshMinus1 = max(levelList) - 1
	# For Multiplication
	lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThreshMinus1, ncol = maxThreshMinus1, free = FALSE, values = 1)
	# Threshold deviation matrix
	deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full", nrow = maxThreshMinus1, ncol = nOrdinal)
	initialLowerLim  = -1
	initialUpperLim  =  1
	# Fill first row of deviations_for_thresh with useful lower thresholds, perhaps -1 or .5 SD (nthresh/2)
	deviations_for_thresh$free[1,]   <- TRUE
	deviations_for_thresh$values[1,] <- initialLowerLim # Start with an even -2. Might spread this a bit for different levels, or centre on 0 for 1 threshold
	deviations_for_thresh$labels[1,] <- paste("ThreshBaseline1", 1:nOrdinal, sep ="_")
	deviations_for_thresh$lbound[1,] <- -7 # baseline limit in SDs
	deviations_for_thresh$ubound[1,] <-  7 # baseline limit in SDs

	for(n in 1:nOrdinal){
		thisThreshMinus1 = levelList[n] -1
		stepSize = (initialUpperLim-initialLowerLim)/thisThreshMinus1
		deviations_for_thresh$values[2:thisThreshMinus1,n] = (initialUpperLim - initialLowerLim) / thisThreshMinus1
		deviations_for_thresh$labels[2:thisThreshMinus1,n] = paste("ThreshDeviation", 2:thisThreshMinus1, n, sep = "_")
		deviations_for_thresh$free  [2:thisThreshMinus1,n] = TRUE
		deviations_for_thresh$lbound[2:thisThreshMinus1,n] = .001
		if(thisThreshMinus1 < maxThreshMinus1) {
			# pad the shorter var's excess rows with fixed@99 so humans can see them...
			deviations_for_thresh$values[(thisThreshMinus1+1):maxThreshMinus1,n] <- (-99)

			deviations_for_thresh$labels[(thisThreshMinus1+1):maxThreshMinus1,n] <- paste("unusedThresh", min(thisThreshMinus1 + 1, maxThreshMinus1), n, sep = "_")
			deviations_for_thresh$free  [(thisThreshMinus1+1):maxThreshMinus1,n] <- F
		}
	}

	threshNames = paste0("Threshold", 1:maxThreshMinus1)
	thresholdsAlgebra = mxAlgebra(lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = list(threshNames, ordNameList), name = "thresholdsAlgebra")
	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra, mxRAMObjective(A="A", S="S", F="F", M="M", thresholds = "thresholdsAlgebra"), mxData(df, type = "raw")))
}


#' Make start values
#'
#' Purpose: Create startvalues for OpenMx paths
#' use cases
#' umx:::xmuStart_value_list(1)
#' umxValues(1) # 1 value, varying around 1, with sd of .1
#' umxValues(1, n=letters) # length(letters) start values, with mean 1 and sd .1
#' umxValues(100, 15)  # 1 start, with mean 100 and sd 15
#'
#' @param mean the mean start value
#' @param sd the sd of values
#' @param n how many to generate
#' @return - start value list
#' @export
#' @family xmu internal not for end user
xmu_start_value_list <- function(mean = 1, sd = NA, n = 1) {
	# TODO: handle connection style
	# nb: bivariate length = n-1 recursive 1=0, 2=1, 3=3, 4=7 i.e., 
	if(is.na(sd)){
		sd = mean/6.6
	}
	if(length(n) > 1){
		n = length(n)
	}
	return(rnorm(n = n, mean = mean, sd = sd))
}

#' xmuPropagateLabels (not a user function)
#'
#' You should be calling \code{\link{umxLabel}}.
#' This function is called by xmuLabel_MATRIX_Model
#'
#' @param model a model to label
#' @param suffix a string to append to each label
#' @param verbose whether to say what is being done
#' @return - \code{\link{mxModel}}
#' @export
#' @family xmu internal not for end user
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
#' m1 = umx:::xmuPropagateLabels(m1, suffix = "MZ")
xmuPropagateLabels <- function(model, suffix = "", verbose = TRUE) {
	model@matrices  <- lapply(model$matrices , xmuLabel_Matrix   , suffix = suffix, verbose = verbose)
	model@submodels <- lapply(model$submodels, xmuPropagateLabels, suffix = suffix, verbose = verbose)
	return(model)
}

#' xmuMI
#'
#' A function to compute and report modifications which would improve fit.
#' You will probably use \code{\link{umxMI}} instead
#'
#' @param model an \code{\link{mxModel}} to derive modification indices for
#' @param vector = Whether to report the results as a vector default = TRUE
#' @family xmu internal not for end user
#' @export
xmuMI <- function(model, vector = TRUE) {
	# modification indices
	# v0.9: written Michael Culbertson
	# v0.91: up on github; added progress bar, Bates
	# http://openmx.ssri.psu.edu/thread/831
	# http://openmx.ssri.psu.edu/thread/1019
	# http://openmx.ssri.psu.edu/sites/default/files/mi-new.r
	steps <- 5
	bar <- txtProgressBar (min=0, max=steps, style=3)
    utils::setTxtProgressBar(bar, 1)
	accumulate <- function(A, B, C, D, d) {
		res <- matrix(0, d^2, d^2)    
		for (ii in 1:(d^2)){
			for (jj in ii:(d^2)){
				g <- 1 + (ii - 1) %% d
				h <- 1 + (ii - 1) %/% d
				i <- 1 + (jj - 1) %% d
				j <- 1 + (jj - 1) %/% d
				res[ii, jj] <- res[jj, ii] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
		}
		res
	}
	accumulate.asym <- function(A, B, C, D, d) {
		res <- matrix(0, d^2, d^2)    
		for (ii in 1:(d^2)){
			for (jj in 1:(d^2)){
				g <- 1 + (ii - 1) %% d
				h <- 1 + (ii - 1) %/% d
				i <- 1 + (jj - 1) %% d
				j <- 1 + (jj - 1) %/% d
				res[ii, jj] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
		}
		res
	}
	A <- model$A$values
	P <- model$S$values
	S <- model$data$observed
	J <- model$F$values
	m <- dim(A)[1]
	which.free <- c(model$A$free, model$S$free & upper.tri(diag(m), diag= TRUE))
	vars       <- colnames(A)
	parNames   <- c(model$A$labels, model$S$labels)
	parNames[is.na(parNames)] <- c(outer(vars, vars, paste, sep=' <- '),
	outer(vars, vars, paste, sep=' <-> '))[is.na(parNames)]
	NM     <- model$data$numObs - 1
	I.Ainv <- solve(diag(m) - A) 
	C      <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
	Cinv   <- solve(C)    
	AA     <- t(I.Ainv) %*% t(J)
	BB     <- J %*% I.Ainv %*% P %*% t(I.Ainv)
	correct <- matrix(2, m, m)
	diag(correct) <- 1
	grad.P <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% t(AA)
	grad.A <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% BB 
	grad   <- c(grad.A, grad.P) * NM
	names(grad) <- parNames
	dF.dBdB <- accumulate(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% BB, AA %*% Cinv %*% BB, t(BB) %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 2)
	dF.dPdP <- accumulate(AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 3)
	dF.dBdP <- accumulate.asym(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 4)
	correct.BB <- correct.PP <- correct.BP <- matrix(1, m^2, m^2)
	correct.BB[diag(m)==0, diag(m)==0] <- 2
	correct.PP[diag(m)==1, diag(m)==1] <- 0.5
	correct.PP[diag(m)==0, diag(m)==0] <- 2
	correct.BP[diag(m)==0, diag(m)==0] <- 2
	Hessian <- NM*rbind(cbind(dF.dBdB * correct.BB,    dF.dBdP * correct.BP),
	cbind(t(dF.dBdP * correct.BP), dF.dPdP * correct.PP))
	rownames(Hessian) <- parNames
	colnames(Hessian) <- parNames
	# list(gradient=grad[which.free], Hessian[which.free, which.free])

	hessian <- Hessian[which.free, which.free]
	E.inv <- solve(hessian)
	par.change <- mod.indices <- rep(0, 2*(m^2))                
	for (i in 1:(2*(m^2))) {
		k <- Hessian[i, i]
		d <- Hessian[i, which.free]
		par.change[i]  <- (-grad[i] / (k - d %*% E.inv %*% d))
		mod.indices[i] <- (-0.5 * grad[i] * par.change[i])
	}
	names(mod.indices) <- parNames
	names(par.change)  <- parNames
	if (vector) {
		which.ret <- c(!model$A$free & !diag(m), !model$S$free) # & upper.tri(diag(m), diag= TRUE))
		sel <- order(mod.indices[which.ret], decreasing= TRUE)
		ret <- list(mi=mod.indices[which.ret][sel], par.change=par.change[which.ret][sel])
	} else {
		mod.A <- matrix(mod.indices[1:(m^2)]   , m, m)
		mod.P <- matrix(mod.indices[-(1:(m^2))], m, m)
		par.A <- matrix(par.change[1:(m^2)]    , m, m)
		par.P <- matrix(par.change[-(1:(m^2))] , m, m)
		rownames(mod.A) <- colnames(mod.A) <- vars
		rownames(mod.P) <- colnames(mod.P) <- vars
		rownames(par.A) <- colnames(par.A) <- vars
		rownames(par.P) <- colnames(par.P) <- vars
		mod.A[model$A$free] <- NA
		par.A[model$A$free] <- NA
		diag(mod.A) <- NA
		diag(par.A) <- NA
		mod.P[model$S$free] <- NA
		par.P[model$S$free] <- NA
		ret <- list(mod.A=mod.A, par.A=par.A, mod.S=mod.P, par.S=par.P)
	}
    utils::setTxtProgressBar(bar, 5)
	close(bar)
	return(ret)
}

#' xmuHasSquareBrackets
#'
#' Tests if an input has square brackets
#'
#' @param input an input to test
#' @return - TRUE/FALSE
#' @export
#' @family xmu internal not for end user
#' @examples
#' xmuHasSquareBrackets("A[1,2]")
xmuHasSquareBrackets <- function (input) {
    match1 <- grep("[", input, fixed = TRUE)
    match2 <- grep("]", input, fixed = TRUE)
    return(length(match1) > 0 && length(match2) > 0)
}

# ===================================
# = Ordinal/Threshold Model Helpers =
# ===================================

#' xmuMaxLevels
#'
#' Get the max levels from df
#'
#' @param df Dataframe to search through
#' @param what Either "value" or "name" ( of the max-level column)
#' @return - max number of levels in frame
#' @export
#' @family xmu internal not for end user
#' @examples
#' xmuMaxLevels(mtcars) # NA = no ordinal vars
#' xmuMaxLevels(umxFactor(mtcars))
#' xmuMaxLevels(umxFactor(mtcars), what = "name")
xmuMaxLevels <- function(df, what = c("value", "name")) {
	what = match.arg(what)
	isOrd = umx_is_ordered(df)
	if(any(isOrd)){
		vars = names(df)[isOrd]
		nLevels = rep(NA, length(vars))
		j = 1
		for (i in vars) {
			nLevels[j] = length(levels(df[,i]))
			j = j + 1
		}	
		if(what == "value"){
			return(max(nLevels))
		} else {
			return(names(df)[which.max(nLevels)])
		}
	} else {
		return(NA)
	}
}

#' xmuMinLevels
#'
#' Get the min levels from df
#'
#' @param df Dataframe to search through
#' @param what Either "value" or "name" (of the min-level column)
#' @return - min number of levels in frame
#' @export
#' @family xmu internal not for end user
#' @examples
#' xmuMinLevels(mtcars) # NA = no ordinal vars
#' xmuMinLevels(umxFactor(mtcars))
#' xmuMinLevels(umxFactor(mtcars), what = "name")
xmuMinLevels <- function(df, what = c("value", "name")) {
	what = match.arg(what)
	isOrd = umx_is_ordered(df)
	if(any(isOrd)){
		vars = names(df)[isOrd]
		nLevels = rep(NA, length(vars))
		j = 1
		for (i in vars) {
			nLevels[j] = length(levels(df[,i]))
			j = j + 1
		}
		if(what == "value"){
			return(min(nLevels))
		} else {
			return(names(df)[which.min(nLevels)])
		}
	} else {
		return(NA)
	}
}

# ===============
# = RAM helpers =
# ===============

#' xmuMakeTwoHeadedPathsFromPathList
#'
#' Make two-headed paths
#'
#' @param pathList A pathlist
#' @return - added items
#' @export
#' @family xmu internal not for end user
xmuMakeTwoHeadedPathsFromPathList <- function(pathList) {
	a       = combn(pathList, 2)
	nVar    = dim(a)[2]
	toAdd   = rep(NA, nVar)
	n       = 1
	for (i in 1:nVar) {
		from = a[1,i]
		to   = a[2,i]
		if(match(to, pathList) > match(from, pathList)){
			labelString = paste0(to, "_with_", from)
		} else {
			labelString = paste0(from, "_with_", to)
		}
		toAdd[n] = labelString
		n = n+1
	}
	return(toAdd)
}

#' xmuMakeOneHeadedPathsFromPathList
#'
#' Make one-headed paths
#'
#' @param sourceList A sourceList
#' @param destinationList A destinationList
#' @return - added items
#' @export
#' @family xmu internal not for end user
xmuMakeOneHeadedPathsFromPathList <- function(sourceList, destinationList) {
	toAdd   = rep(NA, length(sourceList) * length(destinationList))
	n       = 1
	for (from in sourceList) {
		for (to in destinationList) {
			labelString = paste0(from, "_to_", to)
			toAdd[n] = labelString
			n = n + 1
		}
	}
	return(toAdd)
}

#' Internal umx function to help plotting graphviz
#'
#' @description
#' Helper to print a digraph to file and open it
#' @param model an \code{\link{mxModel}} to get the name from 
#' @param file either "name" (use model name) or a file name
#' @param digraph graphviz code for a model
#' @return -
#' @family xmu
xmu_dot_maker <- function(model, file, digraph){
	if(!is.na(file)){
		if(file == "name"){
			file = paste0(model$name, ".gv")
		}
		cat(digraph, file = file) # write to file
		if(umx_set_plot_format(silent = TRUE) == "DiagrammeR"){
				# message("attempting plot")
				print(DiagrammeR::DiagrammeR(diagram = file, type = "grViz"))
		} else {
			if(umx_check_OS("OSX")){
				umx_open(file);
			} else if(umx_check_OS("Windows")){
				shell(paste0("dot -Tpdf -O ", shQuote(file)), "cmd.exe");
				umx_open(paste0(file, ".pdf"))
			} else {
				system(paste0("dot -Tpdf -O ", shQuote(file)));
				umx_open(paste0(file, ".pdf"))
			}
			# dot -Tpdf -O yourFilename.gv
			# creates "yourFilename.gv.pdf"
		}
	} else {
		return (cat(digraph));
	}
}

#' xmu_dot_make_residuals (not for end users)
#'
#'
#' @param mxMat An A or S mxMatrix 
#' @param latents Optional list of latents to alter location of circles (defaults to NULL)
#' @param fixed Whether to show fixed values or not
#' @param digits How many digits to report
#' @param resid How to show residuals and variances default is "circle". Other option is "line"
#' @return - list of variance names and variances
#' @export
#' @family xmu internal not for end user
xmu_dot_make_residuals <- function(mxMat, latents = NULL, fixed = TRUE, digits = 2, resid = c("circle", "line")) {
	mxMat_vals   = mxMat$values
	mxMat_free   = mxMat$free
	mxMat_labels = mxMat$labels
	mxMat_rows = dimnames(mxMat_free)[[1]]
	mxMat_cols = dimnames(mxMat_free)[[2]]

	variances = c()
	varianceNames = c()
	for(to in mxMat_rows ) { # rows
		lowerVars  = mxMat_rows[1:match(to, mxMat_rows)]
		for(from in lowerVars) { # columns
			thisPathLabel = mxMat_labels[to, from]
			thisPathFree  = mxMat_free[to, from]
			thisPathVal   = round(mxMat_vals[to, from], digits)

			if(thisPathFree){ prefix = "" } else { prefix = "@" }
			# TODO currently all variances are labeled "a_with_a"
			# Could diversify to "a_with_a", "var_a" & "resid_a"
			if(thisPathFree | (thisPathVal !=0 && fixed)) {
				if((to == from)) {
					if(resid =="circle"){
						# TODO support latents north (see mxGraphviz)
						if(from %in% latents){
							circleString = paste0(from, ' -> ', from, '[label="', prefix, thisPathVal, '", dir=both, headport=n, tailport=n]')
						} else {
							circleString = paste0(from, ' -> ', from, '[label="', prefix, thisPathVal, '", dir=both, headport=s, tailport=s]')
						}
						variances = append(variances, circleString)
					} else if(resid =="line"){
						varianceNames = append(varianceNames, paste0(from, '_var'))
						variances = append(variances, paste0(from, '_var [label="', prefix, thisPathVal, '", shape = plaintext]'))
					}					
				}
			}
		}
	}
	return(list(varianceNames = varianceNames, variances = variances))
}

#' xmu_dot_make_paths (not for end users)
#'
#' Makes graphviz paths
#'
#' @param mxMat An mxMatrix
#' @param stringIn Input string
#' @param heads 1 or 2 arrows (default NULL - you must set this)
#' @param fixed Whether show fixed values or not (defaults to TRUE)
#' @param comment A comment to include
#' @param showResiduals Whether to show residuals
#' @param pathLabels labels
#' @param digits how many digits to report
#' @return - string
#' @export
#' @family xmu internal not for end user
xmu_dot_make_paths <- function(mxMat, stringIn, heads = NULL, fixed = TRUE, comment = "More paths", showResiduals = TRUE, pathLabels = "labels", digits = 2) {
	if(is.null(heads)){
		stop("You must set 'heads' to 1 or 2 (was NULL)")
	}
	if(!heads %in% 1:2){
		stop("You must set 'heads' to 1 or 2: was ", heads)
	}
	mxMat_vals   = mxMat$values
	mxMat_free   = mxMat$free
	mxMat_labels = mxMat$labels
	mxMat_rows = dimnames(mxMat_free)[[1]]
	mxMat_cols = dimnames(mxMat_free)[[2]]
	if(!is.null(comment)){
		stringIn = paste0(stringIn, "\n\t# ", comment, "\n")
	}
	if(heads == 1){
		for(target in mxMat_rows ) {
			for(source in mxMat_cols) {
				thisPathLabel = mxMat_labels[target, source]
				thisPathFree  = mxMat_free[target, source]
				thisPathVal   = round(mxMat_vals[target, source], digits)

				if(thisPathFree){ labelStart = ' [label="' } else { labelStart = ' [label="@' }

				if(thisPathFree | ((fixed & (thisPathVal != 0))) ) {
					stringIn = paste0(stringIn, "\t", source, " -> ", target, labelStart, thisPathVal, '"];\n')
				}else{
					# print(paste0("thisPathFree = ", thisPathFree , "fixed =", fixed, "; thisPathVal = ", thisPathVal, "\n"))
				}
				
			}
		}
	} else {
		# heads = 2
		for(target in mxMat_rows ) { # rows
			lowerVars  = mxMat_rows[1:match(target, mxMat_rows)]
			for(source in lowerVars) { # columns
				thisPathLabel = mxMat_labels[target, source]
				thisPathFree  = mxMat_free[target, source]
				thisPathVal   = round(mxMat_vals[target, source], digits)

				if(thisPathFree){ prefix = "" } else { prefix = "@" }

				if(thisPathFree | ((fixed & (thisPathVal != 0))) ) {
					if(target == source) {
						if(showResiduals){
							stringIn = paste0(stringIn, "\t", source, "_var -> ", target, ";\n")
						}
					} else {
						if(pathLabels == "both"){
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', thisPathLabel, "=", prefix, thisPathVal, "\"];\n")
						} else if(pathLabels == "labels"){
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', thisPathLabel, "\"];\n")
						}else {
							# pathLabels = "none"
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', prefix, thisPathVal, "\"];\n")
						}
					}
				}
			}
		}
	}
	return(stringIn)
}

# handle sem-style strings

xmu_string2path <- function(from) {
	if(!is.null(from)){
		if(length(from) > 1){
			isSEMstyle = grepl("[<>]", x = from[1])	
		} else {
			isSEMstyle = grepl("[<>]", x = from)				
		}
		if(isSEMstyle){
			stop("sem-style string syntax not yet implemented. In the mean time, try the other features, like with, var, means = , fixedAt = , fixFirst = ")

			if("from contains an arrow"){
				# parse into paths
			} else {
				if(!is.null(with)){
					to = with
					arrows = 2
					connect = "single"
				} else {
					to = to
					arrows = 1
					connect = "single"
				}
			}	
			a = "A ->B;A<-B; A>B; A --> B
			A<->B"
			# remove newlines, replacing with ;
			allOneLine = gsub("\n+", ";", a, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?^<-?>[ \t]?", "->", allOneLine, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?-?>[ \t]?", "<-", regularizedArrows, ignore.case = TRUE)

			# TODO remove duplicate ; 
			pathList = umx_explode(";", allOneLine)
			for (aPath in pathList) {
				if(length(umx_explode("<->", aPath))==3){
					# bivariate
					parts = umx_explode("<->", aPath)
					# not finished, obviously...
					mxPath(from = umx_trim(parts[1]))
				} else if(length(umx_explode("->", aPath))==3){
					# from to
				} else if(length(umx_explode("<-", aPath))==3){
					# to from
				}else{
					# bad line
				}
			}
			umx_explode("", a)
		}
	}
}