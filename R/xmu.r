# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); 
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx"); 
# devtools::build("~/bin/umx"); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxFUNCTION_NAME")
# devtools::show_news()
# ========================================
# = Not Typically used directly by users =
# ========================================

#' xmuLabel_MATRIX_Model (not a user function)
#'
#' This function will label all the free parameters in a (non-RAM) OpenMx \code{\link{mxModel}}
#' nb: We don't assume what each matrix is for. Instead, the function just sticks labels like "a_r1c1" into each cell
#' i.e., matrixname _ r rowNumber c colNumber
#'
#' @param model a matrix-style mxModel to label
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @return - The labeled \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxLabel}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m2 <- mxModel("One Factor",
#' 	mxMatrix("Full", 5, 1, values = 0.2, free = T, name = "A"), 
#' 	mxMatrix("Symm", 1, 1, values = 1, free = F, name = "L"), 
#' 	mxMatrix("Diag", 5, 5, values = 1, free = T, name = "U"), 
#' 	mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
#' 	mxMLObjective("R", dimnames = names(demoOneFactor)), 
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m3 = xmuLabel_MATRIX_Model(m2)
#' m4 = xmuLabel_MATRIX_Model(m2, suffix = "male")
#' # explore these with omxGetParameters(m4)

xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = T) {
	if(!umx_is_MxModel(model) ){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (umx_is_RAM(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "")
	return(model)
}

xmuLabel_RAM_Model <- function(model, suffix = "", labelFixedCells = T, overRideExisting = F) {
	# Purpose: to label all the free parameters of a (RAM) model
	# Use case: model = umxAddLabels(model, suffix = "_male")
	# TODO implement overRideExisting !!!
	if (!umx_is_RAM(model)) {
		stop("'model' must be an OpenMx RAM Model")
	}
	freeA  = model@matrices$A@free
	freeS  = model@matrices$S@free
	namesA = dimnames(freeA)[[1]]
	namesS = dimnames(freeS)[[1]]

	# =========================
	# = Add asymmetric labels =
	# =========================
	theseNames = namesA
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeA[toRow, fromCol]){
			   thisLabel = paste(theseNames[fromCol], "_to_", theseNames[toRow], suffix, sep = "")
			   model@matrices$A@labels[toRow,fromCol] = thisLabel
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
			   model@matrices$S@labels[toRow,fromCol] = thisLabel
			}
		}
	}
	model@matrices$S@labels[lower.tri(model@matrices$S@labels)] = t(model@matrices$S@labels[upper.tri(t(model@matrices$S@labels))])
	toGet = model@matrices$S@labels
	transpose_toGet = t(toGet)
	model@matrices$S@labels[lower.tri(toGet)] = transpose_toGet[lower.tri(transpose_toGet)]

	# ==============================
	# = Add means labels if needed =
	# ==============================
	if(model@data@type == "raw"){
		if(is.null(model@matrices$M)){
			msg("You are using raw data, but have not yet added paths for the means\n")
			stop("You do this with mxPath(from = 'one', to = 'var')")
		} else {
			model@matrices$M@labels = matrix(nrow = 1, paste0(colnames(model@matrices$M@values),"_mean", suffix))
		}
	}
	return(model)
}

xmuLabel_Matrix <- function(mx_matrix = NA, baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA, suffix = "", verbose = T, labelFixedCells = F) {
	# Purpose: label the cells of an mxMatrix
	# Detail: Defaults to the handy "matrixname_r1c1" where 1 is the row or column
	# Use case: You shouldn't be using this: called by umxLabel
	# xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = T), jiggle = .05, boundDiag = NA);
	# xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = T), jiggle = .05, boundDiag = NA);
	# See also: fit2 = omxSetParameters(fit1, labels = "a_r1c1", free = F, value = 0, name = "drop_a_row1_c1")
	if (!is(mx_matrix, "MxMatrix")){ # label a mxMatrix
		stop("I'm sorry Dave... xmuLabel_Matrix works on mxMatrix. You passed an ", class(mx_matrix), ". And why are you calling xmuLabel_Matrix() anyhow? You want umxLabel()")
	}
	type = class(mx_matrix)[1]; # Diag Full  Lower Stand Sdiag Symm Iden Unit Zero
	nrow = nrow(mx_matrix);
	ncol = ncol(mx_matrix);
	newLabels    = mx_matrix@labels;
	mirrorLabels = newLabels

	if(is.na(baseName)) { 
		baseName = mx_matrix@name
	}
	if(suffix != "") {
		baseName = paste(baseName, suffix, sep = "_")
	}

	# Make a matrix of labels in the form "baseName_rRcC"
	for (r in 1:nrow) {
		for (c in 1:ncol) {
			newLabels[r,c] = paste(baseName,"_r", r, "c", c, sep = "")
			if(nrow == ncol) { # Should include all square forms type == "StandMatrix" | type == "SymmMatrix"
				mirrorLabels[c,r] = paste(baseName, "_r", r, "c", c, sep = "")
			}
		}
	}
	if(type == "DiagMatrix"){
		newLabels[lower.tri(newLabels, diag = F)] = NA
		newLabels[upper.tri(newLabels, diag = F)] = NA
	} else if(type == "FullMatrix"){
		# newLabels = newLabels
	} else if(type == "LowerMatrix"){
		newLabels[upper.tri(newLabels, diag = F)] = NA 
	} else if(type == "SdiagMatrix"){
		newLabels[upper.tri(newLabels, diag = T)] = NA
	} else if(type == "SymmMatrix"){
		newLabels[lower.tri(newLabels, diag = F)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = F)] <- mirrorLabels[upper.tri(mirrorLabels, diag = F)]
	} else if(type == "StandMatrix") {
		newLabels[lower.tri(newLabels, diag = F)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = F)] <- mirrorLabels[upper.tri(mirrorLabels, diag = F)]
		diag(newLabels) <- NA
	} else if(type == "IdenMatrix" | type == "UnitMatrix" | type == "ZeroMatrix") {
		message("umxLabel Ignored ", type, " matrix ", mx_matrix@name, " - it has no free values!")
		return(mx_matrix)
	} else {
		return(paste("You tried to set type ", "to '", type, "'", sep = ""));
	}
	# Set labels
	mx_matrix@labels <- newLabels;
	if(setfree == FALSE) {
		# return("Matrix Specification not used: leave free as set in mx_matrix") 
	} else {
		newFree = mx_matrix@free
		# return(newFree)
		newFree[mx_matrix@values == drop] = F;
		newFree[mx_matrix@values != drop] = T;
		if(type=="StandMatrix") {
			newLabels[lower.tri(newLabels, diag = F)] -> lower.labels;
			newLabels[upper.tri(newLabels, diag = F)] <- lower.labels;
		} else {
			mx_matrix@free <- newFree
		}
		# newFree[is.na(newLabels)]=NA; # (validated by mxMatrix???)
	}
	if(!is.na(jiggle)){
		mx_matrix@values <- umxJiggle(mx_matrix@values, mean = 0, sd = jiggle, dontTouch = drop) # Expecting sd
	}
	# TODO this might want something to equate values after jiggling around equal labels?
	if(!is.na(boundDiag)){
		diag(mx_matrix@lbound) <- boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

# TODO document xmuMakeDeviationThresholdsMatrices
xmuMakeDeviationThresholdsMatrices <- function(df, droplevels, verbose) {
	# Purpose: return a mxRAMObjective(A = "A", S = "S", F = "F", M = "M", thresholds = "thresh"), mxData(df, type = "raw")
	# usecase see: umxMakeThresholdMatrix
	# junk[1]; 	junk[[2]]@values; 	junk[3]
	isOrdinalVariable = umx::umxIsOrdinalVar(df) 
	ordinalColumns    = df[,isOrdinalVariable]
	ordNameList       = names(ordinalColumns);
	nOrdinal          = ncol(ordinalColumns);

	levelList = rep(NA, nOrdinal)
	for(n in 1:nOrdinal) {
		levelList[n] = nlevels(ordinalColumns[, n])
	}
	maxThreshMinus1 = max(levelList) - 1
	# For Multiplication
	lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThreshMinus1, ncol = maxThreshMinus1, free = F, values = 1)
	# Threshold deviation matrix
	deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full", nrow = maxThreshMinus1, ncol = nOrdinal)
	initialLowerLim  = -1
	initialUpperLim  =  1
	# Fill first row of deviations_for_thresh with useful lower thresholds, perhaps -1 or .5 SD (nthresh/2)
	deviations_for_thresh@free[1,]   <- TRUE
	deviations_for_thresh@values[1,] <- initialLowerLim # Start with an even -2. Might spread this a bit for different levels, or centre on 0 for 1 threshold
	deviations_for_thresh@labels[1,] <- paste("ThreshBaseline1", 1:nOrdinal, sep ="_")
	deviations_for_thresh@lbound[1,] <- -7 # baseline limit in SDs
	deviations_for_thresh@ubound[1,] <-  7 # baseline limit in SDs

	for(n in 1:nOrdinal){
		thisThreshMinus1 = levelList[n] -1
		stepSize = (initialUpperLim-initialLowerLim)/thisThreshMinus1
		deviations_for_thresh@values[2:thisThreshMinus1,n] = (initialUpperLim - initialLowerLim) / thisThreshMinus1
		deviations_for_thresh@labels[2:thisThreshMinus1,n] = paste("ThreshDeviation", 2:thisThreshMinus1, n, sep = "_")
		deviations_for_thresh@free  [2:thisThreshMinus1,n] = TRUE
		deviations_for_thresh@lbound[2:thisThreshMinus1,n] = .001
		if(thisThreshMinus1 < maxThreshMinus1) {
			# pad the shorter var's excess rows with fixed@99 so humans can see them...
			deviations_for_thresh@values[(thisThreshMinus1+1):maxThreshMinus1,n] <- (-99)
			deviations_for_thresh@labels[(thisThreshMinus1+1):maxThreshMinus1,n] <- paste("unusedThresh", min(thisThreshMinus1 + 1, maxThreshMinus1), n, sep = "_")
			deviations_for_thresh@free  [(thisThreshMinus1+1):maxThreshMinus1,n] <- F
		}
	}

	threshNames = paste("Threshold", 1:maxThreshMinus1, sep = '')
	thresholdsAlgebra = mxAlgebra(lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = list(threshNames, ordNameList), name = "thresholdsAlgebra")
	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra, mxRAMObjective(A="A", S="S", F="F", M="M", thresholds = "thresholdsAlgebra"), mxData(df, type = "raw")))
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
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' \dontrun{
#' junk = xmuMakeThresholdsMatrices(df, droplevels=F, verbose=T)
#' }

xmuMakeThresholdsMatrices <- function(df, droplevels = F, verbose = F) {
	isOrdinalVariable = umxIsOrdinalVar(df) 
	ordinalColumns    = df[,isOrdinalVariable]
	nOrdinal          = ncol(ordinalColumns);
	ordNameList       = names(ordinalColumns);
	levelList         = 1:nOrdinal
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

	threshNames = paste("Threshold", 1:maxThreshMinus1, sep='')
	thresh = mxMatrix("Full", name="thresh", nrow = maxThreshMinus1, ncol = nOrdinal, byrow = F, free = T, values= threshValues, dimnames=list(threshNames,ordNameList))

	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(
		thresh, 
		mxRAMObjective(A="A", S="S", F="F", M="M", thresholds="thresh"), 
		mxData(df, type="raw")
		)
	)
}

xmuStart_value_list <- function(mean = 1, sd = NA, n = 1) {
	# Purpose: Create startvalues for OpenMx paths
	# use cases
	# umxStart(1) # 1 value, varying around 1, with sd of .1
	# umxStart(1, n=letters) # length(letters) start values, with mean 1 and sd .1
	# umxStart(100, 15)  # 1 start, with mean 100 and sd 15
	# TODO: handle connection style
	# nb: bivariate length = n-1 recursive 1=0, 2=1, 3=3, 4=7 i.e., 
	if(is.na(sd)){
		sd = x/6.6
	}
	if(length(n) > 1){
		n = length(n)
	}
	return(rnorm(n = n, mean = mean, sd = sd))
}

#' xmuPropagateLabels (not a user function)
#'
#' You should be calling umxLabel.
#' This function is called by xmuLabel_MATRIX_Model
#'
#' @param model a model to label
#' @param suffix a string to append to each label
#' @return - \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}

xmuPropagateLabels <- function(model, suffix = "") {
    # useage: xmuPropagateLabels(model, suffix = "")
	model@matrices  <- lapply(model@matrices , xmuLabel_Matrix, suffix = suffix)
    model@submodels <- lapply(model@submodels, xmuPropagateLabels, suffix = suffix)
    return(model)
}

#' xmuMI
#'
#' A function to compute and report modifications which would improve fit.
#' You will probably use \code{\link{umxMI}} instead
#'
#' @param model an \code{\link{mxModel}} to derive modification indices for
#' @param vector = Whether to report the results as a vector default = TRUE
#' @seealso - \code{\link{umxMI}}, \code{\link{umxAdd1}}, \code{\link{umxDrop1}}, \code{\link{umxRun}}, \code{\link{umxSummary}}
#' @references - 
#' @export
#' @examples
#' \dontrun{
#' xmuMI(model)
#' }

xmuMI <- function(model, vector = T) {
	# modification indices
	# v0.9: written Michael Culbertson
	# v0.91: up on github; added progress bar, Bates
	# http://openmx.psyc.virginia.edu/thread/831
	# http://openmx.psyc.virginia.edu/thread/1019
	# http://openmx.psyc.virginia.edu/sites/default/files/mi-new.r
	steps <- 5
	bar <- txtProgressBar (min=0, max=steps, style=3)
    setTxtProgressBar(bar, 1)
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
	A <- model$A@values
	P <- model$S@values
	S <- model$data@observed
	J <- model$F@values
	m <- dim(A)[1]
	which.free <- c(model$A@free, model$S@free & upper.tri(diag(m), diag=T))
	vars       <- colnames(A)
	parNames   <- c(model$A@labels, model$S@labels)
	parNames[is.na(parNames)] <- c(outer(vars, vars, paste, sep=' <- '),
	outer(vars, vars, paste, sep=' <-> '))[is.na(parNames)]
	NM     <- model$data@numObs - 1
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
    setTxtProgressBar(bar, 2)
	dF.dPdP <- accumulate(AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), m)
    setTxtProgressBar(bar, 3)
	dF.dBdP <- accumulate.asym(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), m)
    setTxtProgressBar(bar, 4)
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
		which.ret <- c(!model$A@free & !diag(m), !model$S@free) # & upper.tri(diag(m), diag=T))
		sel <- order(mod.indices[which.ret], decreasing=T)
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
		mod.A[model$A@free] <- NA
		par.A[model$A@free] <- NA
		diag(mod.A) <- NA
		diag(par.A) <- NA
		mod.P[model$S@free] <- NA
		par.P[model$S@free] <- NA
		ret <- list(mod.A=mod.A, par.A=par.A, mod.S=mod.P, par.S=par.P)
	}
    setTxtProgressBar(bar, 5)
	close(bar)
	return(ret)
}

xmuHasSquareBrackets <- function (input) {
    match1 <- grep("[", input, fixed = TRUE)
    match2 <- grep("]", input, fixed = TRUE)
    return(length(match1) > 0 && length(match2) > 0)
}

# ===================================
# = Ordinal/Threshold Model Helpers =
# ===================================

xmuMaxLevels <- function(data) {
	anyFactors = F
	maxLevels = 0
	vars = names(data)
	for (i in vars) {
		if(is.factor(mzData[,i])){
			nLevels = length(levels(mzData[,i]))
			if(nLevels > maxLevels){
			 	maxLevels = nLevels
				anyFactors = T
			}
		}
	}	
	if(!anyFactors){
		stop("No columns were type factor")
	} else {
		return(maxLevels)
	}
}

xmuMinLevels <- function(data) {
	# TODO add check that some columns have levels
	anyFactors = F
	minLevels = 1e6 # silly high value
	vars = names(data)
	for (i in vars) {
		if(is.factor(mzData[,i])){
			nLevels = length(levels(mzData[,i]))
			if(nLevels < minLevels){
			 	minLevels = nLevels
				anyFactors = T
			}
		}
	}
	if(!anyFactors){
		stop("No columns were type factor")
	} else {
		return(minLevels)
	}
}

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
