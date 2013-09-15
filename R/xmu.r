# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); devtools::document(); devtools::build(); devtools::install(); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxFUNCTION_NAME")
# devtools::show_news()
# ========================================
# = Not Typically used directly by users =
# ========================================

#' xmuLabel_MATRIX_Model (not a user function)
#'
#' This function label all the free parameters in a (non-RAM) OpenMx \code{\link{mxModel}}
#' nb: We don't assume what each matrix is for, and just stick a_r1c1 into each cell
#'
#' @param model a model to label
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @return - \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxLabel}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' \dontrun{
#' model = xmuLabel_MATRIX_Model(model)
#' model = xmuLabel_MATRIX_Model(model, suffix = "male")
#' }

xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = T) {
	if(!umxIsMxModel(model) ){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (!umxIsRAMmodel(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "")
	return(model)
}

xmuLabel_RAM_Model <- function(model, suffix = "", labelFixedCells = T, overRideExisting = F) {
	# Purpose: to label all the free parameters of a (RAM) model
	# Use case: model = umxAddLabels(model, suffix = "_male")
	# TODO label means if data = raw
	# TODO implement overRideExisting !!!
	if (!umxIsRAMmodel(model)) {
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
	theseNames = namesS
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeS[toRow, fromCol]) {
			   thisLabel = paste0(theseNames[fromCol], "_with_", theseNames[toRow], suffix)
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
		model@matrices$M@labels = matrix(nrow = 1, paste0(colnames(model@matrices$M@values),"_mean", suffix))
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
		newLabels[upper.tri(newLabels, diag=F)] <- mirrorLabels[upper.tri(mirrorLabels, diag = F)]
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
		diag(mx_matrix@lbound)<-boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

# TODO document xmuMakeDeviationThresholdsMatrices
xmuMakeDeviationThresholdsMatrices <- function(df, droplevels, verbose) {
	# Purpose: return a mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")
	# usecase see: umxMakeThresholdMatrix
	# junk[1]; 	junk[[2]]@values; 	junk[3]
	isOrdinalVariable = umxIsOrdinalVar(df) 
	ordinalColumns    = df[,isOrdinalVariable]
	nOrdinal          = ncol(ordinalColumns);
	ordNameList       = names(ordinalColumns);
	levelList         = 1:nOrdinal
	for(n in 1:nOrdinal) {
		levelList[n] = nlevels(ordinalColumns[,n])
	}
	maxThreshMinus1 = max(levelList) - 1
	# For Multiplication
	UnitLower = mxMatrix("Lower", name="UnitLower", nrow = maxThreshMinus1, ncol = maxThreshMinus1, free=F, values=1)
	# Threshold deviation matrix
	threshDeviations = mxMatrix("Full", name="threshDeviations", nrow=maxThreshMinus1, ncol=nOrdinal)
	initialLowerLim = -1
	initialUpperLim =  1
	# Fill first row of threshDeviations with useful lower thresholds, perhaps -1 or .5 SD (nthresh/2)

	threshDeviations@free  [1,] <- TRUE
	threshDeviations@values[1,] <- initialLowerLim # Start with an even -2. Might spread this a bit for different levels, or centre on 0 for 1 threshold
	threshDeviations@labels[1,] <- paste("ThreshBaseline1", 1:nOrdinal, sep="_")
	threshDeviations@lbound[1,] <- -7 # baseline limit in SDs
	threshDeviations@ubound[1,] <-  7 # baseline limit in SDs

	for(n in 1:nOrdinal){
		thisThreshMinus1 = levelList[n] -1
		stepSize = (initialUpperLim-initialLowerLim)/thisThreshMinus1
		threshDeviations@values[2:thisThreshMinus1,n] = (initialUpperLim-initialLowerLim)/thisThreshMinus1
		threshDeviations@labels[2:thisThreshMinus1,n] = paste("ThreshDeviation", 2:thisThreshMinus1, n, sep="_")
		threshDeviations@free  [2:thisThreshMinus1,n] = TRUE
		threshDeviations@lbound[2:thisThreshMinus1,n] = .001
		if(thisThreshMinus1 < maxThreshMinus1) {
			# pad the shorter var's excess rows with fixed@99 so humans can see them...
			threshDeviations@values[(thisThreshMinus1+1):maxThreshMinus1,n] <- (-99)
			threshDeviations@labels[(thisThreshMinus1+1):maxThreshMinus1,n] <- paste("unusedThresh", min(thisThreshMinus1+1, maxThreshMinus1), n, sep="_")
			threshDeviations@free  [(thisThreshMinus1+1):maxThreshMinus1,n] <- F
		}
	}

	# thresh = mxMatrix("Full", name="thresh", nrow = maxThreshMinus1, ncol = nOrdinal, byrow = F, free = myFree, values= myThreshValues)
	threshNames = paste("Threshold", 1:maxThreshMinus1, sep='')
	thresh = mxAlgebra(UnitLower %*% threshDeviations, dimnames=list(threshNames,ordNameList), name="thresh")
	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(UnitLower,threshDeviations, thresh, mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")))
}

#' xmuMakeThresholdsMatrices (not a user function)
#'
#' You should not be calling this directly.
#' This is not as a reliable strategy and likely to be superceeded...
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

xmuStart_value_list <- function(x = 1, sd = NA, n = 1) {
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
	if(length(n)>1){
		n = length(n)
	}
	return(rnorm(n=n, mean=x, sd=sd))
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

umxGraph_RAM <- function(model = NA, std = T, precision = 2, dotFilename = "name", pathLabels = "none", showFixed = F, showError = T) {
	stop("Replace umxGraph_RAM with umxPlot (umxGraph_RAM was deprecated to help people learn umx more quickly)")
}
