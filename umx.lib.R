# umx.lib.R
# To USE ME IN YOUR SCRIPTS SAY: 
# source("http://github.com/tbates/umx/blob/master/umx.lib.R")
# source("https://raw.github.com/tbates/umx/master/umx.lib.R")

# To learn more, see http://www.github.com/tbates/umx/README.md

require(RCurl)
url = "https://raw.github.com/tbates/umx/master/umx.lib.R"
#` Read script lines from github
script <- RCurl::getURL(url, ssl.verifypeer = FALSE)
#` parse then evaluate script in the global environement
eval(parse(text = script))
#` Code borrowed from [here](http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github)

umxUpdateOpenMx <-function(bleedingEdge=FALSE, loadNew=TRUE) {
	# update the OpenMx Library to latest version:
	# umxUpdateOpenMx()
	if( "OpenMx" %in% .packages() ){
		detach(package:OpenMx); # unload existing version
	}	
	if (bleedingEdge){
		install.packages('OpenMx', repos='http://openmx.psyc.virginia.edu/testing/');
	} else {
		if (.Platform$OS.type == "windows") {
			if (!is.null(.Platform$r_arch) && .Platform$r_arch == "x64") {
				stop(paste("OpenMx is not yet supported on 64-bit R for Windows.",
				"Please use 32-bit R in the interim."), call. = FALSE)
			}
			repos <- c('http://openmx.psyc.virginia.edu/packages/')
			install.packages(pkgs=c('OpenMx'), repos=repos)
		} else {
			if (Sys.info()["sysname"] == "Darwin") {
				darwinVers <- as.numeric(substr(Sys.info()['release'], 1, 2))
				if (darwinVers > 10) {
					msg <- paste("We have detected that you are running on OS X 10.7 or greater",
					"whose native version of gcc does not support the OpenMP API.", 
					"As a result your default installation has been set to single-threaded.",
					"If you have installed the mac ports version of gcc to address this issue",
					"please choose the multi-threaded installation option.")
					msg <- gsub('(.{1,80})(\\s|$)', '\\1\n', msg)
					cat(msg)
					cat("1. single-threaded [default]\n")
					cat("2. multi-threaded \n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 1
					} 

				} else {
					cat("1. single-threaded\n")
					cat("2. multi-threaded [default]\n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 2
					}
				}
				} else {
					cat("1. single-threaded\n")
					cat("2. multi-threaded [default]\n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 2
					}
				}

				if (!(select %in% c(1,2))) {
					stop("Please enter '1' or '2'", call. = FALSE)
				}
  
				if (select == 1) {
					repos <- c('http://openmx.psyc.virginia.edu/sequential/')
					install.packages(pkgs=c('OpenMx'), repos=repos, 
					configure.args=c('--disable-openmp'))
				} else if (select == 2) {
					repos <- c('http://openmx.psyc.virginia.edu/packages/')
					install.packages(pkgs=c('OpenMx'), repos=repos)
				} else {
					stop(paste("Unknown installation type", select))
				}
		}
	}
	if(loadNew){
		require("OpenMx")	
	}
}


umxStandardizeRAMModel <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {
	# use case
	# standardizeRAM(model, return="parameters|matrices|model")
	# make sure 'return' is valid
	if (!(return=="parameters"|return=="matrices"|return=="model"))stop("Invalid 'return' parameter. Do you want do get back parameters, matrices or model?")
	suppliedNames = all(!is.na(c(Amatrix,Smatrix)))
	# if the objective function isn't RAMObjective, you need to supply Amatrix and Smatrix
	if (class(model@objective)[1] !="MxRAMObjective" & !suppliedNames ){
		stop("I need either mxRAMObjective or the names of the A and S matrices.")
	}
	output <- model@output
	# stop if there is no objective function
	if (is.null(output))stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
	# stop if there is no output
	if (length(output)<1)stop("Provided model has no output. I can only standardize models that have been run!")
	# Get the names of the A, S and M matrices 
	if (is.character(Amatrix)){nameA <- Amatrix} else {nameA <- model@objective@A}
	if (is.character(Smatrix)){nameS <- Smatrix} else {nameS <- model@objective@S}
	if (is.character(Mmatrix)){nameM <- Mmatrix} else {nameM <- model@objective@M}
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

# =====================
# = Reporting Helpers =
# =====================

umxReportFit<-function(model) {
	# use case
	# umxReportFit(fit1)
	with(summary(model),paste(
		"χ2(", degreesOfFreedom, ") = ", round(Chi,2), ", ",
		"p = "    , round(p,2)  , "; ",
		"CFI = "  , round(CFI,3), "; ",
		"TLI = "  , round(TLI,3), "; ",
		"RMSEA = ", round(RMSEA, 3), sep="")
	)
}

umxMI <- function(model, vector=T) {
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

umxMI_top <- function(fit=NA, numInd=10, typeToShow="both") {
	# depends on umxMI(fit)
	# use cases
	# mi.df = umxMI_top(fit)
	# umxMI_top(fit, numInd=5, typeToShow="add") # valid options are "both|add|delete"
	mi = umxMI(fit, vector=T)
	mi.df = data.frame(path= as.character(attributes(mi$mi)$names), value=mi$mi);
	row.names(mi.df) = 1:nrow(mi.df);
	# TODO: could be a helper: choose direction
	mi.df$from = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\1", mi.df$path)
	mi.df$to   = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\3", mi.df$path)
	mi.df$arrows = 1
	mi.df$arrows[grepl("<->", mi.df$path)]= 2		

	mi.df$action = NA 
	mi.df  = mi.df[order(mi.df[,2], decreasing=T),] 
	mi.df$copy = 1:nrow(mi.df)
	for(n in 1:(nrow(mi.df)-1)) {
		if(grepl(" <- ", mi.df$path[n])){
			tmp = mi.df$from[n]; mi.df$from[n] = mi.df$to[n]; mi.df$to[n] = tmp 
		}
		from = mi.df$from[n]
		to   = mi.df$to[n]
		a = (fit@matrices$S@free[to,from] |fit@matrices$A@free[to,from])
		b = (fit@matrices$S@values[to,from]!=0 |fit@matrices$A@values[to,from] !=0)
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
	mi.df = mi.df[unique(mi.df$copy),] # c("copy")
	if(typeToShow!="both"){
		mi.df = mi.df[mi.df$action==typeToShow,]
	}
	print(mi.df[1:numInd, !(names(mi.df) %in% c("path","copy"))])
	invisible(mi.df)
}

# How long did that take?
umxReportTime <- function(model, formatStr= "%H:%M:%S", tz="GMT"){
	# use case
	# umxReportTime(fit1)
	format(.POSIXct(model$wallTime,tz), formatStr)
}

# =================================
# = Speed  and Efficiency Helpers =
# =================================

umxTryHard <- function(model, n=3, calc_SE=F){
	# TODO: add edit history, history of Mx function tryhard
	# optimise for speed
	model = mxOption(model, "Calculate Hessian", "No")
	model = mxOption(model, "Standard Errors", "No")
	# make an initial run
	model = mxRun(model);
	n = n-1
	tries = 0
	# carryon if we failed
	while(model@output$status[[1]] == 6 && n > 2 ) {
		print(paste("Run", tries+1, "status Red(6): Trying hard...", n, "more times."))
		model <- mxRun(model)
		n <- n-1
		tries = tries+1
	}
	if(tries==0){ 
		print("Ran fine first time!")	
	}
	# get the SEs for summary (if requested)
	if(calc_SE){
		print("Calculating Hessian & SEs")
		model = mxOption(model, "Calculate Hessian", "Yes")
		model = mxOption(model, "Standard Errors", "Yes")
		model = mxRun(model)
	}
	return(model)
	# Use case
	# model <- tryHard(model, n=10)
}

# Parallel helpers to be added here

# ========================================
# = Model building and modifying helpers =
# ========================================
#` ## path-oriented helpers

#` ## matrix-oriented helpers

umxLabeler <- function(mx_matrix= NA, baseName=NA, setfree=F, drop=0, jiggle=NA, boundDiag=NA) {
	# Purpose       : label the cells of an mxMatrix
	# Detail        : Defaults to the handy "matname_r1c1" where 1 is the row or column
	# Related calls : fit2 = omxSetParameters(fit1, labels="a_r1c1", free=F, value = 0, name="drop_a_row1_c1")
	# Use case:
	# umxLabeler(mxMatrix("Lower",3, 3, values=1, name="a", byrow=T), jiggle=.05, boundDiag=NA);
	type      = class(mx_matrix)[1]; # Diag Full  Lower Stand Sdiag Symm Iden Unit Zero
	nrow      = nrow(mx_matrix);
	ncol      = ncol(mx_matrix);
	newLabels = mx_matrix@labels;
	mirrorLabels =newLabels
	if(is.na(baseName)) { baseName = mx_matrix@name }
	# Make a matrix of labels in the form baseNameRowNumColNum
	for (r in 1:nrow) {
		for (c in 1:ncol) {
			newLabels[r,c]= paste(baseName,"_r",r,"c",c, sep="")
			if(nrow == ncol) { # Should include all sqaure forms type=="StandMatrix" | type=="SymmMatrix"
				mirrorLabels[c,r]= paste(baseName,r,c, sep="")
			}
		}
	}
	if(type=="DiagMatrix"){
		newLabels[lower.tri(newLabels, diag=F)]=NA
		newLabels[upper.tri(newLabels, diag=F)]=NA
	} else if(type=="FullMatrix"){
		# newLabels = newLabels
	} else if(type=="LowerMatrix"){
		newLabels[upper.tri(newLabels, diag=F)] = NA 
	} else if(type=="SdiagMatrix"){
		newLabels[upper.tri(newLabels, diag=T)] = NA
	} else if(type=="SymmMatrix"){
		newLabels[lower.tri(newLabels, diag=F)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag=F)] <- mirrorLabels[upper.tri(mirrorLabels, diag=F)]
	} else if(type=="StandMatrix") {
		newLabels[lower.tri(newLabels, diag=F)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag=F)] <- mirrorLabels[upper.tri(mirrorLabels, diag=F)]
		diag(newLabels) <- NA
	} else if(type=="IdenMatrix"|type=="UnitMatrix"|type=="ZeroMatrix") {
		stop("you can't run genEpi_Labeler on an Identity matrix - it has no free values!")
	} else {
		return(paste("you tried to set type ", "to '", type, "'", sep=""));
	}
   # Set labels
	mx_matrix@labels <- newLabels;
	if(setfree==FALSE) {
		# return("Specs not used: leave free as set in mx_matrix") 
	} else {
		newFree = mx_matrix@free
		# return(newFree)
		newFree[mx_matrix@values==drop] = F;
		newFree[mx_matrix@values!=drop] = T;
		if(type=="StandMatrix") {
			newLabels[lower.tri(newLabels, diag=FALSE)] -> lower.labels;
			newLabels[upper.tri(newLabels, diag=FALSE)] <- lower.labels;
		} else {
			mx_matrix@free <- newFree
		}
		# newFree[is.na(newLabels)]=NA; # (validated by mxMatrix???)
	}
	if(!is.na(jiggle)){
		mx_matrix@values <- genEpi_Jiggle(mx_matrix@values, mean=0, sd=jiggle, dontTouch=drop) # Expecting sd
	}
	if(!is.na(boundDiag)){
		diag(mx_matrix@lbound)<-boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

# =================
# = Data handling =
# =================

umxHetCor <- function(data, ML=F, use="pairwise.complete.obs"){
	# use case
	# umxHetCor(data, use="pairwise.complete.obs")
	# heplper to return just the correlations from polycor::hetcor
	require(polycor)
	# TODO add error message if polycor not found
	# install.packages("polycor")
	hetc = polycor::hetcor(data, ML=ML, use=use, std.err=F)
	return(hetc$correlations)
}