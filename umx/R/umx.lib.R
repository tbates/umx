# https://github.com/hadley/devtools/wiki/Package-basics

#' Order a data frame by its columns.
#'
#' This function completes the subsetting, transforming and ordering triad
#' with a function that works in a similar way to \code{\link{subset}} and 
#' \code{\link{transform}} but for reordering a data frame by its columns.
#' This saves a lot of typing!
#'
#' @param df data frame to reorder
#' @param ... expressions evaluated in the context of \code{df} and 
#'   then fed to \code{\link{order}}
#' @keywords manip
#' @export
#' @examples
#' mtcars[with(mtcars, order(cyl, disp)), ]
#' arrange(mtcars, cyl, disp)
#' arrange(mtcars, cyl, desc(disp))
arrange <- function(df, ...) {
  ord <- eval(substitute(order(...)), df, parent.frame())
  unrowname(df[ord, ])
}

# umx.lib.R
# A recent, but not necessarily current version can be sourced like this:
# source("http://timbates.wdfiles.com/local--files/start/umx.lib.R")
# To learn more, see https://github.com/tbates/umx/

# ==========================
# = Example data in OpenMx =
# ==========================
# myFADataRaw # 6 correlated favriable x1:x6

# umx.lib.R
# A recent, but not necessarily current version can be sourced like this:
# source("http://timbates.wdfiles.com/local--files/start/umx.lib.R")
# To learn more, see https://github.com/tbates/umx/

# ==========================
# = Example data in OpenMx =
# ==========================
# myFADataRaw # 6 correlated favriable x1:x6

umxUpdateOpenMx <- function(bleedingEdge = F, loadNew = T, anyOK = F) {
	# Purpose: update the OpenMx Library to latest version:
	# use case:
	# umxUpdateOpenMx()
	if( "OpenMx" %in% .packages() ){
		oldV = mxVersion();
		if(anyOK){
			message("You have version", oldV, "and that's fine")
			return()
		}
		detach(package:OpenMx); # unload existing version
		message("existing version \"" ,oldV, "\" was detached")
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
		# detach(package:OpenMx); # unload existing version
		require("OpenMx")
		newV = mxVersion();
		if(!is.na(oldV)){
			message("Woot: installed the latest and greatest \"", newV, "\" of OpenMx!")
		} else {
			message("Woot: you have upgraded from version \"" ,oldV, "\" to the latest and greatest \"", newV, "\"!")
		}
	}
}

# =============================
# = Fit and Reporting Helpers =
# =============================

umxCompare <- function(base = NA, comparison = NA, all = T, output = "Rout.html") {
	# c("Rout.html", "return")
	# umxCompare(fit11, fit11, all=F, output="Rout.html")
	if(is.na(comparison)){
		comparison = base
	} 
	tableOut  = mxCompare(base = base, comparison = comparison, all = all)
	tableOut  = format(tableOut, scientific = F, digits = 5)
	tableOut = tableOut[, c(2:1, 3, 4, 6:9)]
	names(tableOut)<-c("Comparison", "Base", "ep", "-2LL", "AIC", "delta LL", "delta df", "p")
	if(output=="return"){
		return(tableOut)
	} else {
		print.html(tableOut, output = output, rowlabel="")
	}
	# if(render){
	# 	fName= "Model.Fitting.xls"
	# 	write.table(tableOut,fName, row.names=F,sep="\t", fileEncoding="UTF-8") # macroman UTF-8 UTF-16LE
	# 	system(paste("open", fName));
	# }
}

umxSummary <- function(model, precision = 2, parameters = NA, report = NA) {
	# useage
	# umxSummary(fit1)
	if(!is.na(report)){
		warning("report not implemented")
	}
	x = summary(model)$parameters[,c("row", "col", "Std.Estimate")]
	x$Std.Estimate = round(x$Std.Estimate, precision)
	print(x)
	umxReportFit(model)
}

umxSaturated <- function(model, evaluate = T, verbose = T) {
	# TODO: Update to omxSaturated() and omxIndependenceModel()
	# TODO: Update IndependenceModel to analytic form
	# Use case
	# model_sat = umxSaturated(model)
	# summary(model, SaturatedLikelihood = model_sat$Sat, IndependenceLikelihood = model_sat$Ind)
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
	dataMeans           = colMeans(theData, na.rm = T)
	meansLabels         = paste("mean", 1:nVar, sep = "")
	covData             = cov(theData, use = "pairwise.complete.obs")
	factorLoadingStarts = t(chol(covData))
	independenceStarts  = diag(covData)
	loadingsLabels      = paste("F", 1:nVar, "loading", sep = "")

	# Set latents to a new set of 1 per manifest
	# Set S matrix to an Identity matrix (i.e., variance fixed@1)
	# Set A matrix to a Cholesky, manifests by manifests in size, free to be estimated 
	# TODO: start the cholesky at the cov values
	m2 <- mxModel("sat",
    	# variances set at 1
		# mxMatrix(name = "factorVariances", type="Iden" , nrow = nVar, ncol = nVar), # Bunch of Ones on the diagonal
	    # Bunch of Zeros
		mxMatrix(name = "factorMeans"   , type = "Zero" , nrow = 1   , ncol = nVar), 
	    mxMatrix(name = "factorLoadings", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = factorLoadingStarts), 
		# labels = loadingsLabels),
	    mxAlgebra(name = "expCov", expression = factorLoadings %*% t(factorLoadings)),

	    mxMatrix(name = "expMean", type = "Full", nrow = 1, ncol = nVar, values = dataMeans, free = T, labels = meansLabels),
	    mxFIMLObjective(covariance = "expCov", means = "expMean", dimnames = manifests),
	    mxData(theData, type = "raw")
	)
	m3 <- mxModel("independence",
	    # TODO: slightly inefficient, as this has an analytic solution
	    mxMatrix(name = "variableLoadings" , type="Diag", nrow = nVar, ncol = nVar, free=T, values = independenceStarts), 
		# labels = loadingsLabels),
	    mxAlgebra(name = "expCov", expression = variableLoadings %*% t(variableLoadings)),
	    mxMatrix(name  = "expMean", type = "Full", nrow = 1, ncol = nVar, values = dataMeans, free = T, labels = meansLabels),
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

umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = "std") {
	# Purpose: compactly report fit statistics, as for a paper
	# Use case: umxReportFit(m1, report="table")
	# umxReportFit(m1, saturatedModels = m1_sat)
	# nb: "saturatedModels" is a list of the saturated and independence models from umxSaturated()
	# nb: I now compute this for you if you leave it blank and it is needed…
	# showEstimates = "none|raw|std|both"
	# report = "line|table"
	# TODO make table take lists of models...
	# TODO could have a toggle for computing hte saturated models...

	output <- model@output
	# stop if there is no objective function
	if ( is.null(output) ) stop("Provided model has no objective function, and thus no output. mxRun(model) first")
	# stop if there is no output
	if ( length(output) <1 ) stop("Provided model has no output. I can only standardize models that have been mxRun() first!")
	

	if(is.null(saturatedModels)){
		modelSummary = summary(model)
		if(is.na(modelSummary$SaturatedLikelihood)){
			message("There is no saturated likelihood: computing that now...")
			saturatedModels = umxSaturated(model)
			modelSummary = summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
		}
	} else {
		modelSummary = summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
	}

	if(showEstimates != "none"){
		if("Std.Estimate" %in%  names(modelSummary$parameters)){
			if(showEstimates == "both") {
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error", "Std.Estimate", "Std.SE")
			} else if(showEstimates == "std"){
				namesToShow = c("name", "matrix", "row", "col", "Std.Estimate", "Std.SE")
			}else{
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")					
			}
		} else {
			namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")
		}
		print(modelSummary$parameters[,namesToShow], digits= 3, na.print = "", zero.print = "0", justify = "none")
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
				names(x) = c("model","χ2","p","CFI", "TLI","RMSEA")
				print(x)
			} else {
				x = paste0(
					"χ2(", degreesOfFreedom, ") = ", round(Chi,2),
					", p = "    , formatC(p, format="g"),
					"; CFI = "  , round(CFI,3),
					"; TLI = "  , round(TLI,3),
					"; RMSEA = ", round(RMSEA, 3), 
					", TLI = "  , TLI_OK,
					", RMSEA = ", RMSEA_OK)
					print(x)
			}
	})
	
	# References for OK/bad
	# Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Coventional criteria versus new alternatives. Structural Equation Modeling, 6, 1-55. 
	# Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models with binary and continuous outcomes. University of California, Los Angeles, Los Angeles. Retrieved from http://www.statmodel.com/download/Yudissertation.pdf  
}

umxGraph_RAM <- function(model = NA, std = T, precision = 2, dotFilename = "name", pathLabels = "none", showFixed = F, showError = T) {
	# Purpose: Graphical output of your model using "graphviz":
	# umxGraph_RAM(fit1, std=T, precision=3, dotFilename="name")
	# nb: legal values for "pathLabels" are "both", "none" or "labels"
	latents = model@latentVars   # 'vis', 'math', and 'text' 
	selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap', 'sentence', 'numeric', 'series', and 'arithmet'
	if(std){ model= umxStandardizeModel(model, return="model") }
	out = "";
	# Get Asymmetric Paths
	aRows = dimnames(model[["A"]]@free)[[1]]
	aCols = dimnames(model[["A"]]@free)[[2]]
	for(target in aRows ) {
		for(source in aCols) {
			thisPathFree = model[["A"]]@free[target,source]
			thisPathVal  = round(model[["A"]]@values[target,source],precision)
			if(thisPathFree){
				out = paste(out, ";\n", source, " -> ", target, " [label=\"", thisPathVal, "\"]", sep="")
			} else if(thisPathVal!=0 & showFixed) {
				# TODO Might want to fix this !!! comment out
				out = paste(out, ";\n", source, " -> ", target, " [label=\"@", thisPathVal, "\"]", sep="")
				# return(list(thisLatent,thisManifest))
			}
		}
	}
	variances = c()
	varianceNames = c()
	S <- model[["S"]]
	Svals   = S@values
	Sfree   = S@free
	Slabels = S@labels
	allVars = c(latents, selDVs)
	for(target in allVars ) { # rows
		lowerVars  = allVars[1:match(target,allVars)]
		for(source in lowerVars) { # columns
			thisPathLabel = Slabels[target,source]
			thisPathFree  = Sfree[target,source]
			thisPathVal   = round(Svals[target,source], precision)
			if(thisPathFree | (!(thisPathFree) & thisPathVal !=0 & showFixed)) {
				if(thisPathFree){
					prefix = ""
				} else {
					prefix = "@"
				}
				if((target==source)) {
					if(showError){
						eName     = paste(source, '_var', sep="")
						varToAdd  = paste(eName, ' [label="', prefix, thisPathVal, '", shape = plaintext]', sep="")
						variances = append(variances, varToAdd)
						varianceNames = append(varianceNames, eName)
						out = paste(out, ";\n", eName, " -> ", target, sep = "")
					}
				} else {
					if(pathLabels=="both"){
						out = paste(out, ";\n", source, " -> ", target, " [dir=both, label=\"", thisPathLabel, "=", prefix, thisPathVal, "\"]", sep="")
					} else if(pathLabels=="labels"){
						out = paste(out, ";\n", source, " -> ", target, " [dir=both, label=\"", thisPathLabel, "\"]", sep="")
					}else{
						out = paste(out, ";\n", source, " -> ", target, " [dir=both, label=\"", prefix, thisPathVal, "\"]", sep="")
					}
				}
			} else {
				# return(list(thisFrom,thisTo))
			}
		}
	}
	preOut= "";
	for(var in selDVs) {
	   preOut = paste(preOut, var, " [shape = square];\n", sep="")
	}
	for(var in variances) {
	   preOut = paste(preOut, "\n", var, sep="")
	}
	rankVariables = paste("\n{rank=min; ", paste(latents, collapse="; "), "};")
	rankVariables = paste(rankVariables, "\n{rank=same; ", paste(selDVs, collapse=" "), "};")
	rankVariables = paste(rankVariables, "\n{rank=max; ", paste(varianceNames, collapse=" "), "};")

	# return(out)
	digraph = paste("digraph G {\nsplines=\"FALSE\";\n", preOut, out, rankVariables, "\n}", sep="");
	if(!is.na(dotFilename)){
		if(dotFilename=="name"){
			dotFilename = paste(model@name, ".dot", sep="")
		}
		cat(digraph, file = dotFilename) #write to file
		system(paste("open", shQuote(dotFilename)));
		# return(invisible(cat(digraph)))
	} else {
		return (cat(digraph));
	}
}
# end umxGraph_RAM(fit1,std=T, precision=3, dotFilename="name")

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

umxMI_top <- function(fit=NA, numInd=10, typeToShow="both", decreasing=T, cache=T) {
	# depends on umxMI(fit)
	# use cases
	# mi.df = umxMI_top(fit)
	# umxMI_top(fit, numInd=5, typeToShow="add") # valid options are "both|add|delete"
	if(typeof(fit) == "list"){
		mi.df = fit
	} else {
		mi = umxMI(fit, vector=T)
		mi.df = data.frame(path= as.character(attributes(mi$mi)$names), value=mi$mi);
		row.names(mi.df) = 1:nrow(mi.df);
		# TODO: could be a helper: choose direction
		mi.df$from = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\1", mi.df$path)
		mi.df$to   = sub(pattern="(.*) +(<->|<-|->) +(.*)", replacement="\\3", mi.df$path)
		mi.df$arrows = 1
		mi.df$arrows[grepl("<->", mi.df$path)]= 2		

		mi.df$action = NA 
		# mi.df  = mi.df[order(mi.df[,2], decreasing=decreasing),] 
		mi.df  = mi.df[order(abs(mi.df[,2]), decreasing=decreasing),] 
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
	}
	mi.df = mi.df[unique(mi.df$copy),] # c("copy")
	if(typeToShow!="both"){
		mi.df = mi.df[mi.df$action==typeToShow,]
	}
	print(mi.df[1:numInd, !(names(mi.df) %in% c("path","copy"))])
	invisible(mi.df)		
}

# How long did that take?
umxReportTime <- function(model, formatStr= "H %H M %M S %OS3", tz="GMT"){
	# use case
	# umxReportTime(fit1)
	format(.POSIXct(model@output$wallTime,tz), formatStr)
}

umxUnexplainedCausalNexus <- function(from, delta, to, model) {
	# umxUnexplainedCausalNexus(from, delta, to, model)
	manifests = model@manifestVars
	partialDataRow <- matrix(0, 1, length(manifests))  # add dimnames to support string varnames 
	dimnames(partialDataRow) = list("val", manifests)
	partialDataRow[1, from] <- delta # delta is in raw "from" units
	partialDataRow[1, to]   <- NA
	completedRow <- umxConditionalsFromModel(model, partialDataRow, meanOffsets = T)
	# by default, meanOffsets = F, and the results take expected means into account
	return(completedRow[1, to])
}

umxConditionalsFromModel <- function(model, newData=NULL, returnCovs=F, meanOffsets = F) {
	#` original author: [Timothy Brick](http://openmx.psyc.virginia.edu/users/tbrick)
	#` [history](http://openmx.psyc.virginia.edu/thread/2076)
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

umxComputeConditionals <- function(sigma, mu, current, onlyMean = F) {
	# Usage: umxComputeConditionals(model, newData)
	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	#` original author: [Timothy Brick](http://openmx.psyc.virginia.edu/users/tbrick)
	#` [history](http://openmx.psyc.virginia.edu/thread/2076)
	require(OpenMx)

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

print.dataframe <- function (x, digits = getOption("digits"), quote = FALSE, na.print = "", zero.print = "0", justify = "none", ...){
    xx <- format(x, digits = digits, justify = justify)
    if (any(ina <- is.na(x))) 
        xx[ina] <- na.print
	i0 <- !ina & x == 0
    if (zero.print != "0" && any(i0)) 
        xx[i0] <- zero.print
    if (is.numeric(x) || is.complex(x)){
        print(xx, quote = quote, right = TRUE, ...)
    }else{
		print(xx, quote = quote, ...)	
    }
    invisible(x)
	# use case
	# print.dataframe(bob, digits=2, zero.print = ".", justify="left")
}

# =================================
# = Speed  and Efficiency Helpers =
# =================================

umxRun <- function(model, n = 3, calc_SE = T, calcSat = T){
	# TODO: return change in -2LL
	# Optimise for speed
	# Use case
	# model <- umxRun(model, n = 10)
	model = mxOption(model, "Calculate Hessian", "No")
	model = mxOption(model, "Standard Errors", "No")
	# make an initial run
	model = mxRun(model);
	n = (n-1)
	tries = 0
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
	if((class(model$objective)[1] == "MxRAMObjective") & model@data@type =="raw"){
		# If we have a RAM model with raw data, compute the satuated and indpeendence models
		# TODO: Update to omxSaturated() and omxIndependenceModel()
		# message("computing saturated and independence models so you have access to absoute fit indices for this raw-data model")
		model_sat = umxSaturated(model, evaluate = T, verbose = T)
		model@output$IndependenceLikelihood = model_sat$IndependenceLikelihood@output$Minus2LogLikelihood
		model@output$SaturatedLikelihood    = model_sat$SaturatedLikelihood@output$Minus2LogLikelihood
	}
	return(model)
}
# m1 = umxRun(m1); summary(m1)

umxReRun <- function(lastFit, dropList = NA, regex = NA, free = F, value = 0, freeToStart = NA, name = NA, verbose = F, intervals = F, newName = "deprecated") {
	# fit2 = umxReRun(fit1, regex="Cs", name="AEip")
	if(newName != "deprecated"){
		message("newName is deprecated in umxReRun: use name=\"", newName, "\" instead")
		name = newName
	}
	if(is.na(name)){
		name = lastFit@name
	}
	if(is.na(regex)) {
		if(any(is.na(dropList))) {
			stop("Both dropList and regex cannot be empty!")
		} else {
			x = mxRun(omxSetParameters(lastFit, labels=dropList, free = free, value = value, name= name),intervals = intervals)
		}
	} else {
		x = mxRun(omxSetParameters(lastFit, labels=umxGetLabels(lastFit, regex=regex,free=freeToStart,verbose=verbose), free = free, value = value, name = name),intervals=intervals)
	}
	return(x)
}

# Parallel helpers to be added here

# ========================================
# = Model building and modifying helpers =
# ========================================

umxStart_value_list <- function(x = 1, sd = NA, n = 1) {
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

umxStart <- function(obj = NA, sd = NA, n = 1) {
	# Purpose: Set sane starting values in RAM models OR return a list of start values
	# use case: m1 = umxStart(obj = m1)
	if(is.numeric(obj) ) {
		umxStart_value_list(x = obj, sd = NA, n = 1)
	} else {
		# This is an MxRAM Model: Set sane starting values
		# TODO: Start values in the A matrix...
		# Done: Start values in the S at variance on diag, bit less than cov off diag
		# Done: Start amnifest means in means model
		# TODO: Start latent means?...
		if (!(isS4(obj) && is(obj, "MxModel"))) {
			# TODO: Need to add a check for RAMness
			stop("'obj' must be an mxModel (or a simple number)")
		}
		if (length(obj@submodels) > 0) {
			stop("Cannot yet handle submodels")
		}
		theData = obj@data@observed
		if (is.null(theData)) {
			stop("'model' does not contain any data")
		}
		manifests     = obj@manifestVars
		nVar          = length(manifests)
		if(obj@data@type == "raw"){
			# = Set the means =
			dataMeans   = colMeans(theData[,manifests], na.rm = T)
			freeManifestMeans = (obj@matrices$M@free[1, manifests] == T)
			obj@matrices$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]

			covData     = cov(theData, use = "pairwise.complete.obs")		
		} else {
			covData = theData
		}
		dataVariances = diag(covData)
		# ==========================================================
		# = Fill the free symetrical matrix with good start values =
		# ==========================================================
		# The diagonal is variances
		freePaths = (obj@matrices$S@free[1:nVar, 1:nVar] == TRUE)
		obj@matrices$S@values[1:nVar, 1:nVar][freePaths] = covData[freePaths]
		return(obj)
	}	
}

umxLatent <- function(latent = NA, formedBy = NA, forms = NA, data, endogenous = FALSE, model.name = NA, help = FALSE, labelSuffix = "", verbose = T) {
	# Purpose: make a latent variable formed/or formed by some manifests
	# Use: umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
	# TODO: delete manifestVariance
	# Check both forms and formedBy are not defined
	if( is.na(formedBy) &&  is.na(forms)) { stop("Error in mxLatent: Must define one of forms or formedBy") }
	if(!is.na(formedBy) && !is.na(forms)) { stop("Error in mxLatent: Only one of forms or formedBy can be set") }
	# ==========================================================
	# = NB: If any vars are ordinal, a call to umxMakeThresholdsMatrices
	# = will fix the mean and variance of ordinal vars to 0 and 1
	# ==========================================================
	# manifests <- names(dataFrame)
	# latents   <- c("G")
	# m1 <- mxModel("m1", type="RAM",
	# 	manifestVars = manifests,
	# 	latentVars   = latents,
	# 	# Factor loadings
	# 	mxLatent("Read", forms = readMeasures),
	# 	mxData(cov(dataFrame), type="cov", numObs=100)
	# )
	# m1= mxRun(m1); summary(m1)

	# Warning("If you use this with a dataframe containing ordinal variables, don't forget to call umxAutoThreshRAMObjective(df)")
	if( nrow(data) == ncol(data)) {
		if(all(data[lower.tri(data)] == t(data)[lower.tri(t(data))])){
			isCov = T
			if(verbose){
				message("treating data as cov")
			}
		} else {
			isCov = F
			if(verbose){
				message("treating data as raw: it's a bit odd that it's square, however")
			}
		}
	} else {
		isCov = F
		if(verbose){
			message("treating data as raw")
		}
	}
	if( any(!is.na(forms)) ) {
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
			if( any(!is.na(forms)) ) {
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

	if( any(!is.na(forms)) ) {
		# Handle forms case
		if(!help) {
			# p1 = Residual variance on manifests
			# p2 = Fix latent variance @ 1
			# p3 = Add paths from latent to manifests
			p1 = mxPath(from = manifests, arrows = 2, free = T, values = variances) # umxLabels(manifests, suffix = paste0("unique", labelSuffix))
			if(endogenous){
				# Free latent variance so it can do more than just redirect what comes in
				if(verbose){
					message(paste("latent '", latent, "' is free (treated as a source of variance)", sep=""))
				}
				p2 = mxPath(from=latent, connect="single", arrows=2, free=T, values=.5) # labels=umxLabels(latent, suffix=paste0("var", labelSuffix))
			} else {
				# fix variance at 1 - no inputs
				if(verbose){
					message(paste("latent '", latent, "' has variance fixed @ 1"))
				}
				p2 = mxPath(from=latent, connect="single", arrows=2, free=F, values=1) # labels=umxLabels(latent, suffix=paste0("var", labelSuffix))
			}
			p3 = mxPath(from = latent, to = manifests, connect = "single", free = T, values = variances) # labels = umxLabels(latent, manifests, suffix=paste0("path", labelSuffix))
			if(isCov) {
				# Nothing to do: covariance data don't need means...
				paths = list(p1, p2, p3)
			}else{
				# Add means: fix latent mean @0, and add freely estimated means to manifests
				p4 = mxPath(from = "one", to = latent   , arrows = 1, free = F, values = 0)  # labels=umxLabels("one", latent, suffix = labelSuffix)
				p5 = mxPath(from = "one", to = manifests, arrows = 1, free = T, values = means) # labels=umxLabels("one", manifests, suffix = labelSuffix) 
				paths = list(p1, p2, p3, p4, p5)
			}			
		} else {
			# TODO: display graphVizTextFormed as digraph
			message("Help not implemented: run graphVizTextFormed")
		}
	} else {
		# Handle formedBy case
		if(!help) {
			# Add paths from manifests to the latent
			p1 = mxPath(from = manifests, to = latent, connect = "single", free = T, values = umxStart(.6, n=manifests)) # labels=umxLabels(manifests,latent, suffix=paste0("path", labelSuffix))
			# In general, manifest variance should be left free…
			# TODO If the data were correlations… we can inspect for that, and fix the variance to 1
			p2 = mxPath(from = manifests, connect = "single", arrows = 2, free = T, values = variances) # labels=umxLabels(manifests, suffix=paste0("var", labelSuffix))
			# Allow manifests to intercorrelate
			p3 = mxPath(from = manifests, connect = "unique.bivariate", arrows = 2, free = T, values = umxStart(.3, n = manifests)) #labels = umxLabels(manifests, connect="unique.bivariate", suffix=labelSuffix)
			if(isCov) {
				paths = list(p1, p2, p3)
			}else{
				# Fix latent mean at 0, and freely estimate manifest means
				p4 = mxPath(from="one", to=latent   , free = F, values = 0) # labels = umxLabels("one",latent, suffix=labelSuffix)
				p5 = mxPath(from="one", to=manifests, free = T, values = means) # labels = umxLabels("one",manifests, suffix=labelSuffix)
				paths = list(p1, p2, p3, p4, p5)
			}
		} else {
			# TODO: display graphVizTextForms as digraph
			message("help not implemented: run graphVizTextForms")
		}
	}
	if(!is.na(model.name)) {
		m1 <- mxModel(model.name, type="RAM", manifestVars=manifests, latentVars=latent, paths)
		if(isCov){
			m1 <- mxModel(m1, mxData(cov(df), type="cov", numObs = 100))
			message("\n\nIMPORTANT: you need to see numObs in the mxData() statement\n\n\n")
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
	# readMeasures = paste("test", 1:3, sep="")
	# bad usages
	# mxLatent("Read") # no too defined
	# mxLatent("Read", forms=manifestsRead, formedBy=manifestsRead) #both defined
	# m1 = mxLatent("Read", formedBy = manifestsRead, model.name="base"); umxGraph_RAM(m1, std=F, dotFilename="name")
	# m2 = mxLatent("Read", forms = manifestsRead, as.model="base"); 
	# m2 <- mxModel(m2, mxData(cov(df), type="cov", numObs=100))
	# umxGraph_RAM(m2, std=F, dotFilename="name")
	# mxLatent("Read", forms = manifestsRead)
}

umxModelIsRAM <- function(obj) {
	# test is model is RAM
	# umxModelIsRAM(obj)
	isModel = isS4(obj) & is(obj, "MxModel")
	if(!isModel){
		return(F)
	}
	oldRAM_check = class(obj$objective) == "MxRAMObjective"
	# TODO: get working on both the old and new objective model...
	# newRAM_check = (class(obj$objective)[1] == "MxRAMObjective"))
	if(oldRAM_check) {
		return(T)
	} else {
		return(F)			
	}
}

umxIsMxModel <- function(obj) {
	isS4(obj) & is(obj, "MxModel")	
}

umxIsRAMmodel <- function(obj) {
	(class(obj$objective)[1] == "MxRAMObjective" | class(obj$expectation)[1] == "MxExpectationRAM")	
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

# ======================
# = Labeling functions =
# ======================

umxLabel <- function(obj, suffix = "", baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA, verbose = F) {	
	# Purpose: Label the cells of a matrix, OR the matrices of a RAM model
	# version: 2.0b now that it labels matrices, RAM models, and arbitrary matrix models
	# nb: obj must be either an mxModel or an mxMatrix
	# Use case: m1 = umxLabel(m1)
	# umxLabel(mxMatrix("Full", 3,3, values = 1:9, name = "a"))
	if (is(obj, "MxMatrix") ) { 
		# label an mxMatrix
		xmuLabel_Matrix(obj, baseName, setfree, drop, jiggle, boundDiag, suffix)
	} else if (umxModelIsRAM(obj)) { 
		# label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(obj, suffix))
	} else if (is(obj, "MxModel")) {
		# label a non-RAM matrix model
		return(xmuLabel_MATRIX_Model(obj, suffix))
	} else {
		stop("I can only label OpenMx models and mxMatrix types. You gave me a ", typeof(obj))
	}
}

umxGetLabels <- function(inputTarget, regex = NA, free = NA, verbose = F) {
	# Purpose: a regex-enabled version of omxGetParameters
	# usage e.g.
	# umxGetLabels(model@matrices$as) # all labels of as matrix
	# umxGetLabels(model, regex="as_r_2c_[0-9]", free=T) # get all columns of row 2 or as matrix
	if(class(inputTarget)[1] %in% c("MxRAMModel","MxModel")) {
		topLabels = names(omxGetParameters(inputTarget, indep=FALSE, free=free))
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
		if(length(grep("[\\.\\*\\[\\(\\+\\|]+", regex) )<1){ # no grep found: add some anchors for safety
			regex = paste("^", regex, "[0-9]*$", sep=""); # anchor to the start of the string
			if(verbose==T){
				cat("note: anchored regex to beginning of string and allowed only numeric follow\n");
			}
		}
		
		theLabels = grep(regex, theLabels, perl = F, value=T) # return more detail
		if(length(theLabels)==0){
			stop("found no matching labels!");
		}
	}
	# TODO Be nice to offer a method to handle submodels
	# model@submodels$aSubmodel@matrices$aMatrix@labels
	# model@submodels$MZ@matrices
	return(theLabels)
}

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

#` ## path-oriented helpers
umxStandardizeModel <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {
	# Purpose : standardise a RAM model, usually in order to return a standardized version of the model.
	# Use case: umxStandardizeModel(model, return = "model")
	# note    : Make sure 'return' is a valid option: "parameters", "matrices", or "model"
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

#` ## matrix-oriented helpers

umxPath <- function(from = NA, to = NA, connect = "single", arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA, prefix = "", suffix = "",...) {
	stop("replace umxPath with mxPath, and run umxLabel(model) on the model when you are done to add default labels, plus umxStart(model) to add default start values")
	# {$|single,all.pairs,all.bivariate,unique.pairs,unique.bivariate|}
	# Purpose: make mxPaths with informative labels, comments to tim.bates@ed.ac.uk
	# use case
	# umxPath(from = "OriginSES", to = "PaternalSESn")
	# Use case: umxPath("F1", paste("m",1:4,sep="")) # "F1_to_m1" "F1_to_m2" "F1_to_m3" "F1_to_m4"
	# TODO: make this generate the paths as well... i.e., "umxPath()"
	# TODO: handle connection style
	# nb: bivariate length = n-1 recursive 1=0, 2=1, 3=3, 4=7 i.e., 
	if(any(is.na(to)) & (suffix=="var"| suffix=="unique")){
		# handle from only, variance and residuals
		part1  = paste(prefix, from, sep="")
		myLabels = (paste(part1, suffix, sep="_"))
	} else if(any(from == "one")){
		# handle means (from == "one")
		if(!all(from == "one")){
			stop(cat("Error in umxLabels: from was a mix of one and not-one",from))
		} else {
			myLabels = paste(to, "mean", sep="_")
		}
	} else if(connect == "unique.bivariate") {
		if(!all(is.na(to))){
			if(!(from == to)){
				stop("with connect = 'unique.bivariate', to must be blank, or the same as from")
			}
		}
		labels = as.character(combn(from, m = 2, FUN = paste, collapse = "_"))
		return(labels)
	} else {
		fromPart = paste(prefix, from, sep = "")
		toPart   = paste(to  , suffix, sep = "_")
		myLabels = paste(fromPart, toPart, sep = "_to_")
	}
	mxPath(from = from, to = to, connect = connect, arrows = arrows, free = free, values = values, labels = myLabels, lbound = lbound, ubound = ubound)
}

# ====================
# = Data and Utility =
# ====================

Stouffer.test <- function(p = NULL) {
	# Purpose:
	# Use case: Stouffer.test(p = c(0.13, 0.18, 0.06))
	# http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/CombiningPvalues
	# Stouffer, Samuel A., Edward A. Suchman, Leland C. DeVinney, Shirley A. Star, and Robin M. Williams, Jr. (1949). Studies in Social Psychology in World War II: The American Soldier. Vol. 1, Adjustment During Army Life. Princeton: Princeton University Press.
	# 
	# Bailey TL, Gribskov M (1998). Combining evidence using p-values: application to sequence homology searches. Bioinformatics, 14(1) 48-54.
	# 
	# Fisher RA (1925). Statistical methods for research workers (13th edition). London: Oliver and Boyd.
	# 
	# Manolov R and Solanas A (2012). Assigning and combining probabilities in single-case studies. Psychological Methods 17(4) 495-509. Describes various methods for combining p-values including Stouffer and Fisher and the binomial test.
	# useage: Stouffer.test(p = c(.01, .2, .3))
	pl <- length(p)
	if (is.null(p) | pl < 2) {
		stop("There was an empty array of p-values")
	}
	erf <- function(x) {
		2 * pnorm(2 * x/ sqrt(2)) - 1
	}
	erfinv <- function(x) {
		qnorm( (x + 1) / 2 ) / sqrt(2)
	}
	pcomb <- function(p) {
		(1 - erf(sum(sqrt(2) * erfinv(1 - 2 * p)) / sqrt(2 * length(p))))/2
	}
	pcomb(p)
}

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

umxLower2full <- function(lower.data, diag=F, byrow=T) {
	# lower2full(lower.tri, diag=F)
	# lower2full(lower.data, diag=T, byrow=F)
	# lower2full(lower.no.diag, diag=F, byrow=F)
	# lower2full(lower.bycol, diag=T, byrow=F)
	# lower2full(lower.byrow, diag=T, byrow=T)

	len = length(lower.data)
	if(diag) {
		# len*2 = ((x+.5)^2)-.25
		size = len*2
		size = size + .25
		size = sqrt(size)
		size = size -.5; size
	}else{
		# len = (x*((x+1)/2))-x	
		# .5*(x-1)*x
		size = len *2
		# (x-.5)^2 - .25
		size= size + .25
		size = sqrt(size)
		size = size +.5; size
	}
	mat = diag(size)
	if(byrow){
		# put  data into upper triangle, then transform to lower
		mat[upper.tri(mat,diag=diag)] <- lower.data;
		mat[lower.tri(mat,diag=F)] <- mat[upper.tri(mat,diag=F)]
	}else{                            
		mat[lower.tri(mat,diag=diag)] <- lower.data;
		mat[upper.tri(mat,diag=F)] <-mat[lower.tri(mat,diag=F)]
	}
	return(mat)
}

umxFindObject <- function(grepString = ".*", requiredClass = "MxModel") {
	# Purpose: find objects a certain class, whose name matches a (grep) search string
	# Use case: umxFindObject("Chol*", "MxModel")
	# umxFindObject("", "MxModel")
	matchingNames = ls(envir=sys.frame(-1), pattern=grepString) #envir
	matchingObjects = c()
	for (obj in matchingNames) {
		if(class(get(obj))[1]==requiredClass){
			matchingObjects = c(matchingObjects, obj)
		}
	}
	return(matchingObjects)
}

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

umxMakeThresholdMatrix <- function(df, deviationBased=T, droplevels = T, verbose=F) {	
	# Purpose: return a mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")
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

umxIsOrdinalVar <- function(df, names=F) {
	# Purpose, return which columns are Ordinal
	# use case: isContinuous = !umxIsOrdinalVar(df)
	# nb: can optionally return just the names of these
	nVar = ncol(df);
	# Which are ordered factors?
	factorVariable = rep(F,nVar)
	for(n in 1:nVar) {
		if(is.ordered(df[,n])) {
			factorVariable[n]=T
		}
	}
	if(names){
		return(names(df)[factorVariable])
	} else {
		return(factorVariable)
	}
}

# ==================
# = Model Builders =
# ==================

umxSimpleCFA <- function(name="", latents, data, report =c("shortTable","shortLine","long")){
	# umxSimpleRAM(name="N", latents="N", data)
	manifests <- names(data)
	m1 <- mxModel(name, type="RAM",
		manifestVars = manifests,
		latentVars   = latents,
		# Factor loadings
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2), # manifest residuals 
		mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
		mxData(cov(data), type="cov", numObs = nrow(data))
	)
	m1 = mxRun(m1); 
	if(report == "shortTable") {
		umxReportFit(m1, report = "table");
	} else if(report == "shortLine"){
		umxReportFit(m1, report = "line");
	} else if (report == "long"){
		umxSummary(m1, report = "")
	} else {
		message("Bad setting for report")
	}
	invisible(m1)
}

# ===============
# = RAM Helpers =
# ===============
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
		p1 = mxPath(from=manifests, arrows=2, value=variances) # labels = umxLabels(manifests, suffix = paste0("unique", labelSuffix)))
		return(p1)
	} else {
		manifestOrdVars = mxIsOrdinalVar(data[,manifests])
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

umxJiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {
	mask      = (matrixIn != dontTouch);
	newValues = mask;
	matrixIn[mask==TRUE] = matrixIn[mask==TRUE] + rnorm(length(mask[mask==TRUE]), mean=mean, sd=sd);
	return (matrixIn);
}

# ========================================
# = Not Typically used directly by users =
# ========================================

xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = T) {
	# Purpose: to label all the free parameters of a (non-RAM) model
	# nb: We don't assume what each matrix is for, and just stick a_r1c1 into each cell
	# Use case: model = xmuLabel_MATRIX_Model(model)
	# Use case: model = xmuLabel_MATRIX_Model(model, suffix = "male")
	if(!is(model, "MxModel")){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (umxModelIsRAM(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "")
	return(model)
}

xmuPropagateLabels <- function(model, suffix = "") {
    # Called by xmuLabel_MATRIX_Model
    # useage: xmuPropagateLabels(model, suffix = "")
	model@matrices  <- lapply(model@matrices , xmuLabel_Matrix, suffix = suffix)
    model@submodels <- lapply(model@submodels, xmuPropagateLabels, suffix = suffix)
    return(model)
}

xmuLabel_RAM_Model <- function(model, suffix = "") {
	# Purpose: to label all the free parameters of a (RAM) model
	# Use case: model = umxAddLabels(model, suffix = "_male")
	# TODO label means if data = raw
	if (!umxModelIsRAM(model)) {
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
			if(freeA[toRow, fromCol]){
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
			if(freeS[toRow, fromCol]) {
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
	# TODO should check when autocreating names that they don't clash with existing names
	return(model)
}

xmuLabel_Matrix <- function(mx_matrix = NA, baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA, suffix = "", verbose=T) {
	# Purpose: label the cells of an mxMatrix
	# Detail: Defaults to the handy "matrixname_r1c1" where 1 is the row or column
	# Use case: You shouldn't be using this: called by umxLabel
	# xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = T), jiggle = .05, boundDiag = NA);
	# xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = T), jiggle = .05, boundDiag = NA);
	# See also: fit2 = omxSetParameters(fit1, labels = "a_r1c1", free = F, value = 0, name = "drop_a_row1_c1")
	if (!is(mx_matrix, "MxMatrix")){ # label a mxMatrix
		stop("I'm sorry Dave… xmuLabel_Matrix works on mxMatrix. You passed an ", class(mx_matrix), ". And why are you calling xmuLabel_Matrix() anyhow? You want umxLabel()")
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
		# return("Specs not used: leave free as set in mx_matrix") 
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
	if(!is.na(boundDiag)){
		diag(mx_matrix@lbound)<-boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

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

xmuMakeThresholdsMatrices <- function(df, droplevels, verbose) {
	# stop("I have not written xmuMakeThresholdsMatrices yet as it is fucked as a reliable strategy and likely to be superceeded")
	# usecase
	# junk = xmuMakeThresholdsMatrices(df, droplevels=F, verbose=T)
	# junk[1]; 	junk[[2]]@values; 	junk[3]
	
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

# ==============
# = Deprecated =
# ==============

umxTryHard = function(model, n=3, calc_SE=F){ stop("Use umxRun() in place of umxTryHard") }

umxLabels = function(from=NA, to=NA, connect="single", prefix="", suffix="") {stop("please use umxPath in place of umxLabels. To label models or matrices, use umxLabel")}

genEpi_Jiggle = function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {stop("please use umxJiggle in place of genEpi_Jiggle")}

# ==================================
# = Borrowed for tutorial purposes =
# ==================================

summaryACEFit <- function(fit, accuracy = 2, dotFilename = NA, returnStd = F, extended = F, showRg = F, showStd = T, parentModel = NA, CIs = F, zero.print = ".") {
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
		message("-2 \u00d7 log(Likelihood)") # ×
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
				message("You requested me to print out CIs, but there are none - perhaps you’d like to add 'addStd = T' to your makeACE_2Group() call?")
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
		if(!is.na(dotFilename)) {
			message("making dot file")
			if(showStd){
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


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ===================================
# = Maybe add some printing helpers =
# ===================================

umxReportCIs <- function(model, addCIs = T, runCIs="if necessary") {
	if(is.na(model)){
		message("umxReportCIs adds mxCI() calls for all free parameters in a model, runs them, and reports a neat summary. A use example is:\n umxReportCIs(model)")
		stop();
	}
	message("### CIs for model ", model@name)
	if(addCIs){
		CIs = names(omxGetParameters(model))
		model = mxRun(mxModel(model, mxCI(CIs)), intervals = T)
	} else if(runCIs == "if necessary" & dim(model@output$confidenceIntervals)[1] < 0){
		model = mxRun(model, intervals = T)		
	}
	model_summary = summary(model)
	model_CIs = round(model_summary$CI, 3)
	model_CI_OK = model@output$confidenceIntervalCodes
	colnames(model_CI_OK) <- c("lbound Code", "ubound Code")
	model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
	print(model_CIs)
	invisible(model)
}

renameFile <- function(baseFolder = "Finder", findStr=NA, replaceStr=NA, listPattern = NA, test=T, overwrite=F) {
	# renameFile(baseFolder = "~/Downloads/", findStr="", replaceStr="", listPattern = "", test=T)
	# renameFile(baseFolder = NA, findStr="", replaceStr="", listPattern = "", test=T)
	# uppercase = u$1
	if(baseFolder=="Finder"){
		baseFolder = system(intern=T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
		message("Using front-most Finder window:", baseFolder)
	} else if(baseFolder == "") {
		baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep="") ## choose a directory
		message("Using selected folder:", baseFolder)
	}
	if(is.na(listPattern)){
		listPattern= findStr
	}
	a = list.files(baseFolder, pattern=listPattern)
	message("found ", length(a), " possible files")
	changed = 0
	for (fn in a) {
		findB = grepl(pattern=findStr, fn) #returns 1 if found
		if(findB){
			fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
			if(test){
				message("would change ", fn, " to ", fnew)	
			} else {
				if((!overwrite) & file.exists(paste(baseFolder, fnew, sep=""))){
					message("renaming ", fn, "to", fnew, "failed as already exists. To overwrite set T")
				} else {
					file.rename(paste(baseFolder, fn, sep=""), paste(baseFolder, fnew, sep=""))
					changed = changed+1;
				}
			}
		}else{
			if(test){
				# message(paste("bad file",fn))
			}
		}
	}
	message("changed ", changed)
}

moveFile <- function(baseFolder = NA, findStr=NA, fileNameList = files, destFolder = NA, test = T, overwrite = F) {
	# use case: 
	# base = "/Users/tim/Music/iTunes/iTunes Music/"
	# dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
	# moveFile(baseFolder = base, fileNameList = toMove, destFolder = dest, test=F)
	if(is.na(destFolder)){
		stop("destFolder can't be NA")
	}
	if(baseFolder=="Finder"){
		baseFolder = system(intern=T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
		message("Using front-most Finder window:", baseFolder)
	} else if(baseFolder == "") {
		baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep="") ## choose a directory
		message("Using selected folder:", baseFolder)
	}
	moved = 0
	# mv -n "/Users/tim/Music/iTunes/iTunes Music/Music/"
	for (fn in fileNameList) {
		if(test){
			message("would move ", fn, " to ", destFolder)	
			moved = moved + 1;
		} else {
			if((!overwrite) & file.exists(paste0(destFolder, fn))){
				message("moving ", fn, "to", destFolder, "failed as already exists. To overwrite set T")
			} else {
				file.rename(paste0(baseFolder, fn), paste0(destFolder, fn))
				moved = moved + 1;
			}
		}
	}
	message("moved (or would have moved)", moved)
}

cor.prob <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {
	# cor.prob(myFADataRaw[1:8,])
	R <- cor(X, use = use)
	above <- upper.tri(R)
	below <- lower.tri(R)
	r2 <- R[above]^2
	Fstat <- r2 * df/(1 - r2)
	R[row(R) == col(R)] <- NA # NA on the diagonal
	R[above] <- pf(Fstat, 1, df, lower.tail=F)
	R[below] = round(R[below], digits)
	R[above] = round(R[above], digits)
	# R[above] = paste("p=",round(R[above], digits))
	message("lower tri  = correlation; upper tri = p-value")
	return(R)
}

rowMax <- function(df, na.rm=T) {
	tmp = apply(df, MARGIN=1, FUN=max, na.rm=na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}
rowMin <- function(df, na.rm=T) {
	tmp = apply(df, MARGIN=1, FUN=min, na.rm=na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

pp33 <- function(df_, x_) {
	# http://www.p-curve.com/Supplement/R/pp33.r
	# Find critical value of student (xc) that gives p=.05 when df=df_
	xc = qt(p = .975, df = df_)		
	# Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	f <- function(delta, pr, x, df){
		pt(x, df = df, ncp = delta) - pr
	}
	out <- uniroot(f, c(0, 37.62), pr =2/3, x = xc, df = df_)	
	ncp_=out$root	
	# Find probability of getting x_ or larger given ncp
	p_larger = pt(x_, df = df_, ncp = ncp_)
	# Condition on p < .05 (i.e., get pp-value)
	pp = 3 * (p_larger - 2/3)
	# Print results
	return(pp)
}

# To USE ME IN YOUR SCRIPTS SAY: 
# source("http://github.com/OpenMx/mxHelper.R")

mx_FakeData <- function(dataset, digits=2, n=NA, 
  use.names=TRUE, use.levels=TRUE, use.miss=TRUE,
  mvt.method="eigen", het.ML=FALSE, het.suppress=TRUE){

  # This function takes as argument an existing dataset, which 
  # must be either a matrix or a data frame. Each column of the 
  # dataset must consist either of numeric variables or ordered 
  # factors. When one or more ordered factors are included, 
  # then a heterogeneous correlation matrix is computed using 
  # John Fox's polycor package. Pairwise complete observations 
  # are used for all covariances, and the exact pattern of 
  # missing data present in the input is placed in the output,
  # provided a new sample size is not requested. Warnings from
  # the hetcor function are suppressed.

  # Author:   Ryne Estabrook
  # Created:  17 Aug 2010

  require(mvtnorm)
  require(polycor)

  # requires data frame or matrix
  if((is.data.frame(dataset)+is.matrix(dataset))==0){
    warning("Data must be a data frame or matrix")
  }

  # organization
  row <- dim(dataset)[1] # number of rows
  if(is.na(n))(n <- row) # sets unspecified sample size to num rows
  col <- dim(dataset)[2] # number of columns
  del <- is.na(dataset)  # records position of NAs in dataset
  if(n!=row){
    select <- round(runif(n, 0.5, row+.49),0)
    del <- del[select,]
  }
  num <- rep(NA, col)    # see what's not a factor
  ord <- rep(NA, col)    # see what's an ordered factor

  # which columns are numeric (the others are factors)?
  for (i in 1:col){
    num[i] <- is.numeric(dataset[,i])
    ord[i] <- is.ordered(dataset[,i])
  }

  # check for unordered factors
  location <- !(num|ord)
  unorder <- sum(location)

  if(unorder>0)warning(
    paste("Unordered factor detected in variable(s):", 
      names(dataset)[location]
    )
  )

  # if everything is numeric, don't invoke polycor
  if(sum(!num)==0){
    # generate data with rmvnorm
    fake <- rmvnorm(n, 
      apply(dataset, 2, mean, na.rm=TRUE),
      cov(dataset, use="pairwise.complete.obs"),
      mvt.method)

    # round the data to the requested digits
    fake <- round(fake, digits)

    # insert the missing data, if so requested
    if(use.miss==TRUE)(fake[del] <- NA)

    # give the variables names, if so requested
    if(use.names==TRUE)(names(fake) <- names(dataset))

    # return the new data
    return(fake)
  }

  # if there are factors, we start here

  # find the variable means (constrain to zero for factors)
  mixedMeans <- rep(0, col)
  mixedMeans[num] <- apply(dataset[,num], 2, mean, na.rm=TRUE)

  # estimate a heterogeneous correlation matrix
  if (het.suppress==TRUE){
    suppressWarnings(het <- hetcor(dataset, ML=het.ML))
  } else (het <- hetcor(dataset, ML=het.ML))
  mixedCov <- het$correlations

  # make a diagonal matrix of standard deviations to turn the 
  # correlation matrix into a covariance matrix
  stand <- matrix(0, col, col)
  diag(stand) <- rep(1, col)
  diag(stand)[num] <- apply(dataset[,num], 2, sd, na.rm=TRUE)
  # pre and post multiply hetero cor matrix by diagonal sd matrix
  mixedCov <- stand %*% mixedCov %*% stand

  # generate the data
  fake <- as.data.frame(rmvnorm(row, mixedMeans, mixedCov, mvt.method))

  # insert the missing data, if so requested
  if(use.miss==TRUE)(fake[del] <- NA)

  # turn the required continuous variables into factors
  for (i in (1:col)[!num]){
    # the original data for this column
    old <- dataset[,i]
   
    # the new data for this column, omiting NAs
    new <- fake[!is.na(fake[,i]),i]

    # what are the levels of the original factor?
    lev <- levels(old)

    # establish cutpoints in new variable from cdf of old factor
    cut <- cumsum(table(old))/(sum(!is.na(old)))

    # put continuous variable into a matrix, repeating value across columns
    wide <- matrix(new, length(new), length(lev))

    # put the cutpoints in a matrix, repeating the cut point values across rows
    crit <- matrix(quantile(new, cut), length(new), length(lev), byrow=TRUE)

    # for each value (row of the wide matrix), 
    # how many cutpoints is the value greater than?
    # number of cutpoints surpassed=category
    fake[!is.na(fake[,i]),i] <- apply(wide>crit, 1, sum)

    # make it a factor
    fake[,i] <- factor(fake[,i], ordered=TRUE)

    # give the new factor the same levels as the old variable
    if(length(levels(fake[,i]))!=length(lev))message(
      paste("Fewer categories in simulated variable", 
      names(fake)[i], "than in input variable", names(dataset)[i]))
    if(use.levels==TRUE&(length(levels(fake[,i]))==length(lev))){
      levels(fake[,i]) <- lev} else (levels(fake[,i]) <- 1:length(lev))
  }

  # round the data to the requested digits
  fake[,num] <- round(fake[,num], digits)

  # give the variables names, if so requested
  if(use.names==TRUE)(names(fake) <- names(dataset))
  
  # return the new data
  return(fake)
}

umxSummaryGxE <- function(model = NA, xlab = NA, location = "topleft", separateGraphs = F) {
	# Plot Moderation
	# umxSummaryGxE(model, location = "topright")
	if(is.na(model)){
		message("umxSummaryGxE creates Plots for twin moderation models. A use example is:\n umxSummaryGxE(model, location = \"topright\")")
		stop();
	}
	# get unique values of moderator
	mzData = model@submodels$MZ@data@observed
	dzData = model@submodels$DZ@data@observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = selDefs[1]
	}
	mz1 = as.vector(mzData[,selDefs[1]])
	mz2 = as.vector(mzData[,selDefs[2]])
	dz1 = as.vector(dzData[,selDefs[1]])
	dz2 = as.vector(dzData[,selDefs[2]])
	allValuesOfDefVar = c(mz1,mz2,dz1,dz2)
	defVarValues = sort(unique(allValuesOfDefVar))
	a   = model@submodels$top@matrices$a@values
	c   = model@submodels$top@matrices$c@values
	e   = model@submodels$top@matrices$e@values
	am  = model@submodels$top@matrices$am@values
	cm  = model@submodels$top@matrices$cm@values
	em  = model@submodels$top@matrices$em@values
	Va  = (a + am*defVarValues)^2
	Vc  = (c + cm*defVarValues)^2
	Ve  = (e + em*defVarValues)^2
	Vt  = Va + Vc + Ve
    # c("total", "genetic", "unique", "shared")
	out    = as.matrix(cbind(Va   , Ve   , Vc, Vt))
	outStd = as.matrix(cbind(Va/Vt, Ve/Vt, Vc/Vt))
	if(separateGraphs){
		print("Outputting two graphs")
		matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = 1:4, xlab = xlab, ylab = "Raw Variance", main = "Raw Moderation Effects")
		legend(location, legend = c("Va", "Vc", "Ve", "Vtot"), lty = 1:4, col = 1:4)
		matplot(defVarValues, outStd, type = "l", lty = 1:4, col = 1:4, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main = "Standardized Moderation Effects")
		legend(location, legend = c("Va", "Vc", "Ve"), lty = 1:4, col = 1:4)
		par(mfrow = c(1, 1)) # back to black
	}else{
		par(mfrow = c(2, 1)) # two rows, one column for raw and std variance
		par(mfrow = c(1, 2)) # one row: raw and std variance

		matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = 1:4, xlab = xlab, ylab = "Raw Variance", main = "Raw Moderation Effects")
		legend(location, legend = c("genetic", "unique", "shared", "total"), lty = 1:4, col = 1:4)

		matplot(defVarValues, outStd, type = "l", lty = 1:4, col = 1:4, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main = "Standardized Moderation Effects")
		legend(location, legend = c("genetic", "unique", "shared"), lty = 1:4, col = 1:4)
		par(mfrow = c(1, 1)) # back to black
	}
}