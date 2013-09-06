# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); devtools::document(); devtools::install(); 
# setwd("~/bin/umx"); devtools::check()
# setwd("~/bin/umx"); devtools::release()
# devtools::load_all()
# devtools::dev_help("umxX")
# devtools::show_news()

# =============================
# = Fit and Reporting Helpers =
# =============================
#' umxCompare
#'
#' umxCompare compares two or more \code{\link{mxModel}}s. If you leave comparison blank, it will just give fit info for the base model
#'
#' @param base The base \code{\link{mxModel}} for comparison
#' @param comparison The model (or list of models) which will be compared for fit with the base model (can be empty)
#' @param all Whether to make all possible comparisons if there is more than one base model (defaults to T)
#' @param output Optionally output to an html table which will open your default browser: handy for getting tables into word
#' @seealso - \code{\link{mxCompare}}, \code{\link{umxSummary}}, \code{\link{umxRun}},
#' @references - \url{http://openmx.psyc.virginia.edu/}
#' @export
#' @import OpenMx
#' @examples
#' \dontrun{
#' umxCompare(model1, model2)
#' umxCompare(model1, c(model2, model3))
#' }

umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, output = "return") {
	# output != "return"is interpreted as a file to write html too...
	# umxCompare(fit11, fit11, all=F, output="Rout.html")
	# TODO eliminate this once mxCompare finally updates...
	if(is.null(comparison)){
		comparison <- base
	} else if (is.null(base)) {
		stop("You must provide at least a base model for umxCompare")
	}
	tableOut  = OpenMx::mxCompare(base = base, comparison = comparison, all = all)
	tableOut  = format(tableOut, scientific = F, digits = 5)
	tableOut  = tableOut[, c(2:1, 3, 4, 6:9)]
	names(tableOut) <- c("Comparison", "Base", "ep", "-2LL", "AIC", "delta LL", "delta df", "p")
	if(output == "return"){
		return(tableOut)
	} else {
		print.html(tableOut, output = output, rowlabel="")
	}
	# if(export){
	# 	fName= "Model.Fitting.xls"
	# 	write.table(tableOut,fName, row.names=F,sep="\t", fileEncoding="UTF-8") # macroman UTF-8 UTF-16LE
	# 	system(paste("open", fName));
	# }
}

#' umxSummary
#'
#' Report the fit of a model in a compact form suitable for a journal. Emits a "warning" not 
#' when model fit is worse than accepted criterion (TLI > .95 and RMSEA < .06; (Hu & Bentler, 1999; Yu, 2002).
#'
#' @param model The \code{\link{mxModel}} whose fit will be reported
#' @param saturatedModels Saturated models if needed for fit indices (see example below: 
#' Only needed for raw data, and then not if you've run umxRun
#' @param report The format for the output line or table (default is "line")
#' @param showEstimates What estimates to show. Options are "raw|std|list|NULL" for raw, standardized, a custom list or (default)
#' none (just shows the fit indices)
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance 
#'  structure analysis: Coventional criteria versus new alternatives. Structural Equation Modeling, 6, 1-55. 
#'
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models
#'  with binary and continuous outcomes. University of California, Los Angeles, Los Angeles.
#'  Retrieved from \url{http://www.statmodel.com/download/Yudissertation.pdf}
#'  
#' \url{http://openmx.psyc.virginia.edu}
#' @export
#' @import OpenMx
#' @examples
#' \dontrun{
#' umxSummary(m1)
#' umxSummary(m1, report = "table")
#' umxSummary(m1, saturatedModels = umxSaturated(m1))
#' }

umxSummary <- function(model, saturatedModels = NULL, report = "line", showEstimates = NULL, precision = 2){
	# report = "line|table"
	# showEstimates = "NULL|raw|std|both|c("row", "col", "Std.Estimate")"
	# c("names", "Std.Estimate")
	# TODO make table take lists of models...
	# TODO should/could have a toggle for computing the saturated models...

	output <- model@output
	# stop if there is no objective function
	if ( is.null(output) ) stop("Provided model has no objective function, and thus no output. mxRun(model) first")
	# stop if there is no output
	if ( length(output) < 1 ) stop("Provided model has no output. I can only standardize models that have been mxRun() first!")
	
	if(is.null(saturatedModels)){
		# saturatedModels not passed in from outside, so get them from the model
		modelSummary = OpenMx::summary(model)
		
		if(is.na(modelSummary$SaturatedLikelihood)){
			message("There is no saturated likelihood: computing that now...")
			saturatedModels = umxSaturated(model)
			modelSummary = OpenMx::summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
		}
	} else {
		modelSummary = OpenMx::summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
	}

	# displayColumns
	if(!is.null(showEstimates)){
		if("Std.Estimate" %in%  names(modelSummary$parameters)){
			if(length(showEstimates) >1) {
				namesToShow = showEstimates
			}else if(showEstimates == "both") {
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error", "Std.Estimate", "Std.SE")
			} else if(showEstimates == "std"){
				namesToShow = c("name", "matrix", "row", "col", "Std.Estimate", "Std.SE")
			}else{
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")					
			}
		} else {
			namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")
		}
		print(modelSummary$parameters[,namesToShow], digits= precision, na.print = "", zero.print = "0", justify = "none")
	} else {
		message("For estimates, add showEstimates = 'raw' or 'std' etc")
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
				x = paste0(
					"\u03C7\u00B2(", degreesOfFreedom, ") = ", round(Chi, 2), # was A7
					", p "    , umx_u_APA_pval(p, .001, 3),
					"; CFI = "  , round(CFI,3),
					"; TLI = "  , round(TLI,3),
					"; RMSEA = ", round(RMSEA, 3)
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
}

umxtmp <- function(model){
	summary(model)
}

#' umxReportCIs
#'
#' umxReportCIs umxReportCIs adds mxCI() calls for all free parameters in a model, 
#' runs the CIs, and reports a neat summary.
#'
#' @param model The \code{\link{mxModel}} you wish to report \code{\link{mxCI}}s on
#' @param addCIs Whether or not to add mxCIs if none are found (defaults to TRUE)
#' @param runCIs Whether or not to run the CIs: if F, this function can simply add CIs and return the model. Valid values = "no", "yes", "if necessary"
#' all the CIs and return that model for \code{\link{mxRun}}ning later
#' @return - \code{\link{mxModel}}
#' @seealso - \code{\link{mxCI}}, \code{\link{umxLabel}}, \code{\link{umxRun}}
#' @references - http://openmx.psyc.virginia.edu/
#' @export
#' @examples
#' \dontrun{
#' umxReportCIs(model)
#' }

umxReportCIs <- function(model = NA, addCIs = T, runCIs = "if necessary") {
	# TODO add code to not-run CIs
	if(is.na(model)){
		message("umxReportCIs adds mxCI() calls for all free parameters in a model, runs them, and reports a neat summary. A use example is:\n umxReportCIs(model)")
		stop();
	}
	message("### CIs for model ", model@name)
	if(addCIs){
		CIs   = names(omxGetParameters(model))
		model = mxModel(model, mxCI(CIs))
	}
	runCIs = "yes"
	
	if( (runCIs == "yes") | (umxHasCIs(model) & runCIs != "no") ) {
		model = mxRun(model, intervals = T)
	}

	if(umxHasCIs(model)){
		model_summary = summary(model)
		model_CIs = round(model_summary$CI, 3)
		model_CI_OK = model@output$confidenceIntervalCodes
		colnames(model_CI_OK) <- c("lbound Code", "ubound Code")
		model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
		print(model_CIs)
	}
	invisible(model)
}

#' umxGraph_RAM
#'
#' A function to create graphical path diagrams from your OpenMx models!
#'
#' @param model an \code{\link{mxModel}} to make a path diagram from
#' @param std Whether to standardize the model.
#' @param precision The number of decimal places to add to the path coefficients
#' @param dotFilename A file to write the path model to. if you leave it at the 
#' default "name", then the model's internal name will be used
#' @param pathLabels whether to show labels on the paths. both will show both the parameter and the label. ("both", "none" or "labels")
#' @param showFixed whether to show fixed paths (defaults to FALSE)
#' @param showError whether to show errors
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umxGraph_RAM(model)
#' }

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

#' umxMI
#'
#' umxMI A function to compute and report modifications which would improve fit.
#' You will probably use \code{\link{umxMI_top}} instead
#'
#' @param model an \code{\link{mxModel}} to derive modification indices for
#' @param vector = Whether to report the results as a vector default = TRUE
#' @seealso - \code{\link{umxMI_top}}, \code{\link{umxAdd1}}, \code{\link{umxDrop1}}, \code{\link{umxRun}}, \code{\link{umxSummary}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @export
#' @examples
#' \dontrun{
#' umxMI(model)
#' }

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

#' umxMI
#'
#' umxMI A function to report the top modifications which would improve fit.
#'
#' @param model An \code{\link{mxModel}} for which to report modification indices
#' @param numInd How many modifications to report
#' @param typeToShow Whether to shown additions or deletions (default = "both")
#' @param decreasing How to sort (default = T, decreasing)
#' @param cache = Future function to cahce these time-consuming results
#' @seealso - \code{\link{umxAdd1}}, \code{\link{umxDrop1}}, \code{\link{umxRun}}, \code{\link{umxSummary}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @export
#' @examples
#' \dontrun{
#' umxMI_top(model)
#' umxMI_top(model, numInd=5, typeToShow="add") # valid options are "both|add|delete"
#' }

umxMI <- function(model = NA, numInd = 10, typeToShow = "both", decreasing = T, cache = T) {
	# depends on xmuMI(model)
	if(typeof(model) == "list"){
		mi.df = model
	} else {
		mi = umxMI(model, vector = T)
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
#' @references - http://openmx.psyc.virginia.edu/
#' @export
#' @examples
#' \dontrun{
#' umxUnexplainedCausalNexus(model)
#' }

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

umxConditionalsFromModel <- function(model, newData = NULL, returnCovs = F, meanOffsets = F) {
	# original author: [Timothy Brick](http://openmx.psyc.virginia.edu/users/tbrick)
	# [history](http://openmx.psyc.virginia.edu/thread/2076)
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

umxComputeConditionals <- function(sigma, mu, current, onlyMean = F) {
	# Usage: umxComputeConditionals(model, newData)
	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	# original author: [Timothy Brick](http://openmx.psyc.virginia.edu/users/tbrick)
	# [history](http://openmx.psyc.virginia.edu/thread/2076)
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

