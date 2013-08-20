# https://github.com/hadley/devtools/wiki/Philosophy
# devtools::dev_help("Stouffer.test")
# setwd("~/bin/umx/umx"); devtools::document(); devtools::install(); devtools::load_all()
# devtools::check()

# ====================
# = Updating helpers =
# ====================

#' umxUpdateOpenMx
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  binary factor determining whether to request the beta or relase version (F)
#' @param loadNew binary parameter determining whether to load the library after (optionally) updating
#' @param anyOK minimum version to accept without updating
#' @export
#' @examples
#' \dontrun{
#' umxUpdateOpenMx()
#' }

umxUpdateOpenMx <- function(bleedingEdge = F, loadNew = T, anyOK = F) {
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

# =====================
# = utility functions =
# =====================
#' umxHasCIs
#'
#' umxHasCIs is a utility function to return a binary answer to the question "does this \code{\link{mxModel}} have confidence intervals?" 
#'
#' @param model \code{\link{mxModel}} to check for presence of CIs
#' @return - TRUE or FALSE
#' @export
#' @seealso - \code{\link{mxCI}}, \code{\link{umxRun}}, \code{\link{umxReportCIs}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' \dontrun{
#' umxHasCIs(model)
#' }

umxHasCIs <- function(model) {
	# umxHasCIs(model)
	if(is.null(model@output$confidenceIntervals)){
		hasCIs = F
	} else {
		hasCIs = dim(model@output$confidenceIntervals)[1] >= 0
	}
	return(hasCIs)
}

# How long did that take?
#' umxReportTime
#'
#' umxReportTime Reports how long the model took to execute compactly, without having to run a summary
#'
#' @param model an \code{\link{mxModel}} from which to get the elapsed time
#' @param formatStr A format string, defining how to show the time
#' @param tz The time zone in which the model was executed
#' @export
#' @seealso - \code{\link{summary}}, \code{\link{umxRun}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' \dontrun{
#' umxReportTime(model)
#' }

umxReportTime <- function(model, formatStr= "H %H M %M S %OS3", tz="GMT"){
	# use case
	# umxReportTime(fit1)
	format(.POSIXct(model@output$wallTime,tz), formatStr)
}

#' umxAnovaReport
#'
#' umxAnovaReport is a convenience function to format results for journals. There are others. Bt I made this one.
#' If you give it the output of an lm, it runs anova() and lm.beta(), and puts that together in a regression table...
#' Alternatively if you fill in the optional second model, it compares them (just like \code{\link{umxCompare}})
#' @param model1 an \code{\link{lm}} model to make a table from 
#' @param model2 an (optional) second \code{\link{lm}} model to compare to model 1
#' @param raw should the raw table also be output? (allows checking that nothing crazy is going on)
#' @param format string or markdown format?
#' @param printDIC a boolean toggle whether tou want AIC-type fit change table printed
#' @seealso - \code{\link{umxReportFit}}, \code{\link{umxCompare}}, \code{\link{anova}}, \code{\link{lm.beta}}
#' @references - http://openmx.psyc.virginia.edu/
#' @export
#' @examples
#' model = lm(mpg ~ cyl + disp, data = mtcars)
#' umxAnovaReport(model)

umxAnovaReport <- function(model1, model2 = NULL, raw = T, format = "string", printDIC = F) {
	if(!is.null(model2)){
		# F(-2, 336) =  0.30, p = 0.74
		a = anova(model1, model2)
		if(raw){
			print(a)
		}
		if(format == "string"){
			print( paste0("F(", a[2,"Df"], ",", a[2,"Res.Df"], ") = ",
					round(a[2,"F"],2), ", p ", umx_u_APA_pval(a[2,"Pr(>F)"])
				)
			)
		} else {
			print( paste0("| ", a[2,"Df"], " | ", a[2,"Res.Df"], " | ", 
				round(a[2,"F"],2), " | ", umx_u_APA_pval(a[2,"Pr(>F)"]), " | ")
			)
		}		

	} else {
		a = anova(model1);
		if(require(QuantPsyc, quietly = T)){
			a$beta = c(QuantPsyc::lm.beta(model1), NA);
		} else {
			a$beta = NA
			message("To include beta weights\ninstall.packages(\"QuantPsyc\")")
		}

		x <- c("Df", "beta", "F value", "Pr(>F)");
		a = a[,x]; 
		names(a) <- c("df", "beta", "F", "p"); 
		ci = confint(model1)
		a$lowerCI = ci[,1]
		a$upperCI = ci[,2]
		a <- a[,c("df", "beta", "lowerCI", "upperCI", "F", "p")]; 
		print(a)
		if(printDIC){
			a = drop1(model1); 
			a$DIC = round(a$AIC - a$AIC[1], 2); 
			print(a)	
		}
	}
}

#' print.dataframe
#'
#' print.dataframe is a helper to aid the interpretability of printed tables from OpenMx (and elsewhere).
#' Its most useful characteristic is allowing you to change how NA and zero appear.
#' By default, Zeros have the decimals suppressed, and NAs are suppressed altogether.
#'
#' @param x a data.frame to print
#' @param digits  the number of decimal places to print (defaults to getOption("digits")
#' @param quote  parameter passed to print
#' @param na.print String to replace NA with (default to blank "")
#' @param zero.print String to replace 0.000 with  (defaults to "0")
#' @param justify parameter passed to print
#' @param ... option parameters for print
#' @export
#' @seealso - \code{\link{print}}
#' @examples
#' \dontrun{
#' print.dataframe(model)
#' }
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

# ====================
# = Data and Utility =
# ====================

#' Stouffer.test
#'
#' Runs a Stouffer.test
#'
#' @param p a list of p values, i.e., p(.4, .3, .6, .01)
#' @seealso - 
#' @references - \url{http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/CombiningPvalues}
#' Stouffer, Samuel A., Edward A. Suchman, Leland C. DeVinney, Shirley A. Star, 
#' and Robin M. Williams, Jr. (1949). Studies in Social Psychology in World War II: 
#' The American Soldier. Vol. 1, Adjustment During Army Life. Princeton: Princeton University Press.
#' 
#' Bailey TL, Gribskov M (1998). Combining evidence using p-values: application to sequence
#' homology searches. Bioinformatics, 14(1) 48-54.
#' Fisher RA (1925). Statistical methods for research workers (13th edition). London: Oliver and Boyd.
#' Manolov R and Solanas A (2012). Assigning and combining probabilities in single-case studies.
#' Psychological Methods 17(4) 495-509. Describes various methods for combining p-values including
#' Stouffer and Fisher and the binomial test.
#' \url{http://www.burns-stat.com/pages/Working/perfmeasrandport.pdf}
#' @export
#' @examples
#' Stouffer.test(p = c(.01, .2, .3))

Stouffer.test <- function(p = NULL) {
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

#' umxHetCor
#'
#' umxHetCor Helper to return just the correlations from John Fox's polycor::hetcor function
#'
#' @param data a \code{\link{data.frame}} of columns for which to compute heterochoric correlations
#' @param ML Whether to use Maximum likelihood computation of correlations (default = F)
#' @param use How to delete missing data
#' @return - a matrix of correlations
#' @export
#' @seealso - \code{\link{hetcor}}
#' @references - 
#' @examples
#' \dontrun{
#' cor_mat_ = umxHetCor(df)
#' umxHetCor(data, use="pairwise.complete.obs")
#' }


umxHetCor <- function(data, ML = F, use = "pairwise.complete.obs"){
	if(require(polycor)){
		hetc = polycor::hetcor(data, ML=ML, use=use, std.err=F)
		return(hetc$correlations)
	} else {
		# TODO add error message if polycor not found
		stop("To run umxHetCor, you must install the polycor package\ninstall.packages('polycor')")
	}
}

umxLower2full <- function(lower.data, diag = F, byrow = T) {
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

renameFile <- function(baseFolder = "Finder", findStr = NA, replaceStr = NA, listPattern = NA, test = T, overwrite = F) {
	# renameFile(baseFolder = "~/Downloads/", findStr="", replaceStr="", listPattern = "", test=T)
	# renameFile(baseFolder = NA, findStr="", replaceStr="", listPattern = "", test=T)
	# uppercase = u$1
	if(baseFolder == "Finder"){
		baseFolder = system(intern=T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
		message("Using front-most Finder window:", baseFolder)
	} else if(baseFolder == "") {
		baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep = "") ## choose a directory
		message("Using selected folder:", baseFolder)
	}
	if(is.na(listPattern)){
		listPattern = findStr
	}
	a = list.files(baseFolder, pattern = listPattern)
	message("found ", length(a), " possible files")
	changed = 0
	for (fn in a) {
		findB = grepl(pattern = findStr, fn) #returns 1 if found
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

moveFile <- function(baseFolder = NA, findStr=NA, fileNameList = NA, destFolder = NA, test = T, overwrite = F) {
	# use case: 
	# base = "/Users/tim/Music/iTunes/iTunes Music/"
	# dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
	# moveFile(baseFolder = base, fileNameList = toMove, destFolder = dest, test=F)
	if(is.na(destFolder)){
		stop("destFolder can't be NA")
	}
	if(baseFolder == "Finder"){
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

umx_u_APA_pval <- function(p) {
	# umx_u_APA_pval()
	# umxAnovaReport
	min = .001
	if(p < min){
		return(paste0("< ", min))
	} else {
		return(paste0("= ", round(p, 2)))
	}	
}
