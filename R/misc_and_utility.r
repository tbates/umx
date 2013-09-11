# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); devtools::document(); devtools::install(); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()

# ====================
# = Updating helpers =
# ====================

#' umxUpdateOpenMx
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  A Boolean determining whether to request the beta (TRUE) or relase version (defaults to FALSE)
#' @param loadNew A Boolean parameter determining whether to load the library after (optionally) updating
#' @param anyOK The minimum version to accept without updating
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
#' A utility function to return a binary answer to the question "does this \code{\link{mxModel}} have confidence intervals?" 
#'
#' @param model The \code{\link{mxModel}} to check for presence of CIs
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
#' A functoin to compactly report how long a model took to execute
#'
#' @param model An \code{\link{mxModel}} from which to get the elapsed time
#' @param formatStr A format string, defining how to show the time
#' @param tz The time zone in which the model was executed
#' @export
#' @seealso - \code{\link{summary}}, \code{\link{umxRun}}
#' @references - \url{http://openmx.psyc.virginia.edu}
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
#' @param model1 An \code{\link{lm}} model to make a table from 
#' @param model2 An (optional) second \code{\link{lm}} model to compare to model 1
#' @param raw Should the raw table also be output? (allows checking that nothing crazy is going on)
#' @param format String or markdown format?
#' @param printDIC A Boolean toggle whether tou want AIC-type fit change table printed
#' @seealso - \code{\link{umxSummary}}, \code{\link{umxCompare}}, \code{\link{anova}}, \code{\link{lm.beta}}
#' @references - \url{http://openmx.psyc.virginia.edu}
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
#' A helper to aid the interpretability of printed tables from OpenMx (and elsewhere).
#' Its most useful characteristic is allowing you to change how NA and zero appear.
#' By default, Zeros have the decimals suppressed, and NAs are suppressed altogether.
#'
#' @param x A data.frame to print
#' @param digits  The number of decimal places to print (defaults to getOption("digits")
#' @param quote  Parameter passed to print
#' @param na.print String to replace NA with (default to blank "")
#' @param zero.print String to replace 0.000 with  (defaults to "0")
#' @param justify Parameter passed to print
#' @param ... Optional parameters for print
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
#' @param p A list of p values, i.e., p(.4, .3, .6, .01)
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
#' @param data A \code{\link{data.frame}} of columns for which to compute heterochoric correlations
#' @param ML Whether to use Maximum likelihood computation of correlations (default = F)
#' @param use How to delete missing data
#' @return - A matrix of correlations
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
		hetc = polycor::hetcor(data, ML = ML, use = use, std.err = F)
		return(hetc$correlations)
	} else {
		# TODO add error message if polycor not found
		stop("To run umxHetCor, you must install the polycor package\ninstall.packages('polycor')")
	}
}

#' umxLower2full
#'
#' Take a lower triangle of data (either from a "lower" \code{\link{mxMatrix}}, or entered from  as you might see in a journal article) 
#' and turn it into a full matrix
#' 
#' @param lower.data An \code{\link{mxMatrix}}
#' @param diag A boolean noting whether the lower matrix includes the diagonal
#' @param byrow Whether the matrix is to be filled by row or by column
#' @return - \code{\link{mxMatrix}}
#' 
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' matrix = umxLower2full(matrix)
#' lower2full(lower.tri, diag = F)
#' lower2full(lower.data, diag = T, byrow = F)
#' lower2full(lower.no.diag, diag = F, byrow = F)
#' lower2full(lower.bycol, diag = T, byrow = F)
#' lower2full(lower.byrow, diag = T, byrow = T)
#' }


umxLower2full <- function(lower.data, diag = F, byrow = T) {
	len = length(lower.data)
	if(diag) {
		# len*2 = ((x+.5)^2)-.25
		size = len * 2
		size = size + .25
		size = sqrt(size)
		size = size - .5; size
	}else{
		# len = (x*((x+1)/2))-x	
		# .5*(x-1)*x
		size = len * 2
		# (x-.5)^2 - .25
		size= size + .25
		size = sqrt(size)
		size = size + .5; size
	}
	mat = diag(size)
	if(byrow){
		# put  data into upper triangle, then transform to lower
		mat[upper.tri(mat, diag = diag)] <- lower.data;
		mat[lower.tri(mat, diag = F)] <- mat[upper.tri(mat, diag = F)]
	}else{                            
		mat[lower.tri(mat, diag = diag)] <- lower.data;
		mat[upper.tri(mat, diag = F)] <-mat[lower.tri(mat, diag = F)]
	}
	return(mat)
}

umxFindObject <- function(grepString = ".*", requiredClass = "MxModel") {
	# Purpose: find objects a certain class, whose name matches a (grep) search string
	# Use case: umxFindObject("Chol*", "MxModel")
	# umxFindObject("", "MxModel")
	matchingNames = ls(envir = sys.frame(-1), pattern = grepString) # envir
	matchingObjects = c()
	for (obj in matchingNames) {
		if(class(get(obj))[1] == requiredClass){
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
		baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
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

moveFile <- function(baseFolder = NA, findStr = NA, fileNameList = NA, destFolder = NA, test = T, overwrite = F) {
	# use case: 
	# base = "/Users/tim/Music/iTunes/iTunes Music/"
	# dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
	# moveFile(baseFolder = base, fileNameList = toMove, destFolder = dest, test=F)
	if(is.na(destFolder)){
		stop("destFolder can't be NA")
	}
	if(baseFolder == "Finder"){
		baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
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

# Return the maximum value in a row
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

col.as.numeric <- function(df) {
	# use case
	# col.as.numeric(df)
	for (i in names(df)) {
		df[,i] = as.numeric(df[,i])
	}
	return(df)
}

swapABlock <- function(twinData, rowSelector, T1Names, T2Names) {
	# test = data.frame(a=paste("a",1:10,sep=""),b=paste("b",1:10,sep=""), c=paste("c",1:10,sep=""),d=paste("d",1:10,sep=""),stringsAsFactors=F)
	# swapABlock(test, rowSelector=c(1,2,3,6), T1Names="b", T2Names="c")
	# swapABlock(test, rowSelector=c(1,2,3,6), T1Names=c("a","c"), T2Names=c("b","d"))
	theRows = twinData[rowSelector,]
	old_BlockTwo = theRows[,T2Names]
	theRows[,T1Names] -> theRows[,T2Names]
	theRows[,T1Names] <- old_BlockTwo
	twinData[rowSelector,] <- theRows
	return(twinData)
}

#' umx_u_APA_pval
#'
#' round a p value so you get < .001 instead of .000000002 or .134E-16
#'
#' @param p A p-value to round
#' @param min Threshold to say < min
#' @param rounding Number of decimal to round to 
#' @param addComparison Whether to return the bare number, or to add the appropriate comparison symbol (= <)
#' @return - a value
#' @export
#' @seealso - \code{\link{round}}
#' @examples
#' \dontrun{
#' umx_u_APA_pval(1.23E3)
#' }

umx_u_APA_pval <- function(p, min = .001, rounding = 3, addComparison=T) {
	if(p < min){
		if(addComparison){
			return(paste0("< ", min))
		} else {
			return(min)
		}
	} else {
		if(addComparison){
			return(paste0("= ", round(p, rounding)))
		} else {
			return(round(p, rounding))
		}
	}	
}


rename <- function (x, replace, old = NA) {
	# add to help: see also gdate::rename.vars(data, from, to)	
	# rename(x, replace = c(ages = "age"))
	# rename(x, old= c("ages"), replace = c("age"))
	if(typeof(old)=="character"){
		# message("replacing old with replace")
		if(length(old)!=length(replace)){
			stop("you are trying to replace ", length(old), " old names with ", length(replace), "new names: Lengths must match")
		}
		names_to_replace <- old
		new_names_to_try <- replace
	} else {
		names_to_replace <- names(replace)
		new_names_to_try <- unname(replace)
	}
	old_names <- names(x)

	if(!all(names_to_replace %in% old_names)) {
		warning("The following names did not appear in the dataframe:", 
		paste(names_to_replace[!names_to_replace %in% old_names], collapse=", "), "\nperhaps you already updated them")
	}

	if(anyDuplicated(names_to_replace)) {
	  err <- paste("You are trying to update the following names more than once:", 
	           paste(names_to_replace[duplicated(names_to_replace)], collapse=", "))
	  stop(err)
	}

	if(anyDuplicated(new_names_to_try)) {
	  err <- paste("You have the following duplicates in your replace list:", 
	         	paste(new_names_to_try[duplicated(new_names_to_try)], collapse=", "))
	  stop(err)
	}
	new_names <- new_names_to_try[match(old_names, names_to_replace)]  
	setNames(x, ifelse(is.na(new_names), old_names, new_names))
}



grepSPSS_labels <- function(df, grepString, output="both", ignore.case=T, useNames=F) {
	# output = "both", "label" or "name"
	# grepSPSS_labels(relig, "Race", output="both", ignore.case=T) 
	# grepSPSS_labels(relig, "race", output="label") 
	# grepSPSS_labels(relig, "race", output="name") 
	# need to check this exists
	vLabels = attr(df,"variable.labels") # list of descriptive labels?
	a       = names(df) 
	if(is.null(vLabels)){
		stop("no labels")
	}
	if(useNames) {
		findIndex = grep(grepString,a, value=F, ignore.case=ignore.case)
		return( as.matrix(vLabels[findIndex]))
	} else {
		# need to cope with finding nothing
		findIndex = grep(grepString,vLabels, value=F, ignore.case=ignore.case)
		if(output=="both") {
			theResult <- as.matrix(vLabels[findIndex])
		} else if(output=="label"){
			vLabels= as.vector(vLabels[findIndex])
			theResult <- (vLabels)
		} else if(output=="name"){
			theResult <- names(vLabels)[findIndex]
		}else{
			stop(paste("bad choice of output:", output))
		}
		if(dim(theResult)[1]==0 |is.null(theResult)){
			cat("using names!!!!\n")
			findIndex = grep(grepString,a, value=F, ignore.case=ignore.case)
			return(as.matrix(vLabels[findIndex]))
		} else {
			return(theResult)
		}
	}
}



# ======================
# = Comparison helpers =
# ======================

#' umxGreaterThan
#'
#' A version of less-than that excludes NA as a match
#'
#' \alias "%<%"
#' @export
#' @seealso - \code{\link{%>%}}, 
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' c(1:3,NA,5) = %<% 2 

"%<%"<- function(table, x){
	lessThan = table<  x
	lessThan[is.na(lessThan)] = FALSE
	return(lessThan)
}

#' umxGreaterThan
#'
#' A version of greater-than that excludes NA as a match
#'
#' \alias "%>%" 
#' @export
#' @seealso - \code{\link{%<%}}, 
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' c(1:3,NA,5) = %>% 2 

"%>%"<- function(table, x){
	moreThan = table<  x
	moreThan[is.na(moreThan)] = FALSE
	return(moreThan)
}

# =====================
# = Utility functions =
# =====================

round.num <- function(x, digits) {
	# foreach column, if numeric, round
	rows = dim(x)[1]
	cols = dim(x)[2]
	for(r in rows) {
		for(c in cols) {
			if(is.numeric(x[r,c])){
				x[r,c] = round(x[r,c],digits)
			}
		}
	}
	return(x)
}

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# R2HTML::HTML(a, file="tables.html"); system("open tables.html")

print.html <- function(x, rounding = 3, title=grep(".*\\b",deparse(substitute(x)),value=T),headings=colnames(x), align = paste0(rep('c',ncol(x)), collapse = ''), halign=paste(rep('c',ncol(x)),collapse=''), cgroup=NULL, n.cgroup=NULL, cgroup.just=rep("c",length(n.cgroup)), rgroup=NULL, n.rgroup=NULL, rowlabel=title, ctable=F, caption=NULL, caption.loc='top', label=title, output = "Rout.html",...) {
	# usage: htmlTable(x, output = "Rout.html")
	# options for output = c("Rout.html","cat","return")

    table_str <- "<table class='gmisc_table' style='border-top: 2px solid grey; border-bottom: 2px solid grey;'>"
    
  if (length(label) > 0){ table_str <- sprintf("%s\n\t<a name='%s'></a>", table_str, label) }

  # Not quite as intended but close enough
  if(length(list(...))) x <- format.df(x, numeric.dollar=FALSE, ...)
  # Remove some specifics for LaTeX
  if (is.character(x)){
  	x <- matrix(str_replace(x, "\\\\%", "%"), ncol=ncol(x))
  }
  if (length(caption) > 0){
    if (caption.loc == "bottom"){
      table_str <- sprintf("%s\n\t<caption align=bottom>", table_str)
    }else{
      table_str <- sprintf("%s\n\t<caption align=top>", table_str)
    }
    table_str <- sprintf("%s%s</caption>", table_str, caption)
  }

  set_rownames = (length(rownames(x)) > 0)
  # Add the cgroup table header
  if (length(cgroup) > 0){
    if (length(n.cgroup) == 0 && ncol(x) %% length(cgroup) == 0){
      n.cgroup <- rep(ncol(x)/length(cgroup), times=length(cgroup))
    }else if(sum(n.cgroup) != ncol(x)){
      stop(sprintf("Your columns don't match in the n.cgroup, i.e. %d != %d", sum(n.cgroup), ncol(x)))
    }
    table_str <- sprintf("%s\n\t<tr>", table_str)
    if (set_rownames && length(rowlabel) > 0){
      table_str <- sprintf("%s\n\t\t<th style='font-weight: 900;'>%s</th>", table_str, rowlabel)
    }
    for (i in 1:length(cgroup)){
      table_str <- sprintf("%s\n\t\t<th colspan=%d style='font-weight: 900; border-bottom: 1px solid grey;'>%s</th>", table_str, n.cgroup[i], cgroup[i])
      if (i != length(cgroup))
        table_str <- sprintf("%s<th>&nbsp;</th>", table_str)
    }
    table_str <- sprintf("%s\n\t</tr>", table_str)
  }
  addCells <- function(table_str, rowcells, cellcode, align){
    cgroup_iterator <- 0
    for (nr in 1:length(rowcells)){
      if (length(cgroup) > 0){
        if (cgroup_iterator > 0){
          if (sum(n.cgroup[1:cgroup_iterator]) < nr ){
            table_str <- sprintf("%s\n\t\t<%s>&nbsp</%s>", table_str, cellcode, cellcode)
            cgroup_iterator = cgroup_iterator + 1
          }
        }else{
          cgroup_iterator = cgroup_iterator + 1
        }
      }
      table_str <- sprintf("%s\n\t\t<%s align='%s'>%s</%s>", table_str, cellcode, align, rowcells[nr], cellcode)
    }
    return (table_str)
  }

  # Add the headings
  if (length(headings) > 0){
    table_str <- sprintf("%s\n\t<tr style='border-bottom: 1px solid grey;'>", table_str)
    if (set_rownames && length(cgroup) == 0  && length(rowlabel) > 0){
      table_str <- sprintf("%s\n\t\t<th style='font-weight: 900;'>%s</th>", table_str, rowlabel)
    }else if(set_rownames){
      table_str <- sprintf("%s\n\t\t<th>&nbsp;</th>", table_str)
    }
    table_str <- addCells(table_str = table_str, rowcells = headings, cellcode = "th", align="center")
    table_str <- sprintf("%s\n\t</tr>", table_str)
  }
  if (length(rgroup) > 0 && sum(n.rgroup) !=  nrow(x)){
  	stop(sprintf("Your rows don't match in the n.rgroup, i.e. %d != %d", sum(n.rgroup), nrow(x)))
  }
  rgroup_iterator <- 0
  for (row_nr in 1:nrow(x)){
    if (length(rgroup) > 0){
      if (rgroup_iterator == 0){
        rgroup_iterator = rgroup_iterator + 1
        table_str <- sprintf("%s\n\t<tr><td colspan=%d style='font-weight: 900'>%s</tr>", table_str, 
          ncol(x)+set_rownames, rgroup[rgroup_iterator])
      }else if(row_nr > sum(n.rgroup[1:rgroup_iterator])){
        rgroup_iterator = rgroup_iterator + 1
        table_str <- sprintf("%s\n\t<tr><td colspan=%d style='font-weight: 900; border-top: 1px solid grey;'>%s</tr>", table_str, 
                             ncol(x)+set_rownames, rgroup[rgroup_iterator])
      }
    }
    table_str <- sprintf("%s\n\t<tr>", table_str)
    if (set_rownames){
      if (rgroup_iterator > 0)
        table_str <- sprintf("%s\n\t\t<td style='padding-left: .5em;'>%s</td>", table_str, rowname[row_nr])
      else
        table_str <- sprintf("%s\n\t\t<td>%s</td>", table_str, rowname[row_nr])
    }
    table_str <- addCells(table_str = table_str, rowcells = round(x[row_nr,],rounding), cellcode = "td", align="right")
    table_str <- sprintf("%s\n\t</tr>", table_str)
  }
  table_str <- sprintf("%s\n</table>", table_str)
  if (output=="cat"){
    cat(table_str)
  }else if (output == "return"){
    return(table_str)
  }else{
	cat(table_str, file = output)
	system(paste("open '", output, "'", sep=""));
  }
}

# extracted from Rcmdr
reliability <-function (S){
     reliab <- function(S, R) {
         k <- dim(S)[1]
         ones <- rep(1, k)
         v <- as.vector(ones %*% S %*% ones)
         alpha <- (k/(k - 1)) * (1 - (1/v) * sum(diag(S)))
         rbar <- mean(R[lower.tri(R)])
         std.alpha <- k * rbar/(1 + (k - 1) * rbar)
         c(alpha = alpha, std.alpha = std.alpha)
     }
     result <- list()
     if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S)) || any(abs(S - t(S)) > max(abs(S)) * 1e-10) || nrow(S) < 2)
         stop("argument must be a square, symmetric, numeric covariance matrix")
     k <- dim(S)[1]
     s <- sqrt(diag(S))
     R <- S/(s %o% s)
     rel <- reliab(S, R)
     result$alpha <- rel[1]
     result$st.alpha <- rel[2]
     if (k < 3) {
         warning("there are fewer than 3 items in the scale")
         return(invisible(NULL))
     }
     rel <- matrix(0, k, 3)
     for (i in 1:k) {
         rel[i, c(1, 2)] <- reliab(S[-i, -i], R[-i, -i])
         a <- rep(0, k)
         b <- rep(1, k)
         a[i] <- 1
         b[i] <- 0
         cov <- a %*% S %*% b
         var <- b %*% S %*% b
         rel[i, 3] <- cov/(sqrt(var * S[i, i]))
     }
     rownames(rel) <- rownames(S)
     colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
     result$rel.matrix <- rel
     class(result) <- "reliability"
     result
}

print.reliability <- function (x, digits = 4, ...){
     cat(paste("Alpha reliability = ", round(x$alpha, digits), "\n"))
     cat(paste("Standardized alpha = ", round(x$st.alpha, digits), "\n"))
     cat("\nReliability deleting each item in turn:\n")
     print(round(x$rel.matrix, digits))
     invisible(x)
}

anova.report.F <- function(model, precision=3) {
	# useage
	# anova.report.F(model) # "F(495,1) = 0.002"
	# or
	# 1anova.report.F(anova(m1, m2))
	tmp = class(model)
	if(all(tmp=="lm")){
		a = summary(model)
		dendf = a$fstatistic["dendf"]
		numdf  = a$fstatistic["numdf"]
		value = a$fstatistic["value"]
		return(paste0("F(", dendf, ",",numdf,") = " ,round(value,precision), ",p = ", round(pf(value,numdf,dendf, lower.tail=F),precision)))
	} else {
		if(model[2, "Res.Df"] > model[1, "Res.Df"]){
			message("Have you got the models the right way around?")
		}
		paste0(
			"F(", 
			round(model[2, "Res.Df"]), ",",
			round(model[2,"Df"]), ") = ",
			round(model[2,"F"],precision), ", p = ",
			round(model[2,"Pr(>F)"], precision)
		)
	}	
}

# =====================
# = Statistical tools =
# =====================


# Test differences in Kurtosis and Skewness
kurtosisDiff <- function(x, y, B = 1000){
	require(psych)
	kx <- replicate(B, kurtosi(sample(x, replace = TRUE)))
	ky <- replicate(B, kurtosi(sample(y, replace = TRUE)))
	return(kx - ky)	
}
# Skew
skewnessDiff<- function(x, y, B = 1000){
	require(psych)
	sx <- replicate(B, skew(sample(x, replace = TRUE)))
	sy <- replicate(B, skew(sample(y, replace = TRUE)))
	return(sx - sy)	
}

# get the S3 and its parameter list with
# seq.default()


#' @title umx_pp33
#' @description
#' A utility function to compute the critical value of student (xc) that
#' gives p = .05 when df = df_xc = qt(p = .975, df = target_df)
#' 
#' @details
#' Find noncentrality parameter (ncp) that leads 33% power to obtain xc
#' The original is published at p-curve
#' \url{http://www.p-curve.com/Supplement/R/pp33.r} 
#' 
#' Find noncentrality parameter (ncp) that leads 33% power to obtain xc
#'
#' @param target_df the... TODO
#' @param x the... TODO
#' @return - value
#' @export
#' @seealso - \code{\link{pnorm}}
#' @references - \url{http://www.p-curve.com/Supplement/R/pp33.r}
#' @examples
#' # To find the pp-value for 33% power for a t(38)=2.4, execute 
#' umx_pp33(target_df = 38, x = 2.4)

umx_pp33 <- function(target_df, x) {
	f <- function(delta, pr, x, df){
		pt(x, df = df, ncp = delta) - pr
	}
	# Find critical value of student (xc) that gives p=.05 when df = target_df
	xc = qt(p = .975, df = target_df)		
	# Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	out <- uniroot(f, c(0, 37.62), pr =2/3, x = xc, df = target_df)	
	ncp_ = out$root	
	# Find probability of getting x_ or larger given ncp
	p_larger = pt(x, df = target_df, ncp = ncp_)
	# Condition on p < .05 (i.e., get pp-value)
	pp = 3 * (p_larger - 2/3)
	# Print results
	return(pp)
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
#' @export
#' @seealso - \code{\link{plyr}}
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
    datac <- ddply(data, groupvars, .drop = .drop,
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
    datac    <- rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
}