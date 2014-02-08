# utility naming convention: "umx_" prefix, lowercase, and "_" not camel case for word boundaries
# so umx_swap_a_block()

# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); 
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx"); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()

# =====================
# = utility functions =
# =====================

accumulate <- function(FUN = nlevels, fromEach = "column", of_DataFrame = ordinalColumns) {
	# accumulate(nlevels, fromEach = "column", of_DataFrame = ordinalColumns)
	if(! (fromEach %in% c("column", "row"))){
		stop(paste("fromEach must be either column or row, you gave me", fromEach))
	}
	out = c()
	if(fromEach == "column"){
		for(n in 1:ncol(of_DataFrame)){
			out[n] = nlevels(of_DataFrame[,n])
		}
	} else {
		for(n in 1:nrow(of_DataFrame)){
			out[n] = nlevels(of_DataFrame[n,])
		}
	}
	return(out)
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

# ====================
# = Updating helpers =
# ====================

#' umx_update_OpenMx
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  A Boolean determining whether to request the beta (TRUE) or relase version (defaults to FALSE)
#' @param loadNew A Boolean parameter determining whether to load the library after (optionally) updating
#' @param anyOK The minimum version to accept without updating
#' @export
#' @examples
#' \dontrun{
#' umx_update_OpenMx()
#' }

umx_update_OpenMx <- function(bleedingEdge = F, loadNew = T, anyOK = F) {
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
		install.packages('OpenMx', repos = 'http://openmx.psyc.virginia.edu/testing/');
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

# ===================================
# = Ordinal/Threshold Model Helpers =
# ===================================

# ====================
# = Data and Utility =
# ====================

#' umxHetCor
#'
#' umxHetCor Helper to return just the correlations from John Fox's polycor::hetcor function
#'
#' @param data A \code{\link{data.frame}} of columns for which to compute heterochoric correlations
#' @param ML Whether to use Maximum likelihood computation of correlations (default = F)
#' @param use How to delete missing data: "complete.obs", "pairwise.complete.obs" Default is pairwise.complete.obs
#' @param treatAllAsFactor Whether to treat all columns as factors, whether they are currently or not.
#' @param verbose How much to tell the user about what was done.
#' @return - A matrix of correlations
#' @export
#' @seealso - \code{\link{hetcor}}
#' @references - 
#' @examples
#' \dontrun{
#' cor_df = umxHetCor(df)
#' cor_df = umxHetCor(df, ML = F, use="pairwise.complete.obs")
#' }

umxHetCor <- function(data, ML = F, use = "pairwise.complete.obs", treatAllAsFactor=F, verbose=F){
	if(treatAllAsFactor){
		n = ncol(data)
		for (i in 1:n) {
			data[,i] = factor(data[,i])
		}
	}
	if(require(polycor)){
		hetc = polycor::hetcor(data, ML = ML, use = use, std.err = F)
		if(verbose){
			print(hetc)
		}
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

# ===========
# = Utility =
# ===========

#' umx_find_object
#'
#' Find objects a certain class, whose name matches a search string.
#' The string (pattern) is grep-enabled, so you can match wild-cards
#'
#' @param pattern the pattern that matching objects must contain
#' @param requiredClass the class of object that will be matched
#' @return - a list of objects matching the class and name
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - 
#' @examples
#' \dontrun{
#' umx_find_object("^m[0-9]") # mxModels beginning "m1" etc.
#  umx_find_object("", "MxModel") # all MxModels
#' }


umx_find_object <- function(pattern = ".*", requiredClass = "MxModel") {
	# Use case: umxFindObject("Chol*", "MxModel")
	matchingNames = ls(envir = sys.frame(-1), pattern = pattern) # envir
	matchingObjects = c()
	for (obj in matchingNames) {
		if(class(get(obj))[1] == requiredClass){
			matchingObjects = c(matchingObjects, obj)
		}
	}
	return(matchingObjects)
}

#' umx_rename_file
#'
#' rename files. On OS X, the function can access the current frontmost Finder window.
#' The file renaming is fast and, because you can use regular expressions, powerful
#'
#' @param baseFolder  The folder to search in. If set to "Finder" (and you are 
#' on OS X) it will use the current frontmost Finder window. If it is blank, a choose folder dialog will be thrown.
#' @param findStr = The string to find
#' @param replaceStr = The replacement string
#' @param listPattern = A pre-filter for files
#' @param test Boolean determining whether to chagne the names, or just report on what would have happened
#' @param overwrite Boolean determining 
#' @return - 
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umx_rename_file(baseFolder = "~/Downloads/", findStr = "", replaceStr = "", test = T)
#' umx_rename_file(baseFolder = "Finder", findStr = "[Ss]eason +([0-9]+)", replaceStr="S\1", test = T)
#' }
umx_rename_file <- function(baseFolder = "Finder", findStr = NA, replaceStr = NA, listPattern = NA, test = T, overwrite = F) {
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
		findB = grepl(pattern = findStr, fn) # returns 1 if found
		if(findB){
			fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
			if(test){
				message("would change ", fn, " to ", fnew)	
			} else {
				if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
					message("renaming ", fn, "to", fnew, "failed as already exists. To overwrite set T")
				} else {
					file.rename(paste(baseFolder, fn, sep = ""), paste(baseFolder, fnew, sep = ""))
					changed = changed + 1;
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

#' umx_move_file
#'
#' move files. On OS X, the function can access the current frontmost Finder window.
#' The file moves are fast and, because you can use regular expressions, powerful
#'
#' @param baseFolder  The folder to search in. If set to "Finder" (and you are 
#' on OS X) it will use the current frontmost Finder window. If it is blank, a choose folder dialog will be thrown.
#' @param findStr = The string to find
#' @param replaceStr = The replacement string
#' @param listPattern = A pre-filter for files
#' @param test Boolean determining whether to chagne the names, or just report on what would have happened
#' @param overwrite Boolean determining 
#' @return - 
#' @export
#' @seealso - \code{\link{umx_rename_file}}, \code{\link{file.rename}}
#' @examples
#' \dontrun{
#' base = "/Users/tim/Music/iTunes/iTunes Music/"
#' dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
#' umx_move_file(baseFolder = base, fileNameList = toMove, destFolder = dest, test=F)
#' }
umx_move_file <- function(baseFolder = NA, findStr = NA, fileNameList = NA, destFolder = NA, test = T, overwrite = F) {
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

#' umx_cor
#'
#' Report correlations and their p-values
#'
#' @param X a matrix or dataframe
#' @param df the degrees of freedom for the test
#' @param use how to handle missing data
#' @param digits rounding of answers
#' @return - matrix of correlations and p-values
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umx_cor(myFADataRaw[1:8,])
#' }


umx_cor <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {
	# see also
	# hmisc::rcorr( )
	warning("don't use this until we are computing n properly")
	# nvar    = dim(x)[2]
	# nMatrix = diag(NA, nrow= nvar)
	# for (i in 1:nvar) {
	# 	x[,i]
	# }
	R <- cor(X, use = use)
	above <- upper.tri(R)
	below <- lower.tri(R)
	r2 <- R[above]^2
	Fstat <- r2 * df/(1 - r2)
	R[row(R) == col(R)] <- NA # NA on the diagonal
	R[above] <- pf(Fstat, 1, df, lower.tail = F)
	R[below] = round(R[below], digits)
	R[above] = round(R[above], digits)
	# R[above] = paste("p=",round(R[above], digits))
	message("lower tri  = correlation; upper tri = p-value")
	return(R)
}

# Return the maximum value in a row
rowMax <- function(df, na.rm = T) {
	tmp = apply(df, MARGIN = 1, FUN = max, na.rm = na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

rowMin <- function(df, na.rm=T) {
	tmp = apply(df, MARGIN = 1, FUN = min, na.rm = na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

#' umx.as.numeric
#'
#' Convert each column of a dataframe to numeric
#'
#' @param df a \code{\link{data.frame}} to convert
#' @return - data.frame
#' @export
#' @seealso - 
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' df = umx.as.numeric(df)
#' }
umx.as.numeric <- function(df) {
	# TODO handle case of not being a data.frame...
	for (i in names(df)) {
		df[,i] = as.numeric(df[,i])
	}
	return(df)
}

#' umx_swap_a_block
#'
#' Swap a block of rows of a datset between two lists variables (typically twin 1 and twin2)
#'
#' @param theData a data frame to swap within
#' @param rowSelector rows to swap amongst columns
#' @param T1Names the first set of columns
#' @param T2Names the second set of columns
#' @return - dataframe
#' @export
#' @seealso - \code{\link{subset}}
#' @examples
#' test = data.frame(a=paste("a",1:10,sep=""),b=paste("b",1:10,sep=""), c=paste("c",1:10,sep=""),d=paste("d",1:10,sep=""),stringsAsFactors=F)
#' umx_SwapABlock(test, rowSelector = c(1,2,3,6), T1Names = "b", T2Names = "c")
#' umx_SwapABlock(test, rowSelector = c(1,2,3,6), T1Names = c("a","c"), T2Names = c("b","d"))
#'
umx_swap_a_block <- function(theData, rowSelector, T1Names, T2Names) {
	theRows = theData[rowSelector,]
	old_BlockTwo = theRows[,T2Names]
	theRows[,T1Names] -> theRows[, T2Names]
	theRows[,T1Names] <- old_BlockTwo
	theData[rowSelector,] <- theRows
	return(theData)
}

#' umx_rename
#'
#' Returns a dataframe with variables renamed as desired. Checks that the variables exist, and that the new neames are not already used.
#'
#' @param x the dataframe in which to rename variables
#' @param replace a named list of oldName = "newName" pairs OR a list of new names
#' @param old Optional: a list of names that will be replaced by the contents of replace. defaults to NULL in which case replace must be paired list
#' @return - the renamed dataframe
#' @export
#' @seealso - \code{\link{umx_rename_file}}
#' @references - 
#' @examples
#' \dontrun{
#' rename ages to "age"
#' x = rename(x, replace = c(ages = "age"))
#' alternate style
#' x = rename(x, old = c("ages"), replace = c("age"))
#' }

umx_rename <- function (x, replace, old = NULL) {
	# add to help: see also gdate::rename.vars(data, from, to)	
	if(!is.null(old)){
		# message("replacing old with replace")
		if(length(old) != length(replace)){
			stop("You are trying to replace ", length(old), " old names with ", length(replace), "new names: Lengths must match")
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

#' umx_grep
#'
#' search the labels of an SPSS file
#'
#' @param df an \code{\link{data.frame}} to search the labels of
#' @param grepString the search string
#' @param output the column name, the label, or both (default)
#' @param ignore.case whether to be case sensitive or not (default TRUE)
#' @param useNames whether to search the names as well as the labels
#' @return - list of matched column name and labels
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_grep(mtcars, "hp", output="both", ignore.case=T)
#' umx_grep(mtcars, "^h.*", output="both", ignore.case=T)
#' \dontrun{
#' umx_grep(spss_df, "labeltext", output = "label") 
#' umx_grep(spss_df, "labeltext", output = "name") 
#' }
umx_grep <- function(df, grepString, output="both", ignore.case=T, useNames=F) {
	# output = "both", "label" or "name"
	# if(length(grepString > 1)){
	# 	for (i in grepString) {
	# 		umx_grep_labels(df, i, output=output, ignore.case=ignore.case, useNames=useNames)
	# 	}
	# } else {
		# need to check this exists
		vLabels = attr(df,"variable.labels") # list of descriptive labels?
		a       = names(df) 
		if(is.null(vLabels)){
			# message("No labels found")
			return(grep(grepString, names(df), value=T, ignore.case=T))
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
	# }
}

# ======================
# = Comparison helpers =
# ======================

#' umxLessThan
#'
#' A version of less-than which returns FALSE for NAs (rather than NA)
#'
#' @aliases %%<%%
#'
#' @export
#' @seealso - \code{\link{umx_greater_than}}, 
#' @examples
#' c(1:3, NA, 5) = %<% 2

umx_less_than <- function(table, x){
	lessThan = table < x
	lessThan[is.na(lessThan)] = FALSE
	return(lessThan)
}

#' umxGreaterThan
#'
#' A version of greater-than that excludes NA as a match
#'
#' @aliases %%>%%
#'
#' @export
#' @seealso - \code{\link{umx_less_than}}, 
#' @examples
#' c(1:3,NA,5) = %>% 2 

umx_greater_than <- function(table, x){
	moreThan = table > x
	moreThan[is.na(moreThan)] = FALSE
	return(moreThan)
}

# =====================
# = Utility functions =
# =====================
round.num <- function(x, digits) { stop("Use umx_round()")}

#' umx_round
#'
#' A version of round() which works on dataframes that contain non-numeric data (or data that cannot be coerced to numeric)
#' Helpful for dealing with table output that mixes numeric and string types.
#'
#' @param x an dataframe to round in
#' @param digits how many digits to round to
#' @param coerce whether to make the column numeric if it is not
#' @return - \code{\link{mxModel}}
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_round(mtcars, coerce = T)

umx_round <- function(df, digits, coerce = T) {
	if(!is.data.frame(df)){
		stop(paste0("umx_round takes a dataframe as its first argument. ", quote(df), " isn't a dataframe"))
	}
	# for each column, if numeric, round
	rows = dim(df)[1]
	cols = dim(df)[2]
	for(c in 1:cols) {
		if(coerce){
			for(r in 1:rows) {
				df[r, c] = round(as.numeric(df[r, c]), digits)
			}
		} else {
			if(is.numeric(df[1, c])){
				df[ , c] = round(df[ , c], digits)
			}
		}
	}
	return(df)
}

specify_decimal <- function(x, k) format(round(x, k), nsmall = k)

#' print.html
#'
#' printing method for sending obejcts to html output
#'
#' @param x an object to print
#' @param rounding decimal places (not implemented)
#' @param output file to write to and open in browser
#' @return - 
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' print.html(x, output = "Rout.html")
#' }
print.html <- function(x, rounding = 3, output = "tmp.html") {
	# options for output = c("Rout.html","cat","return")
	R2HTML::HTML(x, file = output, Border = 0, append = F, sortableDF=T); 
	system(paste0("open ", output))
	print("Table opened in browser")
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
    datac <- plyr::ddply(data, groupvars, .drop = .drop,
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
    datac    <- umx_rename(datac, c("mean" = measurevar))
    datac$se <- datac$sd / sqrt(datac$N) # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N - 1)
    datac$ci <- datac$se * ciMult
    return(datac)
}

umxCovData = function(df, columns = manifests, use = "pairwise.complete.obs") {
	if(!all(manifests %in%  names(df))){
		message("You asked for", length(manifests), "variables. Of these the following were not in the dataframe:")
		stop(paste(manifests[!(manifests %in% names(df))], collapse=", "))
	} else {
		return(mxData(cov( df[,manifests], use = use), type = "cov", numObs = nrow(df)))
	}	
}


#' umxCov2cor
#'
#' Just a version of cov2cor that forces the upper and lower triangles to be identical (rather than really similar)
#'
#' @param x something that cov2cor can work on (matrix, df, etc.)
#' @return - a correlation matrix
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' x_cor = umxCov2cor(x)
#' }

umxCov2cor <- function(x) {
	x = cov2cor(x)
	x[lower.tri(x)] <- t(x)[lower.tri(t(x))]
	return(x)
}

# ===========================================
# = Functions to check kind: return Boolean =
# ===========================================

#' umx_has_been_run
#'
#' check if an mxModel has been run or not
#'
#' @param model an \code{\link{mxModel}} to check
#' @return - boolean
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umx_has_been_run(model)
#' }

umx_has_been_run <- function(model, stop = F) {
	output <- model@output
	if (is.null(output)){
		if(stop){
			stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
		}else{
			return(F)
		}
	} else if (length(output) < 1){
		if(stop){
			stop("Provided model has no output. I can only standardize models that have been run!")		
		}else{
			return(F)
		}
	}
}

#' umx_check_names
#'
#' check if a list of names are in the names() of a dataframe
#'
#' @param namesNeeded list of variable names to find
#' @param data data.frame to search in for names
#' @return - boolean
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' umx_check_names(c("x1","x2"), demoOneFactor)
#' umx_check_names(c("z1","x2"), demoOneFactor, die = F)

umx_check_names <- function(namesNeeded, data, die = TRUE){
	if(!is.data.frame(data)){
		stop("data has to be a dataframe")
	}
	namesFound = (namesNeeded %in% names(data))
	if(any(!namesFound)){
		if(die){
			print(namesFound)
			stop("Not all names found. Following were missing from data:\n",
				paste(namesNeeded[!namesFound], collapse="; ")
			)
		} else {
			return(FALSE)
		}
	} else {
		return(TRUE)
	}
}

#' umx_is_ordinal
#'
#' return the names of any ordinal variables in a dataframe
#'
#' @param df an \code{\link{data.frame}} to look in for ordinal variables
#' @return - list of variable names
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umx_is_ordinal(df)
#' }
umx_is_ordinal <- function(df, names=F) {
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

#' umx_is_RAM
#'
#' Utility function returning a binary answer to the question "Is this a RAM model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx RAM \code{\link{mxModel}}
#' @return - Boolean
#' @export
#' @seealso - \code{\link{mxModel}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' fit1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' fit1 = umxRun(fit1, setLabels = T, setValues = T)
#' umxSummary(m1, show = "std")
#' if(umx_is_RAM(fit1)){
#' 	message("nice RAM model!")
#' }
#' \dontrun{
#' if(!umx_is_RAM(fit1)){
#' 	stop("model must be a RAM model")
#' }
#' }

umx_is_RAM <- function(obj) {
	# return((class(obj$objective)[1] == "MxRAMObjective" | class(obj$expectation)[1] == "MxExpectationRAM"))
	if(!umx_is_MxModel(obj)){
		return(F)
	} else if(class(obj)[1] == "MxRAMModel"){
		return(T)
	} else {
		return(class(obj$objective)[1] == "MxRAMObjective")
	}
}

#' umx_is_MxModel
#'
#' Utility function returning a binary answer to the question "Is this an OpenMx model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx \code{\link{mxModel}}
#' @return - Boolean
#' @export
#' @seealso - \code{\link{mxModel}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' if(umx_is_MxModel(fit1)){
#' 	message("nice OpenMx model!")
#' }
#' }
umx_is_MxModel <- function(obj) {
	isS4(obj) & is(obj, "MxModel")	
}


#' umx_is_cov
#'
#' test if a data frame or matrix is cov or cor data, or is likely to be raw...
#'
#' @param data dataframe to test
#' @return - "raw", "cor", or "cov"
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' umx_is_cov(df)
#' }

umx_is_cov <- function(data = NULL, boolean = F, verbose = F) {
	if(is.null(data)) { stop("Error in umx_is_cov: You have to provide the data that you want to check...") }

	if( nrow(data) == ncol(data)) {
		if(all(data[lower.tri(data)] == t(data)[lower.tri(t(data))])){
			if(all(diag(data) == 1)){
				isCov = "cor"
				if(verbose){
					message("treating data as cor")
				}
			} else {
				isCov = "cov"
				if(verbose){
					message("treating data as cov")
				}
			}
		} else {
			isCov = "raw"
			if(verbose){
				message("treating data as raw: it's a bit odd that it's square, however")
			}
		}
	} else {
		isCov = "raw"
		if(verbose){
			message("treating data as raw")
		}
	}
	if(boolean){
		return(isCov %in%  c("cov", "cor"))
	} else {
		return(isCov)
	}
}

#' umx_has_CIs
#'
#' A utility function to return a binary answer to the question "does this \code{\link{mxModel}} have confidence intervals?" 
#'
#' @param model The \code{\link{mxModel}} to check for presence of CIs
#' @return - TRUE or FALSE
#' @export
#' @seealso - \code{\link{mxCI}}, \code{\link{umxCI}}, \code{\link{umxRun}}
#' @references - http://openmx.psyc.virginia.edu/
#' @examples
#' \dontrun{
#' umx_has_CIs(model)
#' }

umx_has_CIs <- function(model) {
	if(is.null(model@output$confidenceIntervals)){
		hasCIs = F
	} else {
		hasCIs = dim(model@output$confidenceIntervals)[1] > 0
	}
	return(hasCIs)
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
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = T)

umx_APA_pval <- function(p, min = .001, rounding = 3, addComparison = NA) {
	# addComparison can be NA to only add when needed
	if(length(p) > 1){
		o = rep(NA, length(p))
		for(i in seq_along(p)) {
		   o[i] = umx_APA_pval(p[i], min = min, rounding = rounding, addComparison = addComparison)
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
				return(format(round(p, rounding), scientific = F, nsmall = rounding))
			}else if(addComparison){				
				return(paste0("= ", format(round(p, rounding), scientific = F, nsmall = rounding)))
			} else {
				return(round(p, rounding))
			}
		}	
	}
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

#' umx_reorder
#'
#' Reorder the variables in a correlation matrix. Can also remove one or more variables from a matrix using this function
#'
#' @param old a square matrix of correlation or covariances to reorder
#' @param newOrder The order you'd like the variables to be in
#' @return - the re-ordered (and/or resized) matrix
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' oldMatrix = cov(mtcars)
#' umx_reorder(oldMatrix, newOrder = c("mpg", "cyl", "disp")) # first 3
#' umx_reorder(oldMatrix, newOrder = c("hp", "disp", "cyl")) # subset and reordered

umx_reorder <- function(old, newOrder) {
	dim_names = dimnames(old)[[1]]
	if(!all(newOrder %in% dim_names)){
		stop("All variable names must appear in the matrix")
	}
	numVarsToRetain = length(newOrder)
	new = old[1:numVarsToRetain, 1:numVarsToRetain]
	dimnames(new) = list(newOrder, newOrder)
	for(r in newOrder) {
		for(c in newOrder) {
			new[r, c] <- old[r, c]
		}
	}
	return(new)
}
#' umx_has_square_brackets
#'
#' Helper function, checking if a label has sqaure brackets
#'
#' @param input 
#' @return - boolean
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_has_square_brackets("[hello]")
#' umx_has_square_brackets("goodbye")

umx_has_square_brackets <- function (input) {
    match1 <- grep("[", input, fixed = TRUE)
    match2 <- grep("]", input, fixed = TRUE)
    return(length(match1) > 0 && length(match2) > 0)
}


#' umx_string_to_algebra
#'
#' This is useful because it lets you use paste() and rep() to quickly and easily insert values from R variables into the string, then parse the string as an mxAlgebra argument. The use case this time was to include a matrix exponent (that is A %*% A %*% A %*% A...) with a variable exponent. 
#'
#' @param algString a string to turn into an algebra
#' @param name of the returned algebra
#' @param dimnames of the returned algebra
#' @return - \code{\link{mxAlgebra}}
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' alg = umx_string_to_algebra(paste(rep("A", nReps), collapse = " %*% "), name = "test_case")
#' }
umx_string_to_algebra <- function(algString, name = NA, dimnames = NA) {
	eval(substitute(mxAlgebra(tExp, name=name, dimnames=dimnames), list(tExp = parse(text=algString)[[1]])))
}

#' umxEval
#'
#' Takes an expression as a string, and evaluates it as an expression in model, optionally computing the result.
#' # TODO Currently broken...
#'
#' @param expstring an expression string, i.e, "a + b"
#' @param model an \code{\link{mxModel}} to evaluate in
#' @param compute 
#' @param show 
#' @return - an openmx algebra (formula)
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#'	fit = mxModel("fit",
#'		mxMatrix("Full", nrow=1, ncol=1, free=T, values=1, name="a"),
#'		mxMatrix("Full", nrow=1, ncol=1, free=T, values=2, name="b"),
#'		mxAlgebra(a %*% b, name="ab"),
#'		mxConstraint(ab ==35, name = "maxHours"),
#'		mxAlgebraObjective(algebra="ab", numObs= NA, numStats=NA)
#'	)
#'	fit = mxRun(fit)
#'	mxEval(list(ab = ab), fit)
#' }
umxEval <- function(expstring, model, compute = F, show = F) {
	return(eval(substitute(mxEval(x, model, compute, show), list(x = parse(text=expstring)[[1]]))))
}
