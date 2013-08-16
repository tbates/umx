# How long did that take?
umxReportTime <- function(model, formatStr= "H %H M %M S %OS3", tz="GMT"){
	# use case
	# umxReportTime(fit1)
	format(.POSIXct(model@output$wallTime,tz), formatStr)
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
