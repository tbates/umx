# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx"); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()

# ==============
# = Deprecated =
# ==============

print.html <- function(x, digits = 3, output = "tmp.html") {stop("Deprecated: used umx_print()")}


#' umx_grep_labels
#'
#' Deprecated function to search the labels and names use \code{\link{umx_grep}} instead
#'
#' @param df an \code{\link{data.frame}} to search the labels of
#' @param grepString the search string
#' @param output the column name, the label, or both (default)
#' @param ignore.case whether to be case sensitive or not (default TRUE)
#' @param useNames whether to search the names as well as the labels
#' @return - list of matched column name and labels
#' @export

umx_grep_labels <- function(df, grepString, output="both", ignore.case=T, useNames=F) { stop("Deprecated: used umx_grep_labels()") }

#' grepSPSS_labels
#'
#' Deprecated function to search the labels of an SPSS file use \code{\link{umx_grep_labels}} instead
#'
#' @param df an \code{\link{data.frame}} to search the labels of
#' @param grepString the search string
#' @param output the column name, the label, or both (default)
#' @param ignore.case whether to be case sensitive or not (default TRUE)
#' @param useNames whether to search the names as well as the labels
#' @return - list of matched column name and labels
#' @export
#' @seealso - \code{\link{umx_grep_labels}}
grepSPSS_labels <- function(df, grepString, output="both", ignore.case=T, useNames=F) { stop("Deprecated: used umx_grep_labels()") }

# umxTryHard
#
# umxTryHard Is deprecated: use \code{\link{umxRun}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/
umxTryHard <- function(model, n=3, calc_SE=F){ stop("Use umxRun() in place of umxTryHard") }

# umxLabels
#
# umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/
umxLabels <- function(from=NA, to=NA, connect="single", prefix="", suffix="") {stop("please use umxPath in place of umxLabels. To label models or matrices, use umxLabel")}

# genEpi_Jiggle
# genEpi_Jiggle is deprecated: use \code{\link{umxJiggle}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/
genEpi_Jiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {stop("please use umxJiggle in place of genEpi_Jiggle")}

# umxPath
# umxPath is deprecated: Use \code{\link{mxPath}} and \code{\link{umxLabel}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/
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

umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = "std", precision = 2, displayColumns = c("row", "col", "Std.Estimate")){
	stop("umxReportFit is deprecated: use umxSummary() in its place")
}

umxGetLabels <- function(inputTarget, regex = NA, free = NA, verbose = F) {
	message("Please use umxGetParameters instead of umxGetLabels")
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
	return(theLabels)
}

col.as.numeric <- function(df) {
	stop("col.as.numeric is deprecated. Please replace with umx.as.numeric()")
	# use case
	# col.as.numeric(df)
	for (i in names(df)) {
		df[,i] = as.numeric(df[,i])
	}
	return(df)
}

cor.prob <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {message("Use umx_cor")}

umx_u_APA_pval <- function(p, min = .001, rounding = 3, addComparison = T) {stop("umx_u_APA_pval is deprecated: Use umx_APA_pval")}


# ==========================
# = Deprecated from genepi =
# ==========================

# ==============
# = Deprecated =
# ==============

makeACE <-function(modelName="ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzAr=.5, dzCr=1, addStd=F) {
	stop("don't call makeACE(), it's so... 2009... Use makeACE_2Group() instead")
}

twoGroupCommonPath <- function(modelName="CP", selDVs, dzData, mzData, nSib=2) {
	stop("Change \"twoGroupCommonPath()\" to: makeCP_2Group()")
}

twoGroupIndependentPath <- function(modelName="IP", selDVs, dzData, mzData, nSib=2, freeLowerE=F) {
	stop("Change \"twoGroupIndependentPath\" to: makeIP_2Group")
}

two_group_G_by_E <- function(modelName = "GbyE", selDVs, selDefs, dzData, mzData, nSib=2) {
	stop("Change \"two_group_G_by_E\" to: makeGxE_2Group")
}

twoGroupCholesky <-function(modelName = "ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzr=.5, addStd=F) {
	stop("Change \"twoGroupCholesky\" to: makeACE_2Group")
}

twoGroupOrdinal <- function(modelName ="Ordinal", selDVs, factorLevels, dzData, mzData, nSib, lowerBoundZ= -5, aceStart=c(.6,.3,.7)) {
	stop("Change \"twoGroupOrdinal\" to: makeOrdinal_2Group")
}

umxGraph_RAM <- function(model = NA, std = T, precision = 2, dotFilename = "name", pathLabels = "none", showFixed = F, showError = T) {
	stop("Replace umxGraph_RAM with umxPlot (umxGraph_RAM was deprecated to help people learn umx more quickly)")
}

tryHard <- function(model, n = 3, calc_SE = F){stop("Use umxRun() in place of umxTryHard() and tryHard()")}

genEpi_ReRun <- function(lastFit, dropList=NA, regex=NA, free=F, value=0, freeToStart=NA, newName=NA, verbose=F, intervals=F) {message("please call umxReRun()")}

mxStart        <- function(x=1, sd=NA, n=1){ traceback(); stop("Deprecated: Please use umxStart() instead of mxStart")}
umxLabeler     <- function(mx_matrix = NA, baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA) {stop("replace 'umxLabeler' with 'umxLabel'")}

standardizeRAM <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {stop("Deprecated: Please use umxStandardizeModel() instead of standardizeRAM")}
genEpi_equate  <- function(myModel, master, slave, free = T, verbose = T, name = NA) {stop("replace genEpi_equate() with umxEquate()") }
genEpi_Path    <- function(from, to,arrows=1, connect="single", free = T, values = NA, labels = NA, lbound = NA, ubound = NA){ message("genEpi_Path is deprecated: replace with umxPath")}
genEpiCompare  <- function(base = NA, comparison = NA, all = T, excel = T) { stop("deprecated, use umxCompare")}
mxLatent       <- function(latent=NA, formedBy=NA, forms=NA, data, endogenous=FALSE, model.name=NA, help=FALSE, labelSuffix="") {message("please call umxLatent()")}

hcor                     <- function(data, ML = F, use="pairwise.complete.obs"){message("Please call umxHetCor() instead of hcor")}
lower2full               <- function(lower.data, diag = F, byrow = T) {stop("please replace lower2full with umxLower2Full()")}
mxIsOrdinalVar           <- function(df, names = F) {message("replace mxIsOrdinalVar with umxIsOrdinalVar")}
genEpi_GetLabels         <- function(inputTarget, regex = NA, free = NA, verbose = F) { stop("please replace genEpi_GetLabels with umxGetLabels()")}
mxMakeThresholdsMatrices <- function(df, deviationBased = T, droplevels = T, verbose = F) {	message("please change to 'umxMakeThresholdMatrix()'")}
mxAutoThreshRAMObjective <- function(df, deviationBased = T, droplevels = T, verbose = F) { message("please call umxThresholdRAMObjective()")}

graphViz_Cholesky <- function(model = NA, dotFilename = "name", accuracy = 2, showMeans=F) {
	stop("Deprecated: PLease replace graphViz_Cholesky with umxPlotCholesky")
}


summaryACEFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs) {
	stop("Deprecated: Please replace summaryACEFit with umxSummaryACE")
}

umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = NULL, precision = 2){
	stop("Deprecated: Please replace umxReportFit with umxSummary")
}

summaryCommonFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace summaryCommonFit with umxSummaryCP")
}

umxPlotCholesky <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace umxPlotCholesky with umxPlotACE")
}


stringToMxAlgebra <- function(algString, name = NA, dimnames = NA) {
	stop("Deprecated: Please replace stringToMxAlgebra with umx_string_to_Algebra")
}

genEpi_EvalQuote <- function(expstring, model, compute, show){
	stop("Deprecated: Please replace genEpi_EvalQuote with umxEval")
}


umxReportCIs <- function(model = NA, addCIs = T, runCIs = "if necessary") {
	stop("Deprecated: Please replace umxReportCIs with umxCI")
}


hasSquareBrackets <- function (input) {
	stop("Deprecated: Please replace hasSquareBrackets with xmuHasSquareBrackets")
}

# fit.cov(indepfit,modelfit)
#      source("http://omxhelper.googlecode.com/svn/trunk/genEpi.lib.R")
# To access me via svn
#      cd  ~/bin/omxhelper/
#      svn checkout https://omxhelper.googlecode.com/svn/trunk/ omxhelper --username timothy.c.bates
# Access on google code base as
#       http://code.google.com/p/omxhelper/source/checkout


graphVizTextForms = 'digraph G {
	rankdir = LR
	/* Entities */
	"Manifests have fixed variance\nAll manifests correlated\nlatent\'s variance determined by manifests" [ shape = plaintext ];
	F1  [shape="circle"]
	m1  [shape="square"]
	m2  [shape="square"]
	m3  [shape="square"]

	/* Relationships */
	/* All manifests load on F1 */
	{m1 m2 m3} -> F1
	/*	All manifests linked, bivariate */
	m1 -> m2 -> m3 -> m1 [dir=both]

	/* var */
	m1 -> m1 [dir=both label="@1"]
	m2 -> m2 [dir=both label="@1"]
	m3 -> m3 [dir=both label="@1"]

	/* Ranks */
	{rank=same; m1 m2 m3};
}';

graphVizTextFormed = 'digraph G {
	/* Entities */
	"Manifests have free residual var\nlatent\'s variance fixed at 1" [ shape = plaintext ];
	F1  [shape="circle"]
	m1  [shape="square"]
	m2  [shape="square"]
	m3  [shape="square"]
	"e1" [ shape = plaintext ];
	"e2" [ shape = plaintext ];
	"e3" [ shape = plaintext ];
	/* Relationships */
	/* All manifests load on F1 */
	F1 -> {m1 m2 m3}
	/* var */
	e1 -> m1
	e2 -> m2
	e3 -> m3
	/* Ranks */
	{rank=max; e1 e2 e3};
	{rank=same; m1 m2 m3};
}';


# update your packages: 
# options(repos = c(CRAN = 'http://cran.r-project.org'))
# update.packages()

# install.packages("devtools")
# library("devtools");
# install_github('knitr', 'yihui', 'fun')
# install.packages("knitr")
# library("knitr")

# http://openmx.psyc.virginia.edu/dev/timeline/listing.php?repname=OpenMx&

# mxOption(model= NULL, key="Number of Threads", value= (omxDetectCores() - 1))
# getOption('mxOptions')$"Number of Threads"
# mxOption(model= NULL, key="Number of Threads")

# ==================================
# = How to do a progress indicator =
# ==================================
# steps <- 5
# bar <- txtProgressBar (min=0, max=steps, style=3)
# for (i in 1:steps){
#     setTxtProgressBar (bar, i)
#     Sys.sleep (.1)
# }
# close(bar)

# require(MASS)
# data(birthwt)
# # compute CIs for correlation between mother's weight and birth weight
# cor.boot <- function(data, k) cor(data[k,])[1,2]
# cor.res <- boot(data = with(birthwt, cbind(lwt, bwt)), statistic=cor.boot, R=500)
# cor.res
# boot.ci(cor.res, type="bca")
# 
# # compute CI for a particular regression coefficient, e.g. bwt ~ smoke + ht
# fm <- bwt ~ smoke + ht
# reg.boot <- function(formula, data, k) coef(lm(formula, data[k,]))
# reg.res <- boot(data=birthwt, statistic=reg.boot, 
#                 R=500, formula=fm)
# boot.ci(reg.res, type="bca", index=2) # smoke

# renameFile(findStr="_", replaceStr=" ", listPattern = "", test=T)

# psych::r.test will test for the difference between independent or dependent correlations
# psych::r.test(n = 100, r12 = .5, r34 = .4, ) 
# z value 0.82    with probability  0.41 

# Inference on second-order quantities, such as correlation coefficients and variances, is not robust to the assumption of a normally-distributed population.
# a good alternative is the bootstrap, implemented in R in the boot package."

# ==============
# = Test suite =
# ==============
# data(twinData)
# latents   = "size" 
# manifests = c("age","wt1", "ht1")
# data= twinData[,manifests]
# a = mxModel("bingo", type="RAM",
# 	latentVars   = latents, 
# 	manifestVars = manifests, 
# 	mxPath(from = latents, arrows = 2),
# 	mxPath(from = manifests, arrows = 2),
# 	mxPath(from = "size", to = c("wt1", "ht1")),
# 	mxPath(from = "age", to = "size"),
# 	mxData(cov(data, use = "pair"), type = "cov", numObs = 2000)
# 	
# )
# a = umxRun(a,setLabels=T, setStart = T)
# a@matrices$A@labels; a@matrices$S@labels;
# umxPlot(a,std=T, dotFilename="name",  pathLabels="none", precision=2)
