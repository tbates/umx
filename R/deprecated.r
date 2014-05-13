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

#' List of deprecated umx functions
#' These functions are deprecated in umx. They may be dropped entirely in future versions. Many already stop() code and ask to be updated
#'
#' @description 
#' umx_grep_labels should be replaced with \code{\link{umx_grep}}
#' 
#' grepSPSS_labels should be replaced with \code{\link{umx_grep}}
#' 
#' umxStart should be replaced with \code{\link{umxValues}}
#' 
#' umxTryHard is deprecated: use \code{\link{umxRun}} instead
#'
#' genEpi_Jiggle is deprecated: use \code{\link{umxJiggle}} instead
#' 
#' umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#' 
#' umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#' 
#' umxPath is deprecated: Use \code{\link{mxPath}} and \code{\link{umxLabel}} instead
#' 
#' umxReportFit is deprecated: use \code{\link{umxSummary}} instead
#' 
#' umxGetLabels is deprecated: use \code{\link{umxGetParameters}} instead
#' 
#' makeACE is deprecated: use \code{\link{umxACE}} instead
#' 
#' umxPlotCholesky is deprecated: please use \code{\link{umxPlotACE}} instead
#'
#' stringToMxAlgebra is deprecated: please use \code{\link{umx_string_to_Algebra}} instead
#'
#' genEpi_EvalQuote is deprecated: please use \code{\link{umxEval}} instead
#'
#' umxReportCIs is deprecated: please use \code{\link{umxCI}} instead
#'
#' hasSquareBrackets is deprecated: please use \code{\link{xmuHasSquareBrackets}} instead
#' 
#' replace genEpi_GetLabels with \code{\link{umxGetParameters}}
#' 
#' replace mxMakeThresholdsMatrices with \code{\link{umxMakeThresholdMatrix}}
#' 
#' replace mxAutoThreshRAMObjective with \code{\link{umxThresholdRAMObjective}}
#' 
#' replace graphViz_Cholesky with \code{\link{umxPlotACE}}
#' 
#' replace summaryACEFit with \code{\link{umxSummaryACE}}
#' 
#' replace umxReportFit with \code{\link{umxSummary}}
#' 
#' replace summaryCommonFit with \code{\link{umxSummaryCP}}
#' 
#' Replace twoGroupOrdinal with \code{\link{umxOrdinal}}
#'
#' Replace umxGraph_RAM with \code{\link{plot}}
#'
#' Replace tryHard with \code{\link{umxRun}}
#'
#' Replace genEpi_ReRun with \code{\link{umxReRun}}
#'
#' Replace mxStart with \code{\link{umxValues}}
#'
#' Replace umxLabeler with \code{\link{umxLabel}}
#'
#' Replace standardizeRAM with \code{\link{umxStandardizeModel}}
#'
#' Replace genEpi_equate with \code{\link{umxEquate}}
#'
#' Replace genEpi_Path with \code{\link{umxPath}}
#'
#' Replace genEpiCompare with \code{\link{umxCompare}}
#'
#' Replace mxLatent with \code{\link{umxLatent}}
#' 
#' Change twoGroupCommonPath to \code{\link{umxCP}}
#' 
#' Change twoGroupIndependentPath to \code{\link{umxIP}}
#' 
#' Change twoGroupCholesky to \code{\link{umxACE}}
#' 
#' Change col.as.numeric is deprecated. Please replace with \code{\link{umx_as_numeric}}
#' 
#' Change cor.prob to \code{\link{umx_cor}}
#' 
#' Change umx_u_APA_pval to \code{\link{umx_APA_pval}}
#'
#' @name umx-deprecated
#' @family umx deprecated
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
NULL

#' @rdname umx-deprecated
#' @export
umx_grep_labels <- function(df, grepString, output="both", ignore.case=T, useNames=F) { stop("Deprecated: Replace with umx_grep()") }

#' @rdname umx-deprecated
#' @export
grepSPSS_labels <- function(df, grepString, output="both", ignore.case=T, useNames=F) { stop("Deprecated: used umx_grep()") }

#' @rdname umx-deprecated
#' @export
umxStart <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = F){ stop("Use umxValues() in place of umxStart (makes it easier learn umx)") }

#' @rdname umx-deprecated
#' @export
umxTryHard <- function(model, n=3, calc_SE=F){ stop("Use umxRun() in place of umxTryHard") }

#' @rdname umx-deprecated
#' @export
umxLabels <- function(from=NA, to=NA, connect="single", prefix="", suffix="") {stop("Replace 'umxLabels' with 'umxLabel'")}

#' @rdname umx-deprecated
#' @export
genEpi_Jiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {stop("please use umxJiggle in place of genEpi_Jiggle")}

#' @rdname umx-deprecated
#' @export
umxPath <- function(from = NA, to = NA, connect = "single", arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA, prefix = "", suffix = "",...) {
	stop("replace umxPath with mxPath, and run umxLabel(model) on the model when you are done to add default labels, plus umxValues(model) to add default start values")
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

#' @rdname umx-deprecated
#' @export
umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = "std", precision = 2, displayColumns = c("row", "col", "Std.Estimate")){
	stop("umxReportFit is deprecated: use umxSummary() in its place")
}

#' @rdname umx-deprecated
#' @export
umxGetLabels <- function(inputTarget, regex = NA, free = NA, verbose = F) {stop("Please use umxGetParameters instead of umxGetLabels")}

#' @rdname umx-deprecated
#' @export
makeACE <-function(modelName="ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzAr=.5, dzCr=1, addStd=F) {
	stop("don't call makeACE(), it's so... 2009... Use umxACE()")
}

#' @rdname umx-deprecated
#' @export
twoGroupCommonPath <- function(modelName="CP", selDVs, dzData, mzData, nSib=2) {
	stop("Change \"twoGroupCommonPath()\" to: umxCP()")
}

#' @rdname umx-deprecated
#' @export
twoGroupIndependentPath <- function(modelName="IP", selDVs, dzData, mzData, nSib=2, freeLowerE=F) {
	stop("Change \"twoGroupIndependentPath\" to: umxIP")
}

#' @rdname umx-deprecated
#' @export
two_group_G_by_E <- function(modelName = "GbyE", selDVs, selDefs, dzData, mzData, nSib=2) {
	stop("Change \"two_group_G_by_E\" to: umxGxE")
}

#' @rdname umx-deprecated
#' @export
twoGroupCholesky <-function(modelName = "ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzr=.5, addStd=F) {
	stop("Change \"twoGroupCholesky\" to: umxACE")
}

#' @rdname umx-deprecated
#' @export
col.as.numeric <- function(df) {stop("col.as.numeric is deprecated. Please replace with umx_as_numeric()")}

#' @rdname umx-deprecated
#' @export
cor.prob <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {message("Use umx_cor")}

#' @rdname umx-deprecated
#' @export
umx_u_APA_pval <- function(p, min = .001, rounding = 3, addComparison = T) {stop("umx_u_APA_pval is deprecated: Use umx_APA_pval")}

#' @rdname umx-deprecated
#' @export
twoGroupOrdinal <- function(modelName ="Ordinal", selDVs, factorLevels, dzData, mzData, nSib, lowerBoundZ= -5, aceStart=c(.6,.3,.7)) {
	stop("Change \"twoGroupOrdinal\" to: umxOrdinal")
}

#' @rdname umx-deprecated
#' @export
umxGraph_RAM <- function(model = NA, std = T, precision = 2, dotFilename = "name", pathLabels = "none", showFixed = F, showError = T) {
	stop("Replace umxGraph_RAM with umxPlot (umxGraph_RAM was deprecated to help people learn umx more quickly)")
}

#' @rdname umx-deprecated
#' @export
tryHard <- function(model, n = 3, calc_SE = F){stop("Use umxRun() in place of umxTryHard() and tryHard()")}

#' @rdname umx-deprecated
#' @export
genEpi_ReRun <- function(lastFit, dropList=NA, regex=NA, free=F, value=0, freeToStart=NA, newName=NA, verbose=F, intervals=F) {message("please call umxReRun()")}

#' @rdname umx-deprecated
#' @export
mxStart        <- function(x=1, sd=NA, n=1){ traceback(); stop("Deprecated: Please use umxValues() instead of mxStart")}

#' @rdname umx-deprecated
#' @export
umxLabeler     <- function(mx_matrix = NA, baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA) {stop("replace 'umxLabeler' with 'umxLabel'")}

#' @rdname umx-deprecated
#' @export
standardizeRAM <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {stop("Deprecated: Please use umxStandardizeModel() instead of standardizeRAM")}

#' @rdname umx-deprecated
#' @export
genEpi_equate  <- function(myModel, master, slave, free = T, verbose = T, name = NA) {stop("replace genEpi_equate() with umxEquate()") }

#' @rdname umx-deprecated
#' @export
genEpi_Path    <- function(from, to,arrows=1, connect="single", free = T, values = NA, labels = NA, lbound = NA, ubound = NA){ message("genEpi_Path is deprecated: replace with umxPath")}

#' @rdname umx-deprecated
#' @export
genEpiCompare  <- function(base = NA, comparison = NA, all = T, excel = T) { stop("deprecated, use umxCompare")}

#' @rdname umx-deprecated
#' @export
mxLatent       <- function(latent=NA, formedBy=NA, forms=NA, data, endogenous=FALSE, model.name=NA, help=FALSE, labelSuffix="") {message("please call umxLatent()")}

#' @rdname umx-deprecated
#' @export
genEpi_GetLabels <- function(inputTarget, regex = NA, free = NA, verbose = F) { stop("please replace genEpi_GetLabels with umxGetParameters()")}

#' @rdname umx-deprecated
#' @export
mxMakeThresholdsMatrices <- function(df, deviationBased = T, droplevels = T, verbose = F) {	message("please change to 'umxMakeThresholdMatrix()'")}

#' @rdname umx-deprecated
#' @export
mxAutoThreshRAMObjective <- function(df, deviationBased = T, droplevels = T, verbose = F) { message("please call umxThresholdRAMObjective()")}

#' @rdname umx-deprecated
#' @export
graphViz_Cholesky <- function(model = NA, dotFilename = "name", accuracy = 2, showMeans=F) {stop("Deprecated: Please replace graphViz_Cholesky with umxPlotACE")}

#' @rdname umx-deprecated
#' @export
summaryACEFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs) {
	stop("Deprecated: Please replace summaryACEFit with umxSummaryACE")
}

#' @rdname umx-deprecated
#' @export
umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = NULL, precision = 2){
	stop("Deprecated: Please replace umxReportFit with umxSummary")
}

#' @rdname umx-deprecated
#' @export
summaryCommonFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace summaryCommonFit with umxSummaryCP")
}

#' @rdname umx-deprecated
#' @export
umxPlotCholesky <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace umxPlotCholesky with umxPlotACE")
}


#' @rdname umx-deprecated
#' @export
stringToMxAlgebra <- function(algString, name = NA, dimnames = NA) {
	stop("Deprecated: Please replace stringToMxAlgebra with umx_string_to_Algebra")
}

#' @rdname umx-deprecated
#' @export
genEpi_EvalQuote <- function(expstring, model, compute, show){
	stop("Deprecated: Please replace genEpi_EvalQuote with umxEval")
}


#' @rdname umx-deprecated
#' @export
umxReportCIs <- function(model = NA, addCIs = T, runCIs = "if necessary") {
	stop("Deprecated: Please replace umxReportCIs with umxCI")
}


#' @rdname umx-deprecated
#' @export
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
