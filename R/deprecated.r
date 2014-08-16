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
#' stringToMxAlgebra is deprecated: please use \code{\link{umx_string_to_algebra}} instead
#'
#' genEpi_EvalQuote is deprecated: please use \code{\link{umxEval}} instead
#'
#' umxReportCIs is deprecated: please use \code{\link{umxCI}} instead
#'
#' hasSquareBrackets is deprecated: please use \code{\link{xmuHasSquareBrackets}} instead
#' 
#' replace genEpi_GetLabels with \code{\link{umxGetParameters}}
#' 
#' replace mxMakeThresholdsMatrices with \code{\link{umxThresholdMatrix}}
#' 
#' replace mxAutoThreshRAMObjective with \code{\link{umxThresholdRAMObjective}}
#' 
#' replace umxThresholdRAMObjective with \code{\link{umx_RAM_thresh_Matrix}}
#' 
#' replace umxThresholdRAMObjective with \code{\link{umx_RAM_ordinal_objective}}
#' 
#' replace umxReportFit with \code{\link{umxSummary}}
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
grepSPSS_labels <- function(...) { stop("Deprecated: used umx_grep()") }

#' @rdname umx-deprecated
#' @export
umxStart <- function(...) { stop("Use umxValues() in place of umxStart (makes it easier learn umx)") }

#' @rdname umx-deprecated
#' @export
umxReportFit <- function(...) {
	stop("umxReportFit is deprecated: use umxSummary() in its place")
}

#' @rdname umx-deprecated
#' @export
col.as.numeric <- function(...) {stop("col.as.numeric is deprecated. Please replace with umx_as_numeric()")}

#' @rdname umx-deprecated
#' @export
cor.prob <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {message("Use umx_cor")}

#' @rdname umx-deprecated
#' @export
mxStart <- function(...) { traceback(); stop("Deprecated: Please use umxValues() instead of mxStart")}

#' @rdname umx-deprecated
#' @export
mxLatent       <- function(...) {message("please look at umxRAM() and umxPath")}

#' @rdname umx-deprecated
#' @export
umxReportFit <- function(...) {
	stop("Deprecated: Please replace umxReportFit with umxSummary")
}


#' @rdname umx-deprecated
#' @export
umxReportCIs <- function(...) {
	stop("Deprecated: Please replace umxReportCIs with umxCI")
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

