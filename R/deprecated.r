# ==============
# = Deprecated =
# ==============

#' Deprecated. May already stop() code and ask to be updated. May be dropped entirely in future.
#'
#' @param ... the old function's parameters (now stripped out to avoid telling people how to do it the wrong way :-)
#' @description 
#' 
#' umxSaturated should be replaced with \code{\link{mxRefModels}}
#' 
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
#' hasSquareBrackets is deprecated: please use \code{\link{umx_has_square_brackets}} instead
#' 
#' xmuHasSquareBrackets is deprecated: please use \code{\link{umx_has_square_brackets}} instead
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
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}, \url{http://openmx.psyc.virginia.edu}
NULL

#' @rdname umx-deprecated
#' @export
umxSaturated <- function(...) { stop("Deprecated: use mxRefModels()") }

#' @rdname umx-deprecated
#' @export
grepSPSS_labels <- function(...) { stop("Deprecated: use umx_grep()") }

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
cor.prob <- function (...) {message("Use umx_cor")}

#' @rdname umx-deprecated
#' @export
mxStart <- function(...) { traceback(); stop("Deprecated: Please use umxValues() instead of mxStart")}

#' @rdname umx-deprecated
#' @export
mxLatent <- function(...) {message("please look at umxRAM() and umxPath")}

#' @rdname umx-deprecated
#' @export
umxReportFit <- function(...) { stop("Deprecated: Please replace umxReportFit with umxSummary") }

#' @rdname umx-deprecated
#' @export
umxReportCIs <- function(...) { stop("Deprecated: Please replace umxReportCIs with umxCI") }
