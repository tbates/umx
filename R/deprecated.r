#   Copyright 2007-2019 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.


# ==============
# = Deprecated =
# ==============


#' Deprecated. May already stop() code and ask to be updated. May be dropped entirely in future.
#'
#' @param ... the old function's parameters (now stripped out to avoid telling people how to do it the wrong way :-)
#' @description 
#' 
#' xmuMakeThresholdsMatrices should be replaced with \code{\link{umxThresholdMatrix}}
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
#' genEpi_EvalQuote is deprecated: please use \code{\link{mxEvalByName}} instead
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
#' Replace tryHard with \code{\link{mxTryHard}}
#'
#' Replace genEpi_ReRun with \code{\link{umxModify}}
#'
#' Replace mxStart with \code{\link{umxValues}}
#'
#' Replace umxLabeler with \code{\link{umxLabel}}
#'
#' Replace standardizeRAM with \code{\link{umx_standardize_RAM}}
#'
#' Replace genEpi_equate with \code{\link{umxEquate}}
#'
#' Replace genEpi_Path with \code{\link{umxPath}}
#'
#' Replace genEpiCompare with \code{\link{umxCompare}}
#'
#' Replace mxLatent with \code{\link{umxLatent}}
#' 
#' Change col.as.numeric to \code{\link{umx_as_numeric}}
#' 
#' Change cor.prob to \code{\link{umx_cor}}
#' 
#' Change umx_u_APA_pval to \code{\link{umx_APA_pval}}
#' 
#'
#' @name umx-deprecated
#' @family umx deprecated
#' @references - \url{https://tbates.github.io}, \url{https://github.com/tbates/umx}, \url{https://openmx.ssri.psu.edu}
NULL