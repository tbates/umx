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
#' xmuMakeThresholdsMatrices should be replaced with [umxThresholdMatrix()]
#' 
#' umxSaturated should be replaced with [mxRefModels()]
#' 
#' umx_grep_labels should be replaced with [umx_grep()]
#' 
#' grepSPSS_labels should be replaced with [umx_grep()]
#' 
#' umxStart should be replaced with [umxValues()]
#' 
#' umxTryHard is deprecated: use [umxRun()] instead
#'
#' genEpi_Jiggle is deprecated: use [umxJiggle()] instead
#' 
#' umxLabels Is deprecated: use [umxLabel()] instead
#' 
#' umxLabels Is deprecated: use [umxLabel()] instead
#' 
#' umxPath is deprecated: Use [mxPath()] and [umxLabel()] instead
#' 
#' umxReportFit is deprecated: use [umxSummary()] instead
#' 
#' umxGetLabels is deprecated: use [umxGetParameters()] instead
#'
#' stringToMxAlgebra is deprecated: please use [umx_string_to_algebra()] instead
#'
#' genEpi_EvalQuote is deprecated: please use [mxEvalByName()] instead
#'
#' umxReportCIs is deprecated: please use [umxCI()] instead
#'
#' hasSquareBrackets is deprecated: please use [umx_has_square_brackets()] instead
#' 
#' xmuHasSquareBrackets is deprecated: please use [umx_has_square_brackets()] instead
#' 
#' replace umxReportFit with [umxSummary()]
#' 
#' Replace umxGraph_RAM with [plot()]
#'
#' Replace tryHard with [mxTryHard()]
#'
#' Replace genEpi_ReRun with [umxModify()]
#'
#' Replace mxStart with [umxValues()]
#'
#' Replace umxLabeler with [umxLabel()]
#'
#' Replace standardizeRAM with [xmu_standardize_RAM()]
#'
#' Replace genEpi_equate with [umxEquate()]
#'
#' Replace genEpi_Path with [umxPath()]
#'
#' Replace genEpiCompare with [umxCompare()]
#'
#' Replace mxLatent with [umxLatent()]
#' 
#' Change col.as.numeric to [umx_as_numeric()]
#' 
#' Change cor.prob to [umx_cor()]
#' 
#' Change umx_u_APA_pval to [umx_APA_pval()]
#' 
#'
#' @name umx-deprecated
#' @family umx deprecated
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
NULL
