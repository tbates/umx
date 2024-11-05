#   Copyright 2007-2022 Timothy C. Bates
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
#' `xmuMakeThresholdsMatrices` should be replaced with [umxThresholdMatrix()]
#' 
#' `umxTryHard` is deprecated: use [umxRun()] instead
#'
#' `stringToMxAlgebra` is deprecated: please use [umx_string_to_algebra()] instead
#'
#' `genEpi_EvalQuote` is deprecated: please use [OpenMx:: mxEvalByName()] instead
#'
#' `umxReportCIs` is deprecated: please use [umxCI()] instead
#'
#' replace `umxReportFit` with [umxSummary()]
#' 
#' Replace `umxGraph_RAM` with [plot()]
#'
#' Replace `tryHard` with [OpenMx:: mxTryHard()]
#'
#' Replace `standardizeRAM` with [umx_standardize()]
#' 
#'
#' @name umx-deprecated
#' @family umx deprecated
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
NULL
