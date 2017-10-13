#
#   Copyright 2007-2017 Copyright 2007-2017 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' A multilevel dataset.
#'
#' A dataset for use in multivariate model example. This is an example of a two-level CFA with
#' continuous factor indicators, a random intercept factor, and covariates
#'
#' \describe{
#'   \item{y1}{continuous indicator 1}
#'   \item{y2}{continuous indicator 2}
#'   \item{y3}{continuous indicator 3}
#'   \item{y4}{continuous indicator 4}
#'   \item{x1}{covariate 1}
#'   \item{x2}{covariate 2}
#'   \item{w}{something}
#'   \item{clusterID}{Clustering variable}
#' }
#' @docType data
#' @keywords datasets
#' @name ex9_6
#' @usage data(ex9_6)
#' @format A data frame with 1000 rows and 8 variables:
#' @source \url{https://www.statmodel.com/usersguide/chap9/ex9.6.html}
#' @examples
#' data(ex9_6)
#' names(ex9_6)
NULL
