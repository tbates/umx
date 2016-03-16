# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# ==================
# = Model Builders =
# ==================
#' umxEFA
#'
#' A helper for EFA that only requires you to choose how many factors and offer up manifest data.
#' This is very much early days. I will add "scores" and "rotation" control if there is demand.
#' 
#' You can create an EFA either by specifying some factor names:
#' 
#' umxEFA(factors = c("g", "v"), data = mtcars)
#' 
#' or 
#' 
#' umxEFA(factors = 2, data = mtcars)
#'
#' \figure{umxEFA.png}
#' 
#' @param name A name for your model.
#' @param factors Either a number (of factors to request) or a list of factor names.
#' @param data A dataframe of manifest columns you are modeling
#' @param covmat Covariance matrix of data you are modeling
#' @param n.obs Number of observations in covmat (if provided, default = NA)
#' @param showEstimates = c("none", "raw", "std", "both", "list of column names")
#' @param digits rounding (defaults to 2)
#' @param report What to report
#' @return - \code{\link{mxModel}}
#' @family Super-easy helpers
#' @export
#' @seealso - \code{\link{factanal}}
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' vars = c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(name= "test", factors = "g", data = mtcars[, vars])
#' m2 = umxEFA(name= "test", factors =   2, data = mtcars[, vars])
#' \dontrun{
#' plot(m2)
#' }
umxEFA <- function(name = "efa", factors = NULL, data = NULL, covmat = NULL, n.obs = NULL, showEstimates = c("none", "raw", "std", "both", "list of column names"), digits = 2, report = c("1", "table", "html")){
	message("umxEFA is beta-only, and not ready for prime time")
	if(!is.null(covmat)){
		stop("Email me to implement cov matrix support.")
	}
	if(is.null(factors)){
		stop("You need to request at least 1 latent factor.")
	} else if( length(factors)==1 && class(factors)=="numeric"){
		factors = paste0("F", c(1:factors))
	}
	if(is.null(data)){
		stop("You need to provide data to factor analyse.")
	}
	# TODO adapt to input datatype, i.e., add cov handler
	manifests <- names(data)
	m1 <- umxRAM(model = name, data = data, autoRun = FALSE,
		umxPath(factors, to = manifests, connect = "unique.bivariate"),
		umxPath(v.m. = manifests),
		umxPath(v1m0 = factors, fixedAt = 1)
	)
	m1 = mxRun(m1)
	umxSummary(m1, showEstimates = showEstimates, digits = digits, report = report);
	invisible(m1)
}