# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# ==================
# = Model Builders =
# ==================
#' umxEFA
#'
#' A helper for EFA that only requires you to enter your latents and manifests
#'
#' @param name the name for your new EFA model
#' @param latents List of factors in your CFA
#' @param data The dataframe of manifest columns you are modeling
#' @param showEstimates = c("none", "raw", "std", "both", "list of column names")
#' @param digits rounding (defaults to 2)
#' @param report What to report
#' @return - \code{\link{mxModel}}
#' @family Super-easy helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' umxEFA(name= "test", latents = "g", data = mtcars[, c("mpg", "disp", "hp", "wt")])
umxEFA <- function(name = "efa", latents = NULL, data = NULL, showEstimates = c("none", "raw", "std", "both", "list of column names"), digits = 2, report = c("1", "table", "html")){
	message("umxEFA is beta-only, and not ready for prime time")
	if(is.null(latents)){
		stop("You need to request at least 1 latent factor.")
	}
	if(is.null(data)){
		stop("You need to provide data to factor analyse.")
	}
	# TODO adapt to input datatype, i.e., add cov handler
	manifests <- names(data)
	m1 <- umxRAM(model = name, data = data, autoRun = FALSE,
		umxPath(latents, to = manifests),
		umxPath(v.m. = manifests),
		umxPath(v1m0 = latents, fixedAt = 1)
	)
	m1 = mxRun(m1)
	umxSummary(m1, showEstimates = showEstimates, digits = digits, report = report);
	invisible(m1)
}