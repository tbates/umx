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
#' @param report What to report
#' @return - \code{\link{mxModel}}
#' @family Super-easy helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umxEFA("test", latents = "g", data = mtcars[, c("mpg", "disp", "hp", "wt")])
#' }

umxEFA <- function(name = "", latents, data, report = c("table", "line", "long")){
	manifests <- names(data)
	m1 <- mxModel(name, type = "RAM",
		manifestVars = manifests,
		latentVars   = latents,
		# Factor loadings
		mxPath(from = latents, to = manifests),
		mxPath(from = manifests, arrows = 2), # manifest residuals 
		mxPath(from = latents, arrows = 2, free = FALSE, values = 1), # latents fixed@1
		mxData(cov(data), type="cov", numObs = nrow(data))
	)
	m1 = mxRun(m1);
	umxSummary(m1, show = "std", report = report);
	invisible(m1)
}
