# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); devtools::document(); devtools::install();
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()

# ==================
# = Model Builders =
# ==================
#' umxCFA
#'
#' A helper for CFA that only requires you to enter your latents and manifests
#'
#' @param name the name for your new CFA model
#' @param latents List of factors in your CFA
#' @param data The dataframe of manifest columns you are modeling
#' @param report What to report
#' @return - \code{\link{mxModel}}
#' @family super easy helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umxCFA("test", "g", mtcars)
#' }

umxCFA <- function(name = "", latents, data, report = c("table", "line", "long")){
	# umxCFA(name="myFirstCFA", latents="g", data = myFAdata)
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
	umxSummary(m1, report = report);
	invisible(m1)
}
