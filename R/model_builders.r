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
#' @param model an \code{\link{mxModel}} to WITH
#' @return - \code{\link{mxModel}}
#' @family super easy helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
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
		mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
		mxData(cov(data), type="cov", numObs = nrow(data))
	)
	m1 = mxRun(m1);
	umxSummary(m1, report = report);
	invisible(m1)
}
