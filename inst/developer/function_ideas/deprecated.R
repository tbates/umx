#' Extract log Likelihood from an MxModel
#'
#' Returns the log likelihood for an OpenMx model. This helper also 
#' enables \code{\link{AIC}}(model); \code{\link{BIC}}(model).
#'
#' *note*: this is the (natural) log of the likelihood, not -2*log(likelihood). To
#' recover -2*ll, multiply the returned value by -2
#' For logLik for other types of object, see \code{\link{logLik}}.
#' 
#' @details
#' hat-tip Andreas Brandmaier
#'
#' @method logLik MxModel
#' @rdname logLik.MxModel
#' @export
#' @param object the \code{\link{mxModel}} from which to get the log likelihood
#' @param ... Optional parameters
#' @return - the log likelihood
#' @seealso - \code{\link{logLik}}, \code{\link{AIC}}, \code{\link{umxCompare}}
#' @family Reporting functions
#' @references - \url{http://openmx.ssri.psu.edu/thread/931#comment-4858}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' logLik(m1)
#' AIC(m1)
#'@md
logLik.MxModel <- function(object, ...) {
	model = object # just to be clear that object is a model
	logLikelihood = NA
	if (!is.null(model$output) & !is.null(model$output$Minus2LogLikelihood)){
		logLikelihood = (-0.5) * model$output$Minus2LogLikelihood		
	}
	if (!is.null(model$data)){
		attr(logLikelihood,"nobs") = model$data$numObs
	}else{ 
		attr(logLikelihood,"nobs") = NA
	}
	if (!is.null(model$output)){
		attr(logLikelihood,"df") = length(model$output$estimate)	
	} else {
		attr(logLikelihood, "df") = NA
	}
	class(logLikelihood) = "logLik"
	return(logLikelihood);
}