library(OpenMx)
nOrd = 3
nVar = 5
fit1 = mxModel("fit1",
	mxMatrix(name = "a"     , "Full", nrow = nVar, ncol = nVar, free = T, values = 1:25),
	mxMatrix(name = "unit"  , "Unit", nrow = nOrd, ncol = 1),
	mxMatrix(name = "Filter", "Full", nrow = nOrd, ncol = nVar, free = FALSE, values = diag(1, nrow = nOrd, ncol = nVar)),
	mxAlgebra(Filter %*% diag2vec(a), name = "ordElements"),
	mxConstraint(name = "con1", ordElements == unit),
	mxFitFunctionAlgebra("ordElements")
)
fit1 = mxRun(fit1)
mxEval(list(ordElements), fit1)

#' umx_ord_filter
#'
#' DESCRIPTION
#'
#' @param x is the data
#' @param 
#' @return - \code{\link{mxMatrix}} which when 
#' @export
#' @family umx core functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' \dontrun{
#' model = umx_ord_filter(model)
#' }

umx_ord_filter <- function(x) {
	mxMatrix(name = "Filter", "Full", nrow = nOrd, ncol = nVar, free = FALSE, values = diag(1, nrow = nOrd, ncol = nVar))
}


umx_constrain_ordinal <- function(x) {
	mxMatrix(name = "a"     , "Full", nrow = nVar, ncol = nVar, free = T, values = 1:25),
	mxMatrix(name = "unit"  , "Unit", nrow = nOrd, ncol = 1),
	mxMatrix(name = "Filter", "Full", nrow = nOrd, ncol = nVar, free = FALSE, values = diag(1, nrow = nOrd, ncol = nVar)),
	mxAlgebra(Filter %*% diag2vec(a), name = "ordElements"),
	mxConstraint(name = "con1", ordElements == unit),
}
