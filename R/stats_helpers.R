# #' Easily use the Box-Cox transform
# #'
# #' @description
# #' Applies Box-Cox to the input columns. Box-Cox finds (x ^ (lambda - 1)) / lambda
# #'
# #' | \eqn{\lambda} | Transformation  |
# #' |:--------------|:----------------|
# #' | -2            | \eqn{1/x^2}     |
# #' | -1            | 1/x             |
# #' | -.5           | 1/sqrt(x)       |
# #' | 0             | log(x)          |
# #' | 0.5           | sqrt(x)         |
# #'
# #' This is a crucial processing step. You should graph (`plotit =TRUE`) and examine.
# #'
# #' Improvements would allow taking more than one column and (optionally)# 
# #' snapping to common values.
# #' @details
# #'
# #' @param x A variable to transform.
# #' @param verbose Plot the likelihood of lambda and show nearest "common" transform.
# #' @return - The Box-Cox transformed variable.
# #' @export
# #' @family Data Functions
# #' @seealso - [MASS::boxcox()]
# #' @references Box, G. E. P. and Cox, D. R. (1964) An analysis of transformations (with discussion). 
# #' *Journal of the Royal Statistical Society B*, **26**, 211-252. <https://www.jstor.org/stable/2984418>
# 
# #' @examples
# #' tmp = log(1:100)
# #' hist(tmp)
# #' hist(umx_boxcox(tmp))
# umx_boxcox <- function(x, verbose = TRUE, shiftMin=FALSE) {
# 	if(min(x)<0){
# 		con.expr
# 	} else {
# 		alt.expr
# 	}
# 	xbox = MASS::boxcox(lm(x ~ 1), plotit = verbose)
# 	lambda = xbox$x[which.max(xbox$y)]
# 	if(lambda == 0){
# 		newX = log(x)
# 	} else {
# 		newX = ((x^lambda) - 1) / lambda
# 	}
# 	if(verbose){
# 		nearest = c("1/x^2", "1/x", "1/sqrt(x)", "log(x)", "sqrt(x)")[which.min(abs(lambda - c(-2, -1, -.5, 0, 0.5)))]
# 		cat("lambda = ", lambda, "\nTransform was: (x ^ ", lambda, " - 1) / ", lambda, "\nThe nearest common transform is:", 		omxQuotes(nearest))
# 	}
# 	return(newX)
# }
