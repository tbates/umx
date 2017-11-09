#' Test if a factor model is identified
#'
#' @description
#' Test if a factor model is identified by establishing if the number of variables is equal too or grater than
#' the number of model parameters. See also \code{\link{mxCheckIdentification}} for checking actual models.
#'
#' @param nVariables the number of variables measured.
#' @param nFactors the number of factors posited.
#' @return - Binary
#' @export
#' @family Teaching Functions
#' @family Reporting Functions
#' @seealso - \code{\link{mxCheckIdentification}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' tmx_is.identified(nVariables = 2, nFactors = 1) # FALSE
#' tmx_is.identified(nVariables = 3, nFactors = 1) # TRUE
#' tmx_is.identified(nVariables = 4, nFactors = 2) # FALSE
#' tmx_is.identified(nVariables = 5, nFactors = 2) # TRUE
tmx_is.identified <- function(nVariables, nFactors){
	as.logical(1 + sign((nVariables * (nVariables - 1)/2) - nVariables * nFactors + nFactors * (nFactors - 1) / 2) )
}


#' Graphical display of genotypic effects
#'
#' @description
#' The d and h increments of the gene difference A – a. Aa may lie on either side of m and the sign of h will
#' vary accordingly; in the case illustrated h would be negative. (Adapted from Mather and Jinks, 1977, p. 32).
#'
#' @param u The XXXX (default = .5)
#' @param v The XXXX (default = .5)
#' @param h The XXXX (default = .5)
#' @param d The XXXX (default = 1)
#' @return - 
#' @export
#' @family Teaching Functions
#' @seealso - 
#' @references - \url{https://github.com/tbates/umx}, \url{https://github.com/tbates/BGBook/issues/23#issuecomment-333834075}
#' @examples
#' tmxGenotypicEffect(u = .5, v=.5, h=.5, d = 1)
tmxGenotypicEffect <- function(u = .5, v=.5, h=.5, d = 1){
	message("not done yet")
	# TODO: tmxGenotypicEffect: output plot of Genotypic Effect ~ Gene Doe
	# TODO: tmxGenotypicEffect: output plot of Genotypic Effect ~ Gene Doe
}

