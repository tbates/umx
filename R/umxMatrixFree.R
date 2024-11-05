#' Sets labeled matrix cells to free 
#'
#' @description
#' In simulation studies, it is often necessary to rewrite the matrices while testing
#' alternative specifications. This can become very tedious with increasing 
#' number of distinct specifications. This tool injects changes into umxMatrix so
#' that this tasks gets more manageable. First, it sets byRow by default. Second,
#' it infers the number of rows automatically. The user needs only passing ncol.
#' Finally and most importantly this function disables auto-labeling, and whenever
#' a label is set, that cell position will be freed. It is required to pass 
#' a matrix of labels, as well as a label name. 
#' 
#' @param name The name of the matrix: Must be set
#' @param nrow Number of rows in the matrix (Optional)
#' @param ncol Number of columns in the matrix (Required)
#' @param free Whether cells are free (Default FALSE)  
#' @param values The values of the matrix (Default NA)
#' @param labels The labels of the matrix (Default NA)
#' @param byrow Default for byrow (TRUE)
#' @param ...  Accepts all other arguments from [umxMatrix()]
#' @return - [OpenMx:: mxMatrix()]
#' @export
#' @seealso - [umxMatrix()]
#' @md
#' @examples
#' \dontrun{
#' 
#' umxMatrixFree('E', type='Symm',  ncol = 3,
#'   labels =c("eb2",NA,NA,
#'              NA,"es2",NA,
#'             NA,NA,NA),
#'   values=c(.2,0,0,
#'            0,.2,0,
#'            0,0,0))
#'            
#'  # Will return a umxMatrix free at the eb2 and es2 positions.
#'}

umxMatrixFree  <- function (name = name, nrow = NULL, ncol = NA, free = FALSE, values = NA, labels = labels, byrow = TRUE, ...){
  if (missing(nrow)) {
    if (!missing(free)) {nrow = dim(matrix(free, ncol = ncol))[1]}
    if (!missing(labels)) {nrow = dim(matrix(labels, ncol = ncol))[1]}
    if (!missing(values)) {nrow = dim(matrix(values, ncol = ncol))[1]}
  }
  
  if (!missing(labels)) {
    free = labels
    free = (!is.na(labels))
  }
  
  dots <- list(...)
  do.call(eval(parse(
    text="umx::umxMatrix")),
    c(list(name = name, nrow = nrow, ncol = ncol, free = free ,values = values, labels = labels, byrow = byrow),
      dots[names(dots)])
  )
}