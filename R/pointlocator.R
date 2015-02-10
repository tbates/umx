#' umx_locate
#'
#' Locate points on a graph by clicking!
#'
#' @param n number of points to locate (default = 1)
#' @param x the x data (to allow the locate function to find your points)
#' @param y the y data 
#' @param col = rgb(1,0,0,0.5)
#' @param pch = 20
#' @param ... Additional parameters (passed into points())
#' @return - vector of points clicked on
#' @export
#' @family umx miscellaneous
#' @references - http://menugget.blogspot.co.uk/2014/12/point-locator-function.html
#' @examples
#' \dontrun{
#' n <- 200
#' x <- sort(runif(n, min = 0, max = 10 * pi))
#' y <- sin(x) + rnorm(n, sd = 0.2)
#'  
#' # Select 10 points at maxima and minima
#' op <- par(mar = c(4, 4, 1, 1))
#' plot(x, y, cex = 2)
#' pos <- umx_locate(10, x, y, col = rgb(1, .2, .2, .75), cex = 2)
#' par(op)
#' pos
#' }
umx_locate <- function(n=1, x, y, col=rgb(1,0,0,0.5), pch=20, ...){
  xsc <- scale(x)
  ysc <- scale(y)
  pos <- seq(n)*NaN
  for(i in seq(n)){
    print(paste("choose point", i))
    pt <- locator(1)
    ptxsc <- scale(pt$x, center = attr(xsc, "scaled:center"), scale = attr(xsc, "scaled:scale"))
    ptysc <- scale(pt$y, center = attr(ysc, "scaled:center"), scale = attr(ysc, "scaled:scale"))
    pos.i <- which.min(sqrt((c(ptxsc) - c(xsc))^2 + (c(ptysc) - c(ysc))^2))
    points(x[pos.i], y[pos.i], col = col, pch = pch, ...)
    pos[i] <- pos.i
  }
  pos    
}

