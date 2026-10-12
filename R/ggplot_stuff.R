#' Add a fit statistic to a ggplot
#'
#' @description
#' Add a fit statistic to a ggplot
#'
#' @param model a statistical model which contains a fit measure.
#' @param effect optional hard coded fit/effect.
#' @param xloc x location of R.
#' @param yloc y location of R.
#' @return - plot
#' @export
#' @family Plotting functions
#' @seealso - [umxPlot()], [umxPlotFun()]
#' @examples
#' \dontrun{
#'	m1 = lm(mpg ~ wt, data = mtcars)
#'	p = ggplot2::ggplot(data = mtcars, aes(x = wt, y = mpg))+ geom_point() +geom_smooth()+
#'	ggAddR(m1, effect = NA, xloc=2, yloc= 10); p
#' }
ggAddR <- function(model, effect = NA, xloc=8, yloc= 10) {
	if(is.na(effect)){
		r2 = round(summary(model)$r.squared, 3)
		lab = bquote(R^2 == .(r2))
		return(cowplot::draw_label(lab, x = xloc, y = yloc, fontfamily = "Times", size = 12))
	} else {
		return(cowplot::draw_label(effect, x = xloc, y = yloc, fontfamily = "Times", size = 12))
	}
}
