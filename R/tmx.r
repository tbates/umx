# Figure released under CCBy at
#    https://figshare.com/s/b6552245d88dd8892726
# This file living at
#    https://github.com/tbates/BGBook/blob/master/figs/c03_genotypic_effect_by_gene_dose.R
# TODO: What does this show
# Used in chapter 03 of BG Methods, 2ed, Derivation of expected twin covariances
# Based on dataf1.ps p44 in previous edition

#' Graphical display of genotypic effects.
#'
#' @description
#' tmx_genotypic_effect allows you to explore the concept of genotypic effect at a locus.
#' The d and h increments of the gene difference A – a. Aa may lie on either side of m and the sign of h will
#' vary accordingly; in the case illustrated h would be negative. (Adapted from Mather and Jinks, 1977, p. 32).
#' See book issue 23
#' 
#' old system:
#' $d$ =  half the homozygotes difference (midpoint = $m$)
#' $h$ = measured deviation of the heterozygote from $m$
#' new system:
#' u, v -> p, q
#' d, h -> a, d
#' 
#' Neale, M. C. (2005). Quantitative Genetics. In Encyclopedia of Life Sciences. New York: John Wiley & Sons, Ltd.
#'
#' @param p The proportion of the A allele (default = .5)
#' @param q The proportion of the a allele (default = .5)
#' @param a Half the difference between the homozygotes (default = .5)
#' @param d The deviation of the heterozygote from m (default = 1)
#' @param m The midpoint between the homozygotes (default = 0)
#' @return - 
#' @export
#' @family Teaching Functions
#' @seealso - 
#' @references - \url{http://www.vipbg.vcu.edu/vipbg/Articles/2005_book_chapters_ascertainment_corrections.pdf},
#'  \url{https://github.com/tbates/umx},
#'  \url{https://github.com/tbates/BGBook/issues/23#issuecomment-333834075}
#' @examples
#' library(umx);
#' library(ggplot2);
#' tmx_GenotypicEffect(p = .5, q =.5, a =.5, d = 1, m = 0)
#' p = tmx_GenotypicEffect(p = .5, q = .5, a = 1, d = .0, m = 0, show = TRUE);
#' p = tmx_GenotypicEffect(p = .5, q = .5, a = 1, d = .5, m = 0, show = TRUE); 
#' p
#' # p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
#' # ggsave(paste0(base, "c03_genotypic_effect_by_gene_dose.pdf"), width = 4.6, height = 4.6)
tmx_GenotypicEffect <- function(p = .5, q = .5, a = 1, d = .5, m = 0, show = TRUE){
	# p  = .5; q = .5; a = 1; d = .5; m = 0
	# ===========================================================
	# = Genotypic Values form (in terms of the regression line) =
	# ===========================================================
	# p56, pink book
	GAA_A = (2 * q) * (a + d * (q - p)) ; GAA_D = 2 * q^2 * d; GAA = (GAA_A - GAA_D); GAA_f = p^2
	GAa_A = (q - p) * (a + d * (q - p)) ; GAa_D = 2 * p*q * d; GAa = (GAa_A + GAa_D); GAa_f = 2 * p * q
	Gaa_A = (-2* p) * (a + d * (q - p)) ; Gaa_D = 2 * p^2 * d; Gaa = (Gaa_A - Gaa_D); Gaa_f = q^2
	
	# mu = 2 * p^2 + 2 * p * q
	# meanDose = 2 * p^2 + 2 * p * q  # eq 3.14
	# TODO maybe print this, with marginal sum?
	# df$effect * df$freq; sum(df$effect * df$freq) # sums to zero
	# TODO add examples on page 57
	df <- data.frame(stringsAsFactors = FALSE,
		dose     = c(0    , 1    , 2    ),
		genotype = c("aa" , "Aa" , "AA" ),
		effect   = c(Gaa  , GAa  , GAA  ),
		freq     = c(Gaa_f, GAa_f, GAA_f)
	)
	# Plot regression line, and points
	p = qplot(x = dose, y = effect, geom = "point", size = freq, xlab = "Gene Dose", ylab = "Genotypic VALUE", data = df)
	print(p)
	# ================================================
	# = Genotypic EFFECT model (the regression line) =
	# ================================================
	# p55, pink book, fig 3.2
	df <- data.frame(stringsAsFactors = FALSE,
		dose     = c(0    , 1    , 2    ),
		genotype = c("aa" , "Aa" , "AA" ),
		effect   = c(a    , d    , -a   ),
		freq     = c(Gaa_f, GAa_f, GAA_f)
	)
	b  = a + (q - p) * d
	umx_msg(b) # 1 in figure 3.2

	# Plot regression line, and points.
	# p = qplot(x = dose, y = (dose-1 * b) + -(.5*h)+ m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic EFFECT", data = df)
	p = qplot(x = dose, y = (dose-1 * b) + (.5 * d) + m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic EFFECT", data = df)
	# adding label directly to plot, in the data coordinates
	p = p + cowplot::draw_label("x", x = 1, y = m, hjust = 0, vjust = 0)

	# correct
	p = p + scale_x_continuous(breaks = c(0, 1, 2))
	if(d == 0){
		p = p + scale_y_continuous(breaks = c(-a, m, a), labels = c("-a", "d = m", "+a"))
	} else {
		p = p + scale_y_continuous(breaks = c(-a, m, d, a), labels = c("-a", "m", "d", "+a"))
	}
	# p = p + geom_text("aa", x = 0, y = -d, hjust = .1)
	p = p + cowplot::draw_text("aa", x = 0, y = -a)
	p = p + cowplot::draw_text("Aa", x = 1, y =  d)
	p = p + cowplot::draw_text("AA", x = 2, y =  a)
	if(show){
		print(p)
	}
	p
}

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
