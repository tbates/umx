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
#' This function lets you explore the simplest two–allele system (B and b), with three possible genotypes, BB, Bb, and bb.
#' Parameter "b" is half the measured difference between the homozygotes BB and bb. The point between the two homozygotes 
#' is "m" -- the mean effect of homozygous genotypes. "d" defines the measured deviation effect of the heterozygote Aa, 
#' relative to this expected midpoint. "a" and "d" are genotypic effects.
#' 
#' old system:
#' 
#' $d$ =  half the homozygotes difference (midpoint = $m$)
#' 
#' $h$ = measured deviation of the heterozygote from $m$
#' 
#' new system:
#' 
#' u, v -> p, q
#' 
#' d, h -> a, d
#' 
#' Neale, M. C. (2005). Quantitative Genetics. In Encyclopedia of Life Sciences. New York: John Wiley & Sons, Ltd.
#' https://github.com/tbates/BGBook/issues/23#issuecomment-333834075
#' 
#' @param p The frequency of the A allele (default .5)
#' @param q The frequency of the a allele (default .5)
#' @param a Half the difference between the AA and aa homozygotes (default .5)
#' @param m The value of the midpoint between the homozygotes (default 0)
#' @param d The deviation of the heterozygote from m (default 1)
#' @return - 
#' @export
#' @family Teaching Functions
#' @seealso - 
#' @references - \url{http://www.vipbg.vcu.edu/vipbg/Articles/2005_book_chapters_ascertainment_corrections.pdf},
#'  \url{https://github.com/tbates/umx}
#' @examples
#' library(umx);
#' tmx_genotypic_effect(p = .5, q =.5, a =.5, d = 1, m = 0)
#' p = tmx_genotypic_effect(p = .5, q = .5, a = 1, d = .0, m = 0, show = TRUE);
#' p = tmx_genotypic_effect(p = .5, q = .5, a = 1, d = .5, m = 0, show = TRUE); 
#' p
#' # p + geom_point() + geom_text(hjust = 0, nudge_x = 0.05)
#' # ggsave(paste0(base, "c03_genotypic_effect_by_gene_dose.pdf"), width = 4.6, height = 4.6)
tmx_genotypic_effect <- function(p = .75, q = .25, a = .5, d = .25, m = 0, show = TRUE){
	require(ggplot2)
	# Genotypes BB Bb bb Frequency p2 2pq q2
	df <- data.frame(stringsAsFactors = FALSE,
		Genotypes = c("Frequency", "fraction"),
		BB = c("p\u00B2", round(p^2, 2)),
		Bb = c("2pq"    , round(2 * p * q, 2)),
		bb = c("q\u00B2", round(q^2, 2))
	)
	message("Genotype frequencies at Hardy Weinberg Equilibrium (p= ", p," q = ", q, ")")
	umx_print(df)
	
	# ===========================================================
	# = Genotypic Values form (in terms of the regression line) =
	# ===========================================================
	# p  = .5; q = .5; a = 1; d = .5; m = 0
	# genotypic values section of pink book
	# G_BB_A = additive component of Genotypic value of BB
	G_BB_A = (2 * q) * (a + d * (q - p));
	G_BB_D =  2 * q^2 * d; 
	G_Bb_A = (q - p) * (a + d * (q - p));
	G_Bb_D =  2 * p*q * d; 
	G_bb_A = (-2* p) * (a + d * (q - p));
	G_bb_D =  2 * p^2 * d; 
	
	G_bb   = (G_bb_A - G_bb_D);
	G_Bb   = (G_Bb_A + G_Bb_D);
	G_BB   = (G_BB_A - G_BB_D);
	G_bb_f = q^2
	G_Bb_f = 2 * p * q
	G_BB_f = p^2
	
	# mu = 2 * p^2 + 2 * p * q
	# meanDose = 2 * p^2 + 2 * p * q  # eq 3.14
	# TODO Print this, with marginal sum? put tables and plot on one page in browser?
	# df$effect * df$freq; sum(df$effect * df$freq) # sums to zero
	# TODO add examples on page 57
	df <- data.frame(stringsAsFactors = FALSE,
		dose     = c(0            , 1     , 2      ),
		genotype = c("bb"         , "Bb"  , "BB"   ),
		value    = c(G_bb         , G_Bb  , G_BB   ),
		freq     = factor(c(G_bb_f, G_Bb_f, G_BB_f))
	)
	# Plot regression line, and points (sized to frequency)
	thePlot = qplot(x = dose, y = value, geom = "point", size = freq, xlab = "Gene Dose", ylab = "Genotypic VALUE", data = df)
	thePlot = thePlot + scale_x_continuous(breaks = c(0, 1, 2)) # just label the legal values: 0, 1, 2
	print(thePlot)

	# ================================================
	# = Genotypic EFFECT model (the regression line) =
	# ================================================
	# p55, pink book, fig 3.2
	df <- data.frame(stringsAsFactors = FALSE,
		dose     = c(0     , 1    , 2   ),
		genotype = c("bb"  , "Bb" , "BB"),
		effect   = c(-a    , d    , a   ),
		freq     = c(G_bb_f, G_Bb_f, G_BB_f)
	)
	message("Genotypic Effects")
	umx_print(df)

	# =====================================
	# = Plot regression line, and points. =
	# =====================================
	# 1. Compute slope (b) of genotypic-effect regression line.
	b  = a + (q - p) * d
	# 2. Plot genotypic-effect regression line.
	thePlot = qplot(x = dose, y = (dose-1 * b) + (.5 * d) + m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic Effect", data = df)
	thePlot = thePlot + theme(text = element_text(family = "Optima", size= 14)) # face = "bold"
	thePlot = thePlot + scale_x_continuous(breaks = c(0, 1, 2))
	
	# 2. Add labels to plot, in the data coordinates.
	# don't draw m, misleading
	# thePlot = thePlot + geom_point(aes(x = 1, y=m), color="red") + cowplot::draw_label("m", x = 1+.1, y = m, fontfamily="Optima")

	# set the y axis
	# TODO: lave numbers on y axis, add text labels beside these?
	if(d == 0){
		thePlot = thePlot + scale_y_continuous(breaks = c(-a, m, a), labels = c("-a", "d = m", "+a"))
	} else {
		thePlot = thePlot + scale_y_continuous(breaks = c(-a, m, d, a), labels = c("-a", "m", "d", "+a"))
	}
	# TODO: plot points on the line, add text labels beside these
	addArrow <- function(thePlot, x1, b, d, m, y2) {
		segs <- data.frame(
			x1 = x1,
			y1 = (x1 - 1) * b + .5 * d + m,
			y2 = y2
		)
		# thePlot + geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), arrow = arrow(length=unit(0.30,"cm")), data = segs)
		thePlot + geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), arrow = arrow(length=unit(0.10,"cm")), data = segs)
	}
	# bb
	thePlot = thePlot + geom_point(aes(x = 0, y = -a), color = "black") 
	thePlot = thePlot + cowplot::draw_label("bb", x = 0+.1, y = -a, fontfamily = "Optima")
	thePlot = addArrow(thePlot, x1 = 0, b=b, d=d, m=m, y2=-a)
	# Bb
	thePlot = thePlot + geom_point(aes(x = 1, y= d), color = "black") 
	thePlot = thePlot + cowplot::draw_label("Bb", x = (1-.1), y =  d, fontfamily = "Optima")
	thePlot = addArrow(thePlot, x1 = 1, b=b, d=d, m=m, y2=d)
	# BB
	thePlot = thePlot + geom_point(aes(x = 2, y= a), color = "black") 
	thePlot = thePlot + cowplot::draw_label("BB", x = 2, y =  (a -.1), fontfamily = "Optima")
	thePlot = addArrow(thePlot, x1 = 2, b = b, d = d, m = m, y2 = a)

	thePlot = thePlot + expand_limits(y = c(-a, a))
	if(show){
		print(thePlot)
	}
	thePlot
	# high blood pressure will be defined as >=130/80 millimeters of mercury (previous guideline = 140/90)
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
