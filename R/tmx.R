# "I like ambitious goals. I

# TODO Create a tmx path tracing rules
# TODO Fix up umxUnexplainedCausalNexus()


# Thresholded normal Figure released under CCBy at
#    https://figshare.com/s/b6552245d88dd8892726
# This file living at
#    https://github.com/tbates/BGBook/blob/master/figs/c03_genotypic_effect_by_gene_dose.R
# TODO: What does this show
# Used Derivation of expected twin covariances. In chapter 03, BG Methods, 2ed.
# See also dataf1.ps p44 in previous edition.

#' Graphical display of genotypic effects.
#'
#' @description
#' `tmx_genotypic_effect` allows you to explore the concept of genotypic effect at a locus. With it,
#' you can interactively explore the effects of allele frequency, additive variance, and 
#' [dominance](https://en.wikipedia.org/wiki/Dominance_(genetics)).
#' 
#' This function lets you explore the simplest two–allele system (`B` and `b`), with three possible 
#' genotypes, `BB`, `Bb`, and `bb`.
#' 
#' The point between the two homozygotes is `m` -- the mean effect of the homozygous genotypes. 
#' 
#' Parameter `a` is half the measured phenotypic difference between the homozygotes `BB` and `bb`.
#' It corresponds to the additive effect of each additional `B` allele, relative to the `bb` phenotype.
#'
#' Parameter `d` is the deviation of the heterozygote `Bb` phenotype from the homozygote mid-point `m`.
#' It corresponds to the non-additive (dominance) effect of the `B` allele. The heterozygote phenotype 
#' may lie on either side of `m` and the sign of `d` will vary accordingly.
#'
#' **Old system** from book ed 2:
#' 
#' Adapted from Mather and Jinks, 1977, p. 32). See book issue old-style nomenclature
#' https://github.com/tbates/BGBook/issues/23 
#' 
#' `u` = Frequency of the dominant allele (now = `p`).
#' `v` = Frequency of the recessive allele (now = `q`).
#'
#' `m` = midpoint between the two homozygotes
#' `d` = half the difference between the two homozygote (now `a`)
#' 
#' `h` = deviation of the heterozygote from `m` (now = `d`)
#' 
#' New system:
#' 
#' `u` and `v` -> `p` and  `q`
#' 
#' `d` and `h` -> `a` and `d`
#' 
#' See BGBook issue 23
#' 
#' @param p The frequency of the B allele (Default .5)
#' @param q The frequency of the b allele (Default 1-p)
#' @param a Half the difference between the two homozygote phenotypes (Default .5)
#' @param m The value of the midpoint between the homozygotes (Default 0)
#' @param d The deviation of the heterozygote from m (Default 0)
#' @param show Whether to draw the plot or just return it (Default = TRUE)
#' @return - optional plot
#' @family Teaching and testing Functions
#' @export
#' @references - Neale, M. C. (2005). Quantitative Genetics. In Encyclopedia of Life Sciences. New York: John Wiley & Sons, Ltd.
#'  [pdf](https://vipbg.vcu.edu/vipbg/Articles/2005_book_chapters_ascertainment_corrections.pdf)
#' @md
#' @examples
#' library(umx);
#' 
#' \donttest{
#' # =========================
#' # = Pure additivity: d= 0 =
#' # =========================
#' tmx_genotypic_effect(p = .5, a = 1, d = 0, m = 0, show = TRUE);
#' 
#' # =============================
#' # = Complete dominance: a=d=1 =
#' # =============================
#' tmx_genotypic_effect(p = .5, q =.5, a = 1, d = 1, m = 0, show = TRUE);
#' 
#' # ===========================
#' # = Over dominance: a< d =1 =
#' # ===========================
#' tmx_genotypic_effect(p = .5, q =.5, a =.5, d = 1, m = 0)
#'
#' p = tmx_genotypic_effect(p = .5, q = .5, a = 1, d = .5, m = 0, show = TRUE); 
#' # p + ggplot2::geom_point() + ggplot2::geom_text(hjust = 0, nudge_x = 0.05)
#' # ggsave(paste0(base, "c03_genotypic_effect_by_gene_dose.pdf"), width = 4.6, height = 4.6)
#' }
#' 
tmx_genotypic_effect <- function(p = .75, q = (1-p), a = .5, d = 0, m = 0, show = TRUE){
	# TODO Print this, with marginal sum? put tables and plot on one page in browser?
	# high blood pressure will be defined as >=130/80 millimeters of mercury (previous guideline = 140/90)

	if(!(p+q)==1){
		stop("p+q must = 1.0 Yours sum to ", p + q)
	}

	residual_with_line <- function(thePlot, lab, x, y, b, d, m, xoffset = .1) {
		# 1. plot the residual descender/ascender line
		segs <- data.frame(
			x1 = x,
			y1 = (x - 1) * b + .5 * d + m,
			y2 = y
		)
		# thePlot + geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), arrow = arrow(length=unit(0.30,"cm")), data = segs)
		thePlot = thePlot + ggplot2::geom_segment(aes(x = x1, y = y1, xend = x1, yend = y2), data = segs)
		# 2. plot the point
		thePlot = thePlot + ggplot2::geom_point(aes(x = x1, y = y2), color = "black", data = segs) 
		# 3. label the point
		thePlot = thePlot + cowplot::draw_label(lab, x = (x + xoffset), y = y, fontfamily = "Times", fontface = "italic")
		return(thePlot)
	}
	# Genotypes BB Bb bb Frequency p2 2pq q2
	df = data.frame(stringsAsFactors = FALSE,
		Genotypes = c("Frequency", "fraction"),
		BB = c("p\u00B2", round(p^2, 2)),
		Bb = c("2pq"    , round(2 * p * q, 2)),
		bb = c("q\u00B2", round(q^2, 2))
	)
	message("Genotype frequencies at Hardy Weinberg Equilibrium (p= ", p," q = ", q, ")")
	
	# ===============================================================
	# = Genotypic Values (G) form (in terms of the regression line) =
	# ===============================================================
	# p  = .5; q = .5; a = 1; d = .5; m = 0
	# genotypic values section of pink book
	# G_BB_A = additive component of Genotypic value of BB

	G_bb_A = (-2 * p) * (a + (d * (q - p)));
	G_Bb_A = ( q - p) * (a + (d * (q - p)));
	G_BB_A = ( 2 * q) * (a + (d * (q - p)));

	G_bb_D =  2 * p^2 * d; 
	G_Bb_D =  2 * p*q * d; 
	G_BB_D =  2 * q^2 * d; 
	
	G_bb   = (G_bb_A - G_bb_D);
	G_Bb   = (G_Bb_A + G_Bb_D);
	G_BB   = (G_BB_A - G_BB_D);

	G_bb_f = q^2
	G_Bb_f = 2 * p * q
	G_BB_f = p^2
	
	# mu = 2 * p^2 + 2 * p * q
	# meanDose = 2 * p^2 + 2 * p * q  # eq 3.14
	# df$effect * df$freq; sum(df$effect * df$freq) # sums to zero
	# TODO add examples on page 57
	df = data.frame(stringsAsFactors = FALSE,
		dose     = c(0            , 1     , 2      ),
		genotype = c("bb"         , "Bb"  , "BB"   ),
		value    = c(G_bb         , G_Bb  , G_BB   ),
		freq     = factor(c(G_bb_f, G_Bb_f, G_BB_f)),
		Frequency  = c(G_bb_f, G_Bb_f, G_BB_f)
	)
	# umx_msg(df$freq)
	# Plot regression line, and points (sized to frequency).
	# Slope for the genotypic value plot.
	b = a + (q - p) * d
	thePlot = ggplot2::qplot(x = dose, y = value, geom = "point", size = Frequency, xlab = "Gene Dose", ylab = "Genotypic VALUE", data = df)
	thePlot = qplot(x = dose, y = ((dose - 1) * b) + (.5 * d) + m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic Effect", data = df)
	
	thePlot = qplot(x = dose, y = value, geom = "point", size = Frequency, xlab = "Gene Dose", ylab = "Genotypic VALUE", data = df)
	thePlot = thePlot + scale_x_continuous(breaks = c(0, 1, 2)) # Just label the legal values: 0, 1, 2
	print(thePlot)

	# ================================================
	# = Genotypic EFFECT model (the regression line) =
	# ================================================
	# p55, pink book, fig 3.2
	df = data.frame(stringsAsFactors = FALSE,
		dose     = c(0     , 1     , 2     ),
		genotype = c("bb"  , "Bb"  , "BB"  ),
		effect   = c(-a    , d     , a     ),
		freq     = c(G_bb_f, G_Bb_f, G_BB_f)
	)
	message("Genotypic Effects")
	umx_print(df)

  # ==================
  # = 1. Create plot =
  # ==================
  
	# 1. Compute slope (b) of genotypic-effect regression line.
	# thePlot = qplot(x = dose, y = ((dose - 1) * b) + (.5 * d) + m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic Effect", data = df)
	b = a
	thePlot = qplot(x = dose, y = ((dose - 1) * b) + (.5 * d) + m, geom = "line", xlab = "Gene Dose", ylab = "Genotypic Effect", data = df)

	thePlot = thePlot + theme(text = element_text(family = "Times", size= 14)) # face = "bold"
	thePlot = thePlot + scale_x_continuous(breaks = c(0, 1, 2))

	# =====================================
	# = Plot regression line, and points. =
	# =====================================
	# 2. Plot genotypic-effect regression line.
	# thePlot = thePlot + geom_abline(intercept = (.5 * d) + m, slope = b)

	# 2. Add labels to plot, in the data coordinates.
	# thePlot = thePlot + geom_point(aes(x = 1, y=m), color="red") + cowplot::draw_label("m", x = 1+.1, y = m, fontfamily = "Times")

	# set the y axis
	# Leave numbers on y axis, add text labels beside these.
	# cowplot::draw_label
	innerLoc = -.2
	thePlot = thePlot + draw_label("-a", x = innerLoc, y = -a, fontfamily = "Times", fontface = "italic")
	thePlot = thePlot + draw_label( "a", x = innerLoc, y =  a, fontfamily = "Times", fontface = "italic")
	if(d == m){
		# thePlot = thePlot + scale_y_continuous(breaks = c(-a, m, a), labels = c("-a", "d = m", "+a"))
		thePlot = thePlot + draw_label( "d=m", x = innerLoc, y =  m, fontfamily = "Times", fontface = "italic")
	} else {
		# thePlot = thePlot + scale_y_continuous(breaks = c(-a, m, d, a), labels = c("-a", "m", "d", "+a"))
		thePlot = thePlot + draw_label( "m", x = innerLoc, y =  m, fontfamily = "Times", fontface = "italic")
		thePlot = thePlot + draw_label( "d", x = innerLoc, y =  d, fontfamily = "Times", fontface = "italic")
	}

	# 2. Plot bb, Bb, and BB points, with text labels, and vertical line segment showing residual from regression
	thePlot = residual_with_line(thePlot, lab = "bb", x = 0, y = -a, b = b, d = d, m = m)
	thePlot = residual_with_line(thePlot, lab = "Bb", x = 1, y =  d, b = b, d = d, m = m)
	thePlot = residual_with_line(thePlot, lab = "BB", x = 2, y =  a, b = b, d = d, m = m)

	thePlot = thePlot + expand_limits(y = c(-a, a), x = c(-.2, 2.2))
	if(show){
		print(thePlot)
	}
	invisible(thePlot)
}

#' Test if a factor model is identified
#'
#' @description
#' Test if a factor model is identified by establishing if the number of variables is equal too or grater than
#' the number of model parameters. See also [mxCheckIdentification()] for checking actual models.
#'
#' @param nVariables the number of variables measured.
#' @param nFactors the number of factors posited.
#' @return - Binary
#' @export
#' @family Teaching and testing Functions
#' @seealso - [mxCheckIdentification()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' tmx_is.identified(nVariables = 2, nFactors = 1) # FALSE
#' tmx_is.identified(nVariables = 3, nFactors = 1) # TRUE
#' tmx_is.identified(nVariables = 4, nFactors = 2) # FALSE
#' tmx_is.identified(nVariables = 5, nFactors = 2) # TRUE
tmx_is.identified <- function(nVariables, nFactors){
	as.logical(1 + sign((nVariables * (nVariables - 1)/2) - nVariables * nFactors + nFactors * (nFactors - 1) / 2) )
}


# Define generic tmx_show ...
#' Show matrices of models in a easy-to-learn-from format. 
#'
#'
#' @param x an object e.g. [umxRAM()] [umxMatrix()] from which to show parameters.
#' @param what legal options are "values" (default), "free", or "labels").
#' @param show filter on what to show c("all", "free", "fixed").
#' @param matrices to show  (default is c("S", "A")). "thresholds" in beta.
#' @param digits precision to report. Default = round to 2 decimal places.
#' @param na.print How to display NAs (default = "")
#' @param zero.print How to display 0 values (default = ".")
#' @param report How to report the results. "html" = open in browser.
#' @param style The style for the table (Defaults to "paper". Other options are "material_dark", "classic", "classic_2", "minimal", "material")
#' @param bootstrap_options border etc. Defaults to c("hover", "bordered", "condensed", "responsive")
#' @param lightable_options Default is "striped"
#' @param html_font Default is null. Set (e.g. "Optima") to override the style's default font.
#' @return None
#' @md
#' @export
#' @family Reporting functions
tmx_show <- function(x, what = c("values", "free", "labels", "nonzero_or_free"), show = c("free", "fixed", "all"), matrices = c("S", "A", "M"), digits = 2, report = c("html", "markdown"), na.print = "", zero.print = ".", html_font = NULL, style = c("paper","material_dark", "classic", "classic_2", "minimal", "material"), bootstrap_options=c("hover", "bordered", "condensed", "responsive"), lightable_options = "striped")  UseMethod("tmx_show", x)

# Define generic RMSEA...
#' Show matrices of models in a easy-to-learn-from format. 
#'
#'
#' @param x an object e.g. [umxRAM()] [umxMatrix()] from which to show parameters.
#' @param what legal options are "values" (default), "free", or "labels").
#' @param show filter on what to show c("all", "free", "fixed").
#' @param matrices to show  (default is c("S", "A")). "thresholds" in beta.
#' @param digits precision to report. Default = round to 2 decimal places.
#' @param na.print How to display NAs (default = "")
#' @param zero.print How to display 0 values (default = ".")
#' @param report How to report the results. "html" = open in browser.
#' @param style The style for the table (Defaults to "paper". Other options are "material_dark", "classic", "classic_2", "minimal", "material")
#' @param bootstrap_options border etc. Defaults to c("hover", "bordered", "condensed", "responsive")
#' @param lightable_options Default is "striped"
#' @param html_font Default is null. Set (e.g. "Optima") to override the style's default font.
#' @return None
#' @md
#' @export
#' @family Reporting functions
tmx_show.MxMatrix <- function(x, what = c("values", "free", "labels", "nonzero_or_free"), show = c("free", "fixed", "all"), matrices = c("S", "A", "M"), digits = 2, report = c("html", "markdown"), na.print = "", zero.print = ".", html_font = NULL, style = c("paper","material_dark", "classic", "classic_2", "minimal", "material"), bootstrap_options=c("hover", "bordered", "condensed", "responsive"), lightable_options = "striped"){
	what   = match.arg(what)
	show   = match.arg(show)
	report = match.arg(report)
	style  = match.arg(style)
	oldTableFormat = umx_set_table_format(report) # side effect
	theMatrix = x # just to be clear what object we are processing
	if(report == "html"){
		file = paste0(what, theMatrix$name, ".html")
		# generate the free + value + popover label using kableExtra
		values = umx_round(theMatrix$values, digits)
		free   = theMatrix$free
		labels = theMatrix$labels
		class  = class(theMatrix)[[1]]
		values[!free & values ==0] = zero.print
		
		tb = kbl(values, caption = paste0(theMatrix$name, " matrix (", class, ")"), format = report)
		# , paths fixed@0 left blank
		tb = footnote(kable_input= tb, general = paste0("Fixed cells in gray, free in black, mouse-over to see labels, paths fixed@0 are shown as ", omxQuotes(zero.print)))
		tb = xmu_style_kable(tb, style = style, html_font = html_font, bootstrap_options= bootstrap_options, lightable_options = lightable_options, full_width = FALSE)

		matCols = dim(values)[[2]]
		tabCols = kableExtra::magic_mirror(tb)$ncol
		offset  = (tabCols - matCols)
		for (thisCol in (1 + offset):tabCols){
			tb = column_spec(tb, thisCol,
				# #666666 red= #D7261E green= #26D71E
				color = ifelse(theMatrix$free[, (thisCol-offset)], "black", "#AAAAAA"),
				tooltip = labels[, (thisCol-offset)]
			)
		}
		print(tb)
	} else {
		if(what == "values"){
			tmp = data.frame(theMatrix$values)
			message("\n", "Values of ", omxQuotes(theMatrix), " matrix (0 shown as .):", appendLF = FALSE)
		}else if(what == "free"){
			tmp = data.frame(theMatrix$free)
			message("\n", "Free cells in ", theMatrix$name, " matrix (FALSE shown as .):", appendLF = FALSE)
		}else if(what == "labels"){
			tmp = theMatrix$labels
			if(show == "free"){
				tmp[theMatrix$free != TRUE] = ""
			} else if (show == "fixed") {
				tmp[theMatrix$free == TRUE] = ""
			}
			message("\n", show, " labels for ", theMatrix$name, " matrix:", appendLF = FALSE)
		}else if(what == "nonzero_or_free"){
			message("99 means parameter is fixed at a non-zero value")
			values = theMatrix$values
			Free   = theMatrix$free
			values[!Free & values !=0] = 99
			tmp = data.frame(values)
			message("\n", what, " for ", theMatrix$name, " matrix (0 shown as '.', 99=fixed non-zero value):", appendLF = FALSE)
		}
		umx_print(tmp, zero.print = zero.print, na.print = na.print, digits = digits, file= NA, report = report)
	} # for each matrix
	umx_set_table_format(oldTableFormat) # side effect
}

#' Show matrices of RAM models in a easy-to-learn-from format. 
#'
#' A great way to learn about models is to look at the matrix contents. `tmx_show` is designed to
#' do this in a way that makes it easy to process for users: The matrix contents are formatted as 
#' tables, and can even be displayed as tables in a web browser.
#' 
#' The user can select which matrices to view, whether to show values, free, and/or labels, and the precision of rounding.
#'
#' @param x an object e.g. [umxRAM()] [umxMatrix()] from which to show parameters.
#' @param what legal options are "values" (default), "free", or "labels").
#' @param show filter on what to show c("all", "free", "fixed").
#' @param matrices to show  (default is c("S", "A")). "thresholds" in beta.
#' @param digits precision to report. Default = round to 2 decimal places.
#' @param na.print How to display NAs (default = "")
#' @param zero.print How to display 0 values (default = ".")
#' @param report How to report the results. "html" = open in browser.
#' @param style The style for the table (Defaults to "paper". Other options are "material_dark", "classic", "classic_2", "minimal", "material")
#' @param bootstrap_options border etc. Defaults to c("hover", "bordered", "condensed", "responsive")
#' @param lightable_options Default is "striped"
#' @param html_font Default is null. Set (e.g. "Optima") to override the style's default font.
#' @return None
#' @export
#' @family Teaching and Testing functions
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("tmx_sh", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' \dontrun{
#' # =============================================
#' # = Show smart table on the web (the default) =
#' # =============================================
#' tmx_show(m1, report = "html")
#' tmx_show(m1, what = "free", matrices = "thresholds")
#' tmx_show(m1, zero.print = "-")
#' }
#'
#' tmx_show(m1, report = "markdown")
#' tmx_show(m1, digits = 3, report = "markdown")
#' tmx_show(m1, matrices = "S", report = "markdown")
#' tmx_show(m1, what = "free"  , report = "markdown")
#' tmx_show(m1, what = "labels", report = "markdown")
#' tmx_show(m1, what = "free", matrices = "A", report= "markdown")
#'
tmx_show.MxModel <- function(x, what = c("values", "free", "labels", "nonzero_or_free"), show = c("free", "fixed", "all"), matrices = c("S", "A", "M"), digits = 2, report = c("html", "markdown"), na.print = "", zero.print = ".", html_font = NULL, style = c("paper","material_dark", "classic", "classic_2", "minimal", "material"), bootstrap_options=c("hover", "bordered", "condensed", "responsive"), lightable_options = "striped") {
	if(!umx_is_RAM(x)){
		stop("Sorry, currently, tmx_show only knows how to display umxRAM models: You gave me a ", class(model)[[1]])
	}
	model = x
	what   = match.arg(what)
	show   = match.arg(show)
	report = match.arg(report)
	style  = match.arg(style)
	
	# filter out non-empty matrices
	requestedMatrices = matrices
	matrices = c()
	for (w in requestedMatrices) {
		if(!is.null(model$matrices[[w]])){
			matrices = c(matrices, w)
		}
	}
	
	oldTableFormat = umx_set_table_format(report) # side effect
	if("thresholds" %in% matrices){
		# TODO tmx_show: Threshold printing not yet finalised
		if(!is.null(model$deviations_for_thresh)){
			dev = TRUE
			x = model$deviations_for_thresh
		} else {
			dev = FALSE
			x = model$threshMat
		}
		if(what == "values"){
			if(dev){
				v = model$lowerOnes_for_thresh$values %*% x$values
			} else {
				v = x$values
			}
			if(show == "free"){
				v[x$free == FALSE] = NA
			} else if (show == "fixed") {
				v[x$free == TRUE] = NA
			}
			umx_print(v, zero.print = zero.print, na.print = na.print, digits = digits)
		}else if(what == "free"){
			umx_print(data.frame(x$free) , zero.print = zero.print, na.print = na.print, digits = digits)
		}else if(what == "labels"){
			l = x$labels
			if(show == "free"){
				l[x$free == FALSE] = ""
			} else if (show=="fixed") {
				l[x$free == TRUE] = ""
			}
			umx_print(l, zero.print = ".", na.print = na.print, digits = digits)
		}
	} else {
		for (w in matrices) {
			if(report == "html"){
				file = paste0(what, w, ".html")
				# generate the free + value + popover label using kableExtra
				values = umx_round(model$matrices[[w]]$values, digits)
				free   = model$matrices[[w]]$free
				labels = model$matrices[[w]]$labels
				class  = class(model$matrices[[w]])[[1]]
				values[!free & values ==0] = zero.print
				
				tb = kbl(values, caption = paste0(w, " matrix (", class, ")"), format = report)
				# , paths fixed@0 left blank
				tb = footnote(kable_input= tb, general = paste0("Fixed cells in gray, free in black, mouse-over to see labels, paths fixed@0 are shown as ", omxQuotes(zero.print)))
				tb = xmu_style_kable(tb, style = style, html_font = html_font, bootstrap_options= bootstrap_options, lightable_options = lightable_options, full_width = FALSE)

				matCols = dim(values)[[2]]
				tabCols = kableExtra::magic_mirror(tb)$ncol
				offset  = (tabCols-matCols)
				for (thisCol in (1+offset):tabCols) {
					tb = column_spec(tb, thisCol, 
						# #666666 red= #D7261E green= #26D71E
						color = ifelse(model$matrices[[w]]$free[, (thisCol-offset)], "black", "#AAAAAA"),
						tooltip = labels[, (thisCol-offset)]
					)
				}
				print(tb)
			} else {
				if(what == "values"){
					tmp = data.frame(model$matrices[[w]]$values)
					message("\n", "Values of ", omxQuotes(w), " matrix (0 shown as .):", appendLF = FALSE)
				}else if(what == "free"){
					tmp = data.frame(model$matrices[[w]]$free)
					message("\n", "Free cells in ", w, " matrix (FALSE shown as .):", appendLF = FALSE)
				}else if(what == "labels"){
					tmp = model$matrices[[w]]$labels
					if(show == "free"){
						tmp[model$matrices[[w]]$free != TRUE] = ""
					} else if (show == "fixed") {
						tmp[model$matrices[[w]]$free == TRUE] = ""
					}
					message("\n", show, " labels for ", w, " matrix:", appendLF = FALSE)
				}else if(what == "nonzero_or_free"){
					message("99 means parameter is fixed at a non-zero value")
					values = model$matrices[[w]]$values
					Free   = model$matrices[[w]]$free
					values[!Free & values !=0] = 99
					tmp = data.frame(values)
					message("\n", what, " for ", w, " matrix (0 shown as '.', 99=fixed non-zero value):", appendLF = FALSE)
				}
				umx_print(tmp, zero.print = zero.print, na.print = na.print, digits = digits, file= NA, report = report)
			}
		} # for each matrix
	}
	umx_set_table_format(oldTableFormat) # side effect	
}