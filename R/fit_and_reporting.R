#   Copyright 2007-2022 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' Quickly plot y ~ x with a regression line and R^2, and nice labels.
#'
#' @description
#' Want a figure for your paper or presentation but not the work of combining [ggplot2::ggplot()], 
#' [ggplot2::geom_smooth()] and `method` options, plus [ggplot2::geom_point()]. Organizing [ggplot2::labs()] and its
#' `x`, `y`, and `title` components. Adding your preferred theme like [ggplot2::theme_gray()], plus recalling
#' for [cowplot::draw_label()], and/or [ggplot2::annotate()] to draw math-enabled labels on the plot, as well
#' as the required [bquote()], and extracting the relevant fit statistics from [lm()] and the subsidiary tasks
#' of [reformulate()] programmatic variables?
#' 
#' `umxPlot` just takes `y ~ x` (or "x" and "y" as strings), and gives you a nicely labelled plot, with a fitted line, the
#' \ifelse{html}{\out{<em>R</em><sup>2</sup>}}{\eqn{R^2}} so readers can see how well this fitted. It knows how to put Greek symbols like \eqn{beta} into axes.
#' With only `x`, discrete-like data (factors, strings, or 12 or fewer unique numbers)
#' get a bar plot and continuous data a density plot; override with `type`. A bare
#' vector or factor `data` is upgraded to a one-column data.frame named "x".
#' @param x formula or (alternatively) x as string
#' @param y variable as string.
#' @param data The data for the graph. A bare vector or factor is upgraded to a one-column data.frame named "x".
#' @param xlab X-axis label (default y).
#' @param ylab Y-axis label (default y).
#' @param title Graph title. Default =  paste0(y, " as a function of ", x)
#' @param r2x x location for the fit summary (default 1).
#' @param r2y y location for the fit summary (default 2).
#' @param geom_point  show points? (TRUE) 
#' @param method Method for fitting curve (default = lm)
#' @param family for glm default = "gaussian"
#' @param type Single-variable plot kind: "auto" (bar if discrete-like, density if continuous), or force "bar", "histogram", or "density".
#' @return - plot you can edit.
#' @export
#' @family Plotting functions
#' @seealso - [ggplot2::ggplot()]
#' @examples
#' data(mtcars)
#' umxPlot(mpg ~ wt, data = mtcars, r2x = 2, r2y = 10)
#' umxPlot(x = "wt", y = "mpg", mtcars, r2x = 2, r2y = 10)
#' umxPlot(data = runif(100))
#' umxPlot(x = "cyl", data = mtcars)
#' umxPlot(x = "mpg", data = mtcars, type = "histogram")
umxPlot <- function(x, y= NULL, data, xlab= NULL, ylab = NULL, title = NULL, r2x=NA, r2y=NA, geom_point = TRUE, method = c("lm", "auto", "loess", "glm", "gam"), family = c("gaussian","binomial", "Gamma", "inverse", "poisson", "quasi", "quasibinomial", "quasipoisson"), type = c("auto", "bar", "histogram", "density")) {
	# Upgrade a bare vector to a one-column data.frame named "x" (must precede substitute(): both it and inherits() fail on missing x)
	if(!missing(data) && !is.data.frame(data) && ((is.atomic(data) && is.null(dim(data))) || is.factor(data))){
		data = data.frame(x = data)
		if(missing(x)){
			x = "x"
		}
	}
	# Polite catch for bare variable names: umxPlot(Weight, MPG.city, data=Cars93) -> suggest string or formula
	# Must capture substitute() BEFORE any evaluation of x/y (e.g. inherits() would trigger 'object not found')
	xSub = substitute(x)
	ySub = substitute(y)
	xIsBare = is.symbol(xSub)
	yIsBare = is.symbol(ySub)
	if(xIsBare || yIsBare){
		xNm = if(xIsBare) paste(deparse(xSub), collapse="") else ""
		yNm = if(yIsBare) paste(deparse(ySub), collapse="") else ""
		# y may be missing (histogram) - handle that case
		if(xIsBare && yIsBare){
			stop(paste0("Polite note: Either use the name as a string \"", xNm, "\", or make x a formula, e.g., ", yNm, " ~ ", xNm), call. = FALSE)
		} else if(xIsBare){
			# need to check if x bare but y was a string already evaluated? In that case yIsBare FALSE but y is still a string
			# Try to get y value if available
			if(!is.null(y) && is.character(y)){
				yNm2 = as.character(y)[1]
				stop(paste0("Polite note: Either use the name as a string \"", xNm, "\", or make x a formula, e.g., ", yNm2, " ~ ", xNm), call. = FALSE)
			}
			stop(paste0("Polite note: Either use the name as a string \"", xNm, "\", or make x a formula, e.g., y ~ ", xNm), call. = FALSE)
		} else if(yIsBare){
			# x is already a string/formula, y is bare
			xNm2 = if(is.character(x)) as.character(x)[1] else paste(deparse(xSub), collapse="")
			stop(paste0("Polite note: Either use the name as a string \"", yNm, "\", or make x a formula, e.g., ", yNm, " ~ ", xNm2), call. = FALSE)
		}
	}
	method = match.arg(method)
	family = match.arg(family)
	type = match.arg(type)
	if(inherits(x, "formula")){
		.formula = x
		tmp = attr(terms(x), "factors")
		tmp = dimnames(tmp)[[1]]
		y = tmp[1]
		x = tmp[2]
		umx_check_names(c(x, y), data = data, die = TRUE)
	} else if(is.null(y)){
		# single variable: bar plot if discrete-like, density plot if continuous
		col = data[[x]]
		is_disc = is.factor(col) || is.character(col) || is.ordered(col) || is.logical(col)
		is_disc = is_disc || (is.numeric(col) && length(unique(na.omit(col))) <= 12)
		if(type == "auto"){
			plotType = ifelse(is_disc, "bar", "density")
		} else {
			plotType = type
		}
		if(plotType == "bar"){
			return(ggplot(data, aes(x =.data[[x]])) + ggplot2::geom_bar() + labs(x = x, title = title))
		} else if(plotType == "histogram"){
			return(ggplot(data, aes(x =.data[[x]])) + ggplot2::geom_histogram(bins = 30) + labs(x = x, title = title))
		} else {
			return(ggplot(data, aes(x =.data[[x]])) + ggplot2::geom_density() + labs(x = x, title = title))
		}
	} else {
		# make formula from x and y strings
		umx_check_names(c(x, y), data = data, die = TRUE)
		.formula = reformulate(paste0(y, "~ ", x))	
	}
	# defaults for labels (defer evaluation so bare names are caught above)
	if(is.null(xlab)){
		xlab = x
	}
	if(is.null(ylab)){
		ylab = y
	}
	if(is.null(title)){
		title = paste0(y, " as a function of ", x)
	}
	p = ggplot(data = data, aes(x =.data[[x]], y =.data[[y]])) + geom_smooth(method = method)
	if(geom_point){
		p = p + geom_point()
	}
	# data[,x] - smarter R2 placement: inset from edges and left-aligned so label is fully visible
	if(is.na(r2x) || is.na(r2y)){
		xVals = data[,x]
		yVals = data[,y]
		xRng = range(xVals, na.rm = TRUE)
		yRng = range(yVals, na.rm = TRUE)
		xDiff = diff(xRng)
		yDiff = diff(yRng)
		if(is.na(r2x)){
			if(is.finite(xDiff) && xDiff > 0){
				r2x = xRng[1] + 0.10 * xDiff
			} else {
				r2x = xRng[1]
			}
		}
		if(is.na(r2y)){
			if(is.finite(yDiff) && yDiff > 0){
				r2y = yRng[2] - 0.10 * yDiff
			} else {
				r2y = yRng[2]
			}
		}
	}

	if(method == "lm"){
		m1  = lm(.formula, data = data)
		r2  = round(summary(m1)$r.squared, 3)
		lab = bquote(R^2 == .(r2))
		p = p + labs(x= xlab, y= ylab, title= title)
		p = p + cowplot::draw_label(lab, x = r2x, y = r2y, hjust = 0, vjust = 1, fontfamily = "Times", size = 12)
	}else if (method == "glm"){
		m1  = glm(.formula, data = data, family=family)
	   r2 = (m1$null.deviance - m1$deviance)/m1$null.deviance
	   lab = bquote(D^2 ==.(round(r2,3)))
		p = p + labs(x= xlab, y= ylab, title= title)
		p = p + cowplot::draw_label(lab, x = r2x, y = r2y, hjust = 0, vjust = 1, fontfamily = "Times", size = 12)
	}else{
		message("polite note: Currently, I only know how to do method = lm or glm")
	}
	p = p + theme_minimal() # gray, bw, linedraw, light, dark, minimal, classic
	p
	# p + annotate("text", 3, 30, label = expression(R^2 == beta + 1 ~ hello), family="Optima")
}


#' `umxPlotPredict` Take a model and plot the y against predicted(y)
#' @description
#' `umxPlotPredict` is a function which
#' @param model lm or other model that understands predict()
#' @param xlab X-axis label (default x).
#' @param ylab Y-axis label (default y).
#' @param r2x x location for the fit summary (default NULL guesses upper-left from the data and prints the guess).
#' @param r2y y location for the fit summary (default NULL guesses upper-left from the data and prints the guess).
#' @param font_size Default 13
#' @param font Default "Times"
#' @param rsq R^2 or r (defaults to FALSE = r) 
#' @return - plot you can edit.
#' @export
#' @family Plotting functions
#' @seealso - [ggplot2::ggplot()]
#' @examples
#' data(mtcars)
#' tmp = lm(mpg ~ wt, data = mtcars)
#' umxPlotPredict(tmp) # R^2 location guessed (upper-left) and printed
#' umxPlotPredict(tmp, r2x = 25, r2y = 15)
umxPlotPredict <- function(model, xlab= "Predicted Y", ylab= "Observed Y", r2x= NULL, r2y= NULL, font_size = 13, rsq = FALSE, font= "Times") {
	if(rsq){
		# lab = paste0("R\u00B2 = ", round(summary(model)$adj.r.squared, 2))
		lab = paste0("italic(R) ^ 2 == ", round(summary(model)$adj.r.squared, 2))
	} else {
		lab = paste0("italic(r) == ", round(summary(model)$adj.r.squared^.5, 2))
	}
	y_var = model.frame(model)[, 1]	
	if(is.null(r2x) || is.null(r2y)){
		if(is.null(r2x)){ r2x = min(predict(model), na.rm = TRUE) + 0.05 * (max(predict(model), na.rm = TRUE) - min(predict(model), na.rm = TRUE)) }
		if(is.null(r2y)){ r2y = max(y_var, na.rm = TRUE) - 0.05 * (max(y_var, na.rm = TRUE) - min(y_var, na.rm = TRUE)) }
		cat("r2x= ", r2x, " r2y= ", r2y, "\n", sep = "")
	}
	p = ggplot() + geom_point(aes(x = predict(model), y = y_var), size = 0.8, color = "gray50")
	p = p + geom_smooth(aes(x = predict(model), y = y_var), method = "lm", se = TRUE, color = "blue")
	p = p + labs(x= xlab, y= ylab)
	p = p + cowplot::draw_label(parse(text = lab), x = r2x, y = r2y, fontfamily = font, size = font_size)
	p + theme_minimal(base_size = font_size, base_family= font)	
}


# =====================
# = Model Diagnostics =
# =====================

#' Diagnose problems in a model - not working!
#'
#' The goal of this function **WILL BE** (not currently functional) to diagnose problems in
#' a model and return suggestions to the user.
#' It is a work in progress, and of no use as yet.
#'
#' Best diagnostics are:
#' 
#' 1. Observed data variances and means
#' 2. Expected variances and means
#' 3. Difference of these?
#' 	
#' Try
#' 	* diagonalizeExpCov diagonal
#' 	* [umx_is_ordered()]
#'
#' Tricky, but reporting variances and standardized thresholds is ideal.
#' Guidance is to start with unit variances and thresholds within
#'  +/- 2 SD of the mean. Like %p option in Classic Mx.
#' @param model an [OpenMx::mxModel()] to diagnose
#' @param tryHard whether I should try and fix it? (defaults to FALSE)
#' @param diagonalizeExpCov Whether to diagonalize the ExpCov
#' @return - helpful messages and perhaps a modified model
#' @export
#' @family Teaching and Testing functions
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' 
#' m1 = umxRAM("OneFactor", data = demoOneFactor, type= "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m1 = mxRun(m1)
#' umxSummary(m1, std = TRUE)
#' umxDiagnose(m1)
#' }
umxDiagnose <- function(model, tryHard = FALSE, diagonalizeExpCov = FALSE){
	# 1. First thing to check is whether the covariance matrix is positive definite.
	minEigen = min(eigen(umxExpCov(model))$values)
	if(minEigen<0){
		message("The expected covariance matrix is not positive definite")
		# now what?
	}
}

# =============================
# = Fit and Reporting Helpers =
# =============================

#' Akaike weights for a set of models
#'
#' @description
#' `aic_weights` converts a vector of AIC values into Akaike weights: the conditional probability that each model is the best in the set, given the data and the candidate models (Wagenmakers and Farrell, 2004).
#'
#' @details
#' Weights are computed from AIC differences relative to the best model in the set:
#'
#' \deqn{w_i = \frac{\exp(-\Delta_i/2)}{\sum_j \exp(-\Delta_j/2)}, \quad \Delta_i = \mathrm{AIC}_i - \min(\mathrm{AIC})}
#'
#' The best model has \eqn{\Delta = 0} and therefore the largest weight. Weights sum to 1. A model 2 AIC units worse than the best has a likelihood ratio of \eqn{\exp(-1) \approx 0.37} relative to it, before normalisation across the set. The result is a statement about this candidate set only, not about the absolute probability that a model is true.
#'
#' @param x Numeric vector of AIC values, one per model. `NA` values are not dropped.
#' @return Numeric vector of weights, the same length as `x`, summing to 1.
#' @export
#' @family Reporting Functions
#' @seealso [umxWeightedAIC()], [AIC()]
#' @references Wagenmakers, E. J., and Farrell, S. (2004). AIC model selection using Akaike weights. *Psychonomic Bulletin & Review*, **11**, 192-196. \doi{10.3758/BF03206482}
#' @md
#' @examples
#' aic_weights(c(100, 102, 110))
#' # best model, 2 AIC worse, 10 AIC worse
aic_weights <- function(x) {
  x = as.numeric(x)
  d = exp(-(x - min(x)) / 2)
  d / sum(d)
}

#' AIC weight-based conditional probabilities.
#' @description
#' Returns the best model by AIC, and computes the probabilities 
#' according to AIC weight-based conditional probabilities (Wagenmakers & Farrell, 2004).
#' @param models a list of models to compare.
#' @param digits (default 2)
#' @return - Best model
#' @export
#' @family Miscellaneous Stats Functions
#' @seealso - [AIC()]
#' @references Wagenmakers E.J., Farrell S. (2004). AIC model selection using Akaike weights. *Psychonomic Bulletin and Review*. **11**, 192-196. \doi{10.3758/bf03206482}.
#' @examples
#' l1 = lm(mpg ~ wt + disp, data = mtcars)
#' l2 = lm(mpg ~ wt, data = mtcars)
#' umxWeightedAIC(models = list(l1, l2))
umxWeightedAIC <- function(models, digits = 2) {
	if(inherits(models[[1]], "numeric")){
		stop("Please input the list of models to compare as a list, i.e. models = list(model1, model2)")
	}
	AIClist = c()
	for (i in models) {
		AIClist = c(AIClist, AIC(i))
	}
	whichBest = which.min(AIClist)
	bestModel = models[[whichBest]]
	aic.weights = round(aic_weights(AIClist), 2)
	
	if(isS4(models[[1]]) & is(models[[1]], "MxModel")){
		message("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model for ", 
			omxQuotes(namez(models)), " respectively are: ",
			omxQuotes(aic.weights), " Using aic_weights().")		
	}else{
		if("call" %in% names(bestModel)){
			# ID = paste0("Model ", omxQuotes(bestModel$call))
			ID = paste0("Model ", whichBest)
		} else {
			ID = paste0("Model ", whichBest)
		}
		message(ID, " is the best fitting model according to AIC.")
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model are (for each model you gave me): ",
			omxQuotes(aic.weights), " Using aic_weights(AIC()).")		
		
	}
	invisible(bestModel)
}

#' Reduce models, and report the results.
#'
#' @description
#' Given a `umx` model (currently `umxACE` and `umxGxE` are supported - ask for more!)
#' `umxReduce` will conduct a formalised reduction process. It will also report
#' Akaike weights are also reported showing relative support across models.
#'
#' Specialized functions are called for different type of input:
#' 1. **GxE model reduction** For [umxGxE()] models [umxReduceGxE()] is called.
#' 2. **ACE model reduction** For [umxACE()] models,[umxReduceACE()] is called. More than one phenotype stops before any nested model is fit.
#' 
#' `umxReduce` reports the results in a table. Set the format of the table with
#' [umx_set_table_format()], or set `report= "html"` to open a
#' table for pasting into a word processor.
#' 
#' `umxReduce` can be extended to new cases as demand emerges.
#'
#' @param model The [OpenMx::mxModel()] which will be reduced.
#' @param report How to report the results. "html" = open in browser
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param testD Whether to test ADE and DE models (TRUE) 
#' @param baseFileName (optional) custom filename for html output (defaults to "tmp")
#' @param tryHard Default = "yes"
#' @param silent Default = FALSE
#' @param ... Other parameters to control model summary
#' @family Model Summary and Comparison
#' @family Twin Modeling Functions
#' @seealso [umxReduceGxE()], [umxReduceACE()]
#' @references Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights.
#'  *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}
#' @export

umxReduce <- function(model, report = c("markdown", "inline", "html"), intervals = TRUE, testD = TRUE, baseFileName = "tmp", tryHard = "yes", silent=FALSE, ...){
	UseMethod("umxReduce", model)
}

#' @export
umxReduce.default <- function(model, report = c("markdown", "inline", "html"), intervals = FALSE, testD = TRUE, baseFileName = "tmp", tryHard = "yes", silent=FALSE, ...){
	stop("umxReduce is not defined for objects of class:", class(model))
}

#' Reduce a GxE model.
#'
#' @description
#' This function can perform model reduction for [umxGxE()] models, 
#' testing dropping a`,c` & e`, as well as c & c`, a & a` etc.
#'
#' It reports the results in a table. Set the format of the table with
#' [umx_set_table_format()]. Or set `report = "html"` to open a
#' table for pasting into a word processor.
#' 
#' In addition to printing a table, the function returns the preferred model.
#' 
#' @param model A [umxGxE()] to reduce.
#' @param report How to report the results. default = "markdown". "html" = open in browser.
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param testD Whether to test ADE and DE models (TRUE)
#' @param baseFileName (optional) custom filename for html output (default = "tmp").
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search", "auto" (picks to suit the model)
#' @param silent Default (FALSE)
#' @param ... Other parameters to control model summary.
#' @return best model
#' @export
#' @family Twin Modeling Functions
#' @seealso [umxReduce()], [umxReduceACE()]
#' @references Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights.
#' *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}.
#' @examples
#' \dontrun{
#' model = umxReduce(model)
#' }
umxReduceGxE <- function(model, report = c("markdown", "inline", "html", "report"), intervals = TRUE, testD = TRUE, baseFileName = "tmp_gxe", tryHard = c("yes", "no", "ordinal", "search", "auto"), silent = FALSE, ...) {
	report  = match.arg(report)
	tryHard = match.arg(tryHard)
	umx_is_MxModel(model)
	if(inherits(model, "MxModelGxE")){
		# Reduce GxE Model

		noAmod = umxModify(model, update = "am_r1c1", name = "No_mod_on_A", tryHard= tryHard)
		noCmod = umxModify(model, update = "cm_r1c1", name = "No_mod_on_C", tryHard= tryHard)
		noEmod = umxModify(model, update = "em_r1c1", name = "No_mod_on_E", tryHard= tryHard)

		noACEmod     = umxModify(model, regex  = "[ace]m_r1c1" , name = "No_moderation", tryHard= tryHard)

		no_a_no_am  = umxModify(noAmod     , update = "a_r1c1" , name = "No_A_no_mod_on_A"  , tryHard= tryHard)
		no_c_no_cm  = umxModify(noCmod     , update = "c_r1c1" , name = "No_C_no_mod_on_C"  , tryHard= tryHard)
		no_c_no_cem = umxModify(no_c_no_cm , update = "em_r1c1", name = "No_c_no_ce_mod"    , tryHard= tryHard)
		no_c_no_mod = umxModify(no_c_no_cem, update = "am_r1c1", name = "No_c_no_moderation", tryHard= tryHard)

		comparisons = c(
			noAmod, noCmod, noEmod, noACEmod,
			no_a_no_am, no_c_no_cm, no_c_no_cem,
			no_c_no_mod
		)

		# ====================
		# = everything table =
		# ====================
		
		umxCompare(model, comparisons, all = TRUE, report = report, file = paste0(baseFileName, "1.html"))
		# umxCompare(no_c_no_cem, no_c_no_moderation, all = TRUE, report = report, file = paste0(baseFileName, "2.html"))
		modelList = c(model, comparisons)
		
		# get list of AICs
		AIClist = c()
		for (i in modelList) {
			AIClist = c(AIClist, AIC(i))
		}
		whichBest = which.min(AIClist)
		bestModel = modelList[[whichBest]]
		message("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
		aic.weights = round(aic_weights(AIClist), 2)
		message("AIC weight-based conditional probabilities {Wagenmakers, 2004, 192-196} of being the best model for ", 
			omxQuotes(namez(modelList)), " respectively are: ",
			omxQuotes(aic.weights), " Using aic_weights(AIC())."
		)
		if(intervals){
			bestModel = mxRun(bestModel, intervals = intervals)
		}
		invisible(bestModel)
	} else {
		stop("This function is for GxE. Feel free to let me know what you want...")
	}
}
#' @export
umxReduce.MxModelGxE <- umxReduceGxE

#' Reduce an ACE model.
#'
#' This function can perform model reduction on [umxACE()] models,
#' testing dropping A and C, as well as an ADE or ACE model, displaying the results
#' in a table, and returning the best model.
#'
#' It stops when the model has more than one phenotype. No nested model is fit. You can offer up either the univariate ACE or ADE base model.
#'
#' Suggestions for more sophisticated automation welcomed!
#'
#' @param model an ACE or ADE [OpenMx::mxModel()] to reduce
#' @param report How to report the results. "html" = open in browser
#' @param intervals Recompute CIs (if any included) on the best model (default = TRUE)
#' @param testD Whether to test ADE and DE models (TRUE)
#' @param baseFileName (optional) custom filename for html output (defaults to "tmp")
#' @param tryHard (default = "yes"). "auto" picks to suit the model.
#' @param silent Don't print the ACE models (default = FALSE)
#' @param digits rounding in printout (default = 2)
#' @param ... Other parameters to control model summary
#' @return Best fitting model
#' @export
#' @family Twin Modeling Functions
#' @seealso [umxReduceGxE()], [umxReduce()]
#' @references Wagenmakers, E.J., & Farrell, S. (2004). AIC model selection using Akaike weights. *Psychonomic Bulletin and Review*, **11**, 192-196. \doi{10.3758/BF03206482}
#' @examples
#' \dontrun{
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#'
#' # ===========================================================================
#' # = Table of parameters + fit comparisons, ready too copy to word processor =
#' # ===========================================================================
#' umxReduce(m1, silent=TRUE, digits=2, repo="h")
#'
#' # ==========================================
#' # = Function captures the preferred model =
#' # ==========================================
#' m2 = umxReduce(m1)
#' umxSummary(m2)
#' 
#' # works for ADE input also
#' m1 = umxACE(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "", dzCr = .25)
#' 
#' }
umxReduceACE <- function(model, report = c("markdown", "inline", "html", "report"), intervals = TRUE, testD = TRUE, baseFileName = "tmp", tryHard = c("yes", "no", "ordinal", "search", "auto"), silent=FALSE, digits = 2, ...) {
	report  = match.arg(report)
	tryHard = match.arg(tryHard)
	nVar = nrow(model$top$a$values)
	if(nVar != 1){
		stop("umxReduceACE is for univariate models. This model has ", nVar, " variables. No nested models were fit.")
	}
	# override umx_set_auto_run?
	oldAutoRun = umx_set_auto_run(autoRun = TRUE)
	if(silent){
		oldSilent = umx_set_silent(TRUE)
	}else{
		oldSilent = FALSE
	}
	oldAutoPlot = umx_set_auto_plot(FALSE, silent = TRUE)


	if(model$top$dzCr$values == 1){
		ACE = model
		ADE = umxModify(model, 'dzCr_r1c1', value = .25, name = "ADE", tryHard = tryHard)
		if(-2*logLik(ACE) > -2*logLik(ADE) && testD){
			CE = umxModify(ADE, regex = "a_r[0-9]+c[0-9]+" , name = "DE", tryHard = tryHard)
			AE = umxModify(ADE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
			 E = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
			message("A dominance model is preferred, set dzCr = 0.25")
		}else{
			CE = umxModify(ACE, regex = "a_r[0-9]+c[0-9]+" , name = "CE", tryHard = tryHard)
			AE = umxModify(ACE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
			 E = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
		}
	}else if(model$top$dzCr$values == .25){
		if(model$name=="ACE"){
			message("You gave me an ADE model, but it was called 'ACE'. I have renamed it ADE for the purposes of clarity in model reduction.")
			model = mxRename(model, newname = "ADE", oldname = "ACE")
		} else {
			message("You gave me an ADE model.")
		}
		ADE = model
		ACE = umxModify(ADE, 'dzCr_r1c1', value = 1, name = "ACE", tryHard = tryHard)
		AE  = umxModify(ADE, regex = "c_r[0-9]+c[0-9]+" , name = "AE", tryHard = tryHard)
		 E  = umxModify( AE, regex = "a_r[0-9]+c[0-9]+" , name =  "E", tryHard = tryHard)
		if(-2*logLik(ADE) > -2*logLik(ACE)){
			CE = umxModify(ACE, regex = "a_r[0-9]+c[0-9]+" , name = "CE", tryHard=tryHard)
			message("An ACE model is preferred, set dzCr = 1.0")
		}else{
			CE = umxModify(ADE, regex = "a_r[0-9]+c[0-9]+" , name = "DE", tryHard=tryHard)
		}
	}else{
		stop(model$top$dzCr$values, " is an odd number for dzCr, isn't it? I was expecting 1 (C) or .25 (D)",
		"\nPerhaps you're a John Loehlin (requiescat in pace :-( ) fan, and are doing an assortative mating test? e-mail me to get this added here.")
		# TODO umxReduceACE handle odd values of dzCr as assortative mating etc.?
		bestModel = model
	}
	# = Show fit table =
	tmp = data.frame(matrix(nrow=5,ncol=4))
	names(tmp) = c("a"              , "c"                 , "e"                 , "d")
	tmp[1,] = c(ACE$top$a_std$result, ACE$top$c_std$result, ACE$top$e_std$result, NA)
	tmp[2,] = c(ADE$top$a_std$result, NA                  , ADE$top$e_std$result, ADE$top$c_std$result)
	tmp[3,] = c( NA                 ,  CE$top$c_std$result,  CE$top$e_std$result, NA)
	tmp[4,] = c( AE$top$a_std$result, NA                  ,  AE$top$e_std$result, NA)
	tmp[5,] = c( NA                 , NA                  ,   E$top$e_std$result, NA)

	biggles = umxCompare(ACE, c(ADE, CE, AE, E), all = TRUE, report = report, silent=TRUE)
	tmp2 = cbind(biggles[, 1, drop = FALSE], tmp, biggles[, 2:dim(biggles)[2] ] )
	umx_print(tmp2, digits = digits, report = report)
	whichBest = which.min(AIC(ACE, ADE, CE, AE)[,"AIC"])[1]
	bestModel = list(ACE, ADE, CE, AE)[[whichBest]]
	message("Among ACE, ADE, CE, and AE models ", omxQuotes(bestModel$name), " fit best according to AIC.\n")
	if(testD){
		aic.weights = round(aic_weights(AIC(ACE, ADE, CE, AE)[,"AIC"]), 2)
		aic.names   = namez(c(ACE, ADE, CE, AE))
	} else {
		aic.weights = round(aic_weights(AIC(ACE, CE, AE)[,"AIC"]), 2)
		aic.names   = namez(c(ACE, CE, AE))
	}
	message("Conditional AIC probability {Wagenmakers, 2004, 192-196} using aic_weights(AIC()) indicated relative model support for ", 
		omxQuotes(aic.names), " respectively as: ", omxQuotes(aic.weights))
		message(paste0(aic.names," (", aic.weights, "%)"))
	if(intervals){
		bestModel = mxRun(bestModel, intervals = intervals)
	}
	umx_set_auto_plot(oldAutoPlot, silent = TRUE)
	umx_set_silent(oldSilent)
	umx_set_auto_run(autoRun = oldAutoRun)
	
	invisible(bestModel)
}
#' @export
umxReduce.MxModelACE <- umxReduceACE

#' Get residuals from an MxModel
#'
#' Return the [residuals()] from an OpenMx RAM model. You can format these (with digits), and suppress small values.
#'
#' @rdname residuals.MxModel
#' @param object An fitted [OpenMx::mxModel()] from which to get residuals
#' @param digits round to how many digits (default = 2)
#' @param suppress smallest deviation to print out (default = NULL = show all)
#' @param reorder optionally reorder the variables in the residuals matrix to show patterns
#' @param ... Optional parameters
#' @return - matrix of residuals
#' @export
#' @family Reporting functions
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#'
#'# ===================================
#'# = Show the residuals of the model =
#'# ===================================
#' residuals(m1)
#' # |   |x1   |x2    |x3   |x4    |x5 |
#' # |:--|:----|:-----|:----|:-----|:--|
#' # |x1 |.    |.     |0.01 |.     |.  |
#' # |x2 |.    |.     |0.01 |-0.01 |.  |
#' # |x3 |0.01 |0.01  |.    |.     |.  |
#' # |x4 |.    |-0.01 |.    |.     |.  |
#' # |x5 |.    |.     |.    |.     |.  |
#' # [1] "nb: You can zoom in on bad values with, e.g. suppress = .01, which
#' #      will hide values smaller than this. Use digits = to round"
#'
#' residuals(m1, digits = 3)
#' residuals(m1, digits = 3, suppress = .005)
#' # residuals are returned as an invisible object you can capture in a variable
#' a = residuals(m1); a
#' }
#' 
residuals.MxModel <- function(object, digits = 2, suppress = NULL, reorder=NULL, ...){
	umx_check_model(object, type = NULL, hasData = TRUE)
	expCov = umxExpCov(object, latents = FALSE)
	if(object$data$type == "raw"){
		obsCov = umxHetCor(object$data$observed)
	} else {
		obsCov = object$data$observed
	}
	resid = cov2cor(obsCov) - cov2cor(expCov)
	if(!is.null(reorder)){
		resid = umx_reorder(resid, newOrder = reorder, force = TRUE)
	}
	umx_print(data.frame(resid), digits = digits, zero.print = ".", suppress = suppress)
	if(is.null(suppress)){
		print("nb: Zoom in on bad values with, e.g. suppress = .1, which will hide smaller values. Use digits = to round")
	}
	invisible(resid)
}

# define generic loadings...
#' loadings
#' Generic loadings function to extract factor loadings from exploratory or confirmatory
#' factor analyses.
#'
#' See \code{\link[umx]{loadings.MxModel}} to access the loadings of OpenMx EFA models.
#' 
#' Base \code{\link[stats]{loadings}} handles [factanal()] objects. 
#'
#' @param x an object from which to get loadings 
#' @param ... additional parameters
#' @return - matrix of loadings
#' @export
#' @family Reporting functions
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>

loadings <- function(x, ...) UseMethod("loadings")
#' @export
loadings.default <- function(x, ...) stats::loadings(x, ...) 

# TODO: alternative approach would be to use setGeneric("loadings")

#' Extract factor loadings from an EFA (factor analysis).
#'
#' loadings extracts the factor loadings from an EFA (factor analysis) model.
#' It behaves equivalently to stats::loadings, returning the loadings from an 
#' EFA (factor analysis). However it does not store the rotation matrix.
#'
#' @param x A RAM model from which to get loadings.
#' @param ... Other parameters (currently unused)
#' @return - loadings matrix
#' @export
#' @family Reporting functions
#' @seealso - [factanal()], [loadings()]
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' myVars = c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(name = "test", factors = 2, data = mtcars[, myVars])
#' loadings(m1)
#' }
#'
loadings.MxModel <- function(x, ...) {
	x$A$values[x@manifestVars, x@latentVars, drop = FALSE]
}


#' Get confidence intervals from a umx model
#'
#' Implements confidence interval function for umx models.
#' 
#' Note: By default, requesting new CIs wipes the existing ones.
#' To keep these, set wipeExistingRequests = FALSE.
#' 
#' Because CIs can take time to run, by default only already-computed CIs will be reported. To run new CIs, set run = TRUE. `run = TRUE` does not print the table: the intervals are on the returned model for [umxSummary()] and [plot()]. Pass `report = TRUE` to print the table as well. `run = FALSE` prints an existing table. Pass `report = FALSE` to return the model without printing.
#'
#' @details *Note*: OpenMx defines a `confint` function which will return SE-based CIs.
#' 
#' If `parm` is empty, and `run = FALSE`, a message will alert you to set `run = TRUE`. 
#'
#' @param object An [OpenMx::mxModel()], possibly already containing [OpenMx::mxCI()]s that have been [OpenMx::mxRun()] with intervals = TRUE))
#' @param parm Which intervals to request. `"existing"` keeps the requests already on the model. `"all"` requests every free parameter label. `"smart"` requests one interval per free cell of the standardized algebras for CP, ACE, and IP (raw `a_cp`, `c_cp`, and `e_cp` for CP; the raw path matrices when no `*_std` algebra is present). For a RAM model, `"smart"` requests every free parameter label. One or more names request those labels.
#' @param level The confidence level required (default = .95)
#' @param run Whether to run the model (defaults to FALSE)
#' @param report Whether to print the computed interval table. Defaults to TRUE when `run` is FALSE, and FALSE when `run` is TRUE.
#' @param wipeExistingRequests Whether to remove existing CIs when adding new ones (ignored if parm = 'existing').
#' @param optimizer For difficult CIs, trying other optimizers can help!
#' @param showErrorCodes (default = FALSE)
#' @export
#' @return - [OpenMx::mxModel()]
#' @family Reporting functions
#' @seealso - [stats::confint()], [OpenMx::mxSE()], [umxCI()], [OpenMx::mxCI()]
#' @references <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' 
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
#' 	umxPath(from = "G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' 
#' m1 = umxConfint(m1, run = TRUE) # There are no existing CI requests...
#' 
#' # Add a CI request for "G_to_x1" and compute it. The table is not printed.
#' m2 = umxConfint(m1, parm = "G_to_x1", run = TRUE)
#' umxSummary(m2)
#' plot(m2)
#' 
#' # Just print out any existing CIs
#' umxConfint(m2)
#' 
#' # Compute and print the table in the same call
#' umxConfint(m2, run = TRUE, report = TRUE)
#' 
#' # CI requests added for free matrix parameters. User prompted to set run = TRUE
#' m3 = umxConfint(m1, "all")
#' 
#' # Run the requested CIs
#' m3 = umxConfint(m3, run = TRUE) 
#' 
#' # Run CIs for free one-headed (asymmetric) paths in RAM model. 
#' #   note: Deletes other existing requests,
#' tmp = umxConfint(m1, parm = "A", run = TRUE)
#' 
#' # Wipe existing CIs, add G_to_x1
#' tmp = umxConfint(m1, parm = "G_to_x1", run = TRUE, wipeExistingRequests = TRUE) 
#' 
#' # smart: free standardized cells for CP, ACE, and IP; every free label for RAM
#' m2 = umxConfint(m1, "smart")
#' }
#'
umxConfint <- function(object, parm = c("existing", "all", "or one or more labels", "smart"), wipeExistingRequests = TRUE, level = 0.95, run = FALSE, report = !run, showErrorCodes = FALSE, optimizer= c("SLSQP", "NPSOL", "CSOLNP", "current")) {
	optimizer = match.arg(optimizer)
	if(optimizer == "current"){
		# optimizer is sent to omxRunCI so set "current" to the actual name
		optimizer = umx_set_optimizer(silent = TRUE)
	}
	parm = xmu_match.arg(parm, c("existing", "all", "or one or more labels", "smart"), check = FALSE)

	# Upgrade "all" to "smart" for CP
	if(class(object)[[1]] == "MxModelCP" && parm == "all"){
		parm = "smart"
	}

	# 1. remove existing CIs if requested to
	if(wipeExistingRequests && (all(parm != "existing"))){
		if(length(object$intervals)){
			object = mxModel(object, remove = TRUE, object$intervals)
			message("Removed existing CIs")
		}
	}
	
	# 2. Add CIs if requested
	if (length(parm) >1){
		# Add requested CIs to model
		# TODO umxConfint: Check that these are valid and not duplicates
		object = mxModel(object, mxCI(parm, interval = level))
	} else if (parm == "all") {
		CIs_to_set = names(omxGetParameters(object, free = TRUE))
		object = mxModel(object, mxCI(CIs_to_set, interval = level))
	} else if (parm == "smart"){
		modelClass = class(object)[[1]]
		stdTargets = NULL
		if(modelClass == "MxModelCP"){
			stdTargets = c(a_cp = "top.a_cp", c_cp = "top.c_cp", e_cp = "top.e_cp", cp_loadings = "top.cp_loadings_std", as = "top.as_std", cs = "top.cs_std", es = "top.es_std")
		} else if(modelClass == "MxModelACE"){
			if(is.null(object$top$a_std)){
				stdTargets = c(a = "top.a", c = "top.c", e = "top.e")
			} else {
				stdTargets = c(a = "top.a_std", c = "top.c_std", e = "top.e_std")
			}
		} else if(modelClass == "MxModelIP"){
			if(is.null(object$top$ai_std)){
				stdTargets = c(ai = "top.ai", ci = "top.ci", ei = "top.ei", as = "top.as", cs = "top.cs", es = "top.es")
			} else {
				stdTargets = c(ai = "top.ai_std", ci = "top.ci_std", ei = "top.ei_std", as = "top.as_std", cs = "top.cs_std", es = "top.es_std")
			}
		}
		if(!is.null(stdTargets)){
			# One request per free cell. The row and column come from the label, so indexes past 9 stay intact.
			CIs2Add = character(0)
			for(matName in names(stdTargets)){
				freeMat = object$top[[matName]]$free
				template = umxMatrix("A", "Full", nrow(freeMat), ncol(freeMat))$labels
				cells = gsub("^.*_r([0-9]+)c([0-9]+)$", paste0(stdTargets[[matName]], "[\\1,\\2]"), template)
				CIs2Add = c(CIs2Add, cells[which(freeMat)])
			}
		} else if(umx_is_RAM(object)){
			CIs2Add = names(omxGetParameters(object, free = TRUE))
		} else {
			stop("umxConfint(parm = 'smart') does not know class ", omxQuotes(modelClass), ". Use parm = 'all' or name the labels.")
		}
		if(length(CIs2Add) == 0){
			message("No free cells to add CIs for.")
		} else {
			umx_msg(CIs2Add)
			object = mxModel(object, mxCI(CIs2Add, interval = level))
			message("added ", length(CIs2Add), " CIs")
		}
	} else if (parm == "existing"){
		# nothing to do
	} else {
		# User requesting 1 new CI
		# TODO umxConfint: Check that these are valid and not duplicates
		object = mxModel(object, mxCI(parm, interval = level))
	}

	# 3. Run CIs if requested
	if(run) {
		# Check there are some in existence
		if(!umx_has_CIs(object, "intervals")) {
			message("Polite note: This model has no CIs yet. Perhaps you wanted to use parm = 'all' to request CIs on all free parameters? Or list some path labels?")
		}else{
			origObject = object
			
			# object = mxRun(object, intervals = TRUE)
			object = omxRunCI(object, optimizer = optimizer)
			# omxRunCI replaces output with the interval table and drops these three. They live on this model.
			if (is.null(object@output$vcov) && !is.null(origObject@output$vcov)) {
				object@output$vcov = origObject@output$vcov
			}
			if (is.null(object@output$standardErrors) && !is.null(origObject@output$standardErrors)) {
				object@output$standardErrors = origObject@output$standardErrors
			}
			if (is.null(object@output$hessian) && !is.null(origObject@output$hessian)) {
				object@output$hessian = origObject@output$hessian
			}
		}
	}
	# 4. Report CIs
	if(!umx_has_CIs(object, "both")) {
		if(run == FALSE){
			message("Polite note: Some CIs have been requested but not run. Add ", omxQuotes("run = TRUE"), " to your umxConfint() call to run them.\n",
			"To store the model capture it from umxConfint like this:\n",
			"m1 = umxConfint(m1, run = TRUE)")
		} else if(length(object$intervals)==0){
			message("No CIs requested...")
		} else{
			message("Polite note: hmmm... You wanted it run, but I don't see any computed CIs despite there being ", length(object$intervals), " requested...",
			"\nThat's a bug. Please report it to timothy.c.bates@gmail.com")
		}
	} else if(isTRUE(report)){
		# model has CIs and they have been run, and the caller asked to see the table
		# 1. Summarize model
		model_summary = summary(object, verbose = TRUE)
		# 2. Extract CIs and details, and arrange for merging
		CIdetail = model_summary$CIdetail
		CIdetail = CIdetail[, c("parameter", "value", "side", "diagnostic", "statusCode")]
		CIdetail$diagnostic = as.character(CIdetail$diagnostic)
		CIdetail$statusCode = as.character(CIdetail$statusCode)
		CIdetail$diagnostic = namez(CIdetail$diagnostic, pattern = "alpha level not reached"         , replacement = "alpha hi")
		CIdetail$statusCode = namez(CIdetail$statusCode, pattern = "infeasible non-linear constraint", replacement = "constrained")
		CIdetail$statusCode = namez(CIdetail$statusCode, pattern = "iteration limit/blue"            , replacement = "blue")

		CIs = model_summary$CI
		CIs$parameter = row.names(CIs)
		row.names(CIs) = NULL
		CIs = CIs[, c("parameter", "estimate", "lbound", "ubound", "note")]
		intersect(names(CIdetail), names(CIs))
		tmp = merge(CIs, CIdetail[CIdetail$side == "lower", ], by = "parameter", all.x = TRUE)
		tmp = merge(tmp, CIdetail[CIdetail$side == "upper", ], by = "parameter", all.x = TRUE, suffixes = c(".lower",".upper"))
		tmp$side.lower = NULL
		tmp$side.upper = NULL

		# 3. Format CIs
		model_CIs   = round(CIs[,c("lbound", "estimate", "ubound")], 3)
		model_CI_OK = object$output$confidenceIntervalCodes
		colnames(model_CI_OK) = c("lbound Code", "ubound Code")
		model_CIs =	cbind(round(model_CIs, 3), model_CI_OK)
		print(model_CIs)
		npsolMessages = list(
		'1' = 'The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN).',
		'2' = 'The linear constraints and bounds could not be satisfied. The problem has no feasible solution.',
		'3' = 'The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.',
		'4' = 'The major iteration limit was reached (Mx status BLUE).',
		'5' = 'not used',
		'6' = 'The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)',
		'7' = 'The function derivatives returned by funcon or funobj appear to be incorrect.',
		'8' = 'not used',
		'9' = 'An input parameter was invalid')
		if(!all(is.null(model_CI_OK)) && (any(model_CI_OK !=0) |any(is.na(model_CI_OK))) && showErrorCodes){
			codeList = c(model_CI_OK[,"lbound Code"], model_CI_OK[,"ubound Code"])
			relevantCodes = unique(codeList); relevantCodes = relevantCodes[relevantCodes !=0]
			for(i in relevantCodes) {
			   print(paste0(i, ": ", npsolMessages[i][[1]]))
			}
		}
	}
	invisible(object)
}

# 1776(liberty), 1789(liberty+terror), 1815 (liberty+inequality)

#' Deprecated. Use [umxConfint()].
#'
#' `umxCI` is deprecated. Use [umxConfint()] for profile intervals, including `parm = "smart"` on ACE, IP, CP, and RAM.
#'
#' This function still adds [OpenMx::mxCI()] requests for free parameter labels (the default), a name list, or a regular expression. It can remove requests. `run = "yes"` refits with [OpenMx::mxRun()]. [umxConfint()] computes intervals with [OpenMx::omxRunCI()] and keeps the covariance matrix, standard errors, and Hessian.
#'
#' @details 
#' `umxCI` also reports if any problems were encountered. The codes are standard OpenMx errors and warnings
#' \itemize{
#' \item 1: The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN)
#' \item 2: The linear constraints and bounds could not be satisfied. The problem has no feasible solution.
#' \item 3: The nonlinear constraints and bounds could not be satisfied. The problem may have no feasible solution.
#' \item 4: The major iteration limit was reached (Mx status BLUE).
#' \item 6: The model does not satisfy the first-order optimality conditions to the required accuracy, and no improved point for the merit function could be found during the final linesearch (Mx status RED)
#' \item 7: The function derivatives returned by funcon or funobj appear to be incorrect.
#' \item 9: An input parameter was invalid.
#' }
#' If `run = "no"`, the function simply adds the CI requests, but returns the model without running them.
#' 
#' @param model The [OpenMx::mxModel()] you wish to report [OpenMx::mxCI()]s on
#' @param which What CIs to add: c("ALL", NA, "list of your making")
#' @param remove = FALSE (if set, removes existing specified CIs from the model)
#' @param run Whether or not to compute the CIs. Valid values = "no" (default), "yes", "if necessary".
#' 'show' means print the intervals if computed, or list their names if not.
#' @param interval The interval for newly added CIs (defaults to 0.95)
#' @param type The type of CI (defaults to "both", options are "lower" and  "upper")
#' @param regex Add CIs for labels matching this regular expression (over-rides which)
#' @param showErrorCodes Whether to show errors (default == TRUE)
#' @return - [OpenMx::mxModel()]
#' @family Reporting functions
#' @seealso - [stats::confint()], [umxConfint()], [umxCI()], [umxModify()]
#' @references <https://github.com/tbates/umx>
#' @export
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m1$intervals # none yet - empty list()
#' m1 = umxCI(m1)
#' m1$intervals # $G_to_x1...
#' m1 = umxCI(m1, remove = TRUE) # remove CIs from the model and return it
#' m1$intervals # none again
#'
#' # Add CIs by name
#' parameters(m1, patt="_with_")
#' m1 = umxCI(m1, which = "x1_with_x1")
#' m1 = umxCI(m1, which = c("x1_with_x1", "x2_with_x2"))
#' m1 = umxCI(m1, regex = "x1_with_", run= "yes")
#' #           lbound estimate ubound lbound Code ubound Code
#' # x1_with_x1  0.036    0.041  0.047           0           0
#' 
#' # ========================
#' # = A twin model example =
#' # ========================
#' data(twinData) 
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = c("bmi1","bmi2"), dzData = dzData, mzData = mzData)
#' umxCI(m1, run = "show") # show what will be requested
#' umxCI(m1, run = "yes") # actually compute the CIs
#' # Don't force update of CIs, but if they were just added, then calculate them
#' umxCI(m1, run = "if necessary")
#' m1 = umxCI(m1, remove = TRUE) # remove them all
#' m1$intervals # none!
#' # Show what parameters are available to get CIs on
#' umxParameters(m1) 
#' # Request a CI by label:
#' m1 = umxCI(m1, which = "a_r1c1", run = "yes")
#' }
umxCI <- function(model = NULL, which = c("ALL", NA, "list of your making"), remove = FALSE, run = c("no", "yes", "if necessary", "show"), interval = 0.95, type = c("both", "lower", "upper"), regex = NULL, showErrorCodes = TRUE) {
	.Deprecated("umxConfint")
	# Note: OpenMx now overloads confint, returning SE-based intervals.
	run = match.arg(run)
	which = xmu_match.arg(which, c("ALL", NA, "list of your making"), check = FALSE)
	if(remove){
		if(!is.null(regex)){
			CIs = namez(model$intervals, pattern = regex)
		} else if(any(which == "ALL")){
			CIs = names(model$intervals)
		} else {
			CIs = which 
		}
		if(length(names(model$intervals)) > 0){
			model = mxModel(model, mxCI(CIs), remove = TRUE)
		} else {
			message("model has no intervals to remove")
		}
		invisible(model)
	} else {
		# Adding CIs
		# TODO Avoid duplicating existing CIs
		# TODO Add each CI individually
		# TODO Break them out into separate models and reassemble if on cluster?
		if(any(is.na(which)) && is.null(regex)){
			# nothing to add
		} else {
			if(!is.null(regex)){
				CIs = umxGetParameters(model, regex = regex, free = TRUE)
			} else if(any(which == "ALL")){
				CIs = names(omxGetParameters(model, free = TRUE))
			} else {
				CIs = which 
			}
			model = mxModel(model, mxCI(CIs, interval = interval, type = type))
		}
	}
	if(run == "yes" | (!umx_has_CIs(model) & run == "if necessary")) {
		model = mxRun(model, intervals = TRUE)
	} else {
		message("Not running CIs, run == ", run)
	}

	if(run == "show") {
		print("CI requests in the model:")
		print(names(model$intervals))
	}
	if(umx_has_CIs(model)){
		umxConfint(model, showErrorCodes = showErrorCodes)
		message("Table: Computed CIs in model ", model$name)
	}
	invisible(model)
}

#' Shows a compact, publication-style, summary of umx models
#'
#' @description
#' Report the fit of a RAM or other umx model (such as ACE, CP etc.)
#' in a compact form suitable for reporting in a journal.
#'
#' See documentation for RAM models summary here: [umxSummary.MxModel()].
#' 
#' View documentation on the ACE model subclass here: [umxSummaryACE()].
#' 
#' View documentation on the ACEv model subclass here: [umxSummaryACEv()].
#' 
#' View documentation on the ACE_DE model subclass here: [umxSummaryACE_DE()].
#' 
#' View documentation on the IP model subclass here: [umxSummaryIP()].
#' 
#' View documentation on the CP model subclass here: [umxSummaryCP()].
#' 
#' View documentation on the GxE model subclass here: [umxSummaryGxE()].
#'
#' @param model The [OpenMx::mxModel()] whose fit will be reported
#' @param ... Other parameters to control model summary
#' @family Model Summary and Comparison

#' @export
umxSummary <- function(model, ...){
	UseMethod("umxSummary", model)
}

#' @export
umxSummary.default <- function(model, ...){
	stop("umxSummary is not defined for objects of class:", class(model))
}



#' umxSummary.MxLISRELModel
#'
#' umxSummary.MxLISRELModel handles summary formatting for LISREL models.
#'
#' @param model A LISREL [OpenMx::mxModel()] to summarize.
#' @param refModels Reference models for comparison.
#' @param std Whether to show standardized path values.
#' @param digits Number of decimal places to round to.
#' @param report Formatting style: markdown or html.
#' @param means Whether to print means.
#' @param residuals Whether to print residual covariances.
#' @param SE Whether to show parameter Standard Errors.
#' @param filter Filter criteria for parameters.
#' @param RMSEA_CI Print RMSEA Confidence Interval.
#' @param ... Extra arguments.
#' @param matrixAddresses Print matrix addresses.
#' @return - model
#' @export
#' @family Model Summary
#' @seealso [umxLISREL()], [plot.MxLISRELModel()]
#' @references <https://github.com/tbates/umx>

umxSummary.MxLISRELModel <- function(model, refModels = NULL, std = FALSE, digits = 2, report = c("markdown", "html"), means = TRUE, residuals = TRUE, SE = TRUE, filter = c("ALL", "NS", "SIG"), RMSEA_CI = FALSE, ..., matrixAddresses = FALSE) {
	report = match.arg(report)
	filter = match.arg(filter)
	
	umx_has_been_run(model, stop = TRUE)
	
	if (is.null(refModels)) {
		modelSummary = summary(model)
	} else if (refModels == FALSE) {
		modelSummary = summary(model)
	} else {
		modelSummary = summary(model, refModels = refModels)
	}
	
	print(modelSummary)
	invisible(model)
}


#' Shows a compact, publication-style, summary of a umx Cholesky ACE model
#'
#' Summarize a fitted Cholesky model returned by [umxACE()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelACE
#' @param model an [OpenMx::mxModel()] to summarize.
#' @param digits round to how many digits (default = 2).
#' Defaults to NA = do not create plot output.
#' @param comparison you can run mxCompare on a comparison model (NULL).
#' @param std Whether to standardize the output (default = TRUE).
#' @param showRg = whether to show the genetic correlations (FALSE).
#' @param CIs Whether to show Confidence intervals if they exist (TRUE).
#' @param returnStd Whether to return the standardized form of the model (default = FALSE).
#' @param report If "html", then open an html table of the results.
#' @param file The name of the dot file for figure: "name" = use the name of the model.
#' @param extended how much to report (FALSE).
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @return - optional [OpenMx::mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACE()], [plot.MxModelACE()], [umxModify()]
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' umxSummaryACE(m1, file = NA);
#' umxSummaryACE(m1, file = "name", std = TRUE)
#' stdFit = umxSummaryACE(m1, returnStd = TRUE);
#' }
#' 
umxSummaryACE <- function(model, digits = 2, comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, trim= TRUE, twinOneOnly= TRUE)
		nVar   = length(selDVs)
		# TODO umxSummaryACE these already exist if a_std exists..
		# TODO replace all this with xmu_standardizeACE
		# Calculate standardized variance components
		a = mxEval(top.a, model) # Path coefficients
		c = mxEval(top.c, model)
		e = mxEval(top.e, model)
		A = mxEval(top.A, model) # Variances
		C = mxEval(top.C, model)
		E = mxEval(top.E, model)
		
		if(std){
			caption = paste0("Standardized parameter estimates from a ", dim(a)[2], "-factor Cholesky ACE model. ")
			Vtot = A + C + E;            # Total variance
			I    = diag(nVar);           # nVar Identity matrix
			SD   = solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations
			# (same as "(\sqrt(I.Vtot))~"

			# Standardized _path_ coefficients ready to be stacked together
			a_std  = SD %*% a; # Standardized path coefficients
			c_std  = SD %*% c;
			e_std  = SD %*% e;
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			caption = paste0("Raw parameter estimates from a ", dim(a)[2], "-factor Cholesky ACE model. ")
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs, stringsAsFactors = FALSE);

		if(model$top$dzCr$values == .25){
			colNames = c("a", "d", "e")
			caption = paste0(caption, "A: additive genetic; D: dominance effects; E: unique environment.")
		} else {
			colNames = c("a", "c", "e")
			caption = paste0(caption, "A: additive genetic; C: common environment; E: unique environment.")
		}
		names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		umx_print(Estimates, digits = digits, caption = caption, report = report, zero.print = zero.print)
		xmu_twin_print_means(model = model, report = report)

		if(extended == TRUE) {
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)] = NA
			cClean[upper.tri(cClean)] = NA
			eClean[upper.tri(eClean)] = NA
			unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = selDVs);
			names(unStandardizedEstimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			umx_print(unStandardizedEstimates, caption = "Unstandardized Cholesky ACE model path coefficients", digits = digits, zero.print = zero.print)
		}

		if(showRg) {
			# Pre & post multiply covariance matrix by inverse of standard deviations
			NAmatrix = matrix(NA, nVar, nVar);
			rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # E correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)] = NA
			rCClean[upper.tri(rCClean)] = NA
			rEClean[upper.tri(rEClean)] = NA
			genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = selDVs);
			names(genetic_correlations) = selDVs
		 	# Make a nice table.
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			umx_print(genetic_correlations, caption = "Genetic correlations", digits = digits, zero.print = zero.print)
		}
		hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO umxACE CI code: Need to refactor into some function calls...
			# TODO and then add to umxSummaryIP and CP
			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
			# Discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			# THIS IS NOT NEEDED: confidenceIntervals come with estimate in the middle now...
			# These can be names ("top.a_std[1,1]") or labels ("a_r1c1")
			# imxEvalByName finds them both
			# outList = c();
			# for(aName in row.names(CIlist)) {
			# 	outList = append(outList, imxEvalByName(aName, model))
			# }
			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			# CIlist = CIlist[, c("lbound", "estimate", "ubound")]
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the CI results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_CI = c_CI = e_CI = matrix(NA, rows, cols)

			# Iterate over each CI
			labelList = imxGenerateLabels(model)	
			rowCount  = dim(CIlist)[1]
			# return(CIlist)
			for(n in 1:rowCount) { # n = 1
				thisName = row.names(CIlist)[n] # thisName = "a11"
					# Convert labels to [bracket] style
					if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString = paste0(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]")

				if(grepl("^a", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
				}
			}
			# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
			# TODO umxSummaryACE: Add option to use mxSE
			Estimates = data.frame(cbind(a_CI, c_CI, e_CI), row.names = selDVs, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			umx_print(Estimates, digits = digits, zero.print = zero.print,  report=report, file = "tmpCI.html")
			xmu_twin_print_means(model, digits = digits, report = report)
		} # end Use CIs
	} # end list catcher?

	if(!is.na(file) && typeof(model) != "list") {
		umxPlotACE(model, file = file, std = std, digits = digits)
	}
	if(returnStd) {
		xmu_standardize_ACE(model)
	}else{
		invisible(Estimates)
	}
}

#' @export
umxSummary.MxModelACE <- umxSummaryACE

#' Summary for a GLM ACE twin model (Cholesky)
#'
#' Path loadings a, c, e on the eta scale, intercepts, -2LL / AIC. No CFI.
#' Extra-Gaussian `e` paths stay in the model (residual unique covariance).
#' For binomial items the e Cholesky diagonal is fixed at 1 (scale). Bernoulli
#' leftover is **not** that path (`e11` = leftover would double-count). Leftover
#' is added only in the E used for shares: `E = ee' + D`, with D = 1 (probit)
#' or \eqn{\pi^2/3} (logit).
#' The printed e matrix is the Cholesky of that `E`. Poisson has `D = 0`.
#' Standardized values divide each row by that phenotype's eta-scale sd
#' (sqrt of diag(A+C+E) with leftover in E). For two traits, prints `rA`,
#' `rC`, and `rE = cov2cor(E_total)` so unique correlation is diluted by
#' leftover on the diagonal (genetic `rA` is not).
#'
#' @param model a model from [umxACE_GLM()]
#' @param digits rounding
#' @param comparison optional comparison model for [umxCompare()]
#' @param std if TRUE, report eta-scale variance shares
#' @param report "markdown" or "html"
#' @param ... unused
#' @return parameter table (invisibly)
#' @export
#' @exportS3Method umxSummary MxModelACE_GLM
#' @family Twin Modeling Functions
#' @seealso [umxACE_GLM()]
#' @md
umxSummaryACE_GLM <- function(model, digits = 2, comparison = NULL, std = TRUE, report = c("markdown", "html"), ...) {
	report = match.arg(report)
	umx_has_been_run(model, stop = TRUE)
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	pars = omxGetParameters(model)
	mzA = if (!is.null(model$MZ)) model$MZ$A else NULL
	aceLabs = names(pars)[grepl("^[ace]_r[0-9]+c[0-9]+$", names(pars))]
	if (!is.null(mzA) && !is.null(mzA$labels)) {
		al = as.character(mzA$labels)
		aceLabs = unique(c(aceLabs, al[grepl("^[ace]_r[0-9]+c[0-9]+$", al)]))
	}
	nVar = 1
	if (length(aceLabs) > 0) {
		nVar = max(as.integer(sub("^.*_r([0-9]+)c[0-9]+$", "\\1", aceLabs)))
	}
	fillChol = function(pref, nVar, pars, Amat = NULL) {
		M = matrix(0, nVar, nVar)
		for (i in 1:nVar) {
			for (j in i:nVar) {
				lab = paste0(pref, "_r", j, "c", i)
				if (lab %in% names(pars)) {
					M[j, i] = as.numeric(pars[lab])
				} else if (!is.null(Amat) && !is.null(Amat$labels)) {
					hit = which(Amat$labels == lab, arr.ind = TRUE)
					if (length(hit) >= 2 || (is.matrix(hit) && nrow(hit) >= 1)) {
						rr = if (is.matrix(hit)) hit[1, 1] else hit[1]
						cc = if (is.matrix(hit)) hit[1, 2] else hit[2]
						M[j, i] = as.numeric(Amat$values[rr, cc])
					}
				}
			}
		}
		M
	}
	a = fillChol("a", nVar, pars, mzA)
	c = fillChol("c", nVar, pars, mzA)
	e = fillChol("e", nVar, pars, mzA)
	mLab = names(pars)[grepl("^one_to_", names(pars))]
	mu = if (length(mLab) >= 1) as.numeric(pars[mLab]) else rep(NA_real_, nVar)
	if (length(mu) < nVar) mu = c(mu, rep(NA_real_, nVar - length(mu)))
	leftoverOne = function(fam) {
		if (is.null(fam) || !is(fam, "MxFamily")) return(0)
		if (!identical(fam@family, "binomial")) return(0)
		if (identical(fam@link, "probit")) return(1)
		pi^2 / 3
	}
	D = rep(0, nVar)
	mzMod = model$MZ
	if (!is.null(mzMod) && is(mzMod$fitfunction, "MxFitFunctionGLM")) {
		famList = mzMod$fitfunction$family
		man = mzMod@manifestVars
		if (length(man) >= nVar && is.list(famList)) {
			for (j in 1:nVar) {
				D[j] = leftoverOne(famList[[man[j]]])
			}
		}
	}
	A = a %*% t(a)
	C = c %*% t(c)
	E = e %*% t(e)
	if (any(D > 0)) {
		for (j in 1:nVar) {
			E[j, j] = E[j, j] + D[j]
		}
		eDisp = tryCatch(t(chol(E)), error = function(err) {
			e2 = e
			for (j in 1:nVar) {
				e2[j, j] = sqrt(max(e[j, j]^2 + D[j], 0))
			}
			e2
		})
	} else {
		eDisp = e
	}
	Vdiag = diag(A + C + E)
	if (isTRUE(std)) {
		aShow = a
		cShow = c
		eShow = eDisp
		for (j in 1:nVar) {
			sdj = sqrt(max(Vdiag[j], 1e-12))
			aShow[j, ] = a[j, ] / sdj
			cShow[j, ] = c[j, ] / sdj
			eShow[j, ] = eDisp[j, ] / sdj
		}
		if (any(D > 0)) {
			caption = "GLM ACE eta-scale standardized paths (row / sd). Bernoulli leftover is in E (ee'+D), not an e path."
		} else {
			caption = "GLM ACE eta-scale standardized paths (row / sd). Poisson leftover is not on eta (D = 0)."
		}
	} else {
		aShow = a
		cShow = c
		eShow = eDisp
		if (any(D > 0)) {
			caption = "GLM ACE raw Cholesky on eta. Printed e is chol(ee'+D); D is Bernoulli leftover."
		} else {
			caption = "GLM ACE raw Cholesky loadings on eta (latents variance 1). D = 0."
		}
	}
	aShow[upper.tri(aShow)] = NA
	cShow[upper.tri(cShow)] = NA
	eShow[upper.tri(eShow)] = NA
	Estimates = data.frame(cbind(aShow, cShow, eShow), stringsAsFactors = FALSE)
	colNames = if (isTRUE(std)) c("a", "c", "e") else c("a", "c", "e")
	names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar, 3))
	umx_print(Estimates, digits = digits, report = report, caption = caption, na.print = "")
	if (nVar > 1) {
		corFromCov = function(M) {
			n = nrow(M)
			R = matrix(NA_real_, n, n)
			s = sqrt(pmax(diag(as.matrix(M)), 0))
			for (i in 1:n) {
				for (j in 1:n) {
					if (s[i] > 1e-12 && s[j] > 1e-12) R[i, j] = M[i, j] / (s[i] * s[j])
				}
			}
			R[upper.tri(R)] = NA
			R
		}
		rA = corFromCov(A)
		rC = corFromCov(C)
		rE = corFromCov(E)
		rgTab = data.frame(cbind(rA, rC, rE), stringsAsFactors = FALSE)
		names(rgTab) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar, 3))
		if (length(mLab) >= nVar) {
			rownames(rgTab) = sub("^one_to_", "", mLab[seq_len(nVar)])
		}
		if (any(D > 0)) {
			capRg = "Factor correlations. rE uses E_total = ee'+D (Bernoulli leftover on the diagonal; unique correlation is diluted). rA is not."
		} else {
			capRg = "Factor correlations from A, C, E on eta (D = 0)."
		}
		umx_print(rgTab, digits = digits, report = report, caption = capRg, na.print = "")
	}
	if (length(mLab) >= 1) {
		muShow = mu[seq_len(min(nVar, length(mu)))]
		muNames = mLab[seq_len(min(nVar, length(mLab)))]
		message("Intercepts (eta): ", paste(paste0(muNames, " = ", round(muShow, digits)), collapse = "; "))
	}
	if (any(D > 0)) {
		message("Bernoulli leftover on E diagonal (not a path): ", paste(round(D, digits), collapse = ", "), ". Extra-Gaussian e paths are residual unique covariance.")
	}
	minus2LL = NA
	if (!is.null(model$output$Minus2LogLikelihood)) minus2LL = model$output$Minus2LogLikelihood
	if (!is.finite(minus2LL) && !is.null(model$output$fit)) minus2LL = model$output$fit
	nPar = length(pars)
	aic = if (is.finite(minus2LL)) minus2LL + 2 * nPar else NA
	status = if (!is.null(model$output$status$code)) model$output$status$code else NA
	fitBits = c()
	if (is.finite(minus2LL)) fitBits = c(fitBits, paste0("-2LL = ", round(minus2LL, digits)))
	fitBits = c(fitBits, paste0("k = ", nPar))
	if (is.finite(aic)) fitBits = c(fitBits, paste0("AIC = ", round(aic, digits)))
	if (length(status) == 1 && !is.na(status)) fitBits = c(fitBits, paste0("status = ", status))
	message(paste0("\nGLM ACE fit: ", paste(fitBits, collapse = "; ")))
	message("No CFI/TLI/RMSEA for GLM ACE. Nested models: umxCompare / AIC.")
	invisible(Estimates)
}

#' @export
umxSummary.MxModelACE_GLM <- umxSummaryACE_GLM

#' Plot a GLM ACE twin model
#'
#' GraphViz of the Cholesky on eta: same compact a/c/e-to-trait diagram as [umxPlotACE()],
#' not the full twin RAM (no T1/T2 duplicate latents). Printed `e` is `chol(ee'+D)`
#' when leftover is on eta (Bernoulli), matching [umxSummaryACE_GLM()].
#'
#' @param x a model from [umxACE_GLM()]
#' @param file NA returns the dot string; `"name"` writes a file named after the model
#' @param digits decimals on path labels (default 2)
#' @param means show intercepts from `one` (default FALSE)
#' @param std eta-scale row standardization (default TRUE), leftover included in E
#' @param strip_zero strip leading 0. from labels (default TRUE)
#' @param showFixed draw fixed non-zero paths (default TRUE so binomial `e` at 1 shows)
#' @param ... unused
#' @return dot string (invisibly) when `file` is NA
#' @export
#' @exportS3Method plot MxModelACE_GLM
#' @family Plotting functions
#' @seealso [umxACE_GLM()], [umxSummaryACE_GLM()], [umxPlotACE()]
#' @aliases plot.MxModelACE_GLM
#' @md
umxPlotACE_GLM <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, showFixed = TRUE, ...) {
	model = x
	if (!is(model, "MxModelACE_GLM")) {
		stop("umxPlotACE_GLM needs an MxModelACE_GLM (from umxACE_GLM()).", call. = FALSE)
	}
	pars = omxGetParameters(model)
	mzA = if (!is.null(model$MZ)) model$MZ$A else NULL
	aceLabs = names(pars)[grepl("^[ace]_r[0-9]+c[0-9]+$", names(pars))]
	if (!is.null(mzA) && !is.null(mzA$labels)) {
		al = as.character(mzA$labels)
		aceLabs = unique(c(aceLabs, al[grepl("^[ace]_r[0-9]+c[0-9]+$", al)]))
	}
	nVar = 1
	if (length(aceLabs) > 0) {
		nVar = max(as.integer(sub("^.*_r([0-9]+)c[0-9]+$", "\\1", aceLabs)))
	}
	fillChol = function(pref, nVar, pars, Amat = NULL) {
		M = matrix(0, nVar, nVar)
		for (i in 1:nVar) {
			for (j in i:nVar) {
				lab = paste0(pref, "_r", j, "c", i)
				if (lab %in% names(pars)) {
					M[j, i] = as.numeric(pars[lab])
				} else if (!is.null(Amat) && !is.null(Amat$labels)) {
					hit = which(Amat$labels == lab, arr.ind = TRUE)
					if (length(hit) >= 2 || (is.matrix(hit) && nrow(hit) >= 1)) {
						rr = if (is.matrix(hit)) hit[1, 1] else hit[1]
						cc = if (is.matrix(hit)) hit[1, 2] else hit[2]
						M[j, i] = as.numeric(Amat$values[rr, cc])
					}
				}
			}
		}
		M
	}
	a = fillChol("a", nVar, pars, mzA)
	c = fillChol("c", nVar, pars, mzA)
	e = fillChol("e", nVar, pars, mzA)
	leftoverOne = function(fam) {
		if (is.null(fam) || !is(fam, "MxFamily")) return(0)
		if (!identical(fam@family, "binomial")) return(0)
		if (identical(fam@link, "probit")) return(1)
		pi^2 / 3
	}
	D = rep(0, nVar)
	mzMod = model$MZ
	if (!is.null(mzMod) && is(mzMod$fitfunction, "MxFitFunctionGLM")) {
		famList = mzMod$fitfunction$family
		man = mzMod@manifestVars
		if (length(man) >= nVar && is.list(famList)) {
			for (j in 1:nVar) {
				D[j] = leftoverOne(famList[[man[j]]])
			}
		}
	}
	A = a %*% t(a)
	C = c %*% t(c)
	E = e %*% t(e)
	if (any(D > 0)) {
		for (j in 1:nVar) {
			E[j, j] = E[j, j] + D[j]
		}
		eDisp = tryCatch(t(chol(E)), error = function(err) {
			e2 = e
			for (j in 1:nVar) {
				e2[j, j] = sqrt(max(e[j, j]^2 + D[j], 0))
			}
			e2
		})
	} else {
		eDisp = e
	}
	Vdiag = diag(A + C + E)
	aShow = a
	cShow = c
	eShow = eDisp
	if (isTRUE(std)) {
		for (j in 1:nVar) {
			sdj = sqrt(max(Vdiag[j], 1e-12))
			aShow[j, ] = a[j, ] / sdj
			cShow[j, ] = c[j, ] / sdj
			eShow[j, ] = eDisp[j, ] / sdj
		}
	}
	obsNames = NULL
	if (!is.null(model$MZ) && !is.null(model$MZ$data) && !is.null(model$MZ$data$observed)) {
		obsNames = colnames(model$MZ$data$observed)
	}
	if (is.null(obsNames) && !is.null(model$MZ)) obsNames = model$MZ@manifestVars
	if (length(obsNames) >= nVar) {
		selDVs = sub("(_T)?[0-9]$", "", obsNames[seq_len(nVar)])
	} else {
		selDVs = paste0("var", seq_len(nVar))
	}
	freeA = matrix(FALSE, nVar, nVar)
	freeC = matrix(FALSE, nVar, nVar)
	freeE = matrix(FALSE, nVar, nVar)
	for (i in 1:nVar) {
		for (j in i:nVar) {
			freeA[j, i] = abs(aShow[j, i]) > 1e-8
			freeC[j, i] = abs(cShow[j, i]) > 1e-8
			freeE[j, i] = abs(eShow[j, i]) > 1e-8
		}
	}
	aMat = umxMatrix("a", "Full", nVar, nVar, free = freeA, values = aShow)
	cMat = umxMatrix("c", "Full", nVar, nVar, free = freeC, values = cShow)
	eMat = umxMatrix("e", "Full", nVar, nVar, free = freeE, values = eShow)
	out = list(str = "", latents = c(), manifests = c())
	out = xmu_dot_mat2dot(aMat, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, p = out)
	out = xmu_dot_mat2dot(cMat, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, p = out)
	out = xmu_dot_mat2dot(eMat, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, p = out)
	if (isTRUE(means)) {
		mu = rep(0, nVar)
		mLab = names(pars)[grepl("^one_to_", names(pars))]
		if (length(mLab) >= 1) {
			for (j in seq_len(min(nVar, length(mLab)))) {
				mu[j] = as.numeric(pars[mLab[j]])
			}
		} else if (!is.null(model$MZ$M)) {
			for (j in seq_len(min(nVar, ncol(model$MZ$M$values)))) {
				mu[j] = as.numeric(model$MZ$M$values[1, j])
			}
		}
		intMat = umxMatrix("intercept", "Full", 1, nVar, free = abs(mu) > 1e-8, values = matrix(mu, 1, nVar))
		out = xmu_dot_mat2dot(intMat, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, digits = digits, p = out)
	}
	preOut = xmu_dot_define_shapes(latents = out$latents, manifests = out$manifests)
	same = xmu_dot_rank(out$manifests, ".", rank = "same")
	top = xmu_dot_rank(out$latents, "^a", rank = "min")
	bottom = xmu_dot_rank(out$latents, "^[ce]", rank = "max")
	digraph = paste0(
		"digraph G {\n\t",
		'label="', model$name, '";\n\t',
		"splines = \"FALSE\";\n",
		preOut, out$str, same, top, bottom, "\n}"
	)
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

#' @export
#' @exportS3Method plot MxModelACE_GLM
plot.MxModelACE_GLM <- umxPlotACE_GLM





#' Present results of a twin ACE-model with covariates in table and graphical forms.
#'
#' Summarize a Cholesky model with random-effects covariates, as returned by [umxACEcov()]
#'
#' @aliases umxSummary.MxModelACEcov
#' @param model A [umxACEcov()] model to summarize
#' @param digits Round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param extended How much to report (FALSE)
#' @param showRg = Whether to show the genetic correlations (FALSE)
#' @param std = Whether to show the standardized model (TRUE)
#' @param comparison You can run mxCompare on a comparison model (NULL)
#' @param CIs Whether to show Confidence intervals if they exist (TRUE)
#' @param zero.print How to show zeros (".")
#' @param report If "html", then open an html table of the results.
#' @param ... Other parameters to control model summary
#' @return - optional [OpenMx::mxModel()]
#' @export
#' @family Summary functions
#' @seealso - [umxACEcov()] 
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACEcov(selDVs = c("bmi", "wt"), selCovs = "ht", dzData = dzData, mzData = mzData, sep="")
#' umxSummaryACEcov(m1, file = NA)
#' umxSummaryACEcov(m1, file = "name", std = TRUE)
#' stdFit = umxSummary(m1, returnStd = TRUE)
#' }
umxSummaryACEcov <- function(model, digits = 2, showRg = FALSE, std = TRUE, comparison = NULL, CIs = TRUE, zero.print = ".", report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, extended = FALSE, ...) {
	umx_msg("Beta function!! Better to use umxACE() with covariates in the means, IMHO.")
	report   = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	
	# Depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACEcov(thisFit, digits = digits, file = file, returnStd = returnStd, extended = extended, showRg = showRg, std = std, comparison = comparison, CIs = CIs, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		# includes covs
		# selDVs = xmu_twin_get_var_names(model, source = "expCovMZ", trim = TRUE, twinOneOnly = TRUE)
		
		# just the DVs, not the covs
		selDVs = dimnames(model$top$a)[[1]]
		nDV    = length(selDVs)

		# Calculate standardized variance components
		a    = mxEval(top.a, model); # Path coefficients
		c    = mxEval(top.c, model);
		e    = mxEval(top.e, model);
		A    = mxEval(top.A, model); # Variances
		C    = mxEval(top.C, model);
		E    = mxEval(top.E, model);
		Vtot = A + C + E; # Total variance
		Iden = diag(nDV) # nDV Identity matrix
		SD   = solve(sqrt(Iden * Vtot)) # Inverse of diagonal matrix of standard deviations
		# (same as "(\sqrt(Iden.Vtot))~"

		# Standardized _path_ coefficients ready to be stacked together
		a_std = SD %*% a; # Standardized path coefficients
		c_std = SD %*% c;
		e_std = SD %*% e;

		if(std){
			caption = "Standardized solution"
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			caption = "Raw solution"
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		rowNames = sub("(_T)?1$", "", selDVs)
		Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);

		names(Estimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));

		umx_print(Estimates, digits = digits, zero.print = zero.print, report=report, caption= caption)
		xmu_twin_print_means(model, digits = digits, report = report)
		
		if(extended == TRUE) {
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)] = NA
			cClean[upper.tri(cClean)] = NA
			eClean[upper.tri(eClean)] = NA
			unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);
			names(unStandardizedEstimates) = paste0(rep(c("a", "c", "e"), each = nDV), rep(1:nDV));
			umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print, caption = "Unstandardized path coefficients")
		}

		# Pre & post multiply covariance matrix by inverse of standard deviations
		if(showRg) {
			NAmatrix = matrix(NA, nDV, nDV);
			rA = tryCatch(solve(sqrt(Iden * A)) %*% A %*% solve(sqrt(Iden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(Iden * C)) %*% C %*% solve(sqrt(Iden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(Iden * E)) %*% E %*% solve(sqrt(Iden * E)), error = function(err) return(NAmatrix)); # E correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)] = NA
			rCClean[upper.tri(rCClean)] = NA
			rEClean[upper.tri(rEClean)] = NA
			genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames)
			names(genetic_correlations) <- rowNames
		 	# Make a nice-ish table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each= nDV), rep(1:nDV))
			umx_print(genetic_correlations, digits=digits, zero.print = zero.print, caption = "Genetic correlations")
		}
		stdFit = model
		hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			message("Creating CI-based report!")
			# CIs exist, get the lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0), ]
			# discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]

			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			CIlist = CIlist[, c("lbound", "estimate", "ubound")] 
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the standardized results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_std = c_std = e_std = matrix(NA, rows, cols)

			# iterate over each CI
			labelList = imxGenerateLabels(model)			
			rowCount = dim(CIlist)[1]
			# return(CIlist)
			for(n in 1:rowCount) { # n = 1
				thisName = row.names(CIlist)[n] # thisName = "a11"
				# convert labels to [bracket] style
					if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts = round(CIlist[n, c("estimate", "lbound", "ubound")], 2)
				thisString = paste(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]", sep="")
				# print(list(CIlist, labelList, rowCount, fullName, thisMatrixName))

				if(grepl("^a", thisMatrixName)) {
					a_std[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_std[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_std[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
				}
			}
			print(a_std)
			print(c_std)
			print(e_std)
		}
	} # Use CIs
	stdFit$top$a$values = a_std
	stdFit$top$c$values = c_std
	stdFit$top$e$values = e_std
	if(!is.na(file)) {
		message("making dot file")
		plot(model, file, std = std)
	}
	if(returnStd) {
		if(CIs){
			message("Returned model won't work if you asked for CIs...")
		}
		return(stdFit)
	}
}
#' @export
umxSummary.MxModelACEcov <- umxSummaryACEcov


#' Present the results of a Common-pathway twin model in table and graphical form
#'
#' Summarizes a Common-Pathway model, as returned by [umxCP()]
#'
#' @aliases umxSummary.MxModelCP
#' @param model A fitted [umxCP()] model to summarize
#' @param digits Round to how many digits (default = 2)
#' @param std Whether to show the standardized model (TRUE) (ignored: used extended = TRUE to get unstandardized)
#' @param CIs Whether to print a profile interval that has already been computed (default TRUE). No interval is computed here, and a model with no computed intervals prints the point estimate.
#' @param showRg Whether to show the genetic correlations (default FALSE)
#' @param comparison Run mxCompare on a comparison model (default NULL)
#' @param report Print tables to the console (as 'markdown'), or open in browser ('html')
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [OpenMx::mxModel()]
#' @export
#' @family Summary functions
#' @seealso - \code{\link{umxCP}()}, [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' 
# # Help optimizer by putting wt on a similar scale to ht
#' twinData$wt1 = twinData$wt1/10
#' twinData$wt2 = twinData$wt2/10
#' selDVs = c("ht", "wt")
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' 
#' m1 = umxCP(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "", optimizer = "SLSQP")
#' umxSummaryCP(m1, file = NA) # Suppress plot creation with file
#' umxSummary(m1, file = NA)   # Generic summary is the same
#' stdFit = umxSummaryCP(m1, digits = 2, std = TRUE, file = NA, returnStd = TRUE);
#' 
#' umxSummary(m1, std = FALSE, showRg = TRUE, file = NA);
#' umxSummary(m1, std = FALSE, file = NA)
#'
#' # =================
#' # = Print example =
#' # =================
#' umxSummary(m1, file = "Figure 3", std = TRUE)
#'
#' # =================
#' # = Confint example =
#' # =================
#' m1 = umxConfint(m1, "smart", run = FALSE);
#' m1 = umxConfint(m1, "smart", run = TRUE);
#' umxSummary(m1, CIs = TRUE, file = NA);
#' }
#'
umxSummaryCP <- function(model, digits = 2, std = TRUE, CIs = TRUE, showRg = FALSE, comparison = NULL, report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, ...) {
	# TODO: Detect value of DZ covariance, and if .25 set "C" to "D" in tables
	report = match.arg(report)

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message(paste("Output for Model: ", omxQuotes(thisFit$name), "\n"))
			umxSummaryCP(thisFit, digits = digits, file = file, returnStd = returnStd, showRg = showRg, comparison = comparison, std = std, CIs = CIs)
		}
	} else {
		umx_check_model(model, "MxModelCP", beenRun = TRUE, callingFn = "umxSummaryCP")
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, source = "expCovMZ", trim = TRUE, twinOneOnly = TRUE)
		nVar   = length(selDVs)
		nFac   = dim(model$top$matrices$a_cp)[[1]]	

		if(any(c(std, returnStd))) {
			model = xmu_standardize_CP(model) # Make a standardized copy of model
		}
		commaSep = paste0(umx_set_separator(silent = TRUE), " ")
		cis = NULL
		if(isTRUE(CIs) && !is.null(model$output$confidenceIntervals) && nrow(model$output$confidenceIntervals) > 0){
			cis = model$output$confidenceIntervals
		}

		# 1. Create Tables of Common Factor Paths
		a_cp = model$top$a_cp$values # nFac * nFac matrix of path coefficients flowing into cp_loadings
		c_cp = model$top$c_cp$values
		e_cp = model$top$e_cp$values

		# Common factor ACE inputs are std to 1
		# Bind diag of a_cp, c and e columns into nFac-row matrix
		commonACE = cbind(diag(a_cp), diag(c_cp), diag(e_cp)) 
		commonACE = data.frame(commonACE, row.names = paste("Common.factor", 1:nFac, sep = "."), stringsAsFactors = FALSE);
		names(commonACE) = c ("A", "C", "E")
		# Common-factor ACE paths stay on the raw a_cp, c_cp, and e_cp algebras. Smart intervals use those addresses.
		if(!is.null(cis)){
			cpMats = c("a_cp", "c_cp", "e_cp")
			for(i in 1:nFac){
				for(j in 1:3){
					address = paste0("top.", cpMats[j], "[", i, ",", i, "]")
					if(address %in% rownames(cis)){
						lb = as.numeric(cis[address, "lbound"])[1]
						ub = as.numeric(cis[address, "ubound"])[1]
						est = as.numeric(cis[address, "estimate"])[1]
						if(is.finite(lb) && is.finite(ub) && is.finite(est)){
							if(is.numeric(commonACE[[j]])){
								commonACE[[j]] = format(round(commonACE[[j]], digits), nsmall = digits, trim = TRUE, scientific = FALSE)
							}
							commonACE[i, j] = paste0(round(est, digits), " [", round(lb, digits), commaSep, round(ub, digits), "]")
						}
					}
				}
			}
		}
		umx_print(commonACE, digits = digits, zero.print = ".", report = report, caption = "Common Factor Paths")
		
		if(class(model$top$matrices$a_cp)[1] == "LowerMatrix"){
			message("You used correlated genetic inputs to the common factor. This is the a_cp matrix")
			print(a_cp)
		}
		
		# 2. Create Table of standardized loadings on Common factors
		cp_loadings = model$top$cp_loadings$values # nVar * nFac matrix
		cp_loadings = data.frame(cp_loadings, row.names = selDVs, stringsAsFactors = FALSE);
		names(cp_loadings) = paste0("CP", 1:length(names(cp_loadings)))
		# Standardized loadings are requested as top.cp_loadings_std. Raw loadings use top.cp_loadings.
		if(!is.null(cis)){
			loadSuffix = if(isTRUE(std) && !is.null(model$top$cp_loadings_std)) "_std" else ""
			for(r in 1:nVar){
				for(c in 1:nFac){
					address = paste0("top.cp_loadings", loadSuffix, "[", r, ",", c, "]")
					if(address %in% rownames(cis)){
						lb = as.numeric(cis[address, "lbound"])[1]
						ub = as.numeric(cis[address, "ubound"])[1]
						est = as.numeric(cis[address, "estimate"])[1]
						if(is.finite(lb) && is.finite(ub) && is.finite(est)){
							if(is.numeric(cp_loadings[[c]])){
								cp_loadings[[c]] = format(round(cp_loadings[[c]], digits), nsmall = digits, trim = TRUE, scientific = FALSE)
							}
							cp_loadings[r, c] = paste0(round(est, digits), " [", round(lb, digits), commaSep, round(ub, digits), "]")
						}
					}
				}
			}
		}
		umx_print(cp_loadings, digits = digits, zero.print = ".", report =report, caption = "Loadings of each trait on the Common Factors")

		# 3. Create Tables of Specific-factor loadings
		# Stack specific path coefficients together
		as = model$top$as$values # Specific factor path coefficients
		cs = model$top$cs$values
		es = model$top$es$values

		specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')), stringsAsFactors = FALSE,
			rbind(diag(as), 
				  diag(cs),
				  diag(es))
		)
		names(specifics) = selDVs
		if(!is.null(cis)){
			specMats = c("as", "cs", "es")
			specSuffix = if(isTRUE(std) && !is.null(model$top$as_std)) "_std" else ""
			for(s in 1:3){
				for(i in 1:nVar){
					address = paste0("top.", specMats[s], specSuffix, "[", i, ",", i, "]")
					if(address %in% rownames(cis)){
						lb = as.numeric(cis[address, "lbound"])[1]
						ub = as.numeric(cis[address, "ubound"])[1]
						est = as.numeric(cis[address, "estimate"])[1]
						if(is.finite(lb) && is.finite(ub) && is.finite(est)){
							if(is.numeric(specifics[[i]])){
								specifics[[i]] = format(round(specifics[[i]], digits), nsmall = digits, trim = TRUE, scientific = FALSE)
							}
							specifics[s, i] = paste0(round(est, digits), " [", round(lb, digits), commaSep, round(ub, digits), "]")
						}
					}
				}
			}
		}
		umx_print(specifics, digits = digits, report = report, caption = "Specific-factor loadings", zero.print = ".")
		xmu_twin_print_means(model, digits = digits, report = report)

		if(showRg) {
			# Make Table of Genetic Correlations
			# Pre & post multiply covariance matrix by inverse of standard deviations
			A = model$top$A$values # Variances
			C = model$top$C$values
			E = model$top$E$values
			Vtot = A + C + E; # Total variance
			nVarIden = diag(nVar)
			NAmatrix = matrix(NA, nVar, nVar);

			rA = tryCatch(solve(sqrt(nVarIden * A)) %*% A %*% solve(sqrt(nVarIden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(nVarIden * C)) %*% C %*% solve(sqrt(nVarIden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(nVarIden * E)) %*% E %*% solve(sqrt(nVarIden * E)), error = function(err) return(NAmatrix)); # E correlations
			genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = selDVs);
			# Make a table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			umx_print(genetic_correlations, digits = digits, zero.print = ".", report = report, caption = "Genetic Correlations")
		}
		if(!is.na(file)){
			umxPlotCP(model, file = file, digits = digits, std = FALSE, means = FALSE)
		}
		if(returnStd) {
			invisible(model)
		}
	}
}

#' @export
umxSummary.MxModelCP <- umxSummaryCP

#' Present the results of an independent-pathway twin model in table and graphical form
#'
#' Summarize a Independent Pathway model, as returned by [umxIP()]
#'
#' @aliases umxSummary.MxModelIP
#' @param model A fitted [umxIP()] model to summarize
#' @param digits round to how many digits (default = 2)
#' @param std = Whether to show the standardized model (TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param comparison Whether to run mxCompare on a comparison model (NULL)
#' @param CIs Whether to print a profile interval that has already been computed (default TRUE). The printed tables are standardized, so the address is `top.ai_std` (and `ci`, `ei`, `as`, `cs`, `es`) when those algebras exist. No interval is computed here.
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param report how to display the results ("html" will open in browser as table)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [OpenMx::mxModel()]
#' @family Summary functions
#' @export
#' @seealso - \code{\link{umxIP}()}, [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF) # family function and well-being data
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("hap", "sat", "AD") # These will be expanded into "hap_T1" "hap_T2" etc.
#' m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' umxSummaryIP(m1)
#' plot(m1)
#' umxSummaryIP(m1, digits = 2, file = "Figure3", showRg = FALSE, CIs = TRUE);
#' }
umxSummaryIP <- function(model, digits = 2, file = getOption("umx_auto_plot"), std = TRUE, showRg = FALSE, comparison = NULL, CIs = TRUE, returnStd = FALSE, report = c("markdown", "html"), ...) {
	umx_check_model(model, "MxModelIP", beenRun = TRUE, callingFn = "umxSummaryIP")
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)

	selDVs = dimnames(model$top.expCovMZ)[[1]]
	stdFit = model; # If we want to output a model with the standardized values (perhaps for drawing a path diagram)
	nVar   = length(selDVs)/2;
	# how to detect how many factors are present?
	# Calculate standardized variance components
	ai = mxEval(top.ai, model); # Column of independent path coefficients (nVar * nFac) 
	ci = mxEval(top.ci, model);
	ei = mxEval(top.ei, model);

	as = mxEval(top.as, model); # nVar * nVar matrix of specific path coefficients (Just diagonal, or possibly Cholesky lower for E)
	cs = mxEval(top.cs, model);
	es = mxEval(top.es, model);

	A  = mxEval(top.A , model); # Totaled Variance components (ai + as etc.)
	C  = mxEval(top.C , model);
	E  = mxEval(top.E , model);

	nFac     = c(a = dim(ai)[2], c = dim(ci)[2], e = dim(ei)[2]);

	Vtot     = A+C+E; # total variance
	nVarIden = diag(nVar); # Make up a little nVar Identity matrix using the behavior of diag to make an nVar*nVar Identity matrix
	SD       = solve(sqrt(nVarIden*Vtot)) # inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	ai_std   = SD %*% ai ; # Standardized path coefficients (independent general factors )
	ci_std   = SD %*% ci ; # Standardized path coefficients (independent general factors )
	ei_std   = SD %*% ei ; # Standardized path coefficients (independent general factors )

	stdFit@submodels$top$ai@values = ai_std
	stdFit@submodels$top$ci@values = ci_std
	stdFit@submodels$top$ei@values = ei_std

	rowNames = sub("(_T)?1$", "", selDVs[1:nVar])
	std_Estimates = data.frame(cbind(ai_std, ci_std, ei_std), row.names = rowNames, stringsAsFactors = FALSE);

	# 1. Make Table of General IP path loadings
	x = sapply(FUN = seq_len, nFac)
	names(std_Estimates) = c(paste0("ai", 1:nFac["a"]), paste0("ci", 1:nFac["c"]), paste0("ei", 1:nFac["e"]))
	commaSep = paste0(umx_set_separator(silent = TRUE), " ")
	cis = NULL
	if(isTRUE(CIs) && !is.null(model$output$confidenceIntervals) && nrow(model$output$confidenceIntervals) > 0){
		cis = model$output$confidenceIntervals
	}
	# The table above is SD %*% ai, which is what top.ai_std stores. Fall back to the raw address when that algebra is absent.
	stdSuffix = if(is.null(model$top$ai_std)) "" else "_std"
	if(!is.null(cis)){
		blockMats = c(rep("ai", nFac["a"]), rep("ci", nFac["c"]), rep("ei", nFac["e"]))
		factorIndex = c(seq_len(nFac["a"]), seq_len(nFac["c"]), seq_len(nFac["e"]))
		for(col in seq_along(blockMats)){
			for(row in 1:nVar){
				address = paste0("top.", blockMats[col], stdSuffix, "[", row, ",", factorIndex[col], "]")
				if(address %in% rownames(cis)){
					lb = as.numeric(cis[address, "lbound"])[1]
					ub = as.numeric(cis[address, "ubound"])[1]
					est = as.numeric(cis[address, "estimate"])[1]
					if(is.finite(lb) && is.finite(ub) && is.finite(est)){
						if(is.numeric(std_Estimates[[col]])){
							std_Estimates[[col]] = format(round(std_Estimates[[col]], digits), nsmall = digits, trim = TRUE, scientific = FALSE)
						}
						std_Estimates[row, col] = paste0(round(est, digits), " [", round(lb, digits), commaSep, round(ub, digits), "]")
					}
				}
			}
		}
	}
	umx_print(std_Estimates, digits = digits, zero.print = ".", report = report, caption = "General Independent Pathway path loadings")

	# 2. Make Table of Standard specific path coefficients ready to be stacked together
	as_std = SD %*% as; # Standardized path coefficients (nVar specific factors matrices)
	cs_std = SD %*% cs;
	es_std = SD %*% es;
	stdFit@submodels$top$as@values = as_std
	stdFit@submodels$top$cs@values = cs_std
	stdFit@submodels$top$es@values = es_std

	std_Specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')),
		rbind(
			diag(as_std), 
			diag(cs_std),
			diag(es_std)
		)
	)
	names(std_Specifics) = rowNames;
	if(!is.null(cis)){
		specMats = c("as", "cs", "es")
		for(s in 1:3){
			for(i in 1:nVar){
				address = paste0("top.", specMats[s], stdSuffix, "[", i, ",", i, "]")
				if(address %in% rownames(cis)){
					lb = as.numeric(cis[address, "lbound"])[1]
					ub = as.numeric(cis[address, "ubound"])[1]
					est = as.numeric(cis[address, "estimate"])[1]
					if(is.finite(lb) && is.finite(ub) && is.finite(est)){
						if(is.numeric(std_Specifics[[i]])){
							std_Specifics[[i]] = format(round(std_Specifics[[i]], digits), nsmall = digits, trim = TRUE, scientific = FALSE)
						}
						std_Specifics[s, i] = paste0(round(est, digits), " [", round(lb, digits), commaSep, round(ub, digits), "]")
					}
				}
			}
		}
	}
	umx_print(std_Specifics, digits = digits, zero.print = ".", report = report, caption = "Specific factor loadings")
	xmu_twin_print_means(model, digits = digits, report = report)
	
	if(showRg) {
		# Pre & post multiply covariance matrix by inverse of standard deviations
		NAmatrix = matrix(NA, nVar, nVar);  
		rA = tryCatch(solve(sqrt(nVarIden*A)) %*% A %*% solve(sqrt(nVarIden*A)), error=function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(nVarIden*C)) %*% C %*% solve(sqrt(nVarIden*C)), error=function(err) return(NAmatrix)); # shared environmental correlations
		rE = tryCatch(solve(sqrt(nVarIden*E)) %*% E %*% solve(sqrt(nVarIden*E)), error=function(err) return(NAmatrix)); # Unique environmental correlations
		genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
		# Make a table
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits = digits, zero.print = ".", report = report, caption = "Genetic Correlations")
	}
	if(!is.na(file)){
		umxPlotIP(x = stdFit, file = file, digits = digits, std = FALSE)
	}
	if(returnStd) {
		return(stdFit)
	}
}

#' @export
umxSummary.MxModelIP <- umxSummaryIP

#' Summarize a GxE model
#'
#' Summarize a genetic moderation model, as returned by [umxGxE()]. Prints the model fit (-2LL, df, AIC) and the parameter table, plus graphs of A, C, and E, standardized and raw.
#'
#' Note: see also [umxReduce()] which knows how to reduce a GxE model.
#'
#' @aliases umxSummary.MxModelGxE
#' @param model A fitted [umxGxE()] model to summarize
#' @param reduce  Whether run and tabulate a complete model reduction...(Defaults to FALSE)
#' @param separateGraphs If TRUE, both std and raw plots in one figure (default FALSE)
#' @param report "markdown" or "html" = open a browser for copyable tables
#' @param gg Whether to use ggplot to create the graphs (default TRUE)
#' @param std Whether to show the standardized model (not implemented! TRUE)
#' @param CIs Confidence intervals (FALSE)
#' @param xlab label for the x-axis of plot
#' @param digits round to how many digits (default = 2)
#' @param location default = "topleft"
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param show not doing anything yet (required for all summary functions)
#' @param ... Optional additional parameters
#' @return - optional [OpenMx::mxModel()]
#' @family Summary functions
#' @export
#' @seealso - [umxGxE()], [umxReduce()], [plot()], [umxSummary)] all work for IP, CP, GxE, and ACE models.
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' # The total sample has been subdivided into a young cohort, 
#' # aged 18-30 years, and an older cohort aged 31 and above.
#' # Cohort 1 Zygosity is coded as follows 1 == MZ females 2 == MZ males 
#' # 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
#  # use ?twinData to learn about this data set.
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, zygosity == "MZFF", selVars)
#' dzData  = subset(twinData, zygosity == "DZMM", selVars)
#' # Exclude cases with missing Def
#' mzData = mzData[!is.na(mzData[selDefs[1]]) & !is.na(mzData[selDefs[2]]),]
#' dzData = dzData[!is.na(dzData[selDefs[1]]) & !is.na(dzData[selDefs[2]]),]
#' m1 = umxGxE(selDVs = "bmi", selDefs = "age", sep="", dzData = dzData, mzData = mzData)
#' # Plot Moderation
#' umxSummaryGxE(m1)
#' umxSummaryGxE(m1, location = "topright")
#' umxSummaryGxE(m1, separateGraphs = FALSE)
#' }
umxSummaryGxE <- function(model = NULL, digits = 2, xlab = NA, location = "topleft", separateGraphs = FALSE, gg=TRUE, file = getOption("umx_auto_plot"), returnStd = NULL, std = NULL, reduce = FALSE, CIs = NULL, report = c("markdown", "html"), show= NULL, ...) {
	report = match.arg(report)
	umx_check(is.null(show), "stop", "For several years now, show has been deprecated in favor of std.\nTo continue, just use 'std = TRUE' instead of show='std'")
	umx_has_been_run(model, stop = TRUE)
	
	if(any(!is.null(c(returnStd, std, CIs) ))){
		message("For GxE, returnStd, std, comparison or CIs are not implemented... The plot will include standardized outcomes, but raw output should be emphasized. SEs are shown rather the CIs, at present")
	}

	if(is.null(model)){
		message("umxSummaryGxE calls plot.MxModelGxE for a twin moderation plot. A use example is:\n umxSummaryGxE(m1, location = \"topleft\")")
		stop();
	}
	fit = as.data.frame(suppressMessages(OpenMx::mxCompare(model)))
	print(data.frame(model = model$name, minus2LL = fit$minus2LL, df = fit$df, AIC = fit$AIC, check.names = FALSE), digits = digits, row.names = FALSE)
	tablePub = summary(model)$parameters[, c("name", "Estimate", "Std.Error")]
	if(report == "html"){
		print(xtable::xtable(tablePub), type = "HTML", file = file, sanitize.text.function = function(x){x}, digits=3)
		umx_open(file)
	} else {
		# markdown
		umx_print(tablePub, digits = digits)
	}

	umxPlotGxE(model, xlab = xlab, location = location, separateGraphs = separateGraphs, gg = gg)

	if(reduce){
		umxReduce(model = model, report = report)
	}
}

#' @export
umxSummary.MxModelGxE <- umxSummaryGxE



#' Get the latentVars from a RAM model
#'
#' @description
#' Get the latentVars from a RAM model, optionally targeting a submodel.
#'
#' @param model a [umxRAM()]
#' @param targetModel name of the model to extract from
#' @return - variables
#' @export
#' @family Reporting Functions
#' @seealso - [umxGetManifests()], [umxRAM()], [umxSuperModel()]
#' @examples
#' \dontrun{
#' library(umx)
#' # Create two sets of data in which X & Y correlate ~ .4 in both datasets.
#' manifests = c("x", "y")
#' tmp = umx_make_TwinData(nMZpairs = 100, nDZpairs = 150, 
#' 		AA = 0, CC = .4, EE = .6, varNames = manifests)
#' 
# Group 1
#' grp1   = tmp[tmp$zygosity == "MZ", manifests]
#' g1Data = mxData(cov(grp1), type = "cov", numObs = nrow(grp1), means=umx_means(grp1))
#' 
# Group 2
#' grp2   = tmp[tmp$zygosity == "DZ", manifests]
#' g2Data = mxData(cov(grp2), type = "cov", numObs = nrow(grp2), means=umx_means(grp2))

#' # Model 1 (could add autoRun = FALSE if you don't want to run this as it is being built)
#' m1 = umxRAM("m1", data = g1Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels = c("Var_x", "Resid_y_grp1")),
#' 	umxPath(means = manifests, labels = c("Mean_x", "Mean_y"))
#' )
#' 
#' # Model 2
#' m2 = umxRAM("m2", data = g2Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels=c("Var_x", "Resid_y_grp2")),
#' 	umxPath(means = manifests, labels=c("Mean_x", "Mean_y"))
#' )
#' 
#' m3 = umxSuperModel('top', m1, m2)
#' umxGetLatents(m3)
#' umxGetLatents(m3, targetModel = "m1")
#' }
umxGetLatents <- function(model, targetModel = NULL) {
	if(is.null(targetModel)){
		return(model$latentVars)
	} else if(model$name == targetModel){
		return(model$latentVars)
	} else{
		return(model$submodels[[targetModel]]$latentVars)
	}
}

#' Get the manifestVars from a RAM model
#'
#' @description
#' Get the latentVars from a RAM model, optionally targeting a submodel.
#'
#' @param model a [umxRAM()]
#' @param targetModel name of the model to extract from
#' @return - variables
#' @export
#' @family Reporting Functions
#' @seealso - [umxGetManifests()], [umxRAM()], [umxSuperModel()]
#' @examples
#' \dontrun{
#' library(umx)
#' # Create two sets of data in which X & Y correlate ~ .4 in both datasets.
#' manifests = c("x", "y")
#' tmp = umx_make_TwinData(nMZpairs = 100, nDZpairs = 150, 
#' 		AA = 0, CC = .4, EE = .6, varNames = manifests)
#' 
# Group 1
#' grp1   = tmp[tmp$zygosity == "MZ", manifests]
#' g1Data = mxData(cov(grp1), type = "cov", numObs = nrow(grp1), means=umx_means(grp1))
#' 
# Group 2
#' grp2   = tmp[tmp$zygosity == "DZ", manifests]
#' g2Data = mxData(cov(grp2), type = "cov", numObs = nrow(grp2), means=umx_means(grp2))

#' # Model 1 (could add autoRun = FALSE if you don't want to run this as it is being built)
#' m1 = umxRAM("m1", data = g1Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels = c("Var_x", "Resid_y_grp1")),
#' 	umxPath(means = manifests, labels = c("Mean_x", "Mean_y"))
#' )
#' 
#' # Model 2
#' m2 = umxRAM("m2", data = g2Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels=c("Var_x", "Resid_y_grp2")),
#' 	umxPath(means = manifests, labels=c("Mean_x", "Mean_y"))
#' )
#' 
#' m3 = umxSuperModel('top', m1, m2)
#' umxGetManifests(m3)
#' umxGetManifests(m3, targetModel = "m1")
#' }
umxGetManifests <- function(model, targetModel = NULL) {
	if(is.null(targetModel)){
		return(model$manifestVars)
	} else if(model$name == targetModel){
		return(model$manifestVars)
	} else {
		return(model$submodels[[targetModel]]$manifestVars)
	}
}

#' Used to get a RAM submodel by name
#'
#' @description
#' Get any model from a RAM model, including submodels.
#'
#' @param model a [umxRAM()] model.
#' @param targetModel name of the model to extract from
#' @return - model
#' @export
#' @family Reporting Functions
#' @seealso - [umxGetManifests()], [umxRAM()], [umxSuperModel()]
#' @examples
#' \dontrun{
#' library(umx)
#' # Create two sets of data in which X & Y correlate ~ .4 in both datasets.
#' manifests = c("x", "y")
#' tmp = umx_make_TwinData(nMZpairs = 100, nDZpairs = 150, 
#' 		AA = 0, CC = .4, EE = .6, varNames = manifests)
#' 
# Group 1
#' grp1   = tmp[tmp$zygosity == "MZ", manifests]
#' g1Data = mxData(cov(grp1), type = "cov", numObs = nrow(grp1), means=umx_means(grp1))
#' 
# Group 2
#' grp2   = tmp[tmp$zygosity == "DZ", manifests]
#' g2Data = mxData(cov(grp2), type = "cov", numObs = nrow(grp2), means=umx_means(grp2))

#' # Model 1 (could add autoRun = FALSE if you don't want to run this as it is being built)
#' m1 = umxRAM("m1", data = g1Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels = c("Var_x", "Resid_y_grp1")),
#' 	umxPath(means = manifests, labels = c("Mean_x", "Mean_y"))
#' )
#' 
#' # Model 2
#' m2 = umxRAM("m2", data = g2Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels = c("Var_x", "Resid_y_grp2")),
#' 	umxPath(means = manifests, labels = c("Mean_x", "Mean_y"))
#' )
#' 
#' m3 = umxSuperModel('top', m1, m2)
#' umxGetModel(m3)
#' umxGetModel(m3, targetModel = "m1")
#' }
umxGetModel <- function(model, targetModel = NULL) {
	if(is.null(targetModel)){
		return(model)
	} else if(model$name == targetModel){
		return(model)
	} else {
		return(model$submodels[[targetModel]])
	}
}


#' umxCI_boot
#'
#' Compute boot-strapped Confidence Intervals for parameters in an [OpenMx::mxModel()]
#' The function creates a sampling distribution for parameters by repeatedly drawing samples
#' with replacement from your data and then computing the statistic for each redrawn sample.
#' @param model is an optimized mxModel
#' @param rawData is the raw data matrix used to estimate model
#' @param type is the kind of bootstrap you want to run. "par.expected" and "par.observed" 
#' use parametric Monte Carlo bootstrapping based on your expected and observed covariance matrices, respectively.
#' "empirical" uses empirical bootstrapping based on rawData.
#' @param std specifies whether you want CIs for unstandardized or standardized parameters (default: std = TRUE)
#' @param rep is the number of bootstrap samples to compute (default = 1000).
#' @param conf is the confidence value (default = 95)
#' @param dat specifies whether you want to store the bootstrapped data in the output (useful for multiple analyses, such as mediation analysis)
#' @param digits rounding precision
#' @return - expected covariance matrix
#' @export
#' @seealso - [umxExpMeans()], [umxExpCov()]
#' @family Reporting functions
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#'
#' umxCI_boot(m1, type = "par.expected")
#'}
umxCI_boot <- function(model, rawData = NULL, type = c("par.expected", "par.observed", "empirical"), std = TRUE, rep = 1000, conf = 95, dat = FALSE, digits = 3) {
	# depends on MASS::mvrnorm
	type = match.arg(type, c("par.expected", "par.observed", "empirical"))
	if(type == "par.expected") {
		exp = umxExpCov(model, latents = FALSE)
	} else if(type == "par.observed") {
		if(model$data$type == "raw") {
			exp = var(mxEval(data, model))
		} else { 
			if(model$data$type == "sscp") {
				exp = mxEval(data, model) / (model$data$numObs - 1)
			} else {
				exp = mxEval(data, model)
			}
		}
	}
	N = round(model$data$numObs)
	pard = t(data.frame("mod" = summary(model)$parameters[, 5 + 2 * std], row.names = summary(model)$parameters[, 1]))
	pb   = txtProgressBar(min = 0, max = rep, label = "Computing confidence intervals", style = 3)

	if(type == "empirical") {
		if(length(rawData) == 0) {
			if(model$data$type == "raw"){
				rawData = mxEval(data, model)
			} else {
				stop("No raw data supplied for empirical bootstrap.")	
			}
		}
		for(i in 1:rep){
			bsample.i = sample.int(N, size = N, replace = TRUE)
			bsample   = var(rawData[bsample.i, ])
			mod       = mxRun(mxModel(model, mxData(observed = bsample, type = "cov", numObs = N)), silent = TRUE)
			pard      = rbind(pard, summary(mod)$parameters[, 5 + 2*std])
			rownames(pard)[nrow(pard)] = i
			utils::setTxtProgressBar(pb, i)
		}
	} else {
		for(i in 1:rep){
			bsample = var(MASS::mvrnorm(N, rep(0, nrow(exp)), exp))
			mod     = mxRun(mxModel(model, mxData(observed = bsample, type = "cov", numObs = N)), silent = TRUE)
			pard    = rbind(pard, summary(mod)$parameters[, 5 + 2 * std])
			rownames(pard)[nrow(pard)] = i
			utils::setTxtProgressBar(pb, i)
		}
	}
	low = (1-conf/100)/2
	upp = ((1-conf/100)/2) + (conf/100)
	LL  = apply(pard, 2, FUN = quantile, probs = low) # lower limit of confidence interval
	UL  = apply(pard, 2, FUN = quantile, probs = upp) # upper quantile for confidence interval
	LL4 = round(LL, 4)
	UL4 = round(UL, 4)
	ci  = cbind(LL4, UL4)
	colnames(ci) = c(paste((low*100), "%", sep = ""), paste((upp*100), "%", sep = ""))
	p         = summary(model)$parameters[, c(1, 2, 3, 4, c(5:6 + 2*std))]
	cols      = sapply(p, is.numeric)
	p[, cols] = round(p[,cols], digits) 
	
	if(dat) {
		return(list("Type" = type, "bootdat" = data.frame(pard), "CI" = cbind(p, ci)))
	} else {
		return(list("CI" = cbind(p, ci)))
	}
}


# ============
# = Graphics =
# ============

#' Create and display a graphical path diagram for a LISREL model.
#'
#' `plot.MxLISRELModel` produces SEM diagrams using [[DiagrammeR::DiagrammeR()] to create the image. 
#' 
#' \emph{Note:} By default, plots open in your browser (or plot pane if using RStudio).
#' 
#' **Opening in an external editor/app**
#' 
#' The underlying format is graphviz.
#' If you use `umx_set_plot_format("graphviz")`, figures will open in a graphviz helper app (if installed).
#' If file is set, we will try and open an app to handle rendering the resulting file.
#' The commercial application \dQuote{OmniGraffle} is great for editing these images.
#' 
#' You may need to associate the \sQuote{.gv} extension with your app (if you use that rather than the browser).
#'
#' @rdname plot.MxLISRELModel
#' @param x A LISREL [OpenMx::mxModel()] from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE).
#' @param fixed Whether to show fixed paths (defaults to TRUE)
#' @param means Whether to show means or not (default = TRUE)
#' @param digits The number of decimal places to add to the path coefficients
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param labels Whether to show labels on the paths. both will show both the parameter and the label. ("both", "none" or "labels")
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param splines Whether to allow lines to curve: defaults to "TRUE" (nb: some models look better with "FALSE")
#' @param min optional list of objects to group at the top of the plot. Default (NULL) chooses automatically.
#' @param same optional list of objects to group at the same rank in the plot. Default (NULL) chooses automatically.
#' @param max optional list of objects to group at the bottom of the plot. Default (NULL) chooses automatically.
#' @param ... Optional parameters
#' @export
#' @seealso - [umx_set_plot_format()], [umx_set_auto_plot()], [umx_set_plot_format()], [plot.MxModel()], [umxPlotACE()], [umxPlotCP()], [umxPlotIP()], [umxPlotGxE()]
#' @family umx S3 functions
#' @family Plotting functions
#' @references <https://github.com/tbates/umx>, <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
#' @examples
#' \dontrun{
#' library(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxLISREL("one_factor", data = demoOneFactor, type = "cov",
#'                umxPath("G", to = manifests),
#'                umxPath(var = manifests),
#'                umxPath(var = "G", fixedAt = 1))
#' plot(m1)
#' }
plot.MxLISRELModel <- function(x = NA, std = FALSE, fixed = TRUE, means = TRUE, digits = 2, file = "name", labels = c("none", "labels", "both"), resid = c("circle", "line", "none"), strip_zero = TRUE, splines = c("TRUE", "FALSE", "compound", "ortho", "polyline"), min = NULL, same = NULL, max = NULL, ...) {
	if(is.logical(splines)){ splines = ifelse(splines, "TRUE", "FALSE")}
	splines = match.arg(splines)

	# loop over submodels
	if(length(x@submodels)){
		n = 1
		for (sub in x@submodels) {
			if(file == "name"){
				thisFile = file
			} else {
				thisFile = paste0(file, "_group_", n)
			}
			plot(sub, std = std, fixed = fixed, means = means, digits = digits, file = thisFile, labels = labels, resid = resid, strip_zero = strip_zero, splines = splines, min = min, same = same, max = max, ...)
			n = n + 1
		}
	} else {
		# ==========
		# = Setup  =
		# ==========
		model   = x # just to be clear that x is a model
		resid   = match.arg(resid)
		labels  = match.arg(labels)
		expect  = model$expectation

		# Update values using compute = T to capture labels with [] references.
		updateMatrixVals <- function(slotName) {
			if (is.null(slotName) || is.na(slotName)) return()
			if (!is.null(model[[slotName]])) {
				model[[slotName]]$values[,] <<- mxEvalByName(slotName, model, compute = TRUE)
			}
		}
		
		updateMatrixVals(expect@LX)
		updateMatrixVals(expect@LY)
		updateMatrixVals(expect@BE)
		updateMatrixVals(expect@GA)
		updateMatrixVals(expect@PH)
		updateMatrixVals(expect@PS)
		updateMatrixVals(expect@TD)
		updateMatrixVals(expect@TE)
		updateMatrixVals(expect@TH)
		updateMatrixVals(expect@TX)
		updateMatrixVals(expect@TY)
		updateMatrixVals(expect@KA)
		updateMatrixVals(expect@AL)

		if(std){ model = umx_standardize(model) }

		# Extract manifests and latents
		matLX <- if(!is.null(expect@LX) && !is.na(expect@LX)) model[[expect@LX]] else NULL
		matLY <- if(!is.null(expect@LY) && !is.na(expect@LY)) model[[expect@LY]] else NULL
		matBE <- if(!is.null(expect@BE) && !is.na(expect@BE)) model[[expect@BE]] else NULL
		matGA <- if(!is.null(expect@GA) && !is.na(expect@GA)) model[[expect@GA]] else NULL
		matPH <- if(!is.null(expect@PH) && !is.na(expect@PH)) model[[expect@PH]] else NULL
		matPS <- if(!is.null(expect@PS) && !is.na(expect@PS)) model[[expect@PS]] else NULL
		matTD <- if(!is.null(expect@TD) && !is.na(expect@TD)) model[[expect@TD]] else NULL
		matTE <- if(!is.null(expect@TE) && !is.na(expect@TE)) model[[expect@TE]] else NULL
		matTH <- if(!is.null(expect@TH) && !is.na(expect@TH)) model[[expect@TH]] else NULL
		matTX <- if(!is.null(expect@TX) && !is.na(expect@TX)) model[[expect@TX]] else NULL
		matTY <- if(!is.null(expect@TY) && !is.na(expect@TY)) model[[expect@TY]] else NULL
		matKA <- if(!is.null(expect@KA) && !is.na(expect@KA)) model[[expect@KA]] else NULL
		matAL <- if(!is.null(expect@AL) && !is.na(expect@AL)) model[[expect@AL]] else NULL

		selDVs_x  <- if(!is.null(matLX)) rownames(matLX$values) else c()
		selDVs_y  <- if(!is.null(matLY)) rownames(matLY$values) else c()
		latents_xi  <- if(!is.null(matPH)) colnames(matPH$values) else (if(!is.null(matLX)) colnames(matLX$values) else c())
		latents_eta <- if(!is.null(matPS)) colnames(matPS$values) else (if(!is.null(matLY)) colnames(matLY$values) else c())

		latents = unique(c(latents_xi, latents_eta))
		selDVs  = unique(c(selDVs_x, selDVs_y))

		# ==================================
		# = Get Asymmetric & Symmetric Paths =
		# ==================================
		out = "";
		
		# Asymmetric paths
		if(!is.null(matLX)) {
			out = xmu_dot_make_paths(matLX, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "LX paths", digits = digits)
		}
		if(!is.null(matLY)) {
			out = xmu_dot_make_paths(matLY, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "LY paths", digits = digits)
		}
		if(!is.null(matBE)) {
			out = xmu_dot_make_paths(matBE, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "BE paths", digits = digits)
		}
		if(!is.null(matGA)) {
			out = xmu_dot_make_paths(matGA, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "GA paths", digits = digits)
		}

		# Symmetric paths (covariances)
		if(!is.null(matPH)) {
			out = xmu_dot_make_paths(matPH, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "PH covariances", digits = digits)
		}
		if(!is.null(matPS)) {
			out = xmu_dot_make_paths(matPS, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "PS covariances", digits = digits)
		}
		if(!is.null(matTD)) {
			out = xmu_dot_make_paths(matTD, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "TD covariances", digits = digits)
		}
		if(!is.null(matTE)) {
			out = xmu_dot_make_paths(matTE, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "TE covariances", digits = digits)
		}
		if(!is.null(matTH)) {
			p_th <- xmu_dot_mat2dot(matTH, cells = "any", from = "cols", fromLabel = colnames(matTH$values), toLabel = rownames(matTH$values), fromType = "manifest", toType = "manifest", arrows = "both", showFixed = fixed, p = list(str = out, latents = c(), manifests = c()))
			out <- p_th$str
		}

		# Residuals / variances
		variances     <- c()
		varianceNames <- c()
		
		if(!is.null(matPH)) {
			tmp <- xmu_dot_make_residuals(matPH, latents = latents_xi, fixed = fixed, digits = digits, resid = resid)
			variances     <- c(variances, tmp$variances)
			varianceNames <- c(varianceNames, tmp$varianceNames)
		}
		if(!is.null(matPS)) {
			tmp <- xmu_dot_make_residuals(matPS, latents = latents_eta, fixed = fixed, digits = digits, resid = resid)
			variances     <- c(variances, tmp$variances)
			varianceNames <- c(varianceNames, tmp$varianceNames)
		}
		if(!is.null(matTD)) {
			tmp <- xmu_dot_make_residuals(matTD, latents = NULL, fixed = fixed, digits = digits, resid = resid)
			variances     <- c(variances, tmp$variances)
			varianceNames <- c(varianceNames, tmp$varianceNames)
		}
		if(!is.null(matTE)) {
			tmp <- xmu_dot_make_residuals(matTE, latents = NULL, fixed = fixed, digits = digits, resid = resid)
			variances     <- c(variances, tmp$variances)
			varianceNames <- c(varianceNames, tmp$varianceNames)
		}

		# =================
		# = Define shapes =
		# =================
		preOut = paste0('\tsplines="', splines , '";\n\t# Latents\n')
		for(var in latents) {
		   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}

		preOut = paste0(preOut, "\n\t# Manifests\n")
		for(var in selDVs) {
		   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
		}

		# ================
		# = handle means =
		# ================
		hasMeans <- !is.null(matTX) || !is.null(matTY) || !is.null(matKA) || !is.null(matAL)
		if(hasMeans && means){
			preOut = paste0(preOut, "\t one [shape = triangle];\n")
			out = paste0(out, "\n\t# Means paths\n")
			
			p_means = list(str = out, latents = c(), manifests = c())
			if (!is.null(matTX)) {
				p_means <- xmu_dot_mat2dot(matTX, cells = "any", from = "cols", fromLabel = "one", toLabel = rownames(matTX$values), fromType = "latent", toType = "manifest", arrows = "forward", showFixed = fixed, p = p_means)
			}
			if (!is.null(matTY)) {
				p_means <- xmu_dot_mat2dot(matTY, cells = "any", from = "cols", fromLabel = "one", toLabel = rownames(matTY$values), fromType = "latent", toType = "manifest", arrows = "forward", showFixed = fixed, p = p_means)
			}
			if (!is.null(matKA)) {
				p_means <- xmu_dot_mat2dot(matKA, cells = "any", from = "cols", fromLabel = "one", toLabel = rownames(matKA$values), fromType = "latent", toType = "latent", arrows = "forward", showFixed = fixed, p = p_means)
			}
			if (!is.null(matAL)) {
				p_means <- xmu_dot_mat2dot(matAL, cells = "any", from = "cols", fromLabel = "one", toLabel = rownames(matAL$values), fromType = "latent", toType = "latent", arrows = "forward", showFixed = fixed, p = p_means)
			}
			out <- p_means$str
		}

		# ===========================
		# = Make the variance lines =
		# ===========================
		preOut = paste0(preOut, "\n\t#Variances/residuals\n")
		for(var in variances) {
		   preOut = paste0(preOut, "\t", var, ";\n")
		}

		# ======================
		# = Set the ranks e.g. =
		# ======================
		if(hasMeans && means){ varianceNames <- append(varianceNames, "one") }

		x_ranks = xmu_dot_move_ranks(max = max, min = min, same = same, old_min = latents, old_same = selDVs, old_max = varianceNames)
		rankVariables = xmu_dot_rank_str(min = x_ranks$min, same = x_ranks$same, max = x_ranks$max)

		# ===================================
		# = Assemble full text to write out =
		# ===================================
		label = model$name
		digraph = paste0(
			"digraph G {\n    ", 
			'label="', label, '";\n',
			preOut, "\n",
			out   , "\n",
			rankVariables, "\n}"
		)
		message("\n?plot.MxLISRELModel options: std, means, digits, strip_zero, file, splines=T/F/ortho,..., min=, max =, same = , fixed, resid= 'circle|line|none'")
		xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	}
}

#' Create and display a graphical path diagram for a model.
#'
#' [plot()] produces SEM diagrams in graphviz format, and relies on [DiagrammeR::DiagrammeR()] to create the image. 
#'
#' \emph{Note:} [DiagrammeR::DiagrammeR()] is supported out of the box.  By default, plots open in your browser. Other options include pdf SVG etc. 
#' 
#' If you use `umx_set_plot_format("graphviz")`, graphs will open in a graphviz helper app (if installed).
#' 
#' The commercial application \dQuote{OmniGraffle} is great for editing these images.
#' On unix and windows, [plot()] will create a pdf and open it in your default pdf reader.
#' 
#' If you use graphviz, we try and use that app, but YOU HAVE TO INSTALL IT!
#' 
#' *MacOS note*: On Mac, we will try and open an app: you may need to associate the \sQuote{.gv}
#' extension with the graphviz app.
#' Find the .gv file made by plot, get info (cmd-I), then choose \dQuote{open with}, 
#' select graphviz.app (or OmniGraffle professional),
#' then set \dQuote{change all}.
#'
#' @aliases plot
#' @rdname plot.MxModel
#' @param x An [OpenMx::mxModel()] from which to make a path diagram
#' @param std Whether to standardize the model (default = FALSE).
#' @param fixed Whether to show fixed paths (defaults to TRUE)
#' @param means Whether to show means or not (default = TRUE)
#' @param digits The number of decimal places to add to the path coefficients
#' @param uncertainty What type of parameter uncertainty to report: "SE" (standard ML standard errors), "MLR" (robust standard errors and robust fit), "CI" (Wald interval from the reported SE), or "none" (no Wald interval or SE). A profile interval already stored for a raw free parameter is drawn on that path, including when this is "none". Standardized diagrams do not use that raw interval.
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param labels Whether to show labels on the paths. "none", "labels", or "both" (parameter + label).
#' @param resid How to show residuals and variances default is "circle". Options are "line" & "none"
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = FALSE)
#' @param splines Whether to allow lines to curve: defaults to "TRUE" (nb: some models look better with "FALSE")
#' @param min optional list of objects to group at the top of the plot. Default (NULL) chooses automatically.
#' @param same optional list of objects to group at the same rank in the plot. Default (NULL) chooses automatically.
#' @param max optional list of objects to group at the bottom of the plot. Default (NULL) chooses automatically.
#' @param format Output format: "current" (default), "graphviz", or "DiagrammeR".
#' @param ... Optional parameters
#' @export
#' @seealso - [umx_set_plot_format()], [plot.MxModel()], [umxPlotACE()], [umxPlotCP()], [umxPlotIP()], [umxPlotGxE()]
#' @family Plotting functions
#' @references <https://github.com/tbates/umx>, <https://en.wikipedia.org/wiki/DOT_(graph_description_language)>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' plot(m1)
#' plot(m1, std = TRUE, resid = "line", digits = 3, strip_zero = FALSE)
#' 
#' # ============================================================
#' # = With a growth model, demonstrate splines= false to force =
#' # = straight lines, and move "rank" of intercept object      =
#' # ============================================================
#' 
#' m1 = umxRAM("grow", data = myGrowthMixtureData,
#'		umxPath(var = manifests, free = TRUE), 
#'		umxPath(means = manifests, fixedAt = 0), 
#'		umxPath(v.m. = c("int","slope")),
#'		umxPath("int", with = "slope"),
#'		umxPath("int", to = manifests, fixedAt = 1), 
#'		umxPath("slope", to = manifests, arrows = 1, fixedAt = c(0,1,2,3,4))
#' )
#'
#' plot(m1, means= FALSE, strip= TRUE, splines= "FALSE", max= "int")
#' } # end dontrun
#'
plot.MxModel <- function(x = NA, std = FALSE, fixed = TRUE, means = TRUE, digits = 2, uncertainty = c("none", "SE", "MLR", "CI"), file = "name", labels = c("none", "labels", "both"), resid = c("circle", "line", "none"), strip_zero = FALSE, splines = c("TRUE", "FALSE", "compound", "ortho", "polyline"), min= NULL, same= NULL, max= NULL, format = c("current", "graphviz", "DiagrammeR"), ...) {
	if (!is.null(x$expectation) && class(x$expectation)[[1]] == "MxExpectationLISREL") {
		return(plot.MxLISRELModel(x = x, std = std, fixed = fixed, means = means, digits = digits, file = file, labels = labels, resid = resid, strip_zero = strip_zero, splines = splines, min = min, same = same, max = max, uncertainty = uncertainty, ...))
	}
	if(is.logical(splines)){ splines = ifelse(splines, "TRUE", "FALSE")}
	splines = match.arg(splines)
	uncertainty = match.arg(uncertainty)
	format = match.arg(format)

	# loop over submodels
	if(length(x@submodels)){
		n = 1
		for (sub in x@submodels) {
			if(!is.na(file) && file == "name"){
				thisFile = file
			} else {
				thisFile = paste0(file, "_group_", n)
			}
			plot.MxModel(sub, std = std, fixed = fixed, means = means, digits = digits, file = file, labels = labels, resid = resid, strip_zero = strip_zero, splines = splines, min= min, same= same, max= max, uncertainty = uncertainty, format = format, ...)
			n = n + 1
		}
	} else {
		# ==========
		# = Setup  =
		# ==========
		model   = x # just to be clear that x is a model
		resid   = match.arg(resid)
		labels  = match.arg(labels)
		latents = model@latentVars # 'vis', 'math', and 'text' 
		selDVs  = model@manifestVars # 'visual', 'cubes', 'paper', 'general', 'paragrap'...
	
		# MLR/RobustSE: ML casewise sandwich; for WLS keep OpenMx GMM/moment-sandwich SEs
		if (uncertainty %in% c("MLR")) {
			if (xmu_is_wls(model)) {
				if (is.null(model$output$vcov)) {
					warning("WLS model has no parameter covariance matrix in output; SEs unavailable.", call. = FALSE)
				}
			} else {
				if (is.null(model$data) || is.null(model$data$observed) || identical(model$data$type, "cov")) {
					stop("Robust standard errors for ML require raw data. The model has covariance or missing data.")
				}
				resRobust = imxRobustSE(model, details = TRUE)
				model@output$vcov = resRobust$cov
				model@output$standardErrors = resRobust$SE
			}
		}

		# Compute standardized/raw paths and standard errors once using mxStandardizeRAMpaths
		pt = NULL
		if (uncertainty %in% c("SE", "MLR", "CI")) {
			flatten_pTable <- function(obj) {
				if (is.data.frame(obj)) {
					return(obj)
				} else if (is.list(obj)) {
					if (length(obj) == 0) return(NULL)
					dfs = lapply(obj, flatten_pTable)
					dfs = dfs[vapply(dfs, is.data.frame, logical(1))]
					if (length(dfs) > 0) {
						return(do.call(rbind, dfs))
					}
				}
				return(NULL)
			}
			hasVcov = !is.null(model$output$vcov)
			pt = mxStandardizeRAMpaths(model, SE = hasVcov)
			pt = flatten_pTable(pt)
			if (!is.null(pt) && nrow(pt) > 0) {
				names(pt) = c("label", "name", "matrix", "row", "col", "Estimate", "SE", "Std.Estimate", "Std.SE")
			}
		}
		
		cis = model$output$confidenceIntervals

		# Update values using compute = T to capture labels with [] references.
		model$S$values = mxEval(S, model, compute = TRUE)
		model$A$values = mxEval(A, model, compute = TRUE)
		if(!is.null(model$M)){
			model$M$values = mxEval(M, model, compute = TRUE)
		}
	
		if(std){ model = xmu_standardize_RAM(model, return = "model") }

		# ========================
		# = Get Symmetric & Asymmetric Paths =
		# ========================
		out = "";
		out = xmu_dot_make_paths(model$matrices$A, stringIn = out, heads = 1, fixed = fixed, labels = labels, comment = "Single arrow paths", digits = digits, pt = pt, std = std, uncertainty = uncertainty, cis = cis)
		if(resid == "circle"){
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE, fixed = fixed, labels = labels, comment = "Covariances", digits = digits, pt = pt, std = std, uncertainty = uncertainty, cis = cis)
		} else if(resid == "line"){
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = TRUE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits, pt = pt, std = std, uncertainty = uncertainty, cis = cis)
		}else{
			out = xmu_dot_make_paths(model$matrices$S, stringIn = out, heads = 2, showResiduals = FALSE , fixed = fixed, labels = labels, comment = "Covariances & residuals", digits = digits, pt = pt, std = std, uncertainty = uncertainty, cis = cis)		
		}
		# TODO should xmu_dot_make_residuals handle fixed or not necessary?
		tmp = xmu_dot_make_residuals(model$matrices$S, latents = latents, digits = digits, resid = resid, pt = pt, std = std, uncertainty = uncertainty, cis = cis)
		variances     = tmp$variances     # either "var_var textbox" or "var -> var port circles"
		varianceNames = tmp$varianceNames # names of residuals/variances. EMPTY if using circles 
		# =================
		# = Define shapes =
		# =================
		preOut = paste0('\tsplines="', splines , '";\n\t# Latents\n')

		for(var in latents) {
		   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}

		preOut = paste0(preOut, "\n\t# Manifests\n")
		for(var in selDVs) {
		   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
		}

	
		# ================
		# = handle means =
		# ================
		if(umx_has_means(model) & means){
			out = paste0(out, "\n\t# Means paths\n")
			# Add a triangle to the list of shapes
			preOut = paste0(preOut, "\t one [shape = triangle];\n")
			mxMat = model$matrices$M
			mxMat_vals   = mxMat$values
			mxMat_free   = mxMat$free
			mxMat_labels = mxMat$labels
			meanVars = colnames(mxMat$values)
			for(to in meanVars) {
				thisPathLabel = mxMat_labels[1, to]
				thisPathFree  = mxMat_free[1, to]
				thisPathVal   = round(mxMat_vals[1, to], digits)
				if(thisPathFree){
					labelStart = ' [label="' 
				} else {
					labelStart = ' [label="@'
				}

				# TODO plot.MxModel: Find way to show means fixed @0
				if(thisPathFree || fixed ) {
					# if(thisPathFree | (fixed & thisPathVal != 0) ) {
					out = paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n')
				}else{
					# cat(paste0(out, "\tone -> ", to, labelStart, thisPathVal, '"];\n'))
					# return(thisPathVal != 0)
				}
			}
		}

		# ===========================
		# = Make the variance lines =
		# ===========================
		# x1_var [label="0.21", shape = plaintext];
		# or (circles)
		# x1 -> x1 [label="0.21", direction = both];
		preOut = paste0(preOut, "\n\t#Variances/residuals\n")
		for(var in variances) {
		   preOut = paste0(preOut, "\t", var, ";\n")
		}
		# ======================
		# = Set the ranks e.g. =
		# ======================
		# {rank=same; x1 x2 x3 x4 x5 };
		# TODO more intelligence possible in plot() perhaps hints like "MIMIC" or "ACE"
		if(umx_has_means(model)){ append(varianceNames, "one")}

		# min = latents; same = selDVs; max = varianceNames
		x = xmu_dot_move_ranks(max = max, min = min, same=same, old_min = latents, old_same = selDVs, old_max = varianceNames)
		rankVariables = xmu_dot_rank_str(min = x$min, same = x$same, max = x$max)

		# ===================
		# = Selection paths =
		# ===================
		if(!is.null(model$expectation$selectionPlan)){
			# Draw each found "from-to" pair as a headless arrow on the diagram.
			
			selPaths = cbind(model$expectation$selectionPlan, value = model[["selectionVector"]]$values)
		  #   step from to     values
		  # 1    1   V1 V2 -0.2373752

			for(thisPath in 1:nrow(selPaths)) {
		 	    step  = selPaths[thisPath, "step"] # color
				color = c("red", "green", "blue")[step]
				from  = selPaths[thisPath, "from"]
				to    = selPaths[thisPath, "to"]
				val   = round(selPaths[thisPath, "value"], digits)
				newPath = paste0("\t", from, " ->", to, "[label='", val, "' color='", color, "' dir='none'];\n")
				out   = paste0(out, newPath)
			}			
		}
		
		# ===================================
		# = Assemble full text to write out =
		# ===================================
		label = model$name
		digraph = paste0(
			"digraph G {\n    ", 
			'label="', label, '";\n',
			preOut, "\n",
			out   , "\n",
			rankVariables, "\n}"
		)
		message("\n?plot.MxModel options: std, means, digits, strip_zero, file, splines=T/F/ortho,..., min=, max =, same = , fixed, resid= 'circle|line|none'")
		# cat(out)
		# plot(),
		if(format != "current"){
			tmp = umx_set_plot_format(silent=TRUE)
			umx_set_plot_format(format)
			res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
			umx_set_plot_format(tmp)
			if(res$visible) res$value else invisible(res$value)
		}else{
			res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
			if(res$visible) res$value else invisible(res$value)
		}
	}
} # end plot.MxModel

#' @rdname plot.MxModel
#' @export
plot.MxRAMModel <- plot.MxModel

#' Make a graphical display of an ACE model
#'
#' plot method for [umxACE()] models. Make a graphical display of an ACE model
#'
#' @aliases plot.MxModelACE
#' @param x [OpenMx::mxModel()] to plot (created by umxACE in order to inherit the MxModelACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param means Whether to show means paths (default is FALSE)
#' @param std Whether to standardize the model (default is TRUE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param showFixed Whether too draw fixed parameters.
#' @param format Output format: "current" (default), "graphviz", or "DiagrammeR".
#' @param showCIs Whether to draw a profile interval that has already been computed (default TRUE). Intervals are looked up on the standardized diagram. `std = FALSE` keeps the point estimate. Means stay point estimates. This does not compute intervals.
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxACE()]
#' @references <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACE("plotACE example", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#' plot(m1, std = FALSE) # don't standardize
#' }
umxPlotACE <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, showFixed = FALSE, format = c("current", "graphviz", "DiagrammeR"), showCIs = TRUE, ...) {
	model = x # just to be clear that x is a model
	format = match.arg(format)
	if(std){model = xmu_standardize_ACE(model)}

	nVar   = dim(model$top$a$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = sub("(_T)?[0-9]$", "", selDVs[1:(nVar)]) # trim "_Tn" from end
	out    = list(str = "", latents = c(), manifests = c())
	# Profile intervals from umxConfint(parm = "smart") are on top.a_std / c_std / e_std.
	# xmu_get_CI maps a_r1c1 to that standardized address, so the raw diagram stays a point estimate.
	ciModel = NULL
	drawCIs = FALSE
	if(isTRUE(showCIs) && isTRUE(std)){
		ciModel = model
		drawCIs = TRUE
	}

	# 2. Factor correlations on the lower
	out = xmu_dot_mat2dot(model$top$a, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, model = ciModel, showCIs = drawCIs, p = out)
	out = xmu_dot_mat2dot(model$top$c, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, model = ciModel, showCIs = drawCIs, p = out)
	out = xmu_dot_mat2dot(model$top$e, cells = "lower_inc", from = "cols", toLabel = selDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, digits = digits, model = ciModel, showCIs = drawCIs, p = out)

	# Process "expMean" 1 * nVar matrix. Means are not part of the smart interval set.
	if(means){
		# from = "one"; target = selDVs[c]
		out = xmu_dot_mat2dot(model$top$intercept , cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, digits = digits, showCIs = FALSE, p = out)
		out = xmu_dot_mat2dot(model$top$meansBetas, cells = "any" , toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, digits = digits, showCIs = FALSE, p = out)
	}

	# TODO: could add self-referential @1 loops to a, c, and e
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = out$manifests)

	same    = xmu_dot_rank(out$manifests, "."    , rank = "same")
	top     = xmu_dot_rank(out$latents  , "^a"   , rank = "min")
	bottom  = xmu_dot_rank(out$latents  , "^[ce]", rank = "max")

	label   = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, out$str, same, top, bottom, "\n}"
	)

	if(format != "current"){
		tmp = umx_set_plot_format(silent=TRUE)
		umx_set_plot_format(format)
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		umx_set_plot_format(tmp)
		if(res$visible) res$value else invisible(res$value)
	}else{
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		if(res$visible) res$value else invisible(res$value)
	}
}
 # end umxPlotACE

#' @export
plot.MxModelACE <- umxPlotACE


#' Make a graphical display of an ACE model with covariates.
#'
#' Make a graphical display of an ACE model with covariates.
#'
#' @aliases plot.MxModelACEcov
#' @param x [OpenMx::mxModel()] to plot (created by umxACE in order to inherit the MxModelACE class)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (default is 2)
#' @param means Whether to show means paths (default is FALSE). A non-ace label `name_rRcC` is drawn to `name_varC`, and the column index keeps every digit, so `expMean_r10c3` goes to `expMean_var3` and `expMean_r1c10` goes to `expMean_var10`.
#' @param std Whether to standardize the model (default is TRUE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE) 
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxACE()]
#' @references <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' # BMI ?twinData from Australian twins. 
#' # Cohort 1 Zygosity 1 == MZ females 3 == DZ females
#' data(twinData)
#' 
#' # Pick the variables. We will use base names (i.e., "bmi") and set suffix.
#' selDVs  = c("bmi")
#' selCovs = c("ht")
#' selVars = umx_paste_names(c(selDVs, selCovs), sep = "", suffixes= 1:2)
#' # Just top few pairs so example runs quickly
#' mzData = subset(twinData, zygosity == "MZFF", selVars)[1:100, ]
#' dzData = subset(twinData, zygosity == "DZFF", selVars)[1:100, ]
#' m1 = umxACEcov(selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep= "")
#' plot(m1)
#' plot(m1, std = FALSE) # don't standardize
#' }
umxPlotACEcov <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, ...) {
	model = x # just to be clear that x is a model

	if(!inherits(x, "MxModelACEcov")){
		stop("The first parameter of umxPlotACEcov must be an ACEcov model, you gave me a ", class(x))
	}

	if(std){model = umx_standardize(model)}
	# Relies on 'a' not having its dimnames stripped off...
	if(model$MZ$data$type == "raw"){
		selDVs = dimnames(model$top$a)[[1]]
		# selDVs = dimnames(model$MZ$data$observed)[[2]]
		selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end
	}else{
		stop("ACEcov has to have raw data...")
	}

	varCount = length(selDVs)*2
	nVar     = length(selDVs)
	selDVs   = selDVs[1:(nVar)]
	out      = "" ;
	latents  = c();
	parameterKeyList = omxGetParameters(model);
	for(thisParam in names(parameterKeyList) ) {
		value = parameterKeyList[thisParam]
		if(inherits(value, "numeric")) {
			value = round(value, digits)
		}
		if (grepl("^[ace]_r[0-9]+c[0-9]+", thisParam)) { # a c e
			show    = TRUE
			search  = '([ace])_r([0-9]+)c([0-9]+)'
			from    = sub(search, '\\1\\3', thisParam, perl = TRUE); # a c or e
			target  = as.numeric(sub(search, '\\2', thisParam, perl = TRUE)); # pull the row
			target  = selDVs[target]
			latents = append(latents, from)
		} else { # means probably
			if(means){
				show = TRUE
			} else {
				show = FALSE
			}
			from   = thisParam; # "one"
			# Column index is the variable. [0-9]+ keeps row 10 and column 10 as one index.
			target = sub('r([0-9]+)c([0-9]+)', 'var\\2', thisParam, perl = TRUE)
		}
		if(show){
			out = paste0(out, from, " -> ", target, " [label = \"", value, "\"]", ";\n")
		}
	}
	preOut  = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}

	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(var in selDVs) {
	   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
	}
	rankVariables = paste("\t{rank = same; ", paste(selDVs, collapse = "; "), "};\n") # {rank = same; v1T1; v2T1;}
	rankA   = paste("\t{rank = min; ", paste(grep('a'   , latents, value = T), collapse = "; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[ce]', latents, value = T), collapse = "; "), "};\n") # {rank=min; c1; e1}
	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut,
		out,
		rankVariables,
		rankA,
		rankCE, "\n}", sep=""
	)
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
} # end umxPlotACEcov

#' @export
plot.MxModelACEcov <- umxPlotACEcov

#' Plot the results of a GxE univariate test for moderation of ACE components.
#'
#' Plot GxE results (univariate environmental moderation of ACE components).
#' Options include plotting the raw and standardized graphs separately, or in a combined panel.
#' You can also set the label for the x axis (xlab), and choose the location of the legend.
#' 
#' *note*: If `gg=TRUE`, the plots are drawn in ggplot, and also returned as a `list(raw, std)` so you can edit them.
#'
#' @aliases plot.MxModelGxE
#' @param x A fitted [umxGxE()] model to plot
#' @param xlab String to use for the x label (default = NA, which will use the variable name)
#' @param location Where to plot the legend (default = "topleft")
#' see ?legend for alternatives like bottomright
#' @param separateGraphs (default = FALSE)
#' @param acergb Colors to use for plot c(a = "red", c = "green", e = "blue", tot = "black")
#' @param gg Use ggplot2 (default = TRUE)
#' @param moderatorValues If you want to pass in your own list of moderator values instead of the real ones in the data (Default = NULL)
#' @param ... Optional additional parameters
#' @return None 
#' @family Plotting functions
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxGxE()]
#' @references <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' twinData$age1 = twinData$age2 = twinData$age
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1= umxGxE(selDVs= "bmi", selDefs= "age", dzData= dzData, mzData= mzData, sep="", tryHard="yes")
#' plot(m1)
#' # Directly call umxPlotGxE
#' umxPlotGxE(m1, xlab = "Age", separateGraphs = TRUE, gg = FALSE)
#' umxPlotGxE(m1, moderatorValues=18:67)
#' }
#' 
umxPlotGxE <- function(x, xlab = NA, location = "topleft", separateGraphs = FALSE, acergb = c("red", "green", "blue", "black"), gg = TRUE, moderatorValues= NULL, ...) {
	if(!class(x)[[1]] == "MxModelGxE"){
		stop("The first parameter of umxPlotGxE must be a GxE model, you gave me a ", class(x)[[1]])
	}
	model = x # to remind us that x has to be a umxGxE model
	mzData = model$MZ$data$observed
	dzData = model$DZ$data$observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = sub("(_T)?[0-9]$", "", selDefs[1])
	}
	# Get unique values of moderator
	mz1 = as.vector(mzData[,selDefs[1]])
	mz2 = as.vector(mzData[,selDefs[2]])
	dz1 = as.vector(dzData[,selDefs[1]])
	dz2 = as.vector(dzData[,selDefs[2]])

	if(is.null(moderatorValues)){
		defVarValues = sort(unique(c(mz1,mz2,dz1,dz2)))
	} else {
		defVarValues = moderatorValues
	}

	a   = model$top$matrices$a$values
	c   = model$top$matrices$c$values
	e   = model$top$matrices$e$values
	am  = model$top$matrices$am$values
	cm  = model$top$matrices$cm$values
	em  = model$top$matrices$em$values

	tmp  = summary(model)$parameters[, c("name", "Estimate", "Std.Error")]
	aSE  = ifelse("a_r1c1"  %in% tmp$name, tmp[tmp$name=="a_r1c1" , "Std.Error"], 0)
	cSE  = ifelse("c_r1c1"  %in% tmp$name, tmp[tmp$name=="c_r1c1" , "Std.Error"], 0)
	eSE  = ifelse("e_r1c1"  %in% tmp$name, tmp[tmp$name=="e_r1c1" , "Std.Error"], 0)
	amSE = ifelse("am_r1c1" %in% tmp$name, tmp[tmp$name=="am_r1c1", "Std.Error"], 0)
	cmSE = ifelse("cm_r1c1" %in% tmp$name, tmp[tmp$name=="cm_r1c1", "Std.Error"], 0)
	emSE = ifelse("em_r1c1" %in% tmp$name, tmp[tmp$name=="em_r1c1", "Std.Error"], 0)

	Va  = (c(a) + c(am) * defVarValues)^2
	Vc  = (c(c) + c(cm) * defVarValues)^2
	Ve  = (c(e) + c(em) * defVarValues)^2

	VaUpper  = (c(a + 1.96*aSE) + c(am + 1.96*amSE) * defVarValues)^2
	VcUpper  = (c(c + 1.96*cSE) + c(cm + 1.96*cmSE) * defVarValues)^2
	VeUpper  = (c(e + 1.96*eSE) + c(em + 1.96*emSE) * defVarValues)^2
	VaLower  = (c(a - 1.96*aSE) + c(am - 1.96*amSE) * defVarValues)^2
	VcLower  = (c(c - 1.96*cSE) + c(cm - 1.96*cmSE) * defVarValues)^2
	VeLower  = (c(e - 1.96*eSE) + c(em - 1.96*emSE) * defVarValues)^2

	Vt  = Va + Vc + Ve
	out    = as.matrix(cbind(Va, Vc, Ve, Vt, VaUpper, VaLower, VcUpper, VcLower, VeUpper, VeLower))
	outStd = as.matrix(cbind(Va = Va/Vt, Vc = Vc/Vt, Ve = Ve/Vt))

	if(gg){
		if(length(location) == 2){
		}else if (location == "topleft"){
			location = c(.1, .9)
		} else if(!(location %in% c("none", "left", "right", "bottom", "top"))){
			message('For gg, location of legend must be in "left", "right", "bottom", "top" or, e.g. c(.1, .9)')			
		}
		tmp = data.frame(cbind(defVarValues, out))
		p = ggplot(data = tmp)
		# CI bounds are NA when standard errors are unavailable: draw each ribbon only
		# over finite bounds, so missing CIs never trigger "removed rows" warnings
		ribbonIfFinite = function(ymin, ymax, ...) {
			keep = is.finite(ymin) & is.finite(ymax)
			if(!any(keep)) return(NULL)
			geom_ribbon(aes(x = defVarValues[keep], ymin = ymin[keep], ymax = ymax[keep]), ...)
		}
		if(!any(is.finite(c(VeLower, VeUpper, VcLower, VcUpper, VaLower, VaUpper)))) {
			message("umxPlotGxE: CIs not drawn: standard errors are not available")
		}
		p = p + ribbonIfFinite(VeLower, VeUpper, alpha = .2, fill = "blue" , show.legend= FALSE, linetype= 0)
		p = p + ribbonIfFinite(VcLower, VcUpper, alpha = .2, fill = "green", show.legend= FALSE, linetype= 0)
		p = p + ribbonIfFinite(VaLower, VaUpper, alpha = .2, fill = "red"  , show.legend= FALSE, linetype= 0)
		# p = p + geom_line(aes(x=defVarValues, y = Vt, group = 4, colour = 'Vtot'))
		p = p + geom_line(aes(x=defVarValues, y = Va, group = 1, colour = 'Va'))
		p = p + geom_line(aes(x=defVarValues, y = Vc, group = 2, colour = 'Vc'))
		p = p + geom_line(aes(x=defVarValues, y = Ve, group = 3, colour = 'Ve'))
		p = p + labs(x = xlab, y = 'Raw Variance', title = "Raw Moderation effects")
		raw = p + theme(legend.title = element_blank() ) + theme(legend.background = element_blank() ) + theme(legend.position = location)


		tmp = data.frame(cbind(defVarValues, outStd))
		p = ggplot(data = tmp) + ggplot2::coord_cartesian(ylim = c(0, 1))
		p = p + geom_line(aes(x=defVarValues, y = Va, group = 1, colour = 'Va'))
		p = p + geom_line(aes(x=defVarValues, y = Vc, group = 2, colour = 'Vc'))
		p = p + geom_line(aes(x=defVarValues, y = Ve, group = 3, colour = 'Ve'))
		p = p + labs(x = xlab, y = 'Standardized Variance', title = "Standardized Moderation effects")
		std = p + theme(legend.title = element_blank() ) + theme(legend.background = element_blank() ) + theme(legend.position = location)

		tmp = list(std, raw)
		if(separateGraphs){
			print(ggdraw(std))
			print(ggdraw(raw))
		}else{
			print(plot_grid(plotlist=tmp))
		}
		invisible(tmp)
		
	} else {
		if(!separateGraphs){
			graphics::par(mfrow = c(1, 2)) # one row * two columns to hold raw and std plots
		}
		graphics::matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = acergb, xlab = xlab, ylab = "Variance", main= "Raw Moderation Effects")
		graphics::legend(x = location, legend = c("genetic", "shared", "unique", "total"), lty = 1:4, col = acergb, bty = "n")
	
		graphics::matplot(defVarValues, outStd, type = "l", lty = 1:4, col = acergb, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main= "Standardized Moderation Effects")
		graphics::legend(x = location, legend = c("genetic", "shared", "unique"), lty = 1:4, col = acergb, bty = "n")
		graphics::par(mfrow = c(1, 1)) # back to black
	
	}
	
}

#' @export
plot.MxModelGxE <- umxPlotGxE

# TODO: umxPlotCPnew Add SEstyle code from plotCP
# TODO: umxPlotCPnew Add new label trimming code if necessary
#' Draw and display a graphical figure of Common Pathway model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' @aliases plot.MxModelCP
#' @param x The [umxCP()] Common Pathway to display graphically
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param showFixed Whether to graph paths that are fixed but != 0 (default = TRUE)
#' @param SEstyle use mxSE "b (se)" instead of mxCI "b \[lower, upper\]" (Default FALSE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param showCIs Whether to show confidence intervals (default = TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxCP()]
#' @family Plotting functions
#' @references <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
# # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP("new", selDVs = selDVs, sep = "_T", 
#' 	dzData = dzData, mzData = mzData, nFac = 3
#' )
#' # m1 = mxTryHardOrdinal(m1)
#' umxPlotCP(m1)
#' plot(m1) # No need to remember a special name: plot works fine!
#' }
umxPlotCP <- function(x = NA, means = FALSE, std = TRUE, digits = 2, showFixed = TRUE, file = "name", format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, showCIs = TRUE, ...) {
	format = match.arg(format)
	model  = x # just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelCP", callingFn = "umxPlotCP")

	if(std){ model = xmu_standardize_CP(model) }
		
	nFac   = dim(model$top$a_cp$labels)[[1]]
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())
	out = xmu_dot_mat2dot(model$top$a_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$c_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$e_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)

	# 2. Factor correlations on the lower
	# from = "<name><rowNum>"; target = "<name><colNum>"
	out = xmu_dot_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)

	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = xmu_dot_mat2dot(model$top$cp_loadings, cells= "any", toLabel= selDVs, from= "cols", fromLabel= "common", fromType= "latent", showFixed = showFixed, p= out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)

	# Process "as" matrix
	out = xmu_dot_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	}
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace]_cp", "min")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s[0-9]+$", "max")
	
	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, 
		top, bottom, 
		out$str, "\n}"
	)
	
	if(format != "current"){
		tmp = umx_set_plot_format(silent=TRUE)
		umx_set_plot_format(format)
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		umx_set_plot_format(tmp)
		if(res$visible) res$value else invisible(res$value)
	}else{
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		if(res$visible) res$value else invisible(res$value)
	}
}

#' @export
plot.MxModelCP <- umxPlotCP

#' Draw a graphical figure for a Independent Pathway model
#'
#' Options include digits (rounding), showing means or not, standardization, and which output format is desired.
#'
#' @aliases plot.MxModelIP
#' @param x The [umxIP()] model to plot
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param showFixed Whether to graph paths that are fixed but != 0 (default = TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR")
#' @param SEstyle Report "b (se)" instead of "b \[lower, upper\]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param showCIs Whether to show confidence intervals (default = TRUE)
#' @param ... Optional additional parameters
#' @return - optionally return the dot code
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxIP()]
#' @family Plotting functions
#' @references <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 =    umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' plot(model)
#' umxPlotIP(model, file = NA)
#' }
umxPlotIP <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, showFixed = TRUE, format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, showCIs = TRUE, ...) {
	format = match.arg(format)
	model = x # Just to emphasize that x has to be a model 
	umx_check_model(model, "MxModelIP", callingFn = "umxPlotIP")
	
	if(std){
		model = xmu_standardize_IP(model)
	}

	# 1. get vars from rows of as matrix
	nFac   = dim(model$top$ai$labels)[[2]] # added! (but won't work, right? can have different numbers of a c, e )
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())

	# 1. Collect ai (the independent latent factors)
	out = xmu_dot_mat2dot(model$top$ai, cells = "any", toLabel = selDVs, from = "cols", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$ci, cells = "any", toLabel = selDVs, from = "cols", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$ei, cells = "any", toLabel = selDVs, from = "cols", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)


	# 2 collect as (the specific latent factors)
	out = xmu_dot_mat2dot(model$top$as, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$cs, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	out = xmu_dot_mat2dot(model$top$es, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)

	# Process "expMean" 1 * nVar matrix e.g. "expMean_gff_T1"
	if(means){
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out, model = model, SEstyle = SEstyle, digits = digits, showCIs = showCIs)
	}
	
	# TODO: Could extract thresholds? "_dev[0-9]+$"

	preOut = "\t# Latents\n"
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace]i", "min")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s", "max")

	label = model$name
	splines = "FALSE"

	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, 
		top, 
		bottom, 
		out$str, "\n}"
	)

	if(format != "current"){
		tmp = umx_set_plot_format(silent=TRUE)
		umx_set_plot_format(format)
		xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
		umx_set_plot_format(tmp)
	}else{
		xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)		
	}
}

#' @export
plot.MxModelIP <- umxPlotIP

#' Report modifications which would improve fit.
#'
#' This function uses the mechanical modification-indices approach to detect single paths which, if added
#' or dropped, would improve fit.
#'
#' Notes:
#' 1. Default `full = FALSE` only moves the parameter under test (fast screen). Set `full = TRUE` to
#'    allow all free parameters to re-adjust (slower; closer to a full re-fit for each candidate).
#' 2. Compared to mxMI, this function returns top changes, and also suppresses the run message.
#' 3. Finally, of course: see the requirements for (legitimate) post-hoc modeling in [OpenMx::mxMI()]
#' You are almost certainly doing better science when testing competing models rather than modifying a model to fit.
#' @param model An [OpenMx::mxModel()] for which to report modification indices
#' @param matrices which matrices to test. The default (NA) will test A & S for RAM models
#' @param full If FALSE (default), only the parameter under test can move. If TRUE, all free parameters re-adjust (much slower).
#' @param numInd How many modifications to report. Use -1 for all. Default (NA) will report all over 6.63 (p = .01)
#' @param typeToShow Whether to shown additions or deletions (default = "both")
#' @param decreasing How to sort (default = TRUE, decreasing)
#' @seealso - [OpenMx::mxMI()]
#' @family Model Summary and Comparison
#' @references <https://github.com/tbates/umx>
#' @export
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' umxMI(m1)
#' umxMI(m1, full = TRUE)  # slower: allow all parameters to re-adjust
#' }
umxMI <- function(model = NA, matrices = NA, full = FALSE, numInd = NA, typeToShow = "both", decreasing = TRUE) {
	if(typeToShow != "both"){
		message("Only showing both add and remove is supported so far")
	}
	if(is.na(matrices)){
		if(umx_is_RAM(model)){
			matrices = c("A", "S")
		}else{
			message("You need to tell me which matrices to test (this is not a RAM model, so I don't know.)")
		}
	}
	suppressMessages({MI = mxMI(model = model, matrices = matrices, full = full)})
	if(typeof(MI)[1]=="list"){
		# old style
		if(full){
			MIlist = MI$MI.Full
		} else {
			MIlist = MI$MI
		}
	} else {
		# dataframe for Nov 2023 OpenMx 6b2cc13d1d17843b505bf4fb295c73bb8a1212b5
		if(full){
			MIlist = MI[, "MI.Full"]
		} else {
			MIlist = MI[, "MI"]
		}
	}
	if(is.na(numInd)){
		thresh = qchisq(p = (1 - 0.01), df = 1) # 6.63
		# check how many sig
		nSig = length(MIlist[MIlist > thresh])
		if(nSig < 1){
			# nothing significant, display top 3 or so
			mostPossible = length(MIlist)
			numInd = min(3, mostPossible)
			suggestions = sort(MIlist, decreasing = decreasing)[1:numInd]
		} else {
			suggestions = sort(MIlist[MIlist > thresh], decreasing = decreasing)
		}		
	} else {
		suggestions = sort(MIlist, decreasing = decreasing)[1:numInd]
	}
	print(suggestions)
	invisible(MI)
}

# ======================
# = Path tracing rules =
# ======================
#' umxUnexplainedCausalNexus
#'
#' umxUnexplainedCausalNexus report the effect of a change (delta) in a variable (from) on an output (to)
#'
#' @param from A variable in the model for which you want to compute the effect of a change.
#' @param delta A the amount to simulate changing \sQuote{from} by. 
#' @param to The dependent variable that you want to watch changing.
#' @param model The model containing variables from and to.
#' @seealso - [OpenMx::mxCheckIdentification()], [umxCompare()]
#' @family Advanced Model Building Functions
#' @references https://github.com/tbates/umx/
#' @export
#' @examples
#' \dontrun{
#' umxUnexplainedCausalNexus(from="yrsEd", delta = .5, to = "income35", model)
#' }
umxUnexplainedCausalNexus <- function(from, delta, to, model= NULL) {
	umx_check_model(model, type = "RAM")
	
	manifests = model@manifestVars
	partialDataRow = matrix(0, 1, length(manifests))  # add dimnames to allow access by name
	dimnames(partialDataRow) = list("val", manifests)
	partialDataRow[1, from]  = delta # delta is in raw "from" units
	partialDataRow[1, to]    = NA
	completedRow = umxConditionalsFromModel(model, partialDataRow, meanOffsets = TRUE)
	# by default, meanOffsets = FALSE, and the results take expected means into account
	return(completedRow[1, to])
}

umxConditionalsFromModel <- function(model, newData = NULL, returnCovs = FALSE, meanOffsets = FALSE) {
	# Usage: umxConditionalsFromModel(model, newData)
	# Called by: umxUnexplainedCausalNexus
	# TODO: Special case for latent variables
	expectation = model$objective
	A = NULL
	S = NULL
	M = NULL
	
	# Handle missing data
	if(is.null(newData)) {
		data = model$data
		if(data$type != "raw") {
			stop("Conditionals requires either new data or a model with raw data.")
		}
		newData = data$observed
	}
	
	# New fit-function style
	eCov  = model$fitfunction$info$expCov
	eMean = model$fitfunction$info$expMean
	expectation = model$expectation
	if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
		A = eval(substitute(model$X$values, list(X=expectation$A)))
		S = eval(substitute(model$X$values, list(X=expectation$S)))
		if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation$M)) {
			M = eval(substitute(model$X$values, list(X=expectation$M)))
		}
	}

	if(!is.null(A)) {
		# RAM model: calculate total expectation
		I = diag(nrow(A))
		Z = solve(I-A)
		eCov = Z %*% S %*% t(Z)
		if(!is.null(M)) {
			eMean = Z %*% t(M)
		}
		latents = model@latentVars
		newData = data.frame(newData, matrix(NA, ncol=length(latents), dimnames=list(NULL, latents)))
	}
	
	# No means
	if(meanOffsets || !dim(eMean)[1]) {
		eMean = matrix(0.0, 1, ncol(eCov), dimnames=list(NULL, colnames(eCov)))
	}
	
	# TODO: Sort by pattern of missingness, lapply over patterns
	nRows = nrow(newData)
	outs = omxApply(newData, 1, umxComputeConditionals, sigma=eCov, mu=eMean, onlyMean=!returnCovs)
	if(returnCovs) {
		means = matrix(NA, nrow(newData), ncol(eCov))
		covs = rep(list(matrix(NA, nrow(eCov), ncol(eCov))), nRows)
		for(i in 1:nRows) {
			means[i,] = outs[[i]]$mu
			covs[[i]] = outs[[i]]$sigma
		}
		return(list(mean = means, cov = covs))
	}
	return(t(outs))
}

umxComputeConditionals <- function(sigma, mu, current, onlyMean = FALSE) {
	# Usage: umxComputeConditionals(model, newData)
	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	# called by umxConditionalsFromModel()
	if(dim(mu)[1] > dim(mu)[2] ) {
		mu = t(mu)
	}

	nVar = length(mu)
	vars = colnames(sigma)

	if(!is.matrix(current)) {
		current = matrix(current, 1, length(current), dimnames=list(NULL, names(current)))
	}
	
	# Check inputs
	if(dim(sigma)[1] != nVar || dim(sigma)[2] != nVar) {
		stop("Non-conformable sigma and mu matrices in conditional computation.")
	}
	
	if(is.null(vars)) {
		vars = rownames(sigma)
		if(is.null(vars)) {
			vars = colnames(mu)
			if(is.null(vars)) {
				vars = names(current)
				if(is.null(vars)) {
					vars = paste("X", 1:dim(sigma)[1], sep = "")
					names(current) = vars
				}
				names(mu) = vars
			}
			dimnames(sigma) = list(vars, vars)
		}
		rownames(sigma) = vars
	}
	
	if(is.null(colnames(sigma))) {
		colnames(sigma) = vars
	}
	
	if(is.null(rownames(sigma))) {
		rownames(sigma) = colnames(sigma)
	}

	if(!setequal(rownames(sigma), colnames(sigma))) {
		stop("Rows and columns of sigma do not match in conditional computation.")
	}
	
	if(!setequal(rownames(sigma), vars) || !setequal(colnames(sigma), vars)) {
		stop("Names of covariance and means in conditional computation fails.")
	}
	
	if(length(current) == 0) {
		if(onlyMean) {
			return(mu)
		}
		return(list(sigma=covMat, mu=current))
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			print(paste("Got data vector of length ", ncol(current), " and names of length ", length(vars)))
			stop("Length and names of current values mismatched in conditional computation.")
		}
		names(current) = vars[1:ncol(current)]
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			if(length(vars) == 0 || ncol(current) != length(vars)) {
				print(paste("Got mean vector of length ", ncol(current), " and names of length ", length(vars)))
				stop("Length and names of mean values mismatched in conditional computation.")
			}
		}
		names(mu) = vars
	}
	
	# Get Missing and Non-missing sets
	if(!setequal(names(current), vars)) {
		newSet          = setdiff(vars, names(current))
		current[newSet] = NA
		current         = current[vars]
	}
	
	# Compute Schur Complement
	# Calculate parts:
	missing    = names(current[is.na(current)])
	nonmissing = setdiff(vars, missing)
	ordering   = c(missing, nonmissing)
	
	totalCondCov = NULL

	# Handle all-missing and none-missing cases
	if(length(missing) == 0) {
		totalMean        = current
		names(totalMean) = names(current)
		totalCondCov     = sigma
	} 

	if(length(nonmissing) == 0) {
		totalMean = mu
		names(totalMean) = names(mu)
		totalCondCov = sigma
	}

	# Compute Conditional expectations
	if(is.null(totalCondCov)) {
		
		covMat   = sigma[ordering, ordering]
		missMean = mu[, missing]
		haveMean = mu[, nonmissing]

		haves    = current[nonmissing]
		haveNots = current[missing]

		missCov = sigma[missing, missing]
		haveCov = sigma[nonmissing, nonmissing]
		relCov  = sigma[missing, nonmissing]
		relCov  = matrix(relCov, length(missing), length(nonmissing))

		invHaveCov = solve(haveCov)
		condMean   = missMean + relCov %*% invHaveCov %*% (haves - haveMean)

		totalMean             = current * 0.0
		names(totalMean)      = vars
		totalMean[missing]    = condMean
		totalMean[nonmissing] = current[nonmissing]
	}

	if(onlyMean) {
		return(totalMean)
	}
	
	if(is.null(totalCondCov)) {
		condCov = missCov - relCov %*% invHaveCov %*% t(relCov)
	
		totalCondCov = sigma * 0.0
		totalCondCov[nonmissing, nonmissing] = haveCov
		totalCondCov[missing, missing] = condCov
	}	
	return(list(sigma = totalCondCov, mu = totalMean))
	
}

# =========================
# = Pull model components =
# =========================

#' Display path estimates from a model, filtering by name and value.
#'
#' @description
#' Often you want to see the estimates from a model, and often you don't want all of them.
#' [umxParameters()] helps in this case, allowing you to select parameters matching a name filter,
#' and also to only show parameters above or below a certain value.
#' 
#' If pattern is a vector, each regular expression is matched, and all unique matches to the whole vector are returned.
#'
#' @details
#' It is on my TODO list to implement filtering by significance, and to add standardizing.
#'
#' @param x an [OpenMx::mxModel()] or model summary from which to report parameter estimates.
#' @param thresh optional: Filter out estimates 'below' or 'above' a certain value (default = "all").
#' @param b Combine with thresh to set a minimum or maximum for which estimates to show.
#' @param pattern Optional string to match in the parameter names. Default '.*' matches all. [regex()] allowed!
#' @param std Standardize output: NOT IMPLEMENTED YET
#' @param digits Round to how many digits (2 = default).
#' @return - list of matching parameters, filtered by name and value
#' @export
#' @family Reporting Functions
#' @seealso - [umxGetParameters()], [umxSummary()], [namez()]
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("OneFactor", data = demoOneFactor,
#' 	umxPath(from = "G", to = manifests), # factor loadings
#' 	umxPath(v.m. = manifests),           # residual variance
#' 	umxPath(v1m0 = "G")                  # standardized latent
#' )
#' # Parameters with values below .1
#' umxParameters(m1, "below", .1)
#' # Parameters with values above .5
#' umxParameters(m1, "above", .5)
#' # Parameters with values below .1 and containing "_to_" in their label
#' umxParameters(m1, "below", .1, "_to_")
#' }
#' 
umxParameters <- function(x, thresh = c("all", "above", "below", ">", "<", "NS", "sig"), b = NULL, pattern = ".*", std = FALSE, digits = 2) {
	# TODO clarify when to use parameters vs. umxGetParameters
	# TODO Add filtering by significance (based on SEs)
	# TODO Offer a method to handle sub-models
	# 	model$aSubmodel$matrices$aMatrix$labels
	# 	model$MZ$matrices
	
	# x = cp4
	if(inherits(thresh, "numeric")){
		stop("You might not have specified the parameter value (b) by name. e.g.:\n
	parameters(cp4, pattern = '_cp_', thresh = 'below', b = .1)\n
or specify all arguments:\n
	parameters(cp4, 'below', .1, '_cp_')
		")
	}
	thresh = match.arg(thresh)

	if(!is.null(b) && (thresh == "all")){
		message("Ignoring b (cutoff) and thresh = all. Set above or below to pick a beta to cut on.")
	}

	if(std){
		x = umx_standardize(x)
	}

	if(!inherits(x, "summary.mxmodel")){
		if(!umx_has_been_run(x)){
			# message("Just a note: Model has not been run. That might not matter for you")
		}
		# model that may or may not have been run: make up a similar dataframe to what we get from summary$parameters
		x = omxGetParameters(x)
		x = data.frame(name = names(x), Estimate = as.numeric(x), stringsAsFactors = FALSE)
	} else {
		x = x$parameters
	}

	# Handle 1 or more regular expressions.
	parList = c()
	for (i in 1:length(pattern)) {
		parList = c(parList, umx_names(x$name, pattern = pattern[i]))
	}
	parList = unique(parList)
	
	if(thresh %in%  c("above", ">") ){
		filter = x$name %in% parList & abs(x$Estimate) > b
	} else if(thresh %in% c("below", "<")){
		filter = x$name %in% parList & abs(x$Estimate) < b
	} else if(thresh == "all"){
		filter = x$name %in% parList
	} else if(thresh == "NS"){
		stop("NS not yet implemented in 'parameters' : email maintainer('umx') to get this done.")
	} else if(thresh == "sig"){
		stop("sig not yet implemented in 'parameters': email maintainer('umx') to get this done.")
	}

	if(sum(filter) == 0){
		message(paste0("Nothing found matching pattern ", omxQuotes(pattern), " and minimum absolute value ", thresh, " ", b, "."))
		
		paste0("Might try flipping the from and to elements of the name, or look in these closest matches for what you intended: ",
			omxQuotes(agrep(pattern = pattern, x = x$name, max.distance = 4, value = TRUE))
		)
	} else {
		umx_round(x[filter, c("name", "Estimate")], digits = digits)
	}
}

#' @rdname umxParameters
#' @export
parameters <- umxParameters

#' Get parameters from a model, with support for pattern matching!
#'
#' umxGetParameters retrieves parameter labels from a model, like [OpenMx::omxGetParameters()].
#' However, it is supercharged with regular expressions, so you can get labels that match a pattern.
#' 
#' In addition, if regex contains a vector, this is treated as a list of raw labels to search for, 
#' and return if all are found.
#' \emph{note}: To return all labels, just leave regex as is.
#'
#' @param inputTarget An object to get parameters from: could be a RAM [OpenMx::mxModel()]
#' @param regex A regular expression to filter the labels. Default (NA) returns all labels. If vector, treated as raw labels to find.
#' @param free  A Boolean determining whether to return only free parameters.
#' @param fetch What to return: "labels" (default) or "values", "free", "lbound", "ubound", or "all"
#' @param verbose How much feedback to give
#' @export
#' @seealso [OpenMx::omxGetParameters()], [parameters()]
#' @family Reporting Functions
#' @references <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' 
#' # Show all parameters
#' umxGetParameters(m1)
#' umxGetParameters(m1, free = TRUE)  # Only free parameters
#' umxGetParameters(m1, free = FALSE) # Only fixed parameters
#' # Complex regex pattern
#' umxGetParameters(m1, regex = "x[1-3]_with_x[2-5]", free = TRUE)
#' }
#' 
umxGetParameters <- function(inputTarget, regex = NA, free = NA, fetch = c("labels", "values", "free", "lbound", "ubound", "all"), verbose = FALSE) {
	# TODO
	# 1. Be nice to offer a method to handle sub-models
	# 	model$aSubmodel$matrices$aMatrix$labels
	# 	model$MZ$matrices
	# 2. Simplify handling
		# allow umxGetParameters to function like omxGetParameters()[name filter]
	# 3. Allow user to request values, free, etc. (already done with umx_parameters)
	fetch = match.arg(fetch)
	umx_check(fetch=="labels", action="stop",message="polite stop: You asked for ", omxQuotes(fetch), ". umxGetParameters only supports fetching labels at present: Make an issue on github to support fetching these, or try parameters() ")
	if(umx_is_MxModel(inputTarget)) {
		topLabels = names(omxGetParameters(inputTarget, indep = FALSE, free = free))
	} else if(methods::is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget$labels
		} else {
			topLabels = inputTarget$labels[inputTarget$free==free]
		}
	}else{
		stop("I am sorry Dave, umxGetParameters needs either a model or an mxMatrix: you offered a ", class(inputTarget)[1])
	}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( length(regex) > 1 || !is.na(regex) ) {
		if(length(regex) > 1){
			# Assume regex is a list of labels
			theLabels = theLabels[theLabels %in% regex]
			if(length(regex) != length(theLabels)){
				msg = "Not all labels found! Missing were:\n"
				stop(msg, regex[!(regex %in% theLabels)]);
			}
		} else {
			# It's a grep string
			theLabels = grep(regex, theLabels, perl = FALSE, value = TRUE) # Return more detail
		}
		if(length(theLabels) == 0){
			msg = paste0("Found no labels matching", omxQuotes(regex), "!\n")
			if(umx_is_MxModel(inputTarget)){
				msg = paste0(msg, "\nUse umxGetParameters(", deparse(substitute(inputTarget)), ") to see all parameters in the model")
			}else{
				msg = paste0(msg, "\nUse umxGetParameters() without a pattern to see all parameters in the model")
			}
			stop(msg);
		}
	}
	return(theLabels)
}


#' Extract AIC from MxModel
#'
#' Returns the AIC for an OpenMx model.
#' Original Author: Brandmaier
#'
#' @method extractAIC MxModel
#' @rdname extractAIC.MxModel
#' @export
#' @param fit an fitted [OpenMx::mxModel()] from which to get the AIC
#' @param scale not used
#' @param k not used
#' @param ... any other parameters (not used)
#' @return - AIC value
#' @seealso - [AIC()], [umxCompare()], [logLik()]
#' @family Reporting functions
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' extractAIC(m1)
#' # -2.615998
#' AIC(m1)
#' }
extractAIC.MxModel <- function(fit, scale, k, ...) {
	a = mxCompare(fit)
	return(a[1, "AIC"])
}

#' Get the expected vcov matrix
#'
#' Extract the expected covariance matrix from an [OpenMx::mxModel()]
#'
#' @aliases vcov.MxModel
#' @param object an [OpenMx::mxModel()] to get the covariance matrix from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. NULL (Default) = no rounding.
#' @param ... extra parameters (to match [vcov()])
#' @return - expected covariance matrix
#' @export
#' @family Reporting functions
#' @seealso - [umxRun()], [umxCI_boot()]
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )#'
#' vcov(m1) # supplied by OpenMx
#' umxExpCov(m1, digits = 3)
#' 
#' }
umxExpCov <- function(object, latents = FALSE, manifests = TRUE, digits = NULL, ...){
	# umx_has_been_run(m1)
	# TODO integrate with mxGetExpected(model, "covariance")
	# mxGetExpected(m1, component= c("means", "covariance", "standVector") )
	if(object$data$type == "raw"){
		manifestNames = names(object$data$observed)
	} else {
		manifestNames = dimnames(object$data$observed)[[1]]
	}
	if(umx_is_RAM(object)){
		if(manifests & !latents){
			thisFit = paste0(object$name, ".fitfunction")
			expCov  = attr(object$output$algebras[[thisFit]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		} else {
			A = mxEval(A, object)
			S = mxEval(S, object)
			I = diag(1, nrow(A))
			E = solve(I - A)
			expCov = E %&% S # The model-implied covariance matrix
			mV = NULL
			if(latents) {
				mV = object@latentVars 
			}
			if(manifests) {
				mV = c(mV, object@manifestVars)
			}
			expCov = expCov[mV, mV]
		}
	} else {
		if(latents){
			stop("I don't know how to reliably get the latents for non-RAM objects... Sorry :-(")
		} else {
			expCov = attr(object$output$algebras[[paste0(object$name, ".fitfunction")]], "expCov")
			dimnames(expCov) = list(manifestNames, manifestNames)
		}
	}
	if(!is.null(digits)){
		expCov = round(expCov, digits)
	}
	return(expCov) 
}

# #' @export
# vcov.MxModel <- umxExpCov


#' Extract the expected means matrix from an [OpenMx::mxModel()]
#'
#' Extract the expected means matrix from an [OpenMx::mxModel()]
#'
#' @param model an [OpenMx::mxModel()] to get the means from
#' @param latents Whether to select the latent variables (defaults to TRUE)
#' @param manifests Whether to select the manifest variables (defaults to TRUE)
#' @param digits precision of reporting. Default (NULL) will not round at all.
#' @return - expected means
#' @export
#' @family Reporting functions
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor,
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G")
#' )
#'
#' umxExpMeans(m1)
#' umxExpMeans(m1, digits = 3)
#' }
umxExpMeans <- function(model, manifests = TRUE, latents = NULL, digits = NULL){
	umx_check_model(model, beenRun = TRUE)
	if(!umx_has_means(model)){
		stop("Model has no means expectation to get: Are there any means in the data? (type='raw', or type = 'cov' with means?)")
	}
	
	if(umx_is_RAM(model)){
		# TODO something nice to do here?
	}
	if(!is.null(latents)){
		# TODO should a function called expMeans get expected means for latents... why not.
		stop("Haven't thought about getting means for latents yet... Bug me about it :-)")
	}
	expMean = attr(model$output$algebras[[paste0(model$name, ".fitfunction")]], "expMean")
	
	if(model$data$type == "raw"){
		manifestNames = names(model$data$observed)
	} else {
		manifestNames = dimnames(model$data$observed)[[1]]
	}
	dimnames(expMean) = list("mean", manifestNames)
	if(!is.null(digits)){
		expMean = round(expMean, digits)
	}
	return(expMean)
}


# Define generic RMSEA...
#' Generic RMSEA function
#'
#' See [RMSEA.MxModel()] to access the RMSEA of MxModels
#'
#' @param x an object from which to get the RMSEA 
#' @param ci.lower the lower CI to compute
#' @param ci.upper the upper CI to compute
#' @param digits digits to show
#' @return - RMSEA object containing value (and perhaps a CI)

#' @export
#' @family Reporting functions
RMSEA <- function(x, ci.lower, ci.upper, digits) UseMethod("RMSEA", x)

#' RMSEA function for MxModels
#'
#' Return RMSEA and its confidence interval on a model.
#' RMSEA(tmp, silent=TRUE)$RMSEA
#'
#' @rdname RMSEA.MxModel
#' @param x an [OpenMx::mxModel()] from which to get RMSEA
#' @param ci.lower the lower CI to compute (only 95%, i.e., .025 supported)
#' @param ci.upper the upper CI to compute (only 95%, i.e., .975 supported)
#' @param digits digits to show (default = 3)
#' @return - object containing the RMSEA, lower and upper bounds, and p-close
#' @export
#' @family Reporting functions
#' @references <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' RMSEA(m1)
#' 
#' x = RMSEA(m1)
#' x$RMSEA # 0.0309761
#'
#' # Raw: needs to be run by umx to get RMSEA
#' m2 = umxRAM("One Factor", data = demoOneFactor,
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G")
#' )
#' RMSEA(m2)
#' }
RMSEA.MxModel <- function(x, ci.lower = .025, ci.upper = .975, digits = 3) { 
	model = x
	if(is.null(model$output$SaturatedLikelihood)){
		# no ref models in summary... compute them
		# no SaturatedLikelihood, compute refModels
		refModels = tryCatch({
		    xmu_mxRefModels(model, run = TRUE, beginMessage = FALSE)
		}, error = function(x) {
		    print("error RMSEA.MxModel calling mxRefModels")
		    NULL
		})
		if(!inherits(refModels, "list")){
			modelSummary = summary(model)
		} else {
			modelSummary = summary(model, refModels = refModels)
		}
	}else{
		modelSummary = summary(model)		
	}
	RMSEA.summary.mxmodel(x= modelSummary, ci.lower = ci.lower, ci.upper = ci.upper, digits = digits)
}

#' RMSEA function for MxModel summary
#'
#' Extract the RMSEA and confidence interval from a model summary and returns it as an RMSEA object.
#' To report just the RMSEA, you can use `RMSEA(model)$RMSEA`
#' 
#' @param x an [OpenMx::mxModel()] summary from which to get RMSEA
#' @param ci.lower the lower CI to compute (only 95% CI (.025) is implemented)
#' @param ci.upper the upper CI to compute (only 95% CI (.975) is implemented)
#' @param digits The number of digits to round data (defaults to 3)
#' @return - object containing the RMSEA and lower and upper bounds
#' @rdname RMSEA.summary.mxmodel
#' @export
#' @family Reporting functions
#' @references <https://github.com/simsem/semTools/wiki/Functions>, <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor[1:100,], type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' tmp = summary(m1)
#' RMSEA(tmp)
#' }
RMSEA.summary.mxmodel <- function(x, ci.lower = .025, ci.upper = .975, digits = 3){
	summary = x # x is a model summary
	if(ci.lower != .025 | ci.upper != .975){
		stop("Polite note: Only 95% CI on RMSEA supported as yet, submit a request to http://github.com/OpenMx/OpenMx/issues for this feature.")
	}
	if(is.na(summary$RMSEA) || is.null(summary$RMSEA)){
		message("Polite note: Model summary has no RMSEA - you might need to run RMSEA on the model, or run umxSummary or mxRefModels models first?")
		return(NA)
	} else {
		output = list(RMSEA = summary$RMSEA, CI.lower = summary$RMSEACI["lower"], CI.upper = summary$RMSEACI["upper"], RMSEA.pvalue = summary$RMSEAClose)
		class(output) = "RMSEA"
		attr(output, 'digits')   = digits
		return(output)
	}
}

#' Print a RMSEA object
#'
#' Print method for "RMSEA" objects: e.g. [umx::RMSEA()]. 
#'
#' @param x RMSEA object.
#' @param ... further arguments passed to or from other methods.
#' @return - invisible
#' @seealso - [umx::RMSEA()], [print()]

#' @method print RMSEA
#' @export
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type= "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' tmp = summary(m1)
#' RMSEA(tmp)
#' }
#'
print.RMSEA <- function(x, ...) {
	# x is an RMSEA object
	if(!is.null(attr(x, 'digits')) ){
		digits = attr(x, 'digits')
	}

	txt = paste0("RMSEA = ", round(x$RMSEA, digits))
	txt = paste0(txt, " CI[")
	txt = paste0(txt, round(x$CI.lower, digits), ", ")
	txt = paste0(txt, round(x$CI.upper, digits), "], ")
	txt = paste0(txt, "Prob(RMSEA <= 0.05) = ", umx_APA_pval(x$RMSEA.pvalue))
	cat(txt)
}

# ===================================
# = Summary Stats and table helpers =
# ===================================

#' Fishers Method of combining p-values.
#'
#' @description
#' `FishersMethod` implements R.A. Fisher's (1925) method for creating a meta-analytic p-value by combining a 
#' set of p-values from tests of the same hypothesis in independent samples. See also Stouffer's method 
#' for combining Z scores, which allows weighting.
#' @param pvalues A vector of p-values, e.g. c(.041, .183)
#' @param ... More p-values if you want to offer them up one by one instead of wrapping in a vector for `pvalues`
#' @return - A meta-analytic p-value
#' @export
#' @family Miscellaneous Stats Functions
#' @references Fisher, R.A. (1925). *Statistical Methods for Research Workers*. Oliver and Boyd (Edinburgh). ISBN 0-05-002170-2.
#' * Fisher, R. A (1948). "Questions and answers #14". *The American Statistician*. **2**: 30-31. \doi{10.2307/2681650}.
#' * Stouffer, S. A. and Suchman, E. A. and DeVinney, L. C. and Star, S. A. and Williams, R. M. Jr. (1949) The American Soldier, 
#' Vol. 1 - Adjustment during Army Life. Princeton, Princeton University Press.
#' @examples
#' FishersMethod(c(.041, .378))
FishersMethod <- function(pvalues, ...){
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	pvalues = c(pvalues, unlist(dot.items))
	pchisq( -2 * sum(log(pvalues)), df= (2 * length(pvalues)), lower.tail = FALSE)
}

#' Geometric Mean
#'
#' @description
#' The Geometric mean is the nth-root of the product of \code{n} input values.
#' Common uses include: 
#' * Compound Annual Growth Rate (CAGR) & Financial Returns: Averaging investment yields over multiple periods. Enter each period as a multiplier, `1` plus the return: a 30% loss is `0.7`, not `-0.3`. The arithmetic mean overstates performance when volatility is present. A +50% gain followed by a -50% loss is `c(1.5, 0.5)`. You finish at 75% of the start, a 25% loss, while the arithmetic mean of the two returns is 0%. The geometric mean of the multipliers is `sqrt(1.5 * 0.5) = 0.866`, which is -13.4% per period. Two periods at that rate reproduce the 25% loss.
#' * Proportional Scaling: Proportional growth of organisms and log-normally distributed physical phenomena.
#' * Normalizing metrics measured on different scales: e.g., computer hardware so no single benchmark disproportionately skews the score.
#' * Utilitarian evaluation of value. For example, the geometric mean utility of `c(1, 2, 10)` is \deqn{(1 * 2 * 10)^\frac{1}{3}} = 2.7 not the arithmetic mean: 4.3.
#'
#' @details
#' The geometric mean is defined as the \eqn{n}-th root of the product of \eqn{n} numbers:
#' \deqn{G = \left( \prod_{i=1}^{n} x_i \right)^{\frac{1}{n}} = \sqrt[n]{x_1 x_2 \cdots x_n}}{G = (prod(x_i))^(1/n)}
#'
#' In practice, to prevent numerical overflow or underflow with large vectors, 
#' it is calculated via the logarithmic transform:
#' \deqn{\ln G = \frac{1}{n} \sum_{i=1}^{n} \ln(x_i) \implies G = \exp\left( \frac{1}{n} \sum_{i=1}^{n} \ln(x_i) \right)}{G = exp(mean(log(x)))}
#'
#' @param x A vector of values.
#' @param na.rm Remove NAs (default = TRUE). If FALSE, NA input returns NA (like [mean()]).
#' @return - Geometric mean of x
#' @export
#' @seealso - [harmonic_mean()], [mean()]
#' @family Miscellaneous Stats Functions
#' @references <https://en.wikipedia.org/wiki/Geometric_mean>
#' @examples
#' geometric_mean(c(1.02, 1.03, 1.1))
#' # = 1.049
#' 
#' # For a given sum, geometric mean is maximised when all values are equal:
#' geometric_mean(c(75,75))
#' geometric_mean(c(50, 100))
#'
#' v = c(1, 149, 500); round(c(geometric_mean(v), mean(v), median(v)), 0)
#' # 42 217 149
#'
#' # Alternate form using mean of the logs
#' exp(mean(log(c(50, 100))))
#'
#' # Underlying logic
#' sqrt(50 * 100)
#' 
#' # Reciprocal duality
#' 1/geometric_mean(c(100, 50))
#' geometric_mean(c(1/100, 1/50))
#'
#' # Missing data are dropped by default (na.rm = TRUE)
#' geometric_mean(c(50, 100, NA)) # 70.7
#' # With na.rm = FALSE, NA input gives NA, like mean()
#' geometric_mean(c(50, 100, NA), na.rm = FALSE) # NA
#'
#' # A 30% gain, a 20% gain, and a 30% loss
#' geometric_mean(c(1.3, 1.2, 0.7)) # 1.030
#' \dontrun{
#' # A negative multiplier is an error. -0.3 is a -130% return, not a 30% loss.
#' geometric_mean(c(1.3, 1.2, -0.3))
#' geometric_mean(c(50, -100, NA))
#' }
geometric_mean = function(x, na.rm = c(TRUE, FALSE)){
	na.rm = xmu_match.arg(na.rm, option_list = c(TRUE, FALSE), check = TRUE)
	negative = which(x < 0)
	if (length(negative) > 0) {
		nShow = min(length(negative), 6)
		bits = paste0(x[negative][seq_len(nShow)], " at position ", negative[seq_len(nShow)])
		if (length(negative) > nShow) {
			bits = c(bits, paste0(length(negative) - nShow, " more"))
		}
		stop(paste0(
			"geometric_mean: negative values are not allowed (",
			paste(bits, collapse = "; "),
			"). A loss is entered as 1 plus the return: a 30% decline is 0.7, not -0.3. ",
			"All values in x must be strictly positive."
		), call. = FALSE)
	}
	if (any(x == 0, na.rm = TRUE)) {
		stop("geometric_mean: zeros are not allowed. All values in x must be strictly positive.", call. = FALSE)
	}
	exp(mean(log(x), na.rm = na.rm))
}

#' Harmonic Mean
#'
#' @description
#' The harmonic mean is the reciprocal of the arithmetic mean of the reciprocals.
#' Use it when you are averaging rates or ratios and the thing you add up is the denominator.
#'
#' * Speeds over a shared distance. Out at 1 mph and back at 10 mph, same distance each way, averages 1.818 mph, not 5.5. Weighting the two speeds by time spent would be the arithmetic mean, which answers a different question.
#' * A portfolio P/E when the same amount of money is invested in each name. The harmonic mean of the P/E ratios equals total price divided by total earnings. If the amounts of money differ, pass those amounts as `weights`. The weights are money invested, not share counts.
#'
#' @details
#' With no weights:
#' \deqn{H = \frac{n}{\sum_{i=1}^{n} \frac{1}{x_i}}}{H = n / sum(1 / x_i)}
#'
#' With weights:
#' \deqn{H = \frac{\sum_{i=1}^{n} w_i}{\sum_{i=1}^{n} \frac{w_i}{x_i}}}{H = sum(w) / sum(w / x)}
#'
#' Every value in `x` must be strictly positive. A zero has an undefined reciprocal. A negative rate or ratio has no harmonic mean of this kind.
#' A weight of zero drops that one observation. A negative weight is an error. After missing values are removed, the weights that remain must sum to more than zero.
#'
#' @param x Positive rates or ratios, such as speeds or P/E ratios.
#' @param weights Optional amounts of the denominator, one per value in `x`. For speeds, the distance of each leg. For P/E ratios, the money invested in each name. A zero drops that observation. Negative weights are an error.
#' @param na.rm Remove NAs (default = TRUE). With weights, a missing value in `x` or in `weights` drops that case from both, so the two stay aligned. If FALSE, any NA returns NA. If TRUE and nothing remains, a warning is given and NA is returned.
#' @return The harmonic mean of `x`.
#' @export
#' @seealso [geometric_mean()], [aggregate()]
#' @family Miscellaneous Stats Functions
#' @references <https://en.wikipedia.org/wiki/Harmonic_mean>
#' @examples
#' # Same money invested in each name.
#' harmonic_mean(c(33, 23)) # 27.11
#' # Different amounts of money. Weights are the money, not the share counts.
#' harmonic_mean(c(33, 23), weights = c(.2, .8)) # 24.48
#'
#' # Out and back over the same distance. Not the arithmetic mean of the two speeds.
#' harmonic_mean(c(1, 10)) # 1.818 mph
#'
#' # Missing data are dropped by default (na.rm = TRUE)
#' harmonic_mean(c(33, 23, NA)) # 27.11
#' harmonic_mean(c(33, 23, NA), na.rm = FALSE) # NA
#' # A missing value drops that case from x and from weights together
#' harmonic_mean(c(33, NA, 23), weights = c(.2, .3, .5)) # 25.18
#' harmonic_mean(c(33, 23), weights = c(.2, NA)) # 33
#' # A zero weight drops that one observation
#' harmonic_mean(c(10, 20), weights = c(1, 0)) # 10
#' \dontrun{
#' harmonic_mean(c(33, 0))
#' harmonic_mean(c(33, -23))
#' harmonic_mean(c(10, 20), weights = c(1, -0.5))
#' harmonic_mean(c(33, 23), weights = c(.2, .3, .5))
#' }
#'
harmonic_mean = function(x, weights = NULL, na.rm = c(TRUE, FALSE)){
	na.rm = xmu_match.arg(na.rm, option_list = c(TRUE, FALSE), check = TRUE)
	showBad = function(values, where) {
		nShow = min(length(where), 6)
		bits = paste0(values[where][seq_len(nShow)], " at position ", where[seq_len(nShow)])
		if (length(where) > nShow) bits = c(bits, paste0(length(where) - nShow, " more"))
		paste(bits, collapse = "; ")
	}
	if (!is.null(weights) && length(weights) != length(x)) {
		stop(paste0(
			"harmonic_mean: weights must be the same length as x (weights has length ",
			length(weights), ", x has length ", length(x), ")."
		), call. = FALSE)
	}
	negative = which(x < 0)
	if (length(negative) > 0) {
		stop(paste0(
			"harmonic_mean: negative values are not allowed (",
			showBad(x, negative),
			"). The harmonic mean averages reciprocals, so every value must be a positive rate or ratio (a speed, a P/E, a resistance). ",
			"All values in x must be strictly positive."
		), call. = FALSE)
	}
	zeros = which(x == 0)
	if (length(zeros) > 0) {
		stop(paste0(
			"harmonic_mean: zeros are not allowed (",
			showBad(x, zeros),
			"). The reciprocal of 0 is undefined. All values in x must be strictly positive."
		), call. = FALSE)
	}
	if (!is.null(weights)) {
		negativeWeights = which(weights < 0)
		if (length(negativeWeights) > 0) {
			stop(paste0(
				"harmonic_mean: negative weights are not allowed (",
				showBad(weights, negativeWeights),
				"). Weights are amounts of the denominator, such as distance for a speed or money invested for a P/E, and must be zero or positive."
			), call. = FALSE)
		}
	}
	if (na.rm) {
		keep = !is.na(x)
		if (!is.null(weights)) keep = keep & !is.na(weights)
		x = x[keep]
		if (!is.null(weights)) weights = weights[keep]
	}
	if (length(x) == 0) {
		warning("harmonic_mean: no non-missing values left to average; returning NA", call. = FALSE)
		return(NA_real_)
	}
	if (is.null(weights)) {
		return(1 / mean(1 / x))
	}
	if (!anyNA(weights) && sum(weights) == 0) {
		stop("harmonic_mean: the weights that remain sum to 0, so there is nothing to average.", call. = FALSE)
	}
	sum(weights) / sum(weights / x)
}

#' Summarizing functions used in umx_aggregate and for umxAPA
#'
#' Miscellaneous functions that are handy in summary and other tasks where you might otherwise have
#' to craft a custom nameless functions. e.g.
#' 
#' * [umx_fun_mean_sd()]: returns "mean (SD)" of x.
#'
#' *note*: if a factor is given, then the mode is returned instead of the mean and SD.
#' @param x input
#' @param na.rm How to handle missing (default = TRUE = remove)
#' @param digits Rounding (default = 2)
#' @return - function result
#' @export
#' @family xmu internal not for end user
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' umxAPA(mtcars[,1:3]) # uses umx_fun_mean_sd
umx_fun_mean_sd = function(x, na.rm = TRUE, digits = 2){
	if(!is.numeric(x)){
		paste0("mode = ", names(which.max(table(x))))
	} else {
		paste0(
			round(mean(x, na.rm = na.rm), digits), " ",
			"(", round(sd(x, na.rm = na.rm),digits = digits), ")"
	)
	}
}

#' Convenient formula-based cross-tabs & built-in summary functions
#'
#' @description
#' A common task is preparing summary tables, aggregating over some grouping factor.
#' Like mean and sd of age, by sex. R's [aggregate()] function is useful and powerful, allowing
#' xtabs based on a formula.
#' 
#' umx_aggregate makes using it a bit easier. In particular, it has some common functions 
#' for summarizing data built-in, like "mean (sd)" (the default).
#' 
#' \code{umx_aggregate(mpg ~ cyl, data = mtcars, what = "mean_sd")}
#' 
#' \tabular{ll}{
#' cyl        \tab mpg\cr
#' 4 (n = 11) \tab 26.66 (4.51)\cr
#' 6 (n = 7)  \tab 19.74 (1.45)\cr
#' 8 (n = 14) \tab 15.1 (2.56)\cr
#' }
#'
#' @param formula The aggregation formula. e.g., DV ~ condition.
#' @param data frame to aggregate (defaults to df for common case)
#' @param what function to use. Default reports "mean (sd)".
#' @param digits to round results to.
#' @param report Format for the table: Default is markdown.
#' @param output Format for the results: "table" (default) or "string".
#' @return - table or character vector of formatted strings
#' @export
#' @family Reporting Functions
#' @seealso - [umx_apply()], [aggregate()]
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' # =====================================
#' # = Basic use, compare with aggregate =
#' # =====================================
#' aggregate(mpg ~ cyl, FUN = mean, na.rm = TRUE, data = mtcars)
#' umx_aggregate(mpg ~ cyl, data = mtcars)
#' 
#' # =============================================
#' # = Use different (or user-defined) functions =
#' # =============================================
#' umx_aggregate(mpg ~ cyl, data = mtcars, what = "n")
#' umx_aggregate(mpg ~ cyl, data = mtcars, what = function(x){sum(!is.na(x))})
#' 
#' # turn off markdown
#' umx_aggregate(mpg ~ cyl, data = mtcars, report = "txt")
#' 
#' # =========================================================
#' # = More than one item on the left hand side (Table 1 style)=
#' # =========================================================
#' umx_aggregate(cbind(mpg, qsec) ~ cyl, data = mtcars, digits = 3)
#' 
#' # =========================
#' # = Output as a string    =
#' # =========================
#' umx_aggregate(mpg ~ cyl, data = mtcars, output = "string")
#' 
#' \dontrun{
#' umx_aggregate(cbind(moodAvg, mood) ~ condition, data = study1)
#' }
umx_aggregate <- function(formula = DV ~ condition, data = df, what = c("mean_sd", "n"), digits = 2, report = c("markdown", "html", "txt"), output = c("table", "string")) {
	report = match.arg(report)
	output = match.arg(output)
	what = xmu_match.arg(what, c("mean_sd", "n"), check = FALSE)
	# TODO: add "sep" to umx_aggregate to make wide data long for summary as in genEpi_TwinDescriptives
	# genEpi_TwinDescriptives(mzData = twinData, dzData = NULL, selDVs = selDVs, groupBy = c("Sex_T1", "Sex_T2"), graph = F)
	# genEpi_twinDescribe(twinData, varsToSummarize="Age", groupBy="Sex", suffix="_T")

	# If 'One' is requested in the RHS but doesn't exist in the data, add it dynamically
	if ("One" %in% all.vars(formula[[3]]) && !("One" %in% colnames(data))) {
		data$One <- 1
	}

	mean_sd = function(x){
		if(is.numeric(x)){
			paste0(round(mean(x, na.rm = TRUE), digits = digits), " (",
				   round(sd(x, na.rm = TRUE), digits = digits), ")"
			)
		} else {
			paste0(names(table(x))," ", table(x), collapse = "; ")
		}
	}
	x_n = function(x){sum(!is.na(x))}

	if(inherits(what, "function")){
		FUN = what
	} else if(!inherits(what, "character")){
		stop("umx_aggregate what should be a built-in name like 'mean_sd', or a function, you gave me a", class(what))
	} else if(what == "mean_sd"){
		FUN = mean_sd
	} else if(what == "n"){
		FUN = x_n
	}

	# Parse LHS and RHS
	vars = all.vars(formula[[2]])
	rhs_vars = all.vars(formula[[3]])
	rhs_str = deparse(formula[[3]])

	if (output == "string") {
		all_strings = c()
		for (v in vars) {
			v_formula = as.formula(paste0(v, " ~ ", rhs_str))
			tmp_v = aggregate(v_formula, FUN = FUN, data = data)
			n_s = aggregate(v_formula, FUN = x_n, data = data)
			
			for (i in 1:nrow(tmp_v)) {
				if (rhs_str == "One" || names(tmp_v)[1] == "One") {
					group_str = ""
				} else {
					group_str = paste0(paste(tmp_v[i, 1:(ncol(tmp_v)-1)], collapse = ", "), ": ")
				}
				val = tmp_v[i, ncol(tmp_v)]
				cnt = n_s[i, ncol(n_s)]
				all_strings = c(all_strings, paste0(group_str, v, " was (n = ", cnt, ": ", val, ")"))
			}
		}
		
		if(report == "html"){
			umx_print(data.frame(String = all_strings), digits = digits, report = report)
		} else if(report == "markdown"){
			return(paste(all_strings, collapse = "\n\n"))
		}else{
			return(all_strings)
		}
	} else {
		# Process table output
		if (length(vars) <= 1) {
			tmp = aggregate(formula, FUN = FUN, data = data)
			n_s = aggregate(formula, FUN = x_n, data = data)
			tmp = data.frame(tmp)
			tmp[, 1] = paste0(as.character(tmp[, 1]), " (n = ", n_s[, 2], ")")
			if(report == "html"){
				umx_print(tmp, digits = digits, report = report)
			} else if(report == "markdown"){
				return(kable(tmp, format="pipe"))
			}else{
				return(tmp)
			}
		} else {
			# Process multiple DVs: construct transposed Table 1 style (DVs as rows, Group levels as columns)
			results = list()
			for (v in vars) {
				v_formula = as.formula(paste0(v, " ~ ", rhs_str))
				results[[v]] = aggregate(v_formula, FUN = FUN, data = data)
			}
			
			# Compute overall group sizes
			data_copy = data
			data_copy$.dummy_n = 1
			n_formula = as.formula(paste0(".dummy_n ~ ", rhs_str))
			n_s = aggregate(n_formula, FUN = sum, data = data_copy)
			
			# Construct transposed table: first column "Variable"
			transposed = data.frame(Variable = vars, stringsAsFactors = FALSE)
			n_groups = nrow(n_s)
			
			# Add each group as a column
			for (i in 1:n_groups) {
				group_val_cols = 1:(ncol(n_s) - 1)
				group_vals = paste(as.character(unlist(n_s[i, group_val_cols])), collapse = ", ")
				col_name = paste0(group_vals, " (n = ", n_s[i, ncol(n_s)], ")")
				
				col_values = c()
				for (v in vars) {
					val = results[[v]][i, ncol(results[[v]])]
					col_values = c(col_values, as.character(val))
				}
				
				transposed[[col_name]] = col_values
			}
			
			if(report == "html"){
				umx_print(transposed, digits = digits, report = report)
			} else if(report == "markdown"){
				return(kable(transposed, format="pipe"))
			}else{
				return(transposed)
			}
		}
	}
}

#' Round p-values according to APA guidelines
#'
#' @description
#' `umx_APA_pval` formats p-values, rounded in APA style. So you get '< .001' instead of .000000002 or 1.00E-09.
#' 
#' You probably would be better off using [umxAPA()], which handles many more object types.
#' 
#' You set the precision with digits. Optionally, you can add '=' '<' etc. The default for addComparison (NA) adds these when needed.
#'
#' @param p The p-value to round
#' @param min Values below min will be reported as "< min"
#' @param digits Number of decimals to which to round (default = 3)
#' @param addComparison Whether to add '=' '<' etc. (NA adds when needed)
#' @family xmu internal not for end user
#' @return - p-value formatted in APA style
#' @export
#' @seealso - [umxAPA()], [round()]
#' @examples
#' umx_APA_pval(.052347)
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(1.23E-4)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = TRUE)
umx_APA_pval <- function(p, min = .001, digits = 3, addComparison = NA) {
	# Typically use umxAPA?
	if(length(p) > 1){
		o = rep(NA, length(p))
		for(i in seq_along(p)) {
		   o[i] = umx_APA_pval(p[i], min = min, digits = digits, addComparison = addComparison)
		}
		return(o)
	} else {
		if(is.nan(p) | is.na(p)){
			if(is.na(addComparison)){
				return(p)
			}else if(addComparison){
				return(paste0("= ", p))
			} else {
				return(p)
			}
		}
		if(p < min){
			if(is.na(addComparison)){
				return(paste0("< ", min))
			}else if(addComparison){
				return(paste0("< ", min))
			} else {
				return(min)
			}
		} else {
			if(is.na(addComparison)){
				return(format(round(p, digits), scientific = FALSE, nsmall = digits))
			}else if(addComparison){				
				return(paste0("= ", format(round(p, digits), scientific = FALSE, nsmall = digits)))
			} else {
				return(round(p, digits))
			}
		}	
	}
}


#' Summarize twin data
#'
#' @description
#' Produce a summary of wide-format twin data, showing the number of individuals,
#' descriptives for each trait, and the twin-pair correlation for each twin-type.
#'
#' Continuous traits show the mean and SD, and Pearson correlations. Binary and ordinal traits
#' (factors, or numeric 0/1 or 1/2) show the proportion endorsed (above the lowest category) with its SE,
#' the liability thresholds with SEs, and polychoric correlations (via [umx_polychoric()]).
#'
#' @details
#' **Groups**: by default the 5-group case (MZFF, MZMM, DZFF, DZMM, DZOS) is used. Set MZ and DZ to summarize
#' the two-group case. If `zyg` holds only MZ/DZ (and SIB) levels, and sex columns (e.g. `sex_T1`, `sex_T2`) are
#' present, pairs are split by sex into MZFF, MZMM, DZFF, DZMM, DZOS (and SIBFF, SIBMM, SIBOS).
#'
#' **Thresholds** are the ML estimates on the liability scale (mean 0, SD 1), pooled over both twins: `qnorm()` of the
#' proportion at or below each category. SEs of proportions are cluster-robust over families
#' (co-twins are not independent), and threshold SEs follow by the delta method.
#'
#' @param data The twin data (wide: one row per family).
#' @param selVars Collection of variables to report on, e.g. c("wt", "ht").
#' @param sep  The separator string that will turn a variable name into a twin variable name, default= "_T" for wt_T1 and wt_T2.
#' @param age The age column in the dataset (default "age").
#' @param sex The sex column in the dataset (default "sex").
#' @param zyg  The zygosity column in the dataset (default "zygosity").
#' @param MZ Set level in zyg corresponding to MZ for two group case (defaults to using 5-group case).
#' @param DZ Set level in zyg corresponding to DZ for two group case (defaults to using 5-group case).
#' @param MZFF The level of zyg corresponding to MZ FF pairs: default= "MZFF".
#' @param DZFF The level of zyg corresponding to DZ FF pairs: default= "DZFF".
#' @param MZMM The level of zyg corresponding to MZ MM pairs: default= "MZMM".
#' @param DZMM The level of zyg corresponding to DZ MM pairs: default= "DZMM".
#' @param DZOS The level of zyg corresponding to DZ OS pairs: default= "DZOS".
#' @param digits Rounding precision of the report (default 2).
#' @param report What to return (default = 'markdown'). Use 'html' to open a web table.
#' @param show Twin correlations as "ci" (default: r \[95% CI\]) or "se": r (SE).
#' @param male Values of sex coding males (default c("M", "male", "Male", "m")).
#' @param female Values of sex coding females (default c("F", "female", "Female", "f")).
#' @param tryHard Passed to [umx_polychoric()] for polychoric correlations (default "no").
#' @return - Invisibly returns the formatted dataframe.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxAPA()], [umx_polychoric()]
#' @references <https://github.com/tbates/umx>
#' @examples
#' data(twinData)
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"))
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"), show="se")
#' MZs = c("MZMM", "MZFF"); DZs = c("DZFF","DZMM", "DZOS")
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"), MZ = MZs, DZ = DZs)
#'
#' \dontrun{
#' # Open as web tables to copy into WORD PROCESSOR
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht"), report="html")
#'
#' # A binary trait: proportion endorsed (SE), threshold (SE), and polychoric twin correlations
#' twinData$obese1 = as.numeric(twinData$bmi1 > 25)
#' twinData$obese2 = as.numeric(twinData$bmi2 > 25)
#' umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "obese"), show = "se")
#' }
umxSummarizeTwinData = function(data = NULL, selVars = NULL, sep = "_T", zyg = "zygosity", age = "age", sex = "sex", MZ = NULL, DZ = NULL, MZFF = "MZFF", DZFF = "DZFF", MZMM = "MZMM", DZMM = "DZMM", DZOS = "DZOS", digits = 2, report = c("markdown", "html"), show = c("ci", "se"), male = c("M", "male", "Male", "m"), female = c("F", "female", "Female", "f"), tryHard = c("no", "yes", "ordinal", "search")) {
	report  = match.arg(report)
	show    = match.arg(show)
	tryHard = match.arg(tryHard)

	# 1. Age Extraction
	if(umx_check_names(age, data = data, die = FALSE)){
		ageCol = data[, age]
		cat(paste0("Mean age = ", round(mean(ageCol, na.rm = TRUE), digits), " (SD = ", round(sd(ageCol, na.rm = TRUE), digits), ")\n"))
	} else if(umx_check_names(paste0(age, sep, 1), data = data, die = FALSE)){
		ageCol = data[, paste0(age, sep, 1)]
		cat(paste0("Mean age = ", round(mean(ageCol, na.rm = TRUE), digits), " (SD = ", round(sd(ageCol, na.rm = TRUE), digits), ")\n"))
	} else {
		message("Polite note: Cannot find age column. Checked for ", omxQuotes(age), " and ", omxQuotes(paste0(age, sep, 1)))
	}

	# 2. Dynamic Sex Extraction
	sex1 = paste0(sex, sep, 1)
	sex2 = paste0(sex, sep, 2)
	hasSex = umx_check_names(c(sex1, sex2), data = data, die = FALSE)
	if(hasSex) {
		cat(sum(c(data[,sex1], data[,sex2]) %in% female, na.rm = TRUE), "female\n")
		cat(sum(c(data[,sex1], data[,sex2]) %in% male, na.rm = TRUE), "male\n")
	} else {
		message("Polite note: No sex columns found (looked for ", omxQuotes(sex1), " and ", omxQuotes(sex2), "): skipping M/F counts. Add per-twin sex columns, or set sex= to their stem.")
	}

	# 3. Setup Long Data
	selDVs = tvars(selVars, sep)
	umx_check_names(selDVs, data = data, die = TRUE)
	long = umx_wide2longTwinData(data = data[, selDVs], sep = sep)

	# 4. High-Performance Pair Counts (Cross-product of non-NA indicators)
	isPresent = !is.na(long[, selVars, drop = FALSE])
	pairCounts = crossprod(isPresent)
	umx_print(pairCounts, caption = "Pairwise Complete Cases", digits = digits, report = report)

	# 5. Groups: a named list of row indices
	if(!is.null(MZ)) {
		groups = list(MZ = which(data[, zyg] %in% MZ), DZ = which(data[, zyg] %in% DZ))
	} else {
		umx_check_names(zyg, data = data, die = TRUE)
		z = as.character(data[, zyg])
		fiveGroup = c(MZFF = MZFF, MZMM = MZMM, DZFF = DZFF, DZMM = DZMM, DZOS = DZOS)
		sibGroup  = c("SIBFF", "SIBMM", "SIBOS")
		if(any(z %in% fiveGroup)) {
			groups = lapply(fiveGroup, function(g) which(z %in% g))
			for (g in sibGroup[sibGroup %in% z]) groups[[g]] = which(z == g)
		} else if(hasSex && all(toupper(stats::na.omit(unique(z))) %in% c("MZ", "DZ", "SIB"))) {
			# Split MZ/DZ/SIB by sex
			sexOf = function(x) ifelse(x %in% female, "F", ifelse(x %in% male, "M", NA))
			s1 = sexOf(data[, sex1]); s2 = sexOf(data[, sex2])
			pairSex = ifelse(s1 == "F" & s2 == "F", "FF", ifelse(s1 == "M" & s2 == "M", "MM", ifelse(!is.na(s1) & !is.na(s2), "OS", NA)))
			pairLabel = paste0(toupper(z), pairSex)
			pairLabel[is.na(z) | is.na(pairSex)] = NA
			order  = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS", "MZOS", "SIBFF", "SIBMM", "SIBOS")
			order  = order[order %in% pairLabel]
			if(length(order) == 0) {
				# Sex columns exist, but no codes matched male/female: fall back to unsplit levels
				warning("umxSummarizeTwinData: no pair sexes recognized from ", omxQuotes(sex1), " and ", omxQuotes(sex2), "; using unsplit ", omxQuotes(zyg), " levels. Check the male and female codes.")
				levs   = sort(unique(stats::na.omit(z)))
				groups = stats::setNames(lapply(levs, function(g) which(z == g)), levs)
			} else {
				groups = stats::setNames(lapply(order, function(g) which(pairLabel == g)), order)
			}
		} else {
			levs   = sort(unique(stats::na.omit(z)))
			groups = stats::setNames(lapply(levs, function(g) which(z == g)), levs)
		}
	}

	# 6. Helpers
	fmtNum = function(x) ifelse(is.na(x), NA_character_, sprintf(paste0("%.", digits, "f"), x))
	isOrdinalLike = function(x) {
		if(is.factor(x)) return(length(levels(x)) >= 2)
		u = unique(stats::na.omit(x))
		is.numeric(x) && length(u) == 2 && (all(u %in% c(0, 1)) || all(u %in% c(1, 2)))
	}
	# Ordered factor with the same levels for both twins
	asOrdered = function(x, levs) factor(as.character(x), levels = levs, ordered = TRUE)

	# Proportion of family members with x <= k, with a cluster-robust (families) SE
	clusterProp = function(ind1, ind2) {
		y = rowSums(cbind(ind1, ind2), na.rm = TRUE)
		n = rowSums(!is.na(cbind(ind1, ind2)))
		keep = n > 0
		y = y[keep]; n = n[keep]
		G = length(n)
		p = sum(y) / sum(n)
		se = if(G > 1) sqrt(G / (G - 1) * sum((y - p * n)^2)) / sum(n) else NA_real_
		c(p = p, se = se)
	}

	# Twin correlation within one group: Pearson for continuous, polychoric for ordinal
	pairCor = function(sub, v1, v2, ordinal, levs) {
		ok = stats::complete.cases(sub[, c(v1, v2)])
		n  = sum(ok)
		if(n < 3) return(list(str = NA_character_, n = n))
		if(!ordinal) {
			res = tryCatch(cor.test(sub[ok, v1], sub[ok, v2]), error = function(e) NULL)
			if(is.null(res)) return(list(str = NA_character_, n = n))
			r = unname(res$estimate); lo = res$conf.int[1]; hi = res$conf.int[2]
			se = (hi - lo) / (2 * 1.96)
		} else {
			tmp = data.frame(a = asOrdered(sub[, v1], levs), b = asOrdered(sub[, v2], levs))
			fit = tryCatch(suppressMessages(umx_polychoric(tmp, tryHard = tryHard)), error = function(e) NULL)
			if(is.null(fit)) return(list(str = NA_character_, n = n))
			r  = fit$polychorics[1, 2]
			se = fit$polychoricStandardErrors[1, 2]
			if(is.null(se)) se = NA_real_
			lo  = r - 1.96 * se; hi = r + 1.96 * se
		}
		if(show == "se") {
			str = if(is.na(se)) fmtNum(r) else paste0(fmtNum(r), " (", fmtNum(se), ")")
		} else {
			str = if(is.na(lo)) fmtNum(r) else paste0(fmtNum(r), " [", fmtNum(lo), ", ", fmtNum(hi), "]")
		}
		list(str = str, n = n)
	}

	# 7. Core DataFrame Assembly
	twoGroup = !is.null(MZ)
	rNames   = paste0("r", names(groups))
	df = data.frame(Var = selVars, Mean = NA_character_, SD = NA_character_, Thresholds = NA_character_, stringsAsFactors = FALSE)
	if(twoGroup) df = cbind(df[, 1, drop = FALSE], "N_MZ_DZ" = NA_character_, df[, -1])
	for (g in rNames) df[[g]] = NA_character_

	for (n in seq_along(selVars)){
		varName = selVars[n]
		v1 = paste0(varName, sep, 1)
		v2 = paste0(varName, sep, 2)
		ordinal = isOrdinalLike(data[, v1]) && isOrdinalLike(data[, v2])
		if(ordinal) {
			levs = union(levels(data[, v1]), levels(data[, v2]))
			if(is.null(levs)) levs = sort(unique(stats::na.omit(c(data[, v1], data[, v2]))))
			x1 = as.integer(asOrdered(data[, v1], levs)); x2 = as.integer(asOrdered(data[, v2], levs))
			# Endorsed: above the lowest category
			endorsed = clusterProp(x1 > 1, x2 > 1)
			df[n, "Mean"] = paste0(fmtNum(endorsed["p"]), " (", fmtNum(endorsed["se"]), ")")
			# Thresholds: qnorm(P(x <= k)), SE by the delta method
			thresh = sapply(seq_len(length(levs) - 1), function(k) {
				pk = clusterProp(x1 <= k, x2 <= k)
				t  = stats::qnorm(pk["p"])
				tSE = pk["se"] / stats::dnorm(t)
				if(!is.finite(t)) tSE = NA_real_ # empty cell: threshold at +/-Inf
				paste0(fmtNum(t), " (", fmtNum(tSE), ")")
			})
			df[n, "Thresholds"] = paste(thresh, collapse = "; ")
		} else {
			levs = NULL
			df[n, "Mean"] = fmtNum(mean(long[, varName], na.rm = TRUE))
			df[n, "SD"]   = fmtNum(sd(long[, varName], na.rm = TRUE))
		}
		nPairs = c()
		for (g in names(groups)) {
			res = pairCor(data[groups[[g]], , drop = FALSE], v1, v2, ordinal, levs)
			df[n, paste0("r", g)] = res$str
			nPairs = c(nPairs, res$n)
		}
		if(twoGroup) df[n, "N_MZ_DZ"] = paste0(nPairs, collapse = "/")
	}
	if(all(is.na(df$Thresholds))) {
		# All continuous: numeric Mean and SD, as before
		df$Thresholds = NULL
		df$Mean = round(as.numeric(df$Mean), digits)
		df$SD   = round(as.numeric(df$SD), digits)
	}
	if(all(is.na(df$SD))) df$SD = NULL
	if(twoGroup) names(df)[names(df) == "N_MZ_DZ"] = "N (MZ/DZ)"

	cat("\n")
	umx_print(df, caption = "Means, SDs, and twin-pair correlations by zygosity", digits = digits, report = report)
	if(show == "ci") cat("\nNote: Bracketed values denote 95% Confidence Intervals.\n") else cat("\nNote: Values in parentheses are standard errors.\n")
	if(any(!is.na(df$Thresholds)) && !is.null(df$Thresholds)) cat("Note: For binary/ordinal traits, Mean is the proportion endorsed (SE), Thresholds are on the liability scale (SE), and r is polychoric.\n")
	if(twoGroup) cat("Note: (n = MZ/DZ) represents exact pairwise complete twin pairs for that variable.\n")

	invisible(df)
}


#' Test the difference between correlations for significance.
#'
#' @description
#' `umx_r_test` is a wrapper around the cocor test of difference between correlations.
#'
#' @details
#' **Non-overlapping (no variable in common) correlations in the same dataset.**
#' If 4 variables are provided in `vars`, `umx_r_test` conducts a test of
#' the correlation of var 1 & 2 differs in magnitude from the correlation of var 3 with var 4.
#' (`r.jk` and `r.hm` in cocor speak).
#' 
#' **Overlapping (1 variable in common) correlations in the same dataset.**
#' If 3 variables are provided in `vars`, `umx_r_test` conducts a test of whether
#' the correlation of var 1 & 2 differs in magnitude from the correlation of var 1 with var 3.
#' (`r.jk` and `r.jh` in cocor speak).
#' 
#' In the future it will be expanded to handle other correlations, and to take correlations as input.
#'
#' @param data The dataset.
#' @param vars Three or 4 variables forming the two pairs of columns.
#' @param alternative A two (default) or one-sided (greater less) test.
#' @return cocor result.
#' @export
#' @family Miscellaneous Stats Functions
#' @examples
#' # Is the correlation of mpg with cylinder count different from that 
#' # obtaining between disp and hp?
#' vars = c("mpg", "cyl", "disp", "hp")
#' umx_r_test(mtcars, vars)
#' umx_r_test(mtcars, c("mpg", "disp", "hp"))
umx_r_test <- function(data = NULL, vars = vars, alternative = c("two.sided", "greater", "less")) {
	alternative = match.arg(alternative)
	alpha        = 0.05
	conf.level   = 0.95
	null.value   = 0
	data.name    = NULL
	var.labels   = NULL
	return.htest = FALSE
	n    = nrow(data)	
	jkhm = data[, vars]
	cors = cor(jkhm)
	# jkhm = 1234
	if(length(vars)==3){
		r.jk = as.numeric(cors[vars[1], vars[2]])
		r.jh = as.numeric(cors[vars[1], vars[3]])
		r.kh = as.numeric(cors[vars[2], vars[3]])
		# tests = (pearson1898, hotelling1940, hendrickson1970, williams1959, olkin1967, dunn1969, steiger1980, meng1992, hittner2003, or zou2007).
		cocor::cocor.dep.groups.overlap(r.jk, r.jh, r.kh, n, alternative = alternative, test = "hittner2003", alpha = alpha, 
			conf.level = conf.level, null.value = null.value, data.name = data.name, var.labels = var.labels, return.htest = return.htest)
	} else {
		r.jk = as.numeric(cors[vars[1], vars[2]])
		r.hm = as.numeric(cors[vars[3], vars[4]])
		r.jh = as.numeric(cors[vars[1], vars[3]])
		r.jm = as.numeric(cors[vars[1], vars[4]])
		r.kh = as.numeric(cors[vars[2], vars[3]])
		r.km = as.numeric(cors[vars[2], vars[4]])
		# test         = "silver2004"
	 	# tests = (pearson1898, dunn1969, steiger1980, raghunathan1996, silver2004, or zou2007).
 		cocor::cocor.dep.groups.nonoverlap(r.jk, r.hm, r.jh, r.jm, r.kh, r.km, n, alternative = alternative, test = "silver2004", alpha = alpha, 
			conf.level = conf.level, null.value = null.value, data.name = data.name, var.labels = var.labels, return.htest = return.htest)
	}

}

#' Plot GWAS Results
#'
#' @description
#' Generates a Manhattan or Q-Q plot for GWAS results. Requires the `ggwas` package.
#' @details
#' See ?manhattan_plot for more parameters. Key ones include chromosomes to restrict chromosomes, highlight_snps to highlight a SNP,
#' label_top_n, downsample (TRUE), and downsample_n (2e+05), title = NULL
#' @param x A `umx_GWAS` object (returned by \code{\link{umxGSEM_GWAS}}).
#' @param type Type of plot: "manhattan" (default) or "qq".
#' @param ... Additional arguments passed to ggwas \code{manhattan_plot} or \code{qq_plot}
#'   (optional package; install from GitHub \code{bczech/ggwas} if needed).
#' @return A `ggplot` object.
#' @export
#' @family Reporting Functions
#' @family GSEM
#' @examples
#' \dontrun{
#' # Requires optional GitHub package ggwas (not on CRAN):
#' #   remotes::install_github("bczech/ggwas")
#' # data(example_gwas, package = "ggwas")
#' # plot.umx_GWAS(example_gwas, type = "manhattan")
#' }
plot.umx_GWAS <- function(x, type = c("manhattan", "qq"), ...) {
	# Declared in DESCRIPTION Suggests (optional; install from GitHub until on CRAN)
	if (!requireNamespace(as.character("ggwas"), quietly = TRUE)) {
		stop("The 'ggwas' package is required for plotting GWAS results. Install it with: remotes::install_github('bczech/ggwas')", call. = FALSE)
	}
	type = match.arg(type)
	if (type == "manhattan") {
		# TODO clean up when ggwas goes on cran
		# 2. Dynamically extract the function to bypass CRAN's static namespace check
		ggwas_manhattan_plot_fn <- utils::getFromNamespace("manhattan_plot", "ggwas")
		# 3. Execute seamlessly
		return(ggwas_manhattan_plot_fn(x, chr = "CHR", bp = "BP", p = "P", snp = "SNP", ...))
	} else if (type == "qq") {
		ggwas_qq_plot_fn <- utils::getFromNamespace("qq_plot", "ggwas")
		return(ggwas_qq_plot_fn(x, p = "P", ...))
	}
}
