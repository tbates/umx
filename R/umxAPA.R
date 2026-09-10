#' Creates nicely formatted journal-style summaries of models, p-values, data-frames and much more.
#'
#' @description
#' `umxAPA` creates APA-style reports from a range of statistical models, or to summarize data. I wrote it to suit me.
#' 
#' Nice alternatives include `jtools::summ`.
#' 
#' Example functionality includes:
#' 
#' 1. Given an [stats::lm()] model, `umxAPA` will return a formatted effect, including 95% CI. 
#' e.g.: `umxAPA(lm(mpg~wt, data=mtcars), "wt")` yields: \eqn{\beta} = -5.34 \[-6.48, -4.20\], p < 0.001. here "wt" 
#' restricts the output to just the named effect.
#' 2. `umxAPA` also supports [t.test()], [stats::glm()], [cor.test()], and others as I need them.
#' 3. Get a CI from `obj=beta` and se=se : `umxAPA(-0.30, .03)` returns \eqn{\beta} = -0.3 \[-0.36, -0.24\]
#' 4. Back out an SE from \eqn{\beta} and CI: `umxAPA(-0.030, c(-0.073, 0.013))` returns \eqn{\beta} = -0.03, se = 0.02
#' 5. Given only a number as obj, will be treated as a p-value, and returned in APA format.
#' 6. Given a dataframe, `umxAPA` will return a table of correlations with means and SDs in the last row. e.g.:
#' `umxAPA(mtcars[,c("cyl", "wt", "mpg", )]` yields:
#'   \tabular{lccc}{
#'            \tab cyl         \tab  wt          \tab mpg          \cr
#'    cyl     \tab 1           \tab  0.78        \tab -0.85        \cr
#'    wt      \tab 0.78        \tab  1           \tab -0.87        \cr
#'    mpg     \tab -0.85       \tab  -0.87       \tab 1            \cr
#'    mean_sd \tab 6.19 (1.79) \tab  3.22 (0.98) \tab 20.09 (6.03)
#'   }
#'
#' @aliases summaryAPA
#' @param obj A model (e.g. [lm()], [nlme::lme()], [glm()], [t.test()]), beta-value, or [data.frame]
#' @param se If obj is a beta, se treated as standard-error (returning a CI). 
#' If obj is a model, used to select effect of interest (blank for all effects). 
#' Finally, set se to the CI c(lower, upper), to back out the SE.
#' @param p If obj is a beta, use p-value to compute SE (returning a CI).
#' @param std Whether to report std betas (re-runs model on standardized data).
#' @param digits How many digits to round output.
#' @param use If obj is a data.frame, how to handle NAs (default = "complete")
#' @param min For a p-value, the smallest value to report numerically (default .001)
#' @param addComparison For a p-value, whether to add "</=" default (NA) adds "<" if necessary
#' @param report What to return (default = 'markdown'). Use 'html' to open a web table. none doesn't print. expression can contain [plotmath()] 
#' @param lower Whether to not show the lower triangle of correlations for a data.frame (Default TRUE)
#' @param SEs Whether or not to show correlations with their SE (Default TRUE)
#' @param means Whether or not to show means in a correlation table (Default TRUE)
#' @param test If obj is a glm, which test to use to generate p-values options = "Chisq", "LRT", "Rao", "F", "Cp"
#' @param suffix A string to append to the result. Mostly used with report = "expression"
#' @param caption Optional caption for html/markdown tables. `NA` (default) auto-generates "Effects on y of a, b, and c" from `formula(obj)` for `lm`/`glm`; `NULL` suppresses caption; a string uses that text.
#' @param cols Optional, pass in a list of column names when using umxAPA with a dataframe input.
#' @param stars Whether to append significance stars to p-values ("*" p < .05, "**" p < .01, "***" p < .001). Default `TRUE` (APA-7 table standard: * p < .05, ** p < .01, *** p < .001; set `stars=FALSE` to suppress stars; enable for reviewers who request them).
#' @return - string
#' @export
#' @seealso [SE_from_p()]
#' @family Reporting Functions
#' @references - <https://stats.oarc.ucla.edu/r/dae/logit-regression/>
#' @examples
#' 
#' # ========================================
#' # = Report lm (regression/anova) results =
#' # ========================================
#' umxAPA(lm(mpg ~ wt + disp, mtcars)) # Report all parameters
#' umxAPA(lm(mpg ~ wt + disp, mtcars), "wt") # Just effect of weight
#' umxAPA(lm(mpg ~ wt + disp, mtcars), std = TRUE) # Standardize model!
#' 
#' ###############
#' # GLM example #
#' ###############
#'
#' df = mtcars
#' df$mpg_thresh = 0
#' df$mpg_thresh[df$mpg > 16] = 1
#' m1 = glm(mpg_thresh ~ wt + gear,data = df, family = binomial)
#' umxAPA(m1)
#' 
#' ###############
#' # A t-Test    #
#' ###############
#'
#' umxAPA(t.test(x = 1:10, y = c(7:20)))
#' umxAPA(t.test(extra ~ group, data = sleep))
#' 
#' # ======================================================
#' # = Summarize DATA FRAME: Correlations + Means and SDs =
#' # ======================================================
#' umxAPA(mtcars[,1:3])
#' umxAPA(mtcars[,1:3], digits = 3)
#' umxAPA(mtcars[,1:3], lower = FALSE)
#' \dontrun{
#' umxAPA(mtcars[,1:3], report = "html")
#' }
#' 
#' # ==========================================
#' # = CONFIDENCE INTERVAL from effect and se =
#' # ==========================================
#' umxAPA(.4, .3) # parameter 2 interpreted as SE
#' 
#' # Input beta and CI, and back out the SE
#' umxAPA(-0.030, c(-0.073, 0.013), digits = 3)
#' 
#' # ====================
#' # = Format a p-value =
#' # ====================
#' umxAPA(.0182613)   #   0.02
#' umxAPA(.00018261) # < 0.001
#' umxAPA(.00018261, addComparison = FALSE) # 0.001
#' 
#' # ========================
#' # = Report a correlation =
#' # ========================
#' data(twinData)
#' tmp = subset(twinData, zygosity %in% c("MZFF", "MZMM"))
#' m1 = cor.test(~ wt1 + wt2, data = tmp)
#' umxAPA(m1)
#'
umxAPA <- function(obj = .Last.value, se = NULL, p = NULL, std = FALSE, digits = 2, use = "complete", min = .001, addComparison = NA, report = c("markdown", "html", "none", "expression"), lower = TRUE, test = c("Chisq", "LRT", "Rao", "F", "Cp"), SEs = TRUE, means = TRUE, suffix="", caption = NA, cols=NA, stars = TRUE) {
	report     = match.arg(report)
	test       = match.arg(test)
	commaSep   = paste0(umx_set_separator(silent = TRUE), " ")
	betaSymbol = ifelse(std, " \u03B2 = ", " B = ")
	# helper for significance stars (reviewer-requested, e.g. .04 * , < .001 ***)
	getStars = function(pval) {
		if(!isTRUE(stars) || is.na(pval)) return("")
		if(pval < .001) return("***")
		if(pval < .01)  return("**")
		if(pval < .05)  return("*")
		return("")
	}

	if("htest" == class(obj)[[1]]){
		# t.test
		if(std){
			message("Polite note: Sorry, I can't standardize a t-test for you")
		}
		
		if(obj$method ==  "Pearson's product-moment correlation"){
			# cor.test
			o = paste0("r = ", round(obj$estimate, digits), " [", round(obj$conf.int[1], digits), commaSep, round(obj$conf.int[2], digits), "]")
			star = getStars(obj$p.value)
			o = paste0(o, ", t(", obj$parameter, ") = ", round(obj$statistic, digits),  ", p = ", umxAPA(obj$p.value), ifelse(star != "", paste0(" ", star), ""))
		} else {
			grpNames = names(obj$estimate)
			if("mean difference" %in% grpNames){
				o = paste0(obj$data.name, " means differed by ", round(obj$estimate, digits), " ")
			} else {
				if(length(grpNames)>1){
					descriptionTxt = paste0("Means in the ", 
						namez(grpNames[1], pattern= "mean (in group|of) ", replacement="")," and ", 
						namez(grpNames[2], pattern= "mean (in group|of) ", replacement=""), " groups were "
					)
				} else {
					descriptionTxt = paste0("Means in the ", obj$data.name, " groups were ")
				}
				o = paste0(descriptionTxt, omxQuotes(round(obj$estimate, digits)), "respectively. ")
			}
			star = getStars(obj$p.value)
			o = paste0(o, "(CI[", round(obj$conf.int[1], 2), ", ", round(obj$conf.int[2], 2), "], ",
				"t(", round(obj$parameter, 2), ") = ", round(obj$statistic, 2), ", p = ", umxAPA(obj$p.value), ifelse(star != "", paste0(" ", star), ""), ")"
			)
		}
		cat(o)
		invisible(o)
	}else if(class(obj)[[1]] %in% c("data.frame", "tbl_df") ) {
		if(class(obj)[[1]] =="tbl_df"){
			obj = data.frame(obj)
		}
		# Generate a summary of correlation and means
		# TODO umxAPA could upgrade strings to factors here (instead of stopping)...
		if(!any(is.na(cols))){
			umx_check_names(cols, data = obj, die = TRUE)
			obj = obj[, cols]
		}
		cor_table = umxHetCor(obj, ML = FALSE, use = use, treatAllAsFactor = FALSE, verbose = FALSE, std.err = SEs, return = "hetcor object")
		# cor_table = x; digits = 2
		# cor_table = umx_apply(FUN= round, of = cor_table, digits = digits) # round correlations
		correlations = round(cor_table$correlations, digits)
		if(SEs){
			std.errors = round(cor_table$std.errors, digits)
			correlations[] = paste0(as.character(correlations), " (", as.character(std.errors), ")")
		}
		cor_table = correlations

		if(lower){
			cor_table[upper.tri(cor_table)] = ""
		}

		if(means){
			mean_sd = umx_apply(umx_fun_mean_sd, of = obj)
			# along the bottom
			# output  = data.frame(rbind(cor_table, mean_sd), stringsAsFactors = FALSE)
			# rownames(output)[length(rownames(output))] = "Mean (SD)"

			output  = data.frame(cbind(mean_sd, cor_table), stringsAsFactors = FALSE)
			# colnames(output)[length(colnames(output))] = "Mean (SD)"
		} else {
			output  = data.frame(cor_table, stringsAsFactors = FALSE)
		}
		captionToUse = caption
		if(!is.null(captionToUse) && length(captionToUse)==1 && is.na(captionToUse)){
			captionToUse = NULL
		}
		umx_print(output, digits = digits, report = report, caption = captionToUse)
		if(anyNA(obj)){
			message("Some rows in dataframe had missing values.")
		}
	} else if("matrix" == class(obj)[[1]]) {
		# Assume these are correlations or similar numbers
		cor_table = umx_apply(round, obj, digits = digits) # round correlations
		output = data.frame(cor_table)
		captionToUse = caption
		if(!is.null(captionToUse) && length(captionToUse)==1 && is.na(captionToUse)){
			captionToUse = NULL
		}
		umx_print(output, digits = digits, report = report, caption = captionToUse)
	} else if("lm" == class(obj)[[1]]) {
		# Report lm summary table
		if(std){
			# Should not touch the left-hand side variable, make sure factors are not touched, inc. binary variables
			# see also summ(transform.response = TRUE)
			# labels gets the RHS, but includes, e.g. interactions, all.vars get the var list, inc. the LHS
			# RHSvars = intersect(labels(obj$terms), all.vars(obj$terms))
			# modelDF = obj$model
			# modelDF[, RHSvars] = umx_scale(modelDF[, RHSvars])
			# obj = update(obj, data = modelDF)
			obj = update(obj, data = umx_scale(obj$model))
		}
		if(report=="html"){
			captionToUse = caption
			if(!is.null(captionToUse) && length(captionToUse)==1 && is.na(captionToUse)){
				f = tryCatch(stats::formula(obj), error=function(e) NULL)
				if(!is.null(f) && length(f)==3){
					dv = paste(deparse(f[[2]]), collapse="")
					rhsTerms = attr(stats::terms(f), "term.labels")
					if(length(rhsTerms)==0){
						captionToUse = paste0("Effects on ", dv, " (intercept only)")
					} else if(length(rhsTerms)==1){
						captionToUse = paste0("Effects on ", dv, " of ", rhsTerms)
					} else if(length(rhsTerms)==2){
						captionToUse = paste0("Effects on ", dv, " of ", paste(rhsTerms, collapse=" and "))
					} else {
						captionToUse = paste0("Effects on ", dv, " of ", paste(rhsTerms[1:(length(rhsTerms)-1)], collapse=", "), ", and ", rhsTerms[length(rhsTerms)])
					}
				} else {
					captionToUse = NULL
				}
			}
			tmp= data.frame(summary(obj)$coefficients, check.names=FALSE)
			names(tmp)= c("Estimate", "SE", "t-value", "p-value")
			if(isTRUE(stars)){
				# add APA stars to p-value column for html tables: * p<.05, ** p<.01, *** p<.001
				pvals     = tmp[["p-value"]]
				starsVec  = vapply(pvals, getStars, character(1))
				formatted = vapply(seq_along(pvals), function(k) paste0(umx_APA_pval(pvals[k], addComparison=TRUE), ifelse(starsVec[k]!="", paste0(" ", starsVec[k]), "")), character(1))
				tmp[["p-value"]] = formatted
				# html with APA footnote *p < .05. **p < .01. ***p < .001
				tmpRounded = umx_round(tmp, digits = digits, coerce = FALSE)
				# umx_round skips character p-value column (already formatted with stars), so tmpRounded keeps formatted p-values
				captionToUse = paste0(captionToUse, "<p>Note: *p < .05. **p < .01. ***p < .001</p>")
				tmp = tmpRounded
			}
			umx_print(tmp, digits= digits, report = "html", caption = captionToUse)
		} else {
			sumry = summary(obj)
			conf  = confint(obj)
			if(is.null(se)){
				se = dimnames(sumry$coefficients)[[1]]
			}
			# aligned output: two-pass collect then format
			termVec = se
			bStrVec = character(length(termVec))
			loStrVec = character(length(termVec))
			hiStrVec = character(length(termVec))
			tStrVec = character(length(termVec))
			pStrVec = character(length(termVec))
			for (k in seq_along(termVec)) {
				i = termVec[k]
				lower   = conf[i, 1]
				upper   = conf[i, 2]
				b_and_p = sumry$coefficients[i, ]
				b       = b_and_p["Estimate"]
				tval    = b_and_p["t value"]
				pval    = b_and_p["Pr(>|t|)"]
				bStrVec[k]  = sprintf(paste0("%0.", digits, "f"), b)
				loStrVec[k] = sprintf(paste0("%0.", digits, "f"), lower)
				hiStrVec[k] = sprintf(paste0("%0.", digits, "f"), upper)
				tStrVec[k]  = sprintf(paste0("%0.", digits, "f"), tval)
				pStrVec[k]  = umx_APA_pval(pval, addComparison = TRUE)
				star = getStars(pval)
				if(star != "") pStrVec[k] = paste0(pStrVec[k], " ", star)
			}
			termW = max(nchar(termVec))
			bW = max(nchar(bStrVec))
			loW = max(nchar(loStrVec))
			hiW = max(nchar(hiStrVec))
			tW = max(nchar(tStrVec))
			for (k in seq_along(termVec)) {
				cat(paste0(format(termVec[k], width = termW, justify = "left"), betaSymbol,
					format(bStrVec[k], width = bW, justify = "right"),
					" [", format(loStrVec[k], width = loW, justify = "right"), commaSep, format(hiStrVec[k], width = hiW, justify = "right"), "], ",
					"t = ", format(tStrVec[k], width = tW, justify = "right"), ", p ", pStrVec[k], "\n"
				))
			}
			cat(paste0("R\u00B2 = ", round(sumry$r.squared, 3), " (adj = ", round(sumry$adj.r.squared, 3), ")"))
		}
		invisible(obj)
	} else if("glm" == class(obj)[[1]]) {
		# report glm summary table
		if(std){
			message("TODO: not sure how to not scale the DV in this glm model: Don't trust this")
			obj = update(obj, data = umx_scale(obj$model))
		}
		# TODO pick test based on family
		# Chisq = "binomial" "Poisson" (Chisq same as "LRT")
		# F = gaussian, quasibinomial, quasipoisson
		# Cp similar to AIC
		# see ?anova.glm 
		cat("Change in the log odds of the outcome for a one unit increase in the predictor variable:\n")
		model_coefficients = summary(obj)$coefficients
		conf = confint(obj)
		if(is.null(se)){
			se = dimnames(model_coefficients)[[1]]
		}
		# aligned output: two-pass
		termVec = se
		bStrVec = character(length(termVec))
		loStrVec = character(length(termVec))
		hiStrVec = character(length(termVec))
		zStrVec = character(length(termVec))
		pStrVec = character(length(termVec))
		for (k in seq_along(termVec)) {
			i = termVec[k]
			lower   = conf[i, 1]
			upper   = conf[i, 2]
			b_and_p = model_coefficients[i, ]
			b       = b_and_p["Estimate"]
			testStat = b_and_p["z value"]
			pval    = b_and_p["Pr(>|z|)"]
			bStrVec[k]  = sprintf(paste0("%0.", digits, "f"), b)
			loStrVec[k] = sprintf(paste0("%0.", digits, "f"), lower)
			hiStrVec[k] = sprintf(paste0("%0.", digits, "f"), upper)
			zStrVec[k]  = sprintf(paste0("%0.", digits, "f"), testStat)
			pStrVec[k]  = umx_APA_pval(pval, addComparison = TRUE)
				star = getStars(pval)
				if(star != "") pStrVec[k] = paste0(pStrVec[k], " ", star)
		}
		termW = max(nchar(termVec))
		bW = max(nchar(bStrVec))
		loW = max(nchar(loStrVec))
		hiW = max(nchar(hiStrVec))
		zW = max(nchar(zStrVec))
		for (k in seq_along(termVec)) {
			cat(paste0(format(termVec[k], width = termW, justify = "left"), " log(odds) = ",
			   format(bStrVec[k], width = bW, justify = "right"),
			   " [", format(loStrVec[k], width = loW, justify = "right"), commaSep, format(hiStrVec[k], width = hiW, justify = "right"), "], ",
			   "z = ", format(zStrVec[k], width = zW, justify = "right"), ", p ", pStrVec[k], "\n"
			))
		}
		if(obj$family$family == "binomial"){
			# https://stats.oarc.ucla.edu/r/dae/logit-regression/
			cat("\nAs ORs (odds ratios, rather than log(odds)):\n")


			model_ORs = exp(coef(obj)) # Odds Ratios OR
			confOR    = exp(conf)
			for (i in 1:length(model_ORs)) {
				lower    = confOR[i, 1]
				upper    = confOR[i, 2]
				OR       = model_ORs[i]
				testStat = model_coefficients[i, "z value"]
				pval     = model_coefficients[i, "Pr(>|z|)"]
				cat(paste0(se[i], " OR = ", round(OR, digits), " [", round(lower, digits), commaSep, round(upper, digits), "], ",
 			    "z = ", round(testStat, digits), ", p ", umx_APA_pval(pval, addComparison = TRUE), "\n"))
			}

			cat("\nAnd as probabilities...\n")
			for (i in 1:length(model_ORs)) {
				OR = model_ORs[i]
				cat(paste0(se[i], " probability = ", round(OR/(1+OR), digits), "\n"))
			}
		}
		cat(paste0("\nAIC = ", round(AIC(obj), 3) ))
	} else if( "lme" == class(obj)[[1]]) {
		# report nlme::lme() summary table
		if(std){
			obj = update(obj, data = umx_scale(obj$data))
		}
		model_coefficients = summary(obj)$tTable
		conf = intervals(obj, which = "fixed")[[1]]
		if(is.null(se)){
			se = dimnames(model_coefficients)[[1]]
		}
		# aligned output: two-pass
		termVec = se
		bStrVec = character(length(termVec))
		loStrVec = character(length(termVec))
		hiStrVec = character(length(termVec))
		tStrVec = character(length(termVec))
		pStrVec = character(length(termVec))
		dfVec = character(length(termVec))
		for (k in seq_along(termVec)) {
			i = termVec[k]
			lower = conf[i, "lower"]
			upper = conf[i, "upper"]
			b     = conf[i, "est."]
			tval  = model_coefficients[i, "t-value"]
			numDF = model_coefficients[i, "DF"]
			pval  = model_coefficients[i, "p-value"]
			bStrVec[k]  = sprintf(paste0("%0.", digits, "f"), b)
			loStrVec[k] = sprintf(paste0("%0.", digits, "f"), lower)
			hiStrVec[k] = sprintf(paste0("%0.", digits, "f"), upper)
			tStrVec[k]  = sprintf(paste0("%0.", digits, "f"), tval)
			pStrVec[k]  = umx_APA_pval(pval, addComparison = TRUE)
			star = getStars(pval)
			if(star != "") pStrVec[k] = paste0(pStrVec[k], " ", star)
			dfVec[k]    = as.character(numDF)
		}
		termW = max(nchar(termVec))
		bW = max(nchar(bStrVec))
		loW = max(nchar(loStrVec))
		hiW = max(nchar(hiStrVec))
		tW = max(nchar(tStrVec))
		for (k in seq_along(termVec)) {
			cat(paste0(format(termVec[k], width = termW, justify = "left"), betaSymbol,
			   format(bStrVec[k], width = bW, justify = "right"),
			   " [", format(loStrVec[k], width = loW, justify = "right"), commaSep, format(hiStrVec[k], width = hiW, justify = "right"), "], ",
			   "t(", dfVec[k], ") = ", format(tStrVec[k], width = tW, justify = "right"), ", p ", pStrVec[k], "\n"
			))
		}
		# return (possibly standardized) model
		invisible(obj)
	} else if(inherits(obj, "anova")){
	  # 2. check order
	  bad_order = FALSE
	  if(nrow(obj) >= 2 &&!is.na(obj$Df[2]) && obj$Df[2] < 0){
	    bad_order = TRUE
	    message("Note: anova has negative Df. For a cleaner table use anova(smaller, larger). Stats are identical, reporting |Df|.")
	  }

	  # 3. nice sentence - works either order
	  # full model's Res.Df is always the smaller one
	  comp_row = nrow(obj) # last comparison
	  df1      = abs(obj$Df[comp_row])
	  df2      = min(obj$Res.Df, na.rm = TRUE) # df of full model
	  Fval     = obj$F[comp_row]
	  pval     = obj$`Pr(>F)`[comp_row]

	  # APA p formatting
	  p_str = umx_APA_pval(pval)
	  apa = sprintf("F(%d, %d) = %.2f, p %s", df1, df2, Fval, ifelse(grepl("<", p_str), p_str, paste0("= ", p_str)))

	  was_str = ifelse(pval<.05, "significantly improved model fit", "did not significantly improve model fit")
	  paste0("Adding the additional predictors ", was_str, ", ", apa, ".")
	  # full sentence if you have heading
	  # attr(obj, "heading") contains the two formulas
	} else {
		if(is.null(se)){
			if(is.null(p)){
				# obj is likely a p value (p not provided separately which is what SE_from_p expects...)
				return(umx_APA_pval(obj, min = min, digits = digits, addComparison = addComparison))
			} else {
				# p-value provided but not SE
				se  = SE_from_p(beta = obj, p = p)
				str = paste0("\u03B2 = ", round(obj, digits), " [", round(obj - (1.96 * se), digits), commaSep, round(obj + (1.96 * se), digits), "]", suffix)
				if(!report %in% c("expression", "none")){ cat(str) }
				invisible(str)
			}
		} else if(length(se) == 2){
			# beta and CI
			# lower = b - (1.96 * se)
			# upper = b + (1.96 * se)
			if(report == "expression"){
				RCI = paste0(round(obj, digits), "SE = ", round((se[2] - se[1])/(1.96 * 2), digits), suffix)
				str = bquote(beta == .(RCI))
			}else{
				str = paste0("\u03B2 = ", round(obj, digits), " SE = ", round((se[2] - se[1])/(1.96 * 2), digits), suffix)
			}
			if(!report %in% c("expression", "none")){ cat(str) }
			invisible(str)
		} else {
			# obj = beta and SE
			if(report == "expression"){
				RCI = paste0(round(obj, digits), " [", round(obj - (1.96 * se), digits), commaSep, round(obj + (1.96 * se), digits), "]", suffix)
				str = bquote(beta == .(RCI))
			}else{
				str = paste0("\u03B2 = ", round(obj, digits), " [", round(obj - (1.96 * se), digits), commaSep, round(obj + (1.96 * se), digits), "]")
			}
			if(!report %in% c("expression", "none")){ cat(str) }
			invisible(str)
		}
	}
}

#' @export
summaryAPA <- umxAPA
