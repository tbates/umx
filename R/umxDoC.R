#' MZ differences method for testing evidence for causality.
#'
#' @description
#' `umxDiffMZ` implements the simple twin1-twin2 based correlation method, e.g. De Moor (2008), in which MZ differences
#' on a variable `x` asserted to be causal of an outcome variable `y` are tested for association with differences on y.
#' The logic of the design is shown below:
#'
#' \if{html}{\figure{DiffMZRainMud.png}{options: width=50% alt="Figure: MZ differences model"}}
#' \if{latex}{\figure{DiffMZRainMud.pdf}{options: width=7cm}}
#'
#' @details
#' Example output is shown below, with the fitted line and fit inscribed. The plot is just a ggplot graph that is returned and can be edited and formatted.
#'
#' \if{html}{\figure{DiffMZexample.png}{options: width=50% alt="Figure: MZ differences model"}}
#' \if{latex}{\figure{DiffMZexample.pdf}{options: width=7cm}}
#' 
#' For a more sophisticated linear mixed model approach, see [umxDiscTwin()].
#' 
#' @param x Presumed causal variable, e.g. "effort"
#' @param y Presumed caused outcome, e.g. "score"
#' @param data Dataframe containing the twin data.
#' @param sep The separator "_T" used to make twin var names from x and y.
#' @param mzZygs The MZ zygosity codes c("MZFF", "MZMM")
#' @param zyg The column containing "zygosity" data
#' @param labxy Where to locate the R2 label (default = c(x=-2,y=3))
#' @param xylim = clip x any axes to range, e.g c(-3,-3)
#' @param digits Rounding for beta (def2)
#' @return - Graph for decorating
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()], [umxDiscTwin()], [umxMR()]
#' @references - De Moor, M. H., Boomsma, D. I., Stubbe, J. H., Willemsen, G., & de Geus, E. J. (2008). Testing causality in the association between regular exercise and symptoms of anxiety and depression. Archives of General Psychiatry, 65(8), 897-905. \doi{10.1001/archpsyc.65.8.897}.
#' @md
#' @examples
#' data(twinData)
#' umxDiffMZ(x="ht", y="wt", labxy = c(-.5, 3), data = twinData, sep = "")
#' umxDiffMZ(x="ht", y="wt", xylim = c( -2, 2), data = twinData, sep = "")
umxDiffMZ <- function(x, y, data, sep = "_T", mzZygs = c("MZFF", "MZMM"), zyg = "zygosity", labxy = c(-1.2, 1.8),  xylim = c(NA, NA), digits = 2) {
	message("umxDiffMZ is pre-alpha quality: Internals are stubs and parameter names may change!")
	# 1. Expand names for ease of use
	x_T1 = paste0(x, sep, 1); x_T2 = paste0(x, sep, 2)
	y_T1 = paste0(y, sep, 1); y_T2 = paste0(y, sep, 2)

	# 2. Scale x and y
	df = umx_scale_wide_twin_data(varsToScale= c(x, y), sep = sep, data= data, twins = 1:2)	

	# 3. Make diff scores
	df$xDiff = df[, x_T1] - df[, x_T2]
	df$yDiff = df[, y_T1] - df[, y_T2]
	df$xMean = df[, x_T1] + df[, x_T2]
	validRows = df[, zyg] %in% mzZygs
	mzData = df[validRows, ]
	
	# 4. Plot
	p = ggplot(aes(x = xDiff, y = yDiff), data = mzData)  + geom_jitter() # + geom_jitter(shape="circle open") # + geom_count(shape="circle open") 
	# p = p + labs(title= "MZ twin intra-pair differences model", x = paste0(x, " \u0394 (Twin 1 - Twin 2)"), y = paste0(y, " \u0394 (Twin 1 - Twin 2)"))
	p = p + labs(title= "MZ twin intra-pair differences model")
	p = p + labs(x = paste("Difference in ", x, " (T1 - T2)")) 
	p = p + labs(y = paste("Difference in ", y, " (T1 - T2)"))
	p = p + geom_smooth()
	# p = p + geom_abline(slope = 1, intercept = 0, linetype = "dotdash", color = "grey")
	# p = p + geom_vline(xintercept = 0, linetype = "dotted", color = "grey")
	p = p + geom_hline(yintercept = 0, linetype = "dotted", color = "grey")
	if(any(is.na(xylim))){
		#xylim not set or not valid
	}else{
		p = p + coord_cartesian(xlim = xylim, ylim = xylim, expand = FALSE)
	}
	
	# model = lm(yDiff ~ xDiff, data = mzData)
	sumry  = summary(lm(yDiff ~ xDiff, data = mzData))
	beta   = sumry$coefficients["xDiff", "Estimate"]
	SE     = sumry$coefficients["xDiff", "Std. Error"]
	pvalue = sumry$coefficients["xDiff", "Pr(>|t|)"]
	R2     = round(sumry$r.squared, 3)
	pvalStr = paste0(", p ", umxAPA(pvalue, addComparison = TRUE, digits = digits, report = "none"))
	blurb  = umxAPA(beta, se=SE, report = "expression", suffix = pvalStr)
	p = p + annotate("text", x = labxy[1], y = labxy[2], label = deparse(blurb), parse = TRUE)
	p = p + theme_bw() # + hrbrthemes::theme_ipsum()
	print(p)
}

#' Intra-pair association in MZ, DZ  twin models. (ALPHA quality!)
#'
#' @description
#' Testing causal claims is often difficult due to an inability to experimentally randomize traits and situations.
#' A combination of control data and data from twins discordant for the putative causal trait can falsify causal hypotheses.
#' 
#' `umxDiscTwin` uses [nlme::nlme()] to compute the beta for x in `y ~ x` in models either a) Only controlling non-independence, 
#' and b) MZ and DZ subsample models in which the family level of the predictor y is also controlled.
#' 
#' If `x` is causal, then the effect size of x on y is expected to be equally large in all three samples.
#' If the population association reflects confounded genes or shared environments,
#' then the association in MZ twins will reduce to zero/non-significance.
#' 
#' \if{html}{\figure{discordantCausalPatterns.png}{options: width=50% alt="Figure: Types of confounding"}}
#' \if{latex}{\figure{discordantCausalPatterns.pdf}{options: width=7cm}}
#' 
#' The function uses the [nlme::lme()] function to compute the effect of the presumed causal variable on the outcome, 
#' controlling, for mid-family score and with random means model using familyID. e.g.:
#' 
#' `mzModel  = lme(fixed = y ~ x + FamMeanX, random = ~ 1+FamMeanX|FAMID, data = umx_scale(MZ), na.action = "na.omit")`
#' 
#' **Example output from `umxDiscTwin`**
#' 
#' \if{html}{\figure{DiscTwinsExample.png}{options: width=50% alt="Figure: Causation in Discordant twins"}}
#' \if{latex}{\figure{DiscTwinsExample.pdf}{options: width=7cm}}
#' 
#' @param x Cause
#' @param y Effect
#' @param data dataframe containing MZ and DZ data
#' @param mzZygs MZ zygosities c("MZFF", "MZMM")
#' @param dzZygs DZ zygosities c("DZFF", "DZMM", "DZOS")
#' @param FAMID The column containing family IDs (default = "FAMID")
#' @param out Whether to return the table or the ggplot (if you want to decorate it)
#' @param use NA handling in corr.test (default= "complete.obs")
#' @param sep The separator in twin variable names, default = "_T", e.g. "dep_T1".
#' @return - table of results
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()], [umxDiffMZ()], [umxMR()]
#' @references - Begg, M. D., & Parides, M. K. (2003). Separation of individual-level and cluster-level covariate effects in regression analysis of correlated data. Stat Med, 22(16), 2591-2602. \doi{10.1002/sim.1524} 
#' * Bergen, S. E., Gardner, C. O., Aggen, S. H., & Kendler, K. S. (2008). Socioeconomic status and social support following illicit drug use: causal pathways or common liability? *Twin Res Hum Genet*, **11**, 266-274. \doi{10.1375/twin.11.3.266}
#' * McGue, M., Osler, M., & Christensen, K. (2010). Causal Inference and Observational Research: The Utility of Twins. *Perspectives on Psychological Science*, **5**, 546-556. \doi{10.1177/1745691610383511}
#' @md
#' @examples
#' \dontrun{
#' data(twinData)
#' # add to test must set FAMID umxDiscTwin(x = "ht", y = "wt", data = twinData, sep="")
#' tmp = umxDiscTwin(x = "ht", y = "wt", data = twinData, sep="", FAMID = "fam")
#' print(tmp, digits = 3)
#' }
umxDiscTwin <- function(x, y, data, mzZygs = c("MZFF", "MZMM"), dzZygs = c("DZFF", "DZMM", "DZOS"), FAMID = "FAMID", out = c("table", "plot", "model"), use = "complete.obs", sep = "_T") {
	message("umxDiscTwin is pre-alpha quality: Internals are just stubs and parameter names may change!")
	out = match.arg(out)
	
	updateDB <- function(xLevel = "e.g. MZ", model, x, FamMeanX = "FamMeanX", group = NA, row = NULL, input = NULL) {
		if(is.numeric(input)){
			nrow = input
			return(data.frame(
				xLevel   = rep(NA,nrow), N         = rep(NA,nrow), 
				Bwithin  = rep(NA,nrow), ci.lower  = rep(NA,nrow), ci.upper = rep(NA,nrow),
				tval     = rep(NA,nrow), pval      = rep(NA,nrow),
				Bbetween = rep(NA,nrow), SEbetween = rep(NA,nrow)) 
			)
		}
		if(is.null(row)){ row = which.max(is.na(input$xLevel)) }

		conf = intervals(model, which = "fixed")[["fixed"]]
		model_coefficients      = summary(model)$tTable
		input[row, "xLevel"]    = xLevel               # e.g. "MZ"
		input[row, "N"]         = model$fixDF$terms[x] # df
		input[row, "Bwithin"]   = conf[x, "est." ]     # intra-pair effect
		input[row, "ci.lower"]  = conf[x, "lower"]     # CI[lower]
		input[row, "ci.upper"]  = conf[x, "upper"]     # CI[,upper]
		input[row, "tval"]      = model_coefficients[x, "t-value"]
		input[row, "pval"]      = model_coefficients[x, "p-value"]

		junk = tryCatch({
			input[row, "Bbetween"]  = model$coefficients$fixed["FamMeanX"]        # between family beta
			input[row, "SEbetween"] = model_coefficients["FamMeanX", "Std.Error"] # between family SE
		}, warning = function(x) {
		    # ignored
		}, error = function(x) {
		    # ignored
		}, finally={
		    # ignored
		})
		return(input)
	}
	r_df = updateDB(input= 3) # Create and initialise the database.

	# 1. Compute the mean for each family, and add variable
	data$FamMeanX = rowMeans(data[, tvars(x, sep = sep)], na.rm = TRUE)

	# recode x to Diff
	data[, paste0("deltaX", sep, 1)] = data[, paste0(x, sep, 1)] - data$FamMeanX
	data[, paste0("deltaX", sep, 2)] = data[, paste0(x, sep, 2)] - data$FamMeanX

	# 2. Check inputs
	# data = twinData; x = "ht"; y = "wt"; FAMID = "fam"; sep = ""
	# TODO: check zyg levels present?
	if(FAMID == "FAMID"){
		umx_check_names("FAMID", data = data, message = "Please set 'FAMID=' to your column containing family IDs")
	} else {
		umx_check_names(FAMID, data = data, message = c("Could not find your FAMID column ", omxQuotes('FAMID')) )
		data$FAMID = data[, FAMID]
	}
	neededVars = c(tvars(c(x, y, "deltaX"), sep = sep), "FAMID", "FamMeanX")
	umx_check_names(neededVars, data = data)

	popData = umx_wide2long(data = data[, neededVars], sep = sep)
	dzData  = umx_wide2long(data = data[data$zyg %in% dzZygs, neededVars], sep = sep)
	mzData  = umx_wide2long(data = data[data$zyg %in% mzZygs, neededVars], sep = sep)

	# TODO create T1 for non-pair comparison

	# 2. Run nlme::lme with formula created from user's x and y, e.g. : IQ ~ EffortMean + SOSeffort
	formula = reformulate(termlabels = c("FamMeanX", x), response = y)

	# obj = lme(reformulate(termlabels = x, response = y), random = ~ 1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(formula, random = ~ 1|FAMID, data = umx_scale(dzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ", model= obj, x= x, input = r_df)
	# obj = lme(formula, random = ~ 1|FAMID, data = umx_scale(mzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ", model= obj, x= x, input = r_df)

	# obj = lme(IQ ~ SOSeffort        , random = ~ 1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ deltaX + FamMeanX, random = ~ 1|FAMID, data = umx_scale(dzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ" , model= obj, x= "deltaX", input = r_df)
	# obj = lme(IQ ~ deltaX + FamMeanX, random = ~ 1|FAMID, data = umx_scale(mzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ" , model= obj, x= "deltaX", input = r_df)

	# obj = lme(IQ ~ SOSeffort, random = ~        1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ deltaX   , random = ~ FamMeanX|FAMID, data = umx_scale(dzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ" , model= obj, x= "deltaX", input = r_df)
	# obj = lme(IQ ~ deltaX   , random = ~ FamMeanX|FAMID, data = umx_scale(mzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ" , model= obj, x= "deltaX", input = r_df)

	# Begg & Parides (2008) Model #3 
	# h(E[Yij|Xij;X􏰄i)=􏰀2 +􏰁2Xij +􏰂2X􏰄i
	# X&‌#772;
	
	obj = lme(IQ ~ SOSeffort           , random = ~        1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ FamMeanX|FAMID, data = umx_scale(dzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ" , model= obj, x= x, input = r_df)
	obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ FamMeanX|FAMID, data = umx_scale(mzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ" , model= obj, x= x, input = r_df)

	# also good
	# obj = lme(IQ ~ SOSeffort           , random = ~ 1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ 1|FAMID, data = umx_scale(dzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ" , model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ 1|FAMID, data = umx_scale(mzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ" , model= obj, x= x, input = r_df)

	# obj = lme(IQ ~ SOSeffort           , random = ~        1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ FamMeanX|FAMID, data = umx_scale(dzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort + FamMeanX, random = ~ FamMeanX|FAMID, data = umx_scale(mzData) , na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ", model= obj, x= x, input = r_df)

	# obj = lme(IQ ~ SOSeffort, random = ~        1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort, random = ~ FamMeanX|FAMID, data = umx_scale(dzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort, random = ~ FamMeanX|FAMID, data = umx_scale(mzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ", model= obj, x= x, input = r_df)

	# obj = lme(IQ ~ SOSeffort, random = ~ 1|FAMID, data = umx_scale(popData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "Pop", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort, random = ~ 1|FAMID/FamMeanX, data = umx_scale(dzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "DZ", model= obj, x= x, input = r_df)
	# obj = lme(IQ ~ SOSeffort, random = ~ 1|FAMID/FamMeanX, data = umx_scale(mzData), na.action = "na.omit", control = list(opt= "optim")); r_df = updateDB(xLevel = "MZ", model= obj, x= x, input = r_df)

	r_df$xLevel = factor(r_df$xLevel, levels = c("Pop", "DZ", "MZ"))
	p = ggplot(r_df, aes(x = xLevel, y = Bwithin, fill = xLevel))
	p = p + geom_bar(position = position_dodge(), stat = "identity", size = .3, colour = "black") # 3 = thin
	p = p + geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), size = .3, width = .2, position = position_dodge(.9)) # Thinner lines
	p = p + xlab("Zygosity") + ylab(expression(beta ~ or ~ beta ~ within ))
	p = p + ggtitle(paste0("Estimated effect of ", x, " on ", y)) + theme_bw()
	# p = p + scale_y_continuous(breaks = 0:20 * 4)
	# Legend label, use darker colors
	p = p + scale_fill_hue(name= "Group", breaks= c("Pop", "MZ", "DZ"), labels= c("Unselected", "DZ within family", "MZ within family"))
	print(p)
	if(out == "plot"){
		return(p)
	} else if(out == "table"){
		return(r_df)
	} else if(out == "model"){
		return(obj)
	}
}

#' Build and run a 2-group Direction of Causation twin models.
#'
#' @description
#' Testing causal claims is often difficult due to an inability to conduct experimental randomization of traits and 
#' situations to people. When twins are available, even when measured on a single occasion, the pattern of cross-twin
#' cross-trait correlations can (given distinguishable modes of inheritance for the two traits) falsify causal hypotheses.
#' 
#' `umxDoC` implements a 2-group model to form latent variables for each of two traits, and allows testing whether 
#' trait 1 causes trait 2, vice-versa, or even reciprocal causation.
#' 
#' Using latent variables instead of a manifest measure for testing causation, avoids the bias created by differences in 
#' measurement error in which the more reliable measure appears to "cause" the less reliable one (Gillespie and Martin, 2005).
#' 
#' The following figure shows how the DoC model appears as a path diagram (for two latent variables X and Y,
#' each with three indicators). Note: For pedagogical reasons, only the model for 1 twin is shown, and only one DoC pathway drawn.
#' 
#' \if{html}{\figure{DoC.png}{options: width=50% alt="Figure: Direction of Causation"}}
#' \if{latex}{\figure{DoC.pdf}{options: width=7cm}}
#'
#' @param name The name of the model (defaults to "DOC").
#' @param var1Indicators variables defining latent trait 1
#' @param var2Indicators variables defining latent trait 2
#' @param causal whether to add the causal paths (default TRUE)
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param sep The separator in twin variable names, default = "_T", e.g. "dep_T1".
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param data = NULL If building the MZ and DZ datasets internally from a complete data set.
#' @param zyg = "zygosity" (for the data= method of using this function)
#' @return - [OpenMx::mxModel()] of subclass MxModelDoC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDiscTwin()]
#' @references - N.A. Gillespie and N.G. Martin (2005). Direction of Causation Models. In *Encyclopedia of Statistics in Behavioral Science*, **1**. 496–499. Eds. Brian S. Everitt & David C. Howell.
#' * McGue, M., Osler, M., & Christensen, K. (2010). Causal Inference and Observational Research: The Utility of Twins. *Perspectives on Psychological Science*, **5**, 546-556. \doi{10.1177/1745691610383511}
#' * Rasmussen, S. H. R., Ludeke, S., & Hjelmborg, J. V. B. (2019). A major limitation of the direction of causation model: non-shared environmental confounding. *Twin Res Hum Genet*, **22**, 1-13. \doi{10.1017/thg.2018.67}
#' @md
#' @examples
#' \dontrun{
#' 
#' # ========================
#' # = Does Rain cause Mud? =
#' # ========================
#'
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' docData = umx_scale_wide_twin_data(c(var1, var2), docData, sep= "_T")
#' mzData  = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData  = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#'
#' # =======================================
#' # = 2. Define manifests for var 1 and 2 =
#' # =======================================
#' var1 = paste0("varA", 1:3)
#' var2 = paste0("varB", 1:3)
#'
#' # =======================================================
#' # = 3. Make the non-causal (Cholesky) and causal models =
#' # =======================================================
#' Chol = umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= FALSE)
#' # nb: DoC initially has causal paths fixed @0
#' DoC  = umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= TRUE)
#' a2b   = umxModify(DoC, "a2b", free = TRUE, name = "a2b"); summary(a2b)
#' b2a   = umxModify(DoC, "b2a", free = TRUE, name = "b2a"); summary(b2a)
#' Recip = umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip"); summary(Recip)
#'
#' # Compare fits
#' umxCompare(Chol, c(a2b, b2a, Recip))
#'
#' # ==========================================
#' # = Alternative call with data in one file =
#' # ==========================================
#' data(docData)
#' docData = umx_scale_wide_twin_data(c(var1, var2), docData, sep= "_T")
#' DoC = umxDoC(var1= paste0("varA", 1:3), var2= paste0("varB", 1:3),
#' 	  mzData= c("MZFF", "MZMM"), dzData= c("DZFF", "DZMM"), data = docData
#' )
#' }
umxDoC <- function(name = "DoC", var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = "_T", causal= TRUE, autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, data = NULL, zyg = "zygosity") {
	# TODO: umxDoC add some name checking to avoid variables like "a1"
	if(name == "DoC"){name = ifelse(causal, "DoC", "Chol")}
	tryHard = match.arg(tryHard)
	umx_check(is.logical(causal), "stop", "causal must be TRUE or FALSE")
		
	if (!is.null(data)) {
	  if ("tbl" %in% class(data)) {
	    data = as.data.frame(data)
	  }
	  mzData = data[data[, zyg] %in% ifelse(is.null(mzData),"MZ", mzData), ]
	  dzData = data[data[, zyg] %in% ifelse(is.null(dzData),"DZ", dzData), ]
	} else {
	  if ("tbl" %in% class(mzData)) {
	    mzData = as.data.frame(mzData)
	    dzData = as.data.frame(dzData)
	  }
	}
	nSib    = 2 # Number of siblings in a twin pair.
	nLat    = 2 # 2 latent variables

	nLat1   = length(var1Indicators) # measures for factor 1
	nLat2   = length(var2Indicators)
	nVar    = nLat1 + nLat2
	selVars = tvars(c(var1Indicators, var2Indicators), sep=sep)
	mzData = xmu_make_mxData(mzData, manifests = selVars)
	dzData = xmu_make_mxData(dzData, manifests = selVars)
	xmu_twin_check(selDVs= c(var1Indicators,var2Indicators), sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)

	# ========================
	# = Make Factor Loadings =
	# ========================
	# 1. Make matrix, initialised to fixed @ 0
	FacLoad = umxMatrix(name="FacLoad", "Full", nrow=nVar, ncol=nLat, free= FALSE, values = 0)
	# 2. Set FacLoad manifest loadings to pattern of 0 and 1
	FacLoad$free[1:nLat1                  ,1] = TRUE
	FacLoad$values[1:nLat1                ,1] = 1
	FacLoad$free[(nLat1+1):(nLat1+nLat2)  ,2] = TRUE
	FacLoad$values[(nLat1+1):(nLat1+nLat2),2] = 1


	top = mxModel("top", # (was "ACE")
		umxMatrix("dzAr" , "Full", nrow=nLat, ncol=nLat, free=FALSE, values= c(1,.5,.5,1) ), # Heredity Matrix for DZ
		umxMatrix("Ones" , "Full", nrow=nLat, ncol=nLat, free=FALSE, values= 1 ),            # Unit Matrix - For Com Env and MZ
		umxMatrix("Diag1", "Iden", nrow=nSib, ncol=nSib),                                    # Identity matrix (2by2: 1s on diag, 0 off diag)

		# Matrices for Cholesky (swapped out after if causal)
		umxMatrix("a", type="Lower", nrow=nLat, ncol=nLat, free= TRUE, values= .2),               # Genetic effects on Latent Variables 
		umxMatrix("c", type="Lower", nrow=nLat, ncol=nLat, free= TRUE, values= .2),               # Common env effects on Latent Variables
		umxMatrix("e", type="Lower", nrow=nLat, ncol=nLat, free= c(FALSE,TRUE,FALSE), values= 1), # Non-shared env effects on Latent Variables 

		# 4x4 Matrices for A, C, and E
		mxAlgebra(name="A"  , Ones  %x% (a %*% t(a))),
		mxAlgebra(name="Adz", dzAr  %x% (a %*% t(a))),
		mxAlgebra(name="C"  , Ones  %x% (c %*% t(c))),
		mxAlgebra(name="E"  , Diag1 %x% (e %*% t(e))),
		mxAlgebra(name="Vmz", A   + C + E),
		mxAlgebra(name="Vdz", Adz + C + E),

		# Generate the Asymmetric Matrix
		# Non-shared env effects on Latent Variables 
		umxMatrix("beta", "Full", nrow=nLat, ncol=nLat, free=FALSE, labels = c("a2a", "a2b", "b2a", "b2b"), values= 0),
		mxAlgebra(name= "cause", Diag1 %x% solve(Diag1 - beta)), 	

		# Generate the Factor Loading Matrix
		FacLoad,
		mxAlgebra(name="FacLoadtw", Diag1 %x% FacLoad),

		# Covariance between the items due to the latent factors
		mxAlgebra(name= "FacCovMZ", FacLoadtw %&% (cause %&% Vmz)),
		mxAlgebra(name= "FacCovDZ", FacLoadtw %&% (cause %&% Vdz)),

		# Matrices for specific a, c, and e path coefficients (residuals for each manifest)
		# TODO: smart var starts here
		umxMatrix("as", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix("cs", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix("es", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3, lbound=1e-5),

		mxAlgebra(name= "Asmz", Ones  %x% as),
		mxAlgebra(name= "Asdz", dzAr  %x% as),
		mxAlgebra(name= "Cstw", Ones  %x% cs),
		mxAlgebra(name= "Estw", Diag1 %x% es),
		mxAlgebra(name= "specCovMZ", Asmz + Cstw + Estw),
		mxAlgebra(name= "specCovDZ", Asdz + Cstw + Estw),

		# Expected Covariance Matrices for MZ and DZ
		mxAlgebra(name= "expCovMZ", FacCovMZ + specCovMZ, dimnames = list(selVars, selVars)),
		mxAlgebra(name= "expCovDZ", FacCovDZ + specCovDZ, dimnames = list(selVars, selVars)),
		
		# Means model
		# TODO: Better starts for means... (easy)
		umxMatrix(name= "Means", "Full", nrow= 1, ncol= nVar, free= TRUE, values= .1),
		mxAlgebra(name= "expMean", cbind(top.Means, top.Means))
		# TODO Why not just make ncol = nCol*2 and allow label repeats the equate means? Alg might be more efficient?
	)

	MZ = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames= selVars), mxFitFunctionML() )
	DZ = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", means= "top.expMean", dimnames= selVars), mxFitFunctionML() )

	if(causal){
		# ===================
		# = DOC-based model =
		# ===================
		# Replace lower ace Matrices with diag.
		# Now that covariance between the traits is "caused", theses matrices are diagonal
		# (no cross paths = no need for lower)
		top = mxModel(top,
			umxMatrix("a", "Diag", nrow=nLat, ncol=nLat, free=TRUE,  values= 0.2), # Genetic effects on Latent Variables 
			umxMatrix("c", "Diag", nrow=nLat, ncol=nLat, free=TRUE,  values= 0.2), # Common env effects on Latent Variables
			umxMatrix("e", "Diag", nrow=nLat, ncol=nLat, free=FALSE, values= 1.0)  # E@1 
		)
	}
	model = mxModel(name, top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")) )

	# Factor loading matrix of Intercept and Slope on observed phenotypes
	# SDt = mxAlgebra(name= "SDt", solve(sqrt(Diag1 *Rt))) # Standardized deviations (inverse)
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelDoC") # set class so that S3s dispatch e.g. plot()
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE)
	return(model)
}


#' Plot a Direction of Causation Model.
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the *Rg* (genetic and environmental correlations), and show confidence intervals.
#' *note*: `std` is not implemented as yet.
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases plot.MxModelDoC
#' @param x a [umxDoC()] model to display graphically
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param showFixed Whether to graph paths that are fixed but != 0 (default = TRUE)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param format = c("current", "graphviz", "DiagrammeR")
#' @param SEstyle report "b (se)" instead of "b \[lower, upper\]" when CIs are found (Default FALSE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Other parameters to control model summary.
#' @references - <https://tbates.github.io>
#' @return - Optionally return the dot code
#' @export
#' @family Plotting functions
#' @seealso - [umxDoC()], [umxSummary.MxModelDoC()], [umxModify()]
#' @md
#' @examples
#'
#' \dontrun{
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' mzData = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' 
#' # =======================================
#' # = 2. Define manifests for var 1 and 2 =
#' # =======================================
#' var1 = paste0("varA", 1:3)
#' var2 = paste0("varB", 1:3)
#'
#' # =======================================================
#' # = 2. Make the non-causal (Cholesky) and causal models =
#' # =======================================================
#' Chol= umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= FALSE)
#' DoC = umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= TRUE)
#'
#' # ================================================
#' # = Make the directional models by modifying DoC =
#' # ================================================
#' a2b = umxModify(DoC, "a2b", free = TRUE, name = "A2B")
#' plot(a2b)
#' 
#' }
umxPlotDoC <- function(x = NA, means = FALSE, std = FALSE, digits = 2, showFixed = TRUE, file = "name", format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = FALSE, ...) {
	message("beta code")
	# 1. ✓ draw latents
	# 2. ✓ draw manifests,
	# 3. ✓ draw ace to latents
	# 4. ✓ draw specifics to manifests (? or omit?)
	# 5. ✓ connect latents to manifests using free elements of columns of FacLoad
	# 6. add causal paths between latents

	format = match.arg(format)
	model = x # just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelDoC", callingFn = "umxPlotDoC")

	if(std){
		message("I'm sorry Dave, no std for DoC yet ;-(")
		# model = xmu_standardize_DoC(model)
	}

	nFac   = dim(model$top$a_cp$labels)[[1]]
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end
	out    = list(str = "", latents = c(), manifests = c())
	selLat = c("a", "b")

	# Process [ace] matrices
	# 1. Collect latents
	out = xmu_dot_mat2dot(model$top$a, cells = "diag", from = "rows", toLabel = selLat, fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$c, cells = "diag", from = "rows", toLabel = selLat, fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$e, cells = "diag", from = "rows", toLabel = selLat, fromType = "latent", showFixed = showFixed, p = out)

	# 2. Process "FacLoad" nVar * nFac matrix of common latents onto manifests.
	out = xmu_dot_mat2dot(model$top$FacLoad, cells= "any", toLabel= selDVs, from= "cols", fromLabel= selLat, fromType= "latent", showFixed = showFixed, p= out)

	# 3. Process "as" matrix
	out = xmu_dot_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)

	# betas are in model$top$beta$labels
	#      [,1]  [,2]
	# [1,] "a2a" "b2a"
	# [2,] "a2b" "b2b"
	out = xmu_dot_mat2dot(model$top$beta, cells = "any", toLabel = selLat, from = "cols", fromType = "latent", showFixed = showFixed, p = out, fromLabel=selLat)

	# Process "expMean" 1 * nVar matrix # from = "one"; target = selDVs[c]
	if(means){
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out)
	}
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace][1-2]$"  , "min")
	same    = xmu_dot_rank(out$latents, "^[ab]$"        , "same")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s[0-9]+$", "max") # specifics

	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut,
		out$str,
		"\n", top, same, bottom, "\n}"
	)
	
	cat("\n?umxPlotDoC options: std=, means=, digits=, strip_zero=, file=, min=, max =")
	if(format != "current"){ umx_set_plot_format(format) }
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

#' @export
plot.MxModelDoC <- umxPlotDoC


#' Shows a compact, publication-style, summary of a umx Direction of Causation model
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelDoC
#' @param model a fitted [umxDoC()] model to summarize.
#' @param digits round to how many digits (default = 2).
#' @param comparison Run mxCompare on a comparison model (default NULL)
#' @param std Whether to standardize the output (default = TRUE).
#' @param showRg = whether to show the genetic correlations (FALSE).
#' @param CIs Whether to show Confidence intervals if they exist (TRUE).
#' @param report Print tables to the console (as 'markdown'), or open in browser ('html')
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output.
#' @param returnStd Whether to return the standardized form of the model (default = FALSE).
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @return - optional [OpenMx::mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxDoC()], [plot.MxModelDoC()], [umxModify()], [umxCP()], [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @md
#' @examples
#' \dontrun{
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' mzData = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' 
#' # =======================================
#' # = 2. Define manifests for var 1 and 2 =
#' # =======================================
#' var1 = paste0("varA", 1:3)
#' var2 = paste0("varB", 1:3)
#'
#' # =======================================================
#' # = 2. Make the non-causal (Cholesky) and causal models =
#' # =======================================================
#' Chol= umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= FALSE)
#' DoC = umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= TRUE)
#'
#' # ================================================
#' # = Make the directional models by modifying DoC =
#' # ================================================
#' A2B = umxModify(DoC, "a2b", free = TRUE, name = "A2B")
#' A2B = umxModify(DoC, "a2b", free = TRUE, name = "A2B", comp=TRUE)
#' B2A = umxModify(DoC, "b2a", free = TRUE, name = "B2A", comp=TRUE)
#' umxCompare(B2A, A2B)
#' 
#' }
umxSummaryDoC <- function(model, digits = 2, comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE , report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, zero.print = ".", ...) {
	message("Summary support for DoC models not complete yet. Feedback welcome at http://github.com/tbates/umx/issues if you are using this.")

	# TODO: Allow "a2b" in place of causal to avoid the make/modify 2-step
	# TODO: Detect value of DZ covariance, and if .25 set "C" to "D" in tables
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message(paste("Output for Model: ", thisFit$name))
			umxSummaryDoC(thisFit, digits = digits, comparison = comparison, file = file, returnStd = returnStd, showRg = showRg, report = report, std = std, CIs = CIs)
		}
	} else {
		umx_check_model(model, "MxModelDoC", beenRun = TRUE, callingFn = "umxSummaryDoC")
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		
		nFac     = dim(model$top$a$labels)[[1]]
		nVar     = dim(model$top$as$values)[[1]]
		selDVs   = dimnames(model$MZ$data$observed)[[2]]
		selDVs   = selDVs[1:(nVar)]
		selDVs   = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

		if(CIs){
			message("CIs not supported for DoC models yet")
			# oldModel = model # Cache this in case we need it (CI stash model has string where values should be).
			# model = xmu_CI_stash(model, digits = digits, dropZeros = TRUE, stdAlg2mat = TRUE)
		} else if(any(c(std, returnStd))) {
			message("Std not support for DoC models yet")
			# model = xmu_standardize_Doc(model) # Make a standardized copy of model
		}

		means = model$top$Means$values
		colnames(means) = selDVs[1:nVar]
		message("Table: Means")
		umx_print(means)
		
		betaNames  = as.vector(model$top$beta$labels)
		betaValues = as.vector(model$top$beta$values)
		message("Table: Causal paths")
		umx_print(data.frame(beta = betaNames, value = betaValues))

		ptable = summary(model)$parameters
		message("Table: Parameter list")
		umx_print(ptable[, c("name", "Estimate", "Std.Error")])

		return("I returned from line 693, which is where implementation of umxSummary for DoC models is up to :-)")
		# model$top$beta$labels[model$top$beta$free]
		# umx_print(ptable[, c("name", "Estimate", "Std.Error")])

		# Print Common Factor paths
		a_cp = model$top$a_cp$values # nFac * nFac matrix of path coefficients flowing into cp_loadings
		c_cp = model$top$c_cp$values
		e_cp = model$top$e_cp$values

		# Common factor ACE inputs are std to 1
		# Bind diags of a_cp, c and e columns into nFac-row matrix
		commonACE = cbind(diag(a_cp), diag(c_cp), diag(e_cp)) 
		commonACE = data.frame(commonACE, row.names = paste("Common.factor", 1:nFac, sep = "."), stringsAsFactors = FALSE);
		names(commonACE) = c ("A", "C", "E")
		if(report == "html"){
			umx_print(commonACE, digits = digits, zero.print = ".", file = "std_spec.html")
		} else {
			umx_print(commonACE, digits = digits, zero.print = ".")
		}
		message("Table: Common Factor paths")
		
		if(class(model$top$matrices$a_cp)[1] == "LowerMatrix"){
			message("You used correlated genetic inputs to the common factor. This is the a_cp matrix")
			print(a_cp)
		}
		
		# Get standardized loadings on Common factors
		rowNames = sub("(_T)?1$", "", selDVs[1:nVar]) # Clean up names
		cp_loadings = model$top$cp_loadings$values # nVar * nFac matrix
		cp_loadings = data.frame(cp_loadings, row.names = rowNames, stringsAsFactors = FALSE);
		names(cp_loadings) = paste0("CP", 1:length(names(cp_loadings)))
		if(report == "html"){
			umx_print(cp_loadings, digits = digits, zero.print = ".", file = "std_common.html");
		} else {
			umx_print(cp_loadings, digits = digits, zero.print = ".")
		}
		message("Table: Loading of each trait on the Common Factors")

		# Specific path coefficients ready to be stacked together
		as = model$top$as$values # Specific factor path coefficients
		cs = model$top$cs$values
		es = model$top$es$values

		specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')), stringsAsFactors = FALSE,
			rbind(diag(as), 
				  diag(cs),
				  diag(es))
		)
		names(specifics) = rowNames;

		if(report == "html"){
			umx_print(specifics, digits = digits, zero.print = ".", file = "std_spec.html")
		} else {
			umx_print(specifics, digits = digits, zero.print = ".")
		}
		message("Table: Specific-factor loadings")
		
		if(showRg) {
			# Pre & post multiply covariance matrix by inverse of standard deviations
			A  = model$top$A$values # Variances
			C  = model$top$C$values
			E  = model$top$E$values
			Vtot = A + C + E; # Total variance
			nVarIden = diag(nVar)
			NAmatrix <- matrix(NA, nVar, nVar);

			rA = tryCatch(solve(sqrt(nVarIden * A)) %*% A %*% solve(sqrt(nVarIden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(nVarIden * C)) %*% C %*% solve(sqrt(nVarIden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(nVarIden * E)) %*% E %*% solve(sqrt(nVarIden * E)), error = function(err) return(NAmatrix)); # E correlations
			genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
			# Make a table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			if(report == "html"){
				umx_print(genetic_correlations, digits = digits, zero.print = ".", file = "geneticCorrs.html")
			} else {
				umx_print(genetic_correlations, digits = digits, zero.print = ".")
			}
			message("Table: Genetic Correlations")
			
		}
		if(!is.na(file)){
			# umxPlotCP(model, file = file, digits = digits, std = FALSE, means = FALSE)
		}
		if(returnStd) {
			invisible(model)
		}
	}
}

#' @export
umxSummary.MxModelDoC <- umxSummaryDoC

