#' Purcell (2002) Bivariate GxE model: Suitable when twins differ on the moderator.
#'
#' GxE interaction models test the hypothesis that the strength of genetic 
#' and environmental influences vary parametrically across levels of a measured environment.
#' 
#' Whereas univariate [umxGxE()] models assume the twins share the moderator,
#' or have zero correlation on the moderator, [umxGxEbiv()] allows testing moderation in 
#' cases where members of a twin pair differ on the moderator, (Purcell, 2002; van der Sluis et al., 2012).
#'
#' This is the same model we teach at Boulder.
#'
#' The following figure shows this bivariate GxE model as a path diagram (Twin 1 shown). Whereas
#' the univariate model incorporates the moderator in the means model, the bivariate model incorporates
#' the moderator as a first class variable, with its own ACE structure, shared pathways to the trait of interest,
#' and the ability to moderate both specific and shared A, C, and E, influences on the trait of interest.
#' 
#'
#' ![](GxEbiv.png)
#'
#' Twin 1 and twin 2 A, C, and E latent traits are connected in the standard fashion, with the
#' covariance of the T1 and T2 latent genetic traits set to .5 for DZ and 1.0 for MZ pairs.
#' For the sake of clarity, C, and E paths are omitted here. These mirror those for A.
#'
#' @param name The name of the model (defaults to "GxEbiv")
#' @param selDVs The dependent variable (e.g. IQ)
#' @param selDefs The definition variable (e.g. socioeconomic status)
#' @param sep Expand variable base names, i.e., "_T" makes var -> var_T1 and var_T2
#' @param dzData The DZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param mzData The MZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param lboundACE If !NA, then lbound the main effects at this value (default = 0)
#' @param lboundM If !NA, then lbound the moderators at this value (default = NA)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (gives a warning) default = FALSE
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search", "auto" (picks to suit the model)
#' @param optimizer Optionally set the optimizer (default NULL does nothing)
#' @return - GxEbiv [OpenMx::mxModel()]
#' @export

#' @family Twin Modeling Functions
#' @seealso - [plot()], [umxSummary()]
#' @references
#' - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. \emph{Twin Research}, 
#' \strong{6}, 554-571. \doi{10.1375/twin.5.6.554}.
#' 
#' - van der Sluis, S., Posthuma, D., & Dolan, C. V. (2012). A note on false positives and
#' power in G x E modelling of twin data. \emph{Behavior Genetics}, 
#' \strong{42}, 170-186. \doi{10.1007/s10519-011-9480-3}.
#'
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs  = "wt"
#' selDefs = "ht"
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData  = subset(df, zygosity %in%  c("MZFF", "MZMM"))
#' dzData  = subset(df, zygosity %in%  c("DZFF", "DZMM", "DZOS"))
#'
#' \dontrun{
#' m1 = umxGxEbiv(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#'
#' # Plot Moderation
#' umxSummaryGxEbiv(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' m2 = umxModify(m1, update = c("cBeta2_r1c1", "eBeta1_r1c1", "eBeta2_r1c1"), comparison = TRUE)
#'
#' # TODO: teach umxReduce to test all relevant hypotheses for umxGxEbiv
#' umxReduce(m1)
#' }
umxGxEbiv <- function(name = "GxEbiv", selDVs, selDefs, dzData, mzData, sep = NULL, lboundACE = 0, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search", "auto"), optimizer = NULL) {
	tryHard = match.arg(tryHard)
	nSib = 2
	if(!is.null(optimizer)){umx_set_optimizer(optimizer)}
	if(!is.null(sep)){
		if(length(sep) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n", 
				"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs, sep, 1:nSib)
		selDefs = umx_paste_names(selDefs, sep, 1:nSib)
	}
	if(any(selDefs %in% selDVs)){
		warning("selDefs was found in selDVs: You probably gave me all the variables in selDVs instead of just the DEPENDENT variable")
	}
	if(length(selDVs)/nSib != 1){
		umx_msg(selDVs)
		stop("selDVs list must be 1 variable... You tried ", length(selDVs)/nSib, " or perhaps you didn't set sep= ?")
	}
	if(length(selDefs) != 2){
		umx_msg(selDefs)
		stop("selDefs list must be 2 variables... You tried ", length(selDefs), " or perhaps you didn't set sep = ?")
	}
	umx_check_names(selDefs, mzData)
	umx_check_names(selDefs, dzData)
	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)

	umx_check(!umx_is_cov(dzData, boolean = TRUE), "stop", "Data must be raw for umxGxEbiv")
	# TODO umxGxEbiv Check Defs are not correlated 1 or 0
	modVar = diag(var(mzData[, selDefs], na.rm = TRUE))[1]
	if(!is.finite(modVar)){ modVar = 1 }
	dvVar = diag(var(mzData[, selDVs], na.rm = TRUE))[1]
	if(!is.finite(dvVar)){ dvVar = 1 }
	startMod = sqrt(c(.8, .0, .6) * modVar)
	startDV  = sqrt(c(.8, .0, .6) * dvVar)
	modMean = mean(unlist(mzData[, selDefs]), na.rm = TRUE)
	if(!is.finite(modMean)){ modMean = 0 }
	dvMean = mean(unlist(mzData[, selDVs]), na.rm = TRUE)
	if(!is.finite(dvMean)){ dvMean = 0 }

	# selVars is used to set the dimnames for the expectation and means ordering, so in this hand-assembled function order matters...
	# 2021-01-19: was selVars = c(selDVs, selDefs)
	# expMean variable order = m_mod, m_trait, m_mod, m_trait
	# expectations have dimnames = selVars
    selVars = c(selDefs[1], selDVs[1], selDefs[2], selDVs[2]) #MN
	# drop any unused variables
	dzData = dzData[ , selVars]
	mzData = mzData[ , selVars]
	mzData = xmu_data_missing(mzData, selVars = selDefs, dropMissingDef=dropMissingDef, hint="mzData")
	dzData = xmu_data_missing(dzData, selVars = selDefs, dropMissingDef=dropMissingDef, hint="dzData")
	model = mxModel(name, 
		mxModel("top",
			umxMatrix("a11"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startMod[1]), 
			umxMatrix("c11"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startMod[2]), 
			umxMatrix("e11"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startMod[3]),
			umxMatrix("a21"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[1]), 
			umxMatrix("c21"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[2]),
			umxMatrix("e21"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[3]),
			umxMatrix("a22"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[1]),
			umxMatrix("c22"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[2]),
			umxMatrix("e22"   , "Lower", nrow = 1, ncol = 1, free = TRUE, values = startDV[3]),
			umxMatrix("aBeta1", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0), 
			umxMatrix("cBeta1", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
			umxMatrix("eBeta1", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),	
			umxMatrix("aBeta2", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
			umxMatrix("cBeta2", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
			umxMatrix("eBeta2", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .0),
			# Assemble Cholesky decomposition for twin 1 and twin 2 
			umxMatrix("PsAmz",   "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 1.0, 0, 1, 0,  1.0, 1, 0, 1)), 
			umxMatrix("PsAdz",   "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 0.5, 0, 1, 0,  0.5, 1, 0, 1)), 
			umxMatrix("PsC",     "Symm", nrow = 4, ncol = 4, free = FALSE, values = c(1,  0, 1.0, 0, 1, 0,  1.0, 1, 0, 1)), 
			umxMatrix("expMean", "Full", nrow = 1, ncol = 4, free = TRUE,  values = c(modMean, dvMean, modMean, dvMean), labels = c("m_mod", "m_trait", "m_mod", "m_trait"))
		),
        mxModel("MZ", 
			# Matrices generated to hold A and E computed Variance Components
			# This is a Cholesky decomposition of A for twin 1 and twin 2 (mz and dz) 
			# note that mod1 appears in the top left part (twin 1) and mod2 in the bottom right part (twin 2)
			# Definition variables to create moderated paths (M -> T)
			umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])), 
            umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])), 
			mxAlgebra(name = "chA", 
				rbind(
					cbind(top.a11,                         0,                               0,       0),
					cbind(top.a21 + (mod1 %x% top.aBeta1), top.a22 + (mod1 %x% top.aBeta2), 0,       0), 
					cbind(0,                               0,                               top.a11, 0), 
					cbind(0,                               0,                               top.a21 + (mod2 %x% top.aBeta1), top.a22 + (mod2 %x% top.aBeta2)))
			), 
			mxAlgebra(name = "chC", 
				rbind(
					cbind(top.c11,                                                       0,                               0,                              0), 
					cbind(top.c21 + (mod1 %x% top.cBeta1), top.c22 + (mod1 %x% top.cBeta2),                               0,                              0), 
					cbind(0,                                                             0,                         top.c11,                              0), 
					cbind(0,                                                             0, top.c21 + (mod2 %x% top.cBeta1), top.c22 + (mod2 %x% top.cBeta2)))
			), 
            mxAlgebra(name = "chE", 
				rbind(
					cbind(                        top.e11,                               0, 0, 0), 
					cbind(top.e21 + (mod1 %x% top.eBeta1), top.e22 + (mod1 %x% top.eBeta2), 0, 0), 
					cbind(                              0,                               0, top.e11, 0), 
					cbind(                              0,                               0, top.e21 + (mod2 %x% top.eBeta1), top.e22 + (mod2 %x% top.eBeta2)))
			), 
			mxAlgebra(name = "Amz", chA %&% top.PsAmz), 
			mxAlgebra(name = "C",   chC %&% top.PsC), 
			mxAlgebra(name = "E",   chE %*% t(chE)), 
            mxAlgebra(name = "expCovMZ", Amz + C + E), 
			mxData(mzData, type = "raw"), 
			mxExpectationNormal("expCovMZ", means = "top.expMean", dimnames = selVars), mxFitFunctionML()
		),
        mxModel("DZ", 
			umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])), # "data.defmod1"
			umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])), # "data.defmod2"
			mxAlgebra(name = "chA", 
				rbind(
					cbind(top.a11,                         0,                               0,       0),
					cbind(top.a21 + (mod1 %x% top.aBeta1), top.a22 + (mod1 %x% top.aBeta2), 0,       0), 
					cbind(0,                               0,                               top.a11, 0), 
					cbind(0,                               0,                               top.a21 + (mod2 %x% top.aBeta1), top.a22 + (mod2 %x% top.aBeta2)))
				), 
			mxAlgebra(name = "chC", 
				rbind(
					cbind(top.c11,                                                       0,                               0,                              0), 
					cbind(top.c21 + (mod1 %x% top.cBeta1), top.c22 + (mod1 %x% top.cBeta2),                               0,                              0), 
					cbind(0,                                                             0,                         top.c11,                              0), 
					cbind(0,                                                             0, top.c21 + (mod2 %x% top.cBeta1), top.c22 + (mod2 %x% top.cBeta2)))
				), 
            mxAlgebra(name = "chE", 
				rbind(
					cbind(                        top.e11,                               0, 0, 0), 
					cbind(top.e21 + (mod1 %x% top.eBeta1), top.e22 + (mod1 %x% top.eBeta2), 0, 0), 
					cbind(                              0,                               0, top.e11, 0), 
					cbind(                              0,                               0, top.e21 + (mod2 %x% top.eBeta1), top.e22 + (mod2 %x% top.eBeta2)))
				), 
			mxAlgebra(name = "Adz", chA %&% top.PsAdz ), 
			mxAlgebra(name = "C",   chC %&% top.PsC   ), 
			mxAlgebra(name = "E",   chE %*% t(chE)    ), 
			mxAlgebra(name = "expCovDZ", Adz + C + E  ), 
			mxData(dzData, type = "raw"), 
			mxExpectationNormal("expCovDZ", means = "top.expMean", dimnames = selVars), mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
    if (!is.na(lboundACE)) {
        model = omxSetParameters(model, labels = c("a11_r1c1", "c11_r1c1", "e11_r1c1","a22_r1c1", "c22_r1c1", "e22_r1c1"), lbound = lboundACE)
    }
    if (!is.na(lboundM)) {
        model = omxSetParameters(model, labels = c("aBeta1_r1c1", "aBeta2_r1c1", "cBeta1_r1c1", "cBeta2_r1c1", "eBeta1_r1c1", "eBeta2_r1c1"), lbound = lboundM)
    }
	model = as(model, "MxModelGxEbiv")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	invisible(model)
}

#' Summarize a bivariate GxE twin model
#'
#' `umxSummaryGxEbiv` summarizes a bivariate moderation model, as returned by [umxGxEbiv()].
#'
#' @aliases umxSummary.MxModelGxEbiv
#' @param model A fitted [umxGxEbiv()] model to summarize
#' @param std Whether to show the standardized model (not implemented! TRUE)
#' @param CIs Confidence intervals (FALSE)
#' @param xlab label for the x-axis of plot
#' @param location default = "topleft"
#' @param comparison mxCompare model with this model if offered up (default = NULL).
#' @param reduce  Whether to run and tabulate a complete model reduction...(Defaults to FALSE)
#' @param separateGraphs Std and raw plots in separate graphs? (default = FALSE)
#' @param report markdown or html (html opens in browser)
#' @param digits round to how many digits (default = 2)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [OpenMx::mxModel()]
#' @family Twin Modeling Functions
#' @export
#' @seealso - \code{\link{umxGxEbiv}()}, [plot()], [umxSummary()] work for IP, CP, GxE, and ACE models.
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
#' data(twinData)
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData  = subset(df, zygosity %in% c("MZFF", "MZMM"))
#' dzData  = subset(df, zygosity %in% c("DZFF", "DZMM", "DZOS"))
#'
#' \dontrun{
#' m1 = umxGxEbiv(selDVs = "wt", selDefs = "ht", 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#' # Plot Moderation
#' umxSummary(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' }
umxSummaryGxEbiv <- function(model = NULL, digits = 2, xlab = NA, location = "topleft", separateGraphs = FALSE, comparison = NULL, std = NULL, reduce = FALSE, CIs = NULL, report = c("markdown", "html"), returnStd = NULL,...) {
	message("umxSummaryGxEbiv is beta code: You likely might also want to run summary(model) ")
	report = match.arg(report)
	umx_has_been_run(model, stop = TRUE)
	
	if(any(!is.null(c(returnStd, std, CIs) ))){
		message("For GxE, returnStd, extended, std, or CIs are not yet implemented...")
	}

	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)

	# TODO umxSummaryGxEbiv these already exist if a_std exists..
	# TODO replace all this with umx_standardizeGxEbiv
	# Calculate standardized variance components

	tmp = cbind(model$top.aBeta1$values, model$top.cBeta1$values, model$top.eBeta1$values)
	tmp = data.frame(tmp)
	names(tmp) = c("aBeta1", "cBeta1", "eBeta1")
	message("a, c, and e Betas on the common paths")
	umx_print(tmp, digits = digits)
	
	tmp = cbind(model$top.aBeta2$values, model$top.cBeta2$values, model$top.eBeta2$values)
	tmp = data.frame(tmp)
	message("a, c, and e Betas on the unique paths")
	names(tmp) = c("aBeta2", "cBeta2", "eBeta2")
	umx_print(tmp, digits = digits)

	tmp = rbind(
		cbind(model$top$a11$values, NA                  , model$top$c11$values, NA                  , model$top$e11$values, NA),
		cbind(model$top$a21$values, model$top$a22$values, model$top$c21$values, model$top$c22$values, model$top$e21$values, model$top$e22$values)
	)
	tmp = data.frame(tmp)
	names(tmp) <-c("a1", "a2", "c1", "c2","e1", "e2")
	umx_print(tmp, digits = digits)

	if(reduce){
		stop("Sorry, umxReduce not yet implemented for umxGxEbiv")
		# TODO umxReduce not yet implemented for umxGxEbiv!
		umxReduce(model = model, report = report)
	}
	invisible(model)
}

#' @export
umxSummary.MxModelGxEbiv <- umxSummaryGxEbiv


#' Moderator values pooled from a GxEbiv model
#'
#' Collects the unique sorted moderator values over both twins and zygosities,
#' used as the x-axis when plotting moderation.
#'
#' @param model A [umxGxEbiv()] model (fitted or not).
#' @return Sorted unique moderator values.
xmu_GxEbiv_moderatorValues <- function(model) {
	mzData  = model$MZ$data$observed
	dzData  = model$DZ$data$observed
	selDefs = c(sub("^data\\.", "", model$MZ$mod1$labels), sub("^data\\.", "", model$MZ$mod2$labels))
	mzdef1 = as.vector(mzData[, selDefs[1]])
	mzdef2 = as.vector(mzData[, selDefs[2]])
	dzdef1 = as.vector(dzData[, selDefs[1]])
	dzdef2 = as.vector(dzData[, selDefs[2]])
	allValuesOfDefVar = c(mzdef1, mzdef2, dzdef1, dzdef2)
	vals = sort(unique(allValuesOfDefVar))
	vals[!is.na(vals)]
}

#' Variance components across moderator values for a GxEbiv model
#'
#' Computes raw and standardized A, C, E variance of the trait at each
#' moderator value from the model's path estimates.
#'
#' @param model A [umxGxEbiv()] model (fitted or not).
#' @param moderatorValues Moderator values at which to compute variances.
#' @return A list with `raw` (Va, Vc, Ve, Vt columns) and `std` matrices.
xmu_GxEbiv_variances <- function(model, moderatorValues) {
	a11 = model$top.a11$values
	a21 = model$top.a21$values
	a22 = model$top.a22$values
	Ba1 = model$top.aBeta1$values
	Ba2 = model$top.aBeta2$values

	c11 = model$top.c11$values
	c21 = model$top.c21$values
	c22 = model$top.c22$values
	Bc1 = model$top.cBeta1$values
	Bc2 = model$top.cBeta2$values

	e11 = model$top.e11$values
	e21 = model$top.e21$values
	e22 = model$top.e22$values
	Be1 = model$top.eBeta1$values
	Be2 = model$top.eBeta2$values	

	Va  = (c(a21) + moderatorValues * c(Ba1))^2 + (c(a22) + moderatorValues * c(Ba2))^2
	Vc  = (c(c21) + moderatorValues * c(Bc1))^2 + (c(c22) + moderatorValues * c(Bc2))^2
	Ve  = (c(e21) + moderatorValues * c(Be1))^2 + (c(e22) + moderatorValues * c(Be2))^2
	Vt  = Va + Vc + Ve

	out    = as.matrix(cbind(Va, Vc, Ve, Vt))
	outStd = as.matrix(cbind(Va/Vt, Vc/Vt, Ve/Vt))
	list(raw = out, std = outStd)
}


#' Plot the results of a bivariate GxE test for moderation of ACE components.
#'
#' Plot GxEbiv results (bivariate environmental moderation of ACE components).
#' Options include plotting the raw and standardized graphs separately, or in a combined panel.
#' You can also set the label for the x axis (xlab), and choose the location of the legend.
#'
#' @aliases plot.MxModelGxEbiv
#' @param x A fitted [umxGxEbiv()] model to plot
#' @param xlab String to use for the x label (default = NA, which will use the variable name)
#' @param location Where to plot the legend (default = "topleft")
#' see ?legend for alternatives like bottomright
#' @param separateGraphs (default = FALSE)
#' @param moderatorValues Moderator values at which to plot (default = NULL, uses the observed values)
#' @param ... Optional additional parameters
#' @return None
#' @family Plotting functions
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxGxEbiv()]
#' @references <https://tbates.github.io>
#' @examples
#' require(umx)
#' data(twinData)
#' \dontrun{
#' selDVs  = "wt"; selDefs = "ht"
#' df = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData  = subset(df, zygosity %in%  c("MZFF", "MZMM"))
#' dzData  = subset(df, zygosity %in%  c("DZFF", "DZMM", "DZOS"))
#'
#' m1 = umxGxEbiv(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#' # Plot Moderation
#' plot(m1)
#' umxPlotGxEbiv(m1, xlab = "wt", separateGraphs = TRUE, location = "topleft")
#' }
umxPlotGxEbiv <- function(x, xlab = NA, location = "topleft", separateGraphs = FALSE, moderatorValues = NULL, ...) {
	if(class(x)[[1]] != "MxModelGxEbiv"){
		stop("The first parameter of umxPlotGxEbiv must be a umxGxEbiv model, you gave me a ", class(x))
	}
	model = x # to remind us that x has to be a umxGxEbiv model
	if(is.null(moderatorValues)){
		defVarValues = xmu_GxEbiv_moderatorValues(model)
	} else {
		defVarValues = moderatorValues
	}
	if(is.na(xlab)){
		defName = sub("^data\\.", "", model$MZ$mod1$labels)
		xlab = sub("(_T)?[0-9]$", "", defName)
	}
	comp = xmu_GxEbiv_variances(model, defVarValues)
	out = comp$raw
	outStd = comp$std

	tmp= data.frame(rbind(
		cbind(model$top.a11$values, NA, model$top.c11$values, NA, model$top.e11$values, NA, model$top.aBeta1$values, model$top.cBeta1$values, model$top.eBeta1$values),
		cbind(model$top.a21$values, model$top.a22$values, model$top.c21$values, model$top.c22$values, model$top.e21$values, model$top.e22$values, model$top.aBeta2$values, model$top.cBeta2$values, model$top.eBeta2$values))
	)
	names(tmp) = c("a1", "a2", "c1", "c2", "e1", "e2", "a_betas", "c_betas", "e_betas")
	row.names(tmp) = c("moderator", "trait")
	umx_print(tmp, digits=2)

	if(!separateGraphs){
		graphics::par(mfrow = c(1, 2)) # one row, two columns for raw and std variance
	}
	acergb = c("red", "green", "blue", "black")
	graphics::matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = acergb, xlab = xlab, ylab = "Variance", main= "Raw Moderation Effects")
	graphics::legend(location, legend = c("genetic", "shared", "unique", "total"), lty = 1:4, col = acergb)
	graphics::matplot(defVarValues, outStd, type = "l", lty = 1:3, col = acergb[1:3], ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main= "Standardized Moderation Effects")
	graphics::legend(location, legend = c("genetic", "shared", "unique"), lty = 1:3, col = acergb[1:3])
	graphics::par(mfrow = c(1, 1)) # back to black
}

#' @export
plot.MxModelGxEbiv <- umxPlotGxEbiv
