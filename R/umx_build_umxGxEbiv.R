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
#' @param lboundACE If !NA, then lbound the main effects at this value (default = NA)
#' @param lboundM If !NA, then lbound the moderators at this value (default = NA)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (gives a warning) default = FALSE
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing)
#' @return - GxEbiv [mxModel()]
#' @export
#' @md
#' @family Twin Modeling Functions
#' @seealso - [plot()], [umxSummary()], [umxReduce()]
#' @references
#' - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. \emph{Twin Research}, 
#' \strong{6}, 554-571. doi:[10.1375/twin.5.6.554](https://doi.org/10.1375/twin.5.6.554).
#' 
#' - van der Sluis, S., Posthuma, D., & Dolan, C. V. (2012). A note on false positives and
#' power in G x E modelling of twin data. \emph{Behavior Genetics}, 
#' \strong{42}, 170-186. doi:[10.1007/s10519-011-9480-3](https://doi.org/10.1007/s10519-011-9480-3).
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
#' #
#' # TODO: teach umxReduce to test all relevant hypotheses for umxGxEbiv
#' umxReduce(m1)
#' }
umxGxEbiv <- function(name = "GxEbiv", selDVs, selDefs, dzData, mzData, sep = NULL, lboundACE = NA, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	tryHard = match.arg(tryHard)
	if(tryHard == "yes"){
		tryHard = "mxTryHard"
	}
	nSib = 2;
	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(!is.null(sep)){
		if(length(sep) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs , sep, 1:nSib)
		selDefs = umx_paste_names(selDefs, sep, 1:nSib)
	}
	if(any(selDefs %in% selDVs)) {
		warning("selDefs was found in selDVs: You probably gave me all the variables in selDVs instead of just the DEPENDENT variable");
	}
	if(length(selDVs)/nSib != 1){
		umx_msg(selDVs)
		stop("selDVs list must be 1 variable... You tried ", length(selDVs)/nSib, " or perhaps you didn't set sep= ?")
	}
	if(length(selDefs) != 2){
		umx_msg(selDefs)
		warning("selDefs must be length = 2. Or perhaps you didn't set sep = ?");
	}

	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	message("selDVs: ", omxQuotes(selDVs))

	umx_check(!umx_is_cov(dzData, boolean = TRUE), "stop", "data must be raw for gxe")
	# TODO umxGxEbiv Check Defs are not correlated 1 or 0
	obsMean   = mean(colMeans(mzData[,selDVs], na.rm = TRUE)); # Just one average mean for all twins
	nVar      = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	rawVar    = diag(var(mzData[,selDVs], na.rm = TRUE))[1]
	startMain = sqrt(c(.8, .0 ,.6) * rawVar)
	
	selVars   = c(selDVs, selDefs)
	# drop any unused variables
	dzData = dzData[,selVars]
	mzData = mzData[,selVars]
	
	if(any(is.na(mzData[,selDefs]))){
		if(dropMissingDef){
			missingT1 = is.na(mzData[,selDefs[1]])
			missingT2 = is.na(mzData[,selDefs[2]])
			missDef = (missingT1 | missingT2)
			message(sum(missDef), " mz rows dropped due to missing def var for Twin 1 or Twin 2 or both")
			mzData = mzData[!missDef, ]
		} else {
			stop("Some rows of mzData have NA definition variables. Remove these yourself, or set dropMissing = TRUE")
		}
	}
	if(any(is.na(dzData[,selDefs]))){
		if(dropMissingDef){
			missDef = is.na(dzData[,selDefs[1]]) | is.na(dzData[,selDefs[2]])
			message(sum(missDef), " dz rows dropped due to missing def var for Twin 1 or Twin 2 or both")
			dzData = dzData[!missDef, ]
		} else {
			stop("Some rows of dzData have NA definition variables. Remove these yourself, or set dropMissing = TRUE")
		}
	}
	model = mxModel(name,
		mxModel("top",
			# TODO:	Add covariates to G x E biv model
			# ACE model moderator – Cholesky elements
			# path coefficients a, c, e
			# Defined as scalars (1x1)
			# TODO Can all of these 1x1s be wrapped up into something bigger?
			# A chol
			# a0m  = a11
			# a0mt = a21
			# a0t  = a22
			# a1mt = aBeta1
			# a1t  = aBeta2

			# ACE model moderator main effects parameters
			# a0m = a11
			umxMatrix("a11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("c11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("e11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),

			# ACE model moderator-trait covariances: main effects
			umxMatrix("a21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("c21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),
			umxMatrix("e21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),

			# ACE model trait-specific main effects parameters
			umxMatrix("a22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
			umxMatrix("c22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
			umxMatrix("e22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),


			# ACE model  moderator-trait covariances: moderation
			umxMatrix("aBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0), 
			umxMatrix("cBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("eBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),	

			# ACE model trait 1 moderator effects
			umxMatrix("aBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("cBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("eBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),

			# Assemble Cholesky decomposition for twin 1 and twin 2 
			umxMatrix("PsAmz", "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1)),
			umxMatrix("PsAdz", "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0,.5, 0, 1, 0,.5, 1, 0, 1)),
			umxMatrix("PsC"  , "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1)),
	
			# Matrix & Algebra for expected means vector (non-moderated)
			# TODO start means at obsMean
			# TODO allow other covs on means?
			umxMatrix("expMean", "Full", nrow = 1, ncol = 4, free = TRUE, values = 0, labels = c("m_mod", "m_trait", "m_mod", "m_trait"))
		),
		mxModel("MZ",
			# Definition variables to create moderated paths (M -> T)
			umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])),
			umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])),

			# Matrices generated to hold A and E computed Variance Components
			# This is a Cholesky decomposition of A for twin 1 and twin 2 (mz and dz) 
			# note that mod1 appears in the top left part (twin 1) and mod2 in the bottom right part (twin 2)

			# A chol
			# a0m  = a11
			# a0mt = a21
			# a0t  = a22
			# a1mt = aBeta1
			# a1t  = aBeta2

			mxAlgebra(name = "chA", rbind(
				cbind(top.a11, 0, 0, 0),
				cbind(top.a21 + mod1 %x% top.aBeta1, top.a22 + mod1 %x% top.aBeta2, 0, 0),
				cbind(0                 , 0               , top.a11, 0),
				cbind(0                 , 0               , top.a21 + mod2 %x% top.aBeta1, top.a22 + mod2 %x% top.aBeta2))
			),

			# C chol
			mxAlgebra(name = "chC", rbind(
				cbind(top.c11, 0, 0, 0),
				cbind(top.c21 + mod1 %x% top.cBeta1, top.c22 + mod1 %x% top.cBeta2, 0  , 0),
				cbind(0                 ,0                , top.c11, 0),
				cbind(0                 ,0                , top.c21 + mod2 %x% top.cBeta1, top.c22 + mod2 %x% top.cBeta2))
			),

			# E chol
			mxAlgebra(name = "chE", rbind(
				cbind(top.e11, 0, 0 , 0),
				cbind(top.e21 + mod1 %x% top.eBeta1, top.e22 + mod1 %x% top.eBeta2, 0 , 0),
				cbind(0                 ,0                ,top.e11, 0),
				cbind(0                 ,0                ,top.e21 + mod2 %x% top.eBeta1, top.e22 + mod2 %x% top.eBeta2))
			),

			# Get the expected covariance matrices 
			mxAlgebra(name = "Amz", chA %&% top.PsAmz),  # variance component A
			mxAlgebra(name = "C"  , chC %&% top.PsC  ),  # variance component C
			mxAlgebra(name = "E"  , chE %*% t(chE)   ),  # variance component E

			# Assemble phenotypic 4 x 4 MZ cov matrix 
			mxAlgebra(name = "expCovMZ", Amz + C + E), 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("expCovMZ", means = "top.expMean", dimnames = selVars),
			mxFitFunctionML()
		),
	  mxModel("DZ",
			# defs, e.g twin1  c("data.divorce1")
			umxMatrix("mod1", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[1])), # "data.defmod1"
			umxMatrix("mod2", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[2])), # "data.defmod2"

			# ========================================
			# = Compute a, c, e, Cholesky components =
			# ========================================

			mxAlgebra(name ="chA", rbind(
				cbind(top.a11                      , 0                            , 0, 0),
				cbind(top.a21 + mod1 %x% top.aBeta1, top.a22 + mod1 %x% top.aBeta2, 0, 0),
				cbind(0, 0, top.a11                      , 0                            ),
				cbind(0, 0, top.a21 + mod2 %x% top.aBeta1, top.a22 + mod2 %x% top.aBeta2))
			),
			# C chol
			mxAlgebra(name ="chC", rbind(
				cbind(top.c11, 0, 0, 0),
				cbind(top.c21 + mod1 %x% top.cBeta1, top.c22 + mod1 %x% top.cBeta2, 0, 0),
				cbind(0, 0, top.c11, 0),
				cbind(0, 0, top.c21 + mod2 %x% top.cBeta1, top.c22 + mod2 %x% top.cBeta2))
			),
			# E chol
			mxAlgebra(name ="chE", rbind(
				cbind(top.e11, 0, 0, 0),
				cbind(top.e21 + mod1 %x% top.eBeta1, top.e22 + mod1 %x% top.eBeta2, 0, 0),
				cbind(0, 0, top.e11, 0),
				cbind(0, 0, top.e21 + mod2 %x% top.eBeta1, top.e22 + mod2 %x% top.eBeta2))
			),

			mxAlgebra(name = "Adz", chA %*% top.PsAdz %*% t(chA)),  # variance component A
			mxAlgebra(name = "C"  , chC %*% top.PsC   %*% t(chC)),  # variance component C
			mxAlgebra(name = "E"  , chE %*% t(chE)              ),  # variance component E

			# Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
			mxAlgebra(name = "expCovDZ", Adz + C + E),
			# Data & Objective
			mxData(dzData, type = "raw"),
			mxExpectationNormal("expCovDZ", means = "top.expMean", dimnames = selVars),
			mxFitFunctionML()
	  ),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)

	if(!is.na(lboundACE)){
		model = omxSetParameters(model, labels = c('a_r1c1', 'c_r1c1', 'e_r1c1'), lbound = lboundACE)
	}
	if(!is.na(lboundM)){
		model = omxSetParameters(model, labels = c('am_r1c1', 'cm_r1c1', 'em_r1c1'), lbound = lboundM)
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
#' @param comparison mxCompare model with comparison (default = FALSE).
#' @param reduce  Whether to run and tabulate a complete model reduction...(Defaults to FALSE)
#' @param separateGraphs Std and raw plots in separate graphs? (default = FALSE)
#' @param report markdown or html (html opens in browser)
#' @param show Here to support being called from generic xmu_safe_run_summary. User should ignore: can be c("std", "raw")
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param ... Optional additional parameters
#' @return - optional [mxModel()]
#' @family Twin Reporting Functions
#' @export
#' @seealso - \code{\link{umxGxEbiv}()}, [plot()], [umxSummary()] work for IP, CP, GxE, and ACE models.
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' data(twinData)
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData  = subset(df, zygosity %in%  c("MZFF", "MZMM"))
#' dzData  = subset(df, zygosity %in%  c("DZFF", "DZMM", "DZOS"))
#'
#' \dontrun{
#' m1 = umxGxEbiv(selDVs = "wt", selDefs = "ht", 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#' # Plot Moderation
#' umxSummary(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' }
umxSummaryGxEbiv <- function(model = NULL, digits = 2, xlab = NA, location = "topleft", separateGraphs = FALSE, file = getOption("umx_auto_plot"), comparison = FALSE, std = NULL, reduce = FALSE, CIs = NULL, report = c("markdown", "html"), returnStd = NULL, show = c("std", "raw"),...) {
	show = match.arg(show, c("std", "raw"))
	if(show != "std"){
		std = FALSE
		# message("Polite message: in next version, show= will be replaced with std=TRUE/FALSE/NULL")
	}
	report = match.arg(report)
	umx_has_been_run(model, stop = TRUE)
	
	if(any(!is.null(c(returnStd, std, CIs) ))){
		message("For GxE, returnStd, extended, std, comparison or CIs are not yet implemented...")
	}

	if(is.null(model)){
		message("umxSummaryGxEbiv calls plot.MxModelGxEbiv for a twin moderation plot. A use example is:\n umxSummaryGxEbiv(model, location = \"topright\")")
		stop();
	}
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	selDVs = model$MZm$expectation$dims
	nVar <- length(selDVs)/2;
	# TODO umxSummaryACE these already exist if a_std exists..
	# TODO replace all this with umx_standardizeACE
	# Calculate standardized variance components
	# chA  <- model$MZ.chA$values # Path coefficients
	# message("chA")
	# umx_print(chA)

	tmp = cbind(model$top.aBeta1$values, model$top.cBeta1$values, model$top.eBeta1$values)
	tmp = data.frame(tmp)
	names(tmp) <-c("aBeta1", "cBeta1", "eBeta1")
	message("Betas on defVar effects")
	umx_print(tmp, digits=3)
	
	tmp = cbind(model$top.aBeta2$values, model$top.cBeta2$values, model$top.eBeta2$values)
	tmp = data.frame(tmp)
	message("Betas on DV effects")
	names(tmp) <-c("aBeta2", "cBeta2", "eBeta2")
	umx_print(tmp, digits = 3)

	# message("Amz")
	# print(model$MZ$Amz$result) # chA %*% top.PsAdz %*% t(chA)),  # variance component A
	# message("Adz")
	# umx_print(model$DZ$Adz$result) # chA %*% top.PsAdz %*% t(chA)),  # variance component A

	# message("C from DZ model")
	# umx_print(model$DZ$C$result) # chA %*% top.PsAdz %*% t(chA)),  # variance component A
	# message("E from DZ model")
	# umx_print(model$DZ$E$result) # chA %*% top.PsAdz %*% t(chA)),  # variance component A
	tmp = rbind(
		cbind(model$top$a11$values, NA                  , model$top$c11$values, NA                  , model$top$e11$values, NA),
		cbind(model$top$a21$values, model$top$a22$values, model$top$c21$values, model$top$c22$values, model$top$e21$values, model$top$e22$values)
	)
	tmp = data.frame(tmp)
	names(tmp) <-c("a1", "a2", "c1", "c2","e1", "e2")
	umx_print(tmp, digits = 3)

	# umxPlotGxEbiv(model, xlab = xlab, location = location, separateGraphs = separateGraphs)

	if(reduce){
		# TODO umxReduce not yet implemented for umxGxEbiv!
		umxReduce(model = model, report = report)
	}
}

#' @export
umxSummary.MxModelGxEbiv <- umxSummaryGxEbiv


#' Plot the results of a GxE univariate test for moderation of ACE components.
#'
#' Plot GxE results (univariate environmental moderation of ACE components).
#' Options include plotting the raw and standardized graphs separately, or in a combined panel.
#' You can also set the label for the x axis (xlab), and choose the location of the legend.
#'
#' @aliases plot.MxModelGxEbiv
#' @param x A fitted [umxGxEbiv()] model to plot
#' @param xlab String to use for the x label (default = NA, which will use the variable name)
#' @param location Where to plot the legend (default = "topleft")
#' see ?legend for alternatives like bottomright
#' @param separateGraphs (default = FALSE)
#' @param ... Optional additional parameters
#' @return None
#' @family Plotting functions
#' @export
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxGxEbiv()]
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' \dontrun{
#' selDVs  = "wt"; selDefs = "ht"
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), suffix = "")
#' mzData  = subset(df, zygosity %in%  c("MZFF", "MZMM"))
#' dzData  = subset(df, zygosity %in%  c("DZFF", "DZMM", "DZOS"))
#'
#' m1 = umxGxEbiv(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#' # Plot Moderation
#' plot(m1)
#' umxPlotGxEbiv(m1, xlab = "wt", separateGraphs = TRUE, location = "topleft")
#' }
umxPlotGxEbiv <- function(x, xlab = NA, location = "topleft", separateGraphs = FALSE, ...) {
	message("umxGxEbiv plot is in early beta: expect problems, and let me know what they are!")
	if(class(x) != "MxModelGxEbiv"){
		stop("The first parameter of umxPlotGxE must be a GxEbiv model, you gave me a ", class(x))
	}
	model = x # to remind us that x has to be a umxGxEbiv model
	# get unique values of moderator
	mzData = model$MZ$data$observed
	dzData = model$DZ$data$observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = selDefs[1]
	}
	mzdef1 = as.vector(mzData[, selDefs[1]])
	mzdef2 = as.vector(mzData[, selDefs[2]])
	dzdef1 = as.vector(dzData[, selDefs[1]])
	dzdef2 = as.vector(dzData[, selDefs[2]])
	allValuesOfDefVar= c(mzdef1, mzdef2, dzdef1, dzdef2)
	defVarValues = sort(unique(allValuesOfDefVar))
	
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

	Va  = (c(a21 + a22) + (defVarValues * c(Ba1 + Ba2)))^2
	Vc  = (c(c21 + c22) + (defVarValues * c(Bc1 + Bc2)))^2
	Ve  = (c(e21 + e22) + (defVarValues * c(Be1 + Be2)))^2
	Vt  = Va + Vc + Ve

	out    = as.matrix(cbind(Va, Vc, Ve, Vt))
	outStd = as.matrix(cbind(Va/Vt, Vc/Vt, Ve/Vt))
	# message("Va, Vc, Ve, Vt")
	# umx_print(out   , digits = 2)
	# message("std: Va, Vc, Ve")
	# umx_print(outStd, digits = 2)

	tmp= data.frame(rbind(
		cbind(a11, NA , c11,  NA, e11,  NA, Ba1, Bc1, Be1),
		cbind(a21, a22, c21, c22, e21, e22, Ba2, Bc2, Be2))
	)
	names(tmp) = c("a1", "a2", "c1", "c2", "e1", "e2", "a_betas", "c_betas", "e_betas")
	umx_print(tmp, digits=2)

	if(separateGraphs){
		print("Outputting two graphs")
	}else{
		graphics::par(mfrow = c(1, 2)) # one row, two columns for raw and std variance
		# par(mfrow = c(2, 1)) # two rows, one column for raw and std variance
	}
	acergb = c("red", "green", "blue", "black")
	graphics::matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = acergb, xlab = xlab, ylab = "Variance", main= "Raw Moderation Effects")
	graphics::legend(location, legend = c("genetic", "shared", "unique", "total"), lty = 1:4, col = acergb)
	# legend(location, legend= c("Va", "Vc", "Ve", "Vt"), lty = 1:4, col = acergb)
	graphics::matplot(defVarValues, outStd, type = "l", lty = 1:4, col = acergb, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main= "Standardized Moderation Effects")
	# legend(location, legend= c("Va", "Vc", "Ve"), lty = 1:4, col = acergb)
	graphics::legend(location, legend = c("genetic", "shared", "unique"), lty = 1:4, col = acergb)
	graphics::par(mfrow = c(1, 1)) # back to black
}

#' @export
plot.MxModelGxEbiv <- umxPlotGxEbiv
