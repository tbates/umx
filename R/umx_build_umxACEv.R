#   Copyright 2007-2020 Timothy C. Bates
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

#' Build and run 2-group uni- or multi-variate ACE models based on VARIANCE (not paths).
#'
#' @description
#' A common task in twin modeling involves using the genetic and environmental differences 
#' between large numbers of pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of one, or, typically, several phenotypes.
#' `umxACEv` directly estimates variance components (ratheer than paths, which 
#' are then squared to produce variance and therefore cannot be negative). It offers better power, 
#' correct Type I error and un-biased estimates (with no zero-bound for the variances) as a saturated model.
#' (Verhulst et al, 2019).
#' 
#' The ACE variance-based model decomposes phenotypic variance into additive genetic (A),
#' unique environmental (E) and, optionally, either common environment (shared-environment, C) or 
#' non-additive genetic effects (D). Scroll down to details for how to use the function, a figure
#' and multiple examples.
#' 
#' The following figure shows the A components of a trivariate ACEv model:
#' 
#'
#' \if{html}{\figure{ACEv.png}{options: width="50\%" alt="Figure: ACEv.png"}}
#' \if{latex}{\figure{ACEv.pdf}{options: width=7cm}}
#' 
#' \emph{NOTE}: This function does not use the Cholesky decomposition. Instead it directly models variance.
#' This ensures unbiased type-I error rates. It means that occasionally
#' estimates of variance may be negative. This should be used as an occasion to inspect you model
#' choices and data. `umxACEv` can be used as a base model to validate the ACE Cholesky model, 
#' a core model in behavior genetics (Neale and Cardon, 1992).
#' 
#' @details
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelihoods summed to form
#' the model-likelihood, which is to be minimized.
#' This feature is used in the non-linear GxE model functions.
#' 
#' \strong{Additional features}
#' The umxACEv function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs (Eaves et al. 1978 p267).
#' 
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin var names, often "_T" in vars like "dep_T1". Simplifies selDVs.
#' @param data If provided, dzData and mzData are treated as valid levels of zyg to select() data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS").
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param boundDiag = Numeric lbound for diagonal of the a, c, and e matrices. Default = NULL (no bound)
#' @param weightVar = If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param covMethod How to treat covariates: "fixed" (default) or "random".
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @return - [mxModel()] subclass `mxModel.ACE`
#' @export
#' @family Twin Modeling Functions
#' @references - Verhulst, B., Prom-Wormley, E., Keller, M., Medland, S., & Neale, M. C. (2019).
#' Type I Error Rates and Parameter Bias in Multivariate Behavioral Genetic Models. *Behav Genet*, 
#' **49**, 99-111. doi:<https://doi.org/10.1007/s10519-018-9942-y>
#' Eaves, L. J., Last, K. A., Young, P. A., & Martin, N. G. (1978). Model-fitting approaches 
#' to the analysis of human behaviour. *Heredity*, **41**, 249-320. <https://www.nature.com/articles/hdy1978101.pdf>
#' @md
#' @examples
#' 
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#'
#' # Things to note: ACE model of weight will return a NEGATIVE variance in C.
#' #  This is exactly why we have ACEv! It suggests we need a different model
#' #  In this case: ADE.
#' # Other things to note:
#' # 1. umxACEv can figure out variable names: provide "sep", and selVars. 
#' #    Function generates: "wt" -> "wt1" "wt2"
#' # 2. umxACEv picks the variables it needs from the data.
#' 
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = "wt", sep = "", dzData = dzData, mzData = mzData)
#' 
#' # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
#' m1 = umxACEv(selDVs = "wt", sep = "", dzData = "MZFF", mzData = "DZFF", data = twinData)
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = "wt", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' # note: the underlying matrices are still called A, C, and E.
#' # I catch this in the summary table, so columns are labeled A, D, and E.
#' # However, currently, the plot will say A, C, E.
#' 
#' # We can modify this model, dropping dominance component (still called C), 
#' # and see a comparison:
#' m3 = umxModify(m2, update = "C_r1c1", comparison = TRUE, name="AE")
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#' 
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' # 
#' # Note: Height has a small variance. umx can typically picks good starts,
#' #    but scaling is advisable.
#' # 
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#' # height var is very small: move from m to cm to increase.
#' twinData[,c("ht1", "ht2")]= twinData[,c("ht1", "ht2")]*100
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
#' umxSummary(m1, std = FALSE) # unstandardized
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' plot(m1)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = "ht", sep="", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # Is ADE better?
#' umxSummary(m2, comparison = m1) # nb: though this is ADE, matrices are still called A,C,E
#'
#' # We can modify this model, dropping shared environment, and see a comparison:
#' m3 = umxModify(m2, update = "C_r1c1", comparison = TRUE, name = "AE")
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' 
#' data(twinData)
#' twinData[,c("ht1", "ht2")]= twinData[,c("ht1", "ht2")]*100
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"), ]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData = mzData[1:80, ] # Quicker run to keep CRAN happy
#' dzData = dzData[1:80, ]
#' m1 = umxACEv(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
#' 
#' # ===================
#' # = Ordinal example =
#' # ===================
#' require(umx)
#' data(twinData)
#' # Cut bmi column to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' selDVs = c("obese")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] = mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData = twinData[twinData$zygosity %in% "MZFF", ][1:80,] # 80 pairs for speed
#' dzData = twinData[twinData$zygosity %in% "DZFF", ][1:80,]
#' str(mzData) # make sure mz, dz, and t1 and t2 have the same levels!
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#' 
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' selDVs = c("wt", "obese")
#' # Cut bmi column to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#'
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] = mxFactor(twinData[, ordDVs], levels = obesityLevels)
#'
#' # umxACEv can trim out unused variables on its own
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' plot(m1)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' cutPoints = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' \dontrun{
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#' }
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' selDVs = c("wt")
#' mz = cov(twinData[twinData$zygosity %in% "MZFF", tvars(selDVs, "")], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in% "DZFF", tvars(selDVs, "")], use = "complete")
#' m1 = umxACEv(selDVs = selDVs, sep= "", dzData = dz, mzData= mz, numObsDZ= 569, numObsMZ= 351)
#' umxSummary(m1, std = FALSE)
#' 
umxACEv <- function(name = "ACEv", selDVs, selCovs = NULL, sep = NULL, dzData, mzData, dzAr = .5, dzCr = 1, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"),
	data = NULL, zyg = "zygosity", weightVar = NULL, numObsDZ = NULL, numObsMZ = NULL, addStd = TRUE, addCI = TRUE, 
	boundDiag = NULL, equateMeans = TRUE, bVector = FALSE,  covMethod = c("fixed", "random"), 
	autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	nSib                = 2 # number of siblings in a twin pair
	type                = match.arg(type)
	covMethod           = match.arg(covMethod)
	allContinuousMethod = match.arg(allContinuousMethod)
	if(dzCr == .25 & name == "ACEv"){ name = "ADEv" }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		if(is.null(dzData)){ dzData = "DZ"; mzData = "MZ" }
		mzData = data[data[,zyg] %in% mzData, ]
		dzData = data[data[,zyg] %in% dzData, ]
		# mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		# dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}

	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)
	
	# If given covariates, call umxACEvcov
	if(covMethod == "random") {
		stop("random covariates for umxACEv not yet implemented")
		# TODO implement umxACEvcov or refactor
		# TODO add allContinuousMethod = allContinuousMethod and type
		# umxACEvcov(name = name, selDVs=selDVs, selCovs=selCovs, dzData=dzData, mzData=mzData, sep = sep, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, autoRun = autoRun, tryHard = tryHard)
	}
	# nSib = 2, equateMeans = TRUE, verbose = verbose
	selVars = tvars(selDVs, sep = sep, suffixes= 1:nSib)
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family) **
	
	model = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar)
	tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)

	# Finish building top
	top = mxModel(model$top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique variance components
		umxMatrix("A", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("C", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("E", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE), 
	
		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(rbind (cbind(ACE, AC),
		                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name = "expCovMZ"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
	)
	model = mxModel(model, top) 
	
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$A@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(A)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$A$lbound = newLbound
			model$top$C$lbound = newLbound
			model$top$E$lbound = newLbound
		}
	}
	if(addStd){
		newTop = mxModel(model$top,
			mxMatrix(name  = "I", "Iden", nVar, nVar),   # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),          # Total variance
			mxAlgebra(name = "InvSD", sqrt(solve(I * Vtot))), # 1/variance

			# Standardised _variance_ coefficients ready to be stacked together
			mxAlgebra(name = "A_std", InvSD %&% A), # standardized A
			mxAlgebra(name = "C_std", InvSD %&% C), # standardized C
			mxAlgebra(name = "E_std", InvSD %&% E)  # standardized E
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.A_std', 'top.C_std', 'top.E_std')))
		}
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACEv") # Set class so that S3 plot() dispatches.
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, summary = TRUE, comparison = FALSE)
	return(model)
} # end umxACEv


#' Shows a compact, publication-style, summary of a variance-based Cholesky ACE model.
#'
#' Summarize a fitted Cholesky model returned by [umxACEv()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelACEv
#' @param model an [mxModel()] to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = no plot.
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (TRUE)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If "html", then open an html table of the results
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param show Here to support being called from generic xmu_safe_run_summary. User should ignore: can be c("std", "raw")
#' @param ... Other parameters to control model summary
#' @return - optional [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACEv()] 
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACEv(selDVs = "bmi", sep = "", dzData = dzData, mzData = mzData)
#' umxSummary(m1, std = FALSE)
#' \dontrun{
#' umxSummary(m1, file = NA);
#' umxSummary(m1, file = "name", std = TRUE)
#' stdFit = umxSummary(m1, returnStd = TRUE)
#' }
umxSummaryACEv <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", show = c("std", "raw"), ...) {
	show = match.arg(show, c("std", "raw"))
	if(show != "std"){
		std = FALSE
		# message("Polite message: in next version, show= will be replaced with std=TRUE/FALSE/NULL")
	}
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	# depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	selDVs = dimnames(model$top.expCovMZ)[[1]]
	nVar   = length(selDVs)/2;

	A  <- mxEval(top.A, model); # Variances
	C  <- mxEval(top.C, model);
	E  <- mxEval(top.E, model);

	if(std){
		message("Standardized solution")
		# TODO replace with call to umx_standardize()??
		# Calculate standardized variance components
		Vtot  = A + C + E; # Total variance
		I     = diag(nVar); # nVar Identity matrix

		# Inverse of diagonal matrix of standard deviations.
		InvSD <- sqrt(solve(I * Vtot));

		# Standardized _variance_ coefficients ready to be stacked together
		A_std = InvSD %&% A 	# Standardized variance coefficients
		C_std = InvSD %&% C
		E_std = InvSD %&% E
		
		AClean = A_std
		CClean = C_std
		EClean = E_std
	} else {
		message("Raw solution")
		AClean = A
		CClean = C
		EClean = E
	}

	AClean[upper.tri(AClean)] = NA
	CClean[upper.tri(CClean)] = NA
	EClean[upper.tri(EClean)] = NA
	rowNames  = sub("(_T)?1$", "", selDVs[1:nVar])
	Estimates = data.frame(cbind(AClean, CClean, EClean), row.names = rowNames, stringsAsFactors = FALSE);

	colNames = c("A", "C", "E")
	if(model$top$dzCr$values == .25){
		colNames = c("A", "D", "E")
	}
	names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
	umx_print(Estimates, digits = digits, zero.print = zero.print, append=FALSE, sortableDF=TRUE, both=TRUE, na.print="NA", file=report)
	xmu_twin_print_means(model, digits = digits, report = report)
	
	if(extended == TRUE) {
		message("Unstandardized path coefficients")
		AClean = A
		CClean = C
		EClean = E
		AClean[upper.tri(AClean)] = NA
		CClean[upper.tri(CClean)] = NA
		EClean[upper.tri(EClean)] = NA
		unStandardizedEstimates = data.frame(cbind(AClean, CClean, EClean), row.names = rowNames);
		names(unStandardizedEstimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print)
	}

	# Pre & post multiply covariance matrix by inverse of standard deviations
	if(showRg) {
		message("Genetic correlations")
		NAmatrix <- matrix(NA, nVar, nVar);
		rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # C correlations
		rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # E correlations
		rAClean = rA
		rCClean = rC
		rEClean = rE
		rAClean[upper.tri(rAClean)] = NA
		rCClean[upper.tri(rCClean)] = NA
		rEClean[upper.tri(rEClean)] = NA
		genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames);
		names(genetic_correlations) <- rowNames
	 	# Make a nice table.
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits = digits, zero.print = zero.print)
	}
	hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO umxACE CI code: Need to refactor into some function calls...
			# TODO and then add to umxSummaryIP and CP
			# 1. Get labels that are free: doesn't matter if they're my format or not
			# 2. Turn each into a bracket[la,ble] (or labels for equates)
			# 3. get the value of the parameter
			# 4. for tables, that’s it: just print them out
			# 5. for plots, tag labels with type+name of from- and to- vars
			# 	* This requires some custom code for each model type.
			# 6. Hence the xmu_CI_stash idea... if it would generalize.

			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0), ]
			# Discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			# These can be names ("top.A_std[1,1]") or labels ("A11")
			# imxEvalByName finds them both
			# outList = c();
			# for(aName in row.names(CIlist)) {
			# 	outList <- append(outList, imxEvalByName(aName, model))
			# }
			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the CI results
			rows = dim(model$top$matrices$A$labels)[1]
			cols = dim(model$top$matrices$A$labels)[2]
			A_CI = C_CI = E_CI = matrix(NA, rows, cols)

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
				CIparts        = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString     = paste0(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]")

				if(grepl("^A", thisMatrixName)) {
					A_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^C", thisMatrixName)){
					C_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^E", thisMatrixName)){
					E_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with A, C, or E. You sent: ", thisMatrixName))
				}
			}
			# TODO umxSummaryACEv: Check the merge of A_, C_ and E_CI INTO the output table works with more than one variable
			# TODO umxSummaryACEv: Add option to use mxSE
			# print(A_CI)
			# print(C_CI)
			# print(E_CI)
			Estimates = data.frame(cbind(A_CI, C_CI, E_CI), row.names = rowNames, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
			if(report == "html"){
				# Depends on R2HTML::HTML
				R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T); 
				umx_open("tmpCI.html")
			}
			CI_Fit = model
			CI_Fit$top$A$values = A_CI
			CI_Fit$top$C$values = C_CI
			CI_Fit$top$E$values = E_CI
		} # end Use CIs
	} # end list catcher?
	
	if(!is.na(file)) {
		if(hasCIs & CIs){
			umxPlotACEv(CI_Fit, file = file, std = FALSE)
		} else {
			umxPlotACEv(model, file = file, std = std)
		}
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		umx_standardize(model)
	}
}

#' @export
umxSummary.MxModelACEv <- umxSummaryACEv

#' Produce a graphical display of an ACE variance-components twin model
#'
#' Plots an ACE model graphically, opening the result in the browser (or a graphviz application).
#'
#' @aliases plot.MxModelACEv
#' @param x [umxACEv()] model to plot.
#' @param file The name of the dot file to write: Default ("name") = use the name of the model. NA = don't plot.
#' @param digits How many decimals to include in path loadings (default = 2)
#' @param means Whether to show means paths (default = FALSE)
#' @param std Whether to standardize the model (default = FALSE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @family Reporting functions
#' @references - <https://www.github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m1 = umxACEv(selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#' umxSummary(m1)
#' umxPlotACEv(m1, std = FALSE) # Don't standardize
#' plot(m1, std = FALSE) # don't standardize
umxPlotACEv <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, ...) {
	# TODO umxPlotACEv: update to matrix version instead of label hunting
	# TODO umxPlotACEv: use xmu_dot_define_shapes etc.?
	# preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:varCount])
	# top     = xmu_dot_rank(out$latents, "^[ace]_cp", "min")
	# bottom  = xmu_dot_rank(out$latents, "^[ace]s[0-9]+$", "max")
	# digraph = paste0("digraph G {\n	splines=\"FALSE\";\n", preOut, top, bottom, out$str, "\n}");
	model = x # Just to be clear that x is a model
	if(std){ model = umx_standardize(model) }

	selDVs = xmu_twin_get_var_names(model)
	# umx_msg(selDVs)
	nVar   = length(selDVs) # assumes 2 siblings
	selDVs = selDVs[1:(nVar)]

	parameterKeyList = omxGetParameters(model) # e.g. expMean_r1c1  A_r1c1  C_r1c1  E_r1c1
	out     = "" 
	latents = c()

	for(thisParam in names(parameterKeyList) ) {
		value = parameterKeyList[thisParam]
		if(class(value) == "numeric") {
			value = round(value, digits)
		}
		if (grepl("^[ACE]_r[0-9]+c[0-9]+", thisParam)) { # fires on things like "A_r1c1"
			from    = sub('([ACE])_r([0-9]+)c([0-9]+)', '\\1\\3', thisParam, perl = TRUE); # "A_r1c1" --> "A1" where 1 is column
			target  = sub('([ACE])_r([0-9]+)c([0-9]+)', '\\1\\2', thisParam, perl = TRUE); # target is [ACE] + row
			latents = append(latents, c(from, target))
			out = paste0(out, "\t", from, " -> ", target, " [dir=both, label = \"", value, "\"]", ";\n")
		} else { # means probably
			from   = thisParam;
			target = sub('r([0-9])c([0-9])', 'var\\2', thisParam, perl=TRUE) 
			if(means){
				out = paste0(out, "\t", from, " -> ", target, " [label = \"", value, "\"]", ";\n")
			}
		}
	}
	# =========================================
	# = list latents and draw them as circles =
	# =========================================
	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}

	# ===========================================
	# = list manifests and draw them as squares =
	# ===========================================
	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(var in selDVs[1:nVar]) {
	   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
	}

	selDVs[1:nVar]
	l_to_v_at_1 = ""
	for(l in latents) {
		var  = as.numeric(sub('([ACE])([0-9]+)', '\\2', l, perl = TRUE)); # target is [ACE]n		
	   l_to_v_at_1 = paste0(l_to_v_at_1, "\t ", l, "-> ", selDVs[var], " [label = \"@1\"];\n")
	}

	rankVariables = paste("\t{rank = same; ", paste(selDVs[1:nVar], collapse = "; "), "};\n") # {rank = same; v1T1; v2T1;}
	# grep('a', latents, value= T)
	rankA   = paste("\t{rank = min; ", paste(grep('A'   , latents, value = TRUE), collapse = "; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[CE]', latents, value = TRUE), collapse = "; "), "};\n") # {rank=min; c1; e1}
	
	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut,
		out,
		l_to_v_at_1,
		rankVariables,
		rankA, 
		rankCE, "\n}"
	)
	
	print("?umxPlotACEv options: std=, means=, digits=, strip_zero=, file=, min=, max =")
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
} # end umxPlotACE

#' @export
plot.MxModelACEv <- umxPlotACEv


#' Standardize an ACE variance components model (ACEv)
#'
#' xmu_standardize_ACE allows umx_standardize to standardize an ACE variance components model.
#'
#' @param model An [umxACEv()] model to standardize.
#' @param ... Other parameters.
#' @return - A standardized [umxACEv()] model.
#' @export
#' @family xmu internal not for end user
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi")
#' mzData <- twinData[twinData$zygosity %in% "MZFF",][1:80,] # 80 pairs for speed
#' dzData <- twinData[twinData$zygosity %in% "DZFF",][1:80,]
#' m1  = umxACEv(selDVs = selDVs, sep="", dzData = dzData, mzData = mzData)
#' std = umx_standardize(m1)
xmu_standardize_ACEv <- function(model, ...) {
	# TODO umxSummaryACEv these already exist if a_std exists..
	message("Standardized variance-based models may yield negative variances...")
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umx_standardize(thisFit)
		}
	} else {
		if(!umx_has_been_run(model)){
			stop("I can only standardize ACEv models that have been run. Just do\n",
			"yourModel = mxRun(yourModel)")
		}
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar <- length(selDVs)/2;
		# Calculate standardized variance components
		A  <- mxEval(top.A, model);   # Variances
		C  <- mxEval(top.C, model);
		E  <- mxEval(top.E, model);

		# this should just be A+C+E?
		# A = cov2cor(abs(A)) * sign(A)
		# C = cov2cor(abs(C)) * sign(C)
		# E = cov2cor(abs(E)) * sign(E)

		Vtot = A + C + E;  # Total variance
		I  = diag(nVar);  # nVar Identity matrix
		# Inverse of diagonal matrix of standard deviations.
		InvSD <- sqrt(solve(I * Vtot));
	
		# Standardized _variance_ coefficients ready to be stacked together
		model$top$A$values = InvSD %&% A; # Standardized variance components
		model$top$C$values = InvSD %&% C;
		model$top$E$values = InvSD %&% E;
		return(model)
	}
}
#' @export
umx_standardize.MxModelACEv <- xmu_standardize_ACEv
