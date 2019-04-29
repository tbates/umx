#' Build and run a 2-group Cholesky twin model (uni-variate or multi-variate)
#'
#' @description
#' Implementing a core task in twin modeling, umxACEold models the genetic and environmental
#' structure of one or more phenotypes (measured variables) using the Cholesky ACE model
#' (Neale and Cardon, 1996).
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxACEold` implements a 2-group model to capture these data and represent the phenotypic variance as a sum of Additive genetic,
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' The following figure shows how the ACE model appears as a path diagram (for one variable):
#' 
#' \if{html}{\figure{ACEunivariate.png}{options: width="50\%" alt="Figure: ACE_full_univariate.png"}}
#' \if{latex}{\figure{ACEunivariate.pdf}{options: width=7cm}}
#' 
#' `umxACEold` allows multivariate analyses, and this brings us to the Cholesky part of the model.
#' 
#' This model creates as many latent A C and E variables as there are phenotypes, and, moving 
#' from left to right, decomposes the variance in each manifest into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram:
#' 
#' \if{html}{\figure{ACEmatrix.png}{options: width="50\%" alt="Figure: ACE matrix.png"}}
#' \if{latex}{\figure{ACEmatrix.pdf}{options: width=7cm}}
#' 
#' In this model, the variance-covariance matrix of the raw data
#' is recovered as the product of the lower Cholesky and its transform.
#' 
#' This Cholesky or lower-triangle decomposition allows a model which is both sure to be 
#' solvable, and also to account for all the variance (with some restrictions) in the data.
#' 
#' This figure also contains the key to understanding how to modify models that `umxACEold` produces.
#' read the "Matrices and Labels in ACE model" section in details below...
#' 
#' **NOTE**: Scroll down to details for how to use the function, a figure
#' and multiple examples.
#' 
#' @details
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modeling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelihoods summed to form
#' the model-likelihood, which is to be minimized.
#' This feature is used in the non-linear GxE model functions.
#' 
#' \strong{Additional features}
#' The umxACEold function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \strong{Matrices and Labels in ACE model}
#' 
#' Matrices 'a', 'c', and 'e' contain the path loadings of the Cholesky ACE factor model.
#' 
#' So, labels relevant to modifying the model are of the form \code{"a_r1c1", "c_r1c1"} etc.
#'
#' Variables are in rows, and factors are in columns. So to drop the influence of factor 2 on variable 3, you would say
#'
#'     \code{m2 = umxModify(m1, update = "c_r3c2")}
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' \code{m1$top$expMean$labels[1, 4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")}
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs (Eaves et al. 1978 p267).
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param covMethod How to treat covariates: "fixed" (default) or "random".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param numObsDZ Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ Number of MZ twins: Set this if you input covariance data.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param boundDiag Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0 since umx version 1.8
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @return - \code{\link{mxModel}} of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot.MxModelACE}}, \code{\link{plot.MxModelACE}}, \code{\link{umxSummaryACE}}, \code{\link{umxModify}}
#' @references - Eaves, L. J., Last, K. A., Young, P. A., & Martin, N. G. (1978). Model-fitting approaches 
#' to the analysis of human behaviour. *Heredity*, **41**, 249-320. \url{https://keppel.qimr.edu.au/contents/p/staff/CV013.pdf}
#' @md
#' @examples
#' 
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#' # Pick the variables
#' twinData[,c("ht1", "ht2")] = twinData[,c("ht1", "ht2")]*100
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEold(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData) # -2ll= 9659, a1 = .92
#' umxSummary(m1, std = FALSE) # un-standardized
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' plot(m1)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEold("ADE", selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) 
#' # nb: Although summary is smart enough to print d, the underlying 
#' #     matrices are still called a, c & e.
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' 
#' # 1. This variable has a large variance, and this makes solution finding very hard.
#' # We'll scale weight to make the Optimizer's task easier.
#'
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("wt"), sep = "")
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # 2. umxACEold can figure out variable names: provide sep= "_T" and selVar = "wt" -> "wt_T1" "wt_T2"
#' 
#' # 3. umxACEold picks the variables it needs from the data.
#' # 4. expert user note: by default, umxACEold lower-bounds a, c, and e at 0.
#' #    This prevents mirror-solutions.
#' #    You can remove this by setting boundDiag = NULL
#' 
#' m1 = umxACEold(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "")
#'
#' # MODEL MODIFICATION
#' # We can modify this model, say testing shared environment, and see a comparison:
#' 
#' m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
#' # nb: You can see names of free parameters with parameters(m1)
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData = mzData[1:80,] # quicker run to keep CRAN happy
#' dzData = dzData[1:80,]
#' selDVs = c("ht", "wt") # umx will add sep (in this case "") + "1" or '2'
#' m1 = umxACEold(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#'
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#'
#'
#' # ===================
#' # = Ordinal example =
#' # ===================
#' require(umx)
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into umxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' mzData = mzData[1:80, ] # Just top 80 pairs to run fast
#' dzData = dzData[1:80, ]
#' str(mzData) # make sure mz, dz, and t1 and t2 have the same levels!
#'
#' # Data-prep done - here's where the model starts:
#' selDVs = c("obese")
#' m1 = umxACEold(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#' 
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("wt"), sep = "")
#' # Cut BMI column to form ordinal obesity variables
#' obesityLevels   = c('normal', 'overweight', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in%  "MZFF",] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF",]
#' mzData <- mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData <- dzData[1:80,]
#' m1 = umxACEold(selDVs = c("wt", "obese"), dzData = dzData, mzData = mzData, sep = '')
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("wt"), sep = "")
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' obesityLevels   = c('normal', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' \dontrun{
#' m1 = umxACEold(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#' }
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("wt"), sep = "")
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' m1 = umxACEold(selDVs = selDVs, dzData = dz, mzData = mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)
umxACEold <- function(name = "ACE", selDVs, selCovs = NULL, covMethod = c("fixed", "random"), dzData, mzData, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, 
	weightVar = NULL, equateMeans = TRUE, bVector = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL, intervals = FALSE) {

		nSib = 2 # Number of siblings in a twin pair.
		covMethod  = match.arg(covMethod)
		type = match.arg(type)

		xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
		if(dzCr == .25 & (name == "ACE")){
			name = "ADE"
		}

		# If given covariates, call umxACEcov
		if(!is.null(selCovs)){
			if(covMethod == "fixed"){
				stop("Fixed covariates are on the roadmap for umx in 2019. Until then, use umx_residualize on the data first.")
				# umxACEdefcov(name = name, selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep = sep, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, autoRun = autoRun, tryHard = tryHard)
			} else if(covMethod == "random"){
				umxACEcov(name = name, selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep = sep, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, autoRun = autoRun, tryHard = tryHard)
			}
		}else{
			if(is.null(sep)){
				selVars = selDVs
			}else{
				selVars = tvars(selDVs, sep = sep, suffixes = 1:nSib)
			}
			nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
			if(!is.null(weightVar)){
				used = c(selVars, weightVar)
			} else {
				used = selVars
			}
			dataType = umx_is_cov(dzData, boolean = FALSE)
			# Compute numbers of ordinal and binary variables.
			if(dataType == "raw"){
				if(!all(is.null(c(numObsMZ, numObsDZ)))){
					stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
				}
				# Drop unused columns from mzData and dzData
				mzData = mzData[, used]
				dzData = dzData[, used]
				isFactor = umx_is_ordered(mzData[, selVars])                      # T/F list of factor columns
				isOrd    = umx_is_ordered(mzData[, selVars], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
				isBin    = umx_is_ordered(mzData[, selVars], binary.only  = TRUE) # T/F list of binary columns
				nFactors = sum(isFactor)
				nOrdVars = sum(isOrd) # total number of ordinal columns
				nBinVars = sum(isBin) # total number of binary columns

				factorVarNames = names(mzData)[isFactor]
				ordVarNames    = names(mzData)[isOrd]
				binVarNames    = names(mzData)[isBin]
				contVarNames   = names(mzData)[!isFactor]
			} else {
				# Summary data
				isFactor = isOrd    = isBin    = c()
				nFactors = nOrdVars = nBinVars = 0
				factorVarNames = ordVarNames = binVarNames = contVarNames = c()
			}

			if(dataType == "raw") {
				# detect weight var if used
				if(!is.null(weightVar)){
					# weight variable provided: check it exists in each frame
					if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
						stop("The weight variable must be included in the mzData and dzData",
							 " frames passed into umxACE when \"weightVar\" is specified",
							 "\n mzData contained:", paste(names(mzData), collapse = ", "),
							 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
							 "\n but I was looking for ", weightVar, " as the moderator."
						)
					}
					mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = FALSE, values = mzData[, weightVar])
					dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = FALSE, values = dzData[, weightVar])
					mzData = mzData[, selVars]
					dzData = dzData[, selVars]
					bVector = TRUE
				} else {
					# no weights
				}

				# =====================================
				# = Add means and var matrices to top =
				# =====================================
				# Figure out start values while we are here
				# varStarts will be used to fill a, c, and e
				# mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)
				allData = rbind(mzData, dzData)
				varStarts = umx_var(mzData[, selVars[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
				
				# TODO repeat sqrt start values for other twin models. 2017-08-19 12:21PM umxACEcov done
				if(nVar == 1){
					# Sqrt to switch from var to path coefficient scale
					varStarts = sqrt(varStarts)/3
				}else{
					varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
				}
				varStarts = matrix(varStarts, nVar, nVar)

				# Mean starts (used across all raw solutions
				obsMeans = umx_means(allData[, selVars], ordVar = 0, na.rm = TRUE)

				# Smarter but not guaranteed
				# a_val = e_val = t(chol(xmu_cov_factor(mzData, use = "pair"))) * .6
				# c_val = t(chol(cov(mzData, use = "pair"))) * .1

				# ===============================
				# = Notes: Ordinal requires:    =
				# ===============================
				# 1. Set to mxFactor
				# 2. For Binary variables:
				#   1. Means of binary variables fixedAt 0
				#   2. A + C + E for binary variables is constrained to 1 
				# 4. For Ordinal variables, first 2 thresholds fixed
				# TODO
				#  2. WLS as an option.
				#  3. TOBIT
				# [] select mxFitFunctionML() of bVector as param
				if(nFactors == 0){
					# =======================================================
					# = Handle all continuous case                          =
					# =======================================================
					message("All variables continuous")
					top = mxModel("top", 
						umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars))
					)
					MZ  = mxModel("MZ" , 
						mxExpectationNormal("top.expCovMZ", "top.expMean"),
						mxFitFunctionML(vector = bVector),
						mxData(mzData, type = "raw")
					)
					DZ  = mxModel("DZ",
						mxExpectationNormal("top.expCovDZ", "top.expMean"),
						mxFitFunctionML(vector = bVector),
						mxData(dzData, type = "raw")
					)
				} else if(sum(isBin) == 0){
					if(is.null(sep)){ stop("Some data are not continuous: I need you to set a seperator so I can be sure what the data names are for each twin") }
					# ==================================================
					# = Handle 1 or more ordinal variables (no binary) =
					# ==================================================
					message("umxACE found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames), " (No binary)")
					if(length(contVarNames) > 0){
						message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames))	
					}
					# Means: all free, start cont at the measured value, ord @0
					meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars))
					# Thresholds
					# for better guessing with low-frequency cells
					allData = rbind(mzData, dzData)

					top = mxModel("top",
						umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars)),
						umxThresholdMatrix(allData, selDVs = selVars, sep = sep, threshMatName = "threshMat", verbose = FALSE)
					)

					MZ  = mxModel("MZ", 
						mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat"), 
						mxFitFunctionML(vector = bVector),
						mxData(mzData, type = "raw")
					)
					DZ  = mxModel("DZ",
						mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat"),
						mxFitFunctionML(vector = bVector),
						mxData(dzData, type = "raw")
					)
				} else if(sum(isBin) > 0){
					# =============================================
					# = Handle case of at least 1 binary variable =
					# =============================================

					message("umxACE found ", sum(isBin)/nSib, " pairs of binary variables:", omxQuotes(binVarNames))
					message("\nI am fixing the latent means and variances of these variables to 0 and 1")
					if(nOrdVars > 0){
						message("There were also ", nOrdVars/nSib, " pairs of ordinal variables:", omxQuotes(ordVarNames))			
					}
					if(length(contVarNames) > 0){
						message("\nand ", length(contVarNames)/nSib, " pairs of continuous variables:", omxQuotes(contVarNames))	
					}else{
						message("No continuous variables")
					}
			
					# ===========================================================================
					# = Means: bin fixed, others free, start cont at the measured value, ord @0 =
					# ===========================================================================
					# ===================================
					# = Constrain Ordinal variance @1  =
					# ===================================
					# Algebra to pick out the ordinal variables
					# TODO check using twin 1 to pick where the bin variables are is robust...
					# Fill with zeros: default for ordinals and binary...
					allData   = rbind(mzData, dzData)
					meansFree = (!isBin) # fix the binary variables at zero
					the_bin_cols = which(isBin)[1:nVar] # columns in which the bin variables appear for twin 1, i.e., c(1,3,5,7)
					binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")

					top = mxModel("top", 
						umxMatrix("expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = obsMeans, dimnames = list("means", selVars)),
						umxThresholdMatrix(allData, selDVs = selVars, sep = sep, threshMatName = "threshMat", verbose = TRUE),
						mxAlgebra(name = "Vtot", A + C + E), # Total variance (redundant but is OK)
						umxMatrix("binLabels"  , "Full", nrow = (nBinVars/nSib), ncol = 1, labels = binBracketLabels),
						umxMatrix("Unit_nBinx1", "Unit", nrow = (nBinVars/nSib), ncol = 1),
						mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
					)
					MZ  = mxModel("MZ",
						mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat"),
						mxFitFunctionML(),
						# mxFitFunctionML(vector = bVector),
						mxData(mzData, type = "raw")
					)
					DZ  = mxModel("DZ",
						mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat"),
						mxFitFunctionML(),
						# mxFitFunctionML(vector = bVector),
						mxData(dzData, type = "raw")
					)
				} else {
					stop("You appear to have something other than I expected in terms of binary, ordinal and continuous variable mix")
				}
				# nb: means not yet equated across twins
			} else if(dataType %in% c("cov", "cor")){
				if(!is.null(weightVar)){
					stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices, or pass in raw data")
				}else{
					message("Summary data")
				}
				umx_check(!is.null(numObsMZ), "stop", paste0("You must set numObsMZ with ", dataType, " data"))
				umx_check(!is.null(numObsDZ), "stop", paste0("You must set numObsDZ with ", dataType, " data"))
				# Drop unused variables from matrix
				het_mz = umx_reorder(mzData, selVars)
				het_dz = umx_reorder(dzData, selVars)
				varStarts = diag(het_mz)[1:nVar]				
				if(nVar == 1){
					varStarts = sqrt(varStarts)/3
				} else {
					varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
				}
				varStarts = matrix(varStarts, nVar, nVar)

				top = mxModel("top")
				MZ = mxModel("MZ", 
					mxExpectationNormal("top.expCovMZ"), 
					mxFitFunctionML(), 
					mxData(het_mz, type = "cov", numObs = numObsMZ)
				)
		
				DZ = mxModel("DZ",
					mxExpectationNormal("top.expCovDZ"),
					mxFitFunctionML(),
					mxData(het_dz, type = "cov", numObs = numObsDZ)
				)
			} else {
				stop("Datatype \"", dataType, "\" not understood. Must be one of ", omxQuotes(type))
			}
		message("treating data as ", dataType)

		# Finish building top
		top = mxModel(top,
			# "top" defines the algebra of the twin model, which MZ and DZ slave off of
			# NB: top already has the means model and thresholds matrix added if necessary  - see above
			# Additive, Common, and Unique environmental paths
			umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
			umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
			umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE), 
		
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
			# Multiply by each path coefficient by its inverse to get variance component
			# Quadratic multiplication to add common_loadings
			mxAlgebra(name = "A", a %*% t(a)), # additive genetic variance
			mxAlgebra(name = "C", c %*% t(c)), # common environmental variance
			mxAlgebra(name = "E", e %*% t(e)), # unique environmental variance
			mxAlgebra(name = "ACE", A+C+E),
			mxAlgebra(name = "AC" , A+C  ),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC),
			                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name = "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
		)

		# =====================================
		# =  Assemble models into supermodel  =
		# =====================================

		if(!bVector){
			model = mxModel(name, MZ, DZ, top,
				mxFitFunctionMultigroup(c("MZ", "DZ"))
			)
		} else {
			# bVector is TRUE
			# To weight objective functions in OpenMx, you specify a container model that applies the weights
			# m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
			# This option makes FIML return individual likelihoods for each row of the data (rather than a single -2LL value for the model)
			# You then optimize weighted versions of these likelihoods by building additional models containing 
			# weight data and an algebra that multiplies the likelihoods from the first model by the weight vector
			model = mxModel(name, MZ, DZ, top,
				mxModel("MZw", mzWeightMatrix,
					mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
					mxFitFunctionAlgebra("mzWeightedCov")
				),
				mxModel("DZw", dzWeightMatrix,
					mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
					mxFitFunctionAlgebra("dzWeightedCov")
				),
				mxFitFunctionMultigroup(c("MZw", "DZw"))
			)
		}
		if(!is.null(boundDiag)){
			if(!is.numeric(boundDiag)){
				stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
			} else {				
				newLbound = model$top$matrices$a@lbound
				if(length(boundDiag) > 1 ){
					if(length(boundDiag) != length(diag(newLbound)) ){
						stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
					}
				}
				diag(newLbound) = boundDiag; 
				model$top$a$lbound = newLbound
				model$top$c$lbound = newLbound
				model$top$e$lbound = newLbound
			}
		}
		if(addStd){
			newTop = mxModel(model$top,
				umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
				# redundant with binary version of top - doesn't matter to add it twice
				mxAlgebra(name = "Vtot", A + C+ E), # Total variance
				# TODO test that these are identical in all cases.
				# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # SD
				mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # Total variance
				mxAlgebra(name = "a_std", SD %*% a), # standardized a
				mxAlgebra(name = "c_std", SD %*% c), # standardized c
				mxAlgebra(name = "e_std", SD %*% e)  # standardized e
			)
			model = mxModel(model, newTop)
			if(addCI){
				if(addStd){
					model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
				}else{
					model = mxModel(model, mxCI(c('top.a', 'top.c', 'top.e')))
				}
			}
		}
		# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
		if(equateMeans & (dataType == "raw")){
			model = omxSetParameters(model,
			  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMean14", "expMean15", "expMean16"),
			  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMean11", "expMean12", "expMean13")
			)
		}
		# Trundle through and make sure values with the same label have the same start value... means for instance.
		model = omxAssignFirstParameters(model)
		model = as(model, "MxModelACE") # set class so that S3 plot() dispatches.
		model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
		return(model)
	}
} # end umxACE



#' Draw and display a graphical figure of Common Pathway model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' @param x The Common Pathway \code{\link{mxModel}} to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param SEstyle report "b (se)" instead of "b [lower, upper]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - \code{\link{umxCP}}
#' @family Plotting functions
#' @family Twin Reporting Functions
#' @references - \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' umxPlotCPold(yourCP_Model) # no need to remember a special name: plot works fine!
#' }
umxPlotCPold <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	if(!class(x) == "MxModelCP"){
		stop("The first parameter of umxPlotCP must be a CP model, you gave me a ", class(x))
	}
	format = match.arg(format)
	model = x # just to emphasise that x has to be a model 
	if(std){
		model = umx_standardize_CP(model)
	}
	facCount = dim(model$top$a_cp$labels)[[1]]
	varCount = dim(model$top$as$values)[[1]]
	selDVs   = dimnames(model$MZ$data$observed)[[2]]
	selDVs   = selDVs[1:(varCount)]
	selDVs   = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	parameterKeyList = omxGetParameters(model)
	out = "";
	latents = c();
	cSpecifics = c();
	for(thisParam in names(parameterKeyList) ) {
		# TODO: umxPlotCP plot functions are in the process of being made more intelligent. see: umxPlotCPnew()
		# This version uses labels. New versions will access the relevant matrices, thus
		# breaking the dependency on label structure. This will allow more flexible labeling

		# Top level a c e inputs to common factors
		if( grepl("^[ace]_cp_r[0-9]", thisParam)) { 
			# Match cp latents, e.g. thisParam = "c_cp_r1c3" (note, row = factor #)
			from    = sub("^([ace]_cp)_r([0-9])"  , '\\1\\2'   , thisParam, perl= TRUE); # "a_cp<r>"
			target  = sub("^([ace]_cp)_r([0-9]).*", 'common\\2', thisParam, perl= TRUE); # "common<r>"
			latents = append(latents, from)
		} else if (grepl("^cp_loadings_r[0-9]+", thisParam)) {
			# Match common loading string e.g. "cp_loadings_r1c1"
			from    = sub("^cp_loadings_r([0-9]+)c([0-9]+)", "common\\2", thisParam, perl= TRUE); # "common<c>"
			thisVar = as.numeric(sub('cp_loadings_r([0-9]+)c([0-9]+)', '\\1', thisParam, perl= TRUE)); # var[r]
			target  = selDVs[as.numeric(thisVar)]
			latents = append(latents,from)
		} else if (grepl("^[ace]s_r[0-9]", thisParam)) {
			# Match specifics, e.g. thisParam = "es_r10c10"
			grepStr = '([ace]s)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1\\3', thisParam, perl= TRUE);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl= TRUE));
			target  = selDVs[as.numeric(targetindex)]			
			latents = append(latents, from)
			cSpecifics = append(cSpecifics, from);
		} else if (grepl("^(exp)?[Mm]ean", thisParam)) { # means probably expMean_r1c1
			grepStr = '(^.*)_r([0-9]+)c([0-9]+)'
			from    = "one"
			targetindex = as.numeric(sub(grepStr, '\\3', thisParam, perl= TRUE))
			target  = selDVs[as.numeric(targetindex)]
		} else if (grepl("_dev[0-9]", thisParam)) { # is a threshold
			# Doesn't need plotting? # TODO umxPlotCP could tabulate thresholds?
			from = "do not plot"
		} else {
			message("While making the plot, I found a path labeled ", thisParam, "\nI don't know where that goes.\n",
			"If you are using umxModify to make newLabels, re-use one of the existing labels to help plot()")
		}
		if(from == "do not plot" || (from == "one" & !means) ){
			# Either this is a threshold, or we're not adding means...
		} else {
			# Get parameter value and make the plot string
			# Convert address to [] address and look for a CI: not perfect, as CI might be label based?
			# If the model already has CIs stashed umx_stash_CIs() then pointless and harmful.
			# Also fails to understand not using _std?
			CIstr = xmu_get_CI(model, label = thisParam, prefix = "top.", suffix = "_std", SEstyle = SEstyle, digits = digits)
			if(is.na(CIstr)){
				val = umx_round(parameterKeyList[thisParam], digits)
			}else{
				val = CIstr
			}
			out = paste0(out, ";\n", from, " -> ", target, " [label=\"", val, "\"]")
		}
	}
	preOut  = umx_dot_define_shapes(latents = latents, manifests = selDVs[1:varCount])
	ranks = paste(cSpecifics, collapse = "; ");
	ranks = paste0("{rank=sink; ", ranks, "}");
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, ranks, out, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	} 
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

#' umxCPold: Build and run a Common pathway twin model
#'
#' Make a 2-group Common Pathway twin model (Common-factor common-pathway multivariate model).
#' 
#' The common-pathway model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences.
#' 
#' umxCP supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Common-pathway path diagram:
#' 
#' \figure{CP.png}
#' 
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotype.
#' 
#' @details
#' Like the \code{\link{umxACE}} model, the CP model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure).
#' 
#' 
#' \strong{Data Input}
#' Currently, the umxCP function accepts only raw data. This may change in future versions.
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' \strong{Additional features}
#' The umxCP function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \strong{Matrices and Labels in CP model}
#' A good way to see which matrices are used in umxCP is to run an example model and plot it.
#'
#' The diagonals of matrices as, cs, and es contain the path loadings specific to each variable. So labels relevant to modifying these are of the form "as_r1c1", "as_r2c2" etc.
#' All the shared matrices are in the model "top". So to see the `as` values, you can say:
#' 
#' `m1$top#as$values`
#' 
#' The common-pathway loadings on the factors are in matrices a_cp, c_cp, e_cp.
#'
#' The common factors themselves are in the matrix cp_loadings (an nVar * 1 matrix)
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#'
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' `m1$top$expMean$labels[1,4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' @param name The name of the model (defaults to "CP").
#' @param selDVs The variables to include.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The suffix for twin 1 and twin 2, often "_T". If set, selDVs is just the base variable names.
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param nFac How many common factors (default = 1)
#' @param freeLowerA Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE Whether to leave the lower triangle of E free (default = FALSE).
#' @param correlatedA ?? (default = FALSE).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param boundDiag = Numeric lbound for diagonal of the a_cp, c_cp, & e_cp matrices. Set = NULL to ignore.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = not yet implemented: Ordinal Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = not yet implemented: Ordinal Number of MZ twins: Set this if you input covariance data.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxACE}()} for more examples of twin modeling, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{https://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData <- subset(GFF, zyg_2grp == "MZ")
#' dzData <- subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxCPold(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' umxParameters(m1, patt = "^c")
#' m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC")
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
#' }
#' @md
umxCPold <- function(name = "CPold", selDVs, dzData, mzData, sep = NULL, nFac = 1, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = FALSE, equateMeans= TRUE, dzAr= .5, dzCr= 1, boundDiag = 0, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL) {
	nSib = 2
	xmu_twin_check(selDVs=selDVs, dzData = dzData, mzData = mzData, optimizer = optimizer, sep = sep, nSib = nSib)
	
	# expand var names
	selDVs   = umx_paste_names(selDVs, sep = sep, suffixes = 1:nSib)
	nVar     = length(selDVs)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	dataType = umx_is_cov(dzData)
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from MZ and DZ Data
		mzData = mzData[, selDVs]
		dzData = dzData[, selDVs]
		# bind the MZ nd DZ data into one frame for precision
		allData = rbind(mzData, dzData)
		
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
		obsMeans = colMeans(allData, na.rm = TRUE);
		top = mxModel("top", 
			# Means (not yet equated across twins)
			umxMatrix("expMean", type = "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selDVs) )
		) 
		MZ = mxModel("MZ", 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"),
			mxFitFunctionML()
		)
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
		top = mxModel("top") # no means
		MZ = mxModel("MZ", 
			mxData(het_mz, type = "cov", numObs = numObsMZ),
			mxExpectationNormal("top.expCovMZ"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(het_dz, type = "cov", numObs = numObsDZ),
			mxExpectationNormal("top.expCovDZ"),
			mxFitFunctionML()
		)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}

	if(correlatedA){
		a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = TRUE, values = .7, jiggle = .05) # Latent common factor
	} else {
		a_cp_matrix = umxMatrix("a_cp", "Diag", nFac, nFac, free = TRUE, values = .7, jiggle = .05)
	}

	model = mxModel(name,
		mxModel(top,
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
			# Latent common factor genetic paths
			a_cp_matrix,
			umxMatrix("c_cp", "Diag", nFac, nFac, free = TRUE, values =  0, jiggle = .05), # latent common factor Common environmental path coefficients
			umxMatrix("e_cp", "Diag", nFac, nFac, free = TRUE, values = .7, jiggle = .05), # latent common factor Unique environmental path coefficients
			# Constrain variance of latent phenotype factor to 1.0
			# Multiply by each path coefficient by its inverse to get variance component
			mxAlgebra(name = "A_cp", a_cp %*% t(a_cp)), # A_cp variance
			mxAlgebra(name = "C_cp", c_cp %*% t(c_cp)), # C_cp variance
			mxAlgebra(name = "E_cp", e_cp %*% t(e_cp)), # E_cp variance
			mxAlgebra(name = "L"   , A_cp + C_cp + E_cp), # total common factor covariance (a+c+e)
			mxMatrix("Unit", nrow=nFac, ncol=1, name = "nFac_Unit"),
			mxAlgebra(diag2vec(L)             , name = "diagL"),
			mxConstraint(diagL == nFac_Unit   , name = "fix_CP_variances_to_1"),

			umxMatrix("as", "Lower", nVar, nVar, free = TRUE, values = .5, jiggle = .05), # Additive gen path 
			umxMatrix("cs", "Lower", nVar, nVar, free = TRUE, values = .1, jiggle = .05), # Common env path 
			umxMatrix("es", "Lower", nVar, nVar, free = TRUE, values = .6, jiggle = .05), # Unique env path
			umxMatrix("cp_loadings", "Full", nVar, nFac, free = TRUE, values = .6, jiggle = .05), # loadings on latent phenotype
			# Quadratic multiplication to add cp_loading effects
			mxAlgebra(cp_loadings %&% A_cp + as %*% t(as), name = "A"), # Additive genetic variance
			mxAlgebra(cp_loadings %&% C_cp + cs %*% t(cs), name = "C"), # Common environmental variance
			mxAlgebra(cp_loadings %&% E_cp + es %*% t(es), name = "E"), # Unique environmental variance
			mxAlgebra(name = "ACE", A + C + E),
			mxAlgebra(name = "AC" , A + C),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name= "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name= "expCovDZ")
		),
		MZ, DZ,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Equate means for twin1 and twin 2 (match labels in first & second halves of means labels matrix)
	if(equateMeans & dataType == "raw"){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
		)
	}
	if(!freeLowerA){
		toset  = model$top$matrices$as$labels[lower.tri(model$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerC){
		toset  = model$top$matrices$cs$labels[lower.tri(model$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerE){
		toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix(name = "I", "Iden", nVar, nVar),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(name = "SD", solve(sqrt(I * ACE))),
			# Standard specific path coefficients
			mxAlgebra(name = "as_std", SD %*% as), # standardized a
			mxAlgebra(name = "cs_std", SD %*% cs), # standardized c
			mxAlgebra(name = "es_std", SD %*% es), # standardized e
			# Standardize loadings on Common factors
			mxAlgebra(SD %*% cp_loadings, name = "cp_loadings_std") # Standardized path coefficients (general factor(s))
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_cp', 'top.c_cp', 'top.e_cp', 'top.as_std', 'top.cs_std', 'top.es_std', 'top.cp_loadings_std')))
		}
	}
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a_cp@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a_cp)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a_cp$lbound = newLbound
			model$top$c_cp$lbound = newLbound
			model$top$e_cp$lbound = newLbound
		}
	}
	# Set values with the same label to the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelCP")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	return(model)
} # end umxCP
