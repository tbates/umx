#
#   Copyright 2007-2017 The OpenMx Project
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' Build and run a 2-group Cholesky twin model (uni-variate or multi-variate) with variance estimates.
#'
#' A common task in twin modeling involves using the genetic and environmental differences 
#' between large numbers of pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of one, or, typically, several phenotypes
#' (measured behaviors). umxACEv supports modeling with the ACE Cholesky model, a core model 
#' in behavior genetics (Cardon and Neale, 1996).
#' 
#' This model decomposes phenotypic variance into Additive genetic,
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D). Scroll down to details for how to use the function, a figure
#' and multiple examples.
#' 
#' The Cholesky or lower-triangle decomposition allows a model which is both sure to be 
#' solvable, and also to account for all the variance (with some restrictions) in the data. 
#' This model creates as many latent A C and E variables as there are phenotypes, and, moving 
#' from left to right, decomposes the variance in each component into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram:
#' 
#' \figure{ACE.png}
#' 
#' @details
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
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
#' The umxACEv function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs {ref?}.
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include suffix in names)
#' @param covMethod How to treat covariates: "fixed" (default) or "random".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin var names, often "_T" in vars like "dep_T1". Simplifies selDVs.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data.
#' @param boundDiag = Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0 since umx version 1.8
#' @param weightVar = If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param thresholds How to implement ordinal thresholds c("deviationBased", "WLS").
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param suffix Allowed as a synonym for "suffix".
#' @return - \code{\link{mxModel}} of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' 
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#' # Pick the variables
#' selDVs = c("ht1", "ht2")
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData) # -2ll= 9659
#' umxSummary(m1, std = FALSE) # unstandardized
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' plot(m1)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = selDVs, dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) # nb: though this is ADE, columns are labeled ACE
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' # 1. This variable has a large variance, but umx picks good starts.
#' # 2. umxACEv can figure out variable names: provide "sep" and "wt" -> "wt1" "wt2"
#' # 3. umxACEv picks the variables it needs from the data.
#' # 4. You can use boundDiag to lbound a, c, and e at 0 (prevents mirror-solutions).
#' m1 = umxACEv(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = 0)

#' # We can modify this model, dropping shared environment, and see a comparison
#' m2 = umxModify(m1, update = "c_r1c1", comparison = TRUE)

#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' mzData <- twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData <- twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData <- mzData[1:80,] # quicker run to keep CRAN happy
#' dzData <- dzData[1:80,]
#' selDVs = c("ht", "wt") # umx will add suffix (in this case "") + "1" or '2'
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' umxSummary(m1)
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
#' # Cut bmi column to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' selDVs = c("obese")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- twinData[twinData$zyg == 1, umx_paste_names(selDVs, "", 1:2)]
#' dzData <- twinData[twinData$zyg == 3, umx_paste_names(selDVs, "", 1:2)]
#' mzData <- mzData[1:80,] # just top 80 pairs to run fast
#' dzData <- dzData[1:80,]
#' str(mzData) # make sure mz, dz, and t1 and t2 have the same levels!
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
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
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- twinData[twinData$zyg == 1,] # umxACEv can trim out unused variables on its own
#' dzData <- twinData[twinData$zyg == 3,]
#' mzData <- mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData <- dzData[1:80,]
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' plot(m1)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' cutPoints <- quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' 
#' selDVs = c("wt", "obese")
#' mzData <- twinData[twinData$zygosity %in% "MZFF", umx_paste_names(selDVs, "", 1:2)]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", umx_paste_names(selDVs, "", 1:2)]
#' \dontrun{
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' umxSummary(m1)
#' }
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zyg == 1, selDVs], use = "complete")
#' dz = cov(twinData[twinData$zyg == 3, selDVs], use = "complete")
#' m1 = umxACEv(selDVs = selDVs, dzData = dz, mzData = mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)

umxACEv <- function(name = "ACE", selDVs, selCovs = NULL, covMethod = c("fixed", "random"), dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, 
	weightVar = NULL, equateMeans = TRUE, bVector = FALSE, thresholds = c("deviationBased", "WLS"), autoRun = getOption("umx_auto_run"), sep = NULL, optimizer = NULL) {

		covMethod = match.arg(covMethod)
		# =================
		# = Set optimizer =
		# =================
		if(!is.null(optimizer)){
			umx_set_optimizer(optimizer)
		}
		# Allow sep as synonym for suffix
		if(!is.null(sep)){
			suffix = sep
		}
		# If given covariates, call umxACEvcov
		if(!is.null(selCovs)){
			if(covMethod == "fixed"){
				stop("Implementing this for version 2.0")
				# umxACEvdefcov(name = name, selDVs=selDVs, selCovs=selCovs, dzData=dzData, mzData=mzData, suffix = suffix, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
			} else if(covMethod == "random") {
				umxACEvcov(name = name, selDVs=selDVs, selCovs=selCovs, dzData=dzData, mzData=mzData, suffix = suffix, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
			}
		} else {
			if(nrow(dzData) == 0){ stop("Your DZ dataset has no rows!") }
			if(nrow(mzData) == 0){ stop("Your MZ dataset has no rows!") }
			thresholds = match.arg(thresholds)
			nSib = 2 # number of siblings in a twin pair
			if(dzCr == .25 & name == "ACE"){
				name = "ADE"
			}
			# look for name conflicts
			badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
			if(!identical(character(0), badNames)){
				stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
				"BadNames included: ", omxQuotes(badNames) )
			}

			if(!is.null(suffix)){
				if(length(suffix) > 1){
					stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
					"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
				}
				selDVs = umx_paste_names(selDVs, suffix, 1:2)
			}
			umx_check_names(selDVs, mzData)
			umx_check_names(selDVs, dzData)
			# message("selDVs: ", omxQuotes(selDVs))
			nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

			used = selDVs
			if(!is.null(weightVar)){
				used = c(used, weightVar)
			}
			dataType = umx_is_cov(dzData, boolean = FALSE)
			# Compute numbers of ordinal and binary variables
			if(dataType == "raw"){
				if(!all(is.null(c(numObsMZ, numObsDZ)))){
					stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
				}
				# Drop unused columns from mz and dzData
				mzData = mzData[, used]
				dzData = dzData[, used]
				isFactor = umx_is_ordered(mzData[, selDVs])                      # T/F list of factor columns
				isOrd    = umx_is_ordered(mzData[, selDVs], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
				isBin    = umx_is_ordered(mzData[, selDVs], binary.only  = TRUE) # T/F list of binary columns
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

			if(nFactors > 0 & is.null(suffix)){
				stop("Please set suffix.\n",
				"Why: You have included ordinal or binary variables. I need to know which variables are for twin 1 and which for twin2.\n",
				"The way I do this is enforcing some naming rules. For example, if you have 2 variables:\n",
				" obesity and depression called: 'obesity_T1', 'dep_T1', 'obesity_T2' and 'dep_T2', you should call umxACEv with:\n",
				"selDVs = c('obesity','dep'), suffix = '_T' \n",
				"suffix is just one word, appearing in all variables (e.g. '_T').\n",
				"This is assumed to be followed by '1' '2' etc...")
			}

			if(dataType == "raw") {
				if(!is.null(weightVar)){
					# weight variable provided: check it exists in each frame
					if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
						stop("The weight variable must be included in the mzData and dzData",
							 " frames passed into umxACEv when \"weightVar\" is specified",
							 "\n mzData contained:", paste(names(mzData), collapse = ", "),
							 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
							 "\nbut I was looking for ", weightVar, " as the moderator."
						)
					}
					mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = FALSE, values = mzData[, weightVar])
					dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = FALSE, values = dzData[, weightVar])
					mzData = mzData[, selDVs]
					dzData = dzData[, selDVs]
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
				varStarts = umx_var(mzData[, selDVs[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
				
				# ==============================
				# = Better start value project =
				# ==============================
				# 2017-04-03 04:34PM toggled to sqrt()
				# TODO repeat for other twin models . 2017-08-19 12:21PM umxACEvcov done
				if(nVar == 1){
					# sqrt to switch from var to path coefficient scale
					varStarts = sqrt(varStarts)/3
				} else {
					varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
				}
				varStarts = matrix(varStarts, nVar, nVar)

				# Mean starts (used across all raw solutions
				obsMZmeans   = umx_means(mzData[, selDVs], ordVar = 0, na.rm = TRUE)
				meanDimNames = list("means", selDVs)		

				# Smarter but not guaranteed
				# a_val = e_val = t(chol(xmu_cov_factor(mzData, use = "pair"))) * .6
				# c_val = t(chol(cov(mzData, use = "pair"))) * .1

				# ===============================
				# = Notes: Ordinal requires:    =
				# ===============================
				# 1. Set to mxFactor
				# 2. For Binary vars:
				#   1. Means of binary vars fixedAt 0
				#   2. A + C + E for binary vars is constrained to 1 
				# 4. For Ordinal vars, first 2 thresholds fixed
				# 5. WLS as an option.
				# 6. Option to fix all (or all but the first 2??) thresholds for left-censored data.
		        #   # TODO
				# 	1. Simple test if results are similar for an ACE model of 1 variable
				if(nFactors == 0) {			
					# =======================================================
					# = Handle all continuous case                          =
					# =======================================================
					message("All variables continuous")
					meansMatrix = umxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
					top = mxModel("top", meansMatrix)
					MZ  = mxModel("MZ" , mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
					DZ  = mxModel("DZ" , mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
				} else if(sum(isBin) == 0){
					# ==================================================
					# = Handle 1 or more ordinal variables (no binary) =
					# ==================================================
					message("umxACEv found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames),
					" (No binary)")			
					if(length(contVarNames) > 0){
						message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames))	
					}else{
						# message("No continuous variables found.")
					}
					# Means: all free, start cont at the measured value, ord @0
					meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
					# Thresholds
					# for better guessing with low-frequency cells
					allData = rbind(mzData, dzData)
					# threshMat is is a matrix, or a list of 2 matrices and an algebra
					threshMat = umxThresholdMatrix(allData, sep = suffix, thresholds = thresholds, threshMatName = "threshMat", verbose = FALSE)
					mzExpect = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
					dzExpect = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")			
					top = mxModel("top", umxLabel(meansMatrix), threshMat)
					MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
					DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
				} else if(sum(isBin) > 0){
					if(thresholds == "left_censored"){
						# TODO this is easy, no? binary is fixed threshold anyhow...
						stop("left_censored doesn't make sense for binary variables. I also can't handle mixtures of censored and binary yet, sorry")
					}
					# =======================================================
					# = Handle case of at least 1 binary variable           =
					# =======================================================

					message("umxACEv found ", sum(isBin)/nSib, " pairs of binary variables:", omxQuotes(binVarNames))
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
					# Fill with zeros: default for ordinals and binary...
					meansFree = (!isBin) # fix the binary variables at zero
					meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = obsMZmeans, dimnames = meanDimNames)

					# = Thresholds =
					# For better guessing with low-freq cells
					allData = rbind(mzData, dzData)
					# threshMat may be a three item list of matrices and algebra
					threshMat = umxThresholdMatrix(allData, sep = suffix, thresholds = thresholds, threshMatName = "threshMat", verbose = TRUE)

					mzExpect  = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
					dzExpect  = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")

					top = mxModel("top", umxLabel(meansMatrix), threshMat)
					MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
					DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

					# ===================================
					# = Constrain Ordinal variance @1  =
					# ===================================
					# Algebra to pick out the ord vars
					# TODO check this way of using twin 1 to pick where the bin vars are is robust...
					the_bin_cols = which(isBin)[1:nVar] # columns in which the bin vars appear for twin 1, i.e., c(1,3,5,7)
					binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")

					top = mxModel(top,
						# Algebra to compute total variances and standard deviations
						mxAlgebra(name = "Vtot", A + C+ E), # Total variance (redundant but is OK)
						mxMatrix(name  = "binLabels"  , "Full", nrow = (nBinVars/nSib), ncol = 1, labels = binBracketLabels),
						mxMatrix(name  = "Unit_nBinx1", "Unit", nrow = (nBinVars/nSib), ncol = 1),
						mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
					)
				} else {
					stop("You appear to have something other than I expected in terms of binary, ordinal and continuous variable mix")
				}
				# nb: means not yet equated across twins
			} else if(dataType %in% c("cov", "cor")){
				if(!is.null(weightVar)){
					stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices, or pass in raw data")
				}
				umx_check(!is.null(numObsMZ), "stop", paste0("You must set numObsMZ with ", dataType, " data"))
				umx_check(!is.null(numObsDZ), "stop", paste0("You must set numObsDZ with ", dataType, " data"))

				# Drop unused variables from matrix
				het_mz = umx_reorder(mzData, selDVs)
				het_dz = umx_reorder(dzData, selDVs)
				varStarts = diag(het_mz)[1:nVar]
				
				if(nVar == 1){
					# 2017-04-03 04:34PM: sqrt to switch from var to path coefficient scale
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
				stop("Datatype \"", dataType, "\" not understood. Must be one of raw, cov, or cor")
			}
			message("treating data as ", dataType)

		# Finish building top
		top = mxModel(top,
			# "top" defines the algebra of the twin model, which MZ and DZ slave off of
			# NB: top already has the means model and thresholds matrix added if necessary  - see above
			# Additive, Common, and Unique variance components
			umxMatrix("A", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
			umxMatrix("C", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
			umxMatrix("E", type = "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE), 
		
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
			# Quadratic multiplication to add common_loadings
			mxAlgebra(name = "ACE", A+C+E),
			mxAlgebra(name = "AC" , A+C  ),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC),
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "expCovDZ")
		)

		# =======================================
		# = 		Assemble models into supermodel =
		# =======================================

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
				mxMatrix(name  = "I", "Iden", nVar, nVar), # nVar Identity matrix
				mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
				# TODO test that these are identical in all cases
				# mxAlgebra(vec2diag(1/diag2vec(Vtot)), name = "Var"), # Total variance
				mxAlgebra(name = "Var", solve(I * Vtot)), # Total variance
				mxAlgebra(name = "A_std", Var %*% A), # standardized A
				mxAlgebra(name = "C_std", Var %*% C), # standardized C
				mxAlgebra(name = "E_std", Var %*% E)  # standardized E
			)
			model = mxModel(model, newTop)
			if(addCI){
				model = mxModel(model, mxCI(c('top.A_std', 'top.C_std', 'top.E_std')))
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
		model = as(model, "MxModel.ACE") # set class so that S3 plot() dispatches.
		
		if(autoRun){
			model = mxRun(model)
			umxSummary(model)
			# if(!is.na(umx_set_auto_plot(silent = TRUE))){
				# plot(model)
			# }
		} else {
			# --
		}
		return(model)
	}
} # end umxACEvv
