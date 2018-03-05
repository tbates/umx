#   Copyright 2007-2018 Timothy C. Bates
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
#' @param boundDiag = Numeric lbound for diagonal of the a, c, and e matrices. Default = NULL (no bound)
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
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' umxSummary(m1, std = FALSE) # unstandardized
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' plot(m1)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = selDVs, dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) # nb: though this is ADE, matrices are still called A,C,E
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' # 1. This variable has a large variance, but umx picks good starts.
#' # 2. umxACEv can figure out variable names: provide "sep" and "wt" -> "wt1" "wt2"
#' # 3. umxACEv picks the variables it needs from the data.
#' 
#' # We can modify this model, dropping shared environment, and see a comparison:
#' m2 = umxModify(m1, update = "C_r1c1", comparison = TRUE)

#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' 
#' data(twinData)
#' twinData$ht1 = twinData$ht1 *1000 # convert m to mm
#' twinData$ht2 = twinData$ht2 *1000
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData = mzData[1:80,] # quicker run to keep CRAN happy
#' dzData = dzData[1:80,]
#' m1 = umxACEv(selDVs = c("ht", "wt"), suffix = '', dzData = dzData, mzData = mzData)
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

umxACEv <- function(name = "ACE", selDVs, selCovs = NULL, covMethod = c("fixed", "random"), dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = NULL, 
	weightVar = NULL, equateMeans = TRUE, bVector = FALSE, thresholds = c("deviationBased", "WLS"), autoRun = getOption("umx_auto_run"), sep = NULL, optimizer = NULL) {

		# message("This is STRICTLY experimental, and not complete (prep for Boulder 2018 use of variance components modeling)")
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
				stop("Implementing fixed means effects for version 2.0")
				# umxACEvdefcov(name = name, selDVs=selDVs, selCovs=selCovs, dzData=dzData, mzData=mzData, suffix = suffix, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
			} else if(covMethod == "random") {
				message("umxACEvcov not yet implemented")
				# TODO implement umxACEvcov or refactor
				# umxACEvcov(name = name, selDVs=selDVs, selCovs=selCovs, dzData=dzData, mzData=mzData, suffix = suffix, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
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
				# Drop unused columns from MZ and dzData
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
				# Figure out start values for raw data (used to fill a, c, and e)
				# mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)
				varStarts = umx_var(mzData[, selDVs[1:nVar], drop = FALSE], format = "diag", ordVar = 1, use = "pairwise.complete.obs")
				
				# ==============================
				# = Better start value project =
				# ==============================
				# TODO check varStarts for variance coefficients version!!!!
				if(nVar == 1){
					varStarts = varStarts/3
				} else {
					varStarts = diag(varStarts/3) # Divide variance up equally
				}
				varStarts = matrix(varStarts, nVar, nVar)

				# Mean starts (used across all raw solutions
				obsMZmeans   = umx_means(mzData[, selDVs], ordVar = 0, na.rm = TRUE)
				meanDimNames = list("means", selDVs)		

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
					meansMatrix = umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
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
						stop("left_censored does not make sense for binary variables. I also can't handle mixtures of censored and binary yet, sorry")
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
						# mxAlgebra(name = "Vtot", A + C+ E), # Total variance (redundant but is OK)
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
				het_mz    = umx_reorder(mzData, selDVs)
				het_dz    = umx_reorder(dzData, selDVs)

				# start variances
				varStarts = diag(het_mz)[1:nVar]
				varStarts = matrix(varStarts/3, nVar, nVar)

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
				mxMatrix(name  = "I", "Iden", nVar, nVar), # nVar Identity matrix
				# mxAlgebra(vec2diag(1/diag2vec(Vtot)), name = "Vtot"), # total variance --> SD
				mxAlgebra(name = "Vtot", A + C+ E),        # Total variance
				mxAlgebra(name = "InvSD", sqrt(solve(I * Vtot))), # total variance --> 1/SD
				# TODO test that these are identical in all cases

				# Standardised _variance_ coefficients ready to be stacked together
				# A_std = InvSD %&% A 
				mxAlgebra(name = "A_std", InvSD %&% A), # standardized A
				mxAlgebra(name = "C_std", InvSD %&% C), # standardized C
				mxAlgebra(name = "E_std", InvSD %&% E)  # standardized E
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
		model = as(model, "MxModel.ACEv") # set class so that S3 plot() dispatches.
		
		if(autoRun){
			model = mxRun(model)
			umxSummary(model)
		}
		return(model)
	}
} # end umxACEvv


#' Shows a compact, publication-style, summary of a variance-based Cholesky ACE model.
#'
#' Summarize a fitted Cholesky model returned by \code{\link{umxACEv}}. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for RAM models summary here: \code{\link{umxSummary.MxModel}}.
#' 
#' View documentation on the ACE model subclass here: \code{\link{umxSummary.MxModel.ACE}}.
#' 
#' View documentation on the IP model subclass here: \code{\link{umxSummary.MxModel.IP}}.
#' 
#' View documentation on the CP model subclass here: \code{\link{umxSummary.MxModel.CP}}.
#' 
#' View documentation on the GxE model subclass here: \code{\link{umxSummary.MxModel.GxE}}.
#' 
#' @aliases umxSummary.MxModel.ACEv
#' @param model an \code{\link{mxModel}} to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = no plot.
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If "html", then open an html table of the results
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - \code{\link{umxACEv}} 
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData <- subset(twinData, zygosity == "MZFF")
#' dzData <- subset(twinData, zygosity == "DZFF")
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' \dontrun{
#' umxSummary(m1, file = NA);
#' umxSummary(m1, file = "name", std = TRUE)
#' stdFit = umxSummary(m1, returnStd = TRUE)
#' }
umxSummaryACEv <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	report = match.arg(report)
	# depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	if(is.null(comparison)){
		message(model$name, " -2 \u00d7 log(Likelihood)") # \u00d7 = times sign
		print(-2 * logLik(model));			
	} else {
		message("Comparison of model with parent model:")
		umxCompare(comparison, model, digits = 3)
	}
	selDVs = dimnames(model$top.expCovMZ)[[1]]
	nVar <- length(selDVs)/2;
	# TODO umxSummaryACEv these already exist if a_std exists..
	# TODO replace all this with umx_standardizeACE
	# Calculate standardized variance components

	# a  <- mxEval(top.a, model); # Path coefficients
	# c  <- mxEval(top.c, model);
	# e  <- mxEval(top.e, model);

	A  <- mxEval(top.A, model); # Variances
	C  <- mxEval(top.C, model);
	E  <- mxEval(top.E, model);

	if(std){
		message("Standardized solution")
		Vtot  = A + C + E; # Total variance
		I     = diag(nVar); # nVar Identity matrix
		InvSD = sqrt(solve(I * Vtot))

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
	rowNames = sub("_.1$", "", selDVs[1:nVar])
	Estimates = data.frame(cbind(AClean, CClean, EClean), row.names = rowNames, stringsAsFactors = FALSE);

	colNames = c("A", "C", "E")
	if(model$top$dzCr$values == .25){
		colNames = c("A", "D", "E")
	}
	names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		# depends on R2HTML::HTML
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = F, sortableDF = T); 
		umx_open("tmp.html")
	}
	
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
			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
			# discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			# These can be names ("top.a_std[1,1]") or labels ("a11")
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
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
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
				CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString = paste0(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]")

				if(grepl("^a", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with A, C, or E. You sent: ", thisMatrixName))
				}
			}
			# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
			# TODO umxSummaryACE: Add option to use mxSE
			# print(A_CI)
			# print(C_CI)
			# print(E_CI)
			Estimates = data.frame(cbind(A_CI, C_CI, E_CI), row.names = rowNames, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
			if(report == "html"){
				# depends on R2HTML::HTML
				R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T); 
				umx_open("tmpCI.html")
			}
			CI_Fit = model
			CI_Fit$top$a$values = A_CI
			CI_Fit$top$c$values = C_CI
			CI_Fit$top$e$values = E_CI
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

umxSummary.MxModel.ACEv <- umxSummaryACEv

#' Produce a graphical display of an ACE variance-components twin model
#'
#' Plots an ACE model graphically, opening the result in the browser (or a graphviz application).
#'
#' @aliases plot.MxModel.ACEv
#' @param x \code{\link{umxACEv}} model to plot.
#' @param file The name of the dot file to write: Default ("name") = use the name of the model. NA = don't plot.
#' @param digits How many decimals to include in path loadings (default = 2)
#' @param means Whether to show means paths (default = FALSE)
#' @param std Whether to standardize the model (default = TRUE)
#' @param ... Additional (optional) parameters
#' @return - optionally return the dot code
#' @export
#' @family Plotting functions
#' @family Reporting functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = "bmi"
#' mzData <- subset(twinData, zygosity == "MZFF")
#' dzData <- subset(twinData, zygosity == "DZFF")
#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "")
#' plot(m1, std = FALSE) # don't standardize
umxPlotACEv <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, ...) {
	if(!class(x) == "MxModel.ACEv"){
		stop("The first parameter of umxPlotACE must be an ACEv model, you gave me a ", class(x))
	}
	model = x # Just to be clear that x is a model
	if(std){
		model = umx_standardize(model)
	}
	out = "";
	latents = c();
	if(model$MZ$data$type == "raw"){
		selDVs = names(model$MZ$data$observed)
	}else{
		selDVs = dimnames(model$MZ$data$observed)[[1]]
	}
	varCount = length(selDVs)/2;
	parameterKeyList = omxGetParameters(model);
	for(thisParam in names(parameterKeyList) ) {
		value = parameterKeyList[thisParam]
		if(class(value) == "numeric") {
			value = round(value, digits)
		}
		if (grepl("^[ACE]_r[0-9]+c[0-9]+", thisParam)) { # a c e
			from    = sub('([ACE])_r([0-9]+)c([0-9]+)', '\\1\\3', thisParam, perl = TRUE);  # a c or e
			target  = as.numeric(sub('([ACE])_r([0-9]+)c([0-9]+)', '\\2', thisParam, perl = TRUE));
			target  = selDVs[as.numeric(target)]
			latents = append(latents, from)
			showThis = TRUE
		} else { # means probably
			if(means){
				showThis = TRUE
			} else {
				showThis = FALSE
			}
			from   = thisParam;
			target = sub('r([0-9])c([0-9])', 'var\\2', thisParam, perl=TRUE) 
		}
		if(showThis){
			out = paste0(out, from, " -> ", target, " [label = \"", value, "\"]", ";\n")
		}
	}
	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
	   preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
	}

	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(var in selDVs[1:varCount]) {
	   preOut = paste0(preOut, "\t", var, " [shape = square];\n")
	}

	rankVariables = paste("\t{rank = same; ", paste(selDVs[1:varCount], collapse = "; "), "};\n") # {rank = same; v1T1; v2T1;}
	# grep('a', latents, value=T)
	rankA   = paste("\t{rank = min; ", paste(grep('a'   , latents, value = TRUE), collapse = "; "), "};\n") # {rank=min; a1; a2}
	rankCE  = paste("\t{rank = max; ", paste(grep('[ce]', latents, value = TRUE), collapse = "; "), "};\n") # {rank=min; c1; e1}
	digraph = paste("digraph G {\n\tsplines = \"FALSE\";\n", preOut, out, rankVariables, rankA, rankCE, "\n}", sep="");
	xmu_dot_maker(model, file, digraph)
} # end umxPlotACE

#' @export
plot.MxModel.ACEv <- umxPlotACEv


#' Standardize an ACE variance components model (ACEv)
#'
#' umx_standardize_ACE allows umx_standardize to standardize an ACE variance components model.
#'
#' @param model An \code{\link{umxACEv}} model to standardize.
#' @param ... Other parameters.
#' @return - A standardized \code{\link{umxACEv}} model.
#' @export
#' @family zAdvanced Helpers
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData <- twinData[twinData$zyg == 1, selDVs][1:80,] # 80 pairs for speed
#' dzData <- twinData[twinData$zyg == 3, selDVs][1:80,]
#' m1  = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' std = umx_standardize(m1)
umx_standardize_ACEv <- function(model, ...) {
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
		Vtot = A + C + E;  # Total variance
		I  <- diag(nVar);  # nVar Identity matrix
		# Inverse of diagonal matrix of standard deviations. In old money, this was (\sqrt(I.Vtot))~
		InvSD <- solve(sqrt(I * Vtot)) 
	
		# Standardized _path_ coefficients ready to be stacked together
		model$top$A$values = InvSD %&% A; # Standardized variance components
		model$top$C$values = InvSD %&% C;
		model$top$E$values = InvSD %&% E;
		return(model)
	}
}
#' @export
umx_standardize.MxModel.ACEv <- umx_standardize_ACEv