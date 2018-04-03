#
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

#' Run a Cholesky with covariates  ("fixed" / definition variables in the means style)
#'
#' Often, it is appropriate to include covariates in models.
#' A simple method is to regress covariates from the data using \code{\link{lm}}.
#' This is a 'fixed' effects approach.
#' \code{\link{umx_residualize}} makes this easier, even on twin data, and with complex regression formulas.
#' 
#' While these estimates are unbiased, modeling this regression in the means element of the twin model
#' allows correct tests for significance. Also, if DVs are not continuous, the lm-based approach
#' cannot be used.
#' 
#' For this reason, we have implemented umxACE_cov_fixed, which allows including covariates as definition variables.
#' The following figure shows how the ACE model with fixed covariates appears as a path diagram:
#' \figure{ACEcovOnMeans.png}
#' 
#' @details
#' On the plus side, there is no distributional assumption for this method. A downside of this approach is that all 
#' covariates must be non-NA, thus dropping any rows where one or more covariates are missing.
#' This is wasteful of data, but often cannot be avoided (though see note below).
#' 
#' \emph{note}: An alternative is the \code{\link{umxACEcov}} 'random' option. This model adds covariates to
#' the expected covariance matrix, thus allowing all data to be preserved.
#' The (BIG) downside is that this method has a strong assumption of multivariate normality.
#' Covariates like age, which are perfectly correlated in twins cannot be used.
#' Covariates like sex, which are ordinal, violate the normality assumption.
#' 
#' @param name The name of the model (defaults to"ACEcov").
#' @param selDVs The variables to include from the data (do not include suffixes).
#' @param selCovs The covariates to include from the data (do not include suffixes).
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep Separator text between basename for twin variable names. Often "_T".
#' Used to expand selDVs into full column names, i.e., "dep" --> c("dep_T1", "dep_T2").
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices.
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param thresholds How to implement ordinal thresholds: c("deviationBased", "left_censored").
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param weightVar (optional) Variable containing the weights to apply to data.
#' @param autoRun Whether to run the model and return it, or just return it.
#' @param optimizer (optionally) set the optimizer. Default (NULL) does nothing.
#' @param suffix deprecated synonym for 'sep' (see above).
#' @return - \code{\link{mxModel}} of subclass mxModel.ACEcov
#' @seealso umx_residualize umxACE
#' @family Twin Modeling Functions
#' @export
#' @examples
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#' # Pick the variables
#' selDVs  = "ht"
#' selCovs = "age"
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACE_cov_fixed(selDVs = selDVs, selCovs = selCovs, 
#' 	     dzData = dzData, mzData = mzData, sep = "")
#' m2 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "")
#' # =======================
#' # = lm-based equivalent =
#' # =======================
#' df_res = umx_residualize(ht ~ age, suffixes = c("1", "2"), data = twinData)
#' mzData <- df_res[df_res$zygosity %in% "MZFF", ]
#' dzData <- df_res[df_res$zygosity %in% "DZFF", ]
#' m3 = umxACE("lm_based", selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "")
#' # ===============================
#' # = Example with two covariates =
#' # ===============================
#' selDVs  = "wt"
#' selCovs = c("age", "cohort")
#' twinData$cohort1 = twinData$cohort2 = as.numeric(as.factor(twinData$cohort))
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACE_cov_fixed(selDVs = selDVs, selCovs = selCovs,
#' 	     dzData = dzData, mzData = mzData, sep = "")
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = "")
umxACE_cov_fixed <- function(name = "ACEcov", selDVs, selCovs = NULL, dzData, mzData, sep = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = 0, weightVar = NULL, equateMeans = TRUE, bVector = FALSE, thresholds = c("deviationBased", "WLS"), optimizer = NULL, autoRun = getOption("umx_auto_run"), suffix = NULL) {
		nSib = 2 # number of siblings in a twin pair
		thresholds = match.arg(thresholds)
		if(!is.null(sep)){ suffix = sep }
		if(dzCr == .25 && name == "ACEcov"){ name = "ADEcov"}
		xmu_twin_check(selDVs= c(selDVs, selCovs), dzData = dzData, mzData = mzData, optimizer = optimizer, sep = sep)

		if(is.null(selCovs)){
			stop("You need to give me some covariates (if there are none, just use umxACE)")
		}
		
		baseDV_names  = selDVs
		baseCov_names = selCovs
		selDVs  = umx_paste_names(selDVs , suffix, 1:nSib)
		selCovs = umx_paste_names(selCovs, suffix, 1:nSib)
		nCov = length(baseCov_names)
		nDVs = length(baseDV_names); # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
		used = c(selDVs, selCovs)
		if(!is.null(weightVar)){
			used = c(used, weightVar)
		}
		# Drop unused columns
		mzData = mzData[, used]
		dzData = dzData[, used]
		
		# Drop rows with missing covariates
		OK = complete.cases(mzData[, selCovs])
		if(sum(!OK)>0){
			message(sum(!OK), " cases in mzData do not have complete covariates, and are being dropped from the model.")
			mzData = mzData[OK,]
		}

		OK = complete.cases(dzData[, selCovs])
		if(sum(!OK)>0){
			message(sum(!OK), " cases in dzData do not have complete covariates, and are being dropped from the model.")
			dzData = dzData[OK,]
		}

		# Compute numbers of ordinal and binary variables
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

		if(nFactors > 0 & is.null(suffix)){
			stop("Please set suffix.\n",
			"Why: You have included ordinal or binary variables. I need to know which variables are for twin 1 and which for twin2.\n",
			"The way I do this is enforcing some naming rules. For example, if you have 2 variables:\n",
			" obesity and depression called: 'obesity_T1', 'dep_T1', 'obesity_T2' and 'dep_T2', you should call umxACE with:\n",
			"selDVs = c('obesity','dep'), suffix = '_T' \n",
			"suffix is just one word, appearing in all variables (e.g. '_T').\n",
			"This is assumed to be followed by '1' '2' etc...")
		}

		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 " frames passed into umxACE when \"weightVar\" is specified",
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
		# Figure out start values for a, c, and e
		varStarts = umx_var(mzData[, selDVs[1:nDVs], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
		
		if(nDVs == 1){
			# sqrt to switch from var to path coefficient scale
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nDVs, nDVs)

		# Mean starts
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

			# =====================================================================
			# = Create Matrices for Covariates and linear Regression Coefficients =
			# =====================================================================
			# make a def matrix containing covariates
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEma.R
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEja.R
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEca.R

			# copies to debug
			# nSib            = 2
			# baseDV_names    = c("ht", "wt")
			# baseCov_names   = c("age", "sex")
			# baseDV_names    = c("ht")
			# baseCov_names   = c("age")
			# nDVs            = length(baseDV_names)
			# nCov            = length(baseCov_names)
			# selDVs          = umx_paste_names(baseDV_names, "_T")
			# selCovs         = umx_paste_names(baseCov_names, "_T")
			# obsMZmeans      = rep(0, length(selDVs))

			# ================
			# = Bits for top =
			# ================
			# 1. betas is an [nCov, nDVs] matrix
			# TODO: add intercept to incoming cov list
			# TODO support quadratic betas on means
			
			T1Covs = selCovs[1:nCov]
			T2Covs = selCovs[(nCov+1):length(selCovs)]
			betaLabels = paste0("b", rep(1:nCov, each = nDVs), "_", rep(baseDV_names, nCov))
 			betaDims = list(paste0("bcov", 1:nCov), paste0("var", 1:(nDVs/nSib)))
			betas = umxMatrix("betas", "Full", nrow = nCov, ncol = nDVs, free = TRUE, values = .01, labels = betaLabels, dimnames = betaDims)

			# 2. Intercepts is a [1, nDVs*nSib] matrix (not yet equated across twins...)
			Intercepts = umxMatrix("Intercepts", "Full", nrow = 1, ncol = (nDVs * nSib), free = TRUE, values = obsMZmeans, dimnames = list("int", selDVs))

			top = mxModel("top", betas, Intercepts)
			MZ = mxModel("MZ",
				umxMatrix("defCovT1", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", T1Covs), dimnames = list("defCovT1", T1Covs)),
				umxMatrix("defCovT2", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", T2Covs), dimnames = list("defCovT2", T2Covs)),
				mxAlgebra(name = "expMean", top.Intercepts + cbind(defCovT1 %*% top.betas, defCovT2 %*% top.betas), dimnames = list(NULL, selDVs)),
				mxExpectationNormal("top.expCovMZ", "expMean"),
				mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw")
			)
			DZ = mxModel("DZ", 
				umxMatrix("defCovT1", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", T1Covs), dimnames = list("defCovT1", T1Covs)),
				umxMatrix("defCovT2", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", T2Covs), dimnames = list("defCovT2", T2Covs)),
				mxAlgebra(name = "expMean", top.Intercepts + cbind(defCovT1 %*% top.betas, defCovT2 %*% top.betas), dimnames = list(NULL, selDVs)),
				mxExpectationNormal("top.expCovDZ", "expMean"), 
				mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw")
			)
		} else if(sum(isBin) == 0){
			# ==================================================
			# = Handle 1 or more ordinal variables (no binary) =
			# ==================================================
			message("umxACE found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames), " (No binary)")		
			if(length(contVarNames) > 0){
				message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames))	
			}else{
				# message("No continuous variables found.")
			}
			# Means: all free, start cont at the measured value, ord @0
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nDVs * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
			# Thresholds
			# for better guessing with low-frequency cells
			allData = rbind(mzData, dzData)
			# threshMat is is a matrix, or a list of 2 matrices and an algebra
			threshMat = umxThresholdMatrix(allData, sep = suffix, thresholds = thresholds, threshMatName = "threshMat", verbose = FALSE)
			mzExpect  = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
			dzExpect  = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")			
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
			# Fill with zeros: default for ordinals and binary...
			meansFree = (!isBin) # fix the binary variables at zero
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = nDVs*nSib, free = meansFree, values = obsMZmeans, dimnames = meanDimNames)

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
			the_bin_cols = which(isBin)[1:nDVs] # columns in which the bin vars appear for twin 1, i.e., c(1,3,5,7)
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

	# Finish building top
	top = mxModel(top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique environmental paths
		umxMatrix("a", type = "Lower", nrow = nDVs, ncol = nDVs, free = TRUE, values = varStarts, byrow = TRUE),
		umxMatrix("c", type = "Lower", nrow = nDVs, ncol = nDVs, free = TRUE, values = varStarts, byrow = TRUE),
		umxMatrix("e", type = "Lower", nrow = nDVs, ncol = nDVs, free = TRUE, values = varStarts, byrow = TRUE), 
	
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "A", a %*% t(a)), # additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # unique environmental variance
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(name = "expCovMZ",
					rbind(cbind(ACE, AC),
					      cbind(AC , ACE)), dimnames = list(selDVs, selDVs)),
		mxAlgebra(name = "expCovDZ",
					rbind(cbind(ACE, hAC),
					      cbind(hAC, ACE)), dimnames = list(selDVs, selDVs))
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
			mxMatrix(name  = "I", "Iden", nDVs, nDVs), # nDVs Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),        # Total variance
			# TODO test that these are identical in all cases
			# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # Total variance
			mxAlgebra(name = "SD"   , solve(sqrt(I * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans){
		# in basic ACE models, this acts on expMean.
		# Here, we equate halves of the [1* (nDVs * nSib)] Intercepts matrix
		model = omxSetParameters(model,
		  labels    = paste0("Intercepts_r1c", (nDVs + 1):(nDVs * nSib)), # c("Intercepts14", "Intercepts15", "Intercepts16"),
		  newlabels = paste0("Intercepts_r1c", 1:nDVs)                    # c("Intercepts11", "Intercepts12", "Intercepts13")
		)
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModel.ACE") # set class so that S3 plot() dispatches.
	
	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
	}
	return(model)
} # end umxACE