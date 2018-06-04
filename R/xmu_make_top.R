#' Helper to make a basic top, MZ, and DZ model
#'
#' @description
#' xmu_make_top makes basic `top`, `MZ`, and `DZ` models. It handles the thresholds matrix if needed.
#' 
#' This function takes the mzData and dzData, a list of the selDVs to analyse, along with other relevant information such as whether the user wants to equateMeans, and what threshType to use (currently "deviationBased", but hopefully "WLS" and. It can also handle a weightVar.
#' 
#' `varStarts` is computed as `sqrt(variance)/3` of the DVs and `obsMeans` as the variable means.
#' For raw data, a check is made for ordered variables.
#' 
#' For Binary variables, means are fixed at 0 and total variance (A+C+E) is fixed at 1.
#' 
#' For ordinal variables, the first 2 thresholds are fixed.
#' 
#' For continuous variables, `top` just contains an `expMean` matrix.
#' 
#' `MZ` and `DZ` is the data, and an expectation of for `top.expCovMZ` and `top.expMean`, possibly referencing vector = bVector in the fit function.
#' 
#' For continuous variables, `top` just contains an `expMean` matrix.
#' 
#' For ordinal, `top` gains `top.threshMat` (from a call to `umxThresholdMatrix`). `MZ` and `DZ` are as with continuous, but adding thresholds.
#' 
#' For binary, `Vtot` (A+C+E) is set to 1.
#' 
#' For summary data, top is just  name, and MZ and DZ have cov data in them.
#' 
#' If a weightVar is detected, this column is added to  mzWeightMatrix/mzWeightMatrix
#' 
#' If `equateMeans` is `TRUE`, then the Twin-2 vars in the mean matrix are equated by label with Twin-1.
#'
#' On the TODO-list is to have WLS as an option. Also censored data.
#' @param mzData Dataframe containing the MZ data 
#' @param dzData Dataframe containing the DZ data 
#' @param selDVs List of base (e.g. BMI) (i.e., NOT 'BMI_T1') variable names
#' @param sep Used to expand selDVs into selVars, i.e., "_T" to expand BMI into BMI_T1 and BMI_T2
#' @param nSib Number of members per family (default = 2)
#' @param numObsMZ Number of MZ observations contributing (for summary data only) 
#' @param numObsDZ Number of DZ observations contributing (for summary data only)
#' @param equateMeans Whether to equate T1 and T2 means (default = TRUE)
#' @param threshType what type of thresholds to implement if needed.
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param verbose (default = FALSE)
# #' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @return - \code{\link{mxModel}}s for top, MZ and DZ.
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' # ==============
#' # = Continuous =
#' # ==============
#' library(umx)
#' data(twinData)
#' selDVs = c("wt", "ht")
#' mzData = twinData[twinData$zygosity %in%  "MZFF",] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF",]
#' bits = xmu_make_top(mzData = mzData, dzData = dzData, selDVs= selDVs, sep="", nSib = 2)
#' names(bits) # "top" "MZ"  "DZ" 
#' 
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' selDVs = c("wt", "obese")
#' # Cut BMI column to form ordinal obesity variables
#' ordDVs          = c("obese1", "obese2")
#' obesityLevels   = c('normal', 'overweight', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in%  "MZFF",] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF",]
#' bits = xmu_make_top(mzData = mzData, dzData = dzData, selDVs= selDVs, sep="", nSib = 2)
#' names(bits) # "top" "MZ"  "DZ" 
#'
#' # ==============
#' # = One binary =
#' # ==============
#' data(twinData)
#' cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels   = c('normal', 'obese')
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' bits = xmu_make_top(mzData = mzData, dzData = dzData, selDVs = selDVs, sep = "", nSib = 2)
#'
#' # ============
#' # = Cov data =
#' # ============
#' data(twinData)
#' selDVs = c("wt")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", tvars(selDVs, sep="")], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", tvars(selDVs, sep="")], use = "complete")
#' bits = xmu_make_top(mzData = mzData, dzData = dzData, selDVs= selDVs, sep= "", nSib = 2)
#' # TODO Add selCovs??
xmu_make_top <- function(mzData, dzData, selDVs, sep = NULL, nSib = 2, numObsMZ= NULL, numObsDZ= NULL, equateMeans = TRUE, threshType = c("deviationBased", "WLS"), weightVar = NULL, bVector = FALSE, verbose= FALSE) {
	threshType = match.arg(threshType)
	if(is.null(sep)){
		stop("You MUST set 'sep'. Otherwise xmu_make_top can't reliably expand selDVs into full variable names")
	}
	selVars = tvars(selDVs, sep = sep, suffixes= 1:nSib)
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	if(!is.null(weightVar)){
		used = c(selVars, weightVar)
	}else{
		used = selVars
	}
	dataType = umx_is_cov(dzData, boolean = FALSE)

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# find ordinal variables
		if(any(umx_is_ordered(mzData[,selVars]))){
			isFactor = umx_is_ordered(mzData[, selVars])                      # T/F list of factor columns
			isOrd    = umx_is_ordered(mzData[, selVars], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
			isBin    = umx_is_ordered(mzData[, selVars], binary.only  = TRUE) # T/F list of binary columns
			nFactors = sum(isFactor)
			nOrdVars = sum(isOrd) # total number of ordinal columns
			nBinVars = sum(isBin) # total number of binary columns

			factorVarNames = names(mzData[, selVars])[isFactor]
			ordVarNames    = names(mzData[, selVars])[isOrd]
			binVarNames    = names(mzData[, selVars])[isBin]
			contVarNames   = names(mzData[, selVars])[!isFactor]
		}else{
			# Summary data
			isFactor = isOrd    = isBin    = c()
			nFactors = nOrdVars = nBinVars = 0			
			factorVarNames = ordVarNames = binVarNames = contVarNames = c()
		}
		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 "\n frames passed into umxACE when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\n but I was looking for ", weightVar, " as the moderator."
				)
			}
			mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = FALSE, values = mzData[, weightVar])
			dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = FALSE, values = dzData[, weightVar])
			bVector = TRUE
		} else {
			# no weights
			bVector = FALSE
		}
		# Drop any unused columns from MZ and DZ Data
		mzData  = mzData[, selVars]
		dzData  = dzData[, selVars]
		allData = rbind(mzData, dzData)

		# =====================================
		# = Add means and var matrices to top =
		# =====================================
		# Figure out start values while we are here
		# varStarts will be used to fill a, c, and e

		# TODO could use both twins for variance estimation
		varStarts = umx_var(allData[, selVars[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")

		# sqrt to switch from var to path coefficient scale
		if(nVar == 1){
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		# Mean starts (used across all raw solutions
		obsMeans = umx_means(allData, ordVar = 0, na.rm = TRUE)

		# ===============================
		# = Notes: Ordinal requires:    =
		# ===============================
		# 1. Set to mxFactor
		# 2. For Binary variables:
		#   1. Means of binary variables fixedAt 0
		#   2. A + C + E for binary variables is constrained to 1 
		# 4. For Ordinal variables, first 2 thresholds fixed
		# TODO
		#  1. Simple test if results are similar for an ACE model of 1 variable
		#  2. WLS as an option.
		#  3. Option to fix all (or all but the first 2??) thresholds for left-censored data.
		# [] select mxFitFunctionML() of bVector as param
		
		if(nFactors == 0) {			
			# ==================================================
			# = Handle all continuous case                     =
			# ==================================================
			message("All variables continuous")
			top = mxModel("top", 
				umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars))
			)
			MZ  = mxModel("MZ",
				mxData(mzData, type = "raw"),
				mxExpectationNormal("top.expCovMZ", "top.expMean"), 
				mxFitFunctionML(vector = bVector)
			)
			DZ  = mxModel("DZ" , 
				mxData(dzData, type = "raw"),
				mxExpectationNormal("top.expCovDZ", "top.expMean"), 
				mxFitFunctionML(vector = bVector)
			)			
		} else if(sum(isBin) == 0){
			# ==================================================
			# = Handle 1 or more ordinal variables (no binary) =
			# ==================================================
			message("Found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames), " (No binary)")			
			if(length(contVarNames) > 0){
				message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames[1:(length(contVarNames)/nSib)]))	
			}
			# Means: all free, start cont at the measured value, ord @0

			top = mxModel("top", 
				umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars)),
				umxThresholdMatrix(allData, selDVs = selDVs, sep = sep, thresholds = threshType, threshMatName = "threshMat", verbose = verbose)
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
			message("Found ", sum(isBin)/nSib, " pairs of binary variables:", omxQuotes(binVarNames))
			message("\nI am fixing the latent means and variances of these variables to 0 and 1")
			if(nOrdVars > 0){
				message("There were also ", nOrdVars/nSib, " pairs of ordinal variables:", omxQuotes(ordVarNames))			
			}
			if(length(contVarNames) > 0){
				message("\nand ", length(contVarNames)/nSib, " pairs of continuous variables:", omxQuotes(contVarNames[1:(length(contVarNames)/nSib)]))
			}else{
				message("No continuous variables")
			}

			# ===========================================================================
			# = Means: bin fixed, others free, start cont at the measured value, ord @0 =
			# ===========================================================================
			# ===================================
			# = Constrain Ordinal variance @1  =
			# ===================================
			# Algebra to pick out the ordinal variables.
			# TODO check using twin 1 to pick where the bin variables are is robust...
			# Fill with zeros: default for ordinals and binary...
			meansFree = (!isBin) # fix the binary variables at zero (umx_means did this)
			the_bin_cols = which(isBin)[1:nVar] # columns in which the bin vars appear for T1, i.e., c(1,3,5)
			binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")
			top = mxModel("top", 
				umxMatrix("expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = obsMeans, dimnames = list("means", selVars)),
				umxThresholdMatrix(allData, selDVs = selDVs, sep = sep, thresholds = threshType, threshMatName = "threshMat", verbose = verbose),
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
		stop("Datatype \"", dataType, "\" not understood")
	}
	# Equate means for twin1 and twin 2 (match labels in first & second halves of means labels matrix)
	if(equateMeans & dataType == "raw"){
		top = omxSetParameters(top,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
		)
	}
	
	return(list(top = top, MZ = MZ, DZ = DZ))
}


