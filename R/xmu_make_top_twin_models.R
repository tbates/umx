#' Helper to make a basic top, MZ, and DZ model.
#'
#' @description
#' `xmu_make_top_twin` makes basic `top`, `MZ`, and `DZ` models. It includes thresholds matrices in the twin models if needed.
#'
#' This is used in  [umxCP()], and [umxACE()] and [umxACEv()] and will be added to the other models: [umxGxE()], [umxIP()], 
#' simplifying code maintenance.
#' 
#' `xmu_make_top_twin` takes `mzData` and `dzData`, a list of the `selDVs` to analyse (as well as `sep` and `nSib`), along with other 
#' relevant information such as whether the user wants to `equateMeans`.
#' It can also handle a `weightVar`.
#' 
#' `varStarts` is computed as `sqrt(variance)/3` of the DVs and `meanStarts` as the variable means.
#' For raw data, a check is made for ordered variables. For Binary variables, means are fixed at 0 and 
#' total variance (A+C+E) is fixed at 1. For ordinal variables, the first 2 thresholds are fixed.
#' 
#' **Modeling**
#' 
#' *top model*
#' 
#' For raw and WLS data, `top` contains a means matrix (if needed). For summary data, the top model contains only a name.
#' 
#' For ordinal data, `top` gains `top.threshMat` (from a call to [umxThresholdMatrix()]).
#' 
#' *MZ and DZ models*
#' 
#' `MZ` and `DZ` contain the data, and an expectation referencing `top.expCovMZ` and `top.expMean`, and, `vector = bVector`.
#' For continuous raw data, MZ and DZ contain [OpenMx::mxExpectationNormal()] and [OpenMx::mxFitFunctionML()].
#' For WLS these the fit function is switched to [OpenMx::mxFitFunctionWLS()] with appropriate `type` and `allContinuousMethod`.
#' 
#'
#' For binary, a constraint and algebras are included to constrain `Vtot` (A+C+E) to 1.
#' 
#' If a `weightVar` is detected, this column is added to  mzWeightMatrix/mzWeightMatrix.
#' 
#' If `equateMeans` is `TRUE`, then the Twin-2 vars in the mean matrix are equated by label with Twin-1.
#'
#' **Matrices created**
#' 
#' Decent starts are guessed from the data.
#' Where needed, e.g. continuous raw data, top adds a means matrix "expMean". 
#' For ordinal data, top adds a [umxThresholdMatrix()]. 
#' 
#' If binary variables are present, matrices and a constraint to hold `A+C+E == 1` are added to top.
#'
#' If a weight variable is offered up, an `mzWeightMatrix` will be added.
#'
#' **Data handling**
#' 
#' In terms of data handling, `xmu_make_top_twin` was primarily designed to take 
#' data.frames and process these into mxData. 
#' It can also, however, handle cov and mxData input.
#' 
#' It can process data into all the types supported by `mxData`.
#' 
#' Raw data input with a target of `cov` or `cor` type requires the `numObsMZ` and `numObsDZ` to be set.
#' 
#' Type "WLS", "DWLS", or "ULS", data remain raw, but are handled as WLS in the [OpenMx::mxFitFunctionWLS()].
#' 
#' Unused columns are dropped.
#' 
#' If you pass in raw data, you can't request type cov/cor yet. Will work on this if desired.
#'
#' @param mzData Dataframe containing the MZ data 
#' @param dzData Dataframe containing the DZ data 
#' @param selDVs List of base (e.g. BMI) (i.e., NOT 'BMI_T1') variable names (OR, you don't set "sep", the full variable names)
#' @param sep (optional but desirable) string used to expand selDVs into selVars, i.e., "_T" to expand BMI into BMI_T1 and BMI_T2
#' @param type	One of 'Auto','FIML','cov', 'cor', 'WLS','DWLS', or 'ULS'. Auto tries to react to the incoming mxData type (raw/cov).
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param nSib Number of members per family (default = 2)
#' @param equateMeans Whether to equate T1 and T2 means (default = TRUE).
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param numObsMZ Number of MZ observations contributing (for summary data only) 
#' @param numObsDZ Number of DZ observations contributing (for summary data only)
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param verbose (default = FALSE)
#' @return - [mxModel()]s for top, MZ and DZ.
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
# # TODO add tests with numObsMZ = NULL, numObsDZ = NULL, equateMeans = TRUE,
# # TODO add tests with weightVar = NULL,  bVector = FALSE, 
#' # ==============
#' # = Continuous =
#' # ==============
#' library(umx)
#' data(twinData)
#' selDVs = c("wt", "ht")
#' mzData = twinData[twinData$zygosity %in%  "MZFF",] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF",]
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, selDVs= selDVs, sep= "", nSib= 2)
#' names(bits) # "top" "MZ"  "DZ" "bVector" "mzWeightMatrix" "dzWeightMatrix"
#' class(bits$MZ$fitfunction)[[1]] == "MxFitFunctionML"

# WLS example
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, 
#'		selDVs= selDVs, sep= "", type = "WLS")
#' class(bits$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
#' bits$MZ$fitfunction$type =="WLS"
# # Check default all-continuous method
#' bits$MZ$fitfunction$continuousType == "cumulants"
#' 
#' # Choose non-default type (DWLS)
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData,
#'		selDVs = selDVs, sep= "", type = "DWLS")
#' bits$MZ$fitfunction$type =="DWLS"
#' class(bits$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
#' 
#' # Switch continuous method
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, selDVs= selDVs, sep= "",
#'		type = "WLS", allContinuousMethod = "marginals")
#' bits$MZ$fitfunction$continuousType == "marginals"
#' class(bits$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
#' 
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
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, selDVs= selDVs, sep="", nSib= 2)
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
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, selDVs= selDVs, sep= "", nSib= 2)
#'
#' # ============
#' # = Cov data =
#' # ============
#' data(twinData)
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", tvars(c("wt", "ht"), sep="")], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", tvars(c("wt", "ht"), sep="")], use = "complete")
#' bits = xmu_make_top_twin(mzData= mzData, dzData= dzData, selDVs= "wt", sep= "", nSib= 2)
#' class(bits$MZ$fitfunction)[[1]] =="MxFitFunctionML"
#' names(bits$MZ$data$observed) == c("wt1", "wt2") # height columns dropped
#'
xmu_make_top_twin <- function(mzData, dzData, selDVs, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), nSib = 2, numObsMZ = NULL, numObsDZ = NULL, equateMeans = TRUE, weightVar = NULL, bVector = FALSE, verbose= FALSE) {
	# **TODO list for xmu_make_top_twin**
	# TODO: xmu_make_top_twin Add selCovs
	# TODO: xmu_make_top_twin Add covMethod == "fixed"
	# TODO: xmu_make_top_twin Add beta matrix for fixed covariates in means.
	# TODO: xmu_make_top_twin more tests in a test page
	# TODO: Improve the start guesses based on input model type (ACE, CP, IP etc.)

	# *Note*: If dropping this into an existing model, it replaces all code setting: nVar, selVars, used, 
	# Also any code figuring out data-type
	
	# ===================
	# = match arguments =
	# ===================
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)

	# ====================================================
	# = Figure out selVars, nVar, usedVars, and dataType =
	# ====================================================
	if(is.null(sep)){
		selVars = selDVs
		message("Polite note: It's better to use 'sep' - in future, this might become compulsory as it helps manage the variable names.")
		# stop("You MUST set 'sep'. Otherwise xmu_make_top can't reliably expand selDVs into full variable names")
	}else{
		selVars = tvars(selDVs, sep = sep, suffixes= 1:nSib)
	}
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	if(!is.null(weightVar)){
		usedVars = c(selVars, weightVar)
	}else{
		usedVars = selVars
	}
	dataType = umx_is_cov(dzData, boolean = FALSE)
	
	if(type %in% c("cov", "cor") && !dataType %in% c("cov", "cor")){
		stop("You've requested type= cov or cor, but the provided dataType is ", omxQuotes(dataType), " I don't support that yet. Please pass in cov data.")
	}

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Find ordinal variables
		if(any(umx_is_ordered(mzData[,selVars]))){
			if(is.null(sep)){
				stop("With ordinal data in twins, I have to have a separator to figure out which twin is which. Please set selDVs to the base names, and sep='_T' or whatever you used (see ?umxACE)")
			}
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
			# Weight variable provided: check it exists in each frame.
			if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 "\n frames passed into this twin model when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\n but I was looking for ", weightVar, " as the moderator."
				)
			}
			mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = FALSE, values = mzData[, weightVar])
			dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = FALSE, values = dzData[, weightVar])
			bVector = TRUE
		} else {
			# No weights bVector stays whatever it was.
		}
		# =============================================
		# = Figure out start values while we are here =
		# =============================================

		# ===================================================================
		# = NOTE: selVars is expanded by the time we get to here... no sep. =
		# ===================================================================
		tmp = xmu_starts(mzData= mzData, dzData= dzData, selVars= selVars, equateMeans= equateMeans, nSib= nSib, varForm= "Cholesky")
		varStarts  = tmp$varStarts
		meanStarts = tmp$meanStarts
		meanLabels = tmp$meanLabels

		# ============================================
		# = Make mxData, dropping any unused columns =
		# ============================================
		allData = rbind(mzData, dzData)
		mzData = xmu_make_mxData(mzData, type = type, manifests = selVars)
		dzData = xmu_make_mxData(dzData, type = type, manifests = selVars)

		# =====================================
		# = Add means and var matrices to top =
		# =====================================

		# ===============================
		# = Notes: Ordinal requires:    =
		# ===============================
		# 1. Set to mxFactor
		# 2. For Binary variables:
		#   1. Means of binary variables fixedAt 0
		#   2. A + C + E for binary variables is constrained to 1 
		# 4. For Ordinal variables, first 2 thresholds fixed
		# TODO:
		#  1. Simple test if results are similar for an ACE model of 1 variable
		# [] select mxFitFunctionML() of bVector as param
		
		if(nFactors == 0){
			# ===========================================================================
			# = Handle all continuous as special case (for WLS, can affect mean or not) =
			# ===========================================================================
			if(type %in% c('WLS', 'DWLS', 'ULS') & allContinuousMethod == "cumulants"){
				# No means for WLS with cumulants
				top = mxModel("top")
				MZ  = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ") )
				DZ  = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ") )			
			} else {
				# Plain raw data or intended for WLS and (allContinuousMethod != cumulants) so top needs means and MZ and DZ a means model.
				top = mxModel("top",
					umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = meanStarts, labels = meanLabels, dimnames = list("means", selVars))
				)
				MZ  = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean") )
				DZ  = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean") )			
			}
		} else if(sum(isBin) == 0){
			# ==================================================
			# = Handle 1 or more ordinal variables (no binary) =
			# ==================================================
			message("Found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames), " (No binary)")			
			if(length(contVarNames) > 0){
				message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames[1:(length(contVarNames)/nSib)]))	
			}
			# Means: all free, start cont at the measured value, ordinals @0

			top = mxModel("top",
				umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = meanStarts, labels = meanLabels, dimnames = list("means", selVars)),
				umxThresholdMatrix(allData, selDVs = selVars, sep = sep, verbose = verbose)
			)
			MZ  = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat") )
			DZ  = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat") )
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
				umxMatrix("expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = meanStarts, labels = meanLabels, dimnames = list("means", selVars)),
				umxThresholdMatrix(allData, selDVs = selVars, verbose = verbose),
				mxAlgebra(name = "Vtot", A + C + E), # Total variance (also added by models with std = TRUE, but is OK to add twice)
				umxMatrix("binLabels"  , "Full", nrow = (nBinVars/nSib), ncol = 1, labels = binBracketLabels),
				umxMatrix("Unit_nBinx1", "Unit", nrow = (nBinVars/nSib), ncol = 1),
				mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
			)
			MZ  = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat") )
			DZ  = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat") )
		} else {
			stop("You appear to have something other than I expected in terms of WLS, or binary, ordinal and continuous variable mix")
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

		top = mxModel("top")
		MZ  = mxModel("MZ", mxExpectationNormal("top.expCovMZ"), mxData(het_mz, type = "cov", numObs = numObsMZ) )
		DZ  = mxModel("DZ", mxExpectationNormal("top.expCovDZ"), mxData(het_dz, type = "cov", numObs = numObsDZ) )
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}
	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
		message("data treated as ", type)
		# Still mxExpectationNormal (`top` is not affected - either has or lacks means matrix already).
		# Replace the MZ and DZ model fit functions
		MZ = mxModel(MZ, mxFitFunctionWLS(type= type, allContinuousMethod= allContinuousMethod) )
		DZ = mxModel(DZ, mxFitFunctionWLS(type= type, allContinuousMethod= allContinuousMethod) )
	}else{
		MZ = mxModel(MZ, mxFitFunctionML(vector = bVector) )
		DZ = mxModel(DZ, mxFitFunctionML(vector = bVector) )
	}

	if(bVector){
		return(list(top = top, MZ = MZ, DZ = DZ, bVector = bVector, mzWeightMatrix = mzWeightMatrix, dzWeightMatrix = dzWeightMatrix))
	} else {
		return(list(top = top, MZ = MZ, DZ = DZ, bVector = bVector, mzWeightMatrix = NULL, dzWeightMatrix = NULL))
	}	
}                                           


#' Helper providing twin models with boilerplate means and variance start values
#'
#' @description
#' `xmu_starts` can handle several common/boilerplate situations in which means and variance start values
#' are used in twin models.
#'
#' @param mzData Data for MZ pairs.
#' @param dzData Data for DZ pairs.
#' @param selVars Variable names: If sep = NULL, then treated as full names for both sibs.
#' @param sep All the variables full names.
#' @param equateMeans (NULL)
#' @param nSib How many subjects in a family.
#' @param varForm currently just "Cholesky" style.
#' @param SD = TRUE (FALSE = variance, not SD).
#' @param divideBy = 3 (A,C,E) 1/3rd each. Use 1 to do this yourself post-hoc.
#' @return - varStarts and meanStarts
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' data(twinData)
#' selDVs = c("wt", "ht")
#' mzData = twinData[twinData$zygosity %in%  "MZFF", ] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF", ]
#' 
#' round(sqrt(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3),3)
#' 
#' tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky")
#' tmp
#' 
#' round(var(dzData[,tvars(selDVs, "")], na.rm=TRUE)/3,3)
#' tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#' 
#' tmp
#' 
#' # one variable
#' tmp = xmu_starts(mzData, dzData, selVars = "wt", sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#' 
#' # Ordinal/continuous mix
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
#' # Cut BMI column to form ordinal obesity variables
#' obLevels   = c('normal', 'overweight', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = c(.5, .8), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' # Make the ordinal variables into mxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' tmp = xmu_starts(mzData, dzData, selVars = c("wt","obese"), sep= "", 
#'		equateMeans = TRUE, varForm = "Cholesky", SD= FALSE)
#'
xmu_starts <- function(mzData, dzData, selVars = selVars, sep = NULL, equateMeans= NULL, nSib = 2, varForm = c("Cholesky"), SD= TRUE, divideBy = 3) {
	# Make mxData, dropping any unused columns
	if(!is.null(sep)){
		# sep = ""; nSib = 2; selVars = c("wt", "ht")
		selVars = umx_paste_names(selVars, sep = sep, suffixes = 1:nSib)
	}

	nVar = length(selVars)/nSib
	dataType = umx_is_cov(dzData, boolean = FALSE)
	if(dataType == "raw") {
		if(is.null(equateMeans)){
			stop("you have to tell xmu_starts whether to equate the means or not")
		}
		allData = rbind(mzData, dzData)[,selVars]
		T1 = allData[, 1:nVar, drop = FALSE]
		T2 = allData[, (nVar+1):(nVar*2), drop = FALSE];
		names(T2) = names(T1)
		longData = rbind(T1, T2)[, selVars[1:nVar], drop = FALSE]
		# Mean starts (used across all raw solutions
		meanStarts = umx_means(longData, ordVar = 0, na.rm = TRUE)
		# Make wide again
		meanStarts = c(meanStarts, meanStarts)
		if(equateMeans){
			meanLabels = paste0("expMean_", selVars[1:nVar]) # Names recycled for twin 2
			meanLabels = c(meanLabels, meanLabels)
		} else {
			meanLabels = paste0("expMean_", selVars)
		}
		varStarts = umx_var(longData, format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	} else if(dataType %in% c("cov", "cor")){
		meanStarts = NA # Not used for summary data
		meanLabels = NA
		het_mz = umx_reorder(mzData, selVars)		
		het_dz = umx_reorder(dzData, selVars)
		varStarts = (diag(het_mz)[1:nVar]+ diag(het_dz)[1:nVar])/2
	}else{
		stop("xmu_starts can only handle raw and cov/cor data types. You gave me ", omxQuotes(dataType))
	}
	# Covariance matrix, 1/3 allocated to each of A=C=E.
	varStarts = varStarts/divideBy
	if(varForm == "Cholesky"){
		if(SD){
			# sqrt() to switch from var to path coefficient scale
			# Sqrt to switch from var to path coefficient scale
			varStarts = t(chol(diag(varStarts, length(varStarts))))
			varStarts = matrix(varStarts, nVar, nVar)
		} else {
			varStarts = diag(varStarts, nVar, nVar)
		}
	} else {
		stop("Not sure how to handle varForm = ", omxQuotes(varForm))
	}
	return(list(varStarts=varStarts, meanStarts= meanStarts, meanLabels= meanLabels))
}



#' Assemble top, MZ and DZ into a supermodel
#'
#' @description
#' Assemble top, MZ and DZ into a supermodel: Also copes with weighted analyses.
#'
#' @param name the name of the supermodel
#' @param MZ the MZ model
#' @param DZ the DZ model
#' @param top the top model
#' @param mzWeightMatrix if bVector, then use this as the MZ weights matrix
#' @param dzWeightMatrix if bVector, then use this as the DZ weights matrix
#' @return - [mxModel()]
#' @export
#' @family xmu internal not for end user
#' @md
xmu_assemble_twin_supermodel <- function(name, MZ, DZ, top, mzWeightMatrix = NULL, dzWeightMatrix = NULL) {
	# TODO: xmu_assemble_twin_supermodel: Add working example.
	# TODO: xmu_assemble_twin_supermodel: Add a check that MZ & DZ models have vector=TRUE selected if the mzWeightMatrix's is !NULL
	if(is.null(mzWeightMatrix)){
		model = mxModel(name, MZ, DZ, top,
			mxFitFunctionMultigroup(c("MZ", "DZ"))
		)
	} else {
		# Weighted model with vector objective
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
	return(model)
}


