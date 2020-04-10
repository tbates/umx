#' Helper to make a basic top, MZ, and DZ model.
#'
#' @description
#' `xmu_make_TwinSuperModel` makes basic `top`, `MZ`, and `DZ` models. It includes thresholds matrices in the twin models if needed.
#' 
#' It's the replacement for `xmu_assemble_twin_supermodel` approach.
#'
#' This is used in  [umxCP()], and [umxACE()] and [umxACEv()] and will be added to the other models: [umxGxE()], [umxIP()], 
#' simplifying code maintenance.
#' 
#' `xmu_make_TwinSuperModel` takes `mzData` and `dzData`, a list of the `selDVs` to analyse (as well as `sep` and `nSib`), along with other 
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
#' If a `weightVar` is detected, these columns are used to create a row-weighted MZ and DZ models.
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
#' In terms of data handling, `xmu_make_TwinSuperModel` was primarily designed to take 
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
#' @param name for the supermodel
#' @param mzData Dataframe containing the MZ data 
#' @param dzData Dataframe containing the DZ data 
#' @param selDVs List of manifest base names (e.g. BMI, NOT 'BMI_T1') (OR, you don't set "sep", the full variable names)
#' @param selCovs List of covariate base names (e.g. age, NOT 'age_T1') (OR, you don't set "sep", the full variable names)
#' @param sep string used to expand selDVs into selVars, i.e., "_T" to expand BMI into BMI_T1 and BMI_T2 (optional but STRONGLY encouraged) 
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
#
#' # ==============
#' # = Continuous =
#' # ==============
#' library(umx)
#' data(twinData)
#' twinData = umx_scale(twinData, varsToScale= c('ht1','ht2'))
#' mzData = twinData[twinData$zygosity %in%  "MZFF",] 
#' dzData = twinData[twinData$zygosity %in%  "DZFF",]
#' m1= xmu_make_TwinSuperModel(mzData=mzData, dzData=dzData, selDVs=c("wt","ht"), sep="", nSib=2)
#' names(m1) # "top" "MZ"  "DZ"
#' class(m1$MZ$fitfunction)[[1]] == "MxFitFunctionML"
#'
#' # ===============
#' # = WLS example =
#' # ===============
#' m1=xmu_make_TwinSuperModel(mzData=mzData, dzData=dzData,selDVs=c("wt","ht"),sep="",type="WLS")
#' class(m1$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
#' m1$MZ$fitfunction$type =="WLS"
# # Check default all-continuous method
#' m1$MZ$fitfunction$continuousType == "cumulants"
#' 
#' # Choose non-default type (DWLS)
#' m1= xmu_make_TwinSuperModel(mzData= mzData, dzData= dzData,
#'		selDVs= c("wt","ht"), sep="", type="DWLS")
#' m1$MZ$fitfunction$type =="DWLS"
#' class(m1$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
#' 
#' # Switch WLS method
#' m1 = xmu_make_TwinSuperModel(mzData= mzData, dzData= dzData, selDVs= c("wt","ht"), sep= "",
#'   type = "WLS", allContinuousMethod = "marginals")
#' m1$MZ$fitfunction$continuousType == "marginals"
#' class(m1$MZ$fitfunction)[[1]] == "MxFitFunctionWLS"
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
#' m1 = xmu_make_TwinSuperModel(mzData= mzData, dzData= dzData, selDVs= selDVs, sep="", nSib= 2)
#' names(m1) # "top" "MZ"  "DZ" 
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
#' m1 = xmu_make_TwinSuperModel(mzData= mzData, dzData= dzData, selDVs= selDVs, sep= "", nSib= 2)
#'
#' # ========================================
#' # = Cov data (calls xmuTwinSuper_CovCor) =
#' # ========================================
#'
#' data(twinData)
#' mzData =cov(twinData[twinData$zygosity %in% "MZFF", tvars(c("wt","ht"), sep="")], use="complete")
#' dzData =cov(twinData[twinData$zygosity %in% "DZFF", tvars(c("wt","ht"), sep="")], use="complete")
#' m1 = xmu_make_TwinSuperModel(mzData= mzData, dzData= dzData, selDVs= "wt", sep= "", 
#' 	nSib= 2, numObsMZ = 100, numObsDZ = 100, verbose=TRUE)
#' class(m1$MZ$fitfunction)[[1]] =="MxFitFunctionML"
#' names(m1$MZ$data$observed) == c("wt1", "wt2") # height columns dropped
#'
xmu_make_TwinSuperModel <- function(name="twin_super", mzData, dzData, selDVs, selCovs= NULL, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), numObsMZ = NULL, numObsDZ = NULL, nSib = 2, equateMeans = TRUE, weightVar = NULL, bVector = FALSE, verbose= FALSE) {
	# TODO for xmu_make_TwinSuperModel
	# TODO: 1. xmu_make_TwinSuperModel Add selCovs
	# TODO: 2. xmu_make_TwinSuperModel Add beta matrix for fixed covariates in means.
	# TODO: 4. xmu_make_TwinSuperModel more tests in a test page

	# *Note*: If dropping this into an existing model, it replaces all code setting: nVar, selVars, used, 
	# Also any code figuring out data-type
	
	if(!is.null(selCovs)){
		stop("covariates not implemented in xmu_make_TwinSuperModel yet: use umx_residualize() in the mean time")
	}

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
		selCovs = selCovs
		message("Polite note: It's better to use 'sep'. This might become compulsory as it helps umx manage variable names in twin models.")
		# stop("You MUST set 'sep'. Otherwise xmu_make_top can't reliably expand selDVs into full variable names")
	}else{
		selVars = tvars(selDVs , sep = sep, suffixes= 1:nSib)
		selCovs = tvars(selCovs, sep = sep, suffixes= 1:nSib)
	}

	dataType = umx_is_cov(dzData, boolean = FALSE); if(verbose){ umx_msg(dataType)}

	if(type %in% c("cov", "cor") && !dataType %in% c("cov", "cor")){
		stop("You've requested type= cov or cor, but the provided dataType is ", omxQuotes(dataType), " I don't support that yet. Please pass in cov data.")
	}

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}

		if(!is.null(weightVar)){
			# Weight variable provided: check it exists in each frame.
			mzWeightData = xmu_extract_column(mzData, weightVar)
			dzWeightData = xmu_extract_column(dzData, weightVar)
			bVector = TRUE
		}

		# ============================================
		# = Make mxData, dropping any unused columns =
		# ============================================
		usedVars = c(selVars, selCovs)
		mzData   = xmu_make_mxData(mzData, type = type, manifests = usedVars, numObs = numObsMZ)
		dzData   = xmu_make_mxData(dzData, type = type, manifests = usedVars, numObs = numObsDZ)

		# ========================================================================
		# = 3. Add mxExpectationNormal, means and var matrices to top, MZ and DZ =
		# ========================================================================

		# Find ordinal variables
		colTypes = umx_is_ordered(xmu_extract_column(mzData, selVars), summaryObject= TRUE)

		if(colTypes$nFactors == 0){
			model = xmuTwinSuper_Continuous(name= name, selVars = selVars, defVars = selCovs, mzData = mzData, dzData = dzData, sep = sep, equateMeans= equateMeans, nSib = nSib, type= type, allContinuousMethod= allContinuousMethod)
		} else if(sum(colTypes$isBin) == 0){
			model = xmuTwinSuper_NoBinary(name= name, selVars = selVars, defVars = selCovs, mzData = mzData, dzData = dzData, sep = sep, equateMeans= equateMeans, nSib = nSib)
		} else if(sum(colTypes$isBin) > 0){
			model = xmuTwinSuper_SomeBinary(name= name, selVars = selVars, defVars = selCovs, mzData = mzData, dzData = dzData, sep = sep, equateMeans= equateMeans,  nSib = nSib, verbose = verbose)
		} else {
			stop("You appear to have something other than I expected in terms of WLS, or binary, ordinal and continuous variable mix")
		}
		# nb: means not yet equated across twins	
	} else if(dataType %in% c("cov", "cor")){
		if(!is.null(weightVar)){
			stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices, or pass in raw data")
		}
		if(!is.null(selCovs)){
			stop("You can't set defVars (covariates) when you have cov data: needs raw data to estimate on each row")
		}
		model = xmuTwinSuper_CovCor(name=name, selVars = selVars, mzData= mzData, dzData= dzData, type = dataType, numObsMZ = numObsMZ, numObsDZ = numObsDZ)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
		message("Data treated as ", type)
		# Still mxExpectationNormal (`top` is not affected - either has or lacks means matrix already).
		# Replace the MZ and DZ model FitFunctions
		model$MZ = mxModel(model$MZ, mxFitFunctionWLS(type = type, allContinuousMethod = allContinuousMethod) )
		model$DZ = mxModel(model$DZ, mxFitFunctionWLS(type = type, allContinuousMethod = allContinuousMethod) )
	}else{
		model$MZ = mxModel(model$MZ, mxFitFunctionML(vector = bVector) )
		model$DZ = mxModel(model$DZ, mxFitFunctionML(vector = bVector) )
	}

	if(!is.null(weightVar)){ model = xmu_twin_add_WeightMatrices(model, mzWeights = mzWeightData, dzWeights = dzWeightData) }

	return(model)
}                                           

# =============================
# = raw twin-assembly helpers =
# =============================

#' xmuTwinSuper_Continuous
#'
#' @description
#' Add the ...
#'
#' @param name The name of the supermodel
#' @param selVars  selVars
#' @param defVars  defVars
#' @param mzData An mxData object containing the MZ data
#' @param dzData An mxData object containing the DZ data 
#' @param type type
#' @param allContinuousMethod  allContinuousMethod
#' @param equateMeans whether to equate the means across twins (default TRUE)
#' @param nSib nSib
#' @param sep  default "_T"
#' @return - A twin model
#' @export
#' @family xmu internal not for end user
#' @seealso - [xmu_make_TwinSuperModel]
#' @md
#' @examples
#' # xmuTwinSuper_Continuous(name="twin_super", selVars = selVars, defVars = defVars, 
#' #    mzData = mzData, dzData = dzData, equateMeans = TRUE, type = type, 
#' #    allContinuousMethod = allContinuousMethod, nSib= nSib, sep = "_T"
#' # )
xmuTwinSuper_Continuous <- function(name=NULL, selVars, defVars = NULL, sep = "_T", mzData, dzData, equateMeans, type, allContinuousMethod, nSib){
	# ===========================================================================
	# = Handle all continuous as special case (for WLS, can affect mean or not) =
	# ===========================================================================
	umx_check(!is.null(name), "stop", "I need a name for the super model")
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	if(type %in% c('WLS', 'DWLS', 'ULS') & allContinuousMethod == "cumulants"){
		# Raw data, mo means (WLS with cumulants
		model = mxModel(name,
			mxModel("top"),
			mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ") ),
			mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ") ),
			mxFitFunctionMultigroup(c("MZ", "DZ"))
		)
	} else {
		# Raw data or (WLS && allContinuousMethod != cumulants): Needs means and MZ and DZ a means model.

		# =============================
		# = Figure out start values  =
		# = NOTE: selVars is expanded by the time we get to here... no sep. =
		# ===================================================================
		starts = xmu_starts(mzData= mzData, dzData= dzData, selVars= selVars, equateMeans= equateMeans, nSib= nSib, varForm= "Cholesky")
		# Contains starts$varStarts; starts$meanStarts; starts$meanLabels # (Equated across twins if requested)
		model = mxModel(name,
			mxModel("top", 
				umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = starts$meanStarts, labels = starts$meanLabels, dimnames = list("means", selVars))
			),
			mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean") ),
			mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean") ),			
			mxFitFunctionMultigroup(c("MZ", "DZ"))
		)
	}
	return(model)
}

# xmuTwinSuper_NoBinary(name=name, selVars = selVars, defVars = defVars, mzData = mzData, dzData = dzData, equateMeans= equateMeans, nSib=2)
xmuTwinSuper_NoBinary <- function(name=NULL, selVars, defVars = NULL, mzData, dzData, sep, nSib, equateMeans= TRUE, verbose=FALSE){
	# ============================
	# = Notes: Ordinal requires: =
	# ============================
	# 1. Variable set to mxFactor
	# 2. For Binary variables:
	#   1. Latent means of binary variables fixedAt 0 (or by data.def?)
	#   2. Latent variance (A + C + E) constrained == 1 
	# 3. For Ordinal variables, first 2 thresholds fixed
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	# ==================================================
	# = Handle 1 or more ordinal variables (no binary) =
	# ==================================================
	# Means ordinal, but no binary
	# Means: all free, start cont at the measured value, ordinals @0
	umx_check(!is.null(name), "stop", "I need a name for the super model")
	colTypes = umx_is_ordered(xmu_extract_column(mzData, selVars), summaryObject= TRUE)

	message("Found ", (colTypes$nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(colTypes$ordVarNames), " (No binary)")
	if(length(colTypes$contVarNames) > 0){
		message(length(colTypes$contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(colTypes$contVarNames[1:(length(colTypes$contVarNames)/nSib)]))
	}
	# =============================
	# = Figure out start values  =
	# = NOTE: selVars is expanded by the time we get to here... no sep. =
	# ===================================================================
	starts = xmu_starts(mzData= mzData, dzData= dzData, selVars= selVars, equateMeans= equateMeans, nSib= nSib, varForm= "Cholesky")
	# Contains starts$varStarts; starts$meanStarts; starts$meanLabels # (Equated across twins if requested)

	model = mxModel(name,
		mxModel("top",
			umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = starts$meanStarts, labels = starts$meanLabels, dimnames = list("means", selVars)),
			umxThresholdMatrix(rbind(mzData$observed, dzData$observed), selDVs = selVars, sep = sep, verbose = verbose)
		),
		mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat") ),
		mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat") ),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	return(model)
}

# xmuTwinSuper_SomeBinary(name=NULL, selVars = selVars, defVars = defVars, mzData = mzData, dzData = dzData, nSib, equateMeans= equateMeans, sep = "_T", verbose = verbose)
xmuTwinSuper_SomeBinary <- function(name=NULL, selVars, defVars = NULL, mzData, dzData, sep, nSib, equateMeans= equateMeans, verbose = verbose){
	colTypes = umx_is_ordered(xmu_extract_column(mzData, selVars), summaryObject= TRUE)
	nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	# =============================================
	# = Handle case of at least 1 binary variable =
	# =============================================
	message("Found ", sum(colTypes$isBin)/nSib, " pairs of binary variables:", omxQuotes(colTypes$binVarNames))
	message("\nI am fixing the latent means and variances of these variables to 0 and 1")
	if(colTypes$nOrdVars > 0){
		message("There were also ", colTypes$nOrdVars/nSib, " pairs of ordinal variables:", omxQuotes(colTypes$ordVarNames))			
	}
	if(length(colTypes$contVarNames) > 0){
		message("\nand ", length(colTypes$contVarNames)/nSib, " pairs of continuous variables:", omxQuotes(colTypes$contVarNames[1:(length(colTypes$contVarNames)/nSib)]))
	}else{
		message("No continuous variables")
	}

	# ===========================================================================
	# = Means: bin fixed, others free, start cont at the measured value, ord @0 =
	# ===========================================================================
	# ===================================
	# = Constrain Ordinal variance @1   =
	# ===================================
	# Algebra to pick out the ordinal variables.
	# TODO check using twin 1 to pick where the bin variables are is robust...
	# Fill with zeros: default for ordinals and binary...
	meansFree = (!colTypes$isBin) # fix the binary variables at zero (umx_means did this)
	the_bin_cols = which(colTypes$isBin)[1:nVar] # columns in which the bin vars appear for T1, i.e., c(1,3,5)
	binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")

	# =============================
	# = Figure out start values  =
	# = NOTE: selVars is expanded by the time we get to here... no sep. =
	# ===================================================================
	starts = xmu_starts(mzData= mzData, dzData= dzData, selVars= selVars, equateMeans= equateMeans, nSib= nSib, varForm= "Cholesky")
	# Contains starts$varStarts; starts$meanStarts; starts$meanLabels # (Equated across twins if requested)

	model = mxModel(name,
		mxModel("top",
			umxMatrix("expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = starts$meanStarts, labels = starts$meanLabels, dimnames = list("means", selVars)),
			umxThresholdMatrix(rbind(mzData$observed, dzData$observed), selDVs = selVars, sep = sep, verbose = verbose),
			# NOTE: Assumes A+C+E is Vtot (i.e., these are the three and only components forming expCov)
			mxAlgebra(name = "Vtot", A + C + E), # Total variance (also added by models with std = TRUE, but is OK to add twice)
			umxMatrix("binLabels"  , "Full", nrow = (colTypes$nBinVars/nSib), ncol = 1, labels = binBracketLabels),
			umxMatrix("Unit_nBinx1", "Unit", nrow = (colTypes$nBinVars/nSib), ncol = 1),
			mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
		),
		mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat") ),
		mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat") ),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	return(model)
}

# xmuTwinSuper_CovCor(name=name, selVars = selVars, mzData= mzData, dzData = dzData, numObsMZ = numObsMZ, numObsDZ = numObsDZ)
xmuTwinSuper_CovCor <- function(name=NULL, selVars, mzData, dzData, type, numObsMZ, numObsDZ){
	# Check the data and get it into shape
	umx_check(!is.null(numObsMZ), "stop", paste0("You must set numObsMZ with summary data"))
	umx_check(!is.null(numObsDZ), "stop", paste0("You must set numObsDZ with summary data"))

	mzData = xmu_make_mxData(mzData, type = type, manifests = selVars, numObs = numObsMZ)
	dzData = xmu_make_mxData(dzData, type = type, manifests = selVars, numObs = numObsDZ)

	model = mxModel(name, 
		mxModel("top"),
		mxModel("MZ", mxExpectationNormal("top.expCovMZ"), mzData),
		mxModel("DZ", mxExpectationNormal("top.expCovDZ"), dzData),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	return(model)
}
