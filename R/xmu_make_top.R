#' Helper to make a basic top, MZ, and DZ model.
#'
#' @description
#' `xmu_make_top_twin_models` makes basic `top`, `MZ`, and `DZ` models. It includes a thresholds matrix if needed.
#'
#' This is used in umxCP, and will be added to umxACE and umxIP, simplifying code maintainance.
#' 
#' This function takes the `mzData` and `dzData`, a list of the `selDVs` (as well as sep and nSib) to analyse, along with other 
#' relevant information such as whether the user wants to equateMeans, and what threshType to use (currently "deviationBased").
#' It can also handle a weightVar.
#' 
#' `varStarts` is computed as `sqrt(variance)/3` of the DVs and `obsMeans` as the variable means.
#' For raw data, a check is made for ordered variables.
#' 
#' For Binary variables, means are fixed at 0 and total variance (A+C+E) is fixed at 1.
#' 
#' For ordinal variables, the first 2 thresholds are fixed.
#' 
#' ## Modeling
#' 
#' ### top
#' For raw data, it contains a means matrix. For WLS and summary data, the top model contains only a name.

#' For ordinal, `top` gains `top.threshMat` (from a call to `umxThresholdMatrix`). `MZ` and `DZ` are as with continuous, but adding thresholds.
#' 
#' ### MZ and DZ
#' MZ and DZ contain data, and an expectation and fit function. For WLS this is mxExpectationNormal  and mxFitFunctionWLS.
#' 
#' ## Matrices created
#' 
#' If needed means matrices are added. Decent starts are guessed from the data.
#' For continuous raw data, top contains a means matrix "expMean". 
#' For Models with ordinal but no binary variables, top adds an `umxThresholdMatrix`. 
#' If binary variables are present, matrices and a constraint to hold A+C+E ==1 are added to top.
#' For continuous raw data, MZ and DZ contain mxExpectationNormal and mxFitFunctionML.
#'
#'
#' `MZ` and `DZ` is the data, and an expectation of for `top.expCovMZ` and `top.expMean`, possibly referencing vector = bVector in the fit function.
#'
#' For binary, `Vtot` (A+C+E) is constrained to 1.
#' 
#' If a weightVar is detected, this column is added to  mzWeightMatrix/mzWeightMatrix
#' 
#' If `equateMeans` is `TRUE`, then the Twin-2 vars in the mean matrix are equated by label with Twin-1.
#'
#' ## Data handling
#' 
#' In terms of data handling, it was primarily designed to take data.frames and process these into mxData. It can, however, handle cov and mxData input.
#' 
#' It can process data into all the types supported by `mxData`.
#' 
#' Raw data input with a target of `cov` or `cor` type requires the `numObsMZ` and `numObsDZ` to be set.
#' 
#' If ordinal or binary variables are found in raw data, an `mxThreshold` matrix is added to handle these.
#' 
#' If a weight variable is, an `mzWeightMatrix` will be added.
#' 
#' Type "WLS", "DWLS", or "ULS" will process raw data into a WLS data using `xmu_make_mxData`.
#' 
#' Unused columns are dropped.
#' If you pass in raw data, you can't request type cov/cor yet. Will work on this if desired.
#' 
#' ## TODO list for xmu_make_top_twin_models
#' 
#' 1. Add selCovs
#' 2. Add covMethod == "fixed"
#' 3. Add beta matrix for fixed covariates in means.
#' 4. improve the start guesses based on input model type (ACE, CP, IP etc.)
#'
#' @param mzData Dataframe containing the MZ data 
#' @param dzData Dataframe containing the DZ data 
#' @param selDVs List of base (e.g. BMI) (i.e., NOT 'BMI_T1') variable names
#' @param sep Used to expand selDVs into selDVs, i.e., "_T" to expand BMI into BMI_T1 and BMI_T2
#' @param nSib Number of members per family (default = 2)
#' @param numObsMZ Number of MZ observations contributing (for summary data only) 
#' @param numObsDZ Number of DZ observations contributing (for summary data only)
#' @param equateMeans Whether to equate T1 and T2 means (default = TRUE).
#' @param type	One of 'Auto','FIML','cov', 'cor', 'WLS','DWLS', or 'ULS'. Auto reacts to the incoming mxData type (raw/cov, WLS).
#'  FIML requires that the data are continuous. Remaining options are weighted, diagonally weighted, or unweighted least squares, respectively)
#' @param threshType what type of thresholds to implement if needed.
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param verbose (default = FALSE)
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
#' bits = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs= selDVs, sep="", nSib = 2)
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
#' bits = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs= selDVs, sep="", nSib = 2)
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
#' bits = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs = selDVs, sep = "", nSib = 2)
#'
#' # ============
#' # = Cov data =
#' # ============
#' data(twinData)
#' selDVs = c("wt")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", tvars(selDVs, sep="")], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", tvars(selDVs, sep="")], use = "complete")
#' bits = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs= selDVs, sep= "", nSib = 2)
xmu_make_top_twin_models <- function(mzData, dzData, selDVs, sep = NULL, nSib = 2, numObsMZ= NULL, numObsDZ= NULL, equateMeans = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), threshType = c("deviationBased"), weightVar = NULL, bVector = FALSE, verbose= FALSE) {
	# TODO: xmu_make_top_twin_models Add selCovs
	# TODO: xmu_make_top_twin_models add covMethod == "fixed"
	# TODO: xmu_make_top_twin_models add beta matrix for fixed covariates in means.
	type = match.arg(type)
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
	
	if(type %in% c("cov", "cor") && !dataType %in% c("cov", "cor")){
		stop("You've requested type= cov or cor, but the provided dataType is ", omxQuotes(dataTypeYou), " I don't support that yet. Please pass in cov data.")
	}

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Find ordinal variables
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
			# no weights
			bVector = FALSE
		}
		# Make mxData, dropping any unused columns
		allData = rbind(mzData, dzData)
		mzData  = xmu_make_mxData(mzData, type = type, manifests = selVars)
		dzData  = xmu_make_mxData(dzData, type = type, manifests = selVars)

		# =====================================
		# = Add means and var matrices to top =
		# =====================================
		# Figure out start values while we are here
		# varStarts will be used to fill a, c, and e

		# TODO could use both twins for variance estimation.
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
		# [] select mxFitFunctionML() of bVector as param
		
		if(type %in%  c('WLS', 'DWLS', 'ULS')) {
			message("data treated as ", type)
			top = mxModel("top") # no means for WLS
			MZ  = mxModel("MZ", mzData,
				mxExpectationNormal("top.expCovMZ", "top.expMean"),
				mxFitFunctionWLS()
			)
			DZ  = mxModel("DZ", dzData,
				mxExpectationNormal("top.expCovDZ", "top.expMean"), 
				mxFitFunctionWLS()
			)			
		} else if(nFactors == 0) {
			# ===============================
			# = Handle all continuous case  =
			# ===============================
			top = mxModel("top", 
				umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selVars))
			)
			MZ  = mxModel("MZ", mzData,
				mxExpectationNormal("top.expCovMZ", "top.expMean"), 
				mxFitFunctionML(vector = bVector)
			)
			DZ  = mxModel("DZ", dzData,
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
			MZ  = mxModel("MZ", mzData,
				mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat"), 
				mxFitFunctionML(vector = bVector)
			)
			DZ  = mxModel("DZ", dzData,
				mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat"),
				mxFitFunctionML(vector = bVector)
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
			MZ  = mxModel("MZ", mzData,
				mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat"),
				mxFitFunctionML()
				# mxFitFunctionML(vector = bVector),
			)
			DZ  = mxModel("DZ", dzData,
				mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat"),
				mxFitFunctionML()
				# mxFitFunctionML(vector = bVector),
			)
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
		varStarts = diag(het_mz)[1:nVar]

		if(nVar == 1){
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		top = mxModel("top")
		MZ  = mxModel("MZ", 
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

#' umxIP: Build and run an Independent pathway twin model
#'
#' Make a 2-group Independent Pathway twin model (Common-factor independent-pathway multivariate model)
#' The following figure shows the IP model diagrammatically:
#' \figure{IP.png}
#'
#' @param name The name of the model (defaults to "IP").
#' @param selDVs The variables to include.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The suffix for twin 1 and twin 2, often "_T". If set, you can
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param nFac How many common factors for a, c, and e. If 1 number number is given, applies to all three.
#' @param freeLowerA Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE Whether to leave the lower triangle of E free (default = FALSE).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param correlatedA Whether factors are allowed to correlate (not implemented yet: FALSE).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = TODO: implement ordinal Number of DZ twins: Set this if you input covariance data,
#' @param numObsMZ = TODO: implement ordinal Number of MZ twins: Set this if you input covariance data.
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param suffix Deprecated: use "sep".
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{https://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData <- subset(GFF, zyg_2grp == "MZ")
#' dzData <- subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxIPnew(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' m1 = umxIPnew(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, 
#' 	nFac = c(a=3, c = 1, e = 1)
#' )
#' umxSummary(m1)
#' plot(m1)
#' }
umxIPnew <- function(name = "IP", selDVs, dzData, mzData, sep = NULL, nFac = c(a=1, c=1, e=1), freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, equateMeans = TRUE, dzAr = .5, dzCr = 1, correlatedA = FALSE, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), optimizer = NULL, suffix = "deprecated") {
	# TODO implement correlatedA
	if(suffix != "deprecated"){
		message("Just a message: but please use 'sep' instead of suffix - suffix is deprecated, and will stop working in 2019")
		sep = suffix
	}
	nSib = 2
	xmu_twin_check(selDVs = selDVs, dzData = dzData, mzData = mzData, enforceSep = TRUE, sep = sep, nSib = nSib, optimizer = optimizer)
	# Expand var names
	selVars = umx_paste_names(selDVs, sep = sep, suffixes = 1:nSib)
	selDVs  = umx_paste_names(selDVs, sep, 1:2)
	nVar    = length(selDVs)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	bits    = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs= selDVs, sep = sep, nSib = nSib, equateMeans= equateMeans, verbose= FALSE)
	top     = bits$top
	MZ      = bits$MZ
	DZ      = bits$DZ

	if(length(nFac) == 1){
		nFac = c(a = nFac, c = nFac, e = nFac)
	} else if (length(nFac) != 3){
		stop("nFac must be either 1 number or 3. You gave me ", length(nFac))
	}
	if(name == "IP"){
		# Add nFac to base name if no user-set name provided.
		if (length(nFac) == 1){
			name = paste0(name, nFac, "fac")
		}else{
			name = paste0(name, paste0(c("a", "c", "e"), nFac, collapse = ""))
		}
	}

	if(correlatedA){
		message("I have not implemented correlatedA yet...")
	}
	
	dataType = umx_is_cov(dzData)

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from mz and dzData
		mzData = mzData[, selDVs, drop = FALSE]
		dzData = dzData[, selDVs, drop = FALSE]
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)
		het_dz = umx_reorder(dzData, selDVs)
		stop("COV not fully implemented yet for IP... Not sure if there's any demand, so email me if you see this")
	} else {
		stop("Datatype ", omxQuotes(dataType), " not understood")
	}

	nVar = length(selDVs)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	obsMZmeans = colMeans(mzData, na.rm = TRUE);
	model = mxModel(name,
		mxModel("top",
			umxLabel(mxMatrix("Full", 1, nVar*nSib, free=T, values=obsMZmeans, dimnames=list("means", selDVs), name="expMean")), # Means 
			# (not yet equated for the two twins)
			# Matrices ac, cc, and ec to store a, c, and e path coefficients for independent general factors
			umxMatrix("ai", "Full", nVar, nFac['a'], free=TRUE, values=.6, jiggle=.05), # latent common factor Additive genetic path 
			umxMatrix("ci", "Full", nVar, nFac['c'], free=TRUE, values=.0, jiggle=.05), # latent common factor Common #environmental path coefficient
			umxMatrix("ei", "Full", nVar, nFac['e'], free=TRUE, values=.6, jiggle=.05), # latent common factor Unique environmental path #coefficient
			# Matrices as, cs, and es to store a, c, and e path coefficients for specific factors
			umxMatrix("as", "Lower", nVar, nVar, free=TRUE, values=.6, jiggle=.05), # Additive genetic path 
			umxMatrix("cs", "Lower", nVar, nVar, free=TRUE, values=.0, jiggle=.05), # Common environmental path 
			umxMatrix("es", "Lower", nVar, nVar, free=TRUE, values=.6, jiggle=.05), # Unique environmental path.

			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

			# Multiply by each path coefficient by its inverse to get variance component
			# Sum the squared independent and specific paths to get total variance in each component
			mxAlgebra(name = "A", ai%*%t(ai) + as%*%t(as) ), # Additive genetic variance
			mxAlgebra(name = "C", ci%*%t(ci) + cs%*%t(cs) ), # Common environmental variance
			mxAlgebra(name = "E", ei%*%t(ei) + es%*%t(es) ), # Unique environmental variance

			mxAlgebra(name = "ACE", A+C+E),
			mxAlgebra(name = "AC" , A+C  ),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "expCovDZ"),

			# Algebra to compute total variances and standard deviations (diagonal only)
			mxMatrix("Iden", nrow = nVar, name = "I"),
			mxAlgebra(solve(sqrt(I * ACE)), name = "iSD")
		),
		mxModel("MZ", mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxModel("DZ", mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Equate means for twin1 and twin 2
	if(equateMeans){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar+1):(nVar*2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
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
	} else {
		# set the first column off, bar r1
		model = omxSetParameters(model, labels = "es_r[^1]0-9?c1", free = FALSE, values = 0)

		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)

		# Used to drop the ei paths, as we have a full Cholesky for E, now just set the bottom row TRUE
		# toset = umxGetParameters(model, "^ei_r.c.", free= TRUE)
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix("Iden", nrow = nVar, name = "I"),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(solve(sqrt(I * ACE)), name = "SD"),
			# Standard general path coefficients
			mxAlgebra(SD %*% ai, name = "ai_std"), # standardized ai
			mxAlgebra(SD %*% ci, name = "ci_std"), # standardized ci
			mxAlgebra(SD %*% ei, name = "ei_std"), # standardized ei
			# Standardize specific path coefficients
			mxAlgebra(SD %*% as, name = "as_std"), # standardized as
			mxAlgebra(SD %*% cs, name = "cs_std"), # standardized cs
			mxAlgebra(SD %*% es, name = "es_std")  # standardized es
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.ai_std','top.ci_std','top.ei_std', 'top.as_std','top.cs_std','top.es_std')))
		}
	}
	model  = omxAssignFirstParameters(model) # ensure parameters with the same label have the same start value... means, for instance.
	model = as(model, "MxModelIP")

	if(autoRun){
		tryCatch({
			model = mxRun(model)
			umxSummary(model)
		}, warning = function(w) {
			message("Warning incurred trying to run model")
			message(w)
		}, error = function(e) {
			message("Error incurred trying to run model")
			message(e)
		})
	}

	return(model)
} # end umxIP

