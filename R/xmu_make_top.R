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
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' bits = xmu_make_top(mzData = mzData, dzData = dzData, selDVs= selDVs, nSib = 2)
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

# =============
# = umxCPplay =
# =============
#' umxCP: Build and run a Common pathway twin model
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
#' All the shared matrices are in the model "top". So to see the 'as' values, you can simply execute:
#' 
#' m1$top#as$values
#' 
#' The common-pathway loadings on the factors are in matrices a_cp, c_cp, e_cp.
#'
#' The common factors themselves are in the matrix cp_loadings (an nVar * 1 matrix)
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' m1$top$expMean$labels[1,4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")
#'
#' @param name The name of the model (defaults to "CP").
#' @param selDVs The variables to include.
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep (required) The suffix for twin 1 and twin 2, often "_T". If set, selDVs is just the base variable names.
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
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param suffix DEPRECATED. Use sep instead!
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxSummaryCP}}, \code{\link{umxPlotCP}}. See \code{\link{umxACE}} for more examples of twin modeling. \code{link{plot}} and \code{link{umxSummary}} work for IP, CP, GxE, SAT, and ACE models. For a deep dive, see \code{\link{xmu_make_top}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' # ========================================================
#' # = Run a 3-factor Common pathway twin model of 6 traits =
#' # ========================================================
#' require(umx)
#' # umx_set_optimizer("NPSOL")
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxCP("old", selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' m2 = umxCPplay("new", selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' umxCompare(m1, m2)
#'
#' # =================================================
#' # = Find and test dropping of shared environment  =
#' # =================================================
#' # Show all labels for C parameters  
#' umxParameters(m1, patt = "^c")
#' # Test dropping the 9 specific and common-factor C paths
#' m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC",comp = TRUE)
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' data(GFF)
#' # Cut to form umxFactor  20% depressed  DEP
#' cutPoints = quantile(GFF[, "AD_T1"], probs = .2, na.rm = TRUE)
#' ADLevels  = c('normal', 'depressed')
#' GFF$DEP_T1 = cut(GFF$AD_T1, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' GFF$DEP_T2 = cut(GFF$AD_T2, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' ordDVs = c("DEP_T1", "DEP_T2")
#' GFF[, ordDVs] = umxFactor(GFF[, ordDVs])
#' 
#' selDVs = c("gff","fc","qol","hap","sat","DEP") # These will be expanded into "gff_T1" "gff_T2" etc.
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' allData = rbind(mzData, dzData) 
#' tmp = umxThresholdMatrix(allData[,tvars(selDVs, sep = "_T")], sep = "_T", verbose = TRUE)
#' m1 = umxCPplay(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' m2 = umxModify(m1, regex = "(cs_r[3-5]|c_cp_r[12])", name = "dropC", comp= TRUE)
#' }
#'
umxCPplay <- function(name = "CP", selDVs, dzData, mzData, sep = NULL, nFac = 1, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = FALSE, equateMeans= TRUE, dzAr= .5, dzCr= 1, boundDiag = 0, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), optimizer = NULL, suffix = "deprecated") {
	if(suffix != "deprecated"){
		message("Just a message: but please use 'sep' instead of suffix - suffix is deprecated, and will stop working in 2019")
		sep = suffix
	}
	nSib = 2
	xmu_twin_check(selDVs=selDVs, dzData = dzData, mzData = mzData, enforceSep = TRUE, sep = sep, nSib = nSib, optimizer = optimizer)
	# Expand var names
	selVars = umx_paste_names(selDVs, sep = sep, suffixes = 1:nSib)
	nVar    = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	bits    = xmu_make_top(mzData = mzData, dzData = dzData, selDVs= selDVs, sep = sep, nSib = nSib, equateMeans= equateMeans, verbose= FALSE)
	top     = bits$top
	MZ      = bits$MZ
	DZ      = bits$DZ

	# TODO umxCP: Improve start values (Mike?) 
	if(correlatedA){
		a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = TRUE, values = .7, jiggle = .05) # Latent common factor
	} else {
		a_cp_matrix = umxMatrix("a_cp", "Diag" , nFac, nFac, free = TRUE, values = .7, jiggle = .05)
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
			                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name= "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name= "expCovDZ")
		),
		MZ, DZ,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
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
			# TODO umxCP: break these CIs out into single labels?
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
	
	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
	}
	return(model)
} # end umxCPplay
