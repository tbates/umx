# umxACE that was in umx for a while
#' umxACE
#'
#' Make a 2-group ACE model
#'
#' @param name The name of the model (defaults to "ACE")
#' @param selDVs The variables to include from the data
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data
#' @param weightVar = If provided, a vector objective used to weight the data. (default = NULL) 
#' @param bVector row-wise likelihoods (defaults to FALSE)
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx twin modeling
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' require(umx)
#' data(twinData)
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
#' selDVs = c("bmi1","bmi2")
#' mzData <- as.matrix(subset(twinData, ZYG == "MZFF", selDVs))
#' dzData <- as.matrix(subset(twinData, ZYG == "DZFF", selDVs))
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1)
#' umxSummaryACE(m1)
umxACE <- function(name = "ACE", selDVs, dzData, mzData, numObsDZ = NULL, numObsMZ = NULL, weightVar = NULL, bVector = FALSE) {
	nSib        = 2
	nVar        = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	equateMeans = T
	dzAr        = .5
	dzCr        = 1
	addStd      = T
	boundDiag   = NULL
	dataType    = umx_is_cov(dzData)
	
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObs with ", dataType, " data...")
		}
		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die= FALSE) | !umx_check_names(weightVar, data = dzData, die= FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 " frames passed into umxACE when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\nbut I was looking for ", weightVar, " as the moderator name"
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
		# Add means matrix to top
		obsMZmeans = colMeans(mzData[, selDVs], na.rm = TRUE);
		top = mxModel(name = "top", 
			# means (not yet equated across twins)
			umxLabel(mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * 2), free = TRUE, values = obsMZmeans, dimnames = list("means", selDVs)) )
		)
		modelMZ = mxModel(name = "MZ", 
			# TODO swap these back for 2.0...
			mxFIMLObjective("top.mzCov", "top.expMean", vector = bVector),
			# mxExpectationNormal("top.mzCov", "top.expMean"), mxFitFunctionFIML(vector = bVector),
			mxData(mzData, type = "raw")
		)
		modelDZ = mxModel(name = "DZ", 
			# TODO swap these back for 2.0...
			mxFIMLObjective("top.dzCov", "top.expMean", vector = bVector),
			# mxExpectationNormal("top.dzCov", "top.expMean"), mxFitFunctionFIML(vector = bVector),
			mxData(dzData, type = "raw")
		)
	} else if(dataType %in% c("cov", "cor")){
		if(!is.null(weightVar)){
			stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices or pass in raw data")
		}
		if( is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if( is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)

		top = mxModel(name = "top")

		modelMZ = mxModel(name = "MZ", 
			mxMLObjective("top.mzCov"),
			# TODO swap these back for 2.0...
			# mxExpectationNormal("top.mzCov"), mxFitFunctionML(),			
			mxData(mzData, type = "cov", numObs = numObsMZ)
		)
		modelDZ = mxModel(name = "DZ", 
			mxMLObjective("top.dzCov"),
			# TODO swap these back for 2.0...
			# mxExpectationNormal("top.dzCov"), mxFitFunctionML(),
			mxData(dzData, type = "cov", numObs = numObsDZ)
		)
		message("treating data as ", dataType)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}
	# Finish building top
	top = mxModel(top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model added if raw  - see above
		umxLabel(mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = .6, byrow = TRUE), jiggle = 0.05),  # Additive genetic path 
		umxLabel(mxMatrix(name = "c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = .3, byrow = TRUE), jiggle = 0.05),  # Common environmental path 
		umxLabel(mxMatrix(name = "e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = .6, byrow = TRUE), jiggle = 0.05),  # Unique environmental path
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(a %*% t(a), name = "A"), # additive genetic variance
		mxAlgebra(c %*% t(c), name = "C"), # common environmental variance
		mxAlgebra(e %*% t(e), name = "E"), # unique environmental variance
		mxAlgebra(A+C+E     , name = "ACE"),
		mxAlgebra(A+C       , name = "AC" ),
		mxAlgebra( (dzAr %x% A) + (dzCr %x% C),name = "hAC"),
		mxAlgebra(rbind (cbind(ACE, AC), 
		                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "mzCov"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "dzCov")
	)

	if(!bVector){
		model = mxModel(name, modelMZ, modelDZ, top,
			mxAlgebra(MZ.objective + DZ.objective, name = "twin"),
			# TODO fix this for OpenMx 2
			mxAlgebraObjective("twin")
			# mxFitFunctionAlgebra("twin")
		)
	} else {
		# bVector is TRUE!
		# To weight objective functions in OpenMx, you specify a container model that applies the weights
		# m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
		# This option makes FIML return individual likelihoods for each row of the data (rather than a single -2LL value for the model)
		# You then optimize weighted versions of these likelihoods
		# by building additional models containing weight data and an algebra that multiplies the likelihoods from the first model by the weight vector
		model = mxModel(name, modelMZ, modelDZ, top,
			mxModel("MZw", mzWeightMatrix,
				mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
				# TODO fix this for OpenMx 2
				mxAlgebraObjective("mzWeightedCov")
				# mxFitFunctionAlgebra("mzWeightedCov")
			),
			mxModel("DZw", dzWeightMatrix,
				mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
				# TODO fix this for OpenMx 2
				mxAlgebraObjective("dzWeightedCov")
			    # mxFitFunctionAlgebra("dzWeightedCov")
			),
			mxAlgebra(MZw.objective + DZw.objective, name = "twin"),
			# TODO fix this for OpenMx 2
			mxAlgebraObjective("twin")
		    # mxFitFunctionAlgebra("twin")
		)
	}

	diag(model@submodels$top@matrices$a@lbound) = 0
	diag(model@submodels$top@matrices$c@lbound) = 0
	diag(model@submodels$top@matrices$e@lbound) = 0
	newTop = mxModel(model@submodels$top, 
		mxMatrix("Iden", nVar, nVar, name = "I"), # nVar Identity matrix
		mxAlgebra(A+C+E, name = "Vtot"),          # Total variance
		mxAlgebra(solve(sqrt(I * Vtot)), name = "SD"), # Total variance
		mxAlgebra(SD %*% a, name = "a_std"), # standardized a
		mxAlgebra(SD %*% c, name = "c_std"), # standardized c
		mxAlgebra(SD %*% e, name = "e_std")  # standardized e
	)
	model  = mxModel(model, newTop, mxCI(c('top.a_std','top.c_std','top.e_std')))
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(dataType == "raw"){
		model = omxSetParameters(model,
			labels  = paste0("expMean_r1c", (nVar+1):(nVar*2)), # c("expMean14", "expMean15", "expMean16"),
			newlabels = paste0("expMean_r1c", 1:nVar)           # c("expMean11", "expMean12", "expMean13")
		)
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	return(model)
}
