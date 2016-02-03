# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::release("~/bin/umx", check = TRUE)
# https://www.researchgate.net/publication/287346605_A_New_Approach_to_Handle_Missing_Covariate_Data_in_Twin_Research

#' umxACEcov
#'
#' Make a 2-group ACE Cholesky Twin model with covariates modeled (see Details below)
#' 
#' A common task in twin modelling involves using the genetic and environmental differences 
#' between large numbers of pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of one, or, typically, several phenotypes
#' (measured behaviors).
#' 
#' umxACEcov supports a core model in behavior genetics, known as the ACE Cholesky model
#' (Cardon and Neale, 1996), supplemented with covariates.
#' The ACE model decomposes phenotypic variance into Additive genetic, 
#' unique environmental (E) and, either common xor shared-environment (C) or 
#' non-additive genetic effects (D). The following figure shows how the ACE model appears as a path diagram:
#' 
#' \figure{ACE.png}
#' 
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modelling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelyhoods summed to form
#' the model-likelihood, which is to be minimised.
#' This feature is used in the non-linear GxE model functions.
#' 
#' \strong{Additional features}
#' The umxACE function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modelling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' @param name The name of the model (defaults to"ACE")
#' @param selDVs The variables to include from the data (do not include suffixes)
#' @param selCovs The covariates to include from the data (do not include suffixes)
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param suffix The suffix for twin 1 and twin 2, often "_T" (defaults to NULL) With this, you can
#' omit suffixes from names in SelDV, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE)
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE)
#' @param hint An analysis hint. Options include "none", (default) "left_censored". Default does nothing.
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE)
#' @param autoRun Whether to run the model and return it, or just return it
#' @return - \code{\link{mxModel}} of subclass mxModel.ACEcov
#' @export
#' @family Twin Modeling Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' # Height, weight, and BMI data from Australian twins. 
#' # The total sample has been subdivided into a young cohort, aged 18-30 years,
#' # and an older cohort aged 31 and above.
#' # Cohort 1 Zygosity is coded as follows: 
#' # 1 == MZ females 2 == MZ males 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
#' # tip: ?twinData to learn more about this data set
#' require(umx)
#' data(twinData)
#' tmpTwin <- twinData
#' # add age 1 and age 2 columns
#' tmpTwin$age1 = tmpTwin$age2 = tmpTwin$age
#' # Pick the variables. We will use base names (i.e., "bmi") and set suffix.
# str(twinData)
#' selDVs  = c("bmi")
#' selCovs = c("age")
#' selVars = umx_paste_names(c(selDVs, selCovs), textConstant = "", suffixes= 1:2)
#' # just top 200 so example runs in a couple of secs
#' mzData = subset(tmpTwin, zyg == 1, selVars)[1:200, ]
#' dzData = subset(tmpTwin, zyg == 3, selVars)[1:200, ]
#' # TODO update for new dataset variable zygosity
#' # mzData = subset(tmpTwin, zygosity == "MZFF", selVars)[1:200, ]
#' # dzData = subset(tmpTwin, zygosity == "DZFF", selVars)[1:200, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, 
#' 	 suffix = "", autoRun = F)
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, 
#' 	 suffix = "", autoRun = T)
#' umxSummary(m1)
#' umxSummaryACE(m1)
#' \dontrun{
#' plot(m1)
#' }
umxACEcov <- function(name = "ACEcov", selDVs, selCovs, dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = NULL, equateMeans = TRUE, bVector = FALSE, hint = c("none", "left_censored"), autoRun = getOption("umx_auto_run")) {
	if(nrow(dzData)==0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData)==0){ stop("Your DZ dataset has no rows!") }
	nSib = 2 # number of siblings in a twin pair
	if(dzCr == .25 && name == "ACE"){
		name = "ADEcov"
	}
	# look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(is.null(suffix)){
		stop("I need a suffix, like '_T'. I will add 1 and 2 afterwards... \n",
		"i.e., selDVs should be 'bmi' etc, and I will re-name to 'bmi_T1' and 'bmi_T2' etc.")
	}else if(length(suffix) > 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
	}else{
		# stash base names for use later
		baseDVs = selDVs
		baseCovs = selCovs
		# fill out full trait names
		selDVs  = umx_paste_names(selDVs, suffix, 1:2)
		selCovs = umx_paste_names(selCovs, suffix, 1:2)
	}

	selVars = c(selDVs, selCovs)
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))

	nDV  = length(selDVs) /nSib
	nCov = length(selCovs)/nSib
	nVar = length(selVars)/nSib # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	# Drop unused columns from mz and dzData
	mzData = mzData[, selVars]
	dzData = dzData[, selVars]

	meanDimNames = list(NULL, selVars)
	# meanDimNames= list("means", selVars)
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans){
		meanLabels = c(rep(paste0("expMean_", baseDVs), nSib), paste0("expMean_", selCovs))
	}else{
		stop("Currently, means must be equated")
		# meanLabels = c(paste0("expMean_", selDVs), paste0("expMean_", selCovs))
	}
	
	obsMZmeans  = umx_means(mzData[, selVars], ordVar = 0, na.rm = TRUE)
	meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib),
			free = TRUE, values = obsMZmeans, labels = meanLabels, dimnames = meanDimNames)
	# TODO this should be variance not correlation...
	varStarts = diag(umxHetCor(mzData[,selDVs]))
	if(nDV == 1){
		varStarts = varStarts/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
	}
	# TODO fix varStarts to include covs also
	varStarts = matrix(varStarts, nDV, nDV)

	top = mxModel("top",
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of.
		# NB: top already has the means model and thresholds matrix added if necessary  - see above.
		# Additive, Common, and Unique environmental paths.
		# Matrices a, c, e to store a, c, e path coefficients.
		umxLabel(mxMatrix(name = "a", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE)),
		umxLabel(mxMatrix(name = "c", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE)),
		umxLabel(mxMatrix(name = "e", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE)),  

		mxMatrix(name = "dzAr", type = "Full", nrow = 1, ncol = 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", nrow = 1, ncol = 1, free = FALSE, values = dzCr),

		meansMatrix,
		mxMatrix(name = "lowerB", 'Lower', nrow = nCov, ncol = nCov, values = 0.5, free = TRUE),
		mxMatrix(name = "lowerW", 'Lower', nrow = nCov, ncol = nCov, values = 0.5, free = TRUE),
		mxAlgebra(name= "CovB"  , lowerB %*% t(lowerB)),
		mxAlgebra(name= "CovW"  , lowerW %*% t(lowerW)),
		mxAlgebra(name= "CovWB" , CovW + CovB),
		
		# Matrices A, C,E + compute variance components
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),
		# Declare a vector for the regression parameters
		mxMatrix(name = "beta", type = "Full", nrow = nCov, ncol  = 1, free = TRUE, values = 0, labels = paste0("beta", 1:nCov)),
		# Some handy component algebras
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "hAC", dzAr * AC),
		mxAlgebra(name = "tb_CovWB_b", (t(beta) %*% CovWB) %*% beta),
		mxAlgebra(name = "tb_CovB_b" , (t(beta) %*% CovB) %*% beta),
		mxAlgebra(name = "tb_CovWB"  , t(beta) %*% CovWB),
		mxAlgebra(name = "tb_CovB"   , t(beta) %*% CovB),
		mxAlgebra(name = "CovWB_b"   ,   CovWB %*% beta),
		mxAlgebra(name = "CovB_b"    ,   CovB  %*% beta),
		# Algebra for expected variance/covariance matrix #in MZ twins
		mxAlgebra(name = "expCovMZ", expression = rbind(
			cbind(ACE + tb_CovWB_b, AC  + tb_CovB_b , tb_CovWB, tb_CovB),
			cbind(AC  + tb_CovB_b , ACE + tb_CovWB_b, tb_CovB , tb_CovWB),
			cbind(CovWB_b         , CovB_b          , CovWB   , CovB),
			cbind(CovB_b          , CovWB_b         , CovB    , CovWB))
		),
		# Algebra for expected variance/covariance matrix #in DZ twins
		mxAlgebra(name = "expCovDZ", expression = rbind(
			cbind(ACE + tb_CovWB_b, hAC + tb_CovB_b , tb_CovWB, tb_CovB),
			cbind(hAC + tb_CovB_b , ACE + tb_CovWB_b, tb_CovB , tb_CovWB),
			cbind(CovWB_b         , CovB_b          , CovWB   , CovB),
			cbind(CovB_b          , CovWB_b         , CovB    , CovWB))
		)
	) # end top

	MZ  = mxModel("MZ", mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
	DZ  = mxModel("DZ", mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

	MZ = mxModel("MZ",
		mxData(mzData , type = "raw"),
		mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames = names(mzData)),
		mxFitFunctionML()
	)

	DZ = mxModel("DZ",
		mxData(dzData, type = "raw"),
		mxExpectationNormal("top.expCovDZ", means = "top.expMean", dimnames = names(dzData)),
		mxFitFunctionML()
	)
	
	model = mxModel(name, MZ, DZ, top,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	
	if(addStd){
		newTop = mxModel(model@submodels$top,
			mxMatrix(name  = "Iden", "Iden", nDV, nDV), # nDV Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
			mxAlgebra(name = "SD", solve(sqrt(Iden * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModel.ACEcov") # set class so that S3 plot() dispatches.
	if(autoRun){
		return(mxRun(model))
	} else {
		return(model)
	}
}