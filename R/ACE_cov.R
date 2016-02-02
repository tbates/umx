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
#' umxACE supports a core model in behavior genetics, known as the ACE Cholesky model
#' (Cardon and Neale, 1996). This model decomposes phenotypic variance into Additive genetic, 
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D). This latter restriction emerges due to a lack of degrees of 
#' freedom to simultaneously model C and D with only MZ and DZ twin pairs {ref?}. The Cholesky or 
#' lower-triangle decomposition allows a model which is both sure to be solvable, and also to 
#' account for all the variance (with some restrictions) in the data. This model creates as 
#' many latent A C and E variables as there are phenotypes, and, moving from left to 
#' right, decomposes the variance in each component into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram:
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
#' # Pick the variables
#' selDVs = c("bmi1", "bmi2") # nb: Can also give base name, (i.e., "bmi") AND set suffix.
#' # the function will then make the varnames for each twin using this:
#' # for example. "VarSuffix1" "VarSuffix2"
# str(twinData)
#' selDVs = c("bmi")
#' selCovs = c("age")
#' selVars = umx_paste_names(c(selDVs, selCovs), textConstant = "", suffixes= 1:2)
#' # just top 200 so example runs in a couple of secs
#' mzData = subset(tmpTwin, zyg == 1, selVars)[1:200, ]
#' dzData = subset(tmpTwin, zyg == 3, selVars)[1:200, ]
#' # TODO update for new dataset variable zygosity
#' # mzData = subset(tmpTwin, zygosity == "MZFF", selVars)[1:200, ]
#' # dzData = subset(tmpTwin, zygosity == "DZFF", selVars)[1:200, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, suffix= "", autoRun = FALSE)
#' umxSummary(m1)
#' umxSummaryACE(m1)
#' \dontrun{
#' plot(m1)
#' }
umxACEcov <- function(name = "ACE", selDVs, selCovs, dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = NULL, equateMeans = TRUE, bVector = FALSE, hint = c("none", "left_censored"), autoRun = getOption("umx_auto_run")) {
	if(nrow(dzData)==0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData)==0){ stop("Your DZ dataset has no rows!") }
	nSib = 2 # number of siblings in a twin pair
	if(dzCr == .25 && name == "ACE"){
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
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs, suffix, 1:2)
		selCovs = umx_paste_names(selCovs, suffix, 1:2)
	}
	selVars = c(selDVs, selCovs)
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))

	nDVs    = length(selDVs) /nSib
	nCov    = length(selCovs)/nSib
	nVar    = length(selVars)/nSib # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	nTot    = nVar * 2

	# Drop unused columns from mz and dzData
	mzData = mzData[, selVars]
	dzData = dzData[, selVars]

	obsMZmeans = umx_means(mzData[, selVars], ordVar = 0, na.rm = TRUE)
	meanDimNames = list("means", selVars)
	meanLabels = c(paste0("mean", 1:nTot), paste0("mean", 1:nTot))
	meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)

	varStarts = diag(umxHetCor(mzData))
	if(nVar == 1){
		varStarts = varStarts/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
	}
	varStarts = matrix(varStarts, nVar, nVar)

	# Matrices a,c,e to store a,c,e path coefficients
	top = mxModel("top",
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique environmental paths
		# TODO fix varStarts
		umxLabel(mxMatrix(name = "a", type = "Full", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)),
		umxLabel(mxMatrix(name = "c", type = "Full", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)),
		umxLabel(mxMatrix(name = "e", type = "Full", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)),  

		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),

		umxLabel(meansMatrix),
		mxMatrix(name = "CholCovW", 'Lower', nrow=nVar, ncol=nVar, values=0.5, free=TRUE),
		mxMatrix(name = "CholCovB", 'Lower', nrow=nVar, ncol=nVar, values=0.5, free=TRUE),
		mxAlgebra(name= "CovB"    , CholCovB %*% t(CholCovB)),
		mxAlgebra(name= "CovW"    , CholCovW %*% t(CholCovW)),
		mxAlgebra(name= "WplusB"  , CovB + CovW),
		# Matrices A, C,E + compute variance components
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),
		# Declare a vector for the regression parameters
		mxMatrix(name = "beta", type = "Full", nrow = (nCov * nSib), ncol = 1, free = TRUE, values = 0, labels = c(paste0("beta", 1:nCov), paste0("beta", 1:nCov))),

		mxAlgebra(name = "tBeta", expression = t(beta)),
		# Algebra for expected variance/covariance matrix #in MZ twins
		mxAlgebra(name = "AC"  , A + C),
		mxAlgebra(name = "ACE" , A + C + E),
		mxAlgebra(name = "hAC" , dzAr * AC),
		mxAlgebra(name = "expCovMZ", expression = rbind(
			cbind(ACE + t(beta) %*% WplusB %*% beta, AC  + t(beta) %*% CovB   %*% beta, tBeta %*% WplusB, tBeta %*% CovB),
			cbind(AC  + t(beta) %*% CovB   %*% beta, ACE + t(beta) %*% WplusB %*% beta, tBeta %*% CovB  , tBeta %*% WplusB),
			cbind(WplusB %*% beta                  , CovB   %*% beta      , WplusB          , CovB),
			cbind(CovB   %*% beta                  , WplusB %*% beta      , CovB            , WplusB))
		),
		# Algebra for expected variance/covariance matrix #in DZ twins
		mxAlgebra(name="expCovDZ", expression = rbind(
			cbind(ACE+ t(beta) %*% WplusB %*% beta, hAC+ t(beta) %*% WplusB %*% beta, tBeta %*% WplusB, tBeta %*% CovB),
			cbind(hAC+ t(beta) %*% CovB   %*% beta, ACE+ t(beta) %*% CovB   %*% beta, tBeta %*% CovB  , tBeta %*% WplusB),
			cbind(WplusB %*% beta                 , CovB %*% beta                   , WplusB          , CovB),
			cbind(CovB %*% beta                   , WplusB %*% beta                 , CovB            , WplusB))
		)
	) # end top

	MZ  = mxModel("MZ" , mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
	DZ  = mxModel("DZ" , mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

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
			mxMatrix(name  = "I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
			# TODO test that these are identical in all cases
			# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # Total variance
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
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMean14", "expMean15", "expMean16"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMean11", "expMean12", "expMean13")
		)
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