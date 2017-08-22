umxACE_cov_fixed <- function(name = "ACE", selDVs, selCovs = NULL, dzData, mzData, sep = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = 0, weightVar = NULL, equateMeans = TRUE, bVector = FALSE, thresholds = c("deviationBased", "WLS"), optimizer = NULL, autoRun = getOption("umx_auto_run"), suffix = NULL) {
		nSib = 2 # number of siblings in a twin pair
		thresholds = match.arg(thresholds)
		if(!is.null(sep)){ suffix = sep }
		if(dzCr == .25 && name == "ACE"){ name = "ADE"}
		xmu_twin_check(selDVs, dzData = dzData, mzData = mzData, optimizer = optimizer, suffix = suffix)
		baseDV_names  = selDVs
		baseCov_names = selCovs
		selDVs  = umx_paste_names(selDVs , suffix, 1:nSib)
		selCovs = umx_paste_names(selCovs, suffix, 1:nSib)
		nCov = length(baseCov_names)
		nVar = length(baseDV_names); # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
		used = c(selDVs, selCovs)
		if(!is.null(weightVar)){
			used = c(used, weightVar)
		}
		# Drop unused columns
		mzData = mzData[, used]
		dzData = dzData[, used]
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
		varStarts = umx_var(mzData[, selDVs[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
		
		if(nVar == 1){
			# sqrt to switch from var to path coefficient scale
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

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

			# copies to debug
			nSib    = 2
			baseDV_names  = c("ht", "wt")
			# TODO: add intercept to incoming cov list
			# baseCov_names = c("intercept", "age", "sex")
			baseCov_names = c("age", "sex")
			nVar    = length(baseDV_names)
			nCov    = length(baseCov_names)
			selDVs  = umx_paste_names(baseDV_names, "_T")
			selCovs = umx_paste_names(baseCov_names, "_T")

			# Bits for top
			bLabs      = paste0("cov", rep(1:nCov, each = nVar*nSib), "b_", rep(baseDV_names, nCov))
			bdimnames  = list(paste0(rep(paste0("cov", 1:nCov, "b"), nSib), paste0("_t", rep(1:nSib, each=nCov))), umx_paste_names(paste0("var", 1:length(baseDV_names), "_T")))
			intLabs      = paste0("cov", rep(1:nCov, each = nVar*nSib), "b_", rep(baseDV_names, nCov))
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEma.R
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEja.R
			# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/twoa/twoACEca.R

			top = mxModel("top", 
				# TODO support quadratic betas on means 
				# TODO allow means to differ? (why?)
				# Matrices for betas [nCov*2, nVar*2], and intercepts [1, nVar*2]
				umxMatrix("betas", "Full", nrow = nCov*nSib, ncol = nVar*nSib, free = TRUE, values = .01, labels = bLabs, dimnames = bdimnames, byrow = TRUE)
				umxMatrix("Intercepts", "Full", nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, labels = , dimnames = meanDimNames)
			)

			defCov_dimnames = list("defCov", paste0(baseCov_names, "_t", rep(1:nSib, each = nCov)))
			# defCovs$values
			# betas$values %*% defCovs$values
			MZ = mxModel("MZ", 
				# row of defCovs for t1 and t2
				umxMatrix("defCovs", "Full", nrow = 1, ncol = (nCov * nSib), free = FALSE, 
						   labels = paste0("data.", selCovs), dimnames = defCov_dimnames)
				# Create Algebra for expected Mean Matrices
				mxAlgebra(name = "expMean", top.Intercepts + (defCovs %*% top.betas))
				mxExpectationNormal("top.expCovMZ", "top.expMean"), 
				mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw")
			)
			DZ = mxModel("DZ", 
				# row of defCovs for t1 and t2
				umxMatrix("defCovs", "Full", nrow = 1, ncol = (nCov * nSib), free = FALSE, 
					   labels = paste0("data.", selCovs), dimnames = defCov_dimnames)
				# Create Algebra for expected Mean Matrices
				mxAlgebra(name = "expMean", top.Intercepts + (defCovs %*% top.betas))
				mxExpectationNormal("top.expCovDZ", "top.expMean"), 
				mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw")
			)
		} else if(sum(isBin) == 0){
			# ==================================================
			# = Handle 1 or more ordinal variables (no binary) =
			# ==================================================
			message("umxACE found ", (nOrdVars/nSib), " pair(s) of ordinal variables:", omxQuotes(ordVarNames),
			" (No binary)")			
			if(length(contVarNames) > 0){
				message(length(contVarNames)/nSib, " pair(s) of continuous variables:", omxQuotes(contVarNames))	
			}else{
				# message("No continuous variables found.")
			}
			# Means: all free, start cont at the measured value, ord @0
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
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
				stop("left_censored doesn't make sense for binary variables. I also can't handle mixtures of censored and binary yet, sorry")
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
		umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
		umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
		umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE), 
	
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "A", a %*% t(a)), # additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # unique environmental variance
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
		  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMean11", "expMean12", "expMean13")
		)
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModel.ACE") # set class so that S3 plot() dispatches.
	
	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
		if(!is.na(umx_set_auto_plot(silent = TRUE))){
			plot(model)
		}
	}
	return(model)
} # end umxACE