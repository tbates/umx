umxGxE_bi <- function(name = "G_by_E_bivariate", selDVs, selDefs, dzData, mzData, sep = NULL, lboundACE = NA, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run"), optimizer = NULL) {
	nSib = 2;
	suffix = sep
	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}

	if(!is.null(suffix)){
		if(length(suffix) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs , suffix, 1:2)
		selDefs = umx_paste_names(selDefs, suffix, 1:2)
	}
	if(any(selDefs %in% selDVs)) {
		warning("selDefs was found in selDVs: You probably gave me all the variables in selDVs instead of just the DEPENDENT variable");
	}
	if(length(selDVs)/nSib != 1){
		stop("selDVs list must be 1 variable... You tried ", length(selDVs)/nSib)
	}
	if(length(selDefs) != 2){
		warning("selDefs must be length = 2");
	}

	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	message("selDVs: ", omxQuotes(selDVs))

	selVars   = c(selDVs, selDefs)
	obsMean   = mean(colMeans(mzData[,selDVs], na.rm = TRUE)); # Just one average mean for all twins
	nVar      = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	rawVar    = diag(var(mzData[,selDVs], na.rm = TRUE))[1]
	startMain = sqrt(c(.8, .0 ,.6) * rawVar)	
	umx_check(!umx_is_cov(dzData, boolean = TRUE), "stop", "data must be raw for gxe")
	
	# drop any unused variables
	dzData = dzData[,selVars]
	mzData = mzData[,selVars]
	
	if(any(is.na(mzData[,selDefs]))){
		if(dropMissingDef){
			missingT1 = is.na(mzData[,selDefs[1]])
			missingT2 = is.na(mzData[,selDefs[2]])
			missDef = (missingT1 | missingT2)
			message(sum(missDef), " mz rows dropped due to missing def var for Twin 1 or Twin 2 or both")
			mzData = mzData[!missDef, ]
		} else {
			stop("Some rows of mzData have NA definition variables. Remove these yourself, or set dropMissing = TRUE")
		}
	}
	if(any(is.na(dzData[,selDefs]))){
		if(dropMissingDef){
			missDef = is.na(dzData[,selDefs[1]]) | is.na(dzData[,selDefs[2]])
			message(sum(missDef), " dz rows dropped due to missing def var for Twin 1 or Twin 2 or both")
			dzData = dzData[!missDef, ]
		} else {
			stop("Some rows of dzData have NA definition variables. Remove these yourself, or set dropMissing = TRUE")
		}
	}
	
	
	model = mxModel(name,
		mxModel("top",
			# Matrices a, c, and e to store a, c, and e path coefficients
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[1], name = "a" ), jiggle = .0001),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[2], name = "c" ), jiggle = .0001),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[3], name = "e" ), jiggle = .0001),
			# Matrices to store moderated path coefficients                       
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "am" )),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "cm" )),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "em" )),

			# Matrices A, C, and E compute non-moderated variance components 
			mxAlgebra(name = "A", a %*% t(a) ),
			mxAlgebra(name = "C", c %*% t(c) ),
			mxAlgebra(name = "E", e %*% t(e) ),
			# Algebra to compute total variances and inverse of standard deviations (diagonal only)
			mxAlgebra(name = "V", A + C + E),
			mxMatrix(name  = "I", "Iden", nrow = nVar, ncol = nVar),
			mxAlgebra(name = "iSD", solve(sqrt(I * V)) ),

			# Matrix & Algebra for expected means vector (non-moderated)
			mxMatrix(name = "Means", "Full", nrow = 1, ncol = nVar, free = TRUE, values = obsMean, labels = "mean"), # needs mods for multivariate!
			# Matrices for betas
			mxMatrix(name = "betaLin" , "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "lin11"), 
			mxMatrix(name = "betaQuad", "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "quad11")

			# TODO:	Add covariates to G x E model
			# if(0){
				# mxMatrix(name = "betas" , "Full", nrow = nCov, ncol = nVar, free = T, values = 0.05, labels = paste0("beta_", covariates))
			# }
		),
		mxModel("MZ",
			# matrices for covariates (just on the means)
			# Matrix for moderating/interacting variable
			umxMatrix("Def1", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[1])), # c("data.age1")
			umxMatrix("Def2", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[2])), # c("data.age2")
			# Algebra for expected mean vector
			# TODO simplify this algebra... one for each twin... not 4* cov...
			mxAlgebra(top.betaLin %*% Def1  , name = "Def1Rlin"),
			mxAlgebra(top.betaQuad%*% Def1^2, name = "Def1Rquad"),
			mxAlgebra(top.betaLin %*% Def2  , name = "Def2Rlin"),
			mxAlgebra(top.betaQuad%*% Def2^2, name = "Def2Rquad"),
			# if(0){ # TODO if there are covs
			# 	mxMatrix(name = "covsT1", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT1)),
			# 	mxMatrix(name = "covsT2", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT2)),
			# 	mxAlgebra(top.betas %*% covsT1, name = "predMeanT1"),
			# 	mxAlgebra(top.betas %*% covsT2, name = "predMeanT2"),
			# 	mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad + predMeanT1,
			# 	                 top.Means + Def2Rlin + Def2Rquad + predMeanT2), name = "expMeans")
			# } else {
				# mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeans")
			# },
			mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeanMZ"),

			# Compute ACE variance components
			mxAlgebra((top.a + top.am %*% Def1) %*% t(top.a+ top.am %*% Def1), name = "A11"),
			mxAlgebra((top.c + top.cm %*% Def1) %*% t(top.c+ top.cm %*% Def1), name = "C11"),
			mxAlgebra((top.e + top.em %*% Def1) %*% t(top.e+ top.em %*% Def1), name = "E11"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def1) %*% t(top.a+ top.am %*% Def2), name = "A12"),
			mxAlgebra((top.c + top.cm %*% Def1) %*% t(top.c+ top.cm %*% Def2), name = "C12"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def2) %*% t(top.a+ top.am %*% Def1), name = "A21"),
			mxAlgebra((top.c + top.cm %*% Def2) %*% t(top.c+ top.cm %*% Def1), name = "C21"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def2) %*% t(top.a+ top.am %*% Def2), name = "A22"),
			mxAlgebra((top.c + top.cm %*% Def2) %*% t(top.c+ top.cm %*% Def2), name = "C22"),
			mxAlgebra((top.e + top.em %*% Def2) %*% t(top.e+ top.em %*% Def2), name = "E22"),

			# Algebra for expected variance/covariance matrix and expected mean vector in MZ
			mxAlgebra(rbind(cbind(A11+C11+E11, A12+C12),
			                cbind(A21+C21    , A22+C22+E22) ), name = "expCovMZ"),
			# Data & Objective
			mxData(mzData, type = "raw"),
			mxExpectationNormal("expCovMZ", means = "expMeanMZ", dimnames = selDVs),
			mxFitFunctionML()
		),
	    mxModel("DZ",
			mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[1],sep=""), name="Def1"), # twin1  c("data.divorce1")
			mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[2],sep=""), name="Def2"), # twin2  c("data.divorce2")
			# Compute ACE variance components
			mxAlgebra((top.a+ top.am%*% Def1) %*% t(top.a+ top.am%*% Def1), name="A11"),
			mxAlgebra((top.c+ top.cm%*% Def1) %*% t(top.c+ top.cm%*% Def1), name="C11"),
			mxAlgebra((top.e+ top.em%*% Def1) %*% t(top.e+ top.em%*% Def1), name="E11"),

			mxAlgebra((top.a+ top.am%*% Def1) %*% t(top.a+ top.am%*% Def2), name="A12"),
			mxAlgebra((top.c+ top.cm%*% Def1) %*% t(top.c+ top.cm%*% Def2), name="C12"),

			mxAlgebra((top.a+ top.am%*% Def2) %*% t(top.a+ top.am%*% Def1), name="A21"),
			mxAlgebra((top.c+ top.cm%*% Def2) %*% t(top.c+ top.cm%*% Def1), name="C21"),

			mxAlgebra((top.a+ top.am%*% Def2) %*% t(top.a+ top.am%*% Def2), name="A22"),
			mxAlgebra((top.c+ top.cm%*% Def2) %*% t(top.c+ top.cm%*% Def2), name="C22"),
			mxAlgebra((top.e+ top.em%*% Def2) %*% t(top.e+ top.em%*% Def2), name="E22"),

			# Expected DZ variance/covariance matrix
			mxAlgebra(rbind(cbind(A11+C11+E11  , 0.5%x%A12+C12),
			                cbind(0.5%x%A21+C21, A22+C22+E22) ), name="expCovDZ"),
			# mxAlgebra(rbind(cbind(A11+C11+E11  , 0.5%x%A21+C21),
			#                 cbind(0.5%x%A12+C12, A22+C22+E22) ), name="expCov"),
			# Algebra for expected mean vector
			mxAlgebra(top.betaLin %*% Def1  , name = "Def1Rlin"),
			mxAlgebra(top.betaQuad%*% Def1^2, name = "Def1Rquad"),
			mxAlgebra(top.betaLin %*% Def2  , name = "Def2Rlin"),
			mxAlgebra(top.betaQuad%*% Def2^2, name = "Def2Rquad"),
			mxAlgebra(cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeanDZ"),
			# mxAlgebra(top.betas%*%rbind(Def1, Def1^2), name="Def1R"),
			# mxAlgebra(top.betas%*%rbind(Def2, Def2^2), name="Def2R"),
			# mxAlgebra( cbind(top.Means+Def1R, top.Means+Def2R), name="expMeans"),
			# Data & Objective
	        mxData(dzData, type = "raw"),
			mxExpectationNormal("expCovDZ", means = "expMeanDZ", dimnames = selDVs),
			mxFitFunctionML()
	    ),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)

	if(!is.na(lboundACE)){
		model = omxSetParameters(model, labels = c('a_r1c1', 'c_r1c1', 'e_r1c1'), lbound = lboundACE)
	}
	if(!is.na(lboundM)){
		model = omxSetParameters(model, labels = c('am_r1c1', 'cm_r1c1', 'em_r1c1'), lbound = lboundM)
	}
	model = as(model, "MxModel.GxE")
	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
	}
	return(model)
}
