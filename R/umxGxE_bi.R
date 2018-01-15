#' umxGxEbiv: Implements bivariate ACE models with moderation of variable paths by a moderator.
#'
#' Make a 2-group bivariate GxE model (Dolan 2012). GxE interaction studies test the hypothesis that the strength
#' of genetic (or environmental) influence varies parametrically (usually linear effects on path estimates)
#' across levels of environment. umxGxE allows use of this model in cases where twins are not identical on the moderator (or, rarely, are not completely independent).
#' It supports testing, and visualizing  G xE (or C or E x E) interaction forms.
#' 
#' The following figure the GxE model as a path diagram:
#' # TODO Add bivariate GxE model diagram!!
#' \figure{GxE.png}
#'
#' @param name The name of the model (defaults to "GxEbiv")
#' @param selDVs The dependent variable (e.g. IQ)
#' @param selDefs The definition variable (e.g. socio economic status)
#' @param sep Expand variable base names, i.e., "_T" makes var -> var_T1 and var_T2
#' @param dzData The DZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param mzData The MZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param lboundACE = numeric: If !is.na, then lbound the main effects at this value (default = NA)
#' @param lboundM   = numeric: If !is.na, then lbound the moderators at this value (default = NA)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (gives a warning) default = FALSE
#' @param autoRun Whether to run the model, and return that (default), or just to create it and return without running.
#' @param optimizer optionally set the optimizer (default NULL does nothing)
#' @return - GxEbiv \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}}, \code{\link{umxReduce}}
#' @references - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. \emph{Twin Research}, 
#' \strong{6}, 554-571. Retrieved from https://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=12573187,
#' van der Sluis, S., Posthuma, D., & Dolan, C. V. (2012). A note on false positives and power in G x E modelling of twin data. \emph{Behavior Genetics}, \strong{42}, 170-186. doi:10.1007/s10519-011-9480-3
#' @examples
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, zygosity == c("MZFF" "MZMM"), selVars)[1:80,]
#' dzData  = subset(twinData, zygosity == c("DZFF" "DZMM" "DZOS"), selVars)[1:80,]
#' m1 = umxGxEbiv(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, dropMissingDef = TRUE)
#' # Plot Moderation
#' umxSummaryGxEbiv(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' m2 = umxModify(m1, "am_.*", regex = TRUE, comparison = TRUE)
#' \dontrun{
#' # TODO: The umxReduce function knows how to test all relevant hypotheses
#' # about model reduction for GxE models, reporting these in a nice table.
#' umxReduce(m1)
#' }
umxGxEbiv <- function(name = "GxEbiv", selDVs, selDefs, dzData, mzData, sep = NULL, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run"), optimizer = NULL) {
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
