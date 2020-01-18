# Direction of Causation Modeling 
# http://ibg.colorado.edu/cdrom2016/verhulst/Causation/DOC%20student.R

# TODO: plot.MxModelDoC, and umxSummary.MxModel_DoC methods
# TODO: support D for one or both traits

#' Build and run a 2-group Direction of Causation twin models.
#'
#' @description
#' Scientific theory makes claims about causation, but in social science, testing causal claims is often difficult due to an inability to 
#' conduct experimental randomization of traits and situations to people. When twins are available, even when measured on a single occasion, 
#' the pattern of cross-twin cross-trait correlations can (given distinct modes of inheritance) falsify causal hypotheses
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxDoC` implements a 2-group model to form latent variables for each of two traits, and allows testing whether 
#' trait 1 causes trait 2, vice-versa, or even reciprocal causation.
#' 
#' The following figure shows how the DoC model appears as a path diagram (for two latent variables X and Y,
#' each with three indicators), with model shown only for 1 twin, and only one DoC pathway drawn.
#' 
#' \if{html}{\figure{DoC.png}{options: width="50\%" alt="Figure: Direction of Causation"}}
#' \if{latex}{\figure{DoC.pdf}{options: width=7cm}}
#'
#' @details
#' To be added. 
#' @param name The name of the model (defaults to "DOC").
#' @param var1Indicators variables defining latent trait 1
#' @param var2Indicators variables defining latent trait 2
#' @param causal whether to add the causal paths (default TRUE)
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @return - [mxModel()] of subclass mxModel.DOC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxPlotDOC()], [umxSummaryDOC()], [umxModify()]
#' @references - N.A. Gillespie and N.G. Marting (2005). Direction of Causation Models. 
#' In *Encyclopedia of Statistics in Behavioral Science*, Volume 1 pp. 496–499. Eds. Brian S. Everitt & David C. Howell
#' @md
#' @examples
#' \dontrun{
#' # ========================
#' # = Does Rain cause Mud? =
#' # ========================
#'
#' # ================
#' # = Prepare Data =
#' # ================
#' mzData = read.csv("~/Desktop/DOC/MZdata.csv")
#' dzData = read.csv("~/Desktop/DOC/DZdata.csv")
#' varNames = paste0(rep(c("a", "b"), each= 3), c(1:3))
#'
#' names(mzData) = c("X", paste0(varNames, "_T1"), paste0(varNames, "_T2"), "sex_T1", "sex_T2")
#' names(dzData) = c("X", paste0(varNames, "_T1"), paste0(varNames, "_T2"), "sex_T1", "sex_T2")
#'
#' Chol  = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3), mzData= mzData, dzData= dzData, sep = "_T", causal= FALSE)
#' DoC   = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3), mzData= mzData, dzData= dzData, sep = "_T", causal= TRUE)
#' A2B   = umxModify(DoC, "a2b", free = TRUE, name = "A2B")            ; summary(A2B)
#' B2A   = umxModify(DoC, "b2a", free = TRUE, name = "B2A")            ; summary(B2A)
#' Recip = umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip"); summary(Recip)
#'
#' umxCompare(Chol, c(A2B, B2A, Recip))
#' }
#' 
umxDoC <- function(name = "DOC", var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = NULL, causal= TRUE, autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	tryHard = match.arg(tryHard)
	nSib    = 2 # Number of siblings in a twin pair.
	nLat    = 2 # 2 latent variables

	nLat1   = length(var1Indicators) # measures for factor 1
	nLat2   = length(var2Indicators)
	nVar    = nLat1 + nLat2

	xmu_twin_check(selDVs= c(var1Indicators,var2Indicators), sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)

	# ================
	# = Make FacLoad =
	# ================
	# 1. make matrix, initialized to fixed @ 0
	FacLoad = umxMatrix(name="FacLoad", "Full", nrow=nVar, ncol=nLat, free=FALSE, value = 0)
	# 2. set FacLoad manifest loadings to pattern of 0 and 1
	FacLoad$free[1:nLat1                  ,1] = TRUE
	FacLoad$values[1:nLat1                ,1] = 1
	FacLoad$free[(nLat1+1):(nLat1+nLat2)  ,2] = TRUE
	FacLoad$values[(nLat1+1):(nLat1+nLat2),2] = 1


	top = mxModel("top", # (was "ACE")
		#  Worker matrices
		umxMatrix("her"  , "Full", nrow=nLat, ncol=nLat, free=FALSE, values=c(1,.5,.5,1) ), # Heredity Matrix for DZ
		umxMatrix("unit" , "Full", nrow=nLat, ncol=nLat, free=FALSE, values=1 ),            # Unit Matrix - For Com Env and MZ
		iTwin = umxMatrix("iTwin", "Iden", nrow=nSib, ncol=nSib),                           # Identity matrix
		umxMatrix("iTwin", "Iden", nrow=nSib, ncol=nSib),                                   # Identity matrix (2by2: 1s on diag, 0 off diag)

		# Matrices for Cholesky (swapped out after if causal)
		umxMatrix("a", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Genetic effects on Latent Variables 
		umxMatrix("c", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Common env effects on Latent Variables
		umxMatrix("e", type="Lower", nrow=nLat, ncol=nLat, free= c(FALSE,TRUE,FALSE), values= 1), # Non-shared env effects on Latent Variables 

		# 4x4 Matrices for A, C, and E
		mxAlgebra(name="Amz", unit  %x% (a %*% t(a))),
		mxAlgebra(name="Adz", her   %x% (a %*% t(a))),
		mxAlgebra(name="C"  , unit  %x% (c %*% t(c))),
		mxAlgebra(name="E"  , iTwin %x% (e %*% t(e))),
		mxAlgebra(name="Vmz", (Amz + C + E)),
		mxAlgebra(name="Vdz", (Adz + C + E)),

		### Generate the Asymmetric Matrix
		# Non-shared env effects on Latent Variables 
		umxMatrix("beta", "Full", nrow=nLat, ncol=nLat, free=FALSE, labels = c("a2a", "a2b", "b2a", "b2b"), values= 0),
		mxAlgebra(name= "cause", iTwin %x% solve(iTwin - beta)),

		### Generate the Factor Loading Matrix
		FacLoad,
		mxAlgebra(name="FacLoadtw", iTwin %x% FacLoad),

		## Covariance between the items due to the latent factors
		mxAlgebra(name= "FacCovMZ", FacLoadtw %&% (cause %&% Vmz)),
		mxAlgebra(name= "FacCovDZ", FacLoadtw %&% (cause %&% Vdz)),
		# Matrices to store  a, c, and e "specific" path coefficients (residuals of manifest phenotypes)
		# TODO smart var starts here
		umxMatrix(name= "as", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "cs", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "es", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		mxAlgebra(name= "specAmz",  unit %x% as),
		mxAlgebra(name= "specAdz",   her %x% as),
		mxAlgebra(name= "specCtw",  unit %x% cs),
		mxAlgebra(name= "specEtw", iTwin %x% es),
		mxAlgebra(name= "specCovMZ", specAmz + specCtw + specEtw),
		mxAlgebra(name= "specCovDZ", specAdz + specCtw + specEtw),
		# Expected Covariance Matrices for MZ and DZ
		mxAlgebra(name= "expCovMZ", FacCovMZ + specCovMZ),
		mxAlgebra(name= "expCovDZ", FacCovDZ + specCovDZ),
		# Means for the Manifest Variables
		Mean = umxMatrix(name="Mean", "Full", nrow= 1, ncol= nCol, free= TRUE, values= .1),
		# Why not just make ncol = nCol*2 and allow label repeats the equate means? Alg might be more efficient?
		expMean = mxAlgebra(name= "expMean", cbind(top.Mean, top.Mean))
	)

	MZ = mxModel("MZ", mxData(mzData, type= "raw"), mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames= selVars), mxFitFunctionML() )
	DZ = mxModel("DZ", mxData(dzData, type= "raw"), mxExpectationNormal("top.expCovDZ", means="top.expMean", dimnames=selVars), mxFitFunctionML() )

	if(!causal){
		# ========================
		# = Cholesky-based model =
		# ========================
		model = mxModel("Chol", top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ"))	)
	}else{
		# ===================
		# = DOC-based model =
		# ===================

		# Replace lower ace Matrices with diag for DOC script.
		# Because covariance between the traits is "caused", theses matrices are diagonal instead of lower
		top = mxModel(top,
			umxMatrix("a", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Genetic effects on Latent Variables 
			umxMatrix("c", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Common env effects on Latent Variables
			umxMatrix("e", "Diag", nrow=nLat, ncol=nLat, free=FALSE,values=1)    # Non-shared env effects on Latent Variables 
		)
		model = mxModel("DOC", top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")) )		
	}
	# Factor loading matrix of Intercept and Slope on observed phenotypes
	# SDt = mxAlgebra(name= "SDt", solve(sqrt(iTwin *Rt))) # Standardized deviations (inverse)
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelDoC") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE)
	return(model)
}

