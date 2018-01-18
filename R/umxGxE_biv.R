#' Bivariate ACE models with moderation of variable paths by a moderator.
#'
#' Creates and runs a 2-group bivariate GxE model (Purcell, 2002; van der Sluis et al., 2012). GxE interaction studies test the hypothesis that the strength
#' of genetic (or environmental) influence varies parametrically (usually linear effects on path estimates)
#' across levels of environment. umxGxE_biv allows use of this model in cases where twins are not identical on the moderator (or, rarely, are not completely independent).
#' It supports testing, and visualizing GxE bivariate (or C or E x E) interactions.
#' 
#' The following figure shows the GxE model as a path diagram: *note*: Only Twin 1 is shown.
#' Twin 1 and twin 2 A, C, and E latent traits are connected in the standard fashion, with the
#' covariance of the T1 and T2 latent genetic traits set to .5 for DZ and 1.0 for MZ pairs.
#' For the sake of clarity, C, and E paths are omitted here. These mirror those for A.
#' ![](GxE_biv.png)
#' @param name The name of the model (defaults to "GxE_biv")
#' @param selDVs The dependent variable (e.g. IQ)
#' @param selDefs The definition variable (e.g. socio economic status)
#' @param sep Expand variable base names, i.e., "_T" makes var -> var_T1 and var_T2
#' @param dzData The DZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param mzData The MZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param lboundACE If !NA, then lbound the main effects at this value (default = NA)
#' @param lboundM If !NA, then lbound the moderators at this value (default = NA)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (gives a warning) default = FALSE
#' @param autoRun Whether to run the model, and return that (default), or just to create it and return without running.
#' @param optimizer Optionally set the optimizer (default NULL does nothing)
#' @return - GxE_biv \code{\link{mxModel}}
#' @export
#' @md
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}}, \code{\link{umxReduce}}
#' @references
#' - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. \emph{Twin Research}, 
#' \strong{6}, 554-571. [url](https://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=12573187).
#' 
#' - van der Sluis, S., Posthuma, D., & Dolan, C. V. (2012). A note on false positives and
#' power in G x E modelling of twin data. \emph{Behavior Genetics}, 
#' \strong{42}, 170-186. doi:[10.1007/s10519-011-9480-3](https://doi.org/10.1007/s10519-011-9480-3).
#'
#' @examples
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = "wt"
#' selDefs = "ht"
#' mzData  = subset(twinData, zygosity %in%  c("MZFF", "MZMM"))[1:80,]
#' dzData  = subset(twinData, zygosity %in%  c("DZFF", "DZMM", "DZOS"))[1:80,]
#' m1 = umxGxE_biv(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, sep = "", dropMissingDef = TRUE)
#' # Plot Moderation
#' umxSummaryGxE_biv(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' m2 = umxModify(m1, regex = "am_.*", comparison = TRUE)
#' \dontrun{
#' # TODO: The umxReduce function knows how to test all relevant hypotheses
#' # about model reduction for GxE models, reporting these in a nice table.
#' umxReduce(m1)
#' }
umxGxE_biv <- function(name = "GxE_biv", selDVs, selDefs, dzData, mzData, sep = NULL, lboundACE = NA, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run"), optimizer = NULL) {
	nSib = 2;
	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(!is.null(sep)){
		if(length(sep) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs , sep, 1:2)
		selDefs = umx_paste_names(selDefs, sep, 1:2)
	}
	if(any(selDefs %in% selDVs)) {
		warning("selDefs was found in selDVs: You probably gave me all the variables in selDVs instead of just the DEPENDENT variable");
	}
	if(length(selDVs)/nSib != 1){
		umx_msg(selDVs)
		stop("selDVs list must be 1 variable... You tried ", length(selDVs)/nSib, " or perhaps you didn't set sep= ?")
	}
	if(length(selDefs) != 2){
		umx_msg(selDefs)
		warning("selDefs must be length = 2. Or perhaps you didn't set sep = ?");
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
			# TODO:	Add covariates to G x E biv model
			# ACE model moderator – Cholesky elements
			# path coefficients a, c, e
			# Defined as scalars (1x1)
			# TODO Can all of these 1x1s be wrapped up into something bigger?
			# A chol
			# a0m  = a11
			# a0mt = a21
			# a0t  = a22
			# a1mt = aBeta1
			# a1t  = aBeta2

			# ACE model moderator main effects parameters
			# a0m = a11
			umxMatrix("a11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("c11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("e11", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),

			# ACE model moderator-trait covariances: main effects
			# a0mt = a21
			umxMatrix("a21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6), 
			umxMatrix("c21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),
			umxMatrix("e21", "Lower", nrow=1, ncol=1, free=TRUE, values=.6),

			# ACE model trait-specific main effects parameters
			# a0t = a22
			umxMatrix("a22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
			umxMatrix("c22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),
			umxMatrix("e22", "Lower", nrow = 1, ncol = 1, free = TRUE, values = .6),


			# ACE model  moderator-trait covariances: moderation
			# a1mt = aBeta1?
			umxMatrix("aBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0), 
			umxMatrix("cBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("eBeta1", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),	

			# ACE model trait 1 moderator effects
			# a1t = aBeta2?
			umxMatrix("aBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("cBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
			umxMatrix("eBeta2", "Lower", nrow=1, ncol=1, free=TRUE, values = .0),
	
			# Assemble parameters into Cholesky decomposition for twin 1 and twin 2 
			umxMatrix("PsAmz", "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1)),
			umxMatrix("PsAdz", "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0,.5, 0, 1, 0,.5, 1, 0, 1)),
			umxMatrix("PsC"  , "Symm", nrow=4, ncol=4, free=FALSE, values= c(1, 0, 1, 0, 1, 0, 1, 1, 0, 1)),
	
			# Matrix & Algebra for expected means vector (non-moderated)
			# TODO start means at obsMean
			# TODO allow other covs on means?
			umxMatrix("expMean", "Full", nrow = 1, ncol = 4, free = TRUE, values = 0, labels = c("m_mod", "m_trait", "m_mod", "m_trait"))
		),
		mxModel("MZ",
			# Definition variables to create moderated paths (M -> T)
			umxMatrix("mod1", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[1])),
			umxMatrix("mod2", "Full", nrow = 1, ncol = 1, free = FALSE, labels = paste0("data.", selDefs[2])),

			# Matrices generated to hold A and E computed Variance Components
			# This is a Cholesky decomposition of A for twin 1 and twin 2 (mz and dz) 
			# note that mod1 appears in the top left part (twin 1) and mod2 in the bottom right part (twin 2)

			# A chol
			# a0m  = a11
			# a0mt = a21
			# a0t  = a22
			# a1mt = aBeta1
			# a1t  = aBeta2

			mxAlgebra(name = "chA", rbind(
				cbind(top.a11, 0, 0, 0),
				cbind(top.a21 + mod1 %x% top.aBeta1, top.a22 + mod1 %x% top.aBeta2, 0, 0),
				cbind(0                 , 0               , top.a11, 0),
				cbind(0                 , 0               , top.a21 + mod2 %x% top.aBeta1, top.a22 + mod2 %x% top.aBeta2))
			),

			# C chol
			mxAlgebra(name = "chC", rbind(
				cbind(top.c11, 0, 0, 0),
				cbind(top.c21 + mod1 %x% top.cBeta1, top.c22 + mod1 %x% top.cBeta2, 0  , 0),
				cbind(0                 ,0                , top.c11, 0),
				cbind(0                 ,0                , top.c21 + mod2 %x% top.cBeta1, top.c22 + mod2 %x% top.cBeta2))
			),
			# E chol
			mxAlgebra(name = "chE", rbind(
				cbind(top.e11, 0, 0 , 0),
				cbind(top.e21 + mod1 %x% top.eBeta1, top.e22 + mod1 %x% top.eBeta2, 0 , 0),
				cbind(0                 ,0                ,top.e11, 0),
				cbind(0                 ,0                ,top.e21 + mod2 %x% top.eBeta1, top.e22 + mod2 %x% top.eBeta2))
			),

			# Get the expected covariance matrices 
			mxAlgebra(name = "Amz", chA %&% top.PsAmz),  # variance component A
			mxAlgebra(name = "C"  , chC %&% top.PsC  ),  # variance component C
			mxAlgebra(name = "E"  , chE %*% t(chE)), # variance component E

			# Assemble phenotypic 4 x 4 MZ cov matrix 
			mxAlgebra(name = "expCovMZ", Amz + C + E), 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("expCovMZ", means = "top.expMean", dimnames = selVars),
			mxFitFunctionML()
		),
	    mxModel("DZ",
			# defs, e.g twin1  c("data.divorce1")
			umxMatrix("mod1", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[1])), # "data.defmod1"
			umxMatrix("mod2", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[2])), # "data.defmod2"

			# Matrices generated to hold A and E computed Variance Components
			# A
			mxAlgebra(name ="chA", rbind(
				cbind(top.a11, 0, 0, 0),
				cbind(top.a21 + mod1 %x% top.aBeta1, top.a22 + mod1 %x% top.aBeta2, 0, 0),
				cbind(0, 0, top.a11, 0),
				cbind(0, 0, top.a21 + mod2 %x% top.aBeta1, top.a22 + mod2 %x% top.aBeta2))
			),
			# C chol
			mxAlgebra(name ="chC", rbind(
				cbind(top.c11, 0, 0, 0),
				cbind(top.c21 + mod1 %x% top.cBeta1, top.c22 + mod1 %x% top.cBeta2, 0, 0),
				cbind(0, 0, top.c11, 0),
				cbind(0, 0, top.c21 + mod2 %x% top.cBeta1, top.c22 + mod2 %x% top.cBeta2))
			),
			# E chol
			mxAlgebra(name ="chE", rbind(
				cbind(top.e11, 0, 0, 0),
				cbind(top.e21 + mod1 %x% top.eBeta1, top.e22 + mod1 %x% top.eBeta2, 0, 0),
				cbind(0, 0, top.e11, 0),
				cbind(0, 0, top.e21 + mod2 %x% top.eBeta1, top.e22 + mod2 %x% top.eBeta2))
			),

			mxAlgebra(name = "Adz", chA %*% top.PsAdz %*% t(chA)),  # variance component A
			mxAlgebra(name = "C"  , chC %*% top.PsC   %*% t(chC)),  # variance component C
			mxAlgebra(name = "E"  , chE %*% t(chE)),            # variance component E

			# Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
			mxAlgebra(name = "expCovDZ", Adz + C + E),
			# Data & Objective
			mxData(dzData, type = "raw"),
			mxExpectationNormal("expCovDZ", means = "top.expMean", dimnames = selDVs),
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
