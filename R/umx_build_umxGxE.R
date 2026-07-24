# R/umx_build_umxGxE.R

#' umxGxE: Implements ACE models with moderation of paths, e.g. by SES.
#'
#' Make a 2-group GxE (moderated ACE) model (Purcell, 2002). GxE interaction studies test the hypothesis that the strength
#' of genetic (or environmental) influence varies parametrically (usually linear effects on path estimates)
#' across levels of environment. umxGxE allows detecting,
#' testing, and visualizing  G xE (or C or E x E) interaction forms.
#'
#' Paths are linear in the moderator \(M\): \(a_i = a + a_m M\) (and likewise for \(c\), \(e\)).
#' Estimated main effects (\code{a}, \code{c}, \code{e}) and moderation slopes
#' (\code{am}, \code{cm}, \code{em}) live on the \code{top} submodel.
#'
#' The following figure shows the GxE model as a path diagram:
#'
#' \if{html}{\figure{GxE.png}{options: style="width: 50\%;" alt="Figure: GxE.png"}}
#' \if{latex}{\figure{GxE.pdf}{options: width=7cm}}
#'
#' @section Synthetic data and recovery checks:
#' For benchmarking and teaching, generate wide twin data with known Purcell paths
#' via [umx_make_GxE_data()]. That helper returns a data frame ready for
#' \code{umxGxE} (columns \code{zygosity}, \code{outcome_T*}, \code{outcomeAge_T*},
#' \code{mod_T*}, \code{age_T*}) plus attribute \code{"truth"} with the generative
#' parameters. The attribute is ignored by fitting; use it only to compare estimates
#' to ground truth, e.g. \code{attr(df, "truth")$am}.
#'
#' Defaults in the simulator: \(a=0.50\), \(c=0.30\), \(e=0.60\), \(a_m=0.15\),
#' \(c_m=e_m=0\). Two phenotypes share the same latent ACE draws:
#' \describe{
#'   \item{\code{outcome}}{Pure GxE (no age mean effect). Fit with
#'     \code{selDVs = "outcome"}, \code{selDefs = "mod"}.}
#'   \item{\code{outcomeAge}}{Same residuals plus a mean effect of age
#'     (target \(r \approx 0.2\) with age). Fit with
#'     \code{selDVs = "outcomeAge"}, \code{selDefs = "mod"}, \code{selCovs = "age"}.}
#' }
#' Moderator and age are shared within family by default (\code{mod_T1 == mod_T2},
#' \code{age_T1 == age_T2}).
#'
#' **Power:** Moderated ACE is low-powered. Even with thousands of pairs, recovery of
#' \(a_m\) is noisy; large absolute error relative to the true slope is expected in
#' single replications. Prefer \code{tryHard = "yes"}, adequate \(N\), and
#' simulation studies rather than one-shot recovery demos.
#'
#' @param name The name of the model (default= "G_by_E")
#' @param selDVs The dependent variable (e.g. "IQ")
#' @param selDefs The definition variable (e.g. "SES")
#' @param selCovs (optional) covariates to include in the means model (do not include sep in names)
#' @param sep How to expand selDVs into full names, i.e., "_T" makes "var" -> "var_T1" and "var_T2"
#' @param dzData The DZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param mzData The MZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param data If provided, dzData and mzData are treated as valid levels of zyg to select() data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param digits Rounding precision for tables (default 3)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (default = TRUE). You get a polite note.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param lboundACE If not NA, then lbound the main effects at this value (default = NA, can help to set this to 0)
#' @param lboundM   If not NA, then lbound the moderator effects at this value (default = NA, can help to set this to 0)
#' @param autoRun Optionally run the model (default), or just to create it and return without running.
#' @param tryHard Optionally tryHard to get the model to converge (Default = 'no'). "yes" uses mxTryHard. Other options: "ordinal", "search".
#' @param optimizer Optionally set the optimizer (default NULL does nothing)
#' @return - GxE [OpenMx::mxModel()]
#' @export
#' @seealso [umx_make_GxE_data()], [umxGxE_window()], [umxReduce()], [umxSummary()]
#' @family Twin Modeling Functions
#' @references - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. *Twin Research*,
#'  **6**, 554-571. \doi{10.1375/twin.5.6.554}
#'
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = "bmi"
#' selDefs = "age"
#' mzData  = subset(twinData, zygosity == "MZFF")[1:100,]
#' dzData  = subset(twinData, zygosity == "DZFF")[1:100,]
#' m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
#'
#' # Select the data on the fly with data= and zygosity levels
#' m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep="", dzData= "DZFF", mzData= "MZFF", data= twinData)
#'
#' # ===============================================================
#' # = example with Twins having different values of the moderator =
#' # ===============================================================
#'
#' twinData$age1 = twinData$age2 = twinData$age
#' tmp = twinData
#' tmp$age2 = tmp$age2 +rnorm(n=length(tmp$age2))
#' selDVs  = "bmi"
#' selDefs = "age"
#' mzData = subset(tmp, zygosity == "MZFF")
#' dzData = subset(tmp, zygosity == "DZFF")
#' m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
#'
#' # ====================================
#' # = Controlling output of umxSummary =
#' # ====================================
#' umxSummaryGxE(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = TRUE)
#'
#' # # Test dropping moderation on a path
#' m2 = umxModify(m1, regex = "am_.*", comparison = TRUE, tryHard = "yes")
#'
#' # umxReduce knows how to test all relevant hypotheses for GxE models,
#' # reporting these in a nice table.
#' umxReduce(m1)
#'
#' # ============================================================
#' # = Synthetic ground-truth data: umx_make_GxE_data + umxGxE =
#' # ============================================================
#' # Generate N pairs with known a, c, e, am (see attr(df, "truth"))
#' df = umx_make_GxE_data(nMZpairs = 800, nDZpairs = 800, seed = 1)
#' truth = attr(df, "truth")
#' truth[c("a", "c", "e", "am", "cm", "em")]
#'
#' # Pure GxE phenotype (no age in means)
#' mGxE = umxGxE(selDVs = "outcome", selDefs = "mod", sep = "_T",
#'   data = df, mzData = "MZ", dzData = "DZ", tryHard = "yes")
#' c(est_am = mGxE$top$am$values[1, 1], true_am = truth$am)
#'
#' # Same latents + age mean effect via selCovs
#' mAge = umxGxE(selDVs = "outcomeAge", selDefs = "mod", selCovs = "age",
#'   sep = "_T", data = df, mzData = "MZ", dzData = "DZ", tryHard = "yes")
#'
#' # Custom generative slopes (e.g. null am, nonzero em)
#' df2 = umx_make_GxE_data(nMZpairs = 500, nDZpairs = 500,
#'   am = 0, em = 0.10, seed = 2)
#' attr(df2, "truth")[c("am", "em")]
#' }
umxGxE <- function(name = "G_by_E", selDVs, selDefs, dzData, mzData, sep = NULL, data = NULL, zyg = "zygosity", digits = 3, lboundACE = NA, lboundM = NA, dropMissingDef = TRUE, dzAr = .5,  dzCr = 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, selCovs = NULL) {

	if(dzCr == .25 & name == "G_by_E") name = "G_by_E_ADE"
	tryHard = match.arg(tryHard)
	nSib    = 2;

	# if data provided create twin files 
	if(!is.null(data)){
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		if(is.null(dzData)){ dzData = "DZ"; mzData = "MZ" }
		mzData = data[data[,zyg] %in% mzData, ]
		dzData = data[data[,zyg] %in% dzData, ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}
	xmu_twin_check(selDVs=selDVs, dzData = dzData, mzData = mzData, optimizer = optimizer, sep = sep, nSib = nSib)

	selDVs  = umx_paste_names(selDVs , sep = sep, suffixes = 1:nSib)
	selDefs = umx_paste_names(selDefs, sep = sep, suffixes = 1:nSib)
	if(any(selDefs %in% selDVs)) {
		warning("selDefs was found in selDVs: You probably gave me all the variables in selDVs instead of just the DEPENDENT variable");
	}
	if(length(selDefs) != nSib){
		warning("selDefs must be length = 2");
	}
	
	umx_check(is.numeric(mzData[, selDefs[1] ]), "stop", "Definition vars (selDefs) must be numeric (not, e.g. factor)")
	
	if(length(selDVs) != nSib){
		stop("DV list must be length = 2: 1 variable for each of 2 twins... You tried ", length(selDVs)/nSib)
	}

	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)

	if(!umx_set_silent(silent = TRUE)){
		message("selDVs: ", omxQuotes(selDVs))
	}

	nCov = 0
	if (!is.null(selCovs)) {
		nCov = length(selCovs)
		fullCovs = umx_paste_names(selCovs, sep = sep, suffixes = 1:2)
		umx_check_names(fullCovs, mzData)
		umx_check_names(fullCovs, dzData)
		selVars = c(selDVs, selDefs, fullCovs)
	} else {
		fullCovs = NULL
		selVars = c(selDVs, selDefs)
	}

	colTypes = umx_is_ordered(mzData[, selDVs, drop = FALSE], summaryObject = TRUE)
	isBin    = isTRUE(colTypes$isBin[1])
	isOrd    = isTRUE(colTypes$isOrd[1])
	isFactor = isTRUE(colTypes$isFactor[1])

	obsMean   = mean(umx_means(mzData[, selDVs, drop = FALSE], ordVar = 0, na.rm = TRUE))
	nVar      = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	rawVar    = umx_var(mzData[, selDVs, drop = FALSE], format = "diag", ordVar = 1, use = "pairwise.complete.obs", strict = FALSE)[1]
	startMain = sqrt(c(.8, .0 ,.6) * rawVar)	
	umx_check(!umx_is_cov(dzData, boolean = TRUE), "stop", "data must be raw for gxe")
	
	# drop unused variables
	dzData = dzData[ , selVars]
	mzData = mzData[ , selVars]
	
	mzData = xmu_data_missing(mzData, selVars = c(selDefs, fullCovs), sep = NULL, dropMissingDef = dropMissingDef, hint= "mzData")
	dzData = xmu_data_missing(dzData, selVars = c(selDefs, fullCovs), sep = NULL, dropMissingDef = dropMissingDef, hint= "dzData")
	
	# =====================================
	# = DO T1 and T2 share the moderator? =
	# =====================================
	bModeratorsIdentical = all(mzData[, selDefs[1]] == mzData[, selDefs[2]])
	if(!bModeratorsIdentical){
		message("Twins do not share the moderator... I will regress both twin's moderator from each twin, but you need to check this doesn't violate assumptions")
	}
	
	threshName = ifelse(isFactor, "top.threshMat", as.character(NA))

	model = mxModel(name,
		mxModel("top",		
			# ======================================
			# = Matrices and algebra for the model =
			# ======================================
			# Matrices to store a, c, and e path coefficients
			umxMatrix("a", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[1]),
			umxMatrix("c", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[2]),
			umxMatrix("e", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[3]),
			# Matrices to store moderated path coefficients                       
			umxMatrix("am", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0),
			umxMatrix("cm", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0),
			umxMatrix("em", "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0),

			# Matrices A, C, and E compute non-moderated variance components 
			mxAlgebra(name = "A", a %*% t(a) ),
			mxAlgebra(name = "C", c %*% t(c) ),
			mxAlgebra(name = "E", e %*% t(e) ),
			# Algebra to compute total variances and inverse of standard deviations (diagonal only)
			mxAlgebra(name = "V", A + C + E),
			mxMatrix(name  = "I", "Iden", nrow = nVar, ncol = nVar),
			mxAlgebra(name = "iSD", solve(sqrt(I * V)) ),

			# Matrix & Algebra for intercept of the means vector
			mxMatrix(name = "intercept", "Full", nrow = 1, ncol = nVar, free = !isBin, values = ifelse(isBin, 0, obsMean), labels = "mean"), # needs mods for multivariate!

			if (nCov > 0) {
				betaLabels = paste0(selCovs, "_b_Var1")
				meansBetas = umxMatrix("meansBetas", "Full", nrow = nCov, ncol = nVar, free = TRUE, labels = betaLabels, values = 0, lbound = -2, ubound = 2)
				dimnames(meansBetas) = list(selCovs, selDVs[1])
				meansBetas
			},

			# Matrice for moderated the means model (algebars in the data models)
			if(bModeratorsIdentical){
				# Matrices for betas
				list(
					umxMatrix(name = "betaLin" , "Full", nrow = nVar, ncol = 1, free = !isBin, values = .0, labels = "lin11"),
					umxMatrix(name = "betaQuad", "Full", nrow = nVar, ncol = 1, free = !isBin, values = .0, labels = "quad11")
				)
			}else{
				list(
					umxMatrix(name = "betaSelf"  , "Full", nrow = nVar, ncol = 1, free = !isBin, values = .0), 
					umxMatrix(name = "betaCoTwin", "Full", nrow = nVar, ncol = 1, free = !isBin, values = .0)
				)
			},
			if(isBin){
				mxConstraint(V[1,1] == 1, name = "constrain_Bin_var_to_1")
			},
			if(isFactor){
				umxThresholdMatrix(rbind(mzData, dzData), fullVarNames = selDVs, sep = sep, verbose = FALSE)
			}
		),
		mxModel("MZ",
			# Matrices for moderating/interacting variable
			umxMatrix("DefT1", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[1])), # c("data.age1")
			umxMatrix("DefT2", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[2])), # c("data.age2")
			if (nCov > 0) {
				list(
					umxMatrix("T1DefVars", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", fullCovs[1:nCov])),
					umxMatrix("T2DefVars", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", fullCovs[(nCov+1):(2*nCov)]))
				)
			},
			# ====================================
			# = Algebra for expected mean vector =
			# ====================================

			if(bModeratorsIdentical){
				# do lm(DV ~ moderator + moderator^2)
				if (nCov > 0) {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2) + (T1DefVars %*% top.meansBetas),
						top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2) + (T2DefVars %*% top.meansBetas))
					)
				} else {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2),
						top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2))
					)
				}
			}else{
				# do lm(DV ~ self moderator + co-twin moderator)
				if (nCov > 0) {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2) + (T1DefVars %*% top.meansBetas),
						top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1) + (T2DefVars %*% top.meansBetas))
					)
				} else {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2),
						top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1))
					)
				}
			}
			,

			# Compute ACE variance components
			mxAlgebra(name = "A11", (top.a + top.am %*% DefT1) %*% t(top.a+ top.am %*% DefT1)),
			mxAlgebra(name = "C11", (top.c + top.cm %*% DefT1) %*% t(top.c+ top.cm %*% DefT1)),
			mxAlgebra(name = "E11", (top.e + top.em %*% DefT1) %*% t(top.e+ top.em %*% DefT1)),
                                       
			mxAlgebra(name = "A12", (top.a + top.am %*% DefT1) %*% t(top.a+ top.am %*% DefT2)),
			mxAlgebra(name = "C12", (top.c + top.cm %*% DefT1) %*% t(top.c+ top.cm %*% DefT2)),

			mxAlgebra(name = "A21", (top.a + top.am %*% DefT2) %*% t(top.a+ top.am %*% DefT1)),
			mxAlgebra(name = "C21", (top.c + top.cm %*% DefT2) %*% t(top.c+ top.cm %*% DefT1)),

			mxAlgebra(name = "A22", (top.a + top.am %*% DefT2) %*% t(top.a+ top.am %*% DefT2)),
			mxAlgebra(name = "C22", (top.c + top.cm %*% DefT2) %*% t(top.c+ top.cm %*% DefT2)),
			mxAlgebra(name = "E22", (top.e + top.em %*% DefT2) %*% t(top.e+ top.em %*% DefT2)),

			# Algebra for expected variance/covariance matrix and expected mean vector in MZ
			mxAlgebra(name = "expCovMZ", rbind(
				  cbind(A11+C11+E11, A12+C12),
			      cbind(A21+C21    , A22+C22+E22))
			),

			# Data & Objective
			mxData(mzData, type = "raw"),
			mxExpectationNormal("expCovMZ", means = "expMean", dimnames = selDVs, thresholds = threshName), mxFitFunctionML()
		),
	    mxModel("DZ",
			umxMatrix("DefT1", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[1])), # twin1  c("data.divorce1")
			umxMatrix("DefT2", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[2])), # twin2  c("data.divorce2")
			if (nCov > 0) {
				list(
					umxMatrix("T1DefVars", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", fullCovs[1:nCov])),
					umxMatrix("T2DefVars", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", fullCovs[(nCov+1):(2*nCov)]))
				)
			},
			# =================
			# = Means Algebra =
			# =================
			if(bModeratorsIdentical){
				# do lm(DV ~ moderator + moderator^2)
				if (nCov > 0) {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2) + (T1DefVars %*% top.meansBetas),
						top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2) + (T2DefVars %*% top.meansBetas))
					)
				} else {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2),
						top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2))
					)
				}
			}else{
				# do lm(DV ~ self moderator + co-twin moderator)
				if (nCov > 0) {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2) + (T1DefVars %*% top.meansBetas),
						top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1) + (T2DefVars %*% top.meansBetas))
					)
				} else {
					mxAlgebra(name = "expMean", cbind(
						top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2),
						top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1))
					)
				}
			}
			,
			
			# Compute ACE variance components
			mxAlgebra(name= "A11", (top.a+ top.am%*% DefT1) %*% t(top.a+ top.am%*% DefT1)),
			mxAlgebra(name= "C11", (top.c+ top.cm%*% DefT1) %*% t(top.c+ top.cm%*% DefT1)),
			mxAlgebra(name= "E11", (top.e+ top.em%*% DefT1) %*% t(top.e+ top.em%*% DefT1)),

			mxAlgebra(name= "A12", (top.a+ top.am%*% DefT1) %*% t(top.a+ top.am%*% DefT2)),
			mxAlgebra(name= "C12", (top.c+ top.cm%*% DefT1) %*% t(top.c+ top.cm%*% DefT2)),

			mxAlgebra(name= "A21", (top.a+ top.am%*% DefT2) %*% t(top.a+ top.am%*% DefT1)),
			mxAlgebra(name= "C21", (top.c+ top.cm%*% DefT2) %*% t(top.c+ top.cm%*% DefT1)),

			mxAlgebra(name= "A22", (top.a+ top.am%*% DefT2) %*% t(top.a+ top.am%*% DefT2)),
			mxAlgebra(name= "C22", (top.c+ top.cm%*% DefT2) %*% t(top.c+ top.cm%*% DefT2)),
			mxAlgebra(name= "E22", (top.e+ top.em%*% DefT2) %*% t(top.e+ top.em%*% DefT2)),

			# Expected DZ variance/covariance matrix
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

			mxAlgebra(name= "expCovDZ",
				rbind(cbind(A11+C11+E11, dzAr%x%A12+dzCr%x%C12),
			          cbind(dzAr%x%A21+dzCr%x%C21, A22+C22+E22) )),


			# Data & Objective
	    	mxData(dzData, type = "raw"),
			mxExpectationNormal("expCovDZ", means = "expMean", dimnames = selDVs, thresholds = threshName),
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
	model = as(model, "MxModelGxE")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, digits = digits)
	return(model)
}


#' Implement the moving-window form of GxE analysis.
#'
#' Make a 2-group GxE (moderated ACE) model using LOSEM. In GxE interaction studies, typically,
#' the hypothesis that the strength of genetic influence varies parametrically (usually linear effects
#' on path estimates) across levels of environment. Of course, the function linking genetic influence
#' and context is not necessarily linear, but may react more steeply at the extremes, or take other, unknown forms.
#' To avoid obscuring the underlying shape of the interaction effect, local structural equation
#' modeling (LOSEM) may be used, and GxE_window implements this. LOSEM is a non-parametric,
#' estimating latent interaction effects across the range of a measured moderator using a
#' windowing function which is walked along the context dimension, and which weights subjects
#' near the center of the window highly relative to subjects far above or below the window center.
#' This allows detecting and visualizing arbitrary GxE (or CxE or ExE) interaction forms.
#' 
#' @param selDVs The dependent variables for T1 and T2, e.g. c("bmi_T1", "bmi_T2")
#' @param moderator The name of the moderator variable in the dataset e.g. "age", "SES" etc.
#' @param mzData Dataframe containing the DV and moderator for MZ twins
#' @param dzData Dataframe containing the DV and moderator for DZ twins
#' @param sep (optional) separator, e.g. "_T" which will be used expand base names into full variable names:
#' e.g.: 'bmi' --> c("bmi_T1", "bmi_T2")
#' @param weightCov Whether to use cov.wt matrices or FIML default = FALSE, i.e., FIML
#' @param width An option to widen or narrow the window from its default (of 1)
#' @param target A user-selected list of moderator values to test (default = NULL = explore the full range)
#' @param plotWindow whether to plot the data window.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param return  whether to return the last model (useful for specifiedTargets) or the list of estimates (default = "estimates")
#' @return - Table of estimates of ACE along the moderator
#' @export
#' @seealso [umxGxE()]
#' @family Twin Modeling Functions
#' @references - Hildebrandt, A., Wilhelm, O, & Robitzsch, A. (2009)
#' Complementary and competing factor analytic approaches for the investigation 
#' of measurement invariance. *Review of Psychology*, **16**, 87--107. 
#' 
#' Briley, D.A., Harden, K.P., Bates, T.C., Tucker-Drob, E.M. (2015).
#' Nonparametric Estimates of Gene x Environment Interaction Using Local Structural Equation Modeling.
#' *Behavior Genetics*, **45**, 581-96. \doi{10.1007/s10519-015-9732-8}.
#' 
#' @examples
#' \dontrun{
#' library(umx);
#'
#' # ==============================
#' # = 1. Open and clean the data =
#' # ==============================
#' # umxGxE_window takes a data.frame consisting of a moderator and two DV columns: one for each twin.
#' # The model assumes two groups (MZ and DZ). Moderator can't be missing
#' mod = "age" # The full name of the moderator column in the dataset
#' selDVs = c("bmi1", "bmi2") # The DV for twin 1 and twin 2
#' data(twinData) # Dataset of Australian twins, built into OpenMx
#' # The twinData consist of two cohorts: "younger" and "older".
#' # zygosity is a factor. levels =  MZFF, MZMM, DZFF, DZMM, DZOS.
#' 
#' # Delete missing moderator rows
#' twinData = twinData[!is.na(twinData[mod]), ]
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' 
#' # ========================
#' # = 2. Run the analyses! =
#' # ========================
#' # Run and plot for specified windows (in this case just 1927)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 		target = 40, plotWindow = TRUE)
#' 
#' umxGxE_window(selDVs = "bmi", sep="", moderator = mod, mzData = mzData, dzData = dzData, 
#' 		target = 40, plotWindow = TRUE, tryHard = "yes")
#'
#' # Run with tryHard
#' umxGxE_window(selDVs = "bmi", sep="", moderator = "age", mzData = mzData, dzData = dzData)
#' umxGxE_window(selDVs="bmi", sep="", moderator="age", mzData=mzData, dzData=dzData, tryHard="yes")
#' 
#' # Run creating weighted covariance matrices (excludes missing data)
#' umxGxE_window(selDVs = "bmi", sep="", moderator= "age", mzData = mzData, dzData = dzData, 
#' 		weightCov = TRUE)
#' # This example runs multiple target moderator values
#' mxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 	target = c(39,40,50), plotWindow = TRUE)
#'
#' }
umxGxE_window <- function(selDVs = NULL, moderator = NULL, mzData = mzData, dzData = dzData, sep = NULL, weightCov = FALSE, target = NULL, width = 1, plotWindow = FALSE, tryHard = c("no", "yes", "ordinal", "search"), return = c("estimates","last_model")) {
	return  = match.arg(return)
	tryHard = match.arg(tryHard)
	
	nSib   = 2 # Number of siblings in a twin pair.
	
	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib)

	umx_check(!is.null(moderator), "stop", "Moderator must be set to the name of the moderator column, e.g, moderator = 'birth_year'")
	
	if(!is.null(sep)){
		selVars   = umx_paste_names(selDVs, sep = sep, 1:2)
	}else{
		selVars = selDVs
	}

	# TODO umxGxE_window: allow missing moderator?
	# Check DVs exists in mzData and dzData (and nothing else apart from the moderator)
	umx_check_names(c(selVars, moderator), data = mzData, die = TRUE)
	umx_check_names(c(selVars, moderator), data = dzData, die = TRUE)

	# Drop extraneous columns
	mzData = mzData[, c(selVars, moderator)]
	dzData = dzData[, c(selVars, moderator)]

	# Add zygosity column
	mzData$ZYG = "MZ";
	dzData$ZYG = "DZ"
	# If using cov.wt, remove missing
	if(weightCov){
		dz.complete = complete.cases(dzData)
		if(sum(dz.complete) != nrow(dzData)){
			message("removed ", nrow(dzData) - sum(dz.complete), " cases from DZ data due to missingness. To use incomplete data, set weightCov = FALSE")
			dzData = dzData[dz.complete, ]
		}
		mz.complete = complete.cases(mzData)
		if(sum(mz.complete) != nrow(mzData)){
			message("removed ", nrow(mzData) - sum(mz.complete), " cases from MZ data due to missingness. To use incomplete data, set weightCov = FALSE")
			mzData = mzData[mz.complete, ]
		}
	}
	# bind the MZ and DZ data into one frame
	allData = rbind(mzData, dzData)

	# Create range of moderator values to iterate over
	modVar  = allData[, moderator]
	if(any(is.na(modVar))){		
		stop("Moderator \"", moderator, "\" contains ", length(modVar[is.na(modVar)]), "NAs. This is not currently supported.\n",
			"NA found on rows", paste(which(is.na(modVar)), collapse = ", "), " of the combined data."
		)
	}

	if(!is.null(target)){
		if(any(target < min(modVar))) {
			stop("A target found below the minimum value of moderator. min(modVar) was ", min(modVar))
		} else if(any(target > max(modVar))){
			stop("target above the max value of the moderator. max(modVar) was ", max(modVar))
		} else {
			targetLevels = target
		}
	} else {
		# by default, run across each integer value of the moderator
		targetLevels = seq(min(modVar), max(modVar))
	}

	numPairs     = nrow(allData)
	moderatorSD  = sd(modVar, na.rm = TRUE)
	bw           = 2 * numPairs^(-.2) * moderatorSD *  width # -.2 == -1/5 

	tmp = rep(NA, length(targetLevels))
	out = data.frame(modLevel = targetLevels, Astd = tmp, Cstd = tmp, Estd = tmp, A = tmp, C = tmp, E = tmp)
	n   = 1
	for (i in targetLevels) {
		# i = targetLevels[1]
		message("mod = ", i)
		zx = (modVar - i)/bw
		k = (1 / (2 * pi)^.5) * exp((-(zx)^2) / 2)
		# ===========================================================
		# = Insert the weights variable into data.frames as "weight" =
		# ===========================================================
		allData$weight = k/.399
		mzData = allData[allData$ZYG == "MZ", c(selVars, "weight")]
		dzData = allData[allData$ZYG == "DZ", c(selVars, "weight")]
		if(weightCov){
			mz.wt = cov.wt(mzData[, selVars], mzData$weight)
			dz.wt = cov.wt(dzData[, selVars], dzData$weight)
			m1 = umxACE(selDVs = selDVs, sep=sep, dzData = dz.wt$cov, mzData = mz.wt$cov, numObsDZ = dz.wt$n.obs, numObsMZ = mz.wt$n.obs, autoRun = FALSE)
		} else {
			m1 = umxACE(selDVs = selDVs, sep=sep, dzData = dzData, mzData = mzData, weightVar = "weight", autoRun = FALSE)
		}
		m1 = umxRun(m1, tryHard = tryHard, summary = FALSE)
		if(plotWindow){
			plot(allData[,moderator], allData$weight) # normal-curve yumminess
			umxSummaryACE(m1)
		}
		out[n, ] = mxEval(c(i, top.a_std[1,1], top.c_std[1,1], top.e_std[1,1], top.a[1,1], top.c[1,1], top.e[1,1]), m1)
		n = n + 1
	}
	if(length(targetLevels)==1){
		# no need to plot: perhaps draw the ACE diagram?
		plot(m1)
	} else {
		# plot ACE across levels of the moderator
		ACE = c("A", "C", "E")
		# Squaring paths to produce variances
		out[,ACE] = out[,ACE]^2
		# plotting variance components
		with(out, {
			plot(A ~ modLevel, main = paste0(selDVs[1], " variance"), ylab = "Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
			lines(modLevel, C, col = 'green')
			lines(modLevel, E, col = 'blue')
			legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)

			plot(Astd ~ modLevel, main = paste0(selDVs[1], " std variance"), ylab = "Std Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
			lines(modLevel, Cstd, col = 'green')
			lines(modLevel, Estd, col = 'blue')
			legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)
		})
	}
	if(return == "last_model"){
		invisible(m1)
	} else if(return == "estimates") {
		invisible(out)
	}
}
