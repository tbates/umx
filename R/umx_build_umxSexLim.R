# oddsratio <- function(grp1= c(n, N), grp2= c(n, N)) {
# 	odds1 = odds(grp1[1], grp1[2])
# 	odds2 = odds(grp2[1], grp2[2])
# 	OR = odds1/odds2
# 	list(odds1=odds1,odds2=odds2, OR=OR)
# }
# odds <- function(n,N) {
# 	n/(N-n)
# }

#' Multivariate sex limitation twin model
#'
#' @description
#' Build a multivariate twin analysis allowing for sex limitation (factors operate differently in males 
#' vs. females) based on a correlated factors model.
#' With 5-groups of twins, this model allows for both Quantitative & Qualitative Sex-Limitation.
#' 
#' The correlation approach ensures that variable order does not affect the ability of the model
#' to account for DZOS data.
#' 
#' @details
#' 
#' **A or C**
#' Due to limitations on the degrees of freedom allowed by the twin model, we can model 
#' qualitative sex differences for only one of A or C at a time.
#' 
#' 
#' ** 1. Nonscalar Sex Limitation **
#' Allow Quantitative Sex Differences and, for one of `A or C` only, Qualitative Sex Differences.
#' Male and female paths, plus Ra, Rc and Re between variables for males and for females.
#' Male-Female correlations in DZO group between `A or C` factors and Rao (or Rco) free.
#' Ra constrained across male, female, and opposite-sex pairs.
#'
#' **2. Scalar Sex Limitation**
#'
#' Quantitative Sex Differences but NO Qualitative Sex Differences
#' Male and female paths, but one set of Ra, Rc and Re between variables (same for males and females)
#' Restrictions: Assumes means and variances can be equated across birth order within zygosity groups.
#'
# Equate the R[ace]f and R[ace]m matrix labels by deleting "f" and "m"
# m2 = umxModify(m1, regex = "^(R[ace])[f|m|o]_", newlabels = "\\1_", name = "Homogeneity", autoRun= FALSE)
#
# m2 = mxModel(m2, mxModel(m2$top,
# 	umxMatrix("Rao", "Stand", nrow= nVar, values= .2, baseName= "Ra", lbound= -1, ubound= 1),
# 	umxMatrix("Rco", "Stand", nrow= nVar, values= .2, baseName= "Rc", lbound= -1, ubound= 1)
# ))
#
# m2 = omxAssignFirstParameters(m2)
#'
#' @param name    The name of the model (Default = "sexlim")
#' @param selDVs  BASE NAMES of the variables in the analysis. You MUST provide sep.
#' @param sep Suffix used for twin variable naming. Allows using just the base names in selVars.
#' @param mzmData Dataframe containing the MZ male data.
#' @param dzmData Dataframe containing the DZ male data.
#' @param mzfData Dataframe containing the MZ female data.
#' @param dzfData Dataframe containing the DZ female data.
#' @param dzoData Dataframe containing the DZ opposite-sex data (be sure and get in right order).
#' @param A_or_C  Whether to model sex-limitation on A or on C. (Defaults to "A").
#' @param sexlim  Which model type: "Nonscalar" (default), "Scalar", or "Homogeneity".
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param tryHard optionally tryHard (default 'no' uses normal mxRun). c("no", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch")
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - \code{\link{mxModel}} of subclass mxModel.CFSexLim
#' @export
#' @family Twin Modeling Functions
#' @references - Neale et al. (2006). 
#' Multivariate genetic analysis of sex-lim and GxE interaction.
#' \emph{Twin Research & Human Genetics}, \bold{9}, pp. 481--489.
#' @md
#' @examples
#  # =============================================
#  # = Run Qualitative Sex Differences ACE model =
#  # =============================================
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' \dontrun{
#' require(umx)
#' data("us_skinfold_data")
#' # Rescale vars
#' us_skinfold_data[, c('bic_T1', 'bic_T2')] = us_skinfold_data[, c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[, c('tri_T1', 'tri_T2')] = us_skinfold_data[, c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[, c('caf_T1', 'caf_T2')] = us_skinfold_data[, c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[, c('ssc_T1', 'ssc_T2')] = us_skinfold_data[, c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[, c('sil_T1', 'sil_T2')] = us_skinfold_data[, c('sil_T1', 'sil_T2')]/5
#'
#' # Data for each of the 5 twin-type groups
#' mzmData = subset(us_skinfold_data, zyg == 1)
#' mzfData = subset(us_skinfold_data, zyg == 2)
#' dzmData = subset(us_skinfold_data, zyg == 3)
#' dzfData = subset(us_skinfold_data, zyg == 4)
#' dzoData = subset(us_skinfold_data, zyg == 5)
#'
#' # ==========================
#' # = Run univariate example =
#' # ==========================
#' 
#' m1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", tryHard = "mxTryHard",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#'
#' m2 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", tryHard = "mxTryHard", sexlim = "Scalar",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#'
#' umxCompare(m1, m2)
#'
#' # =====================
#' # = Bivariate example =
#' # =====================
#' m1 = umxSexLim(selDVs = c("bic", "tri"), sep = "_T", A_or_C = "A", autoRun = FALSE,
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' m1 = mxTryHard(m1)
#' 
#' # ============================
#' # = 3. Scalar Sex Limitation =
#' # ============================
#'
#' m2 = umxModify(m1, regex = "^R([ace])[f|m]_", newLabels = "r\\1_", 
#'   name = "Homogeneity", comparison = TRUE)
#' 
#'# =============================
#'# = Run multi-variate example =
#'# =============================
#' # Variables for Analysis
#' selDVs = c('ssc','sil','caf','tri','bic')
#' selDVs = c('ssc','tri','bic')
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", tryHard = "mxTryHard",
#'		mzmData = mzmData, dzmData = dzmData, 
#'    mzfData = mzfData, dzfData = dzfData, dzoData = dzoData
#')
#' m1 = mxTryHard(m1)

#' m2 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", sexlim = "Nonscalar",
#' 	tryHard = "mxTryHard",
#'		mzmData = mzmData, dzmData = dzmData, 
#'    mzfData = mzfData, dzfData = dzfData, dzoData = dzoData
#')
#'
#' # umxSummary(m1)
#' # summary(m1)
#' # summary(m1)$Mi
#' }
umxSexLim <- function(name = "sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, sep = NA, A_or_C = c("A", "C"), sexlim = c("Nonscalar", "Scalar", "Homogeneity"), dzAr = .5, dzCr = 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL){
	A_or_C  = match.arg(A_or_C)
	tryHard = match.arg(tryHard)
	sexlim  = match.arg(sexlim)

	# ================================
	# = 1. Non-scalar Sex Limitation =
	# ================================
	# Quantitative & Qualitative Sex Differences for A (or C)
	# Rc (or Ra) constrained across male/female and opposite sex
	# Male and female paths, plus Ra, Rc and Re between variables for males and females
	# Male-Female correlations in DZO group between A factors (Rao) FREE
	
	# Correlated factors sex limitation

	nSib = 2 # Number of siblings in a twin pair
	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzmData, mzData = mzmData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)

	# Auto-name ADE version
	if(dzCr == .25 && name == "sexlim"){ name = "sexlimADE" }
	nVar    = length(selDVs)
	selVars = umx_paste_names(selDVs, sep= sep, suffixes = 1:2)

	# Check names, and drop unused columns from data
	umx_check_names(selVars, data = mzmData, die = TRUE); mzmData = mzmData[, selVars]
	umx_check_names(selVars, data = dzmData, die = TRUE); dzmData = dzmData[, selVars]
	umx_check_names(selVars, data = mzfData, die = TRUE); mzfData = mzfData[, selVars]
	umx_check_names(selVars, data = dzfData, die = TRUE); dzfData = dzfData[, selVars]
	umx_check_names(selVars, data = dzoData, die = TRUE); dzoData = dzoData[, selVars]

	# Start means at actual means of some group 
	obsMean = umx_means(mzmData[, selVars[1:nVar], drop = FALSE])
	varStarts = umx_var(mzmData[, selVars[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")

	if(nVar == 1){
		varStarts = sqrt(varStarts)/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
	}
	varStarts = matrix(varStarts, nVar, nVar)
	

	# Helpful dimnames for Algebra-based Estimates and Derived Variance Component output (see "VarsZm") 
	colZm = paste0(selDVs, rep(c('Am', 'Cm', 'Em'), each = nVar))
	colZf = paste0(selDVs, rep(c('Af', 'Cf', 'Ef'), each = nVar))

	# Make Rao and Rco matrices 
	if(A_or_C == "A"){
			# Quantitative & Qualitative Sex Differences for A (Ra is Full, Rc is symm)
			# (labels trimmed to Ra at end)
			# TODO: Check Stand (symmetric with 1's on diagonal) OK (was Symm + fix diag @1)
			# TODO: Not sure why, as Symm can become Full... so can't turn Ao into Co...
			Rao = umxMatrix("Rao", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound= -1, ubound= 1)
			Rco = umxMatrix("Rco", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound= -1, ubound= 1)
	} else if (A_or_C == "C"){
			# Quantitative & Qualitative Sex Differences for C (Ra is symm, Rc is Full)
			Rao = umxMatrix("Rao", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound= -1, ubound= 1)
			Rco = umxMatrix("Rco", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound= -1, ubound= 1)
	}

	model = mxModel(name,
		mxModel("top",
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
				
			# Path Coefficient matrices a, c, and e for males and females 
			umxMatrix("am", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("cm", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("em", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("af", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("cf", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("ef", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),

			# Matrices for Correlation Coefficients within/across Individuals 
			# Stand = symmetric with 1's on diagonal
			# NOTE: one of # (Rc[fmo]) or (Ra[fmo]) are equated (labeled "rc") (bottom of script)
			umxMatrix("Ram", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Raf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1), 
			umxMatrix("Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rem", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Ref", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

			# Rao_and_Rco_matrices
			Rao, Rco,

			# Algebra Male and female variance components 
			mxAlgebra(name = "Am", Ram %&% am),
			mxAlgebra(name = "Cm", Rcm %&% cm),
			mxAlgebra(name = "Em", Rem %&% em),

			mxAlgebra(name = "Af", Raf %&% af),
			mxAlgebra(name = "Cf", Rcf %&% cf),
			mxAlgebra(name = "Ef", Ref %&% ef),

			# Opposite-Sex parameters: Rao, Rco, Amf, Cmf 
			mxAlgebra(name = "Amf", am %*% (Rao) %*% t(af)),
			mxAlgebra(name = "Cmf", cm %*% (Rco) %*% t(cf)),

			# Constrain the 6 R*(f|m) Eigen values to be positive 
			umxMatrix("pos1by6", "Full", nrow = 1, ncol = 6, free = FALSE, values = .0001),
			mxAlgebra(name = "minCor", cbind(
				min(eigenval(Ram)), min(eigenval(Rcm)), min(eigenval(Rem)),
			  	min(eigenval(Raf)), min(eigenval(Rcf)), min(eigenval(Ref)))
			),
			mxConstraint(name = "Keep_it_Positive", minCor > pos1by6),

			# Algebra for Total variances and standard deviations (of diagonals) 
			umxMatrix("I", "Iden", nrow = nVar),
			mxAlgebra(name = "Vm", Am + Cm + Em),
			mxAlgebra(name = "Vf", Af + Cf + Ef),
			mxAlgebra(name = "iSDm", solve(sqrt(I * Vm))),
			mxAlgebra(name = "iSDf", solve(sqrt(I * Vf))),

			# Algebras for Parameter Estimates and Derived Variance Components.
			mxAlgebra(name = "VarsZm", cbind(Am/Vm, Cm/Vm, Em/Vm), dimnames = list(selDVs, colZm)),
			mxAlgebra(name = "CorsZm", cbind(Ram  , Rcm  , Rem  ), dimnames = list(selDVs, colZm)),

			mxAlgebra(name = "VarsZf", cbind(Af/Vf, Cf/Vf, Ef/Vf), dimnames = list(selDVs, colZf)),
			mxAlgebra(name = "CorsZf", cbind(Raf  , Rcf  , Ref)  , dimnames = list(selDVs, colZf)),

			# Matrix & Algebra for expected Mean Matrices in MZ & DZ twins (done!!).
			umxMatrix("expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "_mean_m")),
			umxMatrix("expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "_mean_f")),
			umxMatrix("expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, rep(c("_mean_m", "_mean_f"), each = nVar))),

			# Matrix & Algebra for expected Variance/Covariance Matrices in MZ & DZ twins.
			mxAlgebra(name = "expCovMZm", rbind(cbind(Vm,          Am  + Cm)          , cbind(         Am +          Cm, Vm))),
			mxAlgebra(name = "expCovDZm", rbind(cbind(Vm, dzAr %x% Am  + dzCr %x% Cm) , cbind(dzAr %x% Am + dzCr %x% Cm, Vm))),
			mxAlgebra(name = "expCovMZf", rbind(cbind(Vf,          Af  + Cf)          , cbind(         Af +          Cf, Vf))),
			mxAlgebra(name = "expCovDZf", rbind(cbind(Vf, dzAr %x% Af  + dzCr %x% Cf) , cbind(dzAr %x% Af + dzCr %x% Cf, Vf))),
			mxAlgebra(name = "expCovDZo", rbind(cbind(Vm, dzAr %x% Amf + dzCr %x% Cmf), cbind(dzAr %x% t(Amf) +  t(Cmf), Vf)))
		), # end of top

		# 5 group models 
		mxModel("MZm",
			mxExpectationNormal("top.expCovMZm", means = "top.expMeanGm", dimnames = selVars),
			mxFitFunctionML(), mxData(mzmData, type = "raw")
		),
		mxModel("DZm",
			mxExpectationNormal("top.expCovDZm", means = "top.expMeanGm", dimnames = selVars),
			mxFitFunctionML(), mxData(dzmData, type = "raw")
		),
		mxModel("MZf",
			mxExpectationNormal("top.expCovMZf", means = "top.expMeanGf", dimnames = selVars),
			mxFitFunctionML(), mxData(mzfData, type = "raw")
		),
		mxModel("DZf",
			mxExpectationNormal("top.expCovDZf", means = "top.expMeanGf", dimnames = selVars),
			mxFitFunctionML(), mxData(dzfData, type = "raw")
		),
		mxModel("DZo",
			mxExpectationNormal("top.expCovDZo", means = "top.expMeanGo", dimnames = selVars),
			mxFitFunctionML(), mxData(dzoData, type = "raw")
		),
		mxFitFunctionMultigroup(c("MZf", "DZf", "MZm", "DZm", "DZo"))
	) # end supermodel

	if(sexlim == "Nonscalar"){
		# =================================
		# = Non-scalar Sex Limitation (A) =
		# =================================
		# TODO: Quantitative & Qualitative Sex Differences for A || C.
		# Male and female paths, plus Ra, Rc and Re between variables for males and females
		# Male-Female correlations in DZO group between A factors Rao FREE
		# Rc constrained across male, female and opposite-sex pairs

		# Non-scalar (full) sex-lim label tweaks
		if(A_or_C == "A"){
			# Convert Rc[fmo] => "Rc"		
			if("^Rc[fmo](_.*)$" %in% umxGetParameters(model)){
				model = umxModify(model, regex = "^Rc[fmo](_.*)$", newlabels = "Rc\\1", autoRun=FALSE)
			}
		}else if (A_or_C == "C"){
			# Convert Ra[fmo] => "Ra"
			if("^Ra[fmo](_.*)$" %in% umxGetParameters(model)){
				model = umxModify(model, regex = "^Ra[fmo](_.*)$", newlabels = "Ra\\1", autoRun=FALSE)
			}
		}
	} else if (sexlim == "Scalar"){
		# =========================
		# = Scalar Sex Limitation =
		# =========================
		# Quantitative Sex Differences. NO Qualitative Sex Differences.
		# Male and female paths, Only one set of Ra, Rc and Re between variables 
		#  (i.e., same correlations for males and for females)

		# TODO: This is quite complex, so instead of a "reduction" make it an operation which `umxSexLim` can perform
		# 1. Equate f and m correlations by trimming the sex suffix off R[ace]f and R[ace]m matrix labels (i.e., deleting "f" and "m")
		model = umxModify(model, regex = "^(R[ace])[f|m|o]_", newlabels = "\\1_", name = "Scalar", autoRun = FALSE)

		# 2. Replace opposite sex matrices (top.Rao and top.Rco) with labels that have no sex suffix(to match R[ace] same-sex)
		# TODO: Make this a regex also? "Rao_" --> "Ra_" "Rco_" --> "Rc_"
		# Both standardized
		model = mxModel(model, mxModel(model$top,
			umxMatrix("Rao", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Ra", lbound = -1, ubound = 1),
			umxMatrix("Rco", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Rc", lbound = -1, ubound = 1))
		)
	}else{
		# "Homogeneity"
		# Just advise people to do ACE?
	}

	model = omxAssignFirstParameters(model)

	# TODO: umxSexLim equate means would be expMeanGm, expMeanGf, expMeanGo
	model = as(model, "MxModelSexLim") # Set class so umxSummary, plot, etc. work.
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	invisible(model)
}

#' Shows a compact, publication-style, summary of a umx Sex Limitation model
#'
#' Summarize a fitted Cholesky model returned by \code{\link{umxSexLim}}. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. The report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models summary here: \code{\link{umxSummary}}.
#'
#' @aliases umxSummary.MxModelSexLim
#' @param model a \code{\link{umxSexLim}} model to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If "html", then open an html table of the results
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - \code{\link{umxACE}} 
#' @references - \url{https://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' \dontrun{
#' # =================================================================
#' # = NOT WORKING YET! Should be good to use for Boulder/March 2020 =
#' # =================================================================
#'
#' # =============================================
#' # = Run Qualitative Sex Differences ACE model =
#' # =============================================
#'
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' require(umx)
#' umx_set_optimizer("SLSQP")
#' data("us_skinfold_data")
#' # rescale vars
#' us_skinfold_data[, c('bic_T1', 'bic_T2')] = us_skinfold_data[, c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[, c('tri_T1', 'tri_T2')] = us_skinfold_data[, c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[, c('caf_T1', 'caf_T2')] = us_skinfold_data[, c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[, c('ssc_T1', 'ssc_T2')] = us_skinfold_data[, c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[, c('sil_T1', 'sil_T2')] = us_skinfold_data[, c('sil_T1', 'sil_T2')]/5
#'
#' # Variables for Analysis
#' selDVs = c('ssc','sil','caf','tri','bic')
#' # Data for each of the 5 twin-type groups
#' mzmData = subset(us_skinfold_data, zyg == 1)
#' mzfData = subset(us_skinfold_data, zyg == 2)
#' dzmData = subset(us_skinfold_data, zyg == 3)
#' dzfData = subset(us_skinfold_data, zyg == 4)
#' dzoData = subset(us_skinfold_data, zyg == 5)
#'
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", tryHard = "mxTryHard",
#' 	mzmData = mzmData, dzmData = dzmData, 
#' 	mzfData = mzfData, dzfData = dzfData, 
#' 	dzoData = dzoData
#' )
#' umxSummary(m1, file = NA);
#' umxSummarySexLim(m1, file = "name", std = TRUE)
#' stdFit = umxSummarySexLim(m1, returnStd = TRUE);
#' }
umxSummarySexLim <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	message("umxSummarySexLim is a beta feature. Some things are broken. If any desired stats are not presented, let me know what's missing")
	report = match.arg(report)
	# Depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummarySexLim(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	umx_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	selVars = model$MZm$expectation$dims
	selDVs  = dimnames(model$top$VarsZm$result)[[1]]
	nVar    = length(selDVs)
	umx_msg(nVar)
	# umx_msg(selDVs) # [1] "ssc_T1" "sil_T1" "caf_T1" "tri_T1" "bic_T1" "ssc_T2" "sil_T2" "caf_T2" "tri_T2" "bic_T2"

	# 3-wide
	tmpm = model$top$CorsZm$result
	tmpf = model$top$CorsZf$result
	
	# round(m1$VarsZm$result,4); round(m1$CorsZm$result,4)
	# round(m1$VarsZf$result,4); round(m1$CorsZf$result,4)
	
	# If univariate, there are no correlations...
	if(nVar > 1){
		message("Genetic Factor Correlations (male lower triangle, female upper)")
		RAm = tmpm[1:nVar, 1:nVar]
		RAf = tmpf[1:nVar, 1:nVar]
		RAboth = RAm;
		RAboth[upper.tri(RAboth)] = RAf[upper.tri(RAf)]
		dimnames(RAboth)[[2]] = dimnames(RAboth)[[1]]
		umxAPA(RAboth)

		message("C Factor Correlations (male lower triangle, female upper)")
		RCm = tmpm[1:nVar, (nVar + 1):(nVar * 2)]
		RCf = tmpf[1:nVar, (nVar + 1):(nVar * 2)]
		RCboth = RCm
		RCboth[upper.tri(RCboth)] = RCf[upper.tri(RCf)]
		dimnames(RAboth)[[2]] = dimnames(RAboth)[[1]]
		umxAPA(RAboth)

		message("E Factor Correlations (male lower triangle, female upper)")
		REm = tmpm[1:nVar, (nVar * 2 + 1):(nVar * 3)]
		REf = tmpf[1:nVar, (nVar * 2 + 1):(nVar * 3)]
		REboth = REm
		REboth[upper.tri(REboth)] = REf[upper.tri(REf)]
		dimnames(RAboth)[[2]] = dimnames(RAboth)[[1]]
		umxAPA(REboth)
	}

	if(std){
		message("Standardized solution")
		# nb: VarsZm = cbind(Am/Vm, Cm/Vm, Em/Vm), dimnames = list(selDVs, colZm)),
		# = nVar * nVar*3 = 1r * 3c for nVar = 1
		# 3-wide
		tmpm = model$top$VarsZm$result
		tmpf = model$top$VarsZf$result
		Am = diag(as.matrix(tmpm[1:nVar, 1:nVar]))
		Cm = diag(as.matrix(tmpm[1:nVar, (nVar + 1):(nVar * 2)]))
		Em = diag(as.matrix(tmpm[1:nVar, (nVar * 2 + 1):(nVar * 3)]))
		Af = diag(as.matrix(tmpf[1:nVar, 1:nVar]))
		Cf = diag(as.matrix(tmpf[1:nVar, (nVar + 1):(nVar * 2)]))
		Ef = diag(as.matrix(tmpf[1:nVar, (nVar * 2 + 1):(nVar * 3)]))
		# Estimates (will be printed below...)
		Estimates = data.frame(cbind(Am, Cm, Em, Af, Cf, Ef))
	} else {
		# TODO sexlim: Check raw solution
		# Am, Cm, Em, dimnames = list(selDVs, colZm)),
		Am = diag(as.matrix(model$top$Am$result))
		Cm = diag(as.matrix(model$top$Cm$result))
		Em = diag(as.matrix(model$top$Em$result))
		Af = diag(as.matrix(model$top$Af$result))
		Cf = diag(as.matrix(model$top$Cf$result))
		Ef = diag(as.matrix(model$top$Ef$result))
		# Estimates (will be printed below...)
		Estimates = data.frame(cbind(Am, Cm, Em, Af, Cf, Ef))
	}

	if(model$top$dzCr$values == .25){
		names(Estimates) = paste0(rep(c("a", "d", "e")), rep(c("m","f"), each = 3))
	} else {
		# am cm em af cf ef
		names(Estimates) = paste0(rep(c("a", "c", "e")), rep(c("m","f"), each = 3))
	}
	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = FALSE, sortableDF = TRUE); 
		umx_open("tmp.html")
	}
	
	if(extended == TRUE) {
		message("TODO: Not sure what extra information would be helpful (if any)")
	}

	hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO umxACE CI code: Need to refactor into some function calls... and then add to umxSummaryIP and CP
			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
			# discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			# These can be names ("top.a_std[1,1]") or labels ("a11")
			# imxEvalByName finds them both
			# outList = c();
			# for(aName in row.names(CIlist)) {
			# 	outList <- append(outList, imxEvalByName(aName, model))
			# }
			# # Add estimates into the CIlist
			# CIlist$estimate = outList
			# reorder to match summary
			CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the CI results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_CI = c_CI = e_CI = matrix(NA, rows, cols)

			# iterate over each CI
			labelList = imxGenerateLabels(model)			
			rowCount = dim(CIlist)[1]
			# return(CIlist)
			for(n in 1:rowCount) { # n = 1
				thisName = row.names(CIlist)[n] # thisName = "a11"
					# convert labels to [bracket] style
					if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString = paste0(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]")

				if(grepl("^a", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
				}
			}
			# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
			# TODO umxSummarySexLim: Add option to use mxSE
			# print(a_CI)
			# print(c_CI)
			# print(e_CI)

			message("TODO: umxSummary.MxModel.SexLim CI report not yet implemented - use summary")
			# Estimates = data.frame(cbind(a_CI, c_CI, e_CI), row.names = rowNames, stringsAsFactors = FALSE)
			# names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			# Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
			# if(report == "html"){
				# depends on R2HTML::HTML
				# R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T);
				# umx_open("tmpCI.html")
			# }
			# CI_Fit = model
			# CI_Fit$top$a$values = a_CI
			# CI_Fit$top$c$values = c_CI
			# CI_Fit$top$e$values = e_CI
		} # end Use CIs
	} # end list catcher?

	if(!is.na(file)) {
		# message("making dot file")
		if(hasCIs & CIs){
			# TODO turn plot of CI_Fit back on
			# umxPlotSexLim(CI_Fit, file = file, std = FALSE)
		} else {
			message("plot not implemented yet for sex lim")
			# umxPlotSexLim(model, file = file, std = std)
		}
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		umx_standardize_ACE(model)
	}
}

#' @export
umxSummary.MxModelSexLim <- umxSummarySexLim
