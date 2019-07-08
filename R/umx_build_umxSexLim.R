#' Multivariate sex limitation twin model
#'
#' @description
#' Multivariate twin analysis allowing for sex limitation (factors operate differently in males 
#' vs. females) based on a correlated factors model. With 5-groups of twins, this model allows
#' for both Quantitative & Qualitative Sex-Limitation.
#'
#' *Quantitative* differences refer to different amounts of phenotypic variance produced by 
#' the same A, C, or E components when operating in one sex compared to the other sex.
#' 
#' *Qualitative* differences refer to phenotypic variance attributable to an A, C, or E
#' component which operates in one sex one but not in the other.
#' 
#' The correlation approach ensures that variable order does not affect the ability
#' of the model to account for DZOS data.
#' 
#' @details
#' 
#' **A or C**
#' 
#' Due to limitations on the degrees of freedom allowed by the twin model, we can model 
#' qualitative sex differences for only one of A or C at a time.
#' 
#' 
#' ** 1. Nonscalar Sex Limitation **
#' 
#' Allow quantitative (distinct male and female paths) and qualitative sex differences 
#' on A or C. Allows distinct between variable correlations (`Ra`, `Rc` and `Re`)
#' for males and for females. Male-Female correlations also free (`Rao` or `Rco` free in DZO group).
#'
#' **2. Scalar Sex Limitation**
#'
#' Quantitative sex differences only (distinct Male and female paths).
#' Just one set of Ra, Rc and Re between variables (same for males and females)
#' 
#' **3. Homogeneity**
#'
#' This is the model assumed by the basic ACE model: equal variance components in both sexes. 
#' Different means may be allowed for males and females.
#' 
#' *notes*:
#' There is a half-way house model of heterogeneity in which a, c, and e components are scaled by a 
#' scalar constant in one sex. # TODO sexlim: This k scalar heterogeneity model is not yet implemented in umx.
#' 
#' *General restrictions*: Assumes means and variances can be equated across birth order within zygosity groups.
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
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - [mxModel()] of subclass mxModel.CFSexLim
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
#' umxSummarizeTwinData(us_skinfold_data, selVars="bic",zyg="zyg", sep="_T",
#' 		MZFF=2, DZFF=4, MZMM=1, DZMM=3, DZOS=5
#' )
#'
#' # ==========================
#' # = Run univariate example =
#' # ==========================
#' 
#' m1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", tryHard = "yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#'
#' # Drop qualitative sex limitation
#  Distinct af and am (and c and e), but shared Ra (and Rc and Re) between variables (same for males and females)
#' m1a = umxModify(m1, regex = "^Rao_", value=1, name = "no_qual", comparison = TRUE)
#'
#'
#' # Equate a, ac, and try ace across m & f in scalar model
#' m1b = umxModify(m1a, regex = "^a[fm]_", newlabels="a_", name = "eq_a_no_qual", comparison = TRUE)
#' m1c = umxModify(m1b, regex = "^c[fm]_", newlabels="c_", name = "eq_ac_no_qual", comparison = TRUE)
#' m1d = umxModify(m1c, regex = "^e[fm]_", newlabels="e_", name = "eq_ace_no_qual", comparison = TRUE)
#' umxCompare(m1, c(m1a, m1b, m1c, m1d))
#'
#' # ============================
#' # = Scalar Sex Limitation =
#' # ============================
#'
#' m2 = umxSexLim(selDVs = "bic", sep = "_T", sexlim = "Scalar", tryHard = "yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#'
#' # Show our manual drop of qualitative is the same as umxSexLim with sexlim= "scalar"s
#' umxCompare(m1a, m2)
#'
#' # ===============
#' # = Homogeneity =
#' # ===============
#'
#' m3 = umxSexLim(selDVs = "bic", sep = "_T", sexlim = "Homogeneity", tryHard = "yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' umxCompare(m1, c(m2, m3))
#'
#' # ===========================================
#' # = Bivariate example with manual reduction =
#' # ===========================================
#' m1 = umxSexLim(selDVs = c("bic", "tri"), sep = "_T", A_or_C = "A", tryHard="yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' 
#' # Scalar sex limitation (same correlation among components for m and f)
#' m2 = umxSexLim(selDVs = c("bic", "tri"), sep = "_T", 
#' 	A_or_C = "A", tryHard="yes", sexlim="Scalar",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' # Drop qualitative sex limitation
#' #  Distinct af and am (& c & e), but shared Ra (& Rc & Re) between variables
#' #  	i.e., same correlations for males and females.
#' m1a = umxModify(m1 , regex = "^Ra[mfo]_", newlabels="^Ra_", name = "no_qual_a", comparison = TRUE)
#' m1b = umxModify(m1a, regex = "^Rc[mfo]_", newlabels="^Rc_", name = "no_qual_ac", comparison = TRUE)
#' m1c = umxModify(m1b, regex = "^Re[mfo]_", newlabels="^Re_", name = "no_qual_ace", comparison = TRUE)
#' umxCompare(m1, c(m1a, m1b, m1c, m2))
#'
#' # In one smart regular expression
#' m2 = umxModify(m1, regex = "^R([ace])[fmo]_", newlabels = "R\\1_", 
#'   name = "scalar", comparison = TRUE)
#'
#' # Equate a, ac, and try ace across m & f in scalar model
#' m2a = umxModify(m2 , regex = "^a[fm]_", newlabels="a_", name = "eq_a_no_qual"  , comparison = TRUE)
#' m2b = umxModify(m2a, regex = "^c[fm]_", newlabels="c_", name = "eq_ac_no_qual" , comparison = TRUE)
#' m2c = umxModify(m2b, regex = "^e[fm]_", newlabels="e_", name = "eq_ace_no_qual", comparison = TRUE)
#' umxCompare(m1, c(m1a, m1b, m1c, m1d))
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
#'
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
umxSexLim <- function(name = "sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, sep = NA, A_or_C = c("A", "C"), sexlim = c("Nonscalar", "Scalar", "Homogeneity"), dzAr = .5, dzCr = 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL){
	message("umxSexLim is a beta feature. Some things are broken. If any desired stats are not presented, let me know what's missing")
	A_or_C  = match.arg(A_or_C)
	sexlim  = match.arg(sexlim)
	tryHard = match.arg(tryHard)
	if(tryHard == "yes"){
		tryHard = "mxTryHard"
	}

	# ================================
	# = 1. Non-scalar Sex Limitation =
	# ================================
	# Quantitative & Qualitative Sex Differences for A (or C)
	# * Distinct male and female paths (i.e., quantitative differences)
	# * Distinct between-variable Ra, Rc and Re for males and females
	# * Male-Female correlations in DZO group between A (or C) factors (Rao/Rco) FREE
	

	nSib = 2 # Number of siblings in a twin pair
	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzmData, mzData = mzmData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)

	# Auto-name ADE version
	if(name == "sexlim"){
		if(dzCr == .25){
			name = paste0(sexlim, "ADE") # c("Nonscalar", "Scalar", "Homogeneity")
		}else{
			name = paste0(sexlim) # c("Nonscalar", "Scalar", "Homogeneity")
		}
	}
	nVar = length(selDVs)
	selVars = umx_paste_names(selDVs, sep= sep, suffixes = 1:2)

	# Check names, and drop unused columns from data
	umx_check_names(selVars, data = mzmData, die = TRUE); mzmData = mzmData[, selVars]
	umx_check_names(selVars, data = dzmData, die = TRUE); dzmData = dzmData[, selVars]
	umx_check_names(selVars, data = mzfData, die = TRUE); mzfData = mzfData[, selVars]
	umx_check_names(selVars, data = dzfData, die = TRUE); dzfData = dzfData[, selVars]
	umx_check_names(selVars, data = dzoData, die = TRUE); dzoData = dzoData[, selVars]

	# Start means at actual means of some group 
	obsMean   = umx_means(mzmData[, selVars[1:nVar], drop = FALSE])
	varStarts = umx_var(mzmData[, selVars[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")

	if(nVar == 1){
		varStarts = sqrt(varStarts)/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
	}
	varStarts = matrix(varStarts, nVar, nVar)

	# Make Rao and Rco matrices 
	if(A_or_C == "A"){
		# Quantitative & Qualitative Sex Differences for A (Ra is Full, Rc is symm)
		# (labels trimmed to Ra at end)
		# TODO: Check Stand (symmetric with 1's on diagonal) OK (was Symm + fix diag @1)
		# 	Not sure why, as Symm can't become Full... so can't turn Ao into Co...
		Rao = umxMatrix("Rao", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound= -1, ubound= 1)
		Rco = umxMatrix("Rco", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound= -1, ubound= 1)
	} else if (A_or_C == "C"){
		# Quantitative & Qualitative Sex Differences for C (Ra is symm, Rc is Full)
		Rao = umxMatrix("Rao", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound= -1, ubound= 1)
		Rco = umxMatrix("Rco", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound= -1, ubound= 1)
	}

	model = mxModel(name,
		mxModel("top",
			# Make the A and C constants into matrices
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
			# NOTE: one of # (Rc[fmo]) or (Ra[fmo]) must be equated (e.g. labeled "Rc") (bottom of script)
			umxMatrix("Ram", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rem", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

			umxMatrix("Raf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1), 
			umxMatrix("Ref", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

			# Add opposite-sex correlation matrices (Rao & Rco matrices)
			Rao, Rco,

			# Algebras for Male and female variance components
				# %&% pre- and post-multiplies,
				# so (Ram %&%  am) == ("am %*% Ram %*%  am")
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

			# Algebra for Total variances and standard deviations (on diagonals) 
			umxMatrix("I", "Iden", nrow = nVar),
			mxAlgebra(name = "Vm", Am + Cm + Em, list(selDVs, selDVs)),
			mxAlgebra(name = "Vf", Af + Cf + Ef, list(selDVs, selDVs)),
			# not currently used
			mxAlgebra(name = "iSDm", solve(sqrt(I * Vm))),
			mxAlgebra(name = "iSDf", solve(sqrt(I * Vf))),

			mxAlgebra(name = "AmStd", Am/Vm, dimnames = list(selDVs, paste0(selDVs, "AmStd"))),
			mxAlgebra(name = "CmStd", Cm/Vm, dimnames = list(selDVs, paste0(selDVs, "CmStd"))),
			mxAlgebra(name = "EmStd", Em/Vm, dimnames = list(selDVs, paste0(selDVs, "EmStd"))),

			mxAlgebra(name = "AfStd", Af/Vf, dimnames = list(selDVs, paste0(selDVs, "AfStd"))),
			mxAlgebra(name = "CfStd", Cf/Vf, dimnames = list(selDVs, paste0(selDVs, "CfStd"))),
			mxAlgebra(name = "EfStd", Ef/Vf, dimnames = list(selDVs, paste0(selDVs, "EfStd"))),

			# Matrix & Algebra for expected Mean Matrices in MZ & DZ twins (done!!).
			umxMatrix("expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "_mean_m")),
			umxMatrix("expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "_mean_f")),
			umxMatrix("expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, rep(c("_mean_m", "_mean_f"), each = nVar))),

			# Matrix & Algebra for expected Variance/Covariance Matrices in MZ & DZ twins.
			mxAlgebra(name = "expCovMZm", rbind(cbind(Vm,          Am  + Cm)          , cbind(         Am +          Cm, Vm))),
			mxAlgebra(name = "expCovDZm", rbind(cbind(Vm, dzAr %x% Am  + dzCr %x% Cm ), cbind(dzAr %x% Am + dzCr %x% Cm, Vm))),
			mxAlgebra(name = "expCovMZf", rbind(cbind(Vf,          Af  + Cf)          , cbind(         Af +          Cf, Vf))),
			mxAlgebra(name = "expCovDZf", rbind(cbind(Vf, dzAr %x% Af  + dzCr %x% Cf ), cbind(dzAr %x% Af + dzCr %x% Cf, Vf))),
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
	} else if (sexlim %in%  c("Scalar", "Homogeneity")){
		# =========================
		# = Scalar Sex Limitation =
		# =========================
		# Quantitative Sex Differences (Male and female paths).
		# NO Qualitative Sex Differences (only one set of Ra, Rc and Re between variables)
		#  (i.e., same correlations for males and for females)

		# As this modification is somewhat complex, instead of a "reduction" I've made it an
		# operation `umxSexLim` can perform on its own with sexlim = "Scalar".

		# 1. Equate f and m between-variable correlations by trimming the sex suffix off
		#    R[ace]f and R[ace]m matrix labels (i.e., deleting "f" and "m")
		model = umxModify(model, regex = "^(R[ace])[f|m|o]_", newlabels = "\\1_", autoRun = FALSE)

		# 2. Replace opposite sex matrices (top.Rao and top.Rco) with labels that have no sex suffix 
		#    (to match R[ace] same-sex) Both standardized
		# TODO: Make this a regex also? "Rao_" --> "Ra_" "Rco_" --> "Rc_"
		model = mxModel(model, mxModel(model$top,
			umxMatrix("Rao", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Ra", lbound = -1, ubound = 1),
			umxMatrix("Rco", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Rc", lbound = -1, ubound = 1))
		)
		if(sexlim == "Homogeneity"){
			# Equate [ace] across m & f in scalar model
			model = umxModify(model, regex = "^a[fm]_", newlabels="a_", autoRun = FALSE)
			model = umxModify(model, regex = "^c[fm]_", newlabels="c_", autoRun = FALSE)
			model = umxModify(model, regex = "^e[fm]_", newlabels="e_", autoRun = FALSE)
			# TODO: Implement transforms of correlated factors / factanal on factors...
		}
	}

	model = omxAssignFirstParameters(model)

	# TODO: umxSexLim equate means would be expMeanGm, expMeanGf, expMeanGo
	model = as(model, "MxModelSexLim") # Set class so umxSummary, plot, etc. work.
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	invisible(model)
}

#' Shows a compact, publication-style, summary of a umx Sex Limitation model
#'
#' Summarize a fitted Cholesky model returned by [umxSexLim()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. The report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models summary here: [umxSummary()].
#'
#' @aliases umxSummary.MxModelSexLim
#' @param model a [umxSexLim()] model to summarize
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
#' @return - optional [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - [umxSexLim()] 
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' # ======================================================
#' # = Beta: Should be good to use for Boulder/March 2020 =
#' # ======================================================
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
#' # ======================
#' # = Bivariate example =
#' # ======================
#' 
#' selDVs = c('tri','bic')
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", tryHard = "yes",
#' 	mzmData = mzmData, dzmData = dzmData, 
#' 	mzfData = mzfData, dzfData = dzfData, 
#' 	dzoData = dzoData
#' )
#' umxSummary(m1, file = NA);
#' 
#' # ===============
#' # = Switch to C =
#' # ===============
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "C", tryHard = "yes",
#' 	mzmData = mzmData, dzmData = dzmData, 
#' 	mzfData = mzfData, dzfData = dzfData, 
#' 	dzoData = dzoData
#' )
#' }
umxSummarySexLim <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	# Depends on R2HTML::HTML
	message("umxSummarySexLim is a beta feature. Some things are broken. If any desired stats are not presented, let me know what's missing")
	report = match.arg(report)

	tmp_PrintAsUpperLower <- function(bottom, top, selDVs= NULL) {
		bottom[upper.tri(bottom)] = top[upper.tri(top)]
		if(is.null(dimnames(bottom))){
			dimnames(bottom) = list(selDVs, selDVs)
		} else {
			dimnames(bottom)[[2]] = dimnames(bottom)[[1]]
		}
		umxAPA(bottom)	
	}

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummarySexLim(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	umx_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	selVars = model$MZm$expectation$dims
	selDVs  = dimnames(model$top$Vm)[[1]]
	nVar    = length(selDVs)

	# If univariate, there are no correlations...
	if(nVar > 1){
		message(model$name, ": ", nVar, "-variable sex limitation analysis")
		message("Genetic Factor Correlations (male (Ram) lower triangle, female (Raf) upper)")
		tmp_PrintAsUpperLower(bottom = model$top$Ram$values, top = model$top$Raf$values, selDVs= selDVs)

		message("Opposite sex A Correlations Rao")
		tmp = model$top$Rao$values; dimnames(tmp) = list(selDVs, selDVs)
		umxAPA(tmp)	

		message("C Factor Correlations (male (Rcm) lower triangle, female (Rcf) upper)")
		tmp_PrintAsUpperLower(bottom = model$top$Rcm$values, top = model$top$Rcf$values, selDVs= selDVs)

		message("Opposite sex C Correlations Rco")
		tmp = model$top$Rco$values; dimnames(tmp) = list(selDVs, selDVs)
		umxAPA(tmp)	

		message("E Factor Correlations (male (Rem) lower triangle, female (Ref) upper)")
		tmp_PrintAsUpperLower(bottom = model$top$Rem$values, top = model$top$Ref$values, selDVs= selDVs)
		
	}else{
		message(model$name, ": Univariate sex limitation analysis")
	}

	if(std){
		message("Standardized solution (top.[ACE][mf]Std  + R[ac]o matrices). Use std=F for raw variance.")
		Am = diag(as.matrix(model$top$AmStd$result))
		Cm = diag(as.matrix(model$top$CmStd$result))
		Em = diag(as.matrix(model$top$EmStd$result))
		Af = diag(as.matrix(model$top$AfStd$result))
		Cf = diag(as.matrix(model$top$CfStd$result))
		Ef = diag(as.matrix(model$top$EfStd$result))
	} else {
		message("Raw solution (top.[ACE][mf] + R[ac]o matrices). Use std=T for standardized output.")
		Am = diag(as.matrix(model$top$Am$result))
		Cm = diag(as.matrix(model$top$Cm$result))
		Em = diag(as.matrix(model$top$Em$result))
		Af = diag(as.matrix(model$top$Af$result))
		Cf = diag(as.matrix(model$top$Cf$result))
		Ef = diag(as.matrix(model$top$Ef$result))
	}

	Rao = diag(model$top$Rao$values)
	Rco = diag(model$top$Rco$values)

	Estimates = data.frame(row.names=selDVs, cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco), stringsAsFactors=FALSE)
	# Estimates = data.frame(cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco))
	if(model$top$dzCr$values == .25){
		treo = c("a", "d", "e")
	} else {
		treo = c("a", "c", "e")
	}
	names(Estimates) = c(paste0(rep(treo, each = 2), rep(c("m", "f"), times = 3)), "Rao", "Rco")

	Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
	if(report == "html"){
		R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = FALSE, sortableDF = TRUE); 
		umx_open("tmp.html")
	}
	
	if(extended == TRUE) {
		opposite = !std
		if(opposite){
			message("Standardized solution (top.[ACE][mf]Std  + R[ac]o matrices).")
			Am = diag(as.matrix(model$top$AmStd$result))
			Cm = diag(as.matrix(model$top$CmStd$result))
			Em = diag(as.matrix(model$top$EmStd$result))
			Af = diag(as.matrix(model$top$AfStd$result))
			Cf = diag(as.matrix(model$top$CfStd$result))
			Ef = diag(as.matrix(model$top$EfStd$result))
			# Estimates (will be printed below...)
		} else {
			# TODO sexlim: Check raw solution
			message("Raw solution (top.[ACE][mf] + R[ac]o matrices)")
			Am = diag(as.matrix(model$top$Am$result))
			Cm = diag(as.matrix(model$top$Cm$result))
			Em = diag(as.matrix(model$top$Em$result))
			Af = diag(as.matrix(model$top$Af$result))
			Cf = diag(as.matrix(model$top$Cf$result))
			Ef = diag(as.matrix(model$top$Ef$result))
			# Estimates (will be printed below...)
		}

		Rao = diag(model$top$Rao$values)
		Rco = diag(model$top$Rco$values)

		Estimates = data.frame(row.names=selDVs, cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco), stringsAsFactors=FALSE)
		# Estimates = data.frame(cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco))
		if(model$top$dzCr$values == .25){
			treo = c("a", "d", "e")
		} else {
			treo = c("a", "c", "e")
		}
		names(Estimates) = c(paste0(rep(treo, each = 2), rep(c("m", "f"), times = 3)), "Rao", "Rco")

		Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)

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
			# TODO ("plot() not implemented yet for sex lim!")
			umxPlotSexLim(model, file = file, std = std)
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

# TODO: umxPlotSexLim Add SEstyle code from plotCP

#' Draw and display a graphical figure of a Sex limitation model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' # @aliases plot.MxModelCP
#' @param x [mxModel()] to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param SEstyle report "b (se)" instead of "b [lower, upper]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxCP()]
#' @family Plotting functions
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' umx_set_optimizer("SLSQP")
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
#' m1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", autoRun= FALSE,
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' m1 = mxTryHard(m1)
#' umxPlotSexLim(m1)
#' plot(m1) # no need to remember a special name: plot works fine!
#' }
umxPlotSexLim <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	message("no plots for umxPlotSexLim as yet.")
	return()
	# New plot functions no longer dependent on labels. This means they need to know about the correct matrices to examine.
	# 1. a_cp_matrix = A latent (and correlations among latents)
	# 	* These go from a_cp n=row TO common n= row
	# 	* Or for off diag, from a_cp n=col TO a_cp n= row
	# 2. Same again for c_cp_matrix, e_cp_matrix
	# 3. cp_loadings common factor loadings

	format = match.arg(format)
	model = x # Just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelSexLim", callingFn= "umxPlotSexLim")

	umx_has_been_run(model, stop = TRUE)
	selVars = model$MZm$expectation$dims
	selDVs  = dimnames(model$top$Vm)[[1]]
	nVar    = length(selDVs)

	if(std){
		# message("Standardized solution (top.[ACE][mf]Std  + R[ac]o matrices)")
		Am = diag(as.matrix(model$top$AmStd$result))
		Cm = diag(as.matrix(model$top$CmStd$result))
		Em = diag(as.matrix(model$top$EmStd$result))
		Af = diag(as.matrix(model$top$AfStd$result))
		Cf = diag(as.matrix(model$top$CfStd$result))
		Ef = diag(as.matrix(model$top$EfStd$result))
	} else {
		# message("Raw solution (top.[ACE][mf] + R[ac]o matrices)")
		Am = diag(as.matrix(model$top$Am$result))
		Cm = diag(as.matrix(model$top$Cm$result))
		Em = diag(as.matrix(model$top$Em$result))
		Af = diag(as.matrix(model$top$Af$result))
		Cf = diag(as.matrix(model$top$Cf$result))
		Ef = diag(as.matrix(model$top$Ef$result))
	}

	out = list(str = "", latents = c(), manifests = c())

	# 1. Collect latents on the diag
	# TODO sexlim plot: maybe just draw a?
	#   a1f-> man1; a1m-> man1; a2f-> man2; a2m-> man2; a1f<-> c(a1m; a2m; a2f);

	# from   = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())

	# Values from, e.g. AmStd$result
	# |    |   am|   af|cm |   cf|   em|  ef|  Rao| Rco|
	# |:---|----:|----:|:--|----:|----:|---:|----:|---:|
	# |bic | 0.77| 0.74|.  | 0.07| 0.23| 0.2| 0.87|   1|

	# Process diag (a|c|e)(mf) matrices
	# Am cells are Am1 -> selDVs[1]; Am2 -> selDVs[2], etc.
	# TODO need a plain-matrix substitute here...(because sexlim uses algebras for most or what we need)
	out = umx_dot_mat2dot(Am, cells = "diag", from = "cols", fromType = "latent", toLabel = selDVs, p = out)
	out = umx_dot_mat2dot(Cm, cells = "diag", from = "cols", fromType = "latent", toLabel = selDVs, p = out)
	out = umx_dot_mat2dot(Em, cells = "diag", from = "cols", fromType = "latent", toLabel = selDVs, p = out)

	# 2. On the lower
	# from = "<name><rowNum>"; target = "<name><colNum>"
	out = umx_dot_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_dot_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_dot_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", p = out)

	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_dot_mat2dot(model$top$cp_loadings, cells= "any", toLabel= selDVs, from= "cols", fromLabel= "common", fromType= "latent", p= out)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)

	# Process "as" matrix
	out = umx_dot_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = umx_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", p = out)
	}
	preOut  = umx_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = umx_dot_rank(out$latents, "^[ace]_cp", "min")
	bottom  = umx_dot_rank(out$latents, "^[ace]s[0-9]+$", "max")
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, top, bottom, out$str, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	}
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	# TODO umxPlotCP could tabulate thresholds?
	# Process "_dev" (where are these?)
	# cat(out$str)
}

#' @export
plot.MxModelSexLim <- umxPlotSexLim
