# TODO sexlim: Scalar and Homogeneity variants are less tested than Nonscalar.

#' Multivariate sex limitation twin model
#'
#' @description
#' Multivariate twin analysis allowing for sex limitation (factors operate differently in males 
#' vs. females) based on a correlated factors model. With 5-groups of twins, this model allows
#' for both Quantitative and Qualitative Sex-Limitation.
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
#' **1. Nonscalar Sex Limitation**
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
#' @details
#' **A or C**
#' 
#' Due to limitations on the degrees of freedom allowed by the twin model, we can model 
#' qualitative sex differences for only one of A or C at a time.
#' 
#' *notes*: There is a half-way house model of heterogeneity in which a, c, and e components are scaled by a 
#' scalar constant in one sex.
#' 
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
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search", "auto" (picks to suit the model)
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - [OpenMx::mxModel()] of subclass mxModel.CFSexLim
#' @export 
#' @seealso [umxSummarySexLim()], [umxPlotSexLim()]
#' @family Twin Modeling Functions
#' @references Neale, M. C., Roysamb, E., Jacobson, K. (2006). Multivariate genetic analysis of sex-lim and G x E interaction. *Twin Research & Human Genetics*, **9**, pp. 481--489. \doi{10.1375/183242706778024937}
#' @examples
#' #  # =============================================
#' #  # = Run Qualitative Sex Differences ACE model =
#' #  # =============================================
#' 
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
#' m1 = umxSexLim(selDVs = "bic", A_or_C = "A", tryHard = "yes", sep = "_T",
#'		sexlim = "Nonscalar",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#'
#' # Drop qualitative sex limitation
#' # Distinct af and am (and c and e), but shared Ra (and Rc and Re) between variables
#' # (same for males and females)
#' m1a = umxModify(m1, regex = "^Rao_", value=1, name = "no_qual", comparison = TRUE)
#'
#' # Equate a, ac, and try ace across m & f in scalar model
#' m1b = umxModify(m1a, regex = "^a[fm]_", newlabels="a_", name = "eq_a_no_qual"  , comparison = TRUE)
#' m1c = umxModify(m1b, regex = "^c[fm]_", newlabels="c_", name = "eq_ac_no_qual" , comparison = TRUE)
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
#' )
#' umxCompare(m1, c(m2, m3))
#'
#' # ===========================================
#' # = Bivariate example with manual reduction =
#' # ===========================================
#' m1 = umxSexLim(selDVs = c("bic", "tri"), sep = "_T", A_or_C = "A", tryHard="yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#' )
#' 
#' # Scalar sex limitation (same correlation among components for m and f)
#' m2 = umxSexLim(selDVs = c("bic", "tri"), sep = "_T", 
#' 	A_or_C = "A", tryHard="yes", sexlim="Scalar",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#' )
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
#' # =============================
#' # = Run multi-variate example =
#' # =============================
#' # Variables for Analysis
#' selDVs = c('ssc','sil','caf','tri','bic')
#' selDVs = c('ssc','tri','bic')
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", tryHard = "yes",
#'	mzmData = mzmData, dzmData = dzmData, 
#'  mzfData = mzfData, dzfData = dzfData, dzoData = dzoData
#')
#'
#' m2 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", sexlim = "Nonscalar",
#' 	tryHard = "yes",
#'	mzmData = mzmData, dzmData = dzmData, 
#'  mzfData = mzfData, dzfData = dzfData, dzoData = dzoData
#')
#'}
umxSexLim <- function(name = "sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, sep = NA, A_or_C = c("A", "C"), sexlim = c("Nonscalar", "Scalar", "Homogeneity"), dzAr = .5, dzCr = 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search", "auto"), optimizer = NULL){
	A_or_C  = match.arg(A_or_C)
	sexlim  = match.arg(sexlim)
	tryHard = match.arg(tryHard)
	if(sexlim != "Nonscalar" || A_or_C == "C"){
		message("Polite note: umxSexLim with sexlim = '", sexlim, "' and A_or_C = '", A_or_C, "' is less tested. Please report issues at github.com/tbates/umx/issues")
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
		# Quantitative & Qualitative Sex Differences for A (labels trimmed to Ra at end)
		Rao = umxMatrix("Rao", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound= -1, ubound= 1)
		Rco = umxMatrix("Rco", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound= -1, ubound= 1)
	} else if (A_or_C == "C"){
		# Quantitative & Qualitative Sex Differences for C (Ra is Symm, Rc is Full)
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
				# %&% pre- and post-multiplies: Ram %&%  am == ("am %*% Ram %*%  am")
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
		# Quantitative & Qualitative Sex Differences for A or C.
		# Male and female paths, plus Ra, Rc and Re between variables for males and females
		# Male-Female correlations in DZO group between A factors Rao FREE
		# Rc constrained across male, female and opposite-sex pairs

		# Non-scalar (full) sex-lim label pick Ra or Rc, and drop the sex codes on the other.
		if(A_or_C == "A"){
			# Convert Rc[fmo]_r1c1 => "Rc_r1c1"
			if("^Rc[fmo](_.*)$" %in% umxGetParameters(model)){
				model = umxModify(model, regex = "^Rc[fmo](_.*)$", newlabels = "Rc\\1", autoRun=FALSE)
			}
		}else if (A_or_C == "C"){
			# Convert Ra[fmo]_r1c1 => "Ra_r1c1"
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
		#    (to match same-sex Ra and Rc) Both standardized
		model = mxModel(model, mxModel(model$top,
			umxMatrix("Rao", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Ra", lbound = -1, ubound = 1),
			umxMatrix("Rco", "Stand", nrow = nVar, free = TRUE, values = .2, baseName = "Rc", lbound = -1, ubound = 1))
		)
		if(sexlim == "Homogeneity"){
			# Equate [ace] across m & f in scalar model
			model = umxModify(model, regex = "^a[fm]_", newlabels = "a_", autoRun = FALSE)
			model = umxModify(model, regex = "^c[fm]_", newlabels = "c_", autoRun = FALSE)
			model = umxModify(model, regex = "^e[fm]_", newlabels = "e_", autoRun = FALSE)
		}
	}

	model = omxAssignFirstParameters(model)

	# issue #293 umxSexLim equate means would be expMeanGm, expMeanGf, expMeanGo

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
#' See documentation for summary functions for other types of umx model here: [umxSummary()].
#'
#' @aliases umxSummary.MxModelSexLim
#' @param model a [umxSexLim()] model to summarize
#' @param digits round to how many digits (default = 2)
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param report If "html", then open an html table of the results
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output
#' @param show Here to support being called from generic xmu_safe_run_summary. User should ignore: can be c("std", "raw")
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary
#' @return - optional [OpenMx::mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxSexLim()], [umxPlotSexLim()]
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
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
umxSummarySexLim <- function(model, digits = 2, file = NA, comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), extended = FALSE, zero.print = ".", show = c("std", "raw"), returnStd = FALSE, ...) {
	show = match.arg(show, c("std", "raw"))
	if(show != "std"){
		std = FALSE
	}

	# Depends on R2HTML::HTML
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
	xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
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
		message("Standardized solution (top.[ACE][mf]Std  + R[ac]o matrices). Use std=FALSE for raw variance.")
		Am = diag(as.matrix(model$top$AmStd$result))
		Cm = diag(as.matrix(model$top$CmStd$result))
		Em = diag(as.matrix(model$top$EmStd$result))
		Af = diag(as.matrix(model$top$AfStd$result))
		Cf = diag(as.matrix(model$top$CfStd$result))
		Ef = diag(as.matrix(model$top$EfStd$result))
	} else {
		message("Raw solution (top.[ACE][mf] + R[ac]o matrices). Use std=TRUE for standardized output.")
		Am = diag(as.matrix(model$top$Am$result))
		Cm = diag(as.matrix(model$top$Cm$result))
		Em = diag(as.matrix(model$top$Em$result))
		Af = diag(as.matrix(model$top$Af$result))
		Cf = diag(as.matrix(model$top$Cf$result))
		Ef = diag(as.matrix(model$top$Ef$result))
	}

	Rao = diag(model$top$Rao$values)
	Rco = diag(model$top$Rco$values)

	Estimates = data.frame(row.names=selDVs, cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco), stringsAsFactors = FALSE)
	# Estimates = data.frame(cbind(Am, Af, Cm, Cf, Em, Ef, Rao, Rco))
	if(model$top$dzCr$values == .25){
		treo = c("a", "d", "e")
	} else {
		treo = c("a", "c", "e")
	}
	names(Estimates) = c(paste0(rep(treo, each = 2), rep(c("m", "f"), times = 3)), "Rao", "Rco")
	umx_print(Estimates, digits = digits, zero.print = zero.print, report=report,  append = FALSE, sortableDF = TRUE)
	xmu_twin_print_means(model, digits = digits, report = report)
	
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

	if(umx_has_CIs(model) & CIs) {
		message("TODO: umxSummary.MxModel.SexLim CI report not yet implemented - use summary")
	}
	} # end list catcher?

	if(!is.na(file)) {
		# message("making dot file")
		umxPlotSexLim(model, file = file, std = std)
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		xmu_standardize_ACE(model)
	}
}

#' @export
umxSummary.MxModelSexLim <- umxSummarySexLim

#' Draw and display a graphical figure of a Sex limitation model
#'
#' Will plot a graphical figure for a sex limitation model.
#' Options include `digits` (rounding), showing `means` or not, and which output `format` is desired.
#'
#' @aliases plot.MxModelSexLim
#' @param x [umxSexLim()] model to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param SEstyle Deprecated. Please use `uncertainty` instead.
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param showCIs Whether to show intervals already stored on the model (default = TRUE). Intervals computed via `uncertainty` still show when this is FALSE.
#' @param uncertainty What type of parameter uncertainty to show: "none" (default; intervals already stored on the model, nothing computed here), "SE" (standard errors computed per cell with `mxSE()`), or "CI" (intervals already stored on the model, nothing computed here).
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - [umxSexLim()], [umxSummarySexLim()]
#' @family Plotting functions
#' @references <https://tbates.github.io>
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
#' m1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", tryHard="yes",
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#' )
#' plot(m1)
#' # umxPlotSexLim(m1, means = TRUE) #full function name shows available options
#' }
umxPlotSexLim <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, showCIs = TRUE, uncertainty = c("none", "SE", "CI"), ...) {
	format = match.arg(format)
	model = x # Just to emphasize that x has to be a model 
	umx_check_model(model, "MxModelSexLim", callingFn = "umxPlotSexLim")
	umx_has_been_run(model, stop = TRUE)

	# Handle deprecated SEstyle parameter and map it to uncertainty
	if (missing(uncertainty)) {
		if (!missing(SEstyle) && !identical(SEstyle, FALSE)) {
			message("Polite note: SEstyle is deprecated. Please use uncertainty = \"SE\" instead.")
			uncertainty = "SE"
		} else {
			uncertainty = "none"
		}
	} else {
		uncertainty = match.arg(uncertainty)
	}
	
	selDVs  = dimnames(model$top$Vm)[[1]]
	nVar    = length(selDVs)
	c_label = if (model$top$dzCr$values == 0.25) "d" else "c"

	stored = NULL
	haveStored = FALSE
	if (isTRUE(showCIs) && umx_has_CIs(model)) {
		stored = model$output$confidenceIntervals
		haveStored = TRUE
	}
	if (uncertainty == "CI" && isTRUE(showCIs) && !haveStored) {
		message("Polite note: this model has no stored intervals to display. Tip: umxConfint(model, parm = 'all', type = 'wald') computes and stores Wald intervals.")
	}
	
	# Helper to format values
	format_val <- function(val, digits, strip_zero) {
		if (is.na(val)) return("")
		s = sprintf(paste0("%.", digits, "f"), val)
		if (strip_zero) {
			s = gsub("^0\\.", ".", s)
			s = gsub("^-0\\.", "-.", s)
		}
		return(s)
	}

	# Helper to append uncertainty to a formatted value. address is the raw label;
	# stdAlgebra (e.g. "top.AmStd[1,1]") names the standardized variance for std display.
	label_val <- function(val, address, stdAlgebra = NULL) {
		base = format_val(val, digits, strip_zero)
		if (uncertainty == "SE") {
			# Genuine per-cell SEs: never derived from stored intervals.
			seAddress = address
			stdPath = isTRUE(std) && !is.null(stdAlgebra)
			if (stdPath) {
				seAddress = stdAlgebra
			}
			se = suppressWarnings(tryCatch({ mxSE(seAddress, model, silent = TRUE)[1, 1] }, error = function(e){ NA_real_ }))
			if (!is.finite(se)) {
				return(base)
			}
			if (stdPath) {
				# Displayed value is sqrt(variance): delta method for the square root.
				if (!is.finite(val) || val == 0) {
					return(base)
				}
				se = se/(2 * val)
			}
			return(paste0(base, " (", format_val(se, digits, strip_zero), ")"))
		}
		if (uncertainty == "CI" && haveStored && !isTRUE(std)) {
			# Stored intervals show on raw display only: a raw interval would mislabel a standardized value.
			if (length(address) == 1 && !is.na(address) && address %in% rownames(stored)) {
				lb = suppressWarnings(as.numeric(stored[address, "lbound"])[1])
				ub = suppressWarnings(as.numeric(stored[address, "ubound"])[1])
				if (is.finite(lb) && is.finite(ub)) {
					ciEst = suppressWarnings(as.numeric(stored[address, "estimate"])[1])
					if (is.finite(ciEst)) {
						base = format_val(ciEst, digits, strip_zero)
					}
					return(paste0(base, " [", format_val(lb, digits, strip_zero), ", ", format_val(ub, digits, strip_zero), "]"))
				}
			}
		}
		return(base)
	}
	
	# Extract path loadings
	if (std) {
		safe_sqrt_diag <- function(mat) {
			val = diag(as.matrix(mat))
			val[val < 0] = 0
			sqrt(val)
		}
		am_val = safe_sqrt_diag(model$top$AmStd$result)
		cm_val = safe_sqrt_diag(model$top$CmStd$result)
		em_val = safe_sqrt_diag(model$top$EmStd$result)
		af_val = safe_sqrt_diag(model$top$AfStd$result)
		cf_val = safe_sqrt_diag(model$top$CfStd$result)
		ef_val = safe_sqrt_diag(model$top$EfStd$result)
	} else {
		am_val = diag(model$top$am$values)
		cm_val = diag(model$top$cm$values)
		em_val = diag(model$top$em$values)
		af_val = diag(model$top$af$values)
		cf_val = diag(model$top$cf$values)
		ef_val = diag(model$top$ef$values)
	}

	# Fitted twin-1 means (twin-2 columns share the labels; DZO reuses both sets)
	meanM = as.numeric(model$top$expMeanGm$values[1, 1:nVar])
	meanF = as.numeric(model$top$expMeanGf$values[1, 1:nVar])

	# Build dot code
	dot_str = c()
	add_line <- function(...) {
		dot_str <<- c(dot_str, paste0(...))
	}
	
	add_line("digraph G {")
	add_line("\tlabel=\"", model$name, "\";")
	add_line("\tsplines=true;")
	
	# 1. Male Subgraph
	add_line("\tsubgraph cluster_male {")
	add_line("\t\tlabel=\"Males\";")
	add_line("\t\tstyle=dashed; color=blue;")
	
	# Define male manifests
	for (i in 1:nVar) {
		add_line("\t\tM_", selDVs[i], " [shape=box];")
	}
	
	# Define male latents and paths
	for (i in 1:nVar) {
		a_node = paste0("am", i)
		c_node = paste0("cm", i)
		e_node = paste0("em", i)
		
		add_line("\t\t", a_node, " [shape=circle, label=\"a_m", i, "\"];")
		add_line("\t\t", c_node, " [shape=circle, label=\"", c_label, "_m", i, "\"];")
		add_line("\t\t", e_node, " [shape=circle, label=\"e_m", i, "\"];")
		
		add_line("\t\t", a_node, " -> M_", selDVs[i], " [label=\"", label_val(am_val[i], model$top$am$labels[i, i], paste0("top.AmStd[", i, ",", i, "]")), "\"];")
		add_line("\t\t", c_node, " -> M_", selDVs[i], " [label=\"", label_val(cm_val[i], model$top$cm$labels[i, i], paste0("top.CmStd[", i, ",", i, "]")), "\"];")
		add_line("\t\t", e_node, " -> M_", selDVs[i], " [label=\"", label_val(em_val[i], model$top$em$labels[i, i], paste0("top.EmStd[", i, ",", i, "]")), "\"];")
	}

	if (isTRUE(means)) {
		add_line("\t\tone_m [shape=triangle];")
		for (i in 1:nVar) {
			add_line("\t\tone_m -> M_", selDVs[i], " [label=\"", label_val(meanM[i], model$top$expMeanGm$labels[1, i]), "\"];")
		}
	}
	
	# Ranks for males
	add_line("\t\t{rank=same; ", paste0("M_", selDVs, collapse="; "), "}")
	add_line("\t\t{rank=min; ", paste0("am", 1:nVar, collapse="; "), "}")
	add_line("\t\t{rank=max; ", paste0(c("cm", "em"), rep(1:nVar, each=2), collapse="; "), "}")
	
	# Same-sex correlations for males
	if (nVar > 1) {
		Ram = model$top$Ram$values
		Rcm = model$top$Rcm$values
		Rem = model$top$Rem$values
		for (i in 1:(nVar-1)) {
			for (j in (i+1):nVar) {
				if (Ram[i, j] != 0) {
					add_line("\t\tam", i, " -> am", j, " [dir=both, style=dashed, label=\"", label_val(Ram[i, j], model$top$Ram$labels[i, j]), "\"];")
				}
				if (Rcm[i, j] != 0) {
					add_line("\t\tcm", i, " -> cm", j, " [dir=both, style=dashed, label=\"", label_val(Rcm[i, j], model$top$Rcm$labels[i, j]), "\"];")
				}
				if (Rem[i, j] != 0) {
					add_line("\t\tem", i, " -> em", j, " [dir=both, style=dashed, label=\"", label_val(Rem[i, j], model$top$Rem$labels[i, j]), "\"];")
				}
			}
		}
	}
	add_line("\t}")
	
	# 2. Female Subgraph
	add_line("\tsubgraph cluster_female {")
	add_line("\t\tlabel=\"Females\";")
	add_line("\t\tstyle=dashed; color=red;")
	
	# Define female manifests
	for (i in 1:nVar) {
		add_line("\t\tF_", selDVs[i], " [shape=box];")
	}
	
	# Define female latents and paths
	for (i in 1:nVar) {
		a_node = paste0("af", i)
		c_node = paste0("cf", i)
		e_node = paste0("ef", i)
		
		add_line("\t\t", a_node, " [shape=circle, label=\"a_f", i, "\"];")
		add_line("\t\t", c_node, " [shape=circle, label=\"", c_label, "_f", i, "\"];")
		add_line("\t\t", e_node, " [shape=circle, label=\"e_f", i, "\"];")
		
		add_line("\t\t", a_node, " -> F_", selDVs[i], " [label=\"", label_val(af_val[i], model$top$af$labels[i, i], paste0("top.AfStd[", i, ",", i, "]")), "\"];")
		add_line("\t\t", c_node, " -> F_", selDVs[i], " [label=\"", label_val(cf_val[i], model$top$cf$labels[i, i], paste0("top.CfStd[", i, ",", i, "]")), "\"];")
		add_line("\t\t", e_node, " -> F_", selDVs[i], " [label=\"", label_val(ef_val[i], model$top$ef$labels[i, i], paste0("top.EfStd[", i, ",", i, "]")), "\"];")
	}

	if (isTRUE(means)) {
		add_line("\t\tone_f [shape=triangle];")
		for (i in 1:nVar) {
			add_line("\t\tone_f -> F_", selDVs[i], " [label=\"", label_val(meanF[i], model$top$expMeanGf$labels[1, i]), "\"];")
		}
	}
	
	# Ranks for females
	add_line("\t\t{rank=same; ", paste0("F_", selDVs, collapse="; "), "}")
	add_line("\t\t{rank=min; ", paste0("af", 1:nVar, collapse="; "), "}")
	add_line("\t\t{rank=max; ", paste0(c("cf", "ef"), rep(1:nVar, each=2), collapse="; "), "}")
	
	# Same-sex correlations for females
	if (nVar > 1) {
		Raf = model$top$Raf$values
		Rcf = model$top$Rcf$values
		Ref = model$top$Ref$values
		for (i in 1:(nVar-1)) {
			for (j in (i+1):nVar) {
				if (Raf[i, j] != 0) {
					add_line("\t\taf", i, " -> af", j, " [dir=both, style=dashed, label=\"", label_val(Raf[i, j], model$top$Raf$labels[i, j]), "\"];")
				}
				if (Rcf[i, j] != 0) {
					add_line("\t\tcf", i, " -> cf", j, " [dir=both, style=dashed, label=\"", label_val(Rcf[i, j], model$top$Rcf$labels[i, j]), "\"];")
				}
				if (Ref[i, j] != 0) {
					add_line("\t\tef", i, " -> ef", j, " [dir=both, style=dashed, label=\"", label_val(Ref[i, j], model$top$Ref$labels[i, j]), "\"];")
				}
			}
		}
	}
	add_line("\t}")
	
	# 3. Cross-sex correlations
	Rao = model$top$Rao$values
	Rco = model$top$Rco$values
	for (i in 1:nVar) {
		for (j in 1:nVar) {
			if (Rao[i, j] != 0) {
				add_line("\tam", i, " -> af", j, " [dir=both, style=dashed, constraint=false, color=purple, label=\"", label_val(Rao[i, j], model$top$Rao$labels[i, j]), "\"];")
			}
			if (Rco[i, j] != 0) {
				add_line("\tcm", i, " -> cf", j, " [dir=both, style=dashed, constraint=false, color=purple, label=\"", label_val(Rco[i, j], model$top$Rco$labels[i, j]), "\"];")
			}
		}
	}
	
	add_line("}")
	digraph = paste(dot_str, collapse = "\n")
	
	if(format != "current"){
		tmp = umx_set_plot_format(silent = TRUE)
		umx_set_plot_format(format)
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		umx_set_plot_format(tmp)
		if(res$visible) res$value else invisible(res$value)
	}else{
		res = withVisible(xmu_dot_maker(model, file, digraph, strip_zero = strip_zero))
		if(res$visible) res$value else invisible(res$value)
	}
}

#' @export
plot.MxModelSexLim <- umxPlotSexLim

