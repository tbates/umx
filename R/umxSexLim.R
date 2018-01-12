#' Multivariate twin analysis with sex limitation
#'
#' Build a multivariate twin analysis with sex limitation based on a correlated factors model.
#' This allows Quantitative & Qualitative Sex-Limitation. The correlation approach ensures that variable order
#' does NOT affect ability of model to account for DZOS data.
#' Restrictions: Assumes means and variances can be equated across birth order within zygosity groups
#'
#' @param name    The name of the model (Default = "CF_sexlim")
#' @param selDVs  BASE NAMES of the variables in the analysis. You MUST provide suffixes.
#' @param C_or_A  Whether to model sex-limitation on C or on A. (Defaults to "A")
#' @param mzmData Dataframe containing the MZ male data
#' @param dzmData Dataframe containing the DZ male data
#' @param mzfData Dataframe containing the MZ female data
#' @param dzfData Dataframe containing the DZ female data
#' @param dzoData Dataframe containing the DZ opposite-sex data (be sure and get in right order)
#' @param sep Suffix used for twin variable naming. Allows using just the base names in selVars
#' @param autoRun Whether to run the model and return it, or just return it.
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - \code{\link{mxModel}} of subclass mxModel.CFSexLim
#' @export
#' @family Twin Modeling Functions
#' @references - Neale et al. (2006). 
#' Multivariate genetic analysis of sex-lim and GxE interaction.
#' \emph{Twin Research & Human Genetics}, \bold{9}, pp. 481--489. 
#' @examples
#' # see examples page for now
#  # =============================================
#  # = Run Qualitative Sex Differences ACE model =
#  # =============================================
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' require(umx)
#' data("us_skinfold_data")
#' # rescale vars
#' us_skinfold_data[, c('bic_T1', 'bic_T2')] <- us_skinfold_data[, c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[, c('tri_T1', 'tri_T2')] <- us_skinfold_data[, c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[, c('caf_T1', 'caf_T2')] <- us_skinfold_data[, c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[, c('ssc_T1', 'ssc_T2')] <- us_skinfold_data[, c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[, c('sil_T1', 'sil_T2')] <- us_skinfold_data[, c('sil_T1', 'sil_T2')]/5
#'
#' # Variables for Analysis
#' selDVs = c('ssc','sil','caf','tri','bic') # (was Vars)
#' # Data objects for Multiple Groups
#' mzmData = subset(us_skinfold_data, zyg == 1)
#' mzfData = subset(us_skinfold_data, zyg == 2)
#' dzmData = subset(us_skinfold_data, zyg == 3)
#' dzfData = subset(us_skinfold_data, zyg == 4)
#' dzoData = subset(us_skinfold_data, zyg == 5)
#'
#' m1 = umxSexLim(selDVs = selDVs, sep = "_T", A_or_C = "A", autoRun=F,
#'				mzmData = mzmData, dzmData = dzmData, 
#'        mzfData = mzfData, dzfData = dzfData, 
#'        dzoData = dzoData)
#' umxSummary(m1)
#' summary(m1)$Mi
#' 
umxSexLim <- function(name = "sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, sep = NA, A_or_C = c("A", "C"), autoRun = getOption("umx_auto_run"), optimizer = NULL){
	# ================================
	# = 1. Non-scalar Sex Limitation =
	# ================================
	# Quantitative & Qualitative Sex Differences for A
	# Male and female paths, plus Ra, Rc and Re between variables for males and females
	# Male-Female correlations in DZO group between A factors Rao FREE
	# Rc constrained across male/female and opposite sex
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	
	A_or_C = match.arg(A_or_C)
	# Correlated factors sex limitations

	nSib = 2 # Number of siblings in a twin pair
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	# TODO implement ADE version...
	dzCr = 1
	if(dzCr == .25 && name == "sexlim"){
		name = "sexlimADE"
	}
	suffix = sep
	if(is.na(suffix)){
		stop("Please provide sep (e.g. '_T')")
	}
	nVar = length(selDVs) # (was "nv": ntv is this x 2;  nvm1 = nv - 1
	selVars = umx_paste_names(selDVs, suffix, 1:2)
	# Check names, and drop unused columns from data
	umx_check_names(selVars, data = mzmData, die = TRUE); mzmData = mzmData[, selVars]
	umx_check_names(selVars, data = dzmData, die = TRUE); dzmData = dzmData[, selVars]
	umx_check_names(selVars, data = mzfData, die = TRUE); mzfData = mzfData[, selVars]
	umx_check_names(selVars, data = dzfData, die = TRUE); dzfData = dzfData[, selVars]
	umx_check_names(selVars, data = dzoData, die = TRUE); dzoData = dzoData[, selVars]

	# Start means at actual means of some group (done!!!)
	obsMean = umx_means(mzmData[, selVars[1:nVar], drop = FALSE])
	
	varStarts = umx_var(mzmData[, selVars[1:nVar], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	if(nVar == 1){
		varStarts = sqrt(varStarts)/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
	}
	varStarts = matrix(varStarts, nVar, nVar)
	

	# Helpful dimnames for Algebra-based Estimates and Derived Variance Component output (see "VarsZm") (done!!!)
	colZm <- paste0(selDVs, rep(c('Am', 'Cm', 'Em'), each = nVar))
	colZf <- paste0(selDVs, rep(c('Af', 'Cf', 'Ef'), each = nVar))

	# Make Rao and Rco matrices (done!!!)
	if(A_or_C == "A"){
			# Quantitative & Qualitative Sex Differences for A (Ra is Full, Rc is symm) (labels trimmed to ra at end)
			# # TODO Check Stand (symmetric with 1's on diagonal) OK (was Symm fixed diag@1)			
			Rao = umxMatrix("Rao", "Full" , nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound = -1, ubound = 1)
			Rco = umxMatrix("Rco", "Stand", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1)
	} else if (A_or_C == "C"){
			# Quantitative & Qualitative Sex Differences for C (Ra is symm, Rc is Full)
			Rco = umxMatrix("Rco", "Full" , nrow=nVar, ncol=nVar, free=TRUE, values= 1, lbound=-1, ubound=1)
			Rao = umxMatrix("Rao", "Stand", nrow=nVar, ncol=nVar, free=TRUE, values=.4, lbound=-1, ubound=1)
	}
	Rao_and_Rco_matrices = list(Rao, Rco)

	m1 = mxModel(name,
		mxModel("top",
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		
			# ✓ Path Coefficient matrices a, c, and e for males and females (done!!!)
			# TODO Better starts here? @mikeneale?
			umxMatrix("am", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("cm", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("em", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("af", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("cf", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),
			umxMatrix("ef", "Diag", nrow = nVar, free = TRUE, values = varStarts, lbound = .0001),

			# Matrices for Correlation Coefficients within/across Individuals (done!!!)
			# Stand = symmetric with 1's on diagonal
			# NOTE: one of # (Rc[fmo]) or (Ra[fmo]) are equated (labeled "rc") (bottom of script)
			umxMatrix("Ram", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Raf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1), 
			umxMatrix("Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Rem", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
			umxMatrix("Ref", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

			Rao_and_Rco_matrices,

			# Algebra Male and female variance components (done!!!)
			mxAlgebra(name = "Am", Ram %&% am),
			mxAlgebra(name = "Cm", Rcm %&% cm),
			mxAlgebra(name = "Em", Rem %&% em),

			mxAlgebra(name = "Af", Raf %&% af),
			mxAlgebra(name = "Cf", Rcf %&% cf),
			mxAlgebra(name = "Ef", Ref %&% ef),

			# Opposite-Sex parameters: Rao, Rco, Amf, Cmf (done!!!)
			mxAlgebra(name = "Amf", am %*% (Rao) %*% t(af)),
			mxAlgebra(name = "Cmf", cm %*% (Rco) %*% t(cf)),
			

			# Constrain the 6 R*(f|m) Eigen values to be positive (done!!!)
			umxMatrix("pos1by6", "Full", nrow = 1, ncol = 6, free = FALSE, values = .0001),
			mxAlgebra(name = "minCor", cbind(
				min(eigenval(Ram)), min(eigenval(Rcm)), min(eigenval(Rem)),
			  	min(eigenval(Raf)), min(eigenval(Rcf)), min(eigenval(Ref)))
			),
			mxConstraint(name = "Keep_it_Positive_Baby", minCor > pos1by6),

			# Algebra for Total variances and standard deviations (of diagonals) (done!!!)
			umxMatrix("I", "Iden", nrow = nVar),
			mxAlgebra(name = "Vm", Am + Cm + Em),
			mxAlgebra(name = "Vf", Af + Cf + Ef),
			mxAlgebra(name = "iSDm", solve(sqrt(I * Vm))),
			mxAlgebra(name = "iSDf", solve(sqrt(I * Vf))),

			# Algebras for Parameter Estimates and Derived Variance Components (done!!!)
			mxAlgebra(name = "VarsZm", cbind(Am/Vm, Cm/Vm, Em/Vm), dimnames = list(NULL, colZm)),
			mxAlgebra(name = "CorsZm", cbind(Ram, Rcm, Rem)      , dimnames = list(NULL, colZm)),

			mxAlgebra(name = "VarsZf", cbind(Af/Vf, Cf/Vf, Ef/Vf), dimnames = list(NULL, colZf)),
			mxAlgebra(name = "CorsZf", cbind(Raf, Rcf, Ref)      , dimnames = list(NULL, colZf)),

			# Matrix & Algebra for expected Mean Matrices in MZ & DZ twins (done!!)
			umxMatrix("expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "Mm")),
			umxMatrix("expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, "Mf")),
			umxMatrix("expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = obsMean, labels = paste0(selDVs, rep(c("Mm", "Mf"), each = nVar))),

			# Matrix & Algebra for expected Variance/Covariance Matrices in MZ & DZ twins (done!!!)
			mxAlgebra(name = "expCovMZm", rbind(cbind(Vm,         Am + Cm)  , cbind(        Am + Cm, Vm))),
			mxAlgebra(name = "expCovDZm", rbind(cbind(Vm, 0.5 %x% Am + Cm)  , cbind(0.5 %x% Am + Cm, Vm))),
			mxAlgebra(name = "expCovMZf", rbind(cbind(Vf,         Af + Cf)  , cbind(        Af + Cf, Vf))),
			mxAlgebra(name = "expCovDZf", rbind(cbind(Vf, 0.5 %x% Af + Cf)  , cbind(0.5 %x% Af + Cf, Vf))),
			mxAlgebra(name = "expCovDZo", rbind(cbind(Vm, 0.5 %x% Amf + Cmf), cbind(0.5 %x% t(Amf) + t(Cmf), Vf)))
		), # end of top

		# 5 group models (done!!!)
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
	) # end m1

	# Non-scalar (full) sex-lim label tweaks
	
	if(A_or_C == "A"){
		# (Rcf|Rcm|Rco) => "rc"
		m1 = umxModify(m1, regex = "^Rc[fmo](_.*)$", newlabels = "Rc\\1", autoRun=FALSE)
	}else if (A_or_C == "C"){
		# (Raf|Ram|Rao) => "ra"
		m1 = umxModify(m1, regex = "^Ra[fmo](_.*)$", newlabels = "Ra\\1", autoRun=FALSE)
	}

	# Tests: equate means would be expMeanGm, expMeanGf, expMeanGo
	m1 = as(m1, "MxModel.SexLimCF") # set class so umxSummary, plot, etc. work.
	if(autoRun){
		m1 = mxRun(m1)
		umxSummary(m1)
		return(m1)
	} else {
		return(m1)
	}
}