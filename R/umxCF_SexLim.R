#' umxACESexLim
#'
#' Build a multivariate twin analysis with sex limitation
#' MODEL:MV Quantitative & Qualitative Sex-Limitation script (ACE Correlated Factors model & ACE Cholesky model)
#' Correlation Approach to ensure that order of variables does NOT affect ability of model to account for DZOS data
#' Ref: Neale et al., Multivariate genetic analysis of sex-lim and GxE interaction, Twin Research & Human Genetics, 2006 
#' Restrictions: Assumes means and variances can be equated across birth order within zygosity groups
#'
#' @param name    The name of the model (Defaults = "ACE_sexlim")
#' @param selDVs  The variables in the analysis. If you provide a suffix, you can use just the base names
#' @param mzmData Dataframe containing the MZ male data
#' @param dzmData Dataframe containing the DZ male data
#' @param mzfData Dataframe containing the MZ female data
#' @param dzfData Dataframe containing the DZ female data
#' @param dzoData Dataframe containing the DZ opposite-sex data (be sure and get in right order)
#' @param suffix (optional) suffix used for twin variable naming. Allows using just the base names in selVars
#' @return - \code{\link{umxACESexLim}} model
#' @export
#' @family Twin Modeling Functions
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' # Load Libraries
#' require(umx);
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' data("usski")
#' # rescale vars
#' usski[,c('bic_T1', 'bic_T2')] <- usski[,c('bic_T1', 'bic_T2')]/3.4
#' usski[,c('tri_T1', 'tri_T2')] <- usski[,c('tri_T1', 'tri_T2')]/3
#' usski[,c('caf_T1', 'caf_T2')] <- usski[,c('caf_T1', 'caf_T2')]/3
#' usski[,c('ssc_T1', 'ssc_T2')] <- usski[,c('ssc_T1', 'ssc_T2')]/5
#' usski[,c('sil_T1', 'sil_T2')] <- usski[,c('sil_T1', 'sil_T2')]/5
#' # describe(usski, skew = FALSE)
#' 
#' # Select Variables for Analysis
#' varList = c('ssc','sil','caf','tri','bic')
#' selVars = umx_paste_names(varList, suffix, 1:2)
#' 
#' # Data objects for Multiple Groups
#' mzmData = subset(usski, zyg == 1, selVars)
#' dzmData = subset(usski, zyg == 3, selVars)
#' mzfData = subset(usski, zyg == 2, selVars)
#' dzfData = subset(usski, zyg == 4, selVars)
#' dzoData = subset(usski, zyg == 5, selVars)
#'
#'
#' # ===============================
#' # = 1 Nonscalar Sex Limitation  =
#' # ===============================
#' # Quantitative Sex Differences & Qualitative Sex Differences for A
#' # Male and female paths, plus male and female Ra, Rc and Re between variables
#' # Male-Female correlations in DZO group between A factors Rao FREE, Rc constrained across male/female and opp-sex
#' # ===================================================
#' # = Test switching specific a from Males to females =
#' # ===================================================
#' 
#' m1 = umxCF_SexLim(selDVs = varList, mzmData = mzmData, dzmData = dzmData, mzfData = mzfData, dzfData = dzfData, dzoData = dzoData, suffix = "_T")
#' m1 = mxRun(m1)
#' 
#' # ===============================
#' # = 2 Nonscalar Sex Limitation  =
#' # ===============================
#' # Quantitative Sex Differences & Qualitative Sex Differences for C
#' # Male and female paths, plus male and female Ra, Rc and Re between variables
#' # Male-Female correlations in DZO group between C factors Rco FREE, Ra constrained across male/female and oppsex




#' m2 = umxSetParameters(m1, labels = "asm_.*", free = F, values = 0, regex = T)
#' m2 = umxSetParameters(m1, labels = "asf_.*", free = T, values = 0, regex = T)
#' m2 = mxRun(m2)
#' summary(m2)
#' mxCompare(m2, m1)
#' \dontrun{
#' # Does fit move on repeated execution?
#' for (i in 1:4){
#' 	m2 <- mxRun(m2);
#' 	print(m2$output$mi)
#' }
#' }
umxCF_SexLim <- function(name = "ACE_sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, C_or_A = "A", suffix = NA){
	message("Not checked!")
	if(!is.na(suffix)){
		selVars = umx_paste_names(selDVs, suffix, 1:2)
	}else{
		selVars = selDVs
	}
	# Correlated factors sex limitations
	# Algebra to Constrain Correlation Matrices to be Positive Definite

	# get awesome mean-starts
	svMe = colMeans(mzmData[,1:nVar], na.rm = TRUE) # c(5,8,4,4,8) # start value for means
	# dimnames for Algebras generated to hold Parameter Estimates and Derived Variance Components
	colZm <- paste0(varList, rep(c('Am', 'Cm', 'Em'), each = nVar))
	colZf <- paste0(varList, rep(c('Af', 'Cf', 'Ef'), each = nVar))

	m1 = mxModel(name,
		mxModel("top",
		# Matrices a, c, and e to store Path Coefficients
		mxMatrix(name = "am", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		mxMatrix(name = "cm", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		mxMatrix(name = "em", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		mxMatrix(name = "af", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		mxMatrix(name = "cf", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		mxMatrix(name = "ef", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),

		mxMatrix(name = "Ram", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		mxMatrix(name = "Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		mxMatrix(name = "Rem", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		mxMatrix(name = "Raf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		mxMatrix(name = "Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		mxMatrix(name = "Ref", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

		# Opposite-Sex parameters: Rao, Rco, Amf, Cmf
		if(C_or_A == "A"){
			# Quantitative Sex Differences & Qualitative Sex Differences for C
			{
				Rao = mxMatrix(name = "Rao", "Full", nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound = -1, ubound = 1)
				Rco = mxMatrix(name = "Rco", "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1)
				# Fix diag of corRco @1
				diag(Rco$free)   = FALSE
				diag(Rco$values) = 1
				list(Rao, Rco)
			}
		} else if (C_or_A == "C"){
			# Quantitative Sex Differences & Qualitative Sex Differences for C
			{
				Rao = mxMatrix(name = "Rao", "Symm", nrow=nVar, ncol=nVar, free=TRUE, values=.4, lbound=-1, ubound=1)
				Rco = mxMatrix(name = "Rco", "Full", nrow=nVar, ncol=nVar, free=TRUE, values= 1, lbound=-1, ubound=1)
				# Fix diag of corRco @1
				diag(Rao$free)   = FALSE
				diag(Rao$values) = 1
				list(Rao, Rco)
			}
		}

		# Matrices A, C, and E compute variance components
		mxAlgebra(name = "Am" , Ram %&% am),
		mxAlgebra(name = "Cm" , Rcm %&% cm),
		mxAlgebra(name = "Em" , Rem %&% em),

		# Algebra to compute total variances and standard deviations (diagonal only)
		mxAlgebra(name = "Vm", Am + Cm + Em),
		mxAlgebra(name = "VarsZm", cbind(Am/Vm, Cm/Vm, Em/Vm), dimnames = list(NULL, colZm)),
		mxAlgebra(name = "CorsZm", cbind(Ram, Rcm, Rem), dimnames = list(NULL, colZm)),

		mxAlgebra(name = "Af", Raf %&% af),
		mxAlgebra(name = "Cf", Rcf %&% cf),
		mxAlgebra(name = "Ef", Ref %&% ef),
		mxAlgebra(name = "Vf", Af + Cf + Ef),
		mxAlgebra(name = "VarsZf", cbind(Af/Vf, Cf/Vf, Ef/Vf), dimnames = list(NULL, colZf)),
		mxAlgebra(name = "CorsZf", cbind(Raf, Rcf, Ref), dimnames = list(NULL, colZf)),

		mxAlgebra(name = "Amf", Rao %&% af),
		mxAlgebra(name = "Cmf", Rco %&% cf),
		# covAfm = mxAlgebra( expression=af %*% t(Rao) %*% t(am), name="Afm" ) =t(Amf)
		# covCfm = mxAlgebra( expression=cf %*% t(Rco) %*% t(cm), name="Cfm" ) =t(Cmf)

		mxMatrix(name  = "I", "Iden", nrow = nVar),
		mxAlgebra(name = "iSDm", solve(sqrt(I * Vm))),
		mxAlgebra(name = "iSDf", solve(sqrt(I * Vf))),

		mxMatrix(name = "pos1by6", "Full", nrow = 1, ncol = 6, free = FALSE, values = .0001),
		mxAlgebra(name = "minCor", cbind(
			min(eigenval(Ram)), min(eigenval(Rcm)), min(eigenval(Rem)),
		    min(eigenval(Raf)), min(eigenval(Rcf)), min(eigenval(Ref)))
		),
		mxConstraint(name = "constr", minCor > pos1by6),

		# Matrix & Algebra for expected Mean Matrices in MZ & DZ twins
		mxMatrix(name = "expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, label = paste0(Vars, "Mm")),
		mxMatrix(name = "expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, label = paste0(Vars, "Mf")),
		mxMatrix(name = "expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, label = paste0(Vars, rep(c("Mm", "Mf"), each = nVar))),

		# Matrix & Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
		mxAlgebra(name = "expCovMZm", rbind(cbind(Vm, Am + Cm), cbind(Am + Cm, Vm))),
		mxAlgebra(name = "expCovDZm", rbind(cbind(Vm, 0.5 %x% Am + Cm), cbind(0.5 %x% Am + Cm, Vm))),
		mxAlgebra(name = "expCovMZf", rbind(cbind(Vf, Af + Cf), cbind(Af + Cf, Vf))),
		mxAlgebra(name = "expCovDZf", rbind(cbind(Vf, 0.5 %x% Af + Cf), cbind(0.5 %x% Af + Cf, Vf))),
		mxAlgebra(name = "expCovDZo", rbind(cbind(Vm, 0.5 %x% Amf + Cmf), cbind(0.5 %x% t(Amf) + t(Cmf), Vf)))
		), # end of top

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
	)
	return(m1)
}