#
#   Copyright 2007-2017 Copyright 2007-2017 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

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
#' @param suffix Suffix used for twin variable naming. Allows using just the base names in selVars
#' @return - CF SexLim model
#' @export
#' @family Twin Modeling Functions
#' @references - Neale et al. (2006). 
#' Multivariate genetic analysis of sex-lim and GxE interaction.
#' \emph{Twin Research & Human Genetics}, \bold{9}, pp. 481--489. 
#' @examples
#' \dontrun{
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' require(umx)
#' data("us_skinfold_data")
#' # rescale vars
#' us_skinfold_data[,c('bic_T1', 'bic_T2')] <- us_skinfold_data[,c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[,c('tri_T1', 'tri_T2')] <- us_skinfold_data[,c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[,c('caf_T1', 'caf_T2')] <- us_skinfold_data[,c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[,c('ssc_T1', 'ssc_T2')] <- us_skinfold_data[,c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[,c('sil_T1', 'sil_T2')] <- us_skinfold_data[,c('sil_T1', 'sil_T2')]/5
#' # psych::describe(us_skinfold_data, skew = FALSE)
#' 
#' # Select Variables for Analysis
#' varList = c('ssc','sil','caf','tri','bic')
#' selVars = tvars(varList, "_T")
#' nVar    = length(selVars)
#' 
#' # Data objects for Multiple Groups
#' mzmData = subset(us_skinfold_data, zyg == 1, selVars)
#' dzmData = subset(us_skinfold_data, zyg == 3, selVars)
#' mzfData = subset(us_skinfold_data, zyg == 2, selVars)
#' dzfData = subset(us_skinfold_data, zyg == 4, selVars)
#' dzoData = subset(us_skinfold_data, zyg == 5, selVars)
#'
#' m1 = umxSexLim(selDVs = varList, sep = "_T",
#'        mzmData = mzmData, dzmData = dzmData, 
#'        mzfData = mzfData, dzfData = dzfData, 
#'        dzoData = dzoData
#' )
#' m1 = mxRun(m1)
#' summary(m1)
#'
#' # ===============================
#' # = 1 Nonscalar Sex Limitation  =
#' # ===============================
#' # Quantitative Sex Differences & Qualitative Sex Differences for A
#' # Male and female paths, plus male and female Ra, Rc and Re between variables
#' # Male-Female correlations in DZO group between 
#' # A factors Rao FREE, Rc constrained across male/female and opp-sex
#' # ===================================================
#' # = Test switching specific a from Males to females =
#' # ===================================================
#' 
#' m2 = umxSetParameters(m1, labels = "asm_.*", free = FALSE, values = 0, regex = TRUE)
#' m2 = umxSetParameters(m1, labels = "asf_.*", free = TRUE , values = 0, regex = TRUE)
#' m2 = mxRun(m2)
#' summary(m2)
#' mxCompare(m2, m1)
#' # ===============================
#' # = 2 Nonscalar Sex Limitation  =
#' # ===============================
#' # Quantitative Sex Differences & Qualitative Sex Differences for C
#' # Male and female paths, plus male and female Ra, Rc and Re between variables
#' # Male-Female correlations in DZO group between C 
#' # factors Rco FREE, Ra constrained across male/female and oppsex
#' 
#' # -------|---------|---------|---------|---------|---------|---------|---------|---------|-----|
#' # 3 Scalar Sex Limitation 
#' # Quantitative Sex Differences but NO Qualitative Sex Differences
#' # Male and female paths, but one set of Ra, Rc and Re between variables (same for male & female)
#' # ---------------------------------------------------------------------------------------------|
#' 
#' # =================================
#  # = Equate m & f R stand by label =
#' # =================================
#' frODiag   <- c(rep(c(FALSE,rep(TRUE,nVar)),nVar-1),FALSE)
#' svODiag   <- c(rep(c(1,rep(.4,nVar)),nVar-1),1)
#' m3 = umxSetParameters(m2, labels = "asm_.*", free = FALSE, values = 0, regex = TRUE)
#' pathRam = mxMatrix(name="Ram", "Stand", nrow= nVar, free = TRUE, values = .4, 
#'			label = laSdiag("ra", nVar), lbound = -1, ubound = 1)
#' pathRaf = mxMatrix(name = "Raf", "Stand", nrow = nVar, free = TRUE, values = .4, 
#'			label=laSdiag("ra", nVar), lbound = -1, ubound = 1)
#' pathRcm = mxMatrix(name="Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, 
#'			label=laSdiag("rc", nVar), lbound = -1, ubound = 1)
#' pathRcf = mxMatrix(name="Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, 
#'			label=laSdiag("rc", nVar), lbound = -1, ubound = 1)
#' pathRem = mxMatrix(name="Rem", "Stand", nrow= nVar, free=TRUE, values = .4, 
#'			label = laSdiag("re", nVar), lbound = -1, ubound = 1)
#' pathRef = mxMatrix(name="Ref", "Stand", nrow= nVar, free=TRUE, values = .4, 
#'			label = laSdiag("re", nVar), lbound = -1, ubound = 1)
#' corRao  = mxMatrix(name="Rao", "Symm" , nrow= nVar, free = frODiag, values = svODiag,
#'          label = laSymm("ra", nVar), lbound = -1, ubound = 1)
#' corRco  = mxMatrix(name="Rco", "Symm" , nrow= nVar, free = frODiag, values = svODiag, 
#'          label = laSymm("rc", nVar), lbound = -1, ubound = 1)
#' 
#' # m3 <- makeModel("HetCfAce")
#' # m3 <- mxRun(m3)
#' # summary(m3)
#' # round(m3$VarsZm$result,4); round(m3$CorsZm$result,4)
#' # round(m3$VarsZf$result,4); round(m3$CorsZf$result,4)
#' # mxCompare(HetCfAceRgFit, m3)
#' 
#' # ===================
#' # = 4 Homogeneity 
#' # = NO Quantitative Sex Differences AND NO Qualitative Sex Differences
#' # = Same paths for males and females
#' # ===================
#' 
#' # =====================================
#' # = Equate [ace]m and [ace]f matrices =
#' # =====================================
#' 
#' pathAm = mxMatrix(name="am", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("a", nVar))
#' pathCm = mxMatrix(name="cm", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("c", nVar))
#' pathEm = mxMatrix(name="em", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("e", nVar))
#' pathAf = mxMatrix(name="af", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("a", nVar))
#' pathCf = mxMatrix(name="cf", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("c", nVar))
#' pathEf = mxMatrix(name="ef", "Diag", nrow = nVar, free = TRUE, values = .5, 
#'	label = laDiag("e", nVar))
#' 
#' # m4 <- makeModel("HomCfAce")
#' # m4 <- mxRun(m4)
#' # summary(m4)
#' # round(m4$VarsZm$result,4); round(m4$CorsZm$result,4)
#' # round(m4$VarsZf$result,4); round(m4$CorsZf$result,4)
#' # mxCompare(m3, m4)
#' 
#' # ==============================================
#' # = Generate Output Table of all Nested Models =
#' # ==============================================
#' 
#' # mxCompare(HetCfAceRgFit, c(HetCfAceRcFit, m3, m4))
#' 
#' # rbind(
#' # 		mxCompare(HetCfAceRgFit, HetCfAceRcFit),
#' #  	mxCompare(HetCfAceRcFit, m3)[2,],
#' #  	mxCompare(m3, m4)[2,]
#' # )
#' }
umxSexLim <- function(name = "sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, C_or_A = "A", sep = NA){
	# Correlated factors sex limitations
	# stop("Don't use! Not checked!")
	suffix = sep
	if(is.na(suffix)){
		stop("Please provide sep (e.g. '_T')")
	}
	selVars = umx_paste_names(selDVs, suffix, 1:2)
	# Algebra to Constrain Correlation Matrices to be Positive Definite

	# Decent starts for means
	obsMean = umx_means(mzmData[,1:nVar])
	svMe = colMeans(mzmData[,1:nVar], na.rm = TRUE)
	# dimnames for Algebras generated to hold Parameter Estimates and Derived Variance Components
	colZm <- paste0(varList, rep(c('Am', 'Cm', 'Em'), each = nVar))
	colZf <- paste0(varList, rep(c('Af', 'Cf', 'Ef'), each = nVar))

	m1 = mxModel(name,
		mxModel("top",
		# Matrices a, c, and e to store Path Coefficients
		umxMatrix("am", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		umxMatrix("cm", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		umxMatrix("em", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		umxMatrix("af", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		umxMatrix("cf", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),
		umxMatrix("ef", "Diag" , nrow = nVar, free = TRUE, values = .5, lbound = .0001),

		umxMatrix("Ram", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		umxMatrix("Rcm", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		umxMatrix("Rem", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		umxMatrix("Raf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		umxMatrix("Rcf", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),
		umxMatrix("Ref", "Stand", nrow = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1),

		# Opposite-Sex parameters: Rao, Rco, Amf, Cmf
		if(C_or_A == "A"){
			# Quantitative & Qualitative Sex Differences for C
			{
				Rao = mxMatrix(name = "Rao", "Full", nrow = nVar, ncol = nVar, free = TRUE, values =  1, lbound = -1, ubound = 1)
				Rco = mxMatrix(name = "Rco", "Symm", nrow = nVar, ncol = nVar, free = TRUE, values = .4, lbound = -1, ubound = 1)
				# Fix diag of corRco @1
				diag(Rco$free)   = FALSE
				diag(Rco$values) = 1
				list(Rao, Rco)
			}
		} else if (C_or_A == "C"){
			# Quantitative & Qualitative Sex Differences for C
			{
				Rao = mxMatrix(name = "Rao", "Symm", nrow=nVar, ncol=nVar, free=TRUE, values=.4, lbound=-1, ubound=1)
				Rco = mxMatrix(name = "Rco", "Full", nrow=nVar, ncol=nVar, free=TRUE, values= 1, lbound=-1, ubound=1)
				# Fix diag of corRco @1
				diag(Rao$free)   = FALSE
				diag(Rao$values) = 1
				list(Rao, Rco)
			}
		},

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
		umxMatrix("expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(selDVs, "Mm")),
		umxMatrix("expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(selDVs, "Mf")),
		umxMatrix("expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(selDVs, rep(c("Mm", "Mf"), each = nVar))),

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