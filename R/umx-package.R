#   Copyright 2007-2022 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# An institution is the lengthened shadow of one man. Emerson.
# Plays to watch: "Copenhagen" (Michael Frein); "The chemistry between them" (Dorothy Hodgkin)

.onAttach <- function(libname, pkgname){
	# umx_set_condensed_slots(FALSE)
	umx_set_plot_format('DiagrammeR')
	umx_set_dollar_symbol(umx.dollar.symbol = "$") # \u00A3 = GBP
	umx_set_plot_file_suffix(umx.plot.suffix = "gv")
	umx_set_silent(FALSE)

	# if(is.null(getOption('knitr.table.format'))){
	# 	umx_set_table_format('markdown')
	# 	options('knitr.table.format' = "markdown")
	# }
	umx_set_auto_plot(TRUE)
	umx_set_auto_run(TRUE)
	umx_set_data_variance_check(minVar = .1, maxVarRatio = 1000)
	umx_set_separator(umx_default_separator = ",")
	umx_set_silent(FALSE)
	umx_set_table_format("markdown")
	umx_complete_dollar()
	packageStartupMessage("For an overview type '?umx'")
}


# Forgettable functions
# MASS mvrnorm   # Used in umx_make_TwinData
# nlme intervals # Used in umxAPA
# polycor hetcor # Used in umxHetCor
# xtable xtable  # Used in umxCompare
# MuMIn Weights  # Used in umxWeightedAIC
#  methods::setClass is called during build not package source code.

#' @importFrom DiagrammeR grViz
#' @importFrom DiagrammeR DiagrammeR
#' @importFrom graphics plot abline hist par
#' @importFrom methods as getSlots is slotNames setClass
#' @importFrom stats AIC C aggregate as.formula coef complete.cases
#' @importFrom stats confint cor cov cov.wt cov2cor df lm cor.test dnorm pnorm reshape
#' @importFrom stats logLik na.exclude na.omit pchisq pf qchisq predict
#' @importFrom stats qnorm quantile reformulate residuals rnorm runif sd
#' @importFrom stats setNames update var delete.response terms model.frame
#' @importFrom utils combn data flush.console read.table txtProgressBar
#' @importFrom utils globalVariables write.table packageVersion
#' @importFrom utils browseURL install.packages str read.csv read.delim capture.output
#' @importFrom utils tail
#' @importFrom scales dollar
#' @importFrom quantmod Cl
#' @importFrom zoo index
#' @importFrom MASS mvrnorm
#' @importFrom nlme intervals lme
#' @importFrom polycor hetcor
#' @importFrom xtable xtable
#' @importFrom MuMIn Weights
#' @importFrom ggplot2 ggplot qplot ggtitle ylab xlab labs
#' @importFrom ggplot2 scale_x_continuous scale_x_continuous scale_y_continuous theme 
#' @importFrom ggplot2 geom_abline geom_bar geom_curve geom_errorbar geom_hline geom_jitter geom_line 
#' @importFrom ggplot2 geom_point geom_ribbon geom_segment geom_smooth  geom_vline arrow unit
#' @importFrom ggplot2 aes aes_string annotate coord_cartesian element_blank element_text 
#' @importFrom ggplot2 ggtitle xlab ylab scale_fill_hue expand_limits position_dodge 
#' @importFrom ggplot2 theme_bw theme_gray theme_minimal
#' @importFrom cowplot draw_label plot_grid ggdraw 
#' @importFrom knitr kable
#' @importFrom kableExtra kbl add_footnote column_spec footnote
#' @importFrom kableExtra kable_classic kable_classic_2 kable_minimal kable_material kable_material_dark kable_paper

# #' @importFrom Base::charToRaw
# #' @importFrom DiagrammeRsvg export_svg
# #' @importFrom rsvg rsvg_png rsvg_pdf


utils::globalVariables(c(
	## from Doc 
	"top.ManMean",
	'top.bCov',
	 'top.meanT1',
	 'top.meanT2MZ',
	 'top.meanT2DZ',
	 'top.meanT2sib',
	'psi_a',
	'psi_c',
	'psi_e',
	'unitM',
	'Mean',
	'FacMean',
	 'B',
	 'lamba',
	 'solvBE',
	 'psi_a',
	 'psi_c',
	 'psi_e',
	 'filter',
	 'Ts_',
	 'K',
	 'CovMZ_',
	
	## from main umx files
	"xFamMean", "Bwithin",
	"xDiff",
	"yDiff",
    'ci.lower',
	'ci.upper',
	'r',
	'xLevel',
	'N',
	'x',
	'xtable',
	'M', 'S',
	'A', 'E',
	'a', 'c', 'e', 
	'Am', 'Cm', 'Em',
	'am', "cm",'em',
	'Af', 'Cf', 'Ef',
	'af', 'cf', 'ef',
	"as", 'cs', 'es',
	'ai', 'ci', 'ei',

	'a_cp', 'c_cp', 'e_cp',
	'A_cp', 'C_cp', 'E_cp', 

	'ACE', 'AC', "ACf", "ACm", 'hAC', 'hACf', 'hACm',
	'ACEf', "ACEm",
	'A11', 'A12', 'A21', 'A22', 'E11', 'E22','C11', 'C12', 'C21', 'C22', 

	'ACE.Af', 'ACE.Am', 'ACE.Cf', 'ACE.Cm', 'ACE.Ef', 'ACE.Em',
	"ACE.af", "ACE.am", "ACE.cf", "ACE.cm", "ACE.ef", "ACE.em", 
	'ACE.Vf', 'ACE.Vm',

	'Amf', 'Cmf', 
	'Asm', 'Asf', 'Csm', 'Csf',
	'asf', 'asm', 'csf', 'csm',
	'rAf', 'rAm', 'rCf', 'rCm', 'rEf', 'rEm',
	'top.betaLin', 'top.betaQuad',
	'top.a', 'top.c', 'top.e',
	'top.A', 'top.C', 'top.E',
	'top.a_std', 'top.c_std', 'top.e_std',
	'top.A', 'top.a', 'top.af', 'top.ai', 'top.am', 'top.as', 'top.a_cp',
	'top.C', 'top.c', 'top.cf', 'top.ci', 'top.cm', 'top.cs', 'top.c_cp', 
	'top.E', 'top.e', 'top.ef', 'top.ei', 'top.em', 'top.es', 'top.e_cp', 

	'top.expCovDZ', 'top.expCovMZ', 'top.expMeanDZ', 'top.expMeanMZ',
	'top.Means', 'top.nSib', 'top.nVar', 'top.cp_loadings',

	'common_loadings','cp_loadings', 
	'CorMFa', 'CorMFc', 'covT1', 'covT2',  'Def1', 'Def2', 'Def1Rlin', 'Def1Rquad',  'Def2Rlin', 'Def2Rquad', 'L', 'diagL',

	'DZ.covsT1DZ'   , 'MZ.covsT1MZ'   ,
	'DZ.covsT2DZ'   , 'MZ.covsT2MZ'   ,
	'DZ.objective'  , 'MZ.objective'  ,
	'DZf.objective' , 'MZf.objective' ,
	'DZff.objective', 'MZff.objective',
	'DZm.objective' , 'MZm.objective' ,
	'DZmm.objective', 'MZmm.objective',
	'DZfm.objective',

	'Mf', 'Mm',
	'MZW', 'DZW',
	'fmCOV','mfCOV',
	'newLbound',
	# from umxACEv
	'InvSD',
	# from umxACEcov
	'varStarts',
	'lowerB',
	'lowerW',
	'CovB',
	'CovW',
	'CovWB',
	'bCovWBb',
	'bCovBb',
	'bCovWB',
	'bCovB',
	'CovWBb',
	'CovBb',

	# from umxACE_cov_fixed
	'top.Intercepts',
	'defCovT1',
	'top.betas',
	'defCovT2',
	'defCovT1',
	'defCovT2',
	'top.Intercepts',
	'top.betas',

	'Iden',
	'nDv',
	

	'meanDZ', 'meanMZ',

	'nFac_UnitCol', 'nFac_Iden', 'nVar', 'nVar2', 'nVarIdenMatrix', 'nVarUnit', 'betas', 

	'oneTwinMeans', 'predMeanT1DZ', 'predMeanT1MZ', 'predMeanT2DZ', 'predMeanT2MZ',

	'R', 'Ra', 'Raf', 'Ram', 'Rc', 'Rcf', 'Rcm', 'Ref', 'Rem',
	'thresholdDeviations', 'totalVariance', 'UnitLower',
	'V', 'Vf', 'Vm', 'Vtot', 
	"C", "logLik", "var", 'SD', 'StdDev',
	'binLabels', 'Unit_nBinx1',
	
	'correlatedA', 'minCor', 'pos1by6', 'varList', 'Im1', 'IZ', 'ZI', 'Z',

	# umxDOC
	"dzAr", "Vmz", "Vdz",
	"FacLoadtw", "cause", "Diag1", "Ones",
	"Asmz", "Asdz", "Cstw", "Estw",
	"specCovMZ", "specCovDZ", "FacCovMZ", "FacCovDZ",
	
	# Used in tmx_genotypic_effect
	"x1", "y1", "y2", "dose", "value", "freq", "Frequency", 
	
	# Used in umxGxE
    "DefT1", "DefT2", "top.betaCoTwin", "top.betaSelf",

	# Used in umxGxEbiv
	"mod1", "mod2", 
	"Adz", "Amz",
	"PsAdz"    , "PsAmz"    , "PsC", 
	"top.PsAdz", "top.PsAmz", "top.PsC",
	"chA", "chC", "chE", 
	"top.a11", "top.a21", "top.a22", 
	"top.c11", "top.c21", "top.c22", 
	"top.e11", "top.e21", "top.e22", 
	"top.aBeta1", "top.aBeta2", "top.eBeta1", "top.eBeta2", "top.cBeta1", "top.cBeta2",
	
	# Used in umxSimplex
	"at" , "ct" , "et", 
    "Iai", "Ici", "Iei", 
	"SigmaA"   , "SigmaC"     , "SigmaE", 
	"SigmaPh11", "SigmaPh21dz", "SigmaPh21mz",
	
	# Used in poly funs
	"minus2SatLogLik", "nCells", "diffchi",
	 
	# Used in xmu_lavaan_process_group
	"constraintOps", "handledOps",
	# Used in power.ACE.test
	"zygosity",
	
	# xmu_twin_add_WeightMatrices
	"mzWeightMatrix",
	"dzWeightMatrix",
	# xmuTwinUpgradeMeansToCovariateModel
    "top.intercept",
	"top.meansBetas",
	"T1DefVars",
	"T2DefVars",
	"T3DefVars",
	#MRDoC2
	'dzmu',
	'BE',
	'A_',
	'C_',
	'E_',
	'SPh',
	'Smz_',
	# fin functions
	'loanRatio',
	'netGainLoss'
	
))

# ===================================================================
# = Define some class containers to allow specialised model objects =
# = plot, etc. can then operate on these                            =
# ===================================================================
methods::setClass("MxModelDoC"      , contains = "MxModel")
methods::setClass("MxModelMRDoC"    , contains = "MxModel")
methods::setClass("MxModelTwinMaker", contains = "MxModel")

methods::setClass("MxModelACE"    , contains = "MxModel")
methods::setClass("MxModelACEv"   , contains = "MxModelACE")
methods::setClass("MxModelACEcov" , contains = "MxModel")

methods::setClass("MxModelIP"     , contains = "MxModel")
methods::setClass("MxModelCP"     , contains = "MxModel")
methods::setClass("MxModelGxE"    , contains = "MxModel")
methods::setClass("MxModelGxEbiv" , contains = "MxModelGxE")
methods::setClass("MxModelSexLim" , contains = "MxModel")
methods::setClass("MxModelSimplex", contains = "MxModel")
methods::setClass("MxModelGSEM"   , contains = "MxRAMModel")

