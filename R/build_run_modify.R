#   Copyright 2007-2020 Timothy C. Bates
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

# =======================
# = Markdown hyperlinks =
# =======================
# [func()] = function in package = \code{\link[=func]{func()}  Another package =  [pkg::func()]
# [object] = object, class, data set, etc. in same package. Another package [pkg::object]
# [`object`] = object, typeset as code
# [link text][object] = Different link text. Another package = [link text][pkg::object]

# ![](example-plot.jpg "Example Plot Title") = Image in man/figures

# cran_prep <- check_for_cran("~/bin/umx/", show_status=FALSE)
# cran_prep$cran_summary()

# Makevars for clang
# 	mate ~/.R/Makevars
# 	https://gist.github.com/tbates/9cec0a93e04c06c41b550454eaa892a3
# 
# for Mojave
# sudo installer -pkg /Library/Developer/CommandLineTools/Packages/macOS_SDK_headers_for_macOS_10.14.pkg -target /
# 
# Plays to watch: "Copenhagen" (Michael Frein); "The chemistry between them" (Dorothy Hodgkin)
#
# ===============================
# = Highlevel models (ACE, GxE) =
# ===============================
.onAttach <- function(libname, pkgname){
	# umx_set_condensed_slots(FALSE)
	umx_set_plot_format('DiagrammeR')
	umx_set_plot_file_suffix("gv")
	umx_set_plot_use_hrbrthemes(FALSE)
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
	# umx_complete_dollar()
	packageStartupMessage("For an overview type '?umx'")
}

# #' @importFrom Base::charToRaw
#' @importFrom DiagrammeR DiagrammeR grViz
#' @importFrom DiagrammeRsvg export_svg
#' @importFrom rsvg rsvg_png rsvg_pdf
#' @importFrom graphics plot abline
#' @importFrom methods as getSlots is slotNames setClass
# methods::setClass is called during build not package source code.
# suppress NOTE with a spurious importFrom in the namespace
#' @importFrom stats AIC C aggregate as.formula coef complete.cases
#' @importFrom stats confint cor cov cov.wt cov2cor df lm cor.test dnorm pnorm
#' @importFrom stats logLik na.exclude na.omit pchisq pf qchisq
#' @importFrom stats qnorm quantile residuals rnorm runif sd
#' @importFrom stats setNames update var delete.response terms
#' @importFrom utils combn data flush.console read.table txtProgressBar
#' @importFrom utils globalVariables write.table packageVersion
#' @importFrom utils browseURL install.packages str read.csv

#' @importFrom cowplot draw_label plot_grid ggdraw 
#' @importFrom ggplot2 ggplot qplot ggtitle ylab xlab labs
#' @importFrom ggplot2 scale_x_continuous scale_x_continuous theme 
#' @importFrom ggplot2 geom_point geom_segment geom_line geom_ribbon
#' @importFrom ggplot2 element_text element_blank expand_limits aes
#' @importFrom kableExtra kbl add_footnote column_spec
#' @importFrom kableExtra kable_classic kable_classic_2 kable_minimal kable_material kable_material_dark kable_paper

# # ' @importFrom knitr

# Used in plot
#' @importFrom DiagrammeR DiagrammeR

# Used in umx_make_TwinData
#' @importFrom MASS mvrnorm

# Used in umxAPA
#' @importFrom nlme intervals

# Used in umxHetCor
#' @importFrom polycor hetcor

# Used in umxCompare
#' @importFrom xtable xtable

# Used in umxWeightedAIC
#' @importFrom MuMIn Weights


# Not used?
NULL

utils::globalVariables(c(
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

	'nFac_Unit', 'nVar', 'nVar2', 'nVarIdenMatrix', 'nVarUnit', 'betas', 

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
	"T3DefVars"
))

# ===================================================================
# = Define some class containers to allow specialised model objects =
# = plot, etc. can then operate on these                             =
# ===================================================================
methods::setClass("MxModelDoC"    , contains = "MxModel")
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


# ============================
# = Core Modeling Functions =
# ============================

#' Catches users typing umxModel instead of umxRAM.
#'
#' @description
#' Catches a common typo, moving from mxModel to umx.
#'
#' @param ... Anything. We're just going to throw an error.
#' @return None
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()], [mxModel()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' umxModel()
#' }
umxModel <- function(...) {
	stop("You probably meant umxRAM?, not umxModel?")
}

#' Easier path-based SEM modeling.
#'
#' @description
#' `umxRAM` expedites creation of structural equation models, still without doing invisible things to the model. It 
#' supports [umxPath()] but also lavaan-style string specification of models: lavaan's scripting language has become a 
#' lingua franca for SEM books, so supporting this improves science learning.
#' 
#' Here's a path example that models miles per gallon (mpg) as a function of weight (wt) and engine displacement (disp)
#' using the widely used `mtcars` data set.
#' 
#' ```Rsplus
#' m1 = umxRAM("tim", data = mtcars,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(v.m. = c("wt", "disp", "mpg"))
#' )
#' ```
#'
#' As you can see, most of the work is done by [umxPath()]. `umxRAM` wraps these paths up, takes the `data =` input, and 
#' then internally sets up all the labels and start values for the model, runs it, and calls [umxSummary()], and [plot.MxModel()].
#' 
#' Try it, or one of the several models in the examples at the bottom of this page.
#' 
#' A common error is to include data in the main list, a bit like
#' saying `lm(y ~ x + df)` instead of `lm(y ~ x, data = df)`.
#' 
#' **nb**: Because it uses the presence of a variable in the data to detect if a variable is latent or not, `umxRAM` needs data at build time.
#'
#' **String Syntax**
#' 
#' Here is an example using lavaan syntax (for more, see [umxLav2RAM()])
#' 
#' ```Rsplus
#' m1 = umxRAM("mpg ~ wt + disp", data = mtcars)
#' ```
#'
#' **Sketch mode**
#'
#' If you are at the "sketching" stage of theory consideration, `umxRAM` supports
#' a simple vector of manifest names to work with.
#' 
#' ```Rsplus
#' m1 = umxRAM("sketch", data = c("A", "B", "C"),
#' 	umxPath("A", to = "B"),
#' 	umxPath("B", with = "C"),
#' 	umxPath(v.m. = c("A", "B", "C"))
#' )
#' ```
#' Will create this figure:
#' 
#' \if{html}{\figure{sketch.png}{options: width="50\%" alt="Figure: sketch.png"}}
#' \if{latex}{\figure{sketch.pdf}{options: width=7cm}}
#' 
#' @details
#' 
#' **Comparison with OpenMx and mxModel**
#' 
#' umxRAM differs from mxModel in the following ways:
#' 
#' 1. You don't need to set type = "RAM".
#' 2. You don't need to list manifestVars (they are detected from path usage).
#' 3. You don't need to list latentVars (detected as anything in paths but not in \code{mxData}).
#' 4. You don't need to create mxData when you already have a data.frame.
#' 5. You add data with `data = ` (as elsewhere in R, e.g. [lm()]).
#' 6. You don't need to add labels: paths are automatically labelled "a_to_b" etc.
#' 7. You don't need to set start values, they will be done for you.
#' 8. You don't need to `mxRun` the model: it will run automatically, and print a summary.
#' 9. You don't need to run `summary`: with `autoRun=TRUE`, it will print a summary.
#' 10. You get a plot of the model with estimates on the paths, including multiple groups.
#' 11. Less typing: [umxPath()] offers powerful verbs to describe paths.
#' 12. Supports a subset of lavaan string input.
#'
#' **Start values**. Currently, manifest variable means are set to the observed means, residual variances are set to 80% 
#' of the observed variance of each variable, 
#' and single-headed paths are set to a positive starting value (currently .9).
#' *note*: The start-value strategy is subject to improvement, and will be documented in the help for [umxRAM()].
#' 
#' **Comparison with other software**
#' 
#' **Black-box software, defaults, and automatic addition of paths**.
#' 
#' Some SEM software does a lot of behind-the-scenes defaulting and path addition. 
#' If you want this, I'd say use `umxRAM` with lavaan string input.
#' 
#' @param model A model to update (or set to string to use as name for new model)
#' @param data data for the model. Can be an [mxData()] or a data.frame
#' @param ... umxPaths, mxThreshold objects, etc.
#' @param group (optional) Column name to use for a multi-group model (default = NULL)
#' @param group.equal In multi-group models, what to equate across groups (default = NULL: all free)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param suffix String to append to each label (useful if model will be used in a multi-group model)
#' @param name A friendly name for the model
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param std Whether to show standardized estimates, raw (NULL print fit only)
#' @param optimizer optionally set the optimizer (default NULL does nothing)
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param setValues Whether to generate likely good start values (Defaults to TRUE)
#' @param refModels pass in reference models if available. Use FALSE to suppress computing these if not provided.
#' @param independent Whether the model is independent (default = NA)
#' @param remove_unused_manifests Whether to remove variables in the data to which no path makes reference (defaults to TRUE)
#' @param verbose Whether to tell the user what latents and manifests were created etc. (Default = FALSE)
#' @param std.lv Whether to auto standardize latent variables when using string syntax (default = FALSE)
#' @param lavaanMode Defaults when building out string syntax default = "sem" (alternative is "lavaan", with very few defaults)
#' @param printTab (for string input, whether to output a table of paths (FALSE)
#' @return - [mxModel()]
#' @export 
#' @seealso [umxPath()], [umxSummary()], [plot()], [parameters()], [umxSuperModel()], [umxLav2RAM()]
#' @family Core Model Building Functions
#' @references - <https://tbates.github.io>, <https://github.com/tbates/umx>
#' @md
#' @examples
#' 
#' # ============================================
#' # = 1. Here's a simple example with raw data =
#' # ============================================
#' mtcars$litres = mtcars$disp/61.02
#' m1 = umxRAM("tim", data = mtcars,
#' 	umxPath(c("wt", "litres"), to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("wt", "litres", "mpg"))
#' )
#'
#' # 2. Use parameters to see the parameter estimates and labels
#' parameters(m1)
#'
#' # And umxSummary to get standardized parameters, CIs etc from the run model.
#' umxSummary(m1, std=TRUE)
#' # |name           | Std.Estimate| Std.SE|CI                   |
#' # |:--------------|------------:|------:|:--------------------|
#' # |wt_to_mpg      |        -0.54|   0.17|-0.54 [-0.89, -0.2]  |
#' # |disp_to_mpg    |        -0.36|   0.18|-0.36 [-0.71, -0.02] |
#' # |mpg_with_mpg   |         0.22|   0.07|0.22 [0.08, 0.35]    |
#' # |wt_with_wt     |         1.00|   0.00|1 [1, 1]             |
#' # |b1             |         0.89|   0.04|0.89 [0.81, 0.96]    |
#' # |disp_with_disp |         1.00|   0.00|1 [1, 1]             |
#' 
#' \dontrun{
#' # 3. Of course you can plot the model
#' plot(m1)
#' plot(m1, std=TRUE, means=FALSE)
#' plot(m1, std = TRUE, means=FALSE, strip= TRUE, resid = "line")
#'
#' # ===============================================
#' # = lavaan string example (more at ?umxLav2RAM) =
#' # ===============================================
#' m1 = umxRAM(data = mtcars, "#modelName
#'  mpg ~ wt + disp")
#' 
#'
#' # =======================
#' # = A multi-group model =
#' # =======================
#'
#' mtcars$litres = mtcars$disp/61.02
#' m1 = umxRAM("tim", data = mtcars, group = "am",
#' 	umxPath(c("wt", "litres"), to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("wt", "litres", "mpg"))
#' )
#' # In this model, all parameters are free across the two groups.
#'
#' # ====================================
#' # = A cov model, with steps laid out =
#' # ====================================
#'
#' # *note*: The variance of displacement is in cubic inches and is very large.
#' # to help the optimizer, one might, say, multiply disp *.016 to work in litres
#' tmp = mtcars; tmp$disp= tmp$disp *.016
#'
#' # We can just give the raw data and ask for it to be made into type cov:
#' m1 = umxRAM("tim", data = tmp, type="cov",
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(var = c("mpg", "wt", "disp"))
#' )
#'
#' # (see ?umxPath for more nifty options making paths...)
#'
#' # =========================================
#' # = umxRAM can also accept mxData as data =
#' # =========================================
#' # For convenience, list up the manifests you will be using
#' 
#' selVars = c("mpg", "wt", "disp")
#' tmp = mtcars; tmp$disp= tmp$disp *.016
#' myCov = mxData(cov(tmp[, selVars]), type = "cov", numObs = nrow(mtcars) )
#'
#' m1 = umxRAM("tim", data = myCov,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(var = selVars)
#' )
#' 
#' 
#' # =======================
#' # = umxRAM supports WLS =
#' # =======================
#'
#' # 1. Run an all-continuous WLS model
#'  mw = umxRAM("raw", data = mtcars[, c("mpg", "wt", "disp")], 
#'		type = "WLS", allContinuousMethod = "cumulants",
#'  	umxPath(var = c("wt", "disp", "mpg")),
#'  	umxPath(c("wt", "disp"), to = "mpg"),
#'  	umxPath("wt", with = "disp"),
#'      umxPath(var = c("wt", "disp", "mpg"))
#'  )
#' # 2. Switch to marginals to support means
#'  mw = umxRAM("raw", data = mtcars[, c("mpg", "wt", "disp")], 
#'		type = "WLS", allContinuousMethod= "marginals",
#'  	umxPath(var = c("wt", "disp", "mpg")),
#'  	umxPath(c("wt", "disp"), to = "mpg"),
#'  	umxPath("wt", with = "disp"),
#'      umxPath(var = c("wt", "disp", "mpg"))
#'  )
#'
#' 
#' # ===============================
#' # = Using umxRAM in Sketch mode =
#' # ===============================
#' # No data needed: just list variable names!
#' # Resulting model will be plotted automatically
#' m1 = umxRAM("what does unique pairs do, I wonder", data = c("A", "B", "C"),
#'	   umxPath(unique.pairs = c("A", "B", "C"))
#' )
#' 
#' m1 = umxRAM("ring around the rosey", data = c("B", "C"),
#'	  umxPath(fromEach = c("A", "B", "C"))
#' )
#' 
#' m1 = umxRAM("fromEach with to", data = c("B", "C"),
#'	   umxPath(fromEach = c("B", "C"), to= "D")
#' )
#'
#' m1 = umxRAM("CFA_sketch", data = paste0("x", 1:4),
#' 	umxPath("g", to = paste0("x", 1:4)),
#' 	umxPath(var = paste0("x", 1:4)),
#' 	umxPath(v1m0 = "g")
#' )
#'
#' # =================================================
#' # = This is an example of using your own labels:  =
#' #   umxRAM will not over-ride them                =
#' # =================================================
#' m1 = umxRAM("tim", data = mtcars, type="cov",
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath(cov = c("wt", "disp"), labels = "b1"),
#' 	umxPath(var = c("wt", "disp", "mpg"))
#' )
#' omxCheckEquals(m1$S$labels["disp", "wt"], "b1") # label preserved
#' m1$S$labels
#'#      mpg             wt            disp
#'# mpg  "mpg_with_mpg"  "mpg_with_wt" "disp_with_mpg"
#'# wt   "mpg_with_wt"   "wt_with_wt"  "b1"
#'# disp "disp_with_mpg" "b1"          "disp_with_disp"
#' parameters(m1)
#'
#' }
#'
umxRAM <- function(model = NA, ..., data = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, lavaanMode = c("sem", "lavaan"), printTab = FALSE) {
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	dot.items = unlist(dot.items) # In case any dot items are lists of mxPaths, etc...
	type       = match.arg(type)
	tryHard    = match.arg(tryHard)
	lavaanMode = match.arg(lavaanMode)
	allContinuousMethod = match.arg(allContinuousMethod)

	# if data provided check it isn't a tibble
	if(!is.null(data)){
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
	}

	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(!is.null(group)){
		if(class(data) != "data.frame"){
			stop("Currently, for multiple groups, data must be a raw data.frame so I can subset it into multiple groups. You gave me a ", omxQuotes(class(data)))
		}
	}

	# lavaan string style model
	if (is.character(model) && grepl(model, pattern = "(<|~|=~|~~|:=)")){
		# Process lavaanString: need to modify so that all the RAM options are processed: 
		# type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
		# show
		# suffix
		# refModels = NULL
		# comparison
		# allContinuousMethod
		# remove_unused_manifests
		model = umxLav2RAM(model = model, data = data, type = type, group = group, group.equal = group.equal, std.lv = std.lv, name = name, 
					lavaanMode = lavaanMode, autoRun = autoRun, tryHard = tryHard, printTab = printTab)
		return(model)
	}


	# umxPath-based model
	if(typeof(model) == "character"){
		if(is.na(name)){
			name = model
		} else {
			stop("If model is set to a string, don't pass in name as well...")
		}
	} else {
		if(umx_is_RAM(model)){
			# message("Updating existing model")
			if(is.na(name)){
				name = model$name
			}
			if(is.null(data)){
				newModel = mxModel(model, dot.items, name = name)
			} else {
				if(umx_is_MxData(data)){
					newModel = mxModel(model, dot.items, data, name = name)
				} else {
					stop("Polite note: I don't know how to convert raw data into mxData to update your model - can you please do that for me and try again?")
					# newModel = mxModel(model, dot.items, data, name = name)
				}
			}
			# if(setValues){
			# 	newModel = xmuValues(newModel)
			# }
			newModel = xmu_safe_run_summary(newModel, autoRun = autoRun,  tryHard =  tryHard)
			return(newModel)
		} else {
			stop("First item must be either an existing model or a name string. You gave me a ", typeof(model))
		}
	}

	umx_check(!is.null(data), "stop", "In umxRAM, you must set 'data = '. If you're building a model with no data, use mxModel")

	foundNames = c()
	defnNames = c()
	for (thisItem in dot.items) {
		if(!is.list(thisItem)){
			# Sometimes we get a list, so expand everything to a list.
			thisItem = list(thisItem)
		}
		for (i in length(thisItem)) {
			thisIs = class(thisItem[[i]])[1]
			if(thisIs == "MxPath"){
				foundNames = append(foundNames, c(thisItem[[i]]$from, thisItem[[i]]$to))
				tmp = namez(thisItem[[i]]$labels, "data\\.")
				if(length(tmp) > 0){
					defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
				}
			} else {
				if(thisIs == "MxThreshold"){
					# MxThreshold detected
				} else if(umx_is_MxMatrix(thisItem[[i]])){
					# matrix labels might refer to definition variables
					tmp = namez(thisItem[[i]]$labels, "data\\.")
					if(length(tmp) > 0){
						defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
					}
				} else {
					# TODO: umxRAM currently not checking for unsupported items.
					# stop("I can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
					# "You have given me a", class(i)[1],"\n",
					# " To include data in umxRAM, say 'data = yourData'")
				}
			}			
		}
	}

	# ============================
	# = All dot.items processed  =
	# ============================

	# ====================================
	# = Find latentVars and manifestVars =
	# ====================================
	# Get names from data (forms pool of potential usedManifests)
	manifestVars = unique(na.omit(umx_names(data)))

	# Omit NAs from found names (empty "to =" can generate these spuriously)
	foundNames = unique(na.omit(foundNames))
	defnNames  = unique(na.omit(defnNames))
	
	if(length(defnNames)>0){
		# check'm if you've got'm
		umx_check_names(defnNames, data = data, message = "note: used as definition variable, but not present in data")
	}

	# Anything else used as a path, but not found in the data (and not a key word like "one") must be a latent
	latentVars = setdiff(foundNames, c(manifestVars, "one"))
	nLatent = length(latentVars)
	# Report which latents were created
	if (!umx_set_silent(silent=TRUE)) {
    	if(nLatent == 0){
			# message("No latent variables were created.\n")
			# latentVars = NA
	    } else if (nLatent == 1){
			message("A latent variable '", latentVars[1], "' was created. ")
	    } else {
      	  message(nLatent, " latent variables were created:", paste(latentVars, collapse = ", "), ". ")
    	}
	}

	# ===========================================================
	# = TODO handle user adding an mxThreshold object to umxRAM =
	# ===========================================================
	# This will be a model where things are not in the data and are not latent...
	
	# ======================================
	# = List up used and un-used Manifests =
	# ======================================
	# Used = all data columns present in found and not reserved, e.g. "one"
	unusedManifests = setdiff(manifestVars, c(foundNames, defnNames))

	if(remove_unused_manifests & length(unusedManifests) > 0){
		usedManifests = setdiff(intersect(manifestVars, foundNames), "one")
		myData = xmu_make_mxData(data = data, type = type, manifests = c(usedManifests, defnNames), verbose = verbose)
	} else {
		# keep everything
		usedManifests = manifestVars
		myData = xmu_make_mxData(data= data, type = type, verbose = verbose)
	}
	# ==================
	# = Assemble model =
	# ==================

	newModel = do.call("mxModel", list(name = name, type = "RAM",
		manifestVars = usedManifests,
		latentVars  = latentVars,
		independent = independent, dot.items)
	)
	# ============
	# = Add data =
	# ============
	if (class(myData) == "character"){
		# User is just running a trial model, with no data, but provided names for sketch mode
		newModel = xmuLabel(newModel, suffix = suffix)
		if(is.null(group)){
			if(autoRun && umx_set_auto_plot(silent = TRUE)){
				plot(newModel)
			}
			return(newModel)
		} else {
			# will be added to a super model, but no data needed/available to subset
		}
	}else{
		newModel = mxModel(newModel, myData)
		# note: if necessary (group), will be re-processed to add the required data below...
	}
	
	# ==========================
	# = Add means if necessary =
	# ==========================
	# Note: WLS data will be mxData(..., type = "raw") at this stage.
	# Add means if data are raw and means not requested by user
	needsMeans = xmu_check_needs_means(data = myData, type = type, allContinuousMethod = allContinuousMethod)
	if(needsMeans && is.null(newModel$matrices$M)){
		message("You have raw data, but no means model. I added\n",
		"mxPath('one', to = manifestVars)")
		newModel = mxModel(newModel, mxPath("one", usedManifests))
	}

	# =========================
	# = Labels and set values =
	# =========================
	suffix = ifelse(is.null(group), yes = suffix, no = paste0(suffix, "_GROUP"))
	newModel = xmuLabel(newModel, suffix = suffix)
	if(setValues){
		newModel = xmuValues(newModel, onlyTouchZeros = TRUE)
	}

	if(any(umx_is_ordered(myData$observed))){
		newModel = xmuRAM2Ordinal(newModel, verbose = TRUE)
	}

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
		# message("data treated as ", omxQuotes(type), ". allContinuousMethod = ", omxQuotes(allContinuousMethod))
		# Replace newModel fit functions
		# Still mxExpectationNormal and means not affected (either has or lacks means matrix already).
		newModel = mxModel(newModel, mxFitFunctionWLS(type= type, allContinuousMethod = allContinuousMethod) )
	}

	# =====================
	# = Handle group here =
	# =====================
	if(!is.null(group)){
		# 1. Go back to raw data and subset by "group" column
		# 2. Create new mxData,
		# 3. Add data to copy of the model and accumulate in list of models
		# 4. Add list of models to umxSuperModel
		modelList = list()
		groupCol  = data[, group]
		levelsOfGroup = unique(groupCol)
		# already computed above
		# unusedManifests = setdiff(manifestVars, foundNames)
		# usedManifests   = setdiff(intersect(manifestVars, foundNames), "one")
		# usedManifests = manifestVars
		for (thisLevelOfGroup in levelsOfGroup) {
			thisSubset = data[groupCol == thisLevelOfGroup, ]
			if(remove_unused_manifests & length(unusedManifests) > 0){
				myData = xmu_make_mxData(data = thisSubset, type = type, manifests = c(usedManifests, defnNames), verbose = FALSE)
			} else {
				myData = xmu_make_mxData(data= thisSubset, type = type, verbose = FALSE)
			}
			thisModel = mxModel(newModel, myData, name= paste0(name, "_", thisLevelOfGroup))

			if(!is.null(group.equal)){
				message("sorry, haven't implemented group.equal yet")
			}else{
				# replace "_GROUP$" with _thisLevelOfGroup
				thisModel = umxSetParameters(thisModel, regex= "_GROUP$", newlabels= paste0("_", thisLevelOfGroup))
			}

			modelList = c(modelList, thisModel)
		}
		return(umxSuperModel(name = name, modelList, autoRun = autoRun, tryHard = tryHard, std = std))
	}

	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, std = std)
	invisible(newModel)
}

#' Make a multi-group model
#'
#' @description
#' `umxSuperModel` takes 1 or more models and wraps them in a supermodel with a
#' [mxFitFunctionMultigroup()] fit function that minimizes the sum of the
#' fits of the sub-models.
#'
#' *note*: Any duplicate model-names are renamed to be unique by suffixing `_1` etc.
#'
#' @param name The name for the container model (default = 'super')
#' @param ...  Models forming the multiple groups contained in the supermodel.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param std Show standardized parameters, raw (default), or just the fit indices (null)
#' @return - [mxModel()]
#' @export
#' @family Core Model Building Functions
#' @seealso - [mxFitFunctionMultigroup()], [umxRAM()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' 
#' \dontrun{
#' library(umx)
#' # Create two sets of data in which X & Y correlate ~ .4 in both datasets.
#' manifests = c("x", "y")
#' tmp = umx_make_TwinData(nMZpairs = 100, nDZpairs = 150, 
#' 		AA = 0, CC = .4, EE = .6, varNames = manifests)
#' 
#' # Group 1
#' grp1   = tmp[tmp$zygosity == "MZ", manifests]
#' g1Data = mxData(cov(grp1), type = "cov", numObs = nrow(grp1), means=umx_means(grp1))
#' 
#' # Group 2
#' grp2   = tmp[tmp$zygosity == "DZ", manifests]
#' g2Data = mxData(cov(grp2), type = "cov", numObs = nrow(grp2), means=umx_means(grp2))
#' 
#' 
#' # Model 1 (could add autoRun = FALSE if you don't want to run this as it is being built)
#' m1 = umxRAM("m1", data = g1Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels = c("Var_x", "Resid_y_grp1")),
#' 	umxPath(means = manifests, labels = c("Mean_x", "Mean_y"))
#' )
#' 
#' # Model 2
#' m2 = umxRAM("m2", data = g2Data,
#' 	umxPath("x", to = "y", labels = "beta"),
#' 	umxPath(var = manifests, labels=c("Var_x", "Resid_y_grp2")),
#' 	umxPath(means = manifests, labels=c("Mean_x", "Mean_y"))
#' )
#' 
#' # Place m1 and m2 into a supermodel, and autoRun it
#' # NOTE: umxSummary is only semi-smart/certain enough to compute saturated models etc
#' # and report multiple groups correctly.
#' 
#' m3 = umxSuperModel('top', m1, m2)
#' 
#' umxSummary(m3, std= TRUE)
#' 
#' # |name         | Std.Estimate| Std.SE|CI                |
#' # |:------------|------------:|------:|:-----------------|
#' # |beta         |         0.51|   0.05|0.51 [0.41, 0.61] |
#' # |Var_x        |         1.00|   0.00|1 [1, 1]          |
#' # |Resid_y_grp1 |         0.74|   0.05|0.74 [0.64, 0.84] |
#' # |beta         |         0.50|   0.05|0.5 [0.41, 0.6]   |
#' # |Var_x        |         1.00|   0.00|1 [1, 1]          |
#' # |Resid_y_grp2 |         0.75|   0.05|0.75 [0.65, 0.84] |
#' 
#' summary(m3)
#'
#' # ====================================
#' # = Test models with duplicate names =
#' # ====================================
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff", "fc", "qol")
#' m1 = umxCP(selDVs= selDVs, nFac= 1, dzData= dzData, mzData= mzData, sep= "_T", autoRun= TRUE)
#' m2 = mxRename(m1, "CP2")
#' umxModelNames(m1) # "top" "MZ" "DZ"
#' umxModelNames(m2) # "top" "MZ" "DZ"
#' super = umxSuperModel("myModel", m1, m2, autoRun = TRUE)
#' umxModelNames(super)
#' }
umxSuperModel <- function(name = 'super', ..., autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE) {
	tryHard = match.arg(tryHard)
	umx_check(boolean.test= is.character(name), action= "stop", message= "You need to set the name for the supermodel, i.e. add name = 'modelName' ")
	dot.items = list(...) # grab all the dot items: models...	
	dot.items = unlist(dot.items)
	nModels   = length(dot.items)
	# Get list of model names
	modelNames = c()
	for(modelIndex in 1:nModels) {
		thisModel = dot.items[[modelIndex]]
		if(umx_is_MxModel(thisModel)){
			if(is.null(thisModel$fitfunction)){
				# ignore model... no fitfunction to optimize
			} else {
				modelNames = c(modelNames, thisModel$name)
			}
		} else {
		 	stop("Only models can be included in ... ", thisModel, " was a ", class(dot.items[[thisModel]]))
		}
	}
	if(length(modelNames) < 1){
	 	stop("No models in '...' had a fitfunction: At least two models must have a fitfunction and objective for umxSuperModel to jointly optimize")
	}else if(anyDuplicated(modelNames)){
	 	stop("Models must have unique names: Duplicates detected in ", omxQuotes(modelNames))
	}
	
	# multiple group fit function sums the likelihoods of its component models
	newModel = mxModel(name, dot.items, mxFitFunctionMultigroup(modelNames))
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	newModel = omxAssignFirstParameters(newModel)
	# 2. Find and change any duplicate model names inside the models
	# 	1. find all duplicated names
	# 	2. loop over the sub models, finding and changing each duplicate name
	tmpnames = umxModelNames(newModel)
	dupes = tmpnames[duplicated(tmpnames)] # "top" "MZ" "DZ"
	if(length(dupes) > 0){
		suffix = 2
		subNames = names(newModel$submodels)[-1]
		for(thisSub in subNames){
			thisModel = newModel$submodels[[thisSub]]
			for(thisDupName in dupes){
				thisModel = mxRename(thisModel, paste0(thisDupName, "_", suffix), oldname = thisDupName)
			}
			newModel = mxModel(newModel, thisModel)
			suffix = suffix + 1
		}
		print(paste0("Polite note: Renamed sub-models with duplicate names, e.g. ", omxQuotes(dupes[1]), " -> ", omxQuotes(paste0(dupes[1], "_2"))))
	}
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, std = std)
	invisible(newModel)
}

#' umxModify: Add, set, or drop model paths by label.
#' 
#' umxModify allows you to modify, re-run and summarize an [mxModel()],
#' all in one line of script.
#' 
#' @details
#' You can add paths, or other model elements, set path values (default is 0), or replace labels.
#' As an example, this one-liner drops a path labelled "Cs", and returns the updated model:
#' 
#' \code{fit2 = umxModify(fit1, update = "Cs", name = "newModelName", comparison = TRUE)}
#' 
#' Regular expressions are a powerful feature: they let you drop collections of paths by matching patterns
#' for instance, this would match labels containing either "Cs" or "Cr":
#' 
#' ```
#' fit2 = umxModify(fit1, regex = "C\[sr\]", name = "drop_Cs_and_Cr", comparison = TRUE)
#' ```
#' 
#' You may find it easier to be more explicit. Like this:
#' 
#' ```R
#' fit2 = umxSetParameters(fit1, labels = c("Cs", "Cr"), values = 0, free = FALSE, name = "newName")
#' fit2 = mxRun(fit2)
#' summary(fit2)
#' ```
#'
#' *Note*: A (minor) limitation is that you cannot simultaneously set value to 0 
#' AND relabel cells (because the default value is 0, so it is ignored when using newlabels).
#' 
#' @aliases umxModify
#' @param lastFit The [mxModel()] you wish to update and run.
#' @param update What to update before re-running. Can be a list of labels, a regular expression (set regex = TRUE) or an object such as mxCI etc.
#' @param regex  Whether or not update is a regular expression (default FALSE). If you provide a string, it overrides the contents of update, and sets regex to TRUE.
#' @param free The state to set "free" to for the parameters whose labels you specify (defaults to free = FALSE, i.e., fixed)
#' @param value The value to set the parameters whose labels you specify too (defaults to 0)
#' @param newlabels If not NULL, used as a replacement set of labels (can be regular expression). value and free are ignored!
#' @param freeToStart Whether to update parameters based on their current free-state. free = c(TRUE, FALSE, NA), (defaults to NA - i.e, not checked)
#' @param name The name for the new model
#' @param comparison Whether to run umxCompare() on the new and old models.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param master If you set master, then the update labels will be equated to these (i.e. replaced by them).
#' @param intervals Whether to run confidence intervals (see [mxRun()])
#' @param verbose How much feedback to give
#' @return - [mxModel()]
#' @family Core Model Building Functions
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
#' @examples
#' require(umx)
#' 
#' # First we'll just build a 1-factor model
#' umx_set_optimizer("SLSQP")
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' 
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' 
#' # 1. Drop the path to x1 (also updating the name so it's
#' #    self-explanatory, and get a fit comparison
#' m2 = umxModify(m1, update = "G_to_x1", name = "drop_X1", comparison = TRUE)
#' 
#' \dontrun{
#' # 2. Add the path back (setting free = TRUE)
#' m2 = umxModify(m1, update = "G_to_x1", free= TRUE, name = "addback_X1", comparison = TRUE)
#' # 3. Fix a value at a non-zero value
#' m3 = umxModify(m1, update = "G_to_x1", value = .35, name = "fix_G_x1_at_35", comp = TRUE)
#' # You can add objects to models. For instance this would add a path (overwriting the existing one)
#' # (thanks Johannes!)
#' m3 = umxModify(m1, umxPath("G", with = "x1"), name= "addedPath")
#' 
#' # Use regular expression to drop multiple paths: e.g. G to x3, x4, x5
#' m3 = umxModify(m1, regex = "^G_to_x[3-5]", name = "tried_hard", comp = TRUE, tryHard="yes")
#' 
#' # Same, but don't autoRun
#' m2 = umxModify(m1, regex  = "^G_to_x[3-5]", name = "no_G_to_x3_5", autoRun = FALSE) 
#' 
#' # Re-write a label
#' newLabel = "A_rose_by_any_other_name"
#' newModelName = "model_doth_smell_as_sweet"
#' m2 = umxModify(m1, update = "G_to_x1", newlabels= newLabel, name = newModelName, comparison = TRUE)
#' # Change labels in 2 places
#' labsToUpdate = c("G_to_x1", "G_to_x2")
#' newLabel = "G_to_1_or_2"
#' m2 = umxModify(m1, update = labsToUpdate, newlabels= newLabel, name = "equated", comparison = TRUE)
#' 
#' # Advanced!
#' # Regular expressions let you use pieces of the old names in creating new ones!
#' searchString = "G_to_x([0-9])"
#' newLabel = "loading_for_path\\1" # use value in regex group 1
#' m2 = umxModify(m1, regex = searchString, newlabels= newLabel, name = "grep", comparison = TRUE)
#' } # end dontrun
#' 
umxModify <- function(lastFit, update = NULL, regex = FALSE, free = FALSE, value = 0, newlabels = NULL, freeToStart = NA, name = NULL, comparison = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), master = NULL, intervals = FALSE, verbose = FALSE) {
	tryHard = match.arg(tryHard)

	if(!is.null(master)){
		x = umxEquate(lastFit, a = master, b = update, free = freeToStart, verbose = verbose, name = name, autoRun = autoRun, comparison = comparison)
		return(x)
	}

	if (typeof(regex) != "logical"){
		# Use the regex as update, and switch to regex mode
		if(!is.null(update)){
			stop("If you input a regular expression in ", omxQuotes("regex"), " you must leave ", omxQuotes("update"), " set to NULL.")
		}
		update = regex
		regex  = TRUE
	}
	
	if(is.null(update)){
		message("You haven't asked to do anything: the parameters that are free to be dropped are:")
		print(parameters(lastFit))
		stop()
	}

	if(!is.null(newlabels)){
		# check length(update) == length(newlabels) or length(newlabels) == 1
		if(length(update) != length(newlabels)){
			if(length(newlabels) != 1){
				stop(paste0("Length of newlabels must be 1, or same as update. You gave me ", 
				length(update), " labels to update, and ", length(newlabels), " newlabels"))
			}else{
				# Copy out newlabels to match length of update
				newlabels = rep(newlabels, length(update))
			}
		}
	}
	
	if(regex | typeof(update) == "character") {
		newModel = lastFit
		# handle labels as input
		if (!regex) {
			theLabels = update
			# TODO: check the labels are present
			# if not suggest reversal for with items
			if(is.null(newlabels)){
				newModel = omxSetParameters(newModel, labels = theLabels, free = free, values = value, name = name)
			}else{
				newModel = omxSetParameters(newModel, labels = theLabels, newlabels = newlabels, name = name)				
			}
		} else {
			# Handle 1 or more regular expressions.
			for (i in 1:length(update)) {
				match = umxGetParameters(newModel, regex = update[i], free = freeToStart, verbose = verbose)				
				if(verbose){
					message("Matched labels = ", omxQuotes(match))
				}
				if(is.null(newlabels)){
					newModel = omxSetParameters(newModel, labels = match, free = free, values = value, name = name)
				}else{
					# There are new labels to match up
					newFinds = namez(df= match, pattern= update[i], replacement= newlabels[i] )
					newModel = omxSetParameters(newModel, labels = match, newlabels = newFinds, name = name)
					if(verbose){
						message("newlabels = ", omxQuotes(newFinds))
					}
				}
			}
		}
	} else {
		# Add objects passed in under "update"
		# TODO umxModify: if object is RAM, add re-label and re-start new object?
		if(is.null(name)){ name = NA } # i.e. do nothing
		newModel = mxModel(lastFit, update, name = name)
	}
	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, lastFit, autoRun = autoRun, tryHard = tryHard, comparison = comparison)
	return(newModel)
}

# ==================
# = Twin Functions =
# ==================

#' Build and run a 2-group Cholesky twin model (uni-variate or multi-variate)
#'
#' @description
#' Implementing a core task in twin modeling, umxACE models the genetic and environmental
#' structure of one or more phenotypes (measured variables) using the Cholesky ACE model
#' (Neale and Cardon, 1996).
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxACE` implements a 2-group model to capture these data and represent the phenotypic variance as a sum of Additive genetic,
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' The following figure shows how the ACE model appears as a path diagram (for one variable):
#' 
#' \if{html}{\figure{ACEunivariate.png}{options: width="50\%" alt="Figure: ACE univariate.png"}}
#' \if{latex}{\figure{ACEunivariate.pdf}{options: width=7cm}}
#'
#' `umxACE` allows multivariate analyses, and this brings us to the Cholesky part of the model.
#' 
#' This model creates as many latent A C and E variables as there are phenotypes, and, moving 
#' from left to right, decomposes the variance in each manifest into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram:
#' 
#' \if{html}{\figure{ACEmatrix.png}{options: width="50\%" alt="Figure: ACE matrix.png"}}
#' \if{latex}{\figure{ACEmatrix.pdf}{options: width=7cm}}
#'
#' In this model, the variance-covariance matrix of the raw data
#' is recovered as the product of the lower Cholesky and its transform.
#' 
#' This Cholesky or lower-triangle decomposition allows a model which is both sure to be 
#' solvable, and also to account for all the variance (with some restrictions) in the data.
#' 
#' This figure also contains the key to understanding how to modify models that `umxACE` produces.
#' read the "Matrices and Labels in ACE model" section in details below...
#' 
#' **NOTE**: Scroll down to details for how to use the function, a figure
#' and multiple examples.
#' 
#' @details
#' \strong{Covariates}
#' umxACE handles covariates by modelling them in the means.
#' On the plus side, there is no distributional assumption for this method. A downside of this approach is that all 
#' covariates must be non-NA, thus dropping any rows where one or more covariates are missing.
#' This can waste data. See also [umx_residualize()]).
#'
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' The `type` parameter can select how you want the model data treated.
#' "FIML" is the normal treatment. "cov" and "cor" will turn raw data into cor data for analysis, or
#' check that you've provided cor data as input.
#' 
#' Types "WLS", "DWLS", and "ULS" will process raw data into WLS data of these types.
#' 
#' The default, "Auto" will treat data as the type they are provided as.
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modeling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelihoods summed to form
#' the model-likelihood, which is to be minimized.
#' This feature is used in the non-linear GxE model functions.
#' 
#' **Additional features**
#' The umxACE function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in ACE model**
#' 
#' Matrices 'a', 'c', and 'e' contain the path loadings of the Cholesky ACE factor model.
#' 
#' So, labels relevant to modifying the model are of the form `"a_r1c1", "c_r1c1"` etc.
#'
#' Variables are in rows, and factors are in columns. So to drop the influence of factor 2 on variable 3, you would say:
#'
#' `m2 = umxModify(m1, update = "c_r3c2")`
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for 
#' each variable for twin 1, followed by each variable for twin 2.
#' 
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you 
#' could make them independent again with this script:
#'
#' `m1$top$expMean$labels[1, 4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs (Eaves et al. 1978, p267).
#' 
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param data If provided, dzData and mzData are treated as levels of zyg to select() MZ and DZ data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param residualizeContinuousVars Not yet implemented.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param nSib Number of siblings in a family (default - 2). "3" = extra sib.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param numObsDZ Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ Number of MZ twins: Set this if you input covariance data.
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param boundDiag Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0 since umx version 1.8
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @return - [mxModel()] of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxPlotACE()], [umxSummaryACE()], [power.ACE.test()], [umxModify()]
#' @references - Eaves, L. J., Last, K. A., Young, P. A., & Martin, N. G. (1978). Model-fitting approaches 
#' to the analysis of human behaviour. *Heredity*, **41**, 249-320. <https://www.nature.com/articles/hdy1978101.pdf>
#' @md
#' @examples
#' require(umx)
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' 
#' # 1. Height in meters has a tiny variance, and this makes optimising hard.
#' #    We'll scale it by 10x to make the Optimizer's task easier.
#' data(twinData) # ?twinData from Australian twins.
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
#'
#' # 2. Make mz & dz data.frames (no need to select variables: umx will do this)
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # 3. Built & run the model, controlling for age in the means model
#' m1 = umxACE(selDVs = "ht", selCovs = "age", sep = "", dzData = dzData, mzData = mzData)
#'
#' # sidebar: umxACE figures out variable names using sep: 
#' #    e.g. selVars = "wt" + sep= "_T" -> "wt_T1" "wt_T2"
#' 
#' umxSummary(m1, std = FALSE) # un-standardized
#'
#' # tip 1: set report = "html" and umxSummary prints the table to your browser!
#' # tip 2: plot works for umx: Get a figure of the model and parameters
#' # plot(m1) # Also, look at the options for ?plot.MxModel.
#' 
#' \donttest{
#' # ============================
#' # = Model, with 2 covariates =
#' # ============================
#'
#' # Create another covariate: cohort
#' twinData$cohort1 = twinData$cohort2 =twinData$part
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#'
#' # 1. def var approach
#' m2 = umxACE(selDVs = "ht", selCovs = c("age", "cohort"), sep = "", dzData = dzData, mzData = mzData)
#'
#' # 2. Residualized approach: remove height variance accounted-for by age.
#' FFdata = twinData[twinData$zygosity %in% c("MZFF", "DZFF"), ]
#' FFdata = umx_residualize("ht", "age", suffixes = 1:2, data = FFdata)
#' mzData = FFdata[FFdata$zygosity %in% "MZFF", ]
#' dzData = FFdata[FFdata$zygosity %in% "DZFF", ]
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
#'
#' # =============================================================
#' # = ADE: Evidence for dominance ? (DZ correlation set to .25) =
#' # =============================================================
#' m2 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) 
#' # nb: Although summary is smart enough to print d, the underlying 
#' #     matrices are still called a, c & e.
#'
#' # tip: try umxReduce(m1) to automatically build and compare ACE, ADE, AE, CE
#' # including conditional probabilities!
#'
#' # ===================================================
#' # = WLS example using diagonal weight least squares =
#' # ===================================================
#'
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, 
#' 	type = "DWLS", allContinuousMethod='marginals'
#' )
#'
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' 
#' # 1. Weight has a large variance, and this makes solution finding very hard.
#' # Here, we residualize the data for age, which also scales weight and height.
#'
#' data(twinData)
#' tmp = umx_residualize(c("wt", "ht"), cov = "age", suffixes= c(1, 2), data = twinData)
#' mzData = tmp[tmp$zygosity %in% "MZFF", ]
#' dzData = tmp[tmp$zygosity %in% "DZFF", ]
#' 
#' # tip: You might also want transform variables
#' # tmp = twinData$wt1[!is.na(twinData$wt1)]
#' # car::powerTransform(tmp, family="bcPower"); hist(tmp^-0.6848438)
#' # twinData$wt1 = twinData$wt1^-0.6848438
#' # twinData$wt2 = twinData$wt2^-0.6848438
#' 
#' # 4. note: the default boundDiag = 0 lower-bounds a, c, and e at 0.
#' #    Prevents mirror-solutions. If not desired: set boundDiag = NULL.
#'
#' m2 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = NULL)
#'
#' # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
#' m1 = umxACE(selDVs = "wt", sep = "", data = twinData,
#' 	dzData = c("DZMM", "DZFF", "DZOS"), mzData = c("MZMM", "MZFF"))
#' # |   |   a1|c1 |   e1|
#' # |:--|----:|:--|----:|
#' # |wt | 0.93|.  | 0.38|
#'
#' # tip: umx_make_twin_data_nice() will make data into this nice format for you!
#' 
#' # ======================
#' # = MODEL MODIFICATION =
#' # ======================
#' # We can modify this model, e.g. test shared environment. 
#' # Set comparison to modify, and show effect in one step.
#' 
#' m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
#' #*tip* call umxModify(m1) with no parameters, and it will print the labels available to fix!
#' # nb: You can see parameters of any model with parameters(m1)
#'
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' # We'll scale height (ht1 and ht2) and weight
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' m1 = umxACE(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#'
#' # ===================
#' # = Ordinal example =
#' # ===================
#' 
#' # Prep data
#' require(umx)
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obLevels = c('normal', 'overweight', 'obese')
#' cuts = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' 
#' # Make the ordinal variables into umxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # Model and summary!
#' m1 = umxACE(selDVs = "obese", dzData = dzData, mzData = mzData, sep = '')
#'
#' # And controlling age (otherwise manifests appearance as latent C)
#' m1 = umxACE(selDVs = "obese", selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#'
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
#' # Cut BMI column to form ordinal obesity variables
#' obLevels   = c('normal', 'overweight', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' # Make the ordinal variables into mxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' mzData = mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData = dzData[1:80,]
#' m1 = umxACE(selDVs= c("wt","obese"), dzData= dzData, mzData= mzData, sep='')
#' 
#' # And controlling age
#' m1 = umxACE(selDVs = c("wt","obese"), selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#'
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data= twinData,varsToScale= "wt", sep="")
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' obLevels   = c('normal', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1= cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' twinData$obese2= cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#'
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#'
#' require(umx)
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)
#' }
#'
umxACE <- function(name = "ACE", selDVs, selCovs = NULL, dzData= NULL, mzData= NULL, sep = NULL, data = NULL, zyg = "zygosity", type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, residualizeContinuousVars = FALSE, nSib = 2, dzAr = .5, dzCr = 1, weightVar = NULL, equateMeans = TRUE, addStd = TRUE, addCI = TRUE) {
	tryHard = match.arg(tryHard)
	type    = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	if(residualizeContinuousVars){
		stop("residualizing (as opposed to modelling) continuous variables not implemented yet: just set to FALSE for now")
	}

	if(dzCr == .25 & (name == "ACE")){ name = "ADE" }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}

	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
	# nSib = 2, equateMeans = TRUE, verbose = verbose

	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	# full names passed in... gosh I wish I'd not allowed this early on...
	selVars = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)

	model = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp   = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	nVar  = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	
	# Finish building top
	# Finish building top
	if(nSib==2){
		expCovMZ = mxAlgebra(rbind (cbind(ACE,  AC), cbind( AC, ACE)), dimnames = list(selVars, selVars), name = "expCovMZ")
		expCovDZ = mxAlgebra(rbind (cbind(ACE, hAC), cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
	} else if (nSib==3) {
		expCovMZ = mxAlgebra(name="expCovMZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE,  AC, hAC),
		    cbind(AC , ACE, hAC),
		    cbind(hAC, hAC, ACE))
		)
		expCovDZ = mxAlgebra(name= "expCovDZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE, hAC, hAC),
			cbind(hAC, ACE, hAC),
			cbind(hAC, hAC, ACE))
		)
	}else{
		stop("3 sibs is experimental, but ", nSib, "? ... Maybe come back in 2022, best tim :-)")
	}
	
	top = mxModel(model$top,
		# NB: "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# it already has the means model and thresholds matrix added if necessary  - see "xmu_make_TwinSuperModel" above

		# Additive, Common, and Unique environmental paths				
		umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE), 

		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "A", a %*% t(a)), # Additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # Common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # Unique environmental variance
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		expCovMZ, expCovDZ
	)

	model = mxModel(model, top) 

	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be NULL, a value or a vector of values. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a$lbound = newLbound
			model$top$c$lbound = newLbound
			model$top$e$lbound = newLbound
		}
	}
	if(addStd){
		newTop = mxModel(model$top,
			umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # total variance --> 1/SD
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e), # standardized e

			mxAlgebra(name = "A_std", SD %&% A), # standardized A
			mxAlgebra(name = "C_std", SD %&% C), # standardized C
			mxAlgebra(name = "E_std", SD %&% E)  # standardized E
		)
		model = mxModel(model, newTop)
	}
	if(addCI){
		if(addStd){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}else{
			model = mxModel(model, mxCI(c('top.a', 'top.c', 'top.e')))
		}
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACE") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE)
	return(model)
} # end umxACE


#' umxGxE: Implements ACE models with moderation of paths, e.g. by SES.
#'
#' Make a 2-group GxE (moderated ACE) model (Purcell, 2002). GxE interaction studies test the hypothesis that the strength
#' of genetic (or environmental) influence varies parametrically (usually linear effects on path estimates)
#' across levels of environment. umxGxE allows detecting,
#' testing, and visualizing  G xE (or C or E x E) interaction forms.
#' 
#' The following figure the GxE model as a path diagram:
#' 
#' \if{html}{\figure{GxE.png}{options: width="50\%" alt="Figure: GxE.png"}}
#' \if{latex}{\figure{GxE.pdf}{options: width=7cm}}
#'
#' @param name The name of the model (default= "G_by_E")
#' @param selDVs The dependent variable (e.g. "IQ")
#' @param selDefs The definition variable (e.g. "SES")
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
#' @return - GxE [mxModel()]
#' @export
#' @seealso [umxGxE_window()], [umxReduce()], [umxSummary()]
#' @family Twin Modeling Functions
#' @references - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. *Twin Research*,
#'  **6**, 554-571. DOI: \doi{10.1375/twin.5.6.554}
#' @md
#' @examples
#' require(umx)
#' data(twinData) 
# twinData = tibble::as_tibble(twinData)
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = "bmi"
#' selDefs = "age"
#' mzData  = subset(twinData, zygosity == "MZFF")[1:100,]
#' dzData  = subset(twinData, zygosity == "DZFF")[1:100,]
#' m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
#' 
#' \dontrun{
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
# # Test dropping moderation on a path
#' m2 = umxModify(m1, regex = "am_.*", comparison = TRUE, tryHard = "yes")
#' 
#' # umxReduce knows how to test all relevant hypotheses for GxE models,
#' # reporting these in a nice table.
#' umxReduce(m1)
#' }
umxGxE <- function(name = "G_by_E", selDVs, selDefs, dzData, mzData, sep = NULL, data = NULL, zyg = "zygosity", digits = 3, lboundACE = NA, lboundM = NA, dropMissingDef = TRUE, dzAr = .5,  dzCr = 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {

	# ===================================================================
	# = Thoughts about expanding covariate modelling here and elsewhere =
	# ===================================================================
	# In top model (would use xmuTwinUpgradeMeansToCovariateModel)
	# TODO:	umxGxE: Add covariates
	# if(0){
	# 	TODO: umxGxE If there are covs
	# 	mxMatrix(name = "betas" , "Full", nrow = nCov, ncol = nVar, free = TRUE, values = 0.05, labels = paste0("beta_", covariates))
	# }
	
	# And In MZ and DZ models...
	# if(0){ # TODO if there are covs
	#   # matrices for covariates (just on the means)
	# 	mxMatrix(name = "covsT1", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT1)),
	# 	mxMatrix(name = "covsT2", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT2)),
	# 	mxAlgebra(top.betas %*% covsT1, name = "predMeanT1"),
	# 	mxAlgebra(top.betas %*% covsT2, name = "predMeanT2"),
	# 	mxAlgebra( cbind(top.intercept + DefT1Rlin + DefT1Rquad + predMeanT1,
	# 	                 top.intercept + DefT2Rlin + DefT2Rquad + predMeanT2), name = "expMeans")
	# } else {
		# mxAlgebra( cbind(top.intercept + DefT1Rlin + DefT1Rquad, top.intercept + DefT2Rlin + DefT2Rquad), name = "expMeans")
	# },

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
	if(length(selDVs) != nSib){
		stop("DV list must be length = 2: 1 variable for each of 2 twins... You tried ", length(selDVs)/nSib)
	}

	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)

	if(!umx_set_silent(silent = TRUE)){
		message("selDVs: ", omxQuotes(selDVs))
	}

	selVars   = c(selDVs, selDefs)
	obsMean   = mean(colMeans(mzData[,selDVs], na.rm = TRUE)); # Just one average mean for all twins
	nVar      = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	rawVar    = diag(var(mzData[,selDVs], na.rm = TRUE))[1]
	startMain = sqrt(c(.8, .0 ,.6) * rawVar)	
	umx_check(!umx_is_cov(dzData, boolean = TRUE), "stop", "data must be raw for gxe")
	
	# drop unused variables
	dzData = dzData[ , selVars]
	mzData = mzData[ , selVars]
	
	mzData = xmu_data_missing(mzData, selVars = selDefs, sep = NULL, dropMissingDef = dropMissingDef, hint= "mzData")
	dzData = xmu_data_missing(dzData, selVars = selDefs, sep = NULL, dropMissingDef = dropMissingDef, hint= "dzData")
	
	# =====================================
	# = DO T1 and T2 share the moderator? =
	# =====================================
	bModeratorsIdentical = all(mzData[, selDefs[1]] == mzData[, selDefs[2]])
	if(!bModeratorsIdentical){
		message("Twins do not share the moderator... I will regress both twin's moderator from each twin, but you need to check this doesn't violate assumptions")
	}
	
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
			mxMatrix(name = "intercept", "Full", nrow = 1, ncol = nVar, free = TRUE, values = obsMean, labels = "mean"), # needs mods for multivariate!

			# Matrice for moderated the means model (algebars in the data models)
			if(bModeratorsIdentical){
				# Matrices for betas
				list(
					umxMatrix(name = "betaLin" , "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "lin11"),
					umxMatrix(name = "betaQuad", "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "quad11")
				)
			}else{
				list(
					umxMatrix(name = "betaSelf"  , "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0), 
					umxMatrix(name = "betaCoTwin", "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0)
				)
			}
		),
		mxModel("MZ",
			# Matrices for moderating/interacting variable
			umxMatrix("DefT1", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[1])), # c("data.age1")
			umxMatrix("DefT2", "Full", nrow=1, ncol=1, free=FALSE, labels = paste0("data.", selDefs[2])), # c("data.age2")
			# ====================================
			# = Algebra for expected mean vector =
			# ====================================

			if(bModeratorsIdentical){
				# do lm(DV ~ moderator + moderator^2)
				mxAlgebra(name = "expMean", cbind(
					top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2),
					top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2))
				)
			}else{
				# do lm(DV ~ self moderator + co-twin moderator)
				mxAlgebra(name = "expMean", cbind(
					top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2),
					top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1))
				)
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
			mxExpectationNormal("expCovMZ", means = "expMean", dimnames = selDVs), mxFitFunctionML()
		),
	    mxModel("DZ",
			umxMatrix("DefT1", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[1])), # twin1  c("data.divorce1")
			umxMatrix("DefT2", "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", selDefs[2])), # twin2  c("data.divorce2")

			# =================
			# = Means Algebra =
			# =================
			if(bModeratorsIdentical){
				# do lm(DV ~ moderator + moderator^2)
				mxAlgebra(name = "expMean", cbind(
					top.intercept + (top.betaLin  %*% DefT1) + (top.betaQuad %*% DefT1^2),
					top.intercept + (top.betaLin  %*% DefT2) + (top.betaQuad %*% DefT2^2))
				)
			}else{
				# do lm(DV ~ self moderator + co-twin moderator)
				mxAlgebra(name = "expMean", cbind(
					top.intercept + (top.betaSelf %*% DefT1) + (top.betaCoTwin %*% DefT2),
					top.intercept + (top.betaSelf %*% DefT2) + (top.betaCoTwin %*% DefT1))
				)
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
			mxExpectationNormal("expCovDZ", means = "expMean", dimnames = selDVs),
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
#' @param plotWindow whether to plot what the window looks like
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
#' *Behavior Genetics*, **45**, 581-96. doi [10.1007/s10519-015-9732-8](https://link.springer.com/article/10.1007/s10519-015-9732-8)
#' @md
#' @examples
#' library(umx);
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
#' # Delete missing moderator rows
#' twinData = twinData[!is.na(twinData[mod]), ]
#' mzData = subset(twinData, zygosity == "MZFF", c(selDVs, mod))
#' dzData = subset(twinData, zygosity == "DZFF", c(selDVs, mod))
#' 
#' # ========================
#' # = 2. Run the analyses! =
#' # ========================
#' # Run and plot for specified windows (in this case just 1927)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 		target = 40, plotWindow = TRUE)
#' 
#' \dontrun{
#' # Run with FIML (default) uses all information
#' umxGxE_window(selDVs = "bmi", sep="", moderator = "age", mzData = mzData, dzData = dzData)
#' 
#' # Run creating weighted covariance matrices (excludes missing data)
#' umxGxE_window(selDVs = "bmi", sep="", moderator= "age", mzData = mzData, dzData = dzData, 
#' 		weightCov = TRUE)
#' }
#' 
umxGxE_window <- function(selDVs = NULL, moderator = NULL, mzData = mzData, dzData = dzData, sep = NULL, weightCov = FALSE, target = NULL, width = 1, plotWindow = FALSE, return = c("estimates","last_model")) {
	return = match.arg(return)
	nSib   = 2 # Number of siblings in a twin pair.
	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib)

	umx_check(!is.null(moderator), "stop", "Moderator must be set to the name of the moderator column, e.g, moderator = 'birth_year'")
	
	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	if(!is.null(sep)){
		selVars   = umx_paste_names(selDVs, sep = sep, 1:2)
		# moderator = umx_paste_names(moderator, sep = sep, 1:2)
	}else{
		selVars = selDVs
	}

	# TODO umxGxE_window: allow missing moderator?
	# Check DVs exists in mzData and dzData (and nothing else apart from the moderator)
	umx_check_names(c(selVars, moderator), data = mzData, die = TRUE, no_others = TRUE)
	umx_check_names(c(selVars, moderator), data = dzData, die = TRUE, no_others = TRUE)

	# Add a zygosity column (that way we know what it's called)
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
	# bind the MZ and DZ data into one frame so we can work with it repeatedly over weight iterations
	allData = rbind(mzData, dzData)

	# Create range of moderator values to iterate over (using the incoming moderator variable name)
	modVar  = allData[, moderator]
	if(any(is.na(modVar))){		
		stop("Moderator \"", moderator, "\" contains ", length(modVar[is.na(modVar)]), "NAs. This is not currently supported.\n",
			"NA found on rows", paste(which(is.na(modVar)), collapse = ", "), " of the combined data."
		)
	}

	if(!is.null(target)){
		if(target < min(modVar)) {
			stop("specifiedTarget is below the range in moderator. min(modVar) was ", min(modVar))
		} else if(target > max(modVar)){
			stop("specifiedTarget is above the range in moderator. max(modVar) was ", max(modVar))
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
		m1 = mxRun(m1); 
		if(plotWindow){
			plot(allData[,moderator], allData$weight) # normal-curve yumminess
			umxSummaryACE(m1)
		}
		out[n, ] = mxEval(c(i, top.a_std[1,1], top.c_std[1,1], top.e_std[1,1], top.a[1,1], top.c[1,1], top.e[1,1]), m1)
		n = n + 1
	}

	ACE = c("A", "C", "E")
	# Squaring paths to produce variances
	out[,ACE] = out[,ACE]^2
	# plotting variance components
	with(out,{
		plot(A ~ modLevel, main = paste0(selDVs[1], " variance"), ylab = "Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
		lines(modLevel, C, col = 'green')
		lines(modLevel, E, col = 'blue')
		legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)

		plot(Astd ~ modLevel, main = paste0(selDVs[1], " std variance"), ylab = "Std Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
		lines(modLevel, Cstd, col = 'green')
		lines(modLevel, Estd, col = 'blue')
		legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)
	})
	if(return == "last_model"){
		invisible(m1)
	} else if(return == "estimates") {
		invisible(out)
	}else{
		warning("You specified a return type that is invalid. Valid options are last_model and estimates. You requested:", return)
	}
}


#' Run a Cholesky with covariates that are random (in the expected covariance matrix)
#'
#' Often, researchers include covariates in 2-group Cholesky [umxACE()] twin models.
#' The umxACEcov 'random' option models the covariates in the expected covariance matrix, thus allowing
#' all data to be preserved. The downside is that this method has a strong assumption
#' of multivariate normality. Covariates like age, which are perfectly correlated in twins cannot be used.
#' Covariates like sex, which are ordinal, violate the normality assumption.
#'
#' The following figure shows how the ACE model with random covariates appears as a path diagram:
#' 
#' \if{html}{\figure{ACEcovVarianceModel.png}{options: width="50\%" alt="Figure: ACEcovVarianceModel.png"}}
#' \if{latex}{\figure{ACEcovVarianceModel.pdf}{options: width=7cm}}
#'
#' 
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data (do not include sep).
#' @param selCovs The covariates to include from the data (do not include sep).
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep Separator text between basename for twin variable names. Often "_T".
#' Used to expand selDVs into full column names, i.e., "dep" --> c("dep_T1", "dep_T2").
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices.
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer optionally set the optimizer. Default (NULL) does nothing.
#' @return - [mxModel()] of subclass mxModel.ACEcov
#' @export
#' @family Twin Modeling Functions
#' @md
#' @references 
#' Neale, M. C., & Martin, N. G. (1989). The effects of age, sex, 
#' and genotype on self-report drunkenness following a challenge dose of alcohol. 
#' *Behavior Genetics*, **19**, 63-78. doi:\doi{10.1007/BF01065884}.
#' 
#' Schwabe, I., Boomsma, D. I., Zeeuw, E. L., & Berg, S. M. (2015). A New Approach
#' to Handle Missing Covariate Data in Twin Research : With an Application to
#' Educational Achievement Data. *Behavior Genetics*, **46**, 583-95. doi:\doi{10.1007/s10519-015-9771-1}.
#'
#' @examples
#' \dontrun{
#' # ============================================
#' # = BMI, can't use Age as a random covariate =
#' # ============================================
#' require(umx)
#' data(twinData)
#' # Replicate age to age1 & age2
#' twinData$age1 = twinData$age2 = twinData$age
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#'
#' # =====================================================================
#' # = Trying to use identical var (like age) as a random cov is ILLEGAL =
#' # =====================================================================
#' m1 = umxACEcov(selDVs = "bmi", selCovs = "age", dzData = dzData, mzData = mzData, sep = "")
#'
#' # ========================================================
#' # = Use an lm-based age-residualisation approach instead =
#' # ========================================================
#'
#' resid_data = umx_residualize("bmi", "age", suffixes = 1:2, twinData)
#' mzData = subset(resid_data, zygosity == "MZFF")
#' dzData = subset(resid_data, zygosity == "DZFF")
#' m2     = umxACE("resid", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#'
#' # Univariate BMI without covariate of age for comparison
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "DZFF")
#' m3 = umxACE("raw_bmi", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
#' 
#' # ===========================================================================
#' # = A bivariate example (need a dataset with a VIABLE COVARIATE to do this) =
#' # ===========================================================================
#' selDVs  = c("ht", "wt") # Set the DV
#' selCovs = c("income") # Set the COV
#' selVars = umx_paste_names(selDVs, covNames = selCovs, sep = "", sep = 1:2)
#' # 80 rows so example runs fast on CRAN
#' mzData = subset(twinData, zygosity == "MZFF", selVars)[1:80, ]
#' dzData = subset(twinData, zygosity == "DZFF", selVars)[1:80, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs,
#'    dzData = dzData, mzData = mzData, sep = "", autoRun = TRUE
#' )
#' }
#'@md
umxACEcov <- function(name = "ACEcov", selDVs, selCovs, dzData, mzData, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = 0, equateMeans = TRUE, bVector = FALSE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	# TODO sub-class umxACEcov (random covariates) fn to support umxSummary and plot
	
	nSib = 2 # Number of siblings in a twin pair
	tryHard             = match.arg(tryHard)
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	
	if(type!="Auto"){
		stop("type not support in umxACEcov yet, sorry...")
	}

	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if(dzCr == .25 && name == "ACEcov"){
		name = "ADEcov"
	}

	# ==================
	# = Validate input =
	# ==================
	# Look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(is.null(sep)){
		stop("I need a sep, like '_T'. (I will add 1 and 2 after it...) \n",
		"i.e., selDVs should be 'bmi' etc., and I will re-name to 'bmi_T1' and 'bmi_T2' etc.")
	}else if(length(sep) > 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 after that...\n",
			"i.e., if variables are like 'var_T1', give me selVars = 'var' and sep = '_T'")
	}else{
		# stash base names for use later
		baseDVs  = selDVs
		baseCovs = selCovs
		# fill out full trait names
		selDVs  = umx_paste_names(baseDVs , sep = sep, suffixes = (1:nSib) )
		selCovs = umx_paste_names(baseCovs, sep = sep, suffixes = (1:nSib) )
	}

	nDV  = length(baseDVs)
	nCov = length(baseCovs)
	nVar = nDV + nCov # Number of variables per **INDIVIDUAL** ( so * 2 for a family)

	selVars = c(selDVs, selCovs)
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))

	# Drop unused columns from mzData and dzData
	mzData = mzData[, selVars]
	dzData = dzData[, selVars]
	# check covariates are not identical across twins
	for (i in baseCovs) {
		checkVars = umx_paste_names(i , sep = sep, suffixes = (1:nSib) )
		if(cor(mzData[, checkVars], use = "com")[2, 1] == 1){
			stop("The covariate ", omxQuotes(i), " is identical for twin 1 and twin 2... That's not allowed for random-effects covariates. Try modeling this as a def var in the means instead.")
		}		
	}

	if(any(umx_is_ordered(dzData, strict = FALSE))){
		stop("Sorry: umxACEcov can't handle ordinal yet: e-mail tim and chew him out")
	}
	if(nrow(dzData) == 0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData) == 0){ stop("Your MZ dataset has no rows!") }

	# ===============
	# = Setup means =
	# ===============
	# c(T1 DV means, T1 DV means, T1 COV means, T1 COV means)
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans){
		meanDimNames  = list(NULL, selVars)
		meanLabels    = umx_paste_names(varNames = baseDVs, covNames = baseCovs, suffixes = c('',''), prefix = "expMean_")
		DVmeanStarts  = umx_means(mzData[, selDVs[1:nDV]  , drop = FALSE], ordVar = 0, na.rm = TRUE)
		CovMeanStarts = umx_means(mzData[, selCovs[1:nCov], drop = FALSE], ordVar = 0, na.rm = TRUE)
		meanStarts    = c(DVmeanStarts, DVmeanStarts, CovMeanStarts, CovMeanStarts)
	} else {
		stop("Currently, means must be equated... why?")
	}
	
	# make beta labels
	betaLabels = paste0(rep(paste0("var", 1:nDV), each = nCov), rep(paste0("beta", 1:nCov), times = nDV))
	betaLabels = matrix(betaLabels, nrow = nCov, ncol  = nDV, byrow = FALSE)

	# =====================
	# = Set up varStarts  =
	# =====================

	# DVS
	DVvarStarts = umx_var(mzData[, selDVs[1:nDV], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	if(nDV == 1){
		DVvarStarts = sqrt(DVvarStarts/3)
	} else {
		DVvarStarts = t(chol(diag(DVvarStarts/3))) # Divide variance up equally, and set to Cholesky form.
	}
	DVvarStarts = matrix(DVvarStarts, nDV, nDV)
	
	# covs
	covStarts = umx_var(mzData[, selCovs[1:nCov], drop = FALSE], format= "diag", ordVar = 1, use = "pairwise.complete.obs")
	if(nCov == 1){
		covStarts = sqrt(covStarts)
	} else {
		covStarts = t(chol(diag(covStarts))) # Set to Cholesky form.
	}
	covStarts = matrix(covStarts, nCov, nCov)
	
	
	top = mxModel("top",
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of.
		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

		# Matrices a, c, e to store a, c, e path coefficients.
		umxMatrix(name = "a", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE, dimnames = list(baseDVs, baseDVs)),
		umxMatrix(name = "c", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE),
		umxMatrix(name = "e", type = "Lower", nrow = nDV, ncol = nDV, free = TRUE, values = DVvarStarts, byrow = TRUE),  
		# Matrices A, C,E + compute variance components
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),

		umxMatrix("expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = meanStarts, labels = meanLabels, dimnames = meanDimNames),

		# general (between-pair) cov of covariates
		umxMatrix("lowerB", 'Lower', nrow = nCov, ncol = nCov, values = (covStarts * .4), free = TRUE),
		# # specific (within-pair) cov of covariates
		umxMatrix("lowerW", 'Lower', nrow = nCov, ncol = nCov, values = (covStarts * .6), free = TRUE),

		mxAlgebra(name= "CovB" , lowerB %*% t(lowerB)),
		mxAlgebra(name= "CovW" , lowerW %*% t(lowerW)),
		mxAlgebra(name= "CovWB", CovW + CovB),
		
		# ========================================
		# = Beta matrix of regression parameters =
		# ========================================
		mxMatrix(name = "beta", type = "Full", nrow = nCov, ncol  = nDV, free = TRUE, values = 0, labels = betaLabels),
		# Some handy component algebras
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),

		mxAlgebra(name = "bCovWBb", (t(beta) %*% CovWB) %*% beta), # output is[nDV * nDV] to match ACE
		mxAlgebra(name = "bCovBb" , (t(beta) %*% CovB)  %*% beta),
		mxAlgebra(name = "bCovWB" ,  t(beta) %*% CovWB),
		mxAlgebra(name = "bCovB"  ,  t(beta) %*% CovB),
		mxAlgebra(name = "CovWBb" ,              CovWB %*% beta),
		mxAlgebra(name = "CovBb"  ,               CovB %*% beta),
		# Algebra for expected variance/covariance matrix #in MZ twins
		
		mxAlgebra(name = "expCovMZ", dimnames = list(names(mzData), names(mzData)), expression = rbind(
			cbind(ACE + bCovWBb, AC  + bCovBb , bCovWB, bCovB),
			cbind(AC  + bCovBb , ACE + bCovWBb, bCovB , bCovWB),
			cbind(       CovWBb,        CovBb , CovWB , CovB),
			cbind(       CovBb ,        CovWBb, CovB  , CovWB))
		),
		# Algebra for expected variance/covariance matrix #in DZ twins
		mxAlgebra(name = "expCovDZ", dimnames = list(names(dzData), names(dzData)), expression = rbind(
			cbind(ACE + bCovWBb, hAC + bCovBb , bCovWB, bCovB),
			cbind(hAC + bCovBb , ACE + bCovWBb, bCovB , bCovWB),
			cbind(       CovWBb,        CovBb ,  CovWB,  CovB),
			cbind(       CovBb ,        CovWBb,  CovB ,  CovWB))
		)
	) # end top

	MZ  = mxModel("MZ", mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
	DZ  = mxModel("DZ", mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

	MZ = mxModel("MZ",
		mxData(mzData , type = "raw"),
		mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames = names(mzData)),
		mxFitFunctionML()
	)

	DZ = mxModel("DZ",
		mxData(dzData, type = "raw"),
		mxExpectationNormal("top.expCovDZ", means = "top.expMean", dimnames = names(dzData)),
		mxFitFunctionML()
	)
	
	model = mxModel(name, MZ, DZ, top,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a$lbound = newLbound
			model$top$c$lbound = newLbound
			model$top$e$lbound = newLbound
		}
	}
	
	if(addStd){
		newTop = mxModel(model$top,
			mxMatrix(name  = "Iden", "Iden", nDV, nDV), # nDV Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
			mxAlgebra(name = "SD", solve(sqrt(Iden * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACEcov") # set class so umxSummary, plot, etc. work.
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	invisible(model)
}

#' umxCP: Build and run a Common pathway twin model
#'
#' @description
#' Make a 2-group Common Pathway twin model (Common-factor common-pathway multivariate model).
#' 
#' The common-pathway model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences.
#' 
#' umxCP supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Common-pathway path diagram:
#' 
#' \if{html}{\figure{CP.png}{options: width="50\%" alt="Figure: CP.png"}}
#' \if{latex}{\figure{CP.pdf}{options: width=7cm}}
#' 
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotype.
#' 
#' Features include the ability to include more than one common pathway, to use ordinal data.
#' 
#' **note**: The function [umx_set_optimization_options()] allows users to see and set `mvnRelEps` and `mvnMaxPointsA`
#' mvnRelEps defaults to .005. For ordinal models, you might find that '0.01' works better.
#' 
#' @details
#' Like the [umxACE()] model, the CP model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure).
#' 
#' 
#' **Data Input**
#' Currently, the umxCP function accepts only raw data. This may change in future versions.
#' 
#' **Ordinal Data**
#' 
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' **Additional features**
#' 
#' The umxCP function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in CP model**
#' 
#' A good way to see which matrices are used in umxCP is to run an example model and plot it.
#'
#' All the shared matrices are in the model "top".
#' 
#' Matrices `top$as`, `top$cs`, and `top$es` contain the path loadings specific to each variable on their diagonals.
#' 
#' So, to see the 'as' values, labels, or free states, you can say:
#'
#' `m1$top$as$values`
#' 
#' `m1$top$as$free`
#' 
#' `m1$top$as$labels`
#' 
#' Labels relevant to modifying the specific loadings take the form "as_r1c1", "as_r2c2" etc.
#' 
#' The common-pathway loadings on the factors are in matrices `top$a_cp`, `top$c_cp`, `top$e_cp`.
#'
#' The common factors themselves are in the matrix `top$cp_loadings` (an nVar * 1 matrix)
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this line:
#'
#' `m1$top$expMean$labels[1,4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#' 
#' For a deep-dive, see [xmu_make_TwinSuperModel()]
#' 
#' @param name The name of the model (defaults to "CP").
#' @param selDVs The variables to include.
#' omit sep in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs basenames for covariates
#' @param sep (required) The suffix for twin 1 and twin 2, often "_T".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param nFac How many common factors (default = 1)
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param data If provided, dzData and mzData are treated as valid levels of zyg to select() data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param correlatedACE Allows correlations between the factors built by each of the a, c, and e matrices. Default = FALSE.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param boundDiag = Numeric lbound for diagonal of the a_cp, c_cp, & e_cp matrices. Set = NULL to ignore.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = not yet implemented: Ordinal Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = not yet implemented: Ordinal Number of MZ twins: Set this if you input covariance data.
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param freeLowerA (ignore): Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC (ignore): Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE (ignore): Whether to leave the lower triangle of E free (default = FALSE).
#' @param correlatedA deprecated.
#' @return - [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxSummaryCP()], [umxPlotCP()]. See [umxRotate.MxModelCP()] to rotate the factor loadings of a [umxCP()] model. See [umxACE()] for more examples of twin modeling. 
#' [plot()] and [umxSummary()] work for all twin models, e.g., [umxIP()], [umxCP()], [umxGxE()], and [umxACE()].
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' # ========================================================
#' # = Run a 3-factor Common pathway twin model of 6 traits =
#' # ========================================================
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#  # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, tryHard = "yes",
#' 		dzData = dzData, mzData = mzData)
#'
#' # Shortcut using "data ="
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs= selDVs, nFac= 3, data=GFF, zyg="zyg_2grp")
#'
#' # ===================
#' # = Do it using WLS =
#' # ===================
#' m2 = umxCP("new", selDVs = selDVs, sep = "_T", nFac = 3, optimizer = "SLSQP",
#' 		dzData = dzData, mzData = mzData, tryHard = "ordinal", 
#'		type= "DWLS", allContinuousMethod='marginals'
#' )
#' 
#' # =================================================
#' # = Find and test dropping of shared environment  =
#' # =================================================
#' # Show all labels for C parameters 
#' umxParameters(m1, patt = "^c")
#' # Test dropping the 9 specific and common-factor C paths
#' m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC", comp = TRUE)
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' data(GFF)
#' # Cut to form umxFactor 20% depressed  DEP
#' cutPoints = quantile(GFF[, "AD_T1"], probs = .2, na.rm = TRUE)
#' ADLevels  = c('normal', 'depressed')
#' GFF$DEP_T1 = cut(GFF$AD_T1, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' GFF$DEP_T2 = cut(GFF$AD_T2, breaks = c(-Inf, cutPoints, Inf), labels = ADLevels) 
#' ordDVs = c("DEP_T1", "DEP_T2")
#' GFF[, ordDVs] = umxFactor(GFF[, ordDVs])
#' 
# # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff","fc","qol","hap","sat","DEP") 
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' 
#' # umx_set_optimizer("NPSOL")
#' # umx_set_optimization_options("mvnRelEps", .01)
#' m1 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' m2 = umxModify(m1, regex = "(cs_r[3-5]|c_cp_r[12])", name = "dropC", comp= TRUE)
#' 
#' # Do it using WLS
#' m3 = umxCP(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData,
#'			tryHard = "ordinal", type= "DWLS")
#'	# TODO umxCPL fix WLS here
#'	# label at row 1 and column 1 of matrix 'top.binLabels'' in model 'CP3fac' : object 'Vtot'
#'
#' # Correlated factors example
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD")
#' m5 = umxCP("correlated_causes", selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, 
#' 	 nFac = 3, correlatedA = TRUE, tryHard = "yes")
#' 
#' umxCompare(m4, m1)
#'
#' # What are the ace covariance labels? (two ways to get)
#' umx_lower.tri(m5$top$a_cp$labels)
#' parameters(m5, patt = "[ce]_cp")
#'
#' # =================================
#' # = Stop c and e from correlating =
#' # =================================
#' tmp = umx_lower.tri(m5$top$a_cp$labels)
#' tmp = namez(tmp, "a_cp", "[ce]_cp")
#' m6  = umxModify(m5, regex= tmp, name= "onlyAcorr", comp = TRUE)
#' 
#' # 1. Drop all (a|c|e) correlations, then add one a<->a covariance
#' tmp= namez(umx_lower.tri(m5$top$a_cp$labels), "a_cp", replace= "[ace]_cp")
#' m6 = umxModify(m5, regex= tmp, auto = FALSE)
#' # 2. now free back up "a2_a1_cov"
#' m6 = umxModify(m6, regex= "a_cp_r2c1", name= "a2_a1_cov", free=TRUE)
#' umxCompare(m6, m1)
#' } # end dontrun
#'
umxCP <- function(name = "CP", selDVs, selCovs=NULL, dzData= NULL, mzData= NULL, sep = NULL, nFac = 1, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), data = NULL, zyg = "zygosity", allContinuousMethod = c("cumulants", "marginals"), correlatedACE = FALSE, dzAr= .5, dzCr= 1, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, equateMeans= TRUE, weightVar = NULL, bVector = FALSE, boundDiag = 0, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = "deprecated") {
	# TODO umxCP: Add covariates to means model: Will involve xmu_make_top_twin? also means model?
	tryHard             = match.arg(tryHard)
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	nSib                = 2 # Number of siblings in a twin pair.
	# Add nFac to base name if no user-set name provided.
	if(name == "CP"){ name = paste0(name, nFac, "fac") }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}
	xmu_twin_check(selDVs= selDVs, dzData = dzData, mzData = mzData, enforceSep = TRUE, sep = sep, nSib = nSib, optimizer = optimizer)
	
	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	selVars   = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)
	nVar      = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	model     = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp       = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	varStarts = tmp$varStarts
	if(correlatedA != "deprecated"){
		message("Polite message: As of February 2021, please use 'correlatedACE' in place of 'correlatedA'.
		The new behavior with 'correlatedACE' still makes a_cp_matrix etc. type Lower, but leaves the off-diagonal elements fixed at zero.
		(you can then free each one as you choose)")
		correlatedACE = TRUE
	}
	if(correlatedACE){
		if(correlatedA != "deprecated"){
			a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # Latent common factor
			c_cp_matrix = umxMatrix("c_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # latent common factor Common environmental path coefficients
			e_cp_matrix = umxMatrix("e_cp", "Lower", nFac, nFac, free = TRUE, values = 0) # latent common factor Unique environmental path coefficients
		}else{
			a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # Latent common factor
			c_cp_matrix = umxMatrix("c_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # latent common factor Common environmental path coefficients
			e_cp_matrix = umxMatrix("e_cp", "Lower", nFac, nFac, free = FALSE, values = 0) # latent common factor Unique environmental path coefficients			
		}
		diag(a_cp_matrix$free) = TRUE
		diag(c_cp_matrix$free) = TRUE
		diag(e_cp_matrix$free) = TRUE
		diag(a_cp_matrix$values) = .7
		diag(c_cp_matrix$values) = .0
		diag(e_cp_matrix$values) = .7

		# a_cp_matrix$lbound[lower.tri(a_cp_matrix$lbound)] = 0
		# c_cp_matrix$lbound[lower.tri(a_cp_matrix$lbound)] = 0
		# e_cp_matrix$lbound[lower.tri(a_cp_matrix$lbound)] = 0

	} else {
		a_cp_matrix = umxMatrix("a_cp", "Diag" , nFac, nFac, free = TRUE, values = .7)
		c_cp_matrix = umxMatrix("c_cp", "Diag" , nFac, nFac, free = TRUE, values = .0)
		e_cp_matrix = umxMatrix("e_cp", "Diag" , nFac, nFac, free = TRUE, values = .7)
	}
	# Finish building top
	top = mxModel(model$top,
		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		umxMatrix("nFac_Unit", "Unit", nrow = nFac, ncol = 1),
		# Latent common factor genetic paths
		a_cp_matrix, c_cp_matrix, e_cp_matrix,
		# Constrain variance of latent phenotype factor to 1.0
		# Multiply by each path coefficient by its inverse to get variance component
		mxAlgebra(name = "A_cp", a_cp %*% t(a_cp)  ), # A_cp variance
		mxAlgebra(name = "C_cp", c_cp %*% t(c_cp)  ), # C_cp variance
		mxAlgebra(name = "E_cp", e_cp %*% t(e_cp)  ), # E_cp variance
		mxAlgebra(name = "L"   , A_cp + C_cp + E_cp), # total common factor covariance (a+c+e)
		mxAlgebra(name = "diagL", diag2vec(L)),
		mxConstraint(name = "fix_CP_variances_to_1", diagL == nFac_Unit),

		umxMatrix("as", "Lower", nVar, nVar, free = TRUE, values = .5), # Additive gen path 
		umxMatrix("cs", "Lower", nVar, nVar, free = TRUE, values = .1), # Common env path 
		umxMatrix("es", "Lower", nVar, nVar, free = TRUE, values = .5), # Unique env path
		umxMatrix("cp_loadings", "Full", nVar, nFac, free = TRUE, values = .5), # loadings on latent phenotype

		# Quadratic multiplication to add cp_loading effects
		mxAlgebra(name = "A"  , cp_loadings %&% A_cp + as %*% t(as)), # Additive genetic variance
		mxAlgebra(name = "C"  , cp_loadings %&% C_cp + cs %*% t(cs)), # Common environmental variance
		mxAlgebra(name = "E"  , cp_loadings %&% E_cp + es %*% t(es)), # Unique environmental variance
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(name= "expCovMZ", dimnames = list(selVars, selVars), 
					rbind( cbind(ACE, AC), 
		                   cbind(AC , ACE))
		),
		mxAlgebra(name= "expCovDZ", dimnames = list(selVars, selVars), 
					rbind( cbind(ACE, hAC),
		                   cbind(hAC, ACE))
		)
	)
	model = mxModel(model, top) 

	if(!freeLowerA){
		toset  = model$top$matrices$as$labels[lower.tri(model$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerC){
		toset  = model$top$matrices$cs$labels[lower.tri(model$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerE){
		toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix(name = "I", "Iden", nVar, nVar),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(name = "SD", solve(sqrt(I * ACE))),
			# Standard specific path coefficients
			mxAlgebra(name = "as_std", SD %*% as), # standardized a
			mxAlgebra(name = "cs_std", SD %*% cs), # standardized c
			mxAlgebra(name = "es_std", SD %*% es), # standardized e
			# Standardize loadings on Common factors
			mxAlgebra(name = "cp_loadings_std", SD %*% cp_loadings) # Standardized path coefficients (general factor(s))
		)
		model = mxModel(model, newTop)
		if(addCI){
			# TODO umxCP: break these CIs out into single labels?
			model = mxModel(model, mxCI(c('top.a_cp', 'top.c_cp', 'top.e_cp', 'top.as_std', 'top.cs_std', 'top.es_std', 'top.cp_loadings_std')))
		}
	}
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a_cp)")
				}
			}
			newCPLbound = model$top$matrices$a_cp@lbound
			diag(newCPLbound) = boundDiag; 
			model$top$a_cp$lbound = newCPLbound
			model$top$c_cp$lbound = newCPLbound
			model$top$e_cp$lbound = newCPLbound
			newSpecLbound = model$top$matrices$as@lbound
			diag(newSpecLbound) = boundDiag; 
			model$top$as$lbound = newSpecLbound
			model$top$cs$lbound = newSpecLbound
			model$top$es$lbound = newSpecLbound
		}
	}
	# Set values with the same label to the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelCP")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)	
	return(model)
} # end umxCP

#' umxIP: Build and run an Independent pathway twin model
#'
#' @description
#' Make a 2-group Independent Pathway twin model (Common-factor independent-pathway multivariate model).
#' The following figure shows the IP model diagrammatically:
#'
#' \if{html}{\figure{IP.png}{options: width="50\%" alt="Figure: IP.png"}}
#' \if{latex}{\figure{IP.pdf}{options: width=7cm}}
#'
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotype.
#' 
#' Features of the model include the ability to include add more one set of independent pathways, different numbers
#' of pathways for a, c, and e, as well the ability to use ordinal data, and different fit functions, e.g. WLS.
#' 
#' **note**: The function [umx_set_optimization_options()] allows users to see and set `mvnRelEps` and `mvnMaxPointsA`
#' mvnRelEps defaults to .005. For ordinal models, you might find that '0.01' works better.
#' 
#' @details
#' Like the [umxACE()] model, the CP model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent common factors which, in turn, account for 
#' variance in the phenotypes (see Figure).
#' 
#' 
#' **Data Input**
#' Currently, `umxIP` accepts only raw data. This may change in future versions. You can
#' choose other fit functions, e.g. WLS.
#' 
#' **Ordinal Data**
#' 
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' **Additional features**
#' 
#' `umxIP` supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in IP model**
#' 
#' A good way to see which matrices are used in umxIP is to run an example model and plot it.
#'
#' All the shared matrices are in the model "top".
#' 
#' Matrices `as`, `cs`, and `es` contain the path loadings specific to each variable on their diagonals.
#' 
#' To see the 'as' values, you can simply execute:
#'
#' m1$top#as$values
#' 
#' m1$top#as$labels
#' 
#' m1$top#as$free
#' 
#' Labels relevant to modifying the specific loadings take the form "as_r1c1", "as_r2c2" etc.
#' 
#' The independent-pathway loadings on the manifests are in matrices `a_ip`, `c_ip`, `e_ip`.
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`.
#' This has 1 row, and the columns are laid out for each variable 
#' for twin 1, followed by each variable for twin 2.
#'
#' So, in a model where the means for twin 1 and twin 2 had been equated
#' (set = to T1), you could make them independent again with this line:
#'
#' `m1$top$expMean$labels[1,4:6] = c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' @param name The name of the model (defaults to "IP").
#' @param selDVs The base names of the variables to model. note: Omit suffixes - just "dep" not c("dep_T1", "dep_T2")
#' @param sep The suffix for twin 1 and twin 2. e.g. selDVs= "dep", sep= "_T" -> c("dep_T1", "dep_T2")
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param nFac How many common factors for a, c, and e. If one number is given, applies to all three.
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param data If provided, dzData and mzData are treated as levels of zyg to select() MZ and DZ data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param numObsDZ = For cov data, the number of DZ pairs.
#' @param numObsMZ = For cov data, the number of MZ pairs.
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param correlatedA Whether factors are allowed to correlate (not implemented yet: FALSE).
#' @param autoRun Whether to run and return the model (default), or just to create and return without running.
#' @param tryHard Whether to tryHard (default 'no' uses normal mxRun). options: "mxTryHard", "mxTryHardOrdinal", or "mxTryHardWideSearch"
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param weightVar If a weighting variable is provided, a vector objective will be used to weight the data. (default = NULL).
#' @param addStd Whether to add algebras for a standardized model (defaults to TRUE).
#' @param addCI Whether to add CIs (defaults to TRUE).
#' @param freeLowerA ignore: Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC ignore: Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE ignore: Whether to leave the lower triangle of E free (default = FALSE).
#' @return - [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 =    umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' 
#' # WLS example: Use "marginals" method to enable all continuous data with missingness.
#' m3 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, 
#'		type = "DWLS", allContinuousMethod='marginals')
#' # omit missing to enable default WLS method to work on all continuous data
#' dzD = na.omit(dzData[, tvars(selDVs, "_T")])
#' mzD = na.omit(dzData[, tvars(selDVs, "_T")])
#' m4 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzD, mzData = mzD, type = "DWLS")
#'
#' # ====================================================================
#' # = Try with a non-default number of a, c, and e independent factors =
#' # ====================================================================
#' nFac = c(a = 2, c = 1, e = 1)
#' m2 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, nFac = nFac, 
#'		tryHard = "yes")
#' umxCompare(m1, m2)
#' }
#
umxIP <- function(name = "IP", selDVs, dzData, mzData, sep = NULL, nFac = c(a=1, c=1, e=1), data = NULL, zyg = "zygosity", type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals"), dzAr = .5, dzCr = 1, correlatedA = FALSE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, equateMeans = TRUE, weightVar = NULL, addStd = TRUE, addCI = TRUE, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE) {
	# TODO implement correlatedA
	type                = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	tryHard             = match.arg(tryHard)
	nSib                = 2 # Number of siblings in a twin pair.

	if(correlatedA){ message("Sorry, I haven't implemented correlated A yet in umxIP...") }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}
	# TODO umxIP: check covariates
	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)

	if(length(nFac) == 1){
		nFac = c(a = nFac, c = nFac, e = nFac)
	} else if (length(nFac) == 3){
		if(is.null(names(nFac))){
			names(nFac)=c("a","c","e")
		}
		if(!all(names(nFac)==c("a","c","e"))){
			stop("nFac must be named a=, c=, and e=")
		}
	}else{
		stop("nFac must be either 1 number or 3. You gave me ", length(nFac))
	}

	if(dzCr == .25 & (name == "IP")){
		name = "IP_ADE"
	}else if(name == "IP"){
		# Add nFac to base name if no user-set name provided.
		if (length(nFac) == 1){
			name = paste0(name, nFac, "fac")
		}else{
			name = paste0(name, paste0(c("a", "c", "e"), nFac, collapse = ""))
		}
	}

	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	selVars = tvars(selDVs, sep = sep, suffixes = 1:nSib)
	nVar    = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	model   = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= NULL, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, verbose= FALSE)

	# TODO: umxIP improve start values (hard coded at std type values)
	# tmp = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	# varStarts = tmp$varStarts

	top = mxModel(model$top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique environmental paths
		# Matrices ai, ci, and ec, to store a, c, and e path coefficients for independent general factors
		umxMatrix("ai", "Full", nVar, nFac['a'], free = TRUE, values = .6, jiggle = .05), # latent common factor Additive genetic path 
		umxMatrix("ci", "Full", nVar, nFac['c'], free = TRUE, values = .0, jiggle = .05), # latent common factor Common #environmental path coefficient
		umxMatrix("ei", "Full", nVar, nFac['e'], free = TRUE, values = .6, jiggle = .05), # latent common factor Unique environmental path #coefficient
		# Matrices as, cs, and es, to store a, c, and e path coefficients for specific factors
		umxMatrix("as", "Lower", nVar, nVar, free = TRUE, values = .6, jiggle = .05), # Additive genetic path 
		umxMatrix("cs", "Lower", nVar, nVar, free = TRUE, values = .0, jiggle = .05), # Common environmental path 
		umxMatrix("es", "Lower", nVar, nVar, free = TRUE, values = .6, jiggle = .05), # Unique environmental path.

		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

		# Multiply by each path coefficient by its inverse to get variance component
		# Sum the squared independent and specific paths to get total variance in each component
		mxAlgebra(name = "A", ai%*%t(ai) + as%*%t(as) ), # Additive genetic variance
		mxAlgebra(name = "C", ci%*%t(ci) + cs%*%t(cs) ), # Common environmental variance
		mxAlgebra(name = "E", ei%*%t(ei) + es%*%t(es) ), # Unique environmental variance

		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(rbind (cbind(ACE, AC), 
		                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name = "expCovMZ"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ"),

		# Algebra to compute total variances and standard deviations (diagonal only)
		mxMatrix("Iden", nrow = nVar, name = "I"),
		mxAlgebra(solve(sqrt(I * ACE)), name = "iSD")
	)
	model = mxModel(model, top) 

	if(!freeLowerA){
		toset  = model$top$matrices$as$labels[lower.tri(model$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(!freeLowerC){
		toset  = model$top$matrices$cs$labels[lower.tri(model$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	
	if(!freeLowerE){
		toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	} else {
		# set the first column off, bar r1
		model = omxSetParameters(model, labels = "es_r[^1]0-9?c1", free = FALSE, values = 0)

		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)

		# Used to drop the ei paths, as we have a full Cholesky for E, now just set the bottom row TRUE
		# toset = umxGetParameters(model, "^ei_r.c.", free= TRUE)
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix("Iden", nrow = nVar, name = "I"),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(solve(sqrt(I * ACE)), name = "SD"),
			# Standard general path coefficients
			mxAlgebra(SD %*% ai, name = "ai_std"), # standardized ai
			mxAlgebra(SD %*% ci, name = "ci_std"), # standardized ci
			mxAlgebra(SD %*% ei, name = "ei_std"), # standardized ei
			# Standardize specific path coefficients
			mxAlgebra(SD %*% as, name = "as_std"), # standardized as
			mxAlgebra(SD %*% cs, name = "cs_std"), # standardized cs
			mxAlgebra(SD %*% es, name = "es_std")  # standardized es
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.ai_std', 'top.ci_std', 'top.ei_std', 'top.as_std', 'top.cs_std', 'top.es_std')))
		}
	}
	model  = omxAssignFirstParameters(model) # ensure parameters with the same label have the same start value... means, for instance.
	model = as(model, "MxModelIP")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	return(model)
} # end umxIP


#' Generic SEM factor model loading rotation function
#'
#' See [umxRotate.MxModelCP()] to rotate the factor loadings of a [umxCP()] model
#'
#' @param model a model to rotate
#' @param rotation name of the rotation.
#' @param tryHard Default ("yes") is to tryHard
#' @param freeLoadingsAfter Whether to keep the rotated loadings fixed (Default, free them again)
#' @param verbose print detail about the rotation
#' @return - Rotated solution
#' @family Reporting functions
#' @export
#' @md
umxRotate <- function(model, rotation = c("varimax", "promax"),  tryHard = "yes", freeLoadingsAfter = TRUE, verbose = TRUE){
  UseMethod("umxRotate", model)
} 

#' @export
umxRotate.default <- function(model, rotation = c("varimax", "promax"),  tryHard = "yes", freeLoadingsAfter = TRUE, verbose = TRUE){
	stop("umxRotate is not defined for objects of class:", class(model))
}

#' Rotate a CP solution
#'
#' @description
#' Rotate a CP solution.
#' Should work with rotations provided in `library("GPArotation")` and `library("psych")`, e.g
#' 
#' **Orthogonal**: "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor"
#' 
#' **Oblique**: "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin" and "cluster"
#'
#'
#' @details This works by taking the common-pathways loadings matrix from a solved [umxCP()] model, rotating these, placing
#' them back into the loadings matrix, re-estimating the model with the parameters fixed at this rotation, then return the new model.
#'
#' @param model a [umxCP()] model to rotate.
#' @param rotation name of the rotation.
#' @param tryHard Default ("yes") is to tryHard.
#' @param freeLoadingsAfter return the model with factor loadings free (default) or fixed in the new locations.
#' @param verbose print detail about the rotation
#' @return - Rotated solution.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxCP()]
#' @md
#' @examples
#' # Rotate a CP solution(param)
#' # Common pathway model rotation
#' \dontrun{
#' library(umx)
#' # Fit 3 factor CPM
#' data(GFF)
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs = selDVs, nFac = 2, data = data, tryHard = "yes")
#' m2 = umxRotate(m1, rotation = "varimax",  tryHard = "yes")
#' 
#' }
umxRotate.MxModelCP <- function(model, rotation = c("varimax", "promax"),  tryHard = "yes", freeLoadingsAfter = TRUE, verbose = TRUE) {
	rotation = match.arg(rotation)
	# TODO: Check nFac > 1)

	# 1. get loadings
	x = model$top$cp_loadings$values

	# 2. rotate matrix
	rotated = eval(parse(text = paste0(rotation, "(x)")))

	# 3. fix loadings at their new rotated values
	model$top = omxSetParameters(model$top, labels= model$top$cp_loadings$labels, values = rotated$loadings, free = FALSE)
	# run the model to re-estimate common and residual loadings given the (fixed) rotated loadings
	model = xmu_safe_run_summary(model, autoRun = TRUE, tryHard = tryHard, comparison = TRUE, digits = 3)

	# free the values so mxCompare gets the right answers
	if(freeLoadingsAfter){
		model$top = omxSetParameters(model$top, labels= model$top$cp_loadings$labels, free = TRUE)
	}
	if(verbose){
		print("Rotation results")
		print(rotated$loadings) # print out the nice rotation result
		rotmat = rotated$rotmat
		print("Factor Correlation Matrix")
		print(solve(t(rotmat) %*% rotmat))
	}
	return(model)
}


# =====================================
# = Advanced Build and Modify helpers =
# =====================================

#' xmuRAM2Ordinal 
#'
#' xmuRAM2Ordinal: Convert a RAM model whose data contain ordinal variables to a threshold-based model
#'
#' @param model An RAM model to add thresholds too.
#' @param name = A new name for the modified model. Default (NULL) = leave it as is).
#' @param verbose Tell the user what was added and why (Default = TRUE).
#' @return - [mxModel()]
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()]
#' @md
#' @examples
#' \dontrun{
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' obesityLevels   = c('normal', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' m1 = umxRAM("tim", data = mzData,
#'		umxPath("bmi1", with = "bmi2"),
#'		umxPath(v.m.= c("bmi1", "bmi2"))
#')
#'
#' m1 = umxRAM("tim", data = mzData,
#' 	umxPath("obese1", with = "obese2"),
#' 	umxPath(v.m.= c("obese1", "obese2"))
#' )
#' }
xmuRAM2Ordinal <- function(model, verbose = TRUE, name = NULL) {
	if(!umx_is_RAM(model)){
		stop("xmuRAM2Ordinal only works with RAM models, sorry.")
	}
	if(!is.null(name)){
		model = mxRename(model, name)
	}
	model$expectation$thresholds = "threshMat"
	
	model = mxModel(model, umxThresholdMatrix(model$data$observed, fullVarNames = model$manifestVars, verbose = verbose))
	return(model)
}

#' xmuValues: Set values in RAM model, matrix, or path
#'
#' For models to be estimated, it is essential that path values start at credible values. 
#' `xmuValues` takes on that task for you.
#' 
#' xmuValues can set start values for the free parameters in both RAM and Matrix [mxModel()]s. 
#' It can also take an mxMatrix as input.
#' It tries to be smart in guessing starts from the values in your data and the model type.
#' 
#' *note*: If you give xmuValues a numeric input, it will use obj as the mean, and return a 
#' list of length n, with sd = sd.
#'
#' @param obj The RAM or matrix [mxModel()], or [mxMatrix()] that you want to set start values for.
#' @param sd Optional Standard Deviation for start values
#' @param n Optional Mean for start values
#' @param onlyTouchZeros Don't alter parameters that have starts (useful to speed [umxModify()])
#' @return - [mxModel()] with updated start values
#' @export
#' @seealso - Core functions:
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#'
#' # ====================================================================
#' # = Make an OpenMx model (which will lack start values and labels..) =
#' # ====================================================================
#' m1 = mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents  , to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents  , arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs=500)
#' )
#' mxEval(S, m1) # default variances are jiggled away from near-zero
#' # Add start values to the model
#' m1 = xmuValues(m1)
#' mxEval(S, m1) # plausible variances
#' umx_print(mxEval(S,m1), 3, zero.print = ".") # plausible variances
#' xmuValues(14, sd = 1, n = 10) # Return vector of length 10, with mean 14 and sd 1
xmuValues <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = FALSE) {
	if(is.numeric(obj) ) {
		# Use obj as the mean, return a list of length n, with sd = sd
		return(xmu_start_value_list(mean = obj, sd = sd, n = n))
	} else if (umx_is_MxMatrix(obj) ) {
		message("I don't know how to create values for a matrix: too many options.")
	} else if (umx_is_RAM(obj) ) {
		# This is a RAM Model: Set sane starting values
		# Means at manifest means
		# S at variance on diag, quite a bit less than cov off diag
		# TODO: Start latent means?...
		# TODO: Handle sub models...
		if (length(obj$submodels) > 0) {
			stop("xmuValues cannot yet handle sub-models. Build each with umxRAM, then use umxSuperModel to assemble")
		}
		if (is.null(obj$data)) {
			stop("'model' does not contain any data")
		}
		if(!is.null(obj$matrices$Thresholds)){
			message("This is a threshold RAM model... Not sure how to set values in these yet, so left it as-is.")
			return(obj)
		}
		theData   = obj$data$observed
		type      = obj$data$type
		manifests = obj@manifestVars
		latents   = obj@latentVars
		nVar      = length(manifests)

		if(length(latents) > 0){
			lats = (nVar + 1):(nVar + length(latents))
			# The diagonal is variances
			if(onlyTouchZeros) {
				freePaths = (obj$matrices$S$free[lats, lats] == TRUE) & obj$matrices$S$values[lats, lats] == 0
			} else {
				freePaths = (obj$matrices$S$free[lats, lats] == TRUE)			
			}
			# set free latent variances to 1
			obj$S$values[lats, lats][freePaths] = 1
			offDiag = !diag(length(latents))
			newOffDiags = obj$matrices$S$values[lats, lats][offDiag & freePaths]/3
			obj$S@values[lats, lats][offDiag & freePaths] = newOffDiags			
		}
		
		if(nVar == 0){
			# model with no manifests... nothing to set. Maybe it's a model with only defVars or something.
			return(obj)
		}
		# =============
		# = Set means =
		# =============
		# print(str(obj$data))
		# umx_msg(obj$data$preferredFit)
		# umx_msg(obj$data$.wlsContinuousType)

		if(is.null(obj$matrices$M)){
			# no means: must be cov data?
			# We are in a RAM model, so the data must be mxData: check the type, rather than guessing.
			# need to handle raw data that will be treated as WLS and not end up with means
			if(type == "raw"){
				covData = umx_var(df = theData[, manifests, drop = FALSE], format = "full", ordVar = 1, use = "pairwise.complete.obs", allowCorForFactorCovs=TRUE)
			}else if (type == "acov"){
				covData = as.matrix(theData)
			}else if (type %in% c("cov", "cor")){
				covData = as.matrix(theData)
			}else{
				message("xmuValues can't recognise data of type ", type, ". I only know raw, cov, cor, and acov")
				covData = as.matrix(theData)
			}
		} else {
			dataMeans = umx_means(theData[, manifests, drop = FALSE], ordVar = 0, na.rm = TRUE)
			freeManifestMeans = (obj$matrices$M$free[1, manifests] == TRUE)
			obj$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
			# covData = cov(theData, )
			covData = umx_var(df = theData[, manifests, drop = FALSE], format = "full", ordVar = 1, use = "pairwise.complete.obs", allowCorForFactorCovs=TRUE)
		}

		# ==========================================================
		# = Fill the S (symmetrical) matrix with good start values =
		# ==========================================================
		# Set S diagonal (variances) where the cells are free.
		# if(!is.null(dim(covData)) || length(covData) > 1){
			# covData = diag(covData)
		# } else {
			# If this is one variable, leave alone: equivalent to a 1,1, matrix with the diag on the "diag", and zeros elsewhere
		# }
		# diag diag creates a matrix with all zeros off the diagonal
		# covData = diag(diag(covData))

		freePaths = diag(obj$S$free) == TRUE
		if(onlyTouchZeros) {
			freePaths = freePaths & diag(obj$S$values) == 0
		}
		diag(obj$S@values)[freePaths] = diag(covData)[freePaths]

		# =======================
		# = Set off-diag values =
		# =======================
		# TODO decide whether to leave this as independence, or set to non-zero covariances...
		# and off diagonals to the observed covariance,
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# obj$matrices$S$values[1:nVar, 1:nVar][freePaths] = (covData[freePaths]/2)
		# offDiag = !diag(nVar)
		# newOffDiags = obj$matrices$S$values[1:nVar, 1:nVar][offDiag & freePaths]/3
		# obj$matrices$S$values[1:nVar, 1:nVar][offDiag & freePaths] = newOffDiags

		# ======================================================
		# = Put modest starts into the asymmetric (one headed) =
		# ======================================================
		freePaths = obj$matrices$A$free == TRUE
		if(onlyTouchZeros){
			freePaths = freePaths & obj$matrices$A$values == 0
		}
		# TODO umxRAM A starts change from .9 to sqrt(.2*Variance)/nFactors
		obj$A@values[freePaths] = .9
		return(obj)
	} else {
		stop("'obj' must be an mxMatrix, a RAM model, or a simple number")
	}
}

#' xmuLabel: Add labels to a RAM model, matrix, or path
#'
#' xmuLabel adds labels to things, be it an: [mxModel()] (RAM or matrix based), an [mxPath()], or an [mxMatrix()]
#' This is a core function in umx: Adding labels to paths opens the door to [umxEquate()], as well as [omxSetParameters()]
#'
#' @param obj An [mxModel()] (RAM or matrix based), [mxPath()], or [mxMatrix()]
#' @param suffix String to append to each label (might be used to distinguish, say male and female submodels in a model)
#' @param baseName String to prepend to labels. Defaults to NA ("")
#' @param setfree Whether to label only the free paths (defaults to FALSE)
#' @param drop The value to fix "drop" paths to (defaults to 0)
#' @param jiggle How much to jiggle values in a matrix or list of path values
#' @param labelFixedCells = TRUE
#' @param boundDiag Whether to bound the diagonal of a matrix
#' @param verbose How much feedback to give the user (default = FALSE)
#' @param overRideExisting = FALSE
#' @param name Optional new name if given a model. Default (NULL) does not rename model.
#' @return - [mxModel()]
#' @export
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' # ==============================================================
#' # = Show how OpenMx models are not labeled, and then add labels =
#' # ==============================================================
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 = mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents  , to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents  , arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs=500)
#' )
#'
#' umxGetParameters(m1) # Default "matrix address" labels, i.e "One Factor.S[2,2]"
#' m1 = xmuLabel(m1)
#' umxGetParameters(m1, free = TRUE) # Informative labels: "G_to_x1", "x4_with_x4", etc.
#'
#' # =======================================================================
#' # = Create a new model, with suffixes added to paths, and model renamed =
#' # =======================================================================
#' m2 = xmuLabel(m1, suffix= "_male", overRideExisting= TRUE, name = "male")
#' umxGetParameters(m2, free = TRUE) # suffixes added
#' 
#' # =============================
#' # = Example Labeling a matrix =
#' # =============================
#' a = xmuLabel(mxMatrix(name = "a", "Full", 3, 3, values = 1:9))
#' a$labels
#' a = xmuLabel(mxMatrix(name = "a", "Full", 3, 3, values = 1:9), baseName="bob")
#' a$labels
#' # note: labels with "data." in the name are left untouched!
#' a = mxMatrix(name = "a", "Full", 1,3, labels = c("data.a", "test", NA))
#' a$labels
#' xmuLabel(a, verbose = TRUE)
#' xmuLabel(a, verbose = TRUE, overRideExisting = FALSE)
#' xmuLabel(a, verbose = TRUE, overRideExisting = TRUE)
xmuLabel <- function(obj, suffix = "", baseName = NA, setfree = FALSE, drop = 0, labelFixedCells = TRUE, jiggle = NA, boundDiag = NA, verbose = FALSE, overRideExisting = FALSE, name = NULL) {	
	# TODO xmuLabel: Change these to an S3 method with three classes...
	# 	Check that arguments not used by a particular class are not set away from their defaults
	# 	Perhaps make "A_with_A" --> "var_A"
	# 	Perhaps make "one_to_x2" --> "mean_x2" best left as is
	if (is(obj, "MxMatrix") ) { 
		# Label an mxMatrix
		xmuLabel_Matrix(mx_matrix = obj, baseName = baseName, setfree = setfree, drop = drop, labelFixedCells = labelFixedCells, jiggle = jiggle, boundDiag = boundDiag, suffix = suffix, verbose = verbose, overRideExisting = overRideExisting)
	} else if (umx_is_RAM(obj)) { 
		# Label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(model = obj, suffix = suffix, labelFixedCells = labelFixedCells, overRideExisting = overRideExisting, verbose = verbose, name = name))
	} else if (umx_is_MxModel(obj) ) {
		# Label a non-RAM matrix lamodel
		return(xmuLabel_MATRIX_Model(model = obj, suffix = suffix, verbose = verbose))
	} else {
		stop("I can only label OpenMx models and mxMatrix types. You gave me a ", typeof(obj))
	}
}

# TODO implement umxDefVar
# umxDefVar(selDefs[1], name ="mod1"){
# umxDefVar(colName = selDefs[1], name ="mod1"){
# 	# "data.defmod1"
# 	# TODO handle vector of colNames, return list of matrices
# 	umxMatrix(name, "Full", nrow=1, ncol=1, free=FALSE, labels=paste0("data.", colName))
# }

#' Make a mxMatrix with automatic labels. Also takes name as the first parameter for more readable code.
#'
#' @description
#' umxMatrix is a wrapper for mxMatrix which labels cells buy default, and has the name parameter first in order. 
#'
#' @param name The name of the matrix (Default = NA). Note the different order compared to mxMatrix!
#' @param type The type of the matrix (Default = "Full")
#' @param nrow Number of rows in the matrix: Must be set
#' @param ncol Number of columns in the matrix: Must be set
#' @param free Whether cells are free (Default FALSE)
#' @param values The values of the matrix (Default NA)
#' @param labels Either whether to label the matrix (default TRUE), OR a vector of labels to apply.
#' @param lbound Lower bounds on cells (Defaults to NA)
#' @param ubound Upper bounds on cells (Defaults to NA)
#' @param byrow  Whether to fill the matrix down columns or across rows first (Default = getOption('mxByrow')
#' @param dimnames NA
#' @param baseName Set to override the default (which is to use the matrix name as the prefix).
#' @param condenseSlots Whether to save memory by NULLing out unused matrix elements, like labels, ubound etc. Default = getOption('mxCondenseMatrixSlots')
#' @param ... Additional parameters (!! not currently supported by umxMatrix)
#' @param joinKey See mxMatrix documentation: Defaults to as.character(NA)
#' @param joinModel See mxMatrix documentation: Defaults to as.character(NA)
#' @param jiggle = NA passed to xmuLabel to jiggle start values (default does nothing)
#' @return - [mxMatrix()]
#' @export
#' @family Core Model Building Functions
#' @seealso - [xmu_simplex_corner()], [mxMatrix()], [xmuLabel()], [umxRAM()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' # ==================================================================================
#' # = 1. Showing how name is first parameter, and how cells are labelled by default. =
#' # ==================================================================================
#' umxMatrix("test", "Full", 2, 2)$labels
#' #      [,1]        [,2]
#' # [1,] "test_r1c1" "test_r1c2"
#' # [2,] "test_r2c1" "test_r2c2"
#'
#'# ===========================================================
#'# = 2. Over-ride default (matrix name) as prefix for labels =
#'# ===========================================================
#' umxMatrix("test", "Full", 2, 2, baseName = "bob")$labels # bob_r1c1
#'
#'
#'# ==========================================
#'# = 3. User-provided labels are left as-is =
#'# ==========================================
#' umxMatrix("foo", "Lower", nrow=2, ncol=2, labels= c(NA, "beta1", NA))
#' #      [,1]    [,2]
#' # [1,] NA      NA  
#' # [2,] "beta1" NA  
#'
umxMatrix <- function(name = NA, type = "Full", nrow = NA, ncol = NA, free = FALSE, values = NA, labels = TRUE, lbound = NA, ubound = NA, byrow = getOption('mxByrow'), baseName = NA, dimnames = NA, condenseSlots = getOption('mxCondenseMatrixSlots'), ..., joinKey = as.character(NA), joinModel = as.character(NA), jiggle = NA) {
	legalMatrixTypes = c("Diag", "Full", "Iden", "Lower", "Sdiag", "Stand", "Symm", "Unit",  "Zero")
	if(name %in% legalMatrixTypes){
		warning("You used ", omxQuotes(name), " as the name of your matrix: That's also a valid type, so make sure you're not putting type first...")
	}
	if(is.numeric(type)){
		stop("You used ", omxQuotes(type), " as the type of your matrix. You probably need to add something like type='Full' or specify nrow and ncol")
	}

	if(isTRUE(labels)){
		setLabels = TRUE
		labels    = NA
	} else {
		setLabels = FALSE
	}

	x = mxMatrix(type = type, nrow = nrow, ncol = ncol, free = free, values = values, labels = labels, lbound = lbound, ubound = ubound, byrow = byrow, dimnames = dimnames, name = name, condenseSlots = condenseSlots, joinKey = joinKey, joinModel = joinModel, ...)
	if(setLabels){
		x = xmuLabel(x, baseName = baseName, jiggle = jiggle)
	}
	return(x)
}

#' A simple wrapper for mxAlgebra with name as the first parameter for more readable compact code.
#'
#' @description
#' umxAlgebra is a wrapper for mxAlgebra which has the name parameter first in order. 
#'
#' @param name The name of the algebra (Default = NA). Note the different order compared to mxAlgebra!
#' @param expression The algebra
#' @param dimnames Dimnames of the algebra
#' @param ... Other parameters
#' @param fixed = See mxAlgebra documentation
#' @param joinKey See mxAlgebra documentation
#' @param joinModel See mxAlgebra documentation
#' @param verbose Quiet or informative
#' @param initial See mxAlgebra documentation
#' @param recompute See mxAlgebra documentation
#' @return - [mxAlgebra()]
#' @export
#' @family Advanced Model Building Functions	
#' @seealso - [umxMatrix()]
#' @md
#' @examples
#' x = umxAlgebra("circ", 2 * pi)
#' class(x$formula)
#' x = mxAlgebra(name = "circ", 2 * pi)
#' class(x$formula) # "call"
#'
umxAlgebra <- function(name = NA, expression, dimnames = NA, ..., joinKey=as.character(NA), joinModel=as.character(NA), verbose=0L, initial=matrix(as.numeric(NA),1,1), recompute=c('always','onDemand'), fixed = "deprecated_use_recompute") {
	if(class(name) != "character"){
		stop("In umxAlgebra, name comes first, not expression.")
	}
	x = mxAlgebra(expression, name = name, dimnames = dimnames, ..., joinKey=joinKey, joinModel=joinModel, verbose=verbose, initial=initial, recompute = recompute)
	return(x)
}


# =================================
# = Run Helpers =
# =================================

#' umxRun: Run an mxModel
#'
#' `umxRun` is a version of [mxRun()] which can run also set start values, labels, and run multiple times
#' It can also calculate the saturated and independence likelihoods necessary for most fit indices.
#' **Note** this is not needed for umxRAM models or twin models - it is just a convenience to get base OpenMx models to run.
#' @param model The [mxModel()] you wish to run.
#' @param n The maximum number of times you want to run the model trying to get a code green run (defaults to 1)
#' @param calc_SE Whether to calculate standard errors (ignored when n = 1)
#' for the summary (if you use [mxCI()] or [umxCI()], you can turn this off)
#' @param calc_sat Whether to calculate the saturated and independence models (for raw [mxData()] [mxModel()]s) (defaults to TRUE - why would you want anything else?)
#' @param setValues Whether to set the starting values of free parameters (default = FALSE)
#' @param setLabels Whether to set the labels (default =  FALSE)
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE) intervals = FALSE
#' @param comparison Whether to run umxCompare() after umxRun
#' @return - [mxModel()]
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 = mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents  , to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents  , arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs=500)
#' )
#'
#' m1 = umxRun(m1) # just run: will create saturated model if needed
#' \dontrun{
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE) # set start values and label all parameters
#' umxSummary(m1, std = TRUE)
#' m1 = mxModel(m1, mxCI("G_to_x1")) # add one CI
#' m1 = mxRun(m1, intervals = TRUE)
#' residuals(m1, run = TRUE) # get CIs on all free parameters
#' confint(m1) # OpenMx's SE-based CIs
#' umxConfint(m1, run = TRUE) # get likelihood-based CIs on all free parameters
#' m1 = umxRun(m1, n = 10) # re-run up to 10 times if not green on first run
#' }
#' 
umxRun <- function(model, n = 1, calc_SE = TRUE, calc_sat = TRUE, setValues = FALSE, setLabels = FALSE, intervals = FALSE, comparison = NULL){
	# TODO: umxRun: Return change in -2LL for models being re-run
	# TODO: umxRun: Stash saturated model for re-use
	# TODO: umxRun: Optimise for speed
	if(setLabels){
		model = xmuLabel(model)
	}
	if(setValues){
		model = xmuValues(model)
	}
	if(n == 1){
		model = mxRun(model, intervals = intervals);
	} else {
		model = mxOption(model, "Calculate Hessian", "No")
		model = mxOption(model, "Standard Errors", "No")
		# make an initial run
		model = mxRun(model);
		n = (n - 1); tries = 0
		# carry on if we failed
		while(model$output$status[[1]] == 6 && n > 2 ) {
			print(paste("Run", tries+1, "status Red(6): Trying hard...", n, "more times."))
			model <- mxRun(model)
			n <- (n - 1)
			tries = (tries + 1)
		}
		if(tries == 0){ 
			# print("Ran fine first time!")	
		}
		# get the SEs for summary (if requested)
		if(calc_SE){
			# print("Calculating Hessian & SEs")
			model = mxOption(model, "Calculate Hessian", "Yes")
			model = mxOption(model, "Standard Errors", "Yes")
		}
		if(calc_SE | intervals){
			model = mxRun(model, intervals = intervals)
		}
	}
	if(umx_is_RAM(model)){
		if(model$data$type == "raw"){
			# If we have a RAM model with raw data, compute the saturated and independence models
			# message("computing saturated and independence models so you have access to absolute fit indices for this raw-data model")
			ref_models = mxRefModels(model, run = TRUE)
			model@output$IndependenceLikelihood = as.numeric(-2 * logLik(ref_models$Independence))
			model@output$SaturatedLikelihood    = as.numeric(-2 * logLik(ref_models$Saturated))
		}
	}
	if(!is.null(comparison)){ 
		umxCompare(comparison, model) 
	}
	return(model)
}

# ==============================
# = Label and equate functions =
# ==============================

#' Change or fix parameters (e.g. their values, labels, bounds, ..) in a model. 
#'
#' `umxSetParameters` is used to alter values, and other parameter properties in an [mxModel()].
#' A common use is setting new values and changing parameters from free to false. 
#' *Note*: If you just want to modify and re-run a model, you probably want [umxModify()].
#' 
#' Using `umxSetParameters`, you use `labels=` to select the parameters you want to update. 
#' You can set their free/fixed state with `free=`, and set new values with `values = `. Likewise 
#' for bounds. 
#' 
#' `umxSetParameters` supports pattern matching (regular expressions) to select labels. Set `regex=`
#' to a regular expression matching the labels you want to select. e.g. "G_to_.*" would match
#' "G_to_anything".
#' 
#' **Details**
#' Internally, `umxSetParameters` is equivalent to a call to `omxSetParameters` where you 
#' have the ability to generate a pattern-based label list, 
#' and, because this can create duplicate labels, we also call [omxAssignFirstParameters()]
#' to equate the start values for parameters which now have identical labels.
#' 
#' @param model an [mxModel()] to set parameters in.
#' @param labels = labels to find
#' @param free = new value for free
#' @param values = new values
#' @param newlabels = newlabels
#' @param lbound = value for lbound
#' @param ubound = value for ubound
#' @param indep = whether to look in indep models
#' @param strict whether to complain if labels not found
#' @param name = new name for the returned model
#' @param regex patterns to match for labels (or if TRUE, use labels as regular expressions)
#' @param test Just show what you would do? (defaults to FALSE)
#' @return - [mxModel()]
#' @export
#' @family Model Summary and Comparison
#' @seealso - [umxModify()], [xmuLabel()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = mxData(demoOneFactor[1:80,], type = "raw"),
#' 	umxPath(from = latents, to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = latents)
#' )
#' parameters(m1)
#' # Match all labels
#  # Test run, showing all updated with an "m1_" in front
#' umxSetParameters(m1, regex = "^", newlabels= "m1_", test = TRUE)
#' # Change path to x1 to x2, equating these two paths
#' m2 = umxSetParameters(m1, "G_to_x1", newlabels= "G_to_x2", test = FALSE)
#' parameters(m2) 
umxSetParameters <- function(model, labels, free = NULL, values = NULL, newlabels = NULL, lbound = NULL, ubound = NULL, indep = FALSE, strict = TRUE, name = NULL, regex = FALSE, test = FALSE) {
	if(is.character(regex)){
		labels = regex
		regex = TRUE
	}
	nothingDoing = all(is.null(c(free, values, newlabels)))
	if(nothingDoing){
		warning("You are not setting anything: set one or more of free, values, or newlabels to update a parameter")
	}
	if(regex){
		oldLabels = umxGetParameters(model, regex = labels)
		if(!is.null(newlabels)){
			newlabels = gsub(labels, newlabels, oldLabels, ignore.case = FALSE)
		}
		labels = oldLabels
	}
	if(test){
		message("Found labels:", omxQuotes(labels))
		message("New labels:", omxQuotes(newlabels))
	} else {
		a = omxSetParameters(model = model, labels = labels, free = free, values = values,
	    newlabels = newlabels, lbound = lbound, ubound = ubound, indep = indep,
	    strict = strict, name = name)
	return(omxAssignFirstParameters(a, indep = FALSE))
	}
}

#' umxEquate: Equate two or more paths
#'
#' In addition to dropping or adding parameters, a second common task in modeling
#' is to equate parameters. umx provides a convenience function to equate parameters 
#' by setting one or more parameters (the "slave" set) equal to one or more "master" 
#' parameters. These parameters are picked out via their labels, and setting two or more
#' parameters to have the same value is accomplished by setting the slave(s) to have
#' the same label(s) as the master parameters, thus constraining them to take the same
#' value during model fitting.
#' 
#' \emph{note}: In addition to using this method to equating parameters, you can
#' also equate one parameter to another by setting its label to the 
#' "square bracket" address of the master, e.g. "a\[r,c\]".
#' 
#' \emph{Tip}: To find labels of free parameters use [umxGetParameters()] 
#' with free = TRUE
#' 
#' \emph{Tip}: To find labels by name, use the regex parameter of [umxGetParameters()]
#' 
#' @param model   An [mxModel()] within which to equate parameters listed in "a" with those in "b"
#' @param a  one or more parameter labels to equate with b labels
#' @param b  one or more labels to equate (if newNames is not set, these will set to the a labels, thus equating the parameters
#' @param newlabels (optional) list of new labels for the equated parameters.
#' @param free    Should parameter(s) initially be free? (default = TRUE)
#' @param verbose Whether to give verbose feedback (default = TRUE)
#' @param name    name for the returned model (optional: Leave empty to leave name unchanged)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param master  A list of "master" labels to which slave labels will be equated
#' @param slave   A list of slave labels which will be updated to match master labels, thus equating the parameters
#' @return - [mxModel()]
#' @export
#' @seealso [umxModify()], [umxCompare()]
#' @family Model Summary and Comparison
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' # By default, umxEquate just equates master and slave labels: doesn't run model
#' m2 = umxEquate(m1, a = "G_to_x1", b = "G_to_x2", name = "Eq x1 x2 loadings")
#' 
#' # Set autoRun = TRUE and comparison = TRUE to run and output a comparison
#' m2 = umxEquate(m1, autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2",
#' 	     a = "G_to_x1", b = "G_to_x2"
#' )
#'
#' # rename the equated paths
#' m2 = umxEquate(m1, autoRun = TRUE, comparison = TRUE, name = "Eq_x1_x2",
#' 	     a = "G_to_x1", b = "G_to_x2", newlabels = c("equated")
#' )
#' parameters(m2)
umxEquate <- function(model, a, b, newlabels= NULL, free = c(TRUE, FALSE, NA), verbose = FALSE, name = NULL, autoRun = FALSE, tryHard = c("no", "yes", "ordinal", "search"), comparison = TRUE, master= NULL, slave= NULL) {
	free = xmu_match.arg(free, c(TRUE, FALSE, NA)) # match.arg can't handle Boolean as options?
	tryHard = match.arg(tryHard)

	if(!is.null(master)){
		listA = master
		listB = slave
	}else{
		listA = a
		listB = b		
	}

	if(!umx_is_MxModel(model)){
		message("ERROR in umxEquate: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxEquate(model, listA=\"a_to_b\", listB=\"a_to_c\", name=\"model2\") # equate paths a->b and a->c, in a new model called \"model2\"")
		stop()
	}

	if(length(listA) == 1){
		if(length(grep("[\\^\\.\\*\\[\\(\\+\\|]+", listA) ) < 1){ # no grep found: add some anchors
			listA = paste0("^", listA, "$"); # anchor to the start of the string
			listB  = paste0("^", listB,  "$");
			if(verbose == TRUE){
				cat("note: matching whole label\n");
			}
		}
	}
	listALabels = umxGetParameters(model, regex = listA, free = free, verbose = verbose)
	listBLabels = umxGetParameters(model, regex = listB, free = free, verbose = verbose)
	if( length(listBLabels) != length(listALabels) && (length(listALabels)!=1)) {
		print(list(listALabels = listALabels, listBLabels = listBLabels))
		stop("ERROR in umxEquate: listA and listB labels not the same length!\n",
		length(listBLabels), " list B labels found, and ", length(listALabels), " list As")
	}
	if(length(listBLabels) == 0) {
		legal = names(omxGetParameters(model, indep=FALSE, free=free))
		legal = legal[which(!is.na(legal))]
		message("Labels available in model are: ", paste(legal, ", "))
		stop("ERROR in umxEquate: no listB labels found or none requested!")
	}
	# print(list(listALabels = listALabels, listBLabels = listBLabels))
	if(is.null(newlabels)){
		newModel = omxSetParameters(model = model, labels = listBLabels, newlabels = listALabels, name = name)
	} else {
		umx_check(length(newlabels)==length(listALabels), "stop", "newlabels must be the same length as list a. ", 
			"Found ", length(listALabels), " list a labels, and ", length(newlabels), " newlabels"
		)
		newModel = omxSetParameters(model = model   , labels = listALabels, newlabels = newlabels, name = name)
		newModel = omxSetParameters(model = newModel, labels = listBLabels, newlabels = newlabels, name = name)
	}
	
	newModel = omxAssignFirstParameters(newModel, indep = FALSE)
	newModel = xmu_safe_run_summary(newModel, model, autoRun = autoRun, tryHard = tryHard, comparison = comparison)
	return(newModel)
}

#' umxFixAll: Fix all free parameters
#'
#' Fix all free parameters in a model using omxGetParameters()
#'
#' @param model an [mxModel()] within which to fix free parameters
#' @param verbose whether to mention how many paths were fixed (default is FALSE)
#' @param name optional new name for the model. if you begin with a _ it will be made a suffix
#' @param run  whether to fix and re-run the model, or just return it (defaults to FALSE)
#' @return - the fixed [mxModel()]
#' @export
#' @family Advanced Model Building Functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' 
#' m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m2 = umxFixAll(m1, run = TRUE, verbose = TRUE)
#' mxCompare(m1, m2)
umxFixAll <- function(model, name = "_fixed", run = FALSE, verbose= FALSE){
	if(!umx_is_MxModel(model)){
		message("ERROR in umxFixAll: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is m1 = umxFixAll(m1)")
		stop()
	}
	if(is.null(name)){
		name = model$name
	} else if("_" == substring(name, 1, 1)){
		name = paste0(model$name, name)
	}
	oldFree = names(omxGetParameters(model, indep = TRUE, free = TRUE))
	if(verbose){
		message("fixed ", length(oldFree), " paths")
	}
	model = omxSetParameters(model, oldFree, free = FALSE, strict = TRUE, name = name)
	if(run){
		model = mxRun(model)
	}
	return(model)
}



# ===============
# = RAM Helpers =
# ===============


# ===========================
# = matrix-oriented helpers =
# ===========================

#' Create the threshold matrix needed for modeling ordinal data.
#'
#' High-level helper for ordinal modeling. Creates, labels, and sets smart-starts for this 
#' complex set set of an algebra and matrices. Big time saver!
#'
#' @details We often need to model ordinal data: sex, low-med-hi, depressed/normal, etc., 
#' A useful conceptual strategy to handle these data is to build a standard model for normally-varying data 
#' and then to threshold this normal distribution to generate the observed data. Thus an observation of "depressed"
#' is modeled as a high score on the latent normally distributed trait, with thresholds set so that only scores above
#' this threshold (1-minus the number of categories) reach the criteria for the diagnosis.
#' 
#' Making this work can require fixing the first 2 thresholds of ordinal data, or fixing both the mean and variance of
#' a latent variable driving binary data, in order to estimate its one-free parameter: where to place the single threshold
#' separating low from high cases.
#' 
#' The function returns a 3-item list consisting of:
#' 
#' 1. A thresholdsAlgebra (named `threshMatName`)
#' 2. A matrix of deviations for the thresholds (`deviations_for_thresh`)
#' 3. A lower matrix of ones (`lowerOnes_for_thresh`)
#'
#' *Twin Data*
#'
#' With twin data, make sure to provide the **full names** for twin data... this is not standard I know...
#' 
#' For twins (the function currently handles only pairs), the thresholds are equated for both twins using labels:
#'
#' $labels
#' 
#'       obese_T1         obese_T2
#' 
#' dev_1 "obese_dev1"   "obese_dev1"
#'
#' @param df The data being modeled (to allow access to the factor levels and quantiles within these for each variable)
#' @param fullVarNames The variable names. Note for twin data, just the base names, which sep will be used to fill out.
#' @param sep (e.g. "_T") Required for wide (twin) data. It is used to break the base names our from their numeric suffixes.
#' @param method How to implement the thresholds: Mehta, (1 free thresh for binary, first two fixed for ordinal) or "allFree"
#' @param l_u_bound c(NA, NA) by default, you can use this to bound the first (base) threshold.
#' @param droplevels Whether to drop levels with no observed data (defaults to FALSE)
#' @param threshMatName name of the matrix which is returned. Defaults to "threshMat" - best not to change it.
#' @param verbose How much to say about what was done. (defaults to FALSE)
#' @param selDVs deprecated. Use "fullVarNames"
#' @return - list of thresholds matrix, deviations, lowerOnes
#' @export
#' @seealso [OpenMx::mxThreshold()]
#' @family Advanced Model Building Functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#'
#' # ============================
#' # = Simple non-twin examples =
#' # ============================
#'
#' # data: 1 2-level ordered factor
#' x = data.frame(ordered(rbinom(100,1,.5))); names(x) = c("x")
#'
#' tmp = umxThresholdMatrix(x, fullVarNames = "x")
#' # The lower ones matrix (all fixed)
#' tmp[[1]]$values
#' tmp[[1]]$free
#' 
#' # The deviations matrix
#' tmp[[2]]$values
#' tmp[[2]]$labels # note: for twins, labels will be equated across twins
#' 
#' # The algebra that adds the deviations to create thresholds:
#' tmp[[3]]$formula
#' 
#' # Example of a warning to not omit the variable names
#' # tmp = umxThresholdMatrix(x)
#' # Polite message: For coding safety, when calling umxThresholdMatrix, set fullVarNames...
#' 
#' # One ordered factor with 5-levels
#' x = cut(rnorm(100), breaks = c(-Inf,.2,.5, .7, Inf)); levels(x) = 1:5
#' x = data.frame(ordered(x)); names(x) <- c("x")
#' tmp = umxThresholdMatrix(x, fullVarNames = "x")
#' tmp[[2]]$name
#' tmp[[2]]$free # last one is free.. (method = Mehta)
#' 
#' tmp = umxThresholdMatrix(x, fullVarNames = "x", l_u_bound= c(-1,1))
#' tmp[[2]]$lbound # bounds applied to base threshold
#'
#' # =================================
#' # = Binary example with twin data =
#' # =================================
#' # ===============================================================
#' # = Create a series of binary and ordinal columns to work with =
#' # ===============================================================
#' data(twinData)
#' 
#' # Make "obese" variable with ~20% subjects categorised as obese
#' obesityLevels   = c('normal', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Step 2: Make the ordinal variables into umxFactors (ordered, with the levels found in the data)
#' selVars = c("obese1", "obese2")
#' twinData[, selVars] = umxFactor(twinData[, selVars])
#' 
#' # Example 1
#' # use verbose = TRUE to see informative messages
#' tmp = umxThresholdMatrix(twinData, fullVarNames = selVars, sep = "", verbose = TRUE) 
#' 
#' 
#' # ======================================
#' # = Ordinal (n categories > 2) example =
#' # ======================================
#' # Repeat for three-level weight variable
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.4, .7), na.rm = TRUE)
#' twinData$obeseTri1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseTri2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selDVs = "obeseTri"; selVars = tvars(selDVs, sep = "", suffixes = 1:2)
#' twinData[, selVars] = umxFactor(twinData[, selVars])
#' tmp = umxThresholdMatrix(twinData, fullVarNames = selVars, sep = "", verbose = TRUE)
#'
#' 
#' # ========================================================
#' # = Mix of all three kinds example (and a 4-level trait) =
#' # ========================================================
#' obesityLevels = c('underWeight', 'normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.25, .4, .7), na.rm = TRUE)
#' twinData$obeseQuad1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseQuad2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selVars = c("obeseQuad1", "obeseQuad2")
#' twinData[, selVars] = umxFactor(twinData[, selVars])
#'
#' selDVs =c("bmi", "obese", "obeseTri", "obeseQuad")
#' tmp = umxThresholdMatrix(twinData, fullVarNames = tvars(selDVs, sep= ""), sep = "", verbose = TRUE)
#' # The lower ones matrix (all fixed)
#' tmp[[1]]$values
#' # The deviations matrix
#' tmp[[2]]$values
#' tmp[[2]]$labels # note labels are equated across twins
#' # Check to be sure twin-1 column labels same as twin-2
#' tmp[[2]]$labels[,2]==tmp[[2]]$labels[,4]
#' 
#' # The algebra that assembles these into thresholds:
#' tmp[[3]]$formula

#' # =================================
#' # = Example with method = allFree =
#' # =================================
#'
#' tmp = umxThresholdMatrix(twinData, fullVarNames = tvars(selDVs, sep= ""), sep = "", 
#' 	method = "allFree")
#' all(tmp[[2]]$free)
#' 
umxThresholdMatrix <- function(df, fullVarNames = NULL, sep = NULL, method = c("Mehta", "allFree"), threshMatName = "threshMat", l_u_bound = c(NA, NA), droplevels = FALSE, verbose = FALSE, selDVs= "deprecated"){
	# TODO: umxThresholdMatrix: priority A: Move to a more robust way to detect twin than just the sep isn't NULL??
	# TODO: Consider changing from "threshMat" to "Thresholds" to match what mxModel does with mxThresholds internally now...
	method = match.arg(method)
	if(method=="allFree"){
		verbose=FALSE
	}

	if(any(selDVs != "deprecated")){
		message("Polite note: please use fullVarNames instead of selDVs when calling umxThresholdMatrix")
		fullVarNames= selDVs
	}

	if(is.null(fullVarNames)){
		warning("Polite message: For coding safety, when calling umxThresholdMatrix, set fullVarNames to the list of FULL names of all the variables in the model (AND you MUST include sep if this is a twin model!!)")
		fullVarNames = names(df)
		nSib = 1
	} else if(is.null(sep)){
		# no sep: Assume this is not family data
		nSib = 1
	} else {
		# sep provided: Assume this is twin data (already expanded... no way currently to tell if sep was intended to build or decompose vars - see TODO above!!)
		# Set nSib, and break down names into base and suffix if necessary
		msg = paste0("umxThresholdMatrix needs the _FULL_ name of each variable (in addition to the `sep` used to break them down to base names)... 
			you provided: ", omxQuotes(fullVarNames))
		umx_check_names(namesNeeded = fullVarNames, data = df, die = TRUE, message = msg)
		tmp         = umx_explode_twin_names(fullVarNames, sep = sep)
		baseNames   = tmp$baseNames
		twinIndexes = tmp$twinIndexes
		nSib        = length(twinIndexes)
	}
	# Create dataframe with just the requested variables
	df = df[, fullVarNames, drop = FALSE]
	# Check input
	if(dim(df)[1] < 1){ stop("Data input to umxThresholdMatrix had no rows. I use the data to set thresholds, so the data must have rows.") }
	if(droplevels){ stop("Not sure it's wise to drop levels... let me know what you think") }
	
	summaryObj     = umx_is_ordered(df, summaryObject= TRUE)
    isFactor       = summaryObj$isFactor
	isOrd          = summaryObj$isOrd
	isBin          = summaryObj$isBin
	nFactors       = summaryObj$nFactors
	nOrdVars       = summaryObj$nOrdVars
	nBinVars       = summaryObj$nBinVars
	factorVarNames = summaryObj$factorVarNames
	ordVarNames    = summaryObj$ordVarNames
	binVarNames    = summaryObj$binVarNames
	df = df[, factorVarNames, drop = FALSE]
	if(nSib == 2){
		# For precision (placing cuts) and to ensure twins have same levels, copy both halves of the dataframe into each
		T1 = df[, grep(paste0(twinIndexes[1], "$"), factorVarNames, value = TRUE), drop = FALSE]
		T2 = df[, grep(paste0(twinIndexes[2], "$"), factorVarNames, value = TRUE), drop = FALSE]
		names(T2) = names(T1)
		df = cbind(rbind(T1, T2), rbind(T1, T2))
		names(df) = factorVarNames
	} else if(nSib == 1){
		# df is fine as is.		
	} else {
		stop("I can only handle 1 and 2 sib models. The way you called umxThresholdMatrix, I've guessed nSib is ", omxQuotes(nSib), 
			" and separator ", omxQuotes(sep), "\n fullVarNames were: ", omxQuotes(fullVarNames),	". email maintainer('umx') to get this expanded.")
	}
	minLevels = xmuMinLevels(df)
	maxLevels = xmuMaxLevels(df)
	maxThresh = maxLevels - 1

	# ===========================================
	# = Tell the user what we found if they ask =
	# ===========================================
	if((nOrdVars + nBinVars) < 1){
		warning("No ordinal or binary variables in dataframe (or possibly a factor but with only 1 level): no need to call umxThresholdMatrix")
		return(NA) # Probably OK to set thresholds matrix to NA in mxExpectation()
	} else {
		if(verbose){
			theMsg = paste0("object ", omxQuotes(threshMatName), " created to handle")
			if(nSib == 2){
				if(nOrdVars > 0){
					theMsg = paste0(theMsg, ": ", nOrdVars/nSib, " pair(s) of ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					theMsg = paste0(theMsg, ": ", nBinVars/nSib, " pair(s) of binary variables:", omxQuotes(binVarNames), "\n")
				}
			} else {
				if(nOrdVars > 0){
					theMsg = paste0(theMsg, ": ", nOrdVars, " ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					theMsg = paste0(theMsg, ": ", nBinVars, " binary variables:", omxQuotes(binVarNames), "\n")
				}
			}
			message(theMsg)
		}
	}

	# =================================
	# = Create labels and free vector =
	# =================================
	labels = free = c()
	for (thisVarName in factorVarNames) {
		thisCol = df[,thisVarName]
		nThreshThisVar = length(levels(thisCol)) -1
		# TODO maybe make this demand/find basenames?
		if(nSib == 2){
			# Make same label (just baseVarname_thresh) for each twin for each variable
			findStr = paste0(sep, "(", paste(twinIndexes, collapse = "|"), ")$") # e.g. "_T(1|2)$"
			thisLab = sub(findStr, "", thisVarName) # strip sep+0-9 from end of name, e.. remove "_T1"
		} else {
			thisLab = thisVarName
		}
		theseLabels = umx_pad(paste0(thisLab, "_thresh", 1:nThreshThisVar), maxThresh)
		labels = append(labels, theseLabels)
		# ============
		# = Set Free =
		# ============
		theseFree = c(rep(TRUE, nThreshThisVar), rep(FALSE, (maxThresh - nThreshThisVar)))
		free = append(free, theseFree)
	}

	# Create threshMat with size the  order maxThresh rows * nFactors cols
	threshMat = mxMatrix(name = threshMatName, type = "Full",
		nrow     = maxThresh,
		ncol     = nFactors,
		free     = free, 
		values   = rep(NA, (maxThresh * nFactors)),
		labels   = labels,
		# note: these matrix bounds are discarded along with this threshMat now 
		# that thresholds are implemented using deviations
		# Bounds are placed instead on the first deviation threshold 
		lbound   = l_u_bound[1],
		ubound   = l_u_bound[2],
		dimnames = list(paste0("th_", 1:maxThresh), factorVarNames)
	)

	# ====================
	# = talk to the user =
	# ====================
	if(nBinVars > 0){
		if(verbose){
			message(sum(isBin), " trait(s) are binary: ", omxQuotes(binVarNames),
			"\nFor these, you you MUST fix the mean and variance of the latent traits driving each variable (usually 0 & 1 respectively) .\n",
			"See ?mxThresholdMatrix")
		}
	}
	if(nOrdVars > 0){
		if(verbose){
			message(nOrdVars, " variables are ordinal (>2 levels). For these I will use Paras Mehta's 'fix first 2 thresholds' method.\n",
			"It's ESSENTIAL that you leave the means and variances of the latent ordinal traits FREE!\n",
			"See ?mxThresholdMatrix")
		}
	}
	if(minLevels == 1){
		warning("You seem to have a trait with only one category: ", omxQuotes(xmuMinLevels(df, what = "name")), "... makes it a bit futile to model it?")
		stop("Stopping, as I can't handle trait with no variance.")
	}

	# =======================
	# = Estimate thresholds =
	# =======================
	# For each factor variable
	for (thisVarName in factorVarNames) {
		thisCol = df[,thisVarName]
		nThreshThisVar = length(levels(thisCol)) -1 # "0"  "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12"
		
		# ===============================================================
		# = Work out z-values for thresholds based on simple bin counts =
		# ===============================================================
		# Pros: Doesn't assume equal intervals.
		# Problems = empty bins and noise (equal thresholds (illegal) and higher than realistic z-values)
		tab = table(thisCol)/sum(table(thisCol)) # Simple histogram of proportion at each threshold
		cumTab = cumsum(tab)                     # Convert to a cumulative sum (sigmoid from 0 to 1)
		# Use quantiles to get z-equivalent for each level: ditch one to get thresholds...
		zValues = qnorm(p = cumTab, lower.tail = TRUE)
		# take this table as make a simple vector
		zValues = as.numeric(zValues)

		# =======================================================================================
		# = TODO handle where flows over, say, 3.3... squash the top down or let the user know? =
		# =======================================================================================
		if(any(is.infinite(zValues))){
			nPlusInf  = sum(zValues == (Inf))
			nMinusInf = sum(zValues == (-Inf))
			if(nPlusInf){
				maxOK = max(zValues[!is.infinite(zValues)])
				padding = seq(from = (maxOK + .1), by = .1, length.out = nPlusInf)
				zValues[zValues == (Inf)] = padding
			}
			if(nMinusInf){
				minOK = min(zValues[!is.infinite(zValues)])
				padding = seq(from = (minOK - .1), by = (- .1), length.out = nMinusInf)
				zValues[zValues == (-Inf)] = padding
			}
		}
		# =================================
		# = Move duplicates (empty cells) =
		# =================================
		if(any(duplicated(zValues))){
			# message("You have some empty cells")
			# Find where the values change:
			runs         = rle(zValues)
			runLengths   = runs$lengths
			runValues    = runs$values
			distinctCount = length(runValues)
			indexIntoRLE = indexIntoZ = 1
			for (i in runLengths) {
				runLen = i
				if(runLen != 1){
					repeatedValue   = runValues[indexIntoRLE]
					precedingValue = runValues[(indexIntoRLE - 1)]
					minimumStep = .01
					if(indexIntoRLE == distinctCount){
						newValues = seq(from = (precedingValue + minimumStep), by = (minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					} else {
						followedBy = runValues[(indexIntoRLE + 1)]
						minimumStep = min((followedBy - precedingValue)/(runLen + 1), minimumStep)
						newValues = seq(from = (followedBy - minimumStep), by = (-minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					}
				}
				indexIntoZ   = indexIntoZ + runLen
				indexIntoRLE = indexIntoRLE + 1
			}
		}
    	# TODO start from 1, right, not 2?
		values = c(zValues[1:(nThreshThisVar)], rep(.001, (maxThresh - nThreshThisVar)))
		sortValues = sort(zValues[1:(nThreshThisVar)], na.last = TRUE)
		if (!identical(sortValues, zValues[1:(nThreshThisVar)])) {
			umx_msg(values)
			stop("The thresholds for ", thisVarName, " are not in order... oops: that's my fault :-(")
		}
	
		# Already labeled, and all free initialised to TRUE (out of range = FALSE)
		if(nThreshThisVar > 1){ # fix the first 2
			threshMat$free[1:2,   thisVarName] = FALSE
		}	
		threshMat$values[, thisVarName] = values
	} # end for each factor variable

	if(verbose) {
		message("Using deviation-based model: Thresholds will be in", omxQuotes(threshMatName), " based on deviations in ", omxQuotes("deviations_for_thresh"))
	}
	
	# ==========================
	# = Adding deviation model =
	# ==========================
	# Tasks:
	# 1. Convert thresholds into deviations
	#       value 1 for each var = the base, everything else is a deviation
	# 2. Make matrix deviations_for_thresh (similar to existing threshMat), fill values with results from 1
	# 3. Make lower matrix of 1s called "lowerOnes_for_thresh"
	# 4. Create thresholdsAlgebra named threshMatName
	# 5. Return a package of lowerOnes_for_thresh, deviations_for_thresh & thresholdsAlgebra (named threshMatName)

	# =====
	# = 1 =
	# =====
	# startDeviations
	deviationValues = threshMat$values
	nrows = dim(threshMat$values)[1]
	ncols = dim(threshMat$values)[2]
	if (nrows > 1){
		for (col in 1:ncols) {
			# Skip row 1 which is the base
			for (row in 2:nrows) {
				# Convert remaining rows to offsets
				thisValue = threshMat$values[row, col]
				previousValue = threshMat$values[(row-1), col]
				if(!is.na(thisValue)){
					thisOffset = thisValue - previousValue
					if(thisOffset <= 0){
						# tweak to be slightly positive
						thisOffset = .001
					}
					deviationValues[row, col] = thisOffset
				} else {
					# out of range: TODO: simplify by just run to max thresh row
				}
			}
		}
	}

	# threshMat$values
	#          obese1 obeseTri1 obeseQuad1     obese2 obeseTri2 obeseQuad2
	# th_1 -0.4727891 0.2557009 -0.2345662 -0.4727891 0.2557009 -0.2345662
	# th_2         NA 1.0601180  0.2557009         NA 1.0601180  0.2557009
	# th_3         NA        NA  1.0601180         NA        NA  1.0601180
	#
	# threshMat$free
	#      obese1 obeseTri1 obeseQuad1 obese2 obeseTri2 obeseQuad2
	# th_1   TRUE     FALSE      FALSE   TRUE     FALSE      FALSE
	# th_2  FALSE     FALSE      FALSE  FALSE     FALSE      FALSE
	# th_3  FALSE     FALSE       TRUE  FALSE     FALSE       TRUE

	# =====
	# = 2 =
	# =====
	# make a copy of "_thresh" labels, changing to "_dev"
	devLabels = sub("_thresh", "_dev", threshMat$labels, ignore.case = FALSE)
	
	# Create the deviations matrix
	deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full",
		nrow     = maxThresh, ncol = nFactors,
		free     = threshMat$free,
		labels   = devLabels,
		values   = deviationValues,
		lbound   = .001,
		# TODO umxThresholdMatrix: ubound might want to be l_u_bound[2]
		ubound   = NA,
		dimnames = list(paste0("dev_", 1:maxThresh), factorVarNames)
	)

	deviations_for_thresh$lbound[1,] =  l_u_bound[1] # First deviation is special, because it it's the base, not a deviation.
	deviations_for_thresh$ubound[1,] =  l_u_bound[2] # First deviation is special, because it it's the base, not a deviation.
	# 3: Create the lowerOnes matrix
	lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThresh, free = FALSE, values = 1)
	# 4: Create thresholdsAlgebra named threshMatName
	threshDimNames = list(paste0("th_", 1:maxThresh), factorVarNames)
	thresholdsAlgebra = mxAlgebra(name = threshMatName, lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = threshDimNames)

	if(method == "allFree"){
		deviations_for_thresh$free = TRUE
		# !tmpFree[[2]]$free
		# deviations_for_thresh$free[FALSE]=TRUE
		# deviations_for_thresh$free = tmpFree
	}

	return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra))
}
# ===========
# = Utility =
# ===========

# ====================
# = Parallel Helpers =
# ====================

eddie_AddCIbyNumber <- function(model, labelRegex = "") {
	# eddie_AddCIbyNumber(model, labelRegex="[ace][1-9]")
	args     = commandArgs(trailingOnly=TRUE)
	CInumber = as.numeric(args[1]); # get the 1st argument from the cmdline arguments (this is called from a script)
	CIlist   = umxGetParameters(model ,regex= "[ace][0-9]", verbose= FALSE)
	thisCI   = CIlist[CInumber]
	model    = mxModel(model, mxCI(thisCI) )
	return (model)
}

#' Easier (and powerful) specification of paths in SEM.
#'
#' @details This function returns a standard mxPath, but gives new options for specifying the path. In addition to the normal
#' \dQuote{from} and \dQuote{to}, it adds specialised parameters for variances (var), two headed paths (with) and means (mean).
#' There are also new terms to describe fixing values: \dQuote{fixedAt} and \dQuote{fixFirst}.
#' 
#' Finally, (in future) it will allow sem-style \dQuote{A->B} string specification.
#'
#' @description The goal of this function is to enable quick-to-write, quick-to-read, flexible path descriptions for RAM models in OpenMx.
#' 
#' It introduces the following new words to our vocabulary for describing paths: \strong{with}, \strong{var}, \strong{cov}, \strong{means}, \strong{v1m0}, \strong{v0m0,} \strong{v.m0}, \strong{v.m.}, \strong{fixedAt}, \strong{freeAt}, \strong{firstAt}, \strong{unique.bivariate}, \strong{unique.pairs}, \strong{fromEach}, \strong{Cholesky}, \strong{defn}, \strong{forms}.
#'
#' The new preposition \dQuote{with} means you no-longer need set arrows = 2 on covariances. Instead, you can say:
#'
#'    \code{umxPath(A, with = B)} instead of \code{mxPath(from = A, to = B, arrows = 2)}.
#' 
#' Specify a variance for A with
#' 
#' \code{umxPath(var = "A")}.
#' 
#' This is equivalent to \code{mxPath(from = "A", to = "A", arrows = 2)}.
#' 
#' Of course you can use vectors anywhere:
#' 
#' \code{umxPath(var = c('N','E', 'O'))}
#' 
#' To specify a mean, you just say
#' 
#' \code{umxPath(mean = "A")}, which is equivalent to \code{mxPath(from = "one", to = "A")}.
#' 
#' To fix a path at a value, you can say:
#' 
#' \code{umxPath(var = "A", fixedAt = 1)} .
#' 
#' instead of \code{mxPath(from = A, to = A, arrows = 2, free = FALSE, values = 1)} 
#' 
#' The common task of creating a variable with variance fixed at 1 and mean at 0 is done thus:
#' 
#' \code{umxPath(v1m0 = "A")}
#' 
#' For free variance and means use:
#' 
#' \code{umxPath(v.m. = "A")}
#' 
#' umxPath exposes \dQuote{unique.bivariate} and \dQuote{unique.pairs} so you don't have to remember
#' how to fill in connect = in mxPath (you can still use connect if you wish).
#' 
#' So, to create paths creates A<->A, B<->B, and A->B, you would say:
#' 
#' \code{umxPath(unique.pairs = c('A',"B"))} 
#' 
#' To create paths A<->B, B<->C, and A<->C, you would say:
#' \code{umxPath(unique.bivariate = c('A',"B","C"))}
#' 
#' \code{umxPath(fromEach = c('A',"B","C"))} Creates one-headed arrows on the all.bivariate pattern
#'
#'
#' Setting up a latent trait, you can scale with a fixed first path thus:
#' 
#' \code{umxPath("A", to = c("B","C","D"),  firstAt = 1)}  
#' 
#' This is equivalent to \code{mxPath(from = A, to = c(B,C,D), free = c(F, T, T), values = c(1, .5, .4))}.
#' 
#' To create Cholesky-pattern connections:
#' 
#' \code{umxPath(Cholesky = c("A1", "A2"), to c("var1", "var2"))}
#' 
#' 
#' @param from One or more source variables e.g "A" or c("A","B")
#' @param to One or more target variables for one-headed paths, e.g "A" or c("A","B").
#' @param with 2-headed path <--> from 'from' to 'with'.
#' @param var Equivalent to setting 'from' and 'arrows' = 2. nb: from, to, and with must be left empty.
#' @param cov Convenience to allow 2 variables to covary (equivalent to 'from' and 'with'). nb: leave from, to, etc. empty
#' @param means equivalent to "from = 'one', to = x. nb: from, to, with and var must be left empty (their default).
#' @param v1m0 variance of 1 and mean of zero in one call.
#' @param v.m. variance and mean, both free.
#' @param v0m0 variance and mean, both fixed at zero.
#' @param v.m0 variance free, mean fixed at zero.
#' @param fixedAt Equivalent to setting "free = FALSE, values = x" nb: free and values must be left empty (their default)
#' @param freeAt Equivalent to setting "free = TRUE, values = x" nb: free and values must be left empty (their default)
#' @param firstAt first value is fixed at this (values passed to free are ignored: warning if not a single TRUE)
#' @param unique.bivariate equivalent to setting from, and "connect = "unique.bivariate", arrows = 2".
#' nb: from, to, and with must be left empty (their default)
#' @param unique.pairs equivalent to setting "connect = "unique.pairs", arrows = 2" (don't use from, to, or with)
#' @param fromEach Like all.bivariate, but with one head arrows. 'to' can be set.
#' @param forms Build a formative variable. 'from' variables form the latent.
#' Latent variance is fixed at 0. Loading of path 1 is fixed at 1. unique.bivariate between 'from' variables.
#' @param Cholesky Treat \strong{Cholesky} variables as latent and \strong{to} as measured, and connect as in an ACE model.
#' @param defn Implements a definition variable as a latent with zero variance & mean and labeled 'data.defVar'
#' @param connect as in mxPath - nb: from and to must also be set.
#' @param arrows as in mxPath - nb: from and to must also be set.
#' @param free whether the value is free to be optimised
#' @param values default value list
#' @param labels labels for each path
#' @param lbound lower bounds for each path value
#' @param ubound upper bounds for each path value
#' @param hasMeans Used in 'forms' case to know whether the data have means or not.
#' @return - 1 or more [mxPath()]s
#' @export
#' @family Core Model Building Functions
#' @seealso - [mxPath()]
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#'
#' # ==========================================
#' # = Examples of each path type, and option =
#' # ==========================================
#' 
#' umxPath("A", to = "B") # One-headed path from A to B
#' umxPath("A", to = "B", fixedAt = 1) # same, with value fixed @@1
#' umxPath("A", to = c("B", "C"), fixedAt = 1:2) # same, with more than 1 value
#' umxPath("A", to = c("B","C"), firstAt = 1) # Fix only the first path, others free
#' umxPath(var = "A") # Give a variance to A
#' umxPath(var = "A", fixedAt = 1) # Give A variance, fixed at 1
#' umxPath(means = c("A","B")) # Create a means model for A: from = "one", to = "A"
#' umxPath(v1m0 = "A") # Give "A" variance and a mean, fixed at 1 and 0 respectively
#' umxPath(v.m. = "A") # Give "A" variance and a mean, leaving both free.
#' umxPath(v0m0 = "W", label = c(NA, "data.W"))
#' umxPath("A", with = "B") # using with: same as "to = B, arrows = 2"
#' umxPath("A", with = "B", fixedAt = .5) # 2-head path fixed at .5
#' umxPath("A", with = c("B", "C"), firstAt = 1) # first covariance fixed at 1
#' umxPath(cov = c("A", "B"))  # Covariance A <-> B
#' umxPath(defn = "mpg") # create latent called def_mpg, with 0 mean * var, and label = "data.mpg"
#' umxPath(fromEach = c('a','b'), to = c('c','d')) # a->c, a<->d, b<->c, b<->d
#' umxPath(unique.bivariate = c('a','b','c')) # bivariate paths a<->b, a<->c, b<->c etc.
#' umxPath(unique.pairs = letters[1:3]) # all distinct pairs: a<->a, a<->b, a<->c, b<->b, etc.
#' umxPath(Cholesky = c("A1","A2"), to = c("m1", "m2")) # Cholesky
#' 
#' \dontrun{
#' # A worked example
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type= "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' umxSummary(m1, std = TRUE)
#' require(umx)
#'
#' 
#' # ====================
#' # = Cholesky example =
#' # ====================
#' # ======================================================================
#' # = 3-factor Cholesky (A component of a 5-variable 3-factor ACE model) =
#' # ======================================================================
#' latents   = paste0("A", 1:3)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("Chol", data = demoOneFactor, type = "cov",
#' 	umxPath(Cholesky = latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' plot(m1, splines= FALSE)
#'
#' # =========================================================
#' # = Definition variable example.Not much use at present,  =
#' # = as def vars are not readily used in RAM models...     =
#' # = Working on something rational and intuitive.          =
#' # =========================================================
#' data(mtcars)
#' m1 = umxRAM("manifest", data = mtcars,
#'	 umxPath(v.m. = "mpg"),
#'	 umxPath(defn = "mpg")
#' )
#'
#' }
#'
umxPath <- function(from = NULL, to = NULL, with = NULL, var = NULL, cov = NULL, means = NULL, v1m0 = NULL, v.m. = NULL, v0m0 = NULL, v.m0 = NULL, fixedAt = NULL, freeAt = NULL, firstAt = NULL, unique.bivariate = NULL, unique.pairs = NULL, fromEach = NULL, forms = NULL, Cholesky = NULL, defn = NULL, connect = c("single", "all.pairs", "all.bivariate", "unique.pairs", "unique.bivariate"), arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA, hasMeans = NULL) {
	connect = match.arg(connect) # set to single if not overridden by user.
	# xmu_string2path(from)
	n = 0
	for (i in list(with, cov, var, forms, means, fromEach, unique.bivariate, unique.pairs, v.m. , v1m0, v0m0, v.m0, defn, Cholesky)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n > 1){
		stop("At most one of with, cov, var, forms, means, fromEach, unique.bivariate, unique.pairs, v1m0, v.m., v0m0, v.m0, defn, or Cholesky can be set: Use at one time")
	} else if(n == 0){
		# check that from is set?
		if(is.null(from)){
			stop("You must set at least 'from'")
		}	
	}

	n = 0
	for (i in list(v.m. , v1m0, v0m0, v.m0)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n && !is.null(fixedAt)){
		warning("When you use v.m. , v1m0, v0m0, v.m0, don't also set fixedAt - I will ignore it this time")
		fixedAt = NULL
	}

	if(!is.null(defn)){
		if(anyNA(labels)){
			labels = paste0("data.", defn)
			defn   = paste0("def_" , defn)
			message(length(defn), " definition variables created: refer to them/it as: ", omxQuotes(defn))
		} else if(length(labels) != length(defn)){
			stop("Number of labels must match number of definition variables (data source)!\n",
			"You can gave me ", omxQuotes(labels), "labels and ", omxQuotes(defn), " defn vars")
		}else if (length(grep("data\\.", labels, value = FALSE))==0){
			# if user hasn't prepended labels with "data." then add it for them
			labels = paste0("data.", labels)
		}
		a = umxPath(var = defn, fixedAt = 0)
		b = umxPath(means = defn, free = FALSE, labels = labels)
		# var a var@1 mean@0
		return(list(a, b))
	}

	if(!is.null(Cholesky)){
		if(arrows!=1){
			stop("Cholesky paths are one-headed: you set arrows= to something other than 1")
		}
		from  = Cholesky
		nFrom = length(from)
		nTo   = length(to)
		if(!(nTo >= nFrom)){
			stop("Must have at least as many 'to' variables as latents for Cholesky: you gave me ",
			nTo, " to variables and ", nFrom, " Cholesky latents")
		}
		if(!is.na(labels)){
			message("setting labels for Cholesky is tricky: Leave blank to have me do this for you automatically.")
		}
		if(!is.na(lbound)){
			message("setting lbounds for Cholesky is tricky: Leave blank to have me bound the diagonal for you automatically.")
		}else{
			lbound = matrix(NA, nrow = nFrom, ncol = nTo); diag(lbound) = 1e-6
			lbound = lbound[upper.tri(lbound, diag = TRUE)]			
		}
		if(!is.na(ubound)){
			message("nb setting ubound (other than as uniform) is tricky for Cholesky, make sure you're getting what you expected or leave it blank.")
		}
		if(!is.na(values)){
			message("nb setting values is tricky for Cholesky, make sure you're getting what you expected, or leave it blank.")
		}
		labelList = fromList = toList =c()
		n = nTo
		for(i in seq_along(from)) {
			thisFrom  = rep(from[i], n)
			thisTo    = to[i:nTo]
			fromList  = c(fromList, thisFrom)
			toList    = c(toList, thisTo)
			# Needn't bother with this as it will all be taken care of in xmuLabel...
			labelList = c(labelList, paste(thisFrom, thisTo, sep = '_to_'))
			n = (n - 1)
		}
		if(!is.na(labels)){
			labelList = labels
		}
		return(mxPath(from = fromList, to = toList, arrows = 1, free = free, labels = labelList, lbound = lbound, ubound = ubound, values = values))
	}
	if(!is.null(v1m0)){
		# TODO lbound ubound unlikely to be applied to two things, and can't affect result... error if they're not NULL?
		if(!is.na(lbound) && is.na(ubound) && FALSE){
				message("I lbounded var of ", v1m0, " @0")
		}
		if(any(!is.na(labels))){
			if(length(labels)==2){
				a = mxPath(from = v1m0, arrows = 2, free = FALSE, values = 1, labels = labels[1], lbound = 0, ubound = ubound)
				b = mxPath(from = "one", to = v1m0, free = FALSE, values = 0, labels = labels[2], lbound = lbound, ubound = ubound)
			} else {
				stop("Managing which labels apply to the variances and which to the means is error prone:\n",
				"I suggest you call: umxPath(var=) and umxPath(means=) separately"
				)
			}
		} else {
			a = mxPath(from = v1m0, arrows = 2, free = FALSE, values = 1, lbound = 0, ubound = ubound)
			b = mxPath(from = "one", to = v1m0, free = FALSE, values = 0, lbound = lbound, ubound = ubound)
		}
		return(list(a, b))
	}

	if(!is.null(v.m.)){
		# TODO lbound ubound unlikely to be applied to two things. lbound for var should be 0
		if(!is.na(lbound) && is.na(ubound) && FALSE){
			message("I lbounded var of ", v.m. , " @0")
		}
		if(any(!is.na(labels))){
			if(length(labels)==2){
				a = mxPath(from = v.m., arrows = 2, free = TRUE, values = 1, labels = labels[1], lbound = 0, ubound = ubound)
				b = mxPath(from = "one", to = v.m., free = TRUE, values = 0, labels = labels[2], lbound = lbound, ubound = ubound)
			} else {
				stop("Managing which labels apply to the variances and which to the means is error prone:\n",
				"I suggest you call: umxPath(var) and umxPath(means=) separately"
				)
			}
		} else {
			a = mxPath(from = v.m., arrows = 2, free = TRUE, values = 1, lbound = 0, ubound = ubound)
			b = mxPath(from = "one", to = v.m., free = TRUE, values = 0, lbound = lbound, ubound = ubound)
		}
		return(list(a, b))
	}

	if(!is.null(v0m0)){
		if(any(!is.na(labels))){
			if(length(labels)==2){
				a = mxPath(from = v0m0, arrows = 2, free = FALSE, values = 0, labels = labels[1])
				b = mxPath(from = "one", to = v0m0, free = FALSE, values = 0, labels = labels[2])
			} else {
				stop("Managing which labels apply to the variances and which to the means is error prone:\n",
				"I suggest you call: umxPath(var) and umxPath(means=) separately"
				)
			}
		} else {
			a = mxPath(from = v0m0, arrows = 2, free = FALSE, values = 0)
			b = mxPath(from = "one", to = v0m0, free = FALSE, values = 0)
		}
		return(list(a, b))
	}

	if(!is.null(v.m0)){
		# use values, if provided, to start variance
		values = ifelse(is.na(values), 1, values)
		if(any(!is.na(labels))){
			if(length(labels)==2){
				a = mxPath(from = v.m0, arrows = 2, free = TRUE, values = values, labels=labels[1])
				b = mxPath(from = "one", to = v.m0, free = FALSE, values = 0, labels=labels[2])
			} else {
				stop("Managing which labels apply to the variances and which to the means is error prone:\n",
				"I suggest you call: umxPath(var) and umxPath(means=) separately"
				)
			}
		} else {
			a = mxPath(from = v.m0, arrows = 2, free = TRUE, values = values)
			b = mxPath(from = "one", to = v.m0, free = FALSE, values = 0)
		}
		return(list(a, b))
	}

	if(!is.null(forms)){
		# ====================
		# = Handle formative =
		# ====================
		# http://davidakenny.net/cm/mvar.htm

		if(is.null(from)){
			stop("You have to have offer up at least 3 unique 'from' variables to make a formative")
		}
		if(is.null(hasMeans)){
			message("You have to set hasMeans so I know whether to make them for this formative: Assuming TRUE")
			hasMeans = TRUE
		}

		if(length(forms) > 1){
			stop("It's tricky to setup multiple forms variables in 1 line. e-mail if you'd like this to work..")
		} else {
			numPaths  = length(forms)
			free      = rep(TRUE, numPaths)
			free[1]   = FALSE
			values    = rep(NA, numPaths)
			values[1] = 1
		}

		a = mxPath(from = from, connect = "unique.bivariate", arrows = 2)
		b = mxPath(from = from, to = forms, free = free, values = values)
		if(hasMeans){
			c = mxPath(from = forms, arrows = 2, free = FALSE, values = 0)
			d = mxPath(from = "one", to = forms, free = FALSE, values = 0)
			e = mxPath(from = from, arrows = 2, free = TRUE, values = 1, labels = labels, lbound = 0, ubound = ubound)
			f = mxPath(from = "one", to = from, free = TRUE, values = 0, labels = labels, lbound = lbound, ubound = ubound)
			x = list(a, b, c, d, e, f)
		} else {
			c = mxPath(from = forms, arrows = 2, free = FALSE, values = 0)
			e = mxPath(from = from, arrows = 2, free = TRUE, values = 1, labels = labels, lbound = 0, ubound = ubound)
			x = list(a, b, c, e)
		}
		# return(c(a, b))
		return(x)
	}

	if(!is.null(with)){
		# ===============
		# = Handle with =
		# ===============
		if(is.null(from)){
			stop("To use with, you must set 'from = ' also")
		} else {
			from = from
			to   = with
			arrows = 2
			connect = "single"
		}
	} else if(!is.null(cov)){
		# ==============
		# = Handle cov =
		# ==============
		if(!is.null(from) | !is.null(to)){
			stop("To use 'cov = ', 'from' and 'to' should be empty")
		} else if (length(cov) != 2){
			stop("cov must consist of two and only two variables.\n",
			"If you want to covary more variables, use: 'unique.bivariate =' \n",
			"or else use 'from =', 'to=', and 'connect = \"unique.bivariate\"'\n",
			"If you want to set variances for a list of variables, use 'var = c(\"X\")'")
		} else {
			from   = cov[1]
			to     = cov[2]
			arrows = 2
			connect = "single"
		}
	} else if(!is.null(var)){
		# ==============
		# = handle var =
		# ==============
		if(!is.null(from) || !is.null(to)){
			stop("To use 'var = ', 'from' and 'to' should be empty")
		} else {
			from   = var
			to     = var
			arrows = 2
			connect = "single"
			if(is.na(lbound)){
				lbound  = 0
				# message("I lbounded var of ", omxQuotes(var), " @0")
			}
		}
	} else if(!is.null(means)){
		# ================
		# = Handle means =
		# ================
		if(!is.null(from) | !is.null(to)){
			stop("To use means, from and to should be empty.",
			"Just say 'means = c(\"X\",\"Y\").'")
		} else {
			from   = "one"
			to     = means
			arrows = 1
			connect = "single"
		}
	} else if(!is.null(unique.bivariate)){
		# ===========================
		# = Handle unique.bivariate =
		# ===========================
		if(length(unique(unique.bivariate)) < 2){
			stop("You have to have at least 2 unique variables to use unique.bivariate")
		}
		if(!is.null(from)){
			stop("To use unique.bivariate, 'from=' should be empty.\n",
			"Just say 'unique.bivariate = c(\"X\",\"Y\").'\n",
			"or 'unique.bivariate = c(\"X\",\"Y\"), to = \"Z\"")
		} else {
			if(is.null(to)){
				to = NA				
			} else {
				to = to	
			}
			from    = unique.bivariate
			arrows  = 2
			connect = "unique.bivariate"
		}
	} else if(!is.null(fromEach)){
		# ===========================
		# = Handle fromEach =
		# ===========================
		if(!is.null(from)){
			stop("To use fromEach, 'from=' must be empty (perhaps you were trying to set to= but didn't say that?).\n",
			"Just say 'fromEach = c(\"X\",\"Y\").'\n",
			"or 'fromEach = c(\"X\",\"Y\"), to = \"Z\"")
		} else {
			if(is.null(to)){
				to = NA				
			} else {
				to = to	
			}
			from    = fromEach
			arrows  = 1
			connect = "all.bivariate"
		}
	} else if(!is.null(unique.pairs)){
		# ===========================
		# = Handle unique.pairs =
		# ===========================
		if(length(unique(unique.pairs)) < 2){
			stop("You have to have at least 2 unique variables to use unique.pairs")
		}
		if(!is.null(from)){
			stop("To use unique.pairs, 'from=' should be empty.\n",
			"Just say 'unique.pairs = c(\"X\",\"Y\").'\n",
			"or 'unique.pairs = c(\"X\",\"Y\"), to = \"Z\"")
		} else {
			if(is.null(to)){
				to = NA				
			} else {
				to = to	
			}
			from    = unique.pairs
			arrows  = 2
			connect = "unique.pairs"
		}
	} else {
		if(is.null(from) && is.null(to)){
			stop("You don't seem to have requested any paths.\n",
			"see help(umxPath) for all the possibilities")
		} else {
			# assume it is from to
			if(is.null(to)){
				to = NA
			}
			from    = from
			to      = to
			arrows  = arrows
			connect = connect
		}
	}
	# ==============================================
	# = From, to, and connect have now been set... =
	# ==============================================

	# ==============================
	# = Handle fixedAt and firstAt =
	# ==============================
	if(sum(c(is.null(fixedAt), is.null(firstAt), is.null(freeAt))) < 2){
		stop("At most one of fixedAt freeAt and firstAt can be set: You seem to have tried to set more than one.")
	}

	# Handle firstAt
	if(!is.null(firstAt)){
		if(length(from) > 1 && length(to) > 1){
			stop("It's not wise to use firstAt with multiple from sources AND multiple to targets. I'd like to think about this before implementing it..")
		} else {
			numPaths = max(length(from), length(to))
			free    = rep(TRUE, numPaths)
			free[1] = FALSE
			values    = rep(NA, numPaths)
			values[1] = firstAt
		}
	}	
	# Handle fixedAt
	if(!is.null(fixedAt)){
		free = FALSE
		values = fixedAt
	}
	# Handle freeAt
	if(!is.null(freeAt)){
		free = TRUE
		values = freeAt
	}
	mxPath(from = from, to = to, connect = connect, arrows = arrows, free = free, values = values, labels = labels, lbound = lbound, ubound = ubound)
}


#' Functions for Structural Equation Modeling in OpenMx
#'
#' @description
#' `umx` allows you to more easily build, run, modify, and report structural models, 
#' building on the OpenMx package.
#' All core functions are organized into families, so they are easier to find 
#' (see "families" below under \strong{See Also})
#'
#' All the functions have full-featured and well commented examples, some even have *figures*, 
#' so use the help, even if you think it won't help :-)
#' Have a look, for example at [umxRAM()]
#' 
#' Check out NEWS about new features at `news(package = "umx")`
#' 
#' @details
#' Introductory working examples are below. You can run all demos with demo(umx)
#' When I have a vignette, it will be: vignette("umx", package = "umx")
#' 
#' There is a helpful blog at <https://tbates.github.io>
#' 
#' (Only) if you want the bleeding-edge version:
#' 
#' devtools::install_github("tbates/umx")
#' 
#' @docType package
#' @name umx
#' @family Teaching and testing Functions
#' @family Core Model Building Functions
#' @family Model Summary and Comparison
#' @family Summary Functions
#' @family Reporting Functions
#' @family Plotting functions
#' @family Super-easy helpers
#' @family Twin Modeling Functions
#' @family Twin Data functions
#' @family Get and set
#' @family Check or test
#' @family Data Functions
#' @family File Functions
#' @family String Functions
#' @family Miscellaneous Stats Helpers
#' @family Miscellaneous Utility Functions
#' @family datasets
#' @family Advanced Model Building Functions
#' @family zAdvanced Helpers
#' @family xmu internal not for end user
#' @references - <https://github.com/tbates/umx>
#' @md
#' @examples
#' require("umx")
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type="cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G"  , fixedAt= 1)
#' )
#' 
#' # umx added informative labels, created starting values, 
#' # Ran your model (if autoRun is on), and displayed a brief summary
#' # including a comparison if you modified a model...!
#' 
#' # Let's get some journal-ready fit information for standardized parameters
#' 
#' umxSummary(m1, std = TRUE)
#' # You can get the coefficients of an MxModel with coef(), just like for lm etc.
#' coef(m1)
#' 
#' # But with more control using "parameters", for example just the G loadings
#' # above .3, rounded to 2-digits.
#' parameters(m1, thresh="above", b=.3, pattern = "G_to.*", digits = 2)
#'
#' # ==================
#' # = Model updating =
#' # ==================
#' # We'll use umxModify to modify the model...
#' # Can we set the loading of x1 on G to zero? (nope...)
#' m2 = umxModify(m1, "G_to_x1", name = "no_effect_of_g_on_X1", comparison = TRUE)
#'
#' # note1: umxSetParameters can do this with some additional flexibility
#' # note2 "comparison = TRUE" above is the same as calling 
#' # umxCompare, like this
#' umxCompare(m1, m2)
#' 
#' 
#' # ========================
#' # = Confidence intervals =
#' # ========================
#' 
#' # umxSummary() will show these, but you can also use the confint() function
#' confint(m1) # OpenMx's SE-based confidence intervals
#' 
#' 
#' \dontrun{
#' # umxConfint formats everything you need nicely, and allows adding CIs (with parm=)
#' umxConfint(m1, parm = 'all', run = TRUE) # likelihood-based CIs
#' 
#' # And make a Figure and open in browser
#' plot(m1, std = TRUE)
#' 
#' # If you just want the .dot code returned set file = NA
#' plot(m1, std = TRUE, file = NA)
#' }
#'
NULL
