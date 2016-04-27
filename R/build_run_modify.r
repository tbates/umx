# umx_open("~/bin/umx/R/misc_and_utility.r")
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::release("~/bin/umx", check = TRUE)
# devtools::build_win("~/bin/umx")
# devtools::run_examples("~/bin/umx")
# install.packages("curl")
# install.packages("devtools")
# install.packages("digest")
# install.packages("git2r")
# install.packages("httr")
# install.packages("memoise")
# install.packages("openssl")
# install.packages("R6")
# install.packages("Rcpp")
# install.packages("rstudioapi")
# install.packages("withr")
# install.packages("~/Dropbox/shared folders/OpenMx_binaries/OpenMx_2.3.1-239-OSX/3.2/OpenMx_2.3.1-239.tgz")
# ===============================
# = Highlevel models (ACE, GxE) =
# ===============================
.onAttach <- function(libname, pkgname){
	# TODO remove mxCondenseMatrixSlots now that $ get and set are working properly
	options('mxCondenseMatrixSlots'= FALSE)
	options('umx.plot.format' = 'DiagrammeR')
	options("umx_auto_run" = TRUE)
	options("umx_auto_plot" = NA)
	packageStartupMessage("For an overview type '?umx'")
}

#' @importFrom DiagrammeR DiagrammeR
#' @importFrom graphics plot
#' @importFrom MASS mvrnorm
#' @importFrom methods as getSlots is slotNames
#' @importFrom methods setClass
# methods::setClass is called during build not package source code.
# suppress NOTE with a spurious importFrom in the namespace
#' @importFrom numDeriv jacobian
#' @importFrom polycor hetcor
#' @importFrom parallel detectCores
#' @importFrom stats C aggregate as.formula coef complete.cases
#' @importFrom stats confint cor cov cov.wt cov2cor df lm
#' @importFrom stats logLik na.exclude na.omit pchisq pf qchisq
#' @importFrom stats qnorm quantile residuals rnorm runif sd
#' @importFrom stats setNames update var delete.response terms
#' @importFrom utils combn data flush.console read.table txtProgressBar
#' @importFrom utils globalVariables write.table
NULL
	
utils::globalVariables(c(
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

	# from umxACEcov
	'varStarts',
	'lowerB',
	'lowerW',
	'CovB',
	'CovW',
	'CovWB',
	'tb_CovWB_b',
	'tb_CovB_b',
	'tb_CovWB',
	'tb_CovB',
	'CovWB_b',
	'CovB_b',

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
	
	'correlatedA', 'minCor', 'pos1by6', 'varList', 'Im1', 'IZ', 'ZI', 'Z'
	)
)

# ===================================================================
# = Define some class containers to allow specialised model objects =
# = plot, etc can then operate on these                             =
# ===================================================================
methods::setClass("MxModel.ACE"   , contains = "MxModel")
methods::setClass("MxModel.GxE"   , contains = "MxModel")
methods::setClass("MxModel.CP"    , contains = "MxModel")
methods::setClass("MxModel.IP"    , contains = "MxModel")
methods::setClass("MxModel.ACEcov", contains = "MxModel.ACE")

#' umxRAM
#'
#' Making it as simple as possible to create a RAM model, without doing invisible things to the user.
#' 
#' @details Like \code{\link{mxModel}}, you list the theoretical causal paths. Unlike mxModel:
#' \enumerate{
#' \item{type defaults to "RAM"}
#' \item{You don't need to list manifestVars (they are detected from path usage)}
#' \item{You don't need to list latentVars (detected as anything in paths but not in \code{mxData})}
#' \item{You add data like you do in \code{\link{lm}}, with \strong{data = }}
#' \item{with \code{\link{umxPath}} you can use powerful verbs like \strong{var = }}
#' }
#'
#' \strong{Comparison with other software}
#' 
#' Some software has massive behind-the-scenes defaulting and path addition. I've played with 
#' similar features (like auto-creating error and exogenous variances using \code{endog.variances = TRUE}
#' and \code{exog.variances = TRUE}). Also identification helpers like \code{fix = "latents"} 
#' and \code{fix = "firstLoadings"}
#' 
#' To be honest, these are not only more trouble than they are worth, they encourage errors and 
#' poor modelling. I suggest user learn the handful of \code{\link{umxPath}}
#' short cuts and stay clean and explicit!
#' 
#' @param model A model to update (or set to string to use as name for new model)
#' @param data data for the model. Can be an \code{\link{mxData}} or a data.frame
#' @param ... mx or umxPaths, mxThreshold objects, etc.
#' @param setValues Whether to generate likely good start values (Defaults to TRUE)
#' @param name A friendly name for the model
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param independent Whether the model is independent (default = NA)
#' @param remove_unused_manifests Whether to remove variables in the data to which no path makes reference (defaults to TRUE)
#' @param showEstimates Whether to show estimates. Defaults to no (alternatives = "raw", "std", etc.)
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Model Building Functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' # umxRAM is like lm, ggplot2 etc: you give the data in a data = parameter
#' # A common error is to include data in the main list,
#' # a bit like saying lm(y~x + df) instead of lm(y~x, data=dd)...
#' # nb: unlike mxModel, umxRAM needs data at build time.
#' 
#' # 1. For convenience, list up the manifests you will be using
#' selVars = c("mpg", "wt", "disp")
#' 
#' # 2. Create an mxData object
#' myCov = mxData(cov(mtcars[,selVars]), type = "cov", numObs = nrow(mtcars) )
#' 
#' # 3. Create the model (see ?umxPath for more nifty options)
#' m1 = umxRAM("tim", data = myCov,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath(cov = c("wt", "disp")),
#' 	umxPath(var = c("wt", "disp", "mpg"))
#' )
#' 
#' # 4. Print a nice summary 
#' umxSummary(m1, show = "std")
#' 
#' \dontrun{
#' # 5. Draw a nice path diagram (uses DiagrameR package or Graphviz app)
#' plot(m1)
#' plot(m1, resid = "line")
#' 
#' # =================================================
#' # = This is an example of using your own labels:  =
#' #   umxRAM will not over-ride them                =
#' # =================================================
#' m1 = umxRAM("tim", data = myCov,
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
#' }
umxRAM <- function(model = NA, ..., data = NULL, name = NA, comparison = TRUE, setValues = TRUE, independent = NA, remove_unused_manifests = TRUE, showEstimates = c("none", "raw", "std", "both", "list of column names"), autoRun = getOption("umx_auto_run")) {
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	showEstimates = umx_default_option(showEstimates, c("none", "raw", "std", "both", "list of column names"), check = FALSE)
	if(typeof(model) == "character"){
		if(is.na(name)){
			name = model
		} else {
			stop("Don't set model to a string && pass in name as a string as well...")
		}
	} else {
		if(umx_is_RAM(model)){
			message("Updating existing model")
			if(is.na(name)){
				name = model$name
			}
			newModel = mxModel(model, dot.items, name = name)
			# if(setValues){
			# 	newModel = umxValues(newModel)
			# }
			if(autoRun){
				newModel = mxRun(newModel)
				umxSummary(newModel)
				if(comparison){
					if(length(coef(model)) > length(coef(newModel))){
						newMore = FALSE
					} else {
						newMore = TRUE
					}
					if(newMore){
						umxCompare(newModel, model)
					} else {
						umxCompare(model, newModel)
					}
				}
			}			
			return(newModel)
		} else {
			stop("First item must be either an existing model or a name string. You gave me a ", typeof(model))
		}
	}
	if(!length(dot.items) > 0){
		# do we care?
	}

	if(is.null(data)){
		stop("umxRAM needs some mxData. You set this like in lm(), with data = mxData().\nDid you perhaps just add the mxData along with the paths?")
	}

	nPaths       = 0 # initialise
	foundNames   = c()
	manifestVars = NULL
	for (i in dot.items) {
		thisIs = class(i)[1]
		if(thisIs == "MxPath"){
			foundNames = append(foundNames, c(i$from, i$to))
		} else {
			if(thisIs == "MxThreshold"){
				# MxThreshold detected
			} else {
				# stop("I can only handle mxPaths, mxConstraints, and mxThreshold() objects.\n",
				# "You have given me a", class(i)[1],"\n",
				# " To include data in umxRAM, say 'data = yourData'")
			}
		}
	}

	# ========================
	# = All items processed  =
	# ========================
	# ===============
	# = Handle data =
	# ===============
	if(is.null(data)){
		stop("You must include data: either data = dataframe or data = mxData(yourData, type = 'raw|cov)', ...)")
	} else if(class(data)[1] == "data.frame") {
		data = mxData(observed = data, type = "raw")
	}

    if(class(data)[1] %in%  c("MxNonNullData", "MxDataStatic") ) {
		if(data$type == "raw"){
			manifestVars = names(data$observed)
			isRaw = TRUE
		} else {
			manifestVars = colnames(data$observed)
			isRaw = FALSE
		}
		if(is.null(manifestVars)){
			stop("There's something wrong with the mxData - I couldn't get the variable names from it. Did you set type correctly?")
		}
	} else {
		stop("There's something wrong with the data - I expected a dataframe or mxData, but you gave me a ", class(data)[1])		
	}

	foundNames = unique(na.omit(foundNames))

	# Anything not in data -> latent
	latentVars = setdiff(foundNames, c(manifestVars, "one"))
	nLatent = length(latentVars)
	# Report on which latents were created
	if(nLatent == 0){
		# message("No latent variables were created.\n")
		latentVars = NA
	} else if (nLatent == 1){
		message("A latent variable '", latentVars[1], "' was created.\n")
	} else {
		message(nLatent, " latent variables were created:", paste(latentVars, collapse = ", "), ".\n")
	}
	# TODO handle when the user adds mxThreshold object: this will be a model where things are not in the data and are not latent...
	# ====================
	# = Handle Manifests =
	# ====================
	unusedManifests = setdiff(manifestVars, foundNames)
	if(length(unusedManifests) > 0){
		if(length(unusedManifests) > 10){
			varList = paste0("The first 10 were: ", paste(unusedManifests[1:10], collapse = ", "), "\n")
		} else {
			varList = paste0("They were: ", paste(unusedManifests, collapse = ", "), "\n")
		}
		message(length(unusedManifests), " variables in the dataset which were not referenced in any path\n",varList)
		if(remove_unused_manifests){
			# trim down the data to include only the used manifests
			manifestVars = setdiff(manifestVars, unusedManifests)
			if(data$type == "raw"){
				data$observed = data$observed[, manifestVars]
			} else {
				data$observed = umx_reorder(data$observed, manifestVars)
			}
			message("These were dropped from the analysis")
		} else {
			message("I left them in the data. To remove them automatically, next time set remove_unused_manifests = TRUE")
		}		
	}
	message("ManifestVars set to:\n", paste(manifestVars, collapse = ", "), "\n")

	m1 = do.call("mxModel", list(name = name, type = "RAM", 
		manifestVars = manifestVars,
		latentVars  = latentVars,
		independent = independent,
		data, dot.items)
	)
	if(isRaw){
		if(is.null(m1$matrices$M) ){
			message("You have raw data, but no means model. I added\n",
			"mxPath('one', to = manifestVars)")
			m1 = mxModel(m1, mxPath("one", manifestVars))
		} else {
			# leave the user's means as the model
			# print("using your means model")
			# umx_show(m1)
			# print(m1$matrices$M$values)
		}
	}
	m1 = umxLabel(m1)
	if(setValues){
		m1 = umxValues(m1, onlyTouchZeros = TRUE)
	}
	if(autoRun){
		m1 = mxRun(m1)
		umxSummary(m1, showEstimates = showEstimates)
		return(m1)
	} else {
		return(m1)
	}
}

#' umxGxE
#'
#' Make a 2-group GxE (moderated ACE) model (Purcell, 2002). GxE interaction studies test the hypothesis that the strength
#' of genetic (or environmental) influence varies parametrically (usuaally linear effects on path estimates)
#' across levels of environment. umxGxE allows detecting,
#' testing, and visualizing  G xE (or C or E x E) interaction forms.
#' 
#' The following figure the GxE model as a path diagram:
#' \figure{GxE.png}
#'
#' @param name The name of the model (defaults to "G_by_E")
#' @param selDVs The dependent variable (e.g. IQ)
#' @param selDefs The definition variable (e.g. socio economic status)
#' @param suffix Expand variable base names, i.e., "_T" makes var -> var_T1 and var_T2
#' @param dzData The DZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param mzData The MZ dataframe containing the Twin 1 and Twin 2 DV and moderator (4 columns)
#' @param lboundACE = numeric: If !is.na, then lbound the main effects at this value (default = NA)
#' @param lboundM   = numeric: If !is.na, then lbound the moderators at this value (default = NA)
#' @param dropMissingDef Whether to automatically drop missing def var rows for the user (gives a warning) default = FALSE
#' @param autoRun Whether to run the model, and return that (default), or just to create it and return without running.
#' @return - GxE \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()} and \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - Purcell, S. (2002). Variance components models for gene-environment interaction in twin analysis. \emph{Twin Research}, \strong{6}, 554-571. Retrieved from http://www.ncbi.nlm.nih.gov/entrez/query.fcgi?cmd=Retrieve&db=PubMed&dopt=Citation&list_uids=12573187
#' @examples
#' require(umx)
#' data(twinData) 
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi1", "bmi2")
#' selDefs = c("age1", "age2")
#' selVars = c(selDVs, selDefs)
#' mzData  = subset(twinData, zyg == 1, selVars)[1:80,]
#' dzData  = subset(twinData, zyg == 3, selVars)[1:80,]
#' m1 = umxGxE(selDVs = selDVs, selDefs = selDefs, 
#' 	dzData = dzData, mzData = mzData, dropMissing = TRUE)
#' # Plot Moderation
#' umxSummaryGxE(m1)
#' umxSummary(m1, location = "topright")
#' umxSummary(m1, separateGraphs = FALSE)
#' m2 = umxModify(m1, "am_.*", regex=TRUE, comparison = TRUE)
umxGxE <- function(name = "G_by_E", selDVs, selDefs, dzData, mzData, suffix = NULL, lboundACE = NA, lboundM = NA, dropMissingDef = FALSE, autoRun = getOption("umx_auto_run")) {
	nSib = 2;
	if(!is.null(suffix)){
		if(length(suffix) > 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs  = umx_paste_names(selDVs , suffix, 1:2)
		selDefs = umx_paste_names(selDefs, suffix, 1:2)
	}
	if(any(selDefs %in% selDVs)) {
		warning("selDefs was found in selDVs: You probably gave me all the vars in SelDVs instead of just the DEPENDENT variable");
	}
	if(length(selDVs)/nSib!=1){
		stop("DV list must be 1 variable (2 twins)... You tried ", length(selDVs)/nSib)
	}
	if(length(selDefs) != 2){
		warning("selDefs must be length = 2");
	}
	if(length(selDVs) != 2){
		warning("selDVs must be length = 2");
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
			# Matrices a, c, and e to store a, c, and e path coefficients
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[1], name = "a" ), jiggle = .0001),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[2], name = "c" ), jiggle = .0001),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[3], name = "e" ), jiggle = .0001),
			# Matrices to store moderated path coefficients                       
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "am" )),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "cm" )),
			umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "em" )),

			# Matrices A, C, and E compute non-moderated variance components 
			mxAlgebra(name = "A", a %*% t(a) ),
			mxAlgebra(name = "C", c %*% t(c) ),
			mxAlgebra(name = "E", e %*% t(e) ),
			# Algebra to compute total variances and inverse of standard deviations (diagonal only)
			mxAlgebra(name = "V", A + C + E),
			mxMatrix(name  = "I", "Iden", nrow = nVar, ncol = nVar),
			mxAlgebra(name = "iSD", solve(sqrt(I * V)) ),

			# Matrix & Algebra for expected means vector (non-moderated)
			mxMatrix(name = "Means", "Full", nrow = 1, ncol = nVar, free = TRUE, values = obsMean, labels = "mean"), # needs mods for multivariate!
			# Matrices for betas
			mxMatrix(name = "betaLin" , "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "lin11"), 
			mxMatrix(name = "betaQuad", "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "quad11")

			# TODO:	add covariates to G x E model
			# if(0){
				# TODO: if there are covs
				# mxMatrix(name = "betas" , "Full", nrow = nCov, ncol = nVar, free = T, values = 0.05, labels = paste0("beta_", covariates))
			# }
		),
		mxModel("MZ",
			# matrices for covariates (just on the means)
			# Matrix for moderating/interacting variable
			mxMatrix(name="Def1", "Full", nrow=1, ncol=1, free=F, labels = paste0("data.", selDefs[1])), # twin1 c("data.age1")
			mxMatrix(name="Def2", "Full", nrow=1, ncol=1, free=F, labels = paste0("data.", selDefs[2])), # twin2 c("data.age2")
			# Algebra for expected mean vector
			# TODO simplyfy this algebra... one for both twins and all def vars... not 4* cov...
			mxAlgebra(top.betaLin %*% Def1  , name = "Def1Rlin"),
			mxAlgebra(top.betaQuad%*% Def1^2, name = "Def1Rquad"),
			mxAlgebra(top.betaLin %*% Def2  , name = "Def2Rlin"),
			mxAlgebra(top.betaQuad%*% Def2^2, name = "Def2Rquad"),
			# if(0){ # TODO if there are covs
			# 	mxMatrix(name = "covsT1", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT1)),
			# 	mxMatrix(name = "covsT2", "Full", nrow = 1, ncol = nCov, free = FALSE, labels = paste0("data.", covsT2)),
			# 	mxAlgebra(top.betas %*% covsT1, name = "predMeanT1"),
			# 	mxAlgebra(top.betas %*% covsT2, name = "predMeanT2"),
			# 	mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad + predMeanT1,
			# 	                 top.Means + Def2Rlin + Def2Rquad + predMeanT2), name = "expMeans")
			# } else {
				# mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeans")
			# },
			mxAlgebra( cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeanMZ"),
			
			# Compute ACE variance components
			mxAlgebra((top.a + top.am %*% Def1) %*% t(top.a+ top.am %*% Def1), name = "A11"),
			mxAlgebra((top.c + top.cm %*% Def1) %*% t(top.c+ top.cm %*% Def1), name = "C11"),
			mxAlgebra((top.e + top.em %*% Def1) %*% t(top.e+ top.em %*% Def1), name = "E11"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def1) %*% t(top.a+ top.am %*% Def2), name = "A12"),
			mxAlgebra((top.c + top.cm %*% Def1) %*% t(top.c+ top.cm %*% Def2), name = "C12"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def2) %*% t(top.a+ top.am %*% Def1), name = "A21"),
			mxAlgebra((top.c + top.cm %*% Def2) %*% t(top.c+ top.cm %*% Def1), name = "C21"),
                                                                    
			mxAlgebra((top.a + top.am %*% Def2) %*% t(top.a+ top.am %*% Def2), name = "A22"),
			mxAlgebra((top.c + top.cm %*% Def2) %*% t(top.c+ top.cm %*% Def2), name = "C22"),
			mxAlgebra((top.e + top.em %*% Def2) %*% t(top.e+ top.em %*% Def2), name = "E22"),

			# Algebra for expected variance/covariance matrix and expected mean vector in MZ
			mxAlgebra(rbind(cbind(A11+C11+E11, A12+C12),
			                cbind(A21+C21    , A22+C22+E22) ), name = "expCovMZ"),
			# Data & Objective
			mxData(mzData, type = "raw"),
			mxExpectationNormal("expCovMZ", means = "expMeanMZ", dimnames = selDVs),
			mxFitFunctionML()
		),
	    mxModel("DZ",
			mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[1],sep=""), name="Def1"), # twin1  c("data.divorce1")
			mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[2],sep=""), name="Def2"), # twin2  c("data.divorce2")
			# Compute ACE variance components
			mxAlgebra((top.a+ top.am%*% Def1) %*% t(top.a+ top.am%*% Def1), name="A11"),
			mxAlgebra((top.c+ top.cm%*% Def1) %*% t(top.c+ top.cm%*% Def1), name="C11"),
			mxAlgebra((top.e+ top.em%*% Def1) %*% t(top.e+ top.em%*% Def1), name="E11"),

			mxAlgebra((top.a+ top.am%*% Def1) %*% t(top.a+ top.am%*% Def2), name="A12"),
			mxAlgebra((top.c+ top.cm%*% Def1) %*% t(top.c+ top.cm%*% Def2), name="C12"),

			mxAlgebra((top.a+ top.am%*% Def2) %*% t(top.a+ top.am%*% Def1), name="A21"),
			mxAlgebra((top.c+ top.cm%*% Def2) %*% t(top.c+ top.cm%*% Def1), name="C21"),

			mxAlgebra((top.a+ top.am%*% Def2) %*% t(top.a+ top.am%*% Def2), name="A22"),
			mxAlgebra((top.c+ top.cm%*% Def2) %*% t(top.c+ top.cm%*% Def2), name="C22"),
			mxAlgebra((top.e+ top.em%*% Def2) %*% t(top.e+ top.em%*% Def2), name="E22"),

			# Expected DZ variance/covariance matrix
			mxAlgebra(rbind(cbind(A11+C11+E11  , 0.5%x%A12+C12),
			                cbind(0.5%x%A21+C21, A22+C22+E22) ), name="expCovDZ"),
			# mxAlgebra(rbind(cbind(A11+C11+E11  , 0.5%x%A21+C21),
			#                 cbind(0.5%x%A12+C12, A22+C22+E22) ), name="expCov"),
			# Algebra for expected mean vector
			mxAlgebra(top.betaLin %*% Def1  , name = "Def1Rlin"),
			mxAlgebra(top.betaQuad%*% Def1^2, name = "Def1Rquad"),
			mxAlgebra(top.betaLin %*% Def2  , name = "Def2Rlin"),
			mxAlgebra(top.betaQuad%*% Def2^2, name = "Def2Rquad"),
			mxAlgebra(cbind(top.Means + Def1Rlin + Def1Rquad, top.Means + Def2Rlin + Def2Rquad), name = "expMeanDZ"),
			# mxAlgebra(top.betas%*%rbind(Def1, Def1^2), name="Def1R"),
			# mxAlgebra(top.betas%*%rbind(Def2, Def2^2), name="Def2R"),
			# mxAlgebra( cbind(top.Means+Def1R, top.Means+Def2R), name="expMeans"),
			# Data & Objective
	        mxData(dzData, type = "raw"),
			mxExpectationNormal("expCovDZ", means = "expMeanDZ", dimnames = selDVs),
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
		return(mxRun(model))
	} else {
		return(model)
	}
}

#' umxGxE_window
#'
#' Make a 2-group GxE (moderated ACE) model using LOSEM. In GxE interaction studies, typically,
#' the hypothesis that the strength of genetic influence varies parametrically (usuaally linear effects
#' on path estimates) across levels of environment. Of course, the function linking genetic influence
#' and context is not necessarily linear, but may react more steeply at the extremes, or take other, unknown forms.
#' To avoid obscuring the underlying shape of the interaction effect, local structural equation
#' modeling (LOSEM) may be used, and GxE_window implements this. LOSEM is a non-parametric,
#' estimating latent interaction effects across the range of a measured moderator using a
#' windowing function which is walked along the context dimension, and which weights subjects
#' near the centrre of the window highly relative to subjects far above or below the window centre.
#' This allows detecting and visualizing arbitrary GxE (or CxE or ExE) interaction forms.
#' 
#' @param selDVs The dependent variables for T1 and T2, e.g. c("bmi_T1", "bmi_T2")
#' @param moderator The name of the moderator variable in the dataset e.g. "age", "SES" etc.
#' @param mzData Dataframe containing the DV and moderator for MZ twins
#' @param dzData Dataframe containing the DV and moderator for DZ twins
#' @param weightCov Whether to use cov.wt matrices or FIML default = FALSE, i.e., FIML
#' @param width An option to widen or narrow the window from its default (of 1)
#' @param target A user-selected list of moderator values to test (default = NULL = explore the full range)
#' @param plotWindow whether to plot what the window looks like
#' @param return  whether to return the last model (useful for specifiedTargets) or the list of estimates (default = "estimates")
#' @return - Table of estimates of ACE along the moderator
#' @export
#' @examples
#' library(umx);
#' # ==============================
#' # = 1. Open and clean the data =
#' # ==============================
#' # umxGxE_window takes a dataframe consisting of a moderator and two DV columns: one for each twin
#' mod = "age" # The name of the moderator column in the dataset
#' selDVs = c("bmi1", "bmi2") # The DV for twin 1 and twin 2
#' data(twinData) # Dataset of Australian twins, built into OpenMx
#' # The twinData consist of two cohorts. First we label them
#' # TODO: Q for OpenMx team: can I add a cohort column to this dataset?
#' twinData$cohort = 1; twinData$cohort[twinData$zyg %in% 6:10] = 2
#' twinData$zyg[twinData$cohort == 2] = twinData$zyg[twinData$cohort == 2]-5
#' # And set a plain-English label
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # The model also assumes two groups: MZ and DZ. Moderator can't be missing
#' # Delete missing moderator rows
#' twinData = twinData[!is.na(twinData[mod]),]
#' mzData = subset(twinData, ZYG == "MZFF", c(selDVs, mod))
#' dzData = subset(twinData, ZYG == "DZFF", c(selDVs, mod))
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
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData);
#' 
#' # Run creating weighted covariance matrices (excludes missing data)
#' umxGxE_window(selDVs = selDVs, moderator = mod, mzData = mzData, dzData = dzData, 
#' 		weightCov = TRUE); 
#' }
#' 
#' @family Twin Modeling Functions
#' @references - Hildebrandt, A., Wilhelm, O, & Robitzsch, A. (2009)
#' Complementary and competing factor analytic approaches for the investigation 
#' of measurement invariance. \emph{Review of Psychology}, \bold{16}, 87--107. 
#' 
#' Briley, D.A., Harden, K.P., Bates, T.C.,  Tucker-Drob, E.M. (2015).
#' Nonparametric Estimates of Gene x Environment Interaction Using Local Structural Equation Modeling.
#' \emph{Behavior Genetics}.
umxGxE_window <- function(selDVs = NULL, moderator = NULL, mzData = mzData, dzData = dzData, weightCov = FALSE, target = NULL, width = 1, plotWindow = FALSE, return = c("estimates","last_model")) {
	# TODO want to allow missing moderator?
	# Check moderator is set and exists in mzData and dzData
	return = match.arg(return)
	if(is.null(moderator)){
		stop("Moderator must be set to the name of the moderator column, e.g, moderator = \"birth_year\"")
	}
	# Check DVs exists in mzData and dzData (and nothing else apart from the moderator)
	umx_check_names(c(selDVs, moderator), data = mzData, die = TRUE, no_others = TRUE)
	umx_check_names(c(selDVs, moderator), data = dzData, die = TRUE, no_others = TRUE)

	# Add a zygosity column (that way we know what it's called)
	mzData$ZYG = "MZ";
	dzData$ZYG = "DZ"
	# If using cov.wt, remove missings
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
	# bind the MZ nd DZ data into one frame so we can work with it repeatedly over weight iterations
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

	ACE = c("A", "C", "E")
	tmp = rep(NA, length(targetLevels))
	out = data.frame(modLevel = targetLevels, Astd = tmp, Cstd = tmp, Estd = tmp, A = tmp, C = tmp, E = tmp)
	n   = 1
	for (i in targetLevels) {
		# i = targetLevels[1]
		message("mod = ", i)
		zx = (modVar - i)/bw
		k = (1 / (2 * pi)^.5) * exp((-(zx)^2) / 2)
		# ===========================================================
		# = Insert the weights variable into dataframes as "weight" =
		# ===========================================================
		allData$weight = k/.399
		mzData = allData[allData$ZYG == "MZ", c(selDVs, "weight")]
		dzData = allData[allData$ZYG == "DZ", c(selDVs, "weight")]
		if(weightCov){
			mz.wt = cov.wt(mzData[, selDVs], mzData$weight)
			dz.wt = cov.wt(dzData[, selDVs], dzData$weight)
			m1 = umxACE(selDVs = selDVs, dzData = dz.wt$cov, mzData = mz.wt$cov, numObsDZ = dz.wt$n.obs, numObsMZ = mz.wt$n.obs)
		} else {
			m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, weightVar = "weight")
		}
		m1  = mxRun(m1); 
		if(plotWindow){
			plot(allData[,moderator], allData$weight) # normal-curve yumminess
			umxSummaryACE(m1)
		}
		out[n, ] = mxEval(c(i, top.a_std[1,1], top.c_std[1,1],top.e_std[1,1], top.a[1,1], top.c[1,1], top.e[1,1]), m1)
		n = n + 1
	}
	# Squaring paths to produce variances
	out[,ACE] <- out[,ACE]^2
	# plotting variance components
	with(out,{
		plot(A ~ modLevel, main = paste0(selDVs[1], " variance"), ylab = "Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
		lines(modLevel, C, col = 'green')
		lines(modLevel, E, col = 'blue')
		legend('topright', fill = c('red', 'green', 'blue'), legend = ACE, bty = 'n', cex = .8)

		plot(Astd ~ modLevel, main = paste0(selDVs[1], "std variance"), ylab = "Std Variance", xlab=moderator, las = 1, bty = 'l', type = 'l', col = 'red', ylim = c(0, 1), data = out)
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

#' umxACE
#'
#' Make a 2-group ACE Cholesky Twin model (see Details below)
#' 
#' A common task in twin modelling involves using the genetic and environmental differences 
#' between large numbers of pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of one, or, typically, several phenotypes
#' (measured behaviors).
#' 
#' umxACE supports a core model in behavior genetics, known as the ACE Cholesky model
#' (Cardon and Neale, 1996). This model decomposes phenotypic variance into Additive genetic, 
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D). This latter restriction emerges due to a lack of degrees of 
#' freedom to simultaneously model C and D with only MZ and DZ twin pairs {ref?}. The Cholesky or 
#' lower-triangle decomposition allows a model which is both sure to be solvable, and also to 
#' account for all the variance (with some restrictions) in the data. This model creates as 
#' many latent A C and E variables as there are phenotypes, and, moving from left to 
#' right, decomposes the variance in each component into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram:
#' 
#' \figure{ACE.png}
#' 
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modelling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelyhoods summed to form
#' the model-likelihood, which is to be minimised.
#' This feature is used in the non-linear GxE model functions.
#' 
#' \strong{Additional features}
#' The umxACE function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modelling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' @param name The name of the model (defaults to"ACE")
#' @param selDVs The variables to include from the data
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param suffix The suffix for twin 1 and twin 2, often "_T". if set, simplifies
#' SelDVs, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE)
#' @param numObsDZ = Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = Number of MZ twins: Set this if you input covariance data
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices
#' @param weightVar = If provided, a vector objective will be used to weight the data. (default = NULL) 
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE)
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE)
#' @param hint An analysis hint. Options include "none", (default) "left_censored". Default does nothing.
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @return - \code{\link{mxModel}} of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' # BMI ?twinData set from Australian twins.
#' data(twinData)
#' # Pick the variables
#' selDVs = c("bmi1", "bmi2")
#' selDVs = c("bmi1", "bmi2")
#' mzData <- twinData[twinData$zyg == 1, selDVs][1:80,] # 80 pairs for speed
#' dzData <- twinData[twinData$zyg == 3, selDVs][1:80,]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' umxSummary(m1, showStd = TRUE)
#' plot(m1)
#' # ADE model (DZ correlation set to .25)
#' m2 = umxACE("ADE", selDVs = selDVs, dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, compare = m1) # nb: though this is ADE, columns are labeled ACE
#' 
#' 
#' # ===================
#' # = Ordinal example =
#' # ===================
#' require(umx)
#' data(twinData)
#' # Cut bmi colum to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' selDVs = c("obese")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- twinData[twinData$zyg == 1, umx_paste_names(selDVs, "", 1:2)]
#' dzData <- twinData[twinData$zyg == 3, umx_paste_names(selDVs, "", 1:2)]
#' mzData <- mzData[1:80,] # just top 80 pairs to run fast
#' dzData <- dzData[1:80,]
#' str(mzData)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' umxSummary(m1)
#' 
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' selDVs = c("wt", "obese")
#' # Cut bmi column to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- twinData[twinData$zyg == 1, umx_paste_names(selDVs, "", 1:2)]
#' dzData <- twinData[twinData$zyg == 3, umx_paste_names(selDVs, "", 1:2)]
#' mzData <- mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData <- dzData[1:80,]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' umxSummary(m1)
#' 
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' cutPoints <- quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' selDVs = c("wt", "obese")
#' mzData <- twinData[twinData$zyg == 1, umx_paste_names(selDVs, "", 1:2)]
#' dzData <- twinData[twinData$zyg == 3, umx_paste_names(selDVs, "", 1:2)]
#' mzData <- mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData <- dzData[1:80,]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = '')
#' umxSummary(m1)
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zyg == 1, selDVs], use = "complete")
#' dz = cov(twinData[twinData$zyg == 3, selDVs], use = "complete")
#' m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=nrow(dzData), numObsMZ=nrow(mzData))
#' umxSummary(m1)
#' plot(m1)
umxACE <- function(name = "ACE", selDVs, dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = NULL, 
	weightVar = NULL, equateMeans = TRUE, bVector = FALSE, hint = c("none", "left_censored"), autoRun = getOption("umx_auto_run")) {
	if(nrow(dzData) == 0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData) == 0){ stop("Your MZ dataset has no rows!") }
	hint = match.arg(hint)
	nSib = 2 # number of siblings in a twin pair
	if(dzCr == .25 && name == "ACE"){
		name = "ADE"
	}
	# look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(!is.null(suffix)){
		if(length(suffix) > 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
		}
		selDVs = umx_paste_names(selDVs, suffix, 1:2)
	}
	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	# message("selDVs: ", omxQuotes(selDVs))
	nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	dataType = umx_is_cov(dzData, boolean = FALSE)
	# compute numbers of ordinal and binary variables
	if(dataType == "raw"){
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		isFactor = umx_is_ordered(mzData[, selDVs])                      # T/F list of factor columns
		isOrd    = umx_is_ordered(mzData[, selDVs], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
		isBin    = umx_is_ordered(mzData[, selDVs], binary.only  = TRUE) # T/F list of binary columns
		nFactors = sum(isFactor)
		nOrdVars = sum(isOrd) # total number of ordinal columns
		nBinVars = sum(isBin) # total number of binary columns

		factorVarNames = names(mzData)[isFactor]
		ordVarNames    = names(mzData)[isOrd]
		binVarNames    = names(mzData)[isBin]
		contVarNames   = names(mzData)[!isFactor]
	} else {
		# summary data
		isFactor = isOrd = isBin = c()
		nFactors = nOrdVars = nBinVars = 0
		factorVarNames = ordVarNames = binVarNames = contVarNames = c()
	}
	if(nFactors > 0 & is.null(suffix)){
		stop("Please set suffix.\n",
		"Why: You have included ordinal or binary variables. I need to know which variables are for twin 1 and which for twin2.\n",
		"The way I do this is enforcing some naming rules. For example, if you have 2 variables:\n",
		" obesity and depression called: 'obesity_T1', 'dep_T1', 'obesity_T2' and 'dep_T2', you should call umxACE with:\n",
		"selDVs = c('obesity','dep'), suffix = '_T' \n",
		"suffix is just one word, appearing in all variables (e.g. '_T').\n",
		"This is assumed to be followed by '1' '2' etc...")
	}
	used = selDVs
	if(!is.null(weightVar)){
		used = c(used,weightVar)
	}
	# Drop unused columns from mz and dzData
	mzData = mzData[, used]
	dzData = dzData[, used]

	if(dataType == "raw") {
		if(!is.null(weightVar)){
			# weight variable provided: check it exists in each frame
			if(!umx_check_names(weightVar, data = mzData, die = FALSE) | !umx_check_names(weightVar, data = dzData, die = FALSE)){
				stop("The weight variable must be included in the mzData and dzData",
					 " frames passed into umxACE when \"weightVar\" is specified",
					 "\n mzData contained:", paste(names(mzData), collapse = ", "),
					 "\n and dzData contain:", paste(names(dzData), collapse = ", "),
					 "\nbut I was looking for ", weightVar, " as the moderator."
				)
			}
			mzWeightMatrix = mxMatrix(name = "mzWeightMatrix", type = "Full", nrow = nrow(mzData), ncol = 1, free = FALSE, values = mzData[, weightVar])
			dzWeightMatrix = mxMatrix(name = "dzWeightMatrix", type = "Full", nrow = nrow(dzData), ncol = 1, free = FALSE, values = dzData[, weightVar])
			mzData = mzData[, selDVs]
			dzData = dzData[, selDVs]
			bVector = TRUE
		} else {
			# no weights
		}

		# ===============================
		# = Notes: Ordinal requires:    =
		# ===============================
		# 1. Set to mxFactor
		# 2. For Binary vars:
		#   1. Means of binary vars fixedAt 0
		#   2. A+C+E for binary vars is constrained to 1 
		# 4. For Ordinal vars, first 2 thresholds fixed
		# 5. Option to fix all (or all but the first 2??) thresholds for left-censored data.
        #   # TODO
		# 	1. Simple test if results are similar for an ACE model of 1 variable
		# ===========================
		# = Add means matrix to top =
		# ===========================
		# Figure out ace starts while we are here
		# varStarts will be used to fill a, c, and e
		# mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE)
		varStarts = umx_cov_diag(mzData[, selDVs[1:nVar], drop = FALSE], ordVar = 1, use = "pairwise.complete.obs")
		if(nVar == 1){
			varStarts = varStarts/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		# Mean starts (used across all raw solutions
		obsMZmeans = umx_means(mzData[, selDVs], ordVar = 0, na.rm = TRUE)
		meanDimNames = list("means", selDVs)		
		# smarter but not guaranteed
		# a_val = e_val = t(chol(xmu_cov_factor(mzData, use = "pair"))) * .6
		# c_val = t(chol(cov(mzData, use = "pair"))) * .1
		if(nFactors == 0) {			
			# =======================================================
			# = Handle all continuous case                          =
			# =======================================================
			message("All variables continuous")
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
			top = mxModel("top", umxLabel(meansMatrix))
			MZ  = mxModel("MZ" , mxExpectationNormal("top.expCovMZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ" , mxExpectationNormal("top.expCovDZ", "top.expMean"), mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
		} else if(sum(isBin) == 0){
			# =======================================================
			# = Handle some 1 or more ordinal variables (no binary) =
			# =======================================================
			message("umxACE found ", (nOrdVars/nSib), " pairs of ordinal variables:", omxQuotes(ordVarNames),
			"\nNo binary variables were found")			
			if(length(contVarNames) > 0){
				message("There were also ", length(contVarNames)/nSib, " pairs of continuous variables:", omxQuotes(contVarNames))	
			}else{
				message("No continuous variables found.")
			}
			# Means: all free, start cont at the measured value, ord @0
			meansMatrix  = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMZmeans, dimnames = meanDimNames)
			# Thresholds
			# for better guessing with low-freq cells
			allData = rbind(mzData, dzData)
			# threshMat is a three-item list of matrices and algebra
			threshMat = umxThresholdMatrix(allData, suffixes = paste0(suffix, 1:2), verbose = FALSE, hint = hint)
			# return(threshMat)
			mzExpect = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
			dzExpect = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")			
			top = mxModel("top", umxLabel(meansMatrix), threshMat)
			MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )
		} else if(sum(isBin) > 0){
			# =======================================================
			# = Handle case of at least 1 binary variable           =
			# =======================================================

			message("umxACE found ", sum(isBin)/nSib, " pairs of binary variables:", omxQuotes(binVarNames))
			message("\nI am fixing the latent means and variances of these variables to 0 and 1")
			if(nOrdVars > 0){
				message("There were also ", nOrdVars/nSib, " pairs of ordinal variables:", omxQuotes(ordVarNames))			
			}
			if(length(contVarNames) > 0){
				message("\nand ", length(contVarNames)/nSib, " pairs of continuous variables:", omxQuotes(contVarNames))	
			}else{
				message("No continuous variables")
			}
			
			# ===========================================================================
			# = Means: bin fixed, others free, start cont at the measured value, ord @0 =
			# ===========================================================================
			# Fill with zeros: default for ordinals and binary...
			meansFree = (!isBin) # fix the binary variables at zero
			meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = nVar*nSib, free = meansFree, values = obsMZmeans, dimnames = meanDimNames)

			# = Thresholds =
			# For better guessing with low-freq cells
			allData = rbind(mzData, dzData)
			# threshMat may be a three item list of matrices and algebra
			threshMat = umxThresholdMatrix(allData, suffixes = paste0(suffix, 1:2), verbose = TRUE)
			mzExpect  = mxExpectationNormal("top.expCovMZ", "top.expMean", thresholds = "top.threshMat")
			dzExpect  = mxExpectationNormal("top.expCovDZ", "top.expMean", thresholds = "top.threshMat")

			top = mxModel("top", umxLabel(meansMatrix), threshMat)
			MZ  = mxModel("MZ", mzExpect, mxFitFunctionML(vector = bVector), mxData(mzData, type = "raw") )
			DZ  = mxModel("DZ", dzExpect, mxFitFunctionML(vector = bVector), mxData(dzData, type = "raw") )

			# ===================================
			# = Constrain Ordinal variance @1  =
			# ===================================
			# Algebra to pick out the ord vars
			# TODO check this way of using twin 1 to pick where the bin vars are is robust...
			the_bin_cols = which(isBin)[1:nVar] # columns in which the bin vars appear for twin 1, i.e., c(1,3,5,7)
			binBracketLabels = paste0("Vtot[", the_bin_cols, ",", the_bin_cols, "]")

			top = mxModel(top,
				# Algebra to compute total variances and standard deviations
				mxAlgebra(name = "Vtot", A + C+ E), # Total variance (redundant but is OK)
				mxMatrix(name  = "binLabels"  , "Full", nrow = (nBinVars/nSib), ncol = 1, labels = binBracketLabels),
				mxMatrix(name  = "Unit_nBinx1", "Unit", nrow = (nBinVars/nSib), ncol = 1),
				mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
			)
		} else {
			stop("You appear to have something other than I expected in terms of binary, ordinal and continuous variable mix")
		}
		# nb: means not yet equated across twins
	} else if(dataType %in% c("cov", "cor")){
		if(!is.null(weightVar)){
			stop("You can't set weightVar when you give cov data - use cov.wt to create weighted cov matrices, or pass in raw data")
		}
		umx_check(!is.null(numObsMZ), "stop", paste0("You must set numObsMZ with ", dataType, " data"))
		umx_check(!is.null(numObsDZ), "stop", paste0("You must set numObsDZ with ", dataType, " data"))
		# TODO should keep this just as mzData?
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
		varStarts = diag(het_mz)
		if(nVar == 1){
			varStarts = varStarts/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		top = mxModel("top")
		MZ = mxModel("MZ", 
			mxExpectationNormal("top.expCovMZ"), 
			mxFitFunctionML(), 
			mxData(het_mz, type = "cov", numObs = numObsMZ)
		)
		
		DZ = mxModel("DZ",
			mxExpectationNormal("top.expCovDZ"),
			mxFitFunctionML(),
			mxData(het_dz, type = "cov", numObs = numObsDZ)
		)
	} else {
		stop("Datatype \"", dataType, "\" not understood. Must be one of raw, cov, or cor")
	}
	message("treating data as ", dataType)

	# Finish building top
	top = mxModel(top,
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of
		# NB: top already has the means model and thresholds matrix added if necessary  - see above
		# Additive, Common, and Unique environmental paths
		umxLabel(mxMatrix(name = "a", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),
		umxLabel(mxMatrix(name = "c", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),
		umxLabel(mxMatrix(name = "e", type = "Lower", nrow = nVar, ncol = nVar, free = T, values = varStarts, byrow = T)),  
		
		mxMatrix(name = "dzAr", type = "Full", 1, 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", 1, 1, free = FALSE, values = dzCr),
		# Multiply by each path coefficient by its inverse to get variance component
		# Quadratic multiplication to add common_loadings
		mxAlgebra(name = "A", a %*% t(a)), # additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # unique environmental variance
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		mxAlgebra(rbind (cbind(ACE, AC),
		                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "expCovMZ"),
		mxAlgebra(rbind (cbind(ACE, hAC),
		                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "expCovDZ")
	)

	if(!bVector){
		model = mxModel(name, MZ, DZ, top,
			mxFitFunctionMultigroup(c("MZ", "DZ"))
		)
	} else {
		# bVector is TRUE
		# To weight objective functions in OpenMx, you specify a container model that applies the weights
		# m1 is the model with no weights, but with "vector = TRUE" option added to the FIML objective.
		# This option makes FIML return individual likelihoods for each row of the data (rather than a single -2LL value for the model)
		# You then optimize weighted versions of these likelihoods by building additional models containing 
		# weight data and an algebra that multiplies the likelihoods from the first model by the weight vector
		model = mxModel(name, MZ, DZ, top,
			mxModel("MZw", mzWeightMatrix,
				mxAlgebra(-2 * sum(mzWeightMatrix * log(MZ.objective) ), name = "mzWeightedCov"),
				mxFitFunctionAlgebra("mzWeightedCov")
			),
			mxModel("DZw", dzWeightMatrix,
				mxAlgebra(-2 * sum(dzWeightMatrix * log(DZ.objective) ), name = "dzWeightedCov"),
				mxFitFunctionAlgebra("dzWeightedCov")
			),
			mxFitFunctionMultigroup(c("MZw", "DZw"))
		)
	}
	if(!is.null(boundDiag)){
		diag(model$submodels$top$matrices$a$lbound) = boundDiag
		diag(model$submodels$top$matrices$c$lbound) = boundDiag
		diag(model$submodels$top$matrices$e$lbound) = boundDiag
	}
	if(addStd){
		newTop = mxModel(model$submodels$top,
			mxMatrix(name  = "I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E),       # Total variance
			# TODO test that these are identical in all cases
			# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # Total variance
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e)  # standardized e
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}
	}
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans & (dataType == "raw")){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMean14", "expMean15", "expMean16"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMean11", "expMean12", "expMean13")
		)
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModel.ACE") # set class so that S3 plot() dispatches.
	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
		# if(!is.na(umx_set_auto_plot())){
			# plot(model)
		# }
	} else {
		# --
	}
	return(model)
} #end umxACE


# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::release("~/bin/umx", check = TRUE)
# https://www.researchgate.net/publication/287346605_A_New_Approach_to_Handle_Missing_Covariate_Data_in_Twin_Research

#' umxACEcov
#'
#' Make a 2-group ACE Cholesky Twin model with covariates modeled (see Details below)
#' 
#' umxACEcov supplements the \code{\link{umxACE}} Cholesky model with covariates.
#'
#' @param name The name of the model (defaults to"ACE")
#' @param selDVs The variables to include from the data (do not include suffixes)
#' @param selCovs The covariates to include from the data (do not include suffixes)
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param suffix suffix for twin 1 and twin 2, often "_T" Used to expand selDVs into
#' full column names, i,e "dep" -->  c("dep_T1", "dep_T2")
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE)
#' @param boundDiag = Whether to bound the diagonal of the a, c, and e matrices
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE)
#' @param hint An analysis hint. Options include "none", (default) "left_censored". Default does nothing.
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE)
#' @param autoRun Whether to run the model and return it, or just return it
#' @return - \code{\link{mxModel}} of subclass mxModel.ACEcov
#' @export
#' @family Twin Modeling Functions
#' @references - Neale, M. C., & Martin, N. G. (1989). The effects of age, sex, 
#' and genotype on self-report drunkenness following a challenge dose of alcohol. 
#' Behavior Genetics, 19(1), 63-78. doi:10.1007/BF01065884

#' @examples
#' require(umx)
#' data(twinData)
#' # add age 1 and age 2 columns
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi")
#' selCovs = c("age")
#' selVars = umx_paste_names(c(selDVs, selCovs), textConstant = "", suffixes= 1:2)
#' # just top 80 so example runs in a couple of secs
#' mzData = subset(twinData, zyg == 1, selVars)[1:80, ]
#' dzData = subset(twinData, zyg == 3, selVars)[1:80, ]
#' # TODO update for new dataset variable zygosity
#' # mzData = subset(twinData, zygosity == "MZFF", selVars)[1:80, ]
#' # dzData = subset(twinData, zygosity == "DZFF", selVars)[1:80, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, 
#' 	 suffix = "", autoRun = TRUE)
#' umxSummary(m1)
#' plot(m1)
umxACEcov <- function(name = "ACEcov", selDVs, selCovs, dzData, mzData, suffix = NULL, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, boundDiag = NULL, equateMeans = TRUE, bVector = FALSE, hint = c("none", "left_censored"), autoRun = getOption("umx_auto_run")) {
	if(nrow(dzData)==0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData)==0){ stop("Your MZ dataset has no rows!") }
	nSib = 2 # number of siblings in a twin pair
	if(dzCr == .25 && name == "ACE"){
		name = "ADEcov"
	}
	# look for name conflicts
	badNames = umx_grep(selDVs, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}

	if(is.null(suffix)){
		stop("I need a suffix, like '_T'. I will add 1 and 2 afterwards... \n",
		"i.e., selDVs should be 'bmi' etc, and I will re-name to 'bmi_T1' and 'bmi_T2' etc.")
	}else if(length(suffix) > 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., you have to name your variables 'obese_T1' and 'obese_T2' etc.")
	}else{
		# stash base names for use later
		baseDVs = selDVs
		baseCovs = selCovs
		# fill out full trait names
		selDVs  = umx_paste_names(selDVs, suffix, 1:2)
		selCovs = umx_paste_names(selCovs, suffix, 1:2)
	}

	nDV  = length(baseDVs)
	nCov = length(baseCovs)
	nVar = nDV + nCov # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	selVars = c(selDVs, selCovs)
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))

	# Drop unused columns from mz and dzData
	mzData = mzData[, selVars]
	dzData = dzData[, selVars]

	meanDimNames = list(NULL, selVars)
	# meanDimNames= list("means", selVars)
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans){
		meanLabels = c(rep(paste0("expMean_", baseDVs), nSib), paste0("expMean_", selCovs))
	}else{
		stop("Currently, means must be equated")
		# meanLabels = c(paste0("expMean_", selDVs), paste0("expMean_", selCovs))
	}
	
	obsMZmeans  = umx_means(mzData[, selVars], ordVar = 0, na.rm = TRUE)
	meansMatrix = mxMatrix(name = "expMean", "Full" , nrow = 1, ncol = (nVar * nSib),
			free = TRUE, values = obsMZmeans, labels = meanLabels, dimnames = meanDimNames)
	# TODO this should be variance not correlation...
	varStarts = diag(umxHetCor(mzData[,selDVs]))
	if(nDV == 1){
		varStarts = varStarts/3
	} else {
		varStarts = t(chol(diag(varStarts/3))) # divide variance up equally, and set to Cholesky form.
	}
	# TODO fix varStarts to include covs also
	varStarts = matrix(varStarts, nDV, nDV)

	top = mxModel("top",
		# "top" defines the algebra of the twin model, which MZ and DZ slave off of.
		# NB: top already has the means model and thresholds matrix added if necessary  - see above.
		# Additive, Common, and Unique environmental paths.
		# Matrices a, c, e to store a, c, e path coefficients.
		umxLabel(mxMatrix(name = "a", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE, dimnames = list(baseDVs, baseDVs))),
		umxLabel(mxMatrix(name = "c", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE)),
		umxLabel(mxMatrix(name = "e", type = "Full", nrow = nDV, ncol = nDV, free = TRUE, values = varStarts, byrow = TRUE)),  

		mxMatrix(name = "dzAr", type = "Full", nrow = 1, ncol = 1, free = FALSE, values = dzAr),
		mxMatrix(name = "dzCr", type = "Full", nrow = 1, ncol = 1, free = FALSE, values = dzCr),

		meansMatrix,
		mxMatrix(name = "lowerB", 'Lower', nrow = nCov, ncol = nCov, values = 0.5, free = TRUE),
		mxMatrix(name = "lowerW", 'Lower', nrow = nCov, ncol = nCov, values = 0.5, free = TRUE),
		mxAlgebra(name= "CovB"  , lowerB %*% t(lowerB)),
		mxAlgebra(name= "CovW"  , lowerW %*% t(lowerW)),
		mxAlgebra(name= "CovWB" , CovW + CovB),
		
		# Matrices A, C,E + compute variance components
		mxAlgebra(name = "A", a %*% t(a)),
		mxAlgebra(name = "C", c %*% t(c)),
		mxAlgebra(name = "E", e %*% t(e)),
		# Declare a vector for the regression parameters
		mxMatrix(name = "beta", type = "Full", nrow = nCov, ncol  = 1, free = TRUE, values = 0, labels = paste0("beta", 1:nCov)),
		# Some handy component algebras
		mxAlgebra(name = "ACE", A + C + E),
		mxAlgebra(name = "AC" , A + C),
		mxAlgebra(name = "hAC", (dzAr * A) + C),
		mxAlgebra(name = "tb_CovWB_b", (t(beta) %*% CovWB) %*% beta),
		mxAlgebra(name = "tb_CovB_b" , (t(beta) %*% CovB) %*% beta),
		mxAlgebra(name = "tb_CovWB"  , t(beta) %*% CovWB),
		mxAlgebra(name = "tb_CovB"   , t(beta) %*% CovB),
		mxAlgebra(name = "CovWB_b"   ,   CovWB %*% beta),
		mxAlgebra(name = "CovB_b"    ,   CovB  %*% beta),
		# Algebra for expected variance/covariance matrix #in MZ twins
		mxAlgebra(name = "expCovMZ", expression = rbind(
			cbind(ACE + tb_CovWB_b, AC  + tb_CovB_b , tb_CovWB, tb_CovB),
			cbind(AC  + tb_CovB_b , ACE + tb_CovWB_b, tb_CovB , tb_CovWB),
			cbind(CovWB_b         , CovB_b          , CovWB   , CovB),
			cbind(CovB_b          , CovWB_b         , CovB    , CovWB))
		),
		# Algebra for expected variance/covariance matrix #in DZ twins
		mxAlgebra(name = "expCovDZ", expression = rbind(
			cbind(ACE + tb_CovWB_b, hAC + tb_CovB_b , tb_CovWB, tb_CovB),
			cbind(hAC + tb_CovB_b , ACE + tb_CovWB_b, tb_CovB , tb_CovWB),
			cbind(CovWB_b         , CovB_b          , CovWB   , CovB),
			cbind(CovB_b          , CovWB_b         , CovB    , CovWB))
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
	
	if(addStd){
		newTop = mxModel(model$submodels$top,
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
	model = as(model, "MxModel.ACEcov") # set class so that S3 plot() dispatches.
	if(autoRun){
		return(mxRun(model))
	} else {
		return(model)
	}
}

#' umxCP
#'
#' Make a 2-group Common Pathway twin model (Common-factor common-pathway multivariate model).
#' 
#' The common-pathway model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences.
#' 
#' umxCP supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Like the \code{\link{umxACE}} model, the CP model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure below).
#' 
#' Common-pathway path diagram:
#' 
#' \figure{CP.png}
#' 
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotye.
#' 
#' \strong{Data Input}
#' Currently, the umxCP function accepts only raw data. This may change in future versions.
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' \strong{Additional features}
#' The umxCP function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modelling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' @param name The name of the model (defaults to "CP")
#' @param selDVs The variables to include
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param suffix The suffix for twin 1 and twin 2, often "_T". If set, you can
#' omit suffixes in SelDVs, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param nFac How many common factors (default = 1)
#' @param freeLowerA Whether to leave the lower triangle of A free (default = F)
#' @param freeLowerC Whether to leave the lower triangle of C free (default = F)
#' @param freeLowerE Whether to leave the lower triangle of E free (default = F)
#' @param correlatedA ?? (default = FALSE)
#' @param equateMeans Whether to equate the means across twins (defaults to T)
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE)
#' @param numObsDZ = not yet implemented: Ordinal Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = not yet implemented: Ordinal Number of MZ twins: Set this if you input covariance data
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData) 
#' zygList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = zygList)
#' selDVs = c("ht", "wt")
#' mzData <- subset(twinData, ZYG == "MZFF", umx_paste_names(selDVs, "", 1:2))
#' dzData <- subset(twinData, ZYG == "DZFF", umx_paste_names(selDVs, "", 1:2))
#' m1 = umxCP(selDVs = selDVs, dzData = dzData, mzData = mzData, suffix = "")
#' umxSummary(m1, file=NA) # file = NA to avoid opening a plot window during CRAN check
#' umxGetParameters(m1, "^c", free = TRUE)
#' m2 = umxModify(m1, update = "(cs_.*$)|(c_cp_)", regex = TRUE, name = "dropC")
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
umxCP <- function(name = "CP", selDVs, dzData, mzData, suffix = NULL, nFac = 1, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = FALSE, equateMeans=T, dzAr=.5, dzCr=1, addStd = T, addCI = T, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run")) {
	nSib = 2
	# expand var names
	if(!is.null(suffix)){
		if(length(suffix) != 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., set selDVs to 'obese', suffiex to '_T' and I look for 'obese_T1' and 'obese_T2' in the data...\n",
			"PS: variables have to end in 1 or 2, i.e  'example_T1' and 'example_T2'")
		}
		selDVs = umx_paste_names(selDVs, suffix, 1:2)
	}
	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	# message("selDVs: ", omxQuotes(selDVs))
	nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	vars = selDVs[1:nVar]
	dataType = umx_is_cov(dzData)
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from mz and dzData
		mzData = mzData[, selDVs]
		dzData = dzData[, selDVs]
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}

	if(dataType == "raw"){
		obsMZmeans = colMeans(mzData, na.rm = T);
		top = mxModel("top", 
			# means (not yet equated across twins)
			umxLabel(mxMatrix(name = "expMean", type = "Full" , nrow = 1, ncol = (nVar * nSib), 
				free = TRUE, values = obsMZmeans, dimnames = list("means", selDVs)
			))
		) 
		MZ = mxModel("MZ", 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"),
			mxFitFunctionML()
		)
	} else {
		top = mxModel("top") # no means
		# TODO add alernative fit types?
		MZ = mxModel("MZ", 
			mxData(mzData, type = "cov", numObs = numObsMZ),
			mxExpectationNormal("top.expCovMZ"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(dzData, type = "cov", numObs = numObsDZ),
			mxExpectationNormal("top.expCovDZ"),
			mxFitFunctionML()
		)
	}

	if(correlatedA){
		a_cp_matrix = umxLabel(mxMatrix("Lower", nFac, nFac, free = TRUE, values = .7, name = "a_cp"), jiggle = .05) # Latent common factor
	} else {
		a_cp_matrix = umxLabel(mxMatrix("Diag", nFac, nFac, free = TRUE, values = .7, name = "a_cp"), jiggle =.05)
	}

	model = mxModel(name,
		mxModel(top,
			mxMatrix(name = "dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			mxMatrix(name = "dzCr", "Full", 1, 1, free = FALSE, values = dzCr),	
			# Latent common factor genetic paths
			a_cp_matrix,
			umxLabel(mxMatrix(name="c_cp", "Diag", nFac, nFac, free = TRUE, values =  0), jiggle = .05), # latent common factor Common #environmental path coefficients
			umxLabel(mxMatrix(name="e_cp", "Diag", nFac, nFac, free = TRUE, values = .7), jiggle = .05), # latent common factor Unique environmental path coefficients
			# Constrain variance of latent phenotype factor to 1.0
			# Multiply by each path coefficient by its inverse to get variance component
			mxAlgebra(name="A_cp", a_cp %*% t(a_cp)), # A_cp variance
			mxAlgebra(name="C_cp", c_cp %*% t(c_cp)), # C_cp variance
			mxAlgebra(name="E_cp", e_cp %*% t(e_cp)), # E_cp variance
			mxAlgebra(name = "L" , A_cp + C_cp + E_cp), # total common factor covariance (a+c+e)
			mxMatrix("Unit", nrow=nFac, ncol=1, name = "nFac_Unit"),
			mxAlgebra(diag2vec(L)             , name = "diagL"),
			mxConstraint(diagL == nFac_Unit   , name = "fix_CP_variances_to_1"),

			umxLabel(mxMatrix(name = "as", "Lower", nVar, nVar, free = T, values = .5), jiggle = .05), # Additive genetic path 
			umxLabel(mxMatrix(name = "cs", "Lower", nVar, nVar, free = T, values = .1), jiggle = .05), # Common environmental path 
			umxLabel(mxMatrix(name = "es", "Lower", nVar, nVar, free = T, values = .6), jiggle = .05), # Unique environmental path
			umxLabel(mxMatrix(name = "cp_loadings", "Full" , nVar, nFac, free = T, values = .6), jiggle = .05), # loadings on latent phenotype
			# Quadratic multiplication to add cp_loading effects
			mxAlgebra(cp_loadings %&% A_cp + as %*% t(as), name = "A"), # Additive genetic variance
			mxAlgebra(cp_loadings %&% C_cp + cs %*% t(cs), name = "C"), # Common environmental variance
			mxAlgebra(cp_loadings %&% E_cp + es %*% t(es), name = "E"), # Unique environmental variance
			mxAlgebra(A+C+E, name = "ACE"),
			mxAlgebra(A+C  , name = "AC" ),
			mxAlgebra( (dzAr %x% A) + (dzCr %x% C),name="hAC"),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name="expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name="expCovDZ")
		),
		MZ, DZ,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
		# mxCI(c('top.a_cp'))
	)
	# Equate means for twin1 and twin 2 by matching labels in the first and second halves of the means labels matrix
	if(equateMeans & dataType == "raw"){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
		)
	}
	if(!freeLowerA){
		toset  = model$submodels$top$matrices$as$labels[lower.tri(model$submodels$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	
	if(!freeLowerC){
		toset  = model$submodels$top$matrices$cs$labels[lower.tri(model$submodels$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerE){
		toset  = model$submodels$top$matrices$es$labels[lower.tri(model$submodels$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(addStd){
		newTop = mxModel(model$submodels$top,
			# nVar Identity matrix
			mxMatrix(name = "I", "Iden", nVar, nVar),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(name = "SD", solve(sqrt(I * ACE))),
			# Standard specific path coefficients
			mxAlgebra(name = "as_std", SD %*% as), # standardized a
			mxAlgebra(name = "cs_std", SD %*% cs), # standardized c
			mxAlgebra(name = "es_std", SD %*% es), # standardized e
			# Standardize loadings on Common factors
			mxAlgebra(SD %*% cp_loadings, name = "cp_loadings_std") # Standardized path coefficients (general factor(s))
		)
		model = mxModel(model, newTop)
		if(addCI){
			# TODO break these out into single labels.
			model = mxModel(model, mxCI(c('top.as_std', 'top.cs_std', 'top.es_std', 'top.cp_loadings_std')))
		}
	}
	model = omxAssignFirstParameters(model) # Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = as(model, "MxModel.CP")
	if(autoRun){
		return(mxRun(model))
	} else {
		return(model)
	}
} # end umxCP

#' umxIP
#'
#' Make a 2-group Independent Pathway twin model (Common-factor independent-pathway multivariate model)
#' 
#' The following figure shows the IP model diagramatically:
#' 
#' \figure{IP.png}
#'
#' @param name The name of the model (defaults to "IP")
#' @param selDVs The variables to include
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param suffix The suffix for twin 1 and twin 2, often "_T". If set, you can
#' omit suffixes in SelDVs, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param nFac How many common factors (default = 1)
#' @param freeLowerA Whether to leave the lower triangle of A free (default = F)
#' @param freeLowerC Whether to leave the lower triangle of C free (default = F)
#' @param freeLowerE Whether to leave the lower triangle of E free (default = F)
#' @param equateMeans Whether to equate the means across twins (defaults to T)
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param correlatedA Whether factors are allowed to correlate (not implemented yet: FALSE)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE)
#' @param numObsDZ = todo: implement ordinal Number of DZ twins: Set this if you input covariance data
#' @param numObsMZ = todo: implement ordinal Number of MZ twins: Set this if you input covariance data
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(twinData)
#' zygList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = zygList)
#' mzData <- subset(twinData, ZYG == "MZFF")
#' dzData <- subset(twinData, ZYG == "DZFF")
#' selDVs = c("ht", "wt") # These will be expanded into "ht1" "ht2"
#' m1 = umxIP(selDVs = selDVs, suffix = "", dzData = dzData, mzData = mzData)
#' umxSummary(m1, file = NA) # file = NA to avoid opening a plot window during CRAN check
umxIP <- function(name = "IP", selDVs, dzData, mzData, suffix = NULL, nFac = 1, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, equateMeans = TRUE, dzAr = .5, dzCr = 1, correlatedA = FALSE, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run")) {
	# TODO implement correlatedA
	if(correlatedA){
		message("I have not implemented correlatedA yet...")
	}
	nSib = 2;
	# expand var names
	if(!is.null(suffix)){
		if(length(suffix) != 1){
			stop("suffix should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., set selDVs to 'obese', suffix to '_T' and I look for 'obese_T1' and 'obese_T2' in the data...\n",
			"nb: variables MUST be sequentially numbered, i.e  'example_T1' and 'example_T2'")
		}
		selDVs = umx_paste_names(selDVs, suffix, 1:2)
	}
	umx_check_names(selDVs, mzData)
	umx_check_names(selDVs, dzData)
	# message("selDVs: ", omxQuotes(selDVs))
	nVar = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	dataType = umx_is_cov(dzData)
	
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from mz and dzData
		mzData = mzData[, selDVs, drop = FALSE]
		dzData = dzData[, selDVs, drop = FALSE]
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
		stop("COV not fully implemented yet for IP...")
	} else {
		stop("Datatype ", omxQuotes(dataType), " not understood")
	}

	obsMZmeans = colMeans(mzData, na.rm=TRUE);
	nVar       = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
	vars       = selDVs[1:nVar]
	model = mxModel(name,
		mxModel("top",
			umxLabel(mxMatrix("Full", 1, nVar*nSib, free=T, values=obsMZmeans, dimnames=list("means", selDVs), name="expMean")), # Means 
			# (not yet equated for the two twins)
			# Matrices ac, cc, and ec to store a, c, and e path coefficients for independent general factors
			umxLabel(mxMatrix("Full", nVar, nFac, free=T, values=.6, name="ai"), jiggle=.05), # latent common factor Additive genetic path 
			umxLabel(mxMatrix("Full", nVar, nFac, free=T, values=.0, name="ci"), jiggle=.05), # latent common factor Common #environmental path coefficient
			umxLabel(mxMatrix("Full", nVar, nFac, free=T, values=.6, name="ei"), jiggle=.05), # latent common factor Unique environmental path #coefficient
			# Matrices as, cs, and es to store a, c, and e path coefficients for specific factors
			umxLabel(mxMatrix("Lower", nVar, nVar, free=T, values=.6, name="as"), jiggle=.05), # Additive genetic path 
			umxLabel(mxMatrix("Lower", nVar, nVar, free=T, values=.0, name="cs"), jiggle=.05), # Common environmental path 
			umxLabel(mxMatrix("Lower", nVar, nVar, free=T, values=.6, name="es"), jiggle=.05), # Unique environmental path.

			mxMatrix("Full", 1, 1, free = FALSE, values = dzAr, name = "dzAr"),
			mxMatrix("Full", 1, 1, free = FALSE, values = dzCr, name = "dzCr"),

			# Multiply by each path coefficient by its inverse to get variance component
			# Sum the squared independent and specific paths to get total variance in each component
			mxAlgebra(name = "A", ai%*%t(ai) + as%*%t(as) ), # Additive genetic variance
			mxAlgebra(name = "C", ci%*%t(ci) + cs%*%t(cs) ), # Common environmental variance
			mxAlgebra(name = "E", ei%*%t(ei) + es%*%t(es) ), # Unique environmental variance

			mxAlgebra(name="ACE", A+C+E),
			mxAlgebra(name="AC" , A+C  ),
			mxAlgebra( (dzAr %x% A) + (dzCr %x% C),name="hAC"),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name = "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name = "expCovDZ"),

			# Algebra to compute total variances and standard deviations (diagonal only)
			mxMatrix("Iden", nrow = nVar, name = "I"),
			mxAlgebra(solve(sqrt(I * ACE)), name = "iSD")
		),
		mxModel("MZ", 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxModel("DZ", 
			mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Equate means for twin1 and twin 2
	if(equateMeans){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar+1):(nVar*2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
		)
	}
	
	if(!freeLowerA){
		toset  = model$submodels$top$matrices$as$labels[lower.tri(model$submodels$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(!freeLowerC){
		toset  = model$submodels$top$matrices$cs$labels[lower.tri(model$submodels$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	
	if(!freeLowerE){
		toset  = model$submodels$top$matrices$es$labels[lower.tri(model$submodels$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	} else {
		# set the first column off, bar r1
		model = omxSetParameters(model, labels = "es_r[^1]0-9?c1", free = FALSE, values = 0)

		# toset  = model$submodels$top$matrices$es$labels[lower.tri(model$submodels$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
		# toset  = model$submodels$top$matrices$es$labels[lower.tri(model$submodels$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)

		# Used to drop the ei paths, as we have a full Cholesky for E, now just set the bottom row TRUE
		# toset = umxGetParameters(model, "^ei_r.c.", free= TRUE)
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(addStd){
		newTop = mxModel(model$submodels$top,
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
			model = mxModel(model, mxCI(c('top.ai_std','top.ci_std','top.ei_std', 'top.as_std','top.cs_std','top.es_std')))
		}
	}
	model  = omxAssignFirstParameters(model) # ensure parameters with the same label have the same start value... means, for instance.
	model = as(model, "MxModel.IP")
	if(autoRun){
		return(mxRun(model))
	} else {
		return(model)
	}
} # end umxIP

#' umxACESexLim
#'
#' Cholesky style sex-limitation model.
#'
#' This is a multi-variate capable Quantitative & Qualitative Sex-Limitation script using
#' ACE Cholesky modeling. It implements a correlation approach to ensure that order of variables
#' does NOT affect ability of model to account for DZOS data.
#'  
#' Restrictions include the assumtion that twin means and variances can be equated across birth
#' order within zygosity groups.
#' 
#' Note: Qualitative sex differences are differences in the latent A, C, or E latent variables
#' Note: Quantitative sex differences are differences in the path loadings from A, C, or E 
#' to the measured variables
#' @param name The name of the model (defaults to "ACE_sexlim")
#' @param selDVs The variables to include. If you provide a suffix, you can use just the base names.
#' @param mzmData The MZ male dataframe
#' @param dzmData The DZ male dataframe
#' @param mzfData The DZ female dataframe
#' @param dzfData The DZ female dataframe
#' @param dzoData The DZ opposite-sex dataframe. (be sure and get in right order)
#' @param suffix The suffix for twin 1 and twin 2, often "_T". If set, you can
#' omit suffixes in SelDVs, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @return - ACE sexlim model
#' @export
#' @family Twin Modeling Functions
#' @references - Neale et al., (2006). Multivariate genetic analysis of sex-lim and GxE interaction, Twin Research & Human Genetics.,
#' \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' # Load Libraries
#' require(umx);
#' # =========================
#' # = Load and Process Data =
#' # =========================
#' data('us_skinfold_data')
#' # rescale vars
#' us_skinfold_data[,c('bic_T1', 'bic_T2')] <- us_skinfold_data[,c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[,c('tri_T1', 'tri_T2')] <- us_skinfold_data[,c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[,c('caf_T1', 'caf_T2')] <- us_skinfold_data[,c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[,c('ssc_T1', 'ssc_T2')] <- us_skinfold_data[,c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[,c('sil_T1', 'sil_T2')] <- us_skinfold_data[,c('sil_T1', 'sil_T2')]/5
#' 
#' # Select Variables for Analysis
#' varList = c('ssc','sil','caf','tri','bic')
#' selVars = umx_paste_names(varList, "_T", 1:2)
#' 
#' # Data objects for Multiple Groups
#' mzmData = subset(us_skinfold_data, zyg == 1, selVars)
#' dzmData = subset(us_skinfold_data, zyg == 3, selVars)
#' mzfData = subset(us_skinfold_data, zyg == 2, selVars)
#' dzfData = subset(us_skinfold_data, zyg == 4, selVars)
#' dzoData = subset(us_skinfold_data, zyg == 5, selVars)
#' 
#' m1 = umxACESexLim(selDVs = varList, suffix = "_T",
#'        mzmData = mzmData, dzmData = dzmData, 
#'        mzfData = mzfData, dzfData = dzfData, 
#'        dzoData = dzoData)
#' m1 = mxRun(m1)
#' # ===================================================
#' # = Test switching specific a from Males to females =
#' # ===================================================
#' m2 = umxSetParameters(m1, labels = "asm_.*", free = FALSE, values = 0, regex = TRUE)
#' m2 = umxSetParameters(m1, labels = "asf_.*", free = TRUE , values = 0, regex = TRUE)
#' m2 = mxRun(m2)
#' summary(m2)
#' umxCompare(m2, m1)
#' # does fit move on repeated execution?
#' # for (i in 1:4) { m2 <- mxRun(m2); print(m2 $output$mi) }
#' }
umxACESexLim <- function(name = "ACE_sexlim", selDVs, mzmData, dzmData, mzfData, dzfData, dzoData, suffix = NULL, autoRun = getOption("umx_auto_run")){
	if(is.null(suffix)){
		stop("umx functions now require the suffix parameter is set,
		and selDVs is just a list of variable's base names")
	}
	 selDVs = umx_paste_names(selDVs, suffix, 1:2)

	# ==========================
	# = Cholesky (Ch) Approach =
	# ==========================
	# 1 Nonscalar Sex Limitation 
	# Quantitative Sex Differences & Qualitative Sex Differences for A
	# Male and female Cholesky paths, and male- OR female-specific A paths to be estimated
	nVar = length(selDVs)/2 # number of variables
	# Starting Values
	svMe = colMeans(mzmData[, 1:nVar], na.rm = TRUE) # c(5,8,4,4,8) # start value for means
	laMe  = paste0( selDVs, "_Mean")
	svPaD = vech(diag(.2, nVar, nVar))  # start values for diagonal of covariance matrix
	svPeD = vech(diag(.8, nVar, nVar))  # start values for diagonal of covariance matrix
	lbPaD = diag(.0001, nVar, nVar)   # lower bounds for diagonal of covariance matrix
	lbPaD[lower.tri(lbPaD)] = -10     # lower bounds for below diagonal elements
	lbPaD[upper.tri(lbPaD)] = NA      # lower bounds for above diagonal elements

	# Algebra to Constrain Correlation Matrices to be Positive Definite
	# mean-starts computed
	svMe = colMeans(mzmData[,1:nVar], na.rm = TRUE) # c(5,8,4,4,8) # start value for means
	# dimnames for Algebras generated to hold Parameter Estimates and Derived Variance Components
	colZm = paste0(varList , rep(c('Am', 'Cm', 'Em'), each = nVar))
	colZf = paste0(varList , rep(c('Af', 'Cf', 'Ef'), each = nVar))
	colSZm = paste0(varList, rep(c('Asm','Csm')     , each = nVar))
	colSZf = paste0(varList, rep(c('Asf','Csf')     , each = nVar))

	model = mxModel(name,
		mxModel("top",
			# Matrices a, c, and e to store Path Coefficients
			# Male & female parameters <- am cm em, Ram, Rcm, Rem, Am, Cm, Em, Vm, VarsZm, CorsZm
			# Female parameters (parsZf) = Af, Cf, Ef, Raf, Rcf, Ref, Af, Cf, Ef, Vf, VarsZf, CorsZf
			mxMatrix(name = "am", "Lower", nrow = nVar, free = TRUE, values = svPaD, lbound = lbPaD),
			mxMatrix(name = "cm", "Lower", nrow = nVar, free = TRUE, values = svPaD, lbound = lbPaD),
			mxMatrix(name = "em", "Lower", nrow = nVar, free = TRUE, values = svPeD, lbound = lbPaD),
			mxMatrix(name = "af", "Lower", nrow = nVar, free = TRUE, values = svPaD, lbound = lbPaD),
			mxMatrix(name = "cf", "Lower", nrow = nVar, free = TRUE, values = svPaD, lbound = lbPaD),
			mxMatrix(name = "ef", "Lower", nrow = nVar, free = TRUE, values = svPeD, lbound = lbPaD),

			mxMatrix(name = "asm", "Lower", nrow = nVar, ncol = nVar, free = TRUE , values = 0, lbound = lbPaD),
			mxMatrix(name = "asf", "Lower", nrow = nVar, ncol = nVar, free = FALSE, values = 0, lbound = lbPaD),
			mxMatrix(name = "csm", "Lower", nrow = nVar, ncol = nVar, free = FALSE, values = 0, lbound = lbPaD),
			mxMatrix(name = "csf", "Lower", nrow = nVar, ncol = nVar, free = FALSE, values = 0, lbound = lbPaD),

			# Matrices A, C, and E compute variance components
			mxAlgebra(name = "Am" , am %*% t(am)),
			mxAlgebra(name = "Cm" , cm %*% t(cm)),
			mxAlgebra(name = "Em" , em %*% t(em)),
			mxAlgebra(name = "Af" , af %*% t(af)),
			mxAlgebra(name = "Cf" , cf %*% t(cf)),
			mxAlgebra(name = "Ef" , ef %*% t(ef)),
			mxAlgebra(name = "Amf", am %*% t(af)),
			mxAlgebra(name = "Cmf", cm %*% t(cf)),

			# Sex-specific effects to test Qualitative Sex Effects
			mxAlgebra(name = "Asm", asm %*% t(asm)),
			mxAlgebra(name = "Asf", asf %*% t(asf)),
			mxAlgebra(name = "Csm", csm %*% t(csm)),
			mxAlgebra(name = "Csf", csf %*% t(csf)),

			# Algebra to compute total variances and standard deviations (diagonal only)
			mxAlgebra(name = "Vm", Am + Cm + Em + Asm + Csm),
			mxAlgebra(name = "Vf", Af + Cf + Ef + Asf + Csf),
			mxMatrix(name  = "I", "Iden", nrow = nVar),
			mxAlgebra(name = "iSDm", solve(sqrt(I * Vm))),
			mxAlgebra(name = "iSDf", solve(sqrt(I * Vf))),
		
			# Matrices and Algebra to compute Genetic and Environmental Correlations
			mxMatrix(name = "I"  , "Iden", nrow = nVar),
			mxMatrix(name = "Z"  , "Zero", nrow = (nVar - 1), ncol = 1),
			mxMatrix(name = "Im1", "Iden", nrow = (nVar - 1), ncol = (nVar - 1)),
			mxAlgebra(name = "ZI", cbind(Z, Im1)),
			mxAlgebra(name = "IZ", cbind(Im1, Z)),
			mxAlgebra(name = "rAm", ZI %*% (solve(sqrt(I * Am)) %&% Am) %*% t(IZ)),
			mxAlgebra(name = "rCm", ZI %*% (solve(sqrt(I * Cm)) %&% Cm) %*% t(IZ)),
			mxAlgebra(name = "rEm", ZI %*% (solve(sqrt(I * Em)) %&% Em) %*% t(IZ)),
			mxAlgebra(name = "rAf", ZI %*% (solve(sqrt(I * Af)) %&% Af) %*% t(IZ)),
			mxAlgebra(name = "rCf", ZI %*% (solve(sqrt(I * Cf)) %&% Cf) %*% t(IZ)),
			mxAlgebra(name = "rEf", ZI %*% (solve(sqrt(I * Ef)) %&% Ef) %*% t(IZ)),

			mxConstraint(name = "constRa", vech(rAf) == vech(rAm)),
			mxConstraint(name = "constRc", vech(rCf) == vech(rCm)),
			mxConstraint(name = "constRe", vech(rEf) == vech(rEm)),

			# Matrix & Algebra for expected Mean Matrices in MZ & DZ twins
			mxMatrix(name = "expMeanGm", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(varList, "Mm")),
			mxMatrix(name = "expMeanGf", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(varList, "Mf")),
			mxMatrix(name = "expMeanGo", "Full", nrow = 1, ncol = nVar*2, free = TRUE, values = svMe, labels = paste0(varList, rep(c("Mm","Mf"), each = nVar))),

			# Algebras generated to hold Parameter Estimates and Derived Variance Components
			mxAlgebra(name = "VarsZm", cbind(Am/Vm,Cm/Vm,Em/Vm), dimnames = list(NULL, colZm)),
			mxAlgebra(name = "VarsZf", cbind(Af/Vf,Cf/Vf,Ef/Vf), dimnames = list(NULL, colZf)),
			mxAlgebra(name = "VarSZm", cbind(Asm/Vm,Csm/Vm), dimnames = list(NULL, colSZm)),
			mxAlgebra(name = "VarSZf", cbind(Asf/Vf,Csf/Vf), dimnames = list(NULL, colSZf)),
			mxAlgebra(name = "CorsZm", cbind(solve(sqrt(I*(Am+Asm))) %&% (Am+Asm),solve(sqrt(I*(Cm+Csm))) %&% (Cm+Csm),solve(sqrt(I*Em)) %&% Em), dimnames = list(NULL, colZm)),
			mxAlgebra(name = "CorsZf", cbind(solve(sqrt(I*(Af+Asf))) %&% (Af+Asf),solve(sqrt(I*(Cf+Csf))) %&% (Cf+Csf),solve(sqrt(I*Ef)) %&% Ef), dimnames = list(NULL, colZf)),

			# Matrix & Algebra for expected Variance/Covariance Matrices in MZ & DZ twins
			mxAlgebra(name = "expCovMZm", rbind(cbind(Vm, Am + Cm + Asm + Csm), cbind(Am + Cm + Asm + Csm, Vm))),
			mxAlgebra(name = "expCovDZm", rbind(cbind(Vm, 0.5 %x% (Am + Asm) + Cm + Csm), cbind(0.5 %x% (Am + Asm) + Cm + Csm, Vm))),
			mxAlgebra(name = "expCovMZf", rbind(cbind(Vf, Af + Cf + Asf + Csf), cbind(Af + Cf + Asf + Csf, Vf))),
			mxAlgebra(name = "expCovDZf", rbind(cbind(Vf, 0.5 %x% (Af + Asf) + Cf + Csf), cbind(0.5 %x% (Af + Asf) + Cf + Csf, Vf))),
			mxAlgebra(name = "expCovDZo", rbind(cbind(Vm, 0.5 %x% Amf + Cmf), cbind(0.5 %x% t(Amf) + t(Cmf), Vf)))
		),

		mxModel("MZm", mxData(mzmData, type = "raw"),
			mxExpectationNormal("top.expCovMZm", means = "top.expMeanGm", dimnames =  selDVs),
			mxFitFunctionML()
		),
		mxModel("DZm", mxData(dzmData, type = "raw"),
			mxExpectationNormal("top.expCovDZm", means = "top.expMeanGm", dimnames =  selDVs),
			mxFitFunctionML()
		),
		mxModel("MZf", mxData(mzfData, type = "raw"),
			mxExpectationNormal("top.expCovMZf", means = "top.expMeanGf", dimnames =  selDVs),
			mxFitFunctionML()
		),
		mxModel("DZf", mxData(dzfData, type = "raw"),
			mxExpectationNormal("top.expCovDZf", means = "top.expMeanGf", dimnames =  selDVs),
			mxFitFunctionML()
		),
		mxModel("DZo", mxData(dzoData, type = "raw"),
			mxExpectationNormal("top.expCovDZo", means = "top.expMeanGo", dimnames =  selDVs),
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZf", "DZf", "MZm", "DZm", "DZo"))
	)
	model = umxLabel(model)
	if(autoRun){
		return(mxRun(model))
	} else {
		return(model)
	}
}

# ========================================
# = Model building and modifying helpers =
# ========================================

#' umxValues
#'
#' For models to be estimated, it is essential that path values start at credible values. umxValues takes on that task for you.
#' umxValues can set start values for the free parameters in both RAM and Matrix \code{\link{mxModel}}s. It can also take an mxMatrix as input.
#' It tries to be smart in guessing starts from the values in your data and the model type.
#' note: If you give it a numeric input, it will use obj as the mean, return a list of length n, with sd = sd
#'
#' @param obj The RAM or matrix \code{\link{mxModel}}, or \code{\link{mxMatrix}} that you want to set start values for.
#' @param sd Optional Standard Deviation for start values
#' @param n  Optional Mean for start values
#' @param onlyTouchZeros Don't alter parameters that appear to have already been started (useful for speeding \code{\link{umxModify}})
#' @return - \code{\link{mxModel}} with updated start values
#' @export
#' @seealso - Core functions:
#' @family Model Building Functions
#' @references - \url{http://www.github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' mxEval(S, m1) # default variances are 0
#' m1 = umxValues(m1)
#' mxEval(S, m1) # plausible variances
#' umx_print(mxEval(S,m1), 3, zero.print = ".") # plausible variances
#' umxValues(14, sd = 1, n = 10) # Return vector of length 10, with mean 14 and sd 1
#' # todo: handle complex guided matrix value starts...
umxValues <- function(obj = NA, sd = NA, n = 1, onlyTouchZeros = FALSE) {
	if(is.numeric(obj) ) {
		# Use obj as the mean, return a list of length n, with sd = sd
		return(xmu_start_value_list(mean = obj, sd = sd, n = n))
	} else if (umx_is_MxMatrix(obj) ) {
		message("TODO put values into a matrix.")
	} else if (umx_is_RAM(obj) ) {
		# This is a RAM Model: Set sane starting values
		# Means at manifest means
		# S at variance on diag, quite a bit less than cov off diag
		# TODO: Start latent means?...
		# TODO: Handle sub models...
		if (length(obj$submodels) > 0) {
			stop("Cannot yet handle submodels")
		}
		if (is.null(obj$data)) {
			stop("'model' does not contain any data")
		}
		if(!is.null(obj$matrices$Thresholds)){
			message("This is a threshold RAM model... I'm not sure how to handle setting values in these yet")
			return(obj)
		}
		theData   = obj$data$observed
		manifests = obj@manifestVars
		latents   = obj@latentVars
		nVar      = length(manifests)

		if(length(latents) > 0){
			lats  =  (nVar+1):(nVar + length(latents))
			# The diagonal is variances
			if(onlyTouchZeros) {
				freePaths = (obj$matrices$S$free[lats, lats] == TRUE) & obj$matrices$S$values[lats, lats] == 0
			} else {
				freePaths = (obj$matrices$S$free[lats, lats] == TRUE)			
			}
			obj$S$values[lats, lats][freePaths] = 1
			offDiag = !diag(length(latents))
			newOffDiags = obj$matrices$S$values[lats, lats][offDiag & freePaths]/3
			obj$S@values[lats, lats][offDiag & freePaths] = newOffDiags			
		}
		# =============
		# = Set means =
		# =============
		if(obj$data$type == "raw"){
			# = Set the means =
			if(is.null(obj$matrices$M)){
				warning("You are using raw data, but have not yet added paths for the means\n")
				stop("You do this with mxPath(from = 'one', to = 'var')")
			} else {
				dataMeans = umx_means(theData[, manifests], ordVar = 0, na.rm = TRUE)
				freeManifestMeans = (obj$matrices$M$free[1, manifests] == TRUE)
				obj$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
				# covData = cov(theData, )
				covData = umx_cov_diag(theData[, manifests], ordVar = 1, format = "diag", use = "pairwise.complete.obs")
				covData = diag(covData)
			}
		} else {
			covData = diag(diag(theData))
		}
		# ======================================================
		# = Fill the symmetrical matrix with good start values =
		# ======================================================
		# The diagonal is variances
		if(onlyTouchZeros) {
			freePaths = (obj$S$free[1:nVar, 1:nVar] == TRUE) & obj$S$values[1:nVar, 1:nVar] == 0
		} else {
			freePaths = (obj$S$free[1:nVar, 1:nVar] == TRUE)			
		}
		obj$S@values[1:nVar, 1:nVar][freePaths] = covData[freePaths]
		# ================
		# = set off diag =
		# ================
		# TODO decide whether to leave this as independence, or set to non-zero covariances...
		# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
		# obj$matrices$S$values[1:nVar, 1:nVar][freePaths] = (covData[freePaths]/2)
		# offDiag = !diag(nVar)
		# newOffDiags = obj$matrices$S$values[1:nVar, 1:nVar][offDiag & freePaths]/3
		# obj$matrices$S$values[1:nVar, 1:nVar][offDiag & freePaths] = newOffDiags

		# ==========================================
		# = Put modest starts into the asymmetrics =
		# ==========================================
		Arows = nrow(obj$matrices$A$free)
		Acols = ncol(obj$matrices$A$free)
		if(onlyTouchZeros) {
			freePaths = (obj$matrices$A$free[1:Arows, 1:Acols] == TRUE) & obj$matrices$A$values[1:Arows, 1:Acols] == 0
		} else {
			freePaths = (obj$matrices$A$free[1:Arows, 1:Acols] == TRUE)			
		}
		obj$A@values[1:Arows, 1:Acols][freePaths] = .9
		return(obj)
	} else {
		stop("'obj' must be an mxMatrix, a RAM model, or a simple number")
	}
}

#' umxLabel
#'
#' umxLabel adds labels to things, be it an: \code{\link{mxModel}} (RAM or matrix based), an \code{\link{mxPath}}, or an \code{\link{mxMatrix}}
#' This is a core function in umx: Adding labels to paths opens the door to \code{\link{umxEquate}}, as well as \code{\link{omxSetParameters}}
#'
#' @param obj An \code{\link{mxModel}} (RAM or matrix based), \code{\link{mxPath}}, or \code{\link{mxMatrix}}
#' @param suffix String to append to each label (might be used to distinguish, say male and female submodels in a model)
#' @param baseName String to prepend to labels. Defaults to NA ("")
#' @param setfree Whether to label only the free paths (defaults to FALSE)
#' @param drop The value to fix "drop" paths to (defaults to 0)
#' @param jiggle How much to jiggle values in a matrix or list of path values
#' @param labelFixedCells = TRUE
#' @param boundDiag Whether to bound the diagonal of a matrix
#' @param verbose How much feedback to give the user (default = FALSE)
#' @param overRideExisting = FALSE
#' 
#' @return - \code{\link{mxModel}}
#' @export
#' @family Model Building Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umxGetParameters(m1) # Default "matrix address" labels, i.e "One Factor.S[2,2]"
#' m1 = umxLabel(m1)
#' umxGetParameters(m1, free = TRUE) # Informative labels: "G_to_x1", "x4_with_x4", etc.
#' # Labeling a matrix
#' a = umxLabel(mxMatrix(name = "a", "Full", 3, 3, values = 1:9))
#' a$labels
#' # labels with "data." in the name are left alone
#' a = mxMatrix(name = "a", "Full", 1,3, labels = c("data.a", "test", NA))
#' umxLabel(a, verbose = TRUE)
#' umxLabel(a, verbose = TRUE, overRideExisting = FALSE)
#' umxLabel(a, verbose = TRUE, overRideExisting = TRUE)
#' umxLabel(a, verbose = TRUE, overRideExisting = TRUE)
umxLabel <- function(obj, suffix = "", baseName = NA, setfree = FALSE, drop = 0, labelFixedCells = TRUE, jiggle = NA, boundDiag = NA, verbose = FALSE, overRideExisting = FALSE) {	
	# TODO umxLabel: change these to an S3 method with three classes...
	# TODO umxLabel: test that arguments not used by a particular class are not set away from their defaults
	# TODO umxLabel: perhaps make "A_with_A" --> "var_A"
	# TODO umxLabel: perhaps make "one_to_x2" --> "mean_x2" best left as is
	if (is(obj, "MxMatrix") ) { 
		# Label an mxMatrix
		xmuLabel_Matrix(mx_matrix = obj, baseName = baseName, setfree = setfree, drop = drop, labelFixedCells = labelFixedCells, jiggle = jiggle, boundDiag = boundDiag, suffix = suffix, verbose = verbose, overRideExisting = overRideExisting)
	} else if (umx_is_RAM(obj)) { 
		# Label a RAM model
		if(verbose){message("RAM")}
		return(xmuLabel_RAM_Model(model = obj, suffix = suffix, labelFixedCells = labelFixedCells, overRideExisting = overRideExisting, verbose = verbose))
	} else if (umx_is_MxModel(obj) ) {
		# Label a non-RAM matrix lamodel
		return(xmuLabel_MATRIX_Model(model = obj, suffix = suffix, verbose = verbose))
	} else {
		stop("I can only label OpenMx models and mxMatrix types. You gave me a ", typeof(obj))
	}
}

# =================================
# = Run Helpers =
# =================================

#' umxRun
#'
#' umxRun is a version of \code{\link{mxRun}} which can run also set start values, labels, and run multiple times
#' It can also calculate the saturated and independence likelihoods necessary for most fit indices.
#'
#' @param model The \code{\link{mxModel}} you wish to run.
#' @param n The maximum number of times you want to run the model trying to get a code green run (defaults to 1)
#' @param calc_SE Whether to calculate standard errors (not used when n = 1)
#' for the summary (they are not very accurate, so if you use \code{\link{mxCI}} or \code{\link{umxCI}}, you can turn this off)
#' @param calc_sat Whether to calculate the saturated and independence models (for raw \code{\link{mxData}} \code{\link{mxModel}}s) (defaults to TRUE - why would you want anything else?)
#' @param setValues Whether to set the starting values of free parameters (defaults to F)
#' @param setLabels Whether to set the labels (defaults to F)
#' @param intervals Whether to run mxCI confindence intervals (defaults to F)
#' @param comparison Whether to run umxCompare() after umxRun
#' @param setStarts Deprecated way to setValues
#' @return - \code{\link{mxModel}}
#' @family Model Building Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1) # just run: will create saturated model if needed
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE) # set start values and label all parameters
#' umxSummary(m1, show = "std")
#' m1 = mxModel(m1, mxCI("G_to_x1")) # add one CI
#' m1 = mxRun(m1, intervals = TRUE)
#' residuals(m1, run = TRUE) # get CIs on all free parameters
#' confint(m1, run = TRUE) # get CIs on all free parameters
#' m1 = umxRun(m1, n = 10) # re-run up to 10 times if not green on first run
umxRun <- function(model, n = 1, calc_SE = TRUE, calc_sat = TRUE, setValues = FALSE, setLabels = FALSE, intervals = FALSE, comparison = NULL, setStarts = NULL){
	# TODO: return change in -2LL for models being re-run
	# TODO: stash saturated model for re-use
	# TODO: Optimise for speed
	if(!is.null(setStarts)){
		message("change setStarts to setValues (more consistent)")
		setValues = setStarts
	}
	if(setLabels){
		model = umxLabel(model)
	}
	if(setValues){
		model = umxValues(model)
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
			# If we have a RAM model with raw data, compute the satuated and indpendence models
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

#' umxModify
#' 
#' umxModify allows you to modify, re-run and summarize an \code{\link{mxModel}},
#' all in one line of script. 
#' You can add paths, or other model elements, set paths or drop them.
#' As an example, this one-liner drops a path labelled "Cs", and returns the updated model:
#' 
#' \code{fit2 = umxModify(fit1, update = "Cs", name = "newModelName", comparison = TRUE)}
#' 
#' Regular expressions are a powerful feature: they let you drop collections of paths by matching patterns
#' fit2 = umxModify(fit1, regex = "C[sr]", name = "drop_Cs_andCr", comparison = TRUE)
#' 
#' If you are just starting out, you might find it easier to be more explicit. Like this: 
#' 
#' fit2 = omxSetParameters(fit1, labels = "Cs", values = 0, free = FALSE, name = "newModelName")
#' fit2 = mxRun(fit2)
#' summary(fit2)
#' 
#' @aliases umxReRun umxModify
#' @param lastFit  The \code{\link{mxModel}} you wish to update and run.
#' @param update What to update before re-running. Can be a list of labels, a regular expression (set regex = TRUE) or an object such as mxCI etc.
#' @param regex    Whether or not update is a regular expression (defaults to FALSE). If you provide a string, it
#' over-rides the contents of update, and sets regex to TRUE.
#' @param free     The state to set "free" to for the parameters whose labels you specify (defaults to free = FALSE, i.e., fixed)
#' @param value    The value to set the parameters whose labels you specify too (defaults to 0)
#' @param freeToStart Whether to update parameters based on their current free-state. free = c(TRUE, FALSE, NA), (defaults to NA - i.e, not checked)
#' @param name      The name for the new model
#' @param verbose   How much feedback to give
#' @param intervals Whether to run confidence intervals (see \code{\link{mxRun}})
#' @param comparison Whether to run umxCompare() after umxRun
#' @param dropList (deprecated: use 'update' instead.
#' @return - \code{\link{mxModel}}
#' @family Model Building Functions
#' @references - \url{http://github.com/tbates/umx}
#' @export
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = umxModify(m1, update = "G_to_x1", name = "drop_X1")
#' umxSummary(m2); umxCompare(m1, m2)
#' # 1-line version including comparison
#' m2 = umxModify(m1, update = "G_to_x1", name = "drop_X1", comparison = TRUE)
#' m2 = umxModify(m1, update = "^G_to_x[3-5]", regex = TRUE, name = "no_G_to_x3_5", comp = TRUE)
#' m2 = umxModify(m1, regex = "^G_to_x[3-5]", name = "no_G_to_x3_5") # same, but shorter
#' m2 = umxModify(m1, update = "G_to_x1", value = .2, name = "fix_G_x1_at_point2", comp = TRUE)
#' m3 = umxModify(m2, update = "G_to_x1", free = TRUE, name = "free_G_x1_again", comparison = TRUE)
umxModify <- function(lastFit, update = NULL, regex = FALSE, free = FALSE, value = 0, freeToStart = NA, name = NULL, verbose = FALSE, intervals = FALSE, comparison = FALSE, dropList = "deprecated") {
	if(dropList != "deprecated"){
		stop("hi. Sorry for the change, but please replace ", omxQuotes("dropList"), " with ", omxQuotes("update"),". e.g.:\n",
			"umxModify(m1, dropList = ", omxQuotes("E_to_heartRate"), ")\n",
			"becomes\n",
			"umxModify(m1, update = ", omxQuotes("E_to_heartRate"), ")\n",
			 "\nThis regular expression will do it for you:\n",
			 "find    = regex *= *(\\\"[^\\\"]+\\\"),\n",
			 "replace = update = $1, regex = TRUE,"
		)
	}
	if (typeof(regex) != "logical"){
		# Use the regex as input, and switch to regex mode
		if(!is.null(update)){
			stop("If you input a regular expression in ", omxQuotes("regex"), " you must leave ", omxQuotes("update"), " set to NULL.")
		}
		update = regex
		regex = TRUE
	}

	if (typeof(free) != "logical"){
		# Use the free as input, and switch to free = TRUE
		update = free
		free = TRUE
	}

	if(is.null(update)){
		message("As you haven't asked to do anything: the parameters that are free to be dropped are:")
		print(umxGetParameters(lastFit))
		stop()
	}else{
		if(regex | typeof(update) == "character") {
			if (regex) {
				theLabels = umxGetParameters(lastFit, regex = update, free = freeToStart, verbose = verbose)
			}else {
				theLabels = update
			}
			x = omxSetParameters(lastFit, labels = theLabels, free = free, values = value, name = name)		
		} else {
			# TODO Label and start new object
			if(is.null(name)){ name = NA }
			x = mxModel(lastFit, update, name = name)
		}
		x = mxRun(x, intervals = intervals)
		if(comparison){
			umxSummary(x)			
			if(free){ # new model has fewer df
				umxCompare(x, lastFit)
			} else {
				umxCompare(lastFit, x)
			}
		}
		return(x)
	}
}

#' @export
umxReRun <- umxModify

# ==============================
# = Label and equate functions =
# ==============================

#' umxGetParameters
#'
#' Get the parameter labels from a model. Like \code{\link{omxGetParameters}},
#' but supercharged with regular expressions for more power and ease!
#'
#' @param inputTarget An object to get parameters from: could be a RAM \code{\link{mxModel}}
#' @param regex A regular expression to filter the labels defaults to NA - just returns all labels)
#' @param free  A Boolean determining whether to return only free parameters.
#' @param verbose How much feedback to give
#' @export
#' @family Modify or Compare Models
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1)
#' umxGetParameters(m1)
#' m1 = umxRun(m1, setLabels = TRUE)
#' umxGetParameters(m1)
#' umxGetParameters(m1, free = TRUE) # only the free parameter
#' umxGetParameters(m1, free = FALSE) # only parameters which are fixed
#' \dontrun{
#' # Complex regex patterns
#' umxGetParameters(m2, regex = "S_r_[0-9]c_6", free = TRUE) # Column 6 of matrix "as"
#' }
umxGetParameters <- function(inputTarget, regex = NA, free = NA, verbose = FALSE) {
	# TODO
	# 1. Be nice to offer a method to handle submodels
	# 	model$submodels$aSubmodel$matrices$aMatrix$labels
	# 	model$submodels$MZ$matrices
	# 2. Simplify handling
		# allow umxGetParameters to function like omxGetParameters()[name filter]
	# 3. All user to request values, free, etc.
	if(umx_is_MxModel(inputTarget)) {
		topLabels = names(omxGetParameters(inputTarget, indep = FALSE, free = free))
	} else if(methods::is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget$labels
		} else {
			topLabels = inputTarget$labels[inputTarget$free==free]
		}
	}else{
		stop("I am sorry Dave, umxGetParameters needs either a model or an mxMatrix: you offered a ", class(inputTarget)[1])
	}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( length(regex) > 1 || !is.na(regex) ) {
		if(length(regex) > 1){
			# assume regex is a list of labels
			theLabels = theLabels[theLabels %in% regex]
			if(length(regex) != length(theLabels)){
				msg = "Not all labels found! Missing were:\n"
				stop(msg, regex[!(regex %in% theLabels)]);
			}
		} else {
			# it's a grep string
			if(length(grep("[\\.\\*\\[\\(\\+\\|^]+", regex) ) < 1){ # no grep found: add some anchors for safety
				regex = paste0("^", regex, "[0-9]*$"); # anchor to the start of the string
				anchored = TRUE
				if(verbose == TRUE) {
					message("note: anchored regex to beginning of string and allowed only numeric follow\n");
				}
			}else{
				anchored = FALSE
			}
			theLabels = grep(regex, theLabels, perl = FALSE, value = TRUE) # return more detail
		}
		if(length(theLabels) == 0){
			msg = "Found no matching labels!\n"
			if(anchored == TRUE){
				msg = paste0(msg, "note: anchored regex to beginning of string and allowed only numeric follow:\"", regex, "\"")
			}
			if(umx_is_MxModel(inputTarget)){
				msg = paste0(msg, "\nUse umxGetParameters(", deparse(substitute(inputTarget)), ") to see all parameters in the model")
			}else{
				msg = paste0(msg, "\nUse umxGetParameters() without a pattern to see all parameters in the model")
			}
			stop(msg);
		}
	}
	return(theLabels)
}

#' @rdname umxGetParameters
#' @export
parameters <- umxGetParameters

#' umxSetParameters
#'
#' Free or fix parameters in an \code{\link{mxModel}}.
#' This allows similar actions that \code{\link{update}} enables
#' for lm models.
#' Updating can create duplicate labels, so this function also calls \code{\link{omxAssignFirstParameters}}
#' to equate the start values for parameters which now have identical labels.
#' 
#' It also supports regular expressions to select labels. In this respect,
#' it is similar to \code{\link{umxModify}} without running the model.
#' 
#' @param model an \code{\link{mxModel}} to WITH
#' @param labels = labels to find
#' @param free = new value for free
#' @param values = new values
#' @param newlabels = newlabels
#' @param lbound = value for lbound
#' @param ubound = value for ubound
#' @param indep = whether to look in indep models
#' @param strict whether to complain if labels not found
#' @param name = new name for the returned model
#' @param regex Is labels a regular expression (defaults to FALSE)
#' @param test just show what you would do? (defaults to FALSE)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Modify or Compare Models
#' @seealso - \code{\link{umxModify}}, \code{\link{umxLabel}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(demoOneFactor[1:80,], type = "raw"),
#' 	umxPath(from = latents, to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = latents)
#' )
#' parameters(m1, free=TRUE)
#' m2 = umxSetParameters(m1, "G_to_x1", newlabels= "G_to_x2", test = FALSE)
#' m2 = umxSetParameters(m1, "^", newlabels= "m1_", regex = TRUE, test = TRUE)
umxSetParameters <- function(model, labels, free = NULL, values = NULL, newlabels = NULL, lbound = NULL, ubound = NULL, indep = FALSE, strict = TRUE, name = NULL, regex = FALSE, test = FALSE) {
	# TODO Add update() S3 function?
	nothingDoing = all(is.null(c(free, values, newlabels)))
	if(nothingDoing){
		warning("You are not setting anything: set one or more of free, values, or newLabels to update a parameter")
	}
	if(regex){
		oldLabels = umxGetParameters(model, regex = labels)
		if(!is.null(newlabels)){
			newlabels = gsub(labels, newlabels, oldLabels, ignore.case = F)
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

#' umxEquate
#'
#' In addition to dropping or adding parameters, a second common task in modelling
#' is to equate parameters. umx provides a convenience function to equate parameters 
#' by setting one or more parameters (the "slave" set) equal to one or more "master" 
#' parameters. These parameters are picked out via their labels, and setting two or more
#' parameters to have the same value is accomplished by setting the slave(s) to have
#' the same label(s) as the master parameters, thus constraining them to take the same
#' value during model fitting.
#' 
#' \emph{note}: In addition to using this method to equating parameters, you can
#' also equate one parameter to another by setting its label to the 
#' "square bracket" address of the master, e.g. "a[r,c]".
#' 
#' Tip: To find labels of free parameters use \code{\link{umxGetParameters}} with free = T
#' Tip: To find labels by name, use the regex parameter of \code{\link{umxGetParameters}}
#' 
#' @param model   An \code{\link{mxModel}} within which to equate parameters
#' @param master  A list of "master" labels to which slave labels will be equated
#' @param slave   A list of slave labels which will be updated to match master labels, thus equating the parameters
#' @param free    Should parameter(s) initally be free? (default = TRUE)
#' @param verbose Whether to give verbose feedback (default = TRUE)
#' @param name    name for the returned model (optional: Leave empty to leave name unchanged)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Modify or Compare Models
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' m2 = umxEquate(m1, master = "G_to_x1", slave = "G_to_x2", name = "Equate x1 and x2 loadings")
#' m2 = mxRun(m2) # have to run the model again...
#' umxCompare(m1, m2) # not good :-)
umxEquate <- function(model, master, slave, free = c(TRUE, FALSE, NA), verbose = TRUE, name = NULL) {	
	free = umx_default_option(free, c(TRUE, FALSE, NA))
	if(!umx_is_MxModel(model)){
		message("ERROR in umxEquate: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxEquate(model, master=\"a_to_b\", slave=\"a_to_c\", name=\"model2\") # equate paths a->b and a->c, in a new model called \"model2\"")
		stop()
	}
	if(length(grep("[\\^\\.\\*\\[\\(\\+\\|]+", master) ) < 1){ # no grep found: add some anchors
		master = paste0("^", master, "$"); # anchor to the start of the string
		slave  = paste0("^", slave,  "$");
		if(verbose == TRUE){
			cat("note: matching whole label\n");
		}
	}
	masterLabels = umxGetParameters(model, regex = master, free = free, verbose = verbose)
	slaveLabels  = umxGetParameters(model, regex = slave , free = free, verbose = verbose)
	if( length(slaveLabels) != length(masterLabels) && (length(masterLabels)!=1)) {
		print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
		stop("ERROR in umxEquate: master and slave labels not the same length!\n",
		length(slaveLabels), " slavelabels found, and ", length(masterLabels), " masters")
	}
	if(length(slaveLabels) == 0) {
		legal = names(omxGetParameters(model, indep=FALSE, free=free))
		legal = legal[which(!is.na(legal))]
		message("Labels available in model are: ", paste(legal, ", "))
		stop("ERROR in umxEquate: no slave labels found or none requested!")
	}
	print(list(masterLabels = masterLabels, slaveLabels = slaveLabels))
	model = omxSetParameters(model = model, labels = slaveLabels, newlabels = masterLabels, name = name)
	model = omxAssignFirstParameters(model, indep = FALSE)
	return(model)
}

#' umxFixAll
#'
#' Fix all free parameters in a model using omxGetParameters()
#'
#' @param model an \code{\link{mxModel}} within which to fix free parameters
#' @param verbose whether to mention how many paths were fixed (default is FALSE)
#' @param name optional new name for the model. if you begin with a _ it will be made a suffix
#' @param run  whether to fix and re-run the model, or just return it (defaults to FALSE)
#' @return - the fixed \code{\link{mxModel}}
#' @export
#' @family Modify or Compare Models
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("OneFactor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' m2 = umxFixAll(m1, run = TRUE, verbose = TRUE)
#' mxCompare(m1, m2)
umxFixAll <- function(model, name = "_fixed", run = FALSE, verbose= FALSE){
	if(!umx_is_MxModel(model)){
		message("ERROR in umxFixAll: model must be a model, you gave me a ", class(model)[1])
		message("A usage example is umxFixAll(model)")
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

#' umxDrop1
#'
#' Drops each free parameter (selected via regex), returning an \code{\link{mxCompare}}
#' table comparing the effects. A great way to quickly determine which of several 
#' parameters can be dropped without excessive cost
#'
#' @param model An \code{\link{mxModel}} to drop parameters from 
#' @param regex A string to select parameters to drop. leave empty to try all.
#' This is regular expression enabled. i.e., "^a_" will drop parameters beginning with "a_"
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of model comparisons
#' @export
#' @family Modify or Compare Models
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umxDrop1(fit3) # try dropping each free parameters (default)  
#' # drop "a_r1c1" and "a_r1c2" and see which matters more.
#' umxDrop1(model, regex="a_r1c1|a_r1c2")
#' }
umxDrop1 <- function(model, regex = NULL, maxP = 1) {
	if(is.null(regex)) {
		toDrop = umxGetParameters(model, free = TRUE)
	} else if (length(regex) > 1) {
		toDrop = regex
	} else {
		toDrop = grep(regex, umxGetParameters(model, free = TRUE), value = TRUE, ignore.case = TRUE)
	}
	message("Will drop each of ", length(toDrop), " parameters: ", paste(toDrop, collapse = ", "), ".\nThis might take some time...")
	out = list(rep(NA, length(toDrop)))
	for(i in seq_along(toDrop)){
		tryCatch({
			message("item ", i, " of ", length(toDrop))
        	out[i] = umxModify(model, name = paste0("drop_", toDrop[i]), regex = toDrop[i])
		}, warning = function(w) {
			message("Warning incurred trying to drop ", toDrop[i])
			message(w)
		}, error = function(e) {
			message("Error occurred trying to drop ", toDrop[i])
			message(e)
		})
	}
	out = data.frame(umxCompare(model, out))
	out[out=="NA"] = NA
	suppressWarnings({
		out$p   = as.numeric(out$p) 
		out$AIC = as.numeric(out$AIC)
	})
	n_row = dim(out)[1] # 2 9
	sortedOrder = order(out$p[2:n_row])+1
	out[2:n_row, ] <- out[sortedOrder, ]
	good_rows = out$p < maxP
	good_rows[1] = T
	message(sum(good_rows)-1, "of ", length(out$p)-1, " items were beneath your p-threshold of ", maxP)
	return(out[good_rows, ])
}

#' umxAdd1
#'
#' Add each of a set of paths you provide to the model, returning a table of theire effect on fit
#'
#' @param model an \code{\link{mxModel}} to alter
#' @param pathList1 a list of variables to generate a set of paths
#' @param pathList2 an optional second list: IF set paths will be from pathList1 to members of this list
#' @param arrows Make paths with one or two arrows
#' @param maxP The threshold for returning values (defaults to p==1 - all values)
#' @return a table of fit changes
#' @export
#' @family Modify or Compare Models
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model = umxAdd1(model)
#' }
umxAdd1 <- function(model, pathList1 = NULL, pathList2 = NULL, arrows = 2, maxP = 1) {
	# DONE: RAM paths
	# TODO add non-RAM
	if ( is.null(model$output) ) stop("Provided model hasn't been run: use mxRun(model) first")
	# stop if there is no output
	if ( length(model$output) < 1 ) stop("Provided model has no output. use mxRun() first!")

	if(arrows == 2){
		if(!is.null(pathList2)){
			a = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			b = xmuMakeTwoHeadedPathsFromPathList(pathList2)
			a_to_b = xmuMakeTwoHeadedPathsFromPathList(c(pathList1, pathList2))
			toAdd = a_to_b[!(a_to_b %in% c(a,b))]
		}else{
			if(is.null(pathList1)){
				stop("best to set pathList1!")
				# toAdd = umxGetParameters(model, free = FALSE)
			} else {
				toAdd = xmuMakeTwoHeadedPathsFromPathList(pathList1)
			}
		}
	} else if(arrows == 1){
		if(is.null(pathList2)){
			stop("pathList2 must not be empty for arrows = 1: it forms the target of each path")
		} else {
			toAdd = xmuMakeOneHeadedPathsFromPathList(pathList1, pathList2)
		}
	}else{
		stop("You idiot :-) : arrows must be either 1 or 2, you tried", arrows)
	}
	# TODO fix count? or drop giving it?
	message("You gave me ", length(pathList1), "source variables. I made ", length(toAdd), " paths from these.")

	# Just keep the ones that are not already free... (if any)
	toAdd2 = toAdd[toAdd %in% umxGetParameters(model, free = FALSE)]
	if(length(toAdd2) == 0){
		if(length(toAdd[toAdd %in% umxGetParameters(model, free = NA)] == 0)){
			message("I couldn't find any of those paths in this model.",
				"The most common cause of this error is submitting the wrong model")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}else{
			message("I found (at least some) of those paths in this model, but they were already free")
			message("You asked for: ", paste(toAdd, collapse=", "))
		}		
		stop()
	}else{
		toAdd = toAdd2
	}
	message("Of these ", length(toAdd), " were currently fixed, and I will try adding them")
	message(paste(toAdd, collapse = ", "))

	message("This might take some time...")
	flush.console()
	# out = data.frame(Base = "test", ep = 1, AIC = 1.0, p = 1.0); 
	row1Cols = c("Base", "ep", "AIC", "p")
	out = data.frame(umxCompare(model)[1, row1Cols])
	for(i in seq_along(toAdd)){
		# model = fit1 ; toAdd = c("x2_with_x1"); i=1
		message("item ", i, " of ", length(toAdd))
		tmp = omxSetParameters(model, labels = toAdd[i], free = TRUE, values = .01, name = paste0("add_", toAdd[i]))
		tmp = mxRun(tmp)
		mxc = umxCompare(tmp, model)
		newRow = mxc[2, row1Cols]
		newRow$AIC = mxc[1, "AIC"]
		out = rbind(out, newRow)
	}

	out[out=="NA"] = NA
	out$p   = round(as.numeric(out$p), 3)
	out$AIC = round(as.numeric(out$AIC), 3)
	out <- out[order(out$p),]
	if(maxP==1){
		return(out)
	} else {
		good_rows = out$p < maxP
		message(sum(good_rows, na.rm = TRUE), "of ", length(out$p), " items were beneath your p-threshold of ", maxP)
		message(sum(is.na(good_rows)), " was/were NA")
		good_rows[is.na(good_rows)] = T
		return(out[good_rows, ])
	}
}

# ===============
# = RAM Helpers =
# ===============

#' umxLatent
#'
#' Helper to ease the creation of latent variables including formative and reflective variables (see below)
#' For formative variables, the manifests define (form) the latent.
#' This function takes care of intercorrelating manifests for formatives, and fixing variances correctly
#' 
#' The following figures show how a reflective and a formative variable look as path diagrams:
#' \figure{reflective.png}
#' formative\figure{formative.png}
#'
#' @param latent the name of the latent variable (string)
#' @param formedBy the list of variables forming this latent
#' @param forms the list of variables which this latent forms (leave blank for formedBy)
#' @param data the dataframe being used in this model
#' @param type = \"exogenous|endogenous\"
#' @param name A name for the path NULL
#' @param labelSuffix a suffix string to append to each label
#' @param verbose  Default is TRUE as this function does quite a lot
#' @param endogenous This is now deprecated. use type= \"exogenous|endogenous\"
#' @return - path list
#' @export
#' @family Model Building Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' library(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor) # x1-5
#' theData = cov(demoOneFactor)
#' m1 = mxModel("reflective", type = "RAM",
#'	manifestVars = manifests,
#'	latentVars   = latents,
#'	# Factor loadings
#'	umxLatent("G", forms = manifests, type = "exogenous", data = theData),
#'	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE); umxSummary(m1, show="std")
#' plot(m1)
#' 
#' m2 = mxModel("formative", type = "RAM",
#'	manifestVars = manifests,
#'	latentVars   = latents,
#'	# Factor loadings
#'	umxLatent("G", formedBy = manifests, data = theData),
#'	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
#' )
#' m2 = umxRun(m2, setValues = TRUE, setLabels = TRUE);
#' umxSummary(m2, show = "std")
#' plot(m2)
#' }
umxLatent <- function(latent = NULL, formedBy = NULL, forms = NULL, data = NULL, type = NULL,  name = NULL, labelSuffix = "", verbose = TRUE, endogenous = "deprecated") {
	# Purpose: make a latent variable formed/or formed by some manifests
	# Use: umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
	if(!endogenous == "deprecated"){
		if(endogenous){
			stop("Error in mxLatent: Use of endogenous=T is deprecated. Remove it and replace with type = \"endogenous\"") 
		} else {
			stop("Error in mxLatent: Use of endogenous=F is deprecated. Remove it and replace with type = \"exogenous\"") 
		}
	}
	if(is.null(latent)) { stop("Error in mxLatent: you have to provide the name of the latent variable you want to create") }
	# Check both forms and formedBy are not defined
	if( is.null(formedBy) &&  is.null(forms)) { stop("Error in mxLatent: Must define one of forms or formedBy") }
	if(!is.null(formedBy) && !is.null(forms)) { stop("Error in mxLatent: Only one of forms or formedBy can be set") }
	if(is.null(data)){ stop("Error in mxLatent: you have to provide the data that will be used in the model") }
	# ==========================================================
	# = NB: If any vars are ordinal, a call to umxMakeThresholdsMatrices will be made
	isCov = umx_is_cov(data, boolean = TRUE)
	if( any(!is.null(forms))) {
		manifests <- forms
	}else{
		manifests <- formedBy
	}
	if(isCov) {
		variances = diag(data[manifests, manifests])
	} else {
		manifestOrdVars = umx_is_ordered(data[,manifests])
		if(any(manifestOrdVars)) {
			means         = rep(0, times = length(manifests))
			variances     = rep(1, times = length(manifests))
			contMeans     = colMeans(data[,manifests[!manifestOrdVars], drop = F], na.rm = TRUE)
			contVariances = diag(cov(data[,manifests[!manifestOrdVars], drop = F], use = "complete"))
			if( any(!is.null(forms)) ) {
				contVariances = contVariances * .1 # hopefully residuals are modest
			}
			means[!manifestOrdVars] = contMeans				
			variances[!manifestOrdVars] = contVariances				
		}else{
			if(verbose){
				message("No ordinal variables")
			}
			means     = colMeans(data[, manifests], na.rm = TRUE)
			variances = diag(cov(data[, manifests], use = "complete"))
		}
	}

	if( any(!is.null(forms)) ) {
		# Handle forms case
		# p1 = Residual variance on manifests
		# p2 = Fix latent variance @1
		# p3 = Add paths from latent to manifests
		p1 = mxPath(from = manifests, arrows = 2, free = TRUE, values = variances)
		if(is.null(type)){ stop("Error in mxLatent: You must set type to either exogenous or endogenous when creating a latent variable with an outgoing path") }
		if(type == "endogenous"){
			# Free latent variance so it can do more than just redirect what comes in
			if(verbose){
				message(paste("latent '", latent, "' is free (treated as a source of variance)", sep=""))
			}
			p2 = mxPath(from=latent, connect="single", arrows = 2, free = TRUE, values = .5)
		} else {
			# fix variance at 1 - no inputs
			if(verbose){
				message(paste("latent '", latent, "' has variance fixed @1"))
			}
			p2 = mxPath(from = latent, connect = "single", arrows = 2, free = FALSE, values = 1)
		}
		p3 = mxPath(from = latent, to = manifests, connect = "single", free = TRUE, values = variances)
		if(isCov) {
			# Nothing to do: covariance data don't need means...
			paths = list(p1, p2, p3)
		}else{
			# Add means: fix latent mean @0, and add freely estimated means to manifests
			p4 = mxPath(from = "one", to = latent   , arrows = 1, free = FALSE, values = 0)
			p5 = mxPath(from = "one", to = manifests, arrows = 1, free = TRUE, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	} else {
		# Handle formedBy case
		# Add paths from manifests to the latent
		p1 = mxPath(from = manifests, to = latent, connect = "single", free = TRUE, values = umxValues(.6, n=manifests)) 
		# In general, manifest variance should be left free...
		# TODO If the data were correlations... we can inspect for that, and fix the variance to 1
		p2 = mxPath(from = manifests, connect = "single", arrows = 2, free = TRUE, values = variances)
		# Allow manifests to intercorrelate
		p3 = mxPath(from = manifests, connect = "unique.bivariate", arrows = 2, free = TRUE, values = umxValues(.3, n = manifests))
		if(isCov) {
			paths = list(p1, p2, p3)
		}else{
			# Fix latent mean at 0, and freely estimate manifest means
			p4 = mxPath(from="one", to=latent   , free = FALSE, values = 0)
			p5 = mxPath(from="one", to=manifests, free = TRUE, values = means)
			paths = list(p1, p2, p3, p4, p5)
		}
	}
	if(!is.null(name)) {
		m1 <- mxModel(name, type="RAM", manifestVars = manifests, latentVars = latent, paths)
		if(isCov){
			m1 <- mxModel(m1, mxData(cov(df), type="cov", numObs = 100))
			message("\n\nIMPORTANT: you need to set numObs in the mxData() statement\n\n\n")
		} else {
			if(any(manifestOrdVars)){
				stop("Sorry, I can't yet handle ordinal manifests automatically :-(.")
				# m1 <- mxModel(m1, umxThresholdRAMObjective(data, deviationBased = TRUE, droplevels = TRUE, verbose = TRUE))
			} else {
				m1 <- mxModel(m1, mxData(data, type = "raw"))
			}
		}
		return(m1)
	} else {
		return(paths)
	}
	# TODO	shift this to a test file
	# readMeasures = paste("test", 1:3, sep="")
	# bad usages
	# mxLatent("Read") # no too defined
	# mxLatent("Read", forms=manifestsRead, formedBy=manifestsRead) #both defined
	# m1 = mxLatent("Read", formedBy = manifestsRead, model.name="base"); umxPlot(m1, std = FALSE, file="name")
	# m2 = mxLatent("Read", forms = manifestsRead, as.model="base"); 
	# m2 <- mxModel(m2, mxData(cov(df), type="cov", numObs=100))
	# plot(m2, std=FALSE, file="name")
	# mxLatent("Read", forms = manifestsRead)
}

# ===========================
# = matrix-oriented helpers =
# ===========================

#' umxThresholdMatrix
#'
#' High-level helper for ordinal modeling. Creates, labels, and sets smart-starts for this complex matrix. Big time saver!
#'
#' When modeling ordinal data (sex, low-med-hi, 
#' depressed/normal, not at all, rarely, often, always), a useful conceptual strategy to handle expectations
#' is to build a standard-normal model (i.e., a latent model with zero-means, and unit (1.0) variances),
#' and then to threshold this normal distribution to generate the observed data. Thus an observation of "depressed"
#' is modeled as a high score on the latent normally distributed trait, with thresholds set so that only scores above
#' this threshold (1-minus the number of categories).
#'
#' For \strong{deviation methods}, it returns a list of lowerOnes_for_thresh, deviations_for_thresh & thresholdsAlgebra (named threshMatName)
#'
#' For \strong{direct}, it returns a thresholdsMatrix (named threshMatName)
#'
#' @param df the data being modelled (to allow access to the factor levels and quantiles within these for each variable)
#' @param suffixes e.g. c("T1", "T2") - Use for data with repeated observations in a row (i.e., twin data) (defaults to NA)
#' @param threshMatName name of the matrix which is returned. Defaults to "threshMat" - best not to change it.
#' @param method  How to set the thresholds: auto (the default), Mehta, which fixes the first two (auto chooses this for ordinal) or "allFree" (auto chooses this for binary)
#' @param l_u_bound c(NA, NA) by default, you can use this to bound the thresholds. Careful you don't set bounds too close if you do.
#' @param deviationBased Whether to build a helper matrix to keep the thresholds in order (defaults to = TRUE)
#' @param droplevels Whether to drop levels with no observed data (defaults to FALSE)
#' @param verbose (defaults to FALSE))
#' @param hint currently used for "left_censored" data (defaults to "none"))
#' @return - thresholds matrix
#' @export
#' @family Model Building Functions
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' x = data.frame(ordered(rbinom(100,1,.5))); names(x) <- c("x")
#' umxThresholdMatrix(x)
#' x = cut(rnorm(100), breaks = c(-Inf,.2,.5, .7, Inf)); levels(x) = 1:5
#' x = data.frame(ordered(x)); names(x) <- c("x")
#' tm = umxThresholdMatrix(x)
#' 
#' # ==================
#' # = Binary example =
#' # ==================
#' require(umx)
#' data(twinData)
#' labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
#' twinData$zyg = factor(twinData$zyg, levels = 1:5, labels = labList)
#' # Cut to form category of 80 % obese subjects
#' cutPoints <- quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Step 2: Make the ordinal variables into mxFactors
#' # this ensures ordered= TRUE + requires user to confirm levels
#' selDVs = c("obese1", "obese2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' tm = umxThresholdMatrix(mzData, suffixes = 1:2, verbose = TRUE) # informative messages
#' 
#' # ======================================
#' # = Ordinal (n categories > 2) example =
#' # ======================================
#' # Cut to form three categories of weight
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.4, .7), na.rm = TRUE)
#' obesityLevels = c('normal', 'overweight', 'obese')
#' twinData$obeseTri1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseTri2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selDVs = c("obeseTri1", "obeseTri2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' tm = umxThresholdMatrix(mzData, suffixes = 1:2, verbose = TRUE)
#'
#' # ========================================================
#' # = Mix of all three kinds example (and a 4-level trait) =
#' # ========================================================
#' 
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.25, .4, .7), na.rm = TRUE)
#' obesityLevels = c('underWeight', 'normal', 'overweight', 'obese')
#' twinData$obeseQuad1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obeseQuad2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' selDVs = c("obeseQuad1", "obeseQuad2")
#' twinData[, selDVs] <- mxFactor(twinData[, selDVs], levels = obesityLevels)
#' 
#' selDVs = umx_paste_names(c("bmi", "obese", "obeseTri", "obeseQuad"), "", 1:2)
#' mzData <- subset(twinData, zyg == "MZFF", selDVs)
#' str(mzData)
#' tm = umxThresholdMatrix(mzData, suffixes = 1:2, verbose = TRUE)
#'
#' # ===================
#' # = "left_censored" =
#' # ===================
#' 
#' x = round(10 * rnorm(1000, mean = -.2))
#' x[x < 0] = 0
#' x = mxFactor(x, levels = sort(unique(x)))
#' x = data.frame(x)
#' # umxThresholdMatrix(x, deviation = FALSE, hint = "left_censored")
umxThresholdMatrix <- function(df, suffixes = NA, threshMatName = "threshMat", method = c("auto", "Mehta", "allFree"), l_u_bound = c(NA, NA), deviationBased = TRUE, droplevels = FALSE, verbose = FALSE, hint = c("none", "left_censored")){
	if(droplevels){ stop("Not sure it's wise to drop levels...") }
	hint        = match.arg(hint)
	method      = match.arg(method)
	nSib        = length(suffixes)
	isFactor    = umx_is_ordered(df) # all ordered factors including binary
	isOrd       = umx_is_ordered(df, ordinal.only = TRUE) # only ordinals
	isBin       = umx_is_ordered(df, binary.only  = TRUE) # only binaries
	nFactors    = sum(isFactor)
	nOrdVars    = sum(isOrd)
	nBinVars    = sum(isBin)
	factorVarNames = names(df)[isFactor]
	ordVarNames    = names(df)[isOrd]
	binVarNames    = names(df)[isBin]
	if((nOrdVars + nBinVars) < 1){
		message("No ordinal or binary variables in dataframe: no need to call umxThresholdMatrix")
		return(NA) # probably OK to set thresholds matrix to NA in mxExpectation()
		# TODO check if we should die here instead
	} else {
		if(verbose){
			message("'threshMat' created to handle ")
			if(nSib == 2){
				if(nOrdVars > 0){
					message(nOrdVars/nSib, " pair(s) of ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					message(nBinVars/nSib, " pair(s) of binary variables:", omxQuotes(binVarNames), "\n")
				}
			} else {
				if(nOrdVars > 0){
					message(nOrdVars, " ordinal variables:", omxQuotes(ordVarNames), "\n")
				}
				if(nBinVars > 0){
					message(nBinVars, " binary variables:", omxQuotes(binVarNames), "\n")
				}
			}
		}
	}
	minLevels = xmuMinLevels(df)
	maxLevels = xmuMaxLevels(df)
	maxThresh = maxLevels - 1

	# TODO simplify for n = bin, n= ord, n= cont msg
	if(nBinVars > 0){
		binVarNames = names(df)[isBin]
		if(verbose){
			message(sum(isBin), " trait(s) are binary (only 2-levels).\n",
			omxQuotes(binVarNames),
			"\nFor these, you you MUST fix or constrain (usually mean==0 & var==1) the latent traits driving each ordinal variable.\n",
			"See ?mxThresholdMatrix")
		}
	}
	if(nOrdVars > 0){
		if(verbose){
			message(nOrdVars, " variables are ordinal (at least three levels). For these I will use Paras Mehta's 'fix first 2 thresholds' method.\n",
			"It's ESSENTIAL that you leave FREE the means and variances of the latent ordinal traits!!!\n",
			"See ?mxThresholdMatrix")
		}
	}
	if(minLevels == 1){
		stop("You seem to have a trait with only one category... makes it a bit futile to model it?")
	}

	df = df[, factorVarNames, drop = FALSE]

	if(nSib == 2){
		# For better precision, copy both halves of the dataframe into each
		T1 = df[, grep(paste0(suffixes[1], "$"), factorVarNames, value = TRUE), drop = FALSE]
		T2 = df[, grep(paste0(suffixes[2], "$"), factorVarNames, value = TRUE), drop = FALSE]
		names(T2) <- names(T1)
		dfLong = rbind(T1, T2)
		df = cbind(dfLong, dfLong)
		names(df) = factorVarNames
	} else if(nSib == 1){
		# df is fine as is.		
	} else {
		stop("I can only handle 1 and 2 sib models. You gave me ", nSib, " suffixes.")
	}
	
	# Size the threshMat to order maxThresh rows * nFactors cols
	threshMat = mxMatrix(name = threshMatName, type = "Full",
		nrow     = maxThresh,
		ncol     = nFactors,
		free     = TRUE, 
		values = rep(NA, (maxThresh * nFactors)),
		lbound   = l_u_bound[1],
		ubound   = l_u_bound[2],
		dimnames = list(paste0("th_", 1:maxThresh), factorVarNames)
	)
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
					preceedingValue = runValues[(indexIntoRLE - 1)]
					minimumStep = .01
					if(indexIntoRLE == distinctCount){
						newValues = seq(from = (preceedingValue + minimumStep), by = (minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					} else {
						followedBy = runValues[(indexIntoRLE + 1)]
						minimumStep = min((followedBy - preceedingValue)/(runLen + 1), minimumStep)
						newValues = seq(from = (followedBy - minimumStep), by = (-minimumStep), length.out = runLen)
						zValues[c(indexIntoZ:(indexIntoZ + runLen - 1))] = rev(newValues)
					}
				}
				indexIntoZ   = indexIntoZ + runLen
				indexIntoRLE = indexIntoRLE + 1
				# Play "The chemistry between them", Dorothy Hodgkin
				# Copenhagen, Michael Frein
			}
		}
        # TODO start from 1, right, not 2?
		# note 2015-03-22: rep(0) was rep(NA). But with deviation-based, the matrix can't contain NAs as it gets %*% by lowerones
		values = c(zValues[1:(nThreshThisVar)], rep(.001, (maxThresh - nThreshThisVar)))
		sortValues <- sort(zValues[1:(nThreshThisVar)], na.last = TRUE)
		if (!identical(sortValues, zValues[1:(nThreshThisVar)])) {
			umx_msg(values)
			stop("The thresholds for ", thisVarName, " are not in order... oops: that's my fault :-(")
		}

		# ==============
		# = Set labels =
		# ==============
		if(nSib == 2){
			# Search string to find all sib's versions of a var
			findStr = paste0( "(", paste(suffixes, collapse = "|"), ")$")
			thisLab = sub(findStr, "", thisVarName)		
		} else {
			thisLab = thisVarName
		}
        labels = c(paste0(thisLab, "_thresh", 1:nThreshThisVar), rep(NA, (maxThresh - nThreshThisVar)))
        
		# ============
		# = Set Free =
		# ============
		free = c(rep(TRUE, nThreshThisVar), rep(FALSE, (maxThresh - nThreshThisVar)))
		
		if(nThreshThisVar > 1){ # fix the first 2
			free[1:2] = FALSE
		}
		
		threshMat$labels[, thisVarName] = labels
		threshMat$free[,   thisVarName] = free
		threshMat$values[, thisVarName] = values
	} # end for each factor variable
	
	# TODO describe what we have at this point
	
	if(deviationBased) {
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
		# 1
		# startDeviations
		startDeviations = threshMat$values
		nrows = dim(threshMat$values)[1]
		ncols = dim(threshMat$values)[2]
		if (nrows > 1){
			for (col in 1:ncols) {
				# Skip row 1 which is the base
				for (row in 2:nrows) {
					# Convert remaining rows to offsets
					thisOffset = threshMat$values[row, col] - threshMat$values[(row-1), col]
					if(thisOffset<0){
						thisOffset = .001
					}
					startDeviations[row, col] = thisOffset
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
		devLabels = sub("_thresh", "_dev", threshMat$labels, ignore.case = F)
		deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full",
			nrow     = maxThresh, ncol = nFactors,
			free     = threshMat$free,
			labels   = devLabels,
			values   = startDeviations,
			lbound   = .001,
			# TODO ubound might want to be NA
			ubound   = 4,
			dimnames = list(paste0("dev_", 1:maxThresh), factorVarNames)
		)
		# 2
		lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThresh, free = FALSE, values = 1)
		# 3
		threshDimNames = list(paste0("th_", 1:maxThresh), factorVarNames)
		thresholdsAlgebra = mxAlgebra(name = threshMatName, lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = threshDimNames)

		return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra))
	} else if (hint == "left_censored"){
		# ignore everything above...
		message("using ", hint, " fixed thresholds")
		stop("tobit not implemented yet")
		return(threshMat)
	} else {
		return(threshMat)
	}
}
# ===========
# = Utility =
# ===========

umxCheck <- function(fit1){
	# are all the manifests in paths?
	# do the manifests have residuals?
	if(any(duplicated(fit1@manifestVars))){
		stop(paste("manifestVars contains duplicates:", duplicated(fit1@manifestVars)))
	}
	if(length(fit1@latentVars) == 0){
		# Check none are duplicates, none in manifests
		if(any(duplicated(fit1@latentVars))){
			stop(paste("latentVars contains duplicates:", duplicated(fit1@latentVars)))
		}
		if(any(duplicated(c(fit1@manifestVars, fit1@latentVars)))){
			stop(
				paste("manifest and latent lists contain clashing names:", duplicated(c(fit1@manifestVars, fit1@latentVars)))
			)
		}
	}
	# Check manifests in dataframe
}

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

#' umxPath: Flexible specification of sem paths
#'
#' @details This function returns a standard mxPath, but gives new options for specifying the path. In addition to the normal
#' \dQuote{from} and \dQuote{to}, it adds specialised parameters for variances (var), two headed paths (with) and means (mean).
#' There are also new terms to describe fixing values: \dQuote{fixedAt} and \dQuote{fixFirst}.
#' 
#' Finally, (in future) it will allow sem-style \dQuote{A->B} string specification.
#'
#' @description The goal of this function is to enable quck-to-write, quick-to-read, flexible path descriptions for RAM models in OpenMx.
#' 
#' It introduces 11 new words to our vocabulary for describing paths: \strong{with}, \strong{var}, \strong{cov}, \strong{unique.bivariate}, \strong{Cholesky}, \strong{means}, \strong{v1m0}, \strong{v.m.}, \strong{fixedAt}, \strong{freeAt}, \strong{firstAt}.
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
#' To fix a path at a value, instead of to \code{mxPath(from = A, to = A, arrows = 2, free = FALSE, values = 1)} you can say:
#' 
#' \code{umxPath(var = "A", fixedAt = 1)} .
#' 
#' The common task of creating a variable with variance fixed at 1 and mean at 0 is done thus:
#' 
#' \code{umxPath(v1m0 = "A")}
#' 
#' For free variance and means use:
#' 
#' \code{umxPath(v.m. = "A")}
#' 
#' umxPath exposes \dQuote{unique.bivariate} so you don't have to remember
#' how to fill in connect = in mxPath (you can still use connect if you wish).
#' 
#' So, to create paths A<->B, B<->C, and A<->C, you would say:
#' 
#' \code{umxPath(unique.bivariate = c('A',"B","C"))}
#' 
#' Setting up a latent trait, you can fix the loading of the first path with
#' 
#' \code{mxPath(A, to = c(B,C,D), fixFirst = TRUE)}  
#' 
#' This is equivalent to \code{mxPath(from = A, to = c(B,C,D), free = c(F, T, T), values = c(1, .5, .4))}.
#' 
#' A new feature is the ability to create Cholesky-pattern connections:
#' 
#' \code{umxPath(Cholesky = c("A1", "A2"), to c("var1", "var2"))}
#' 
#' Finally, a feature, not implemented in this release, but intended for the future is
#' John Fox "sem"-package style notation,
#' 
#' i.e., "A -> B; X <-> B; "
#' 
#' 
#' @param from either a source variable e.g "A" or c("A","B"), OR a sem-style path description, e.g. "A-> B" or "C <> B"
#' @param to one or more target variables for one-headed paths, e.g "A" or c("A","B") 
#' @param with same as "to = vars, arrows = 2". nb: from, to= and var=  must be left empty (their default)
#' @param var equivalent to setting "from = vars, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param cov equivalent to setting "from = X, to = Y, arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param unique.bivariate equivalent to setting "connect = "unique.bivariate", arrows = 2". nb: from, to, and with must be left empty (their default)
#' @param formative Paired with to, this will build a formative variable, from the formatives, allowing these to
#' covary, and to the latent "to" variable, fixing its variance to zero.
#' @param Cholesky Treat \strong{Cholesky} vars as latent and \strong{to} as measured, and connect as in an ACE model.
#' @param means equivalent to "from = 'one', to = x. nb: from, to, with and var must be left empty (their default).
#' @param v1m0 variance of 1 and mean of zero in one call.
#' @param v.m. variance and mean, both free.
#' @param fixedAt Equivalent to setting "free = FALSE, values = x" nb: free and values must be left empty (their default)
#' @param freeAt Equivalent to setting "free = TRUE, values = x" nb: free and values must be left empty (their default)
#' @param firstAt first value is fixed at this (values passed to free are ignored: warning if not a single TRUE)
#' @param connect as in mxPath - nb: Only used when using from and to
#' @param arrows as in mxPath - nb: Only used when using from and to
#' @param free whether the value is free to be optimised
#' @param values default value list
#' @param labels labels for each path
#' @param lbound lower bounds for each path value
#' @param ubound upper bounds for each path value
#' @return - 1 or more \code{\link{mxPath}}s
#' @export
#' @family Model Building Functions
#' @seealso - \code{\link{mxPath}}
#' @references - \url{http://tbates.github.io}
#' @examples
#' require(umx)
#' # Some examples of paths with umxPath
#' umxPath("A", to = "B") # One-headed path from A to B
#' umxPath("A", to = "B", fixedAt = 1) # same, with value fixed @@1
#' umxPath("A", to = LETTERS[2:4], firstAt = 1) # Fix only the first path, others free
#' umxPath(var = "A") # Give a variance to A
#' umxPath(var = "A", fixedAt = 1) # Give a variance, fixed at 1
#' umxPath(var = LETTERS[1:5], fixedAt = 1)
#' umxPath(means = c("A","B")) # Create a means model for A: from = "one", to = "A"
#' umxPath(v1m0 = "A") # Give "A" variance and a mean, fixed at 1 and 0 respectively
#' umxPath(v.m. = "A") # Give "A" variance and a mean, leaving both free.
#' umxPath("A", with = "B") # using with: same as "to = B, arrows = 2"
#' umxPath("A", with = "B", fixedAt = .5) # 2-head path fixed at .5
#' umxPath("A", with = c("B", "C"), firstAt = 1) # first covariance fixed at 1
#' umxPath(cov = c("A", "B"))  # Covariance A <-> B
#' umxPath(unique.bivariate = letters[1:4]) # bivariate paths a<->b, a<->c, a<->d, b<->c etc.
#' umxPath(Cholesky = c("A1","A2"), to = c("m1", "m2")) # Cholesky
#' # A worked example
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' myData = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' m1 <- umxRAM("One Factor", data = myData,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' umxSummary(m1, show = "std")
#'
#' # ====================
#' # = Cholesky example =
#' # ====================
#' latents   = paste0("A", 1:3)
#' manifests = names(demoOneFactor)
#' myData = mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' m1 <- umxRAM("Chol", data = myData,
#' 	umxPath(Cholesky = latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' umxSummary(m1, show = "std")
#'
#' # The following NOT YET implemented!!
#' # umxPath("A <-> B") # same path as above using a string
#' # umxPath("A -> B") # one-headed arrow with string syntax
#' # umxPath("A <> B; A <-- B") # This is ok too
#' # umxPath("A -> B; B>C; C --> D") # two paths. white space and hyphens not needed
#' # # manifests is a reserved word, as is latents.
#' # # It allows the string syntax to use the manifestVars variable
#' # umxPath("A -> manifests") 
umxPath <- function(from = NULL, to = NULL, with = NULL, var = NULL, cov = NULL, unique.bivariate = NULL, formative = NULL, Cholesky = NULL, means = NULL, v1m0 = NULL, v.m. = NULL, fixedAt = NULL, freeAt = NULL, firstAt = NULL, connect = c("single", "all.pairs", "all.bivariate", "unique.pairs", "unique.bivariate"), arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA) {
	connect = match.arg(connect) # set to single if not overridden by user.
	if(!is.null(from)){
		if(length(from) > 1){
			isSEMstyle = grepl("[<>]", x = from[1])	
		} else {
			isSEMstyle = grepl("[<>]", x = from)				
		}
		if(isSEMstyle){
			stop("sem-style string syntax not yet implemented. In the mean time, try the other features, like with, var, means = , fixedAt = , fixFirst = ")
			if("from contains an arrow"){
				# parse into paths
			} else {
				if(!is.null(with)){
					to = with
					arrows = 2
					connect = "single"
				} else {
					to = to
					arrows = 1
					connect = "single"
				}
			}	
			a = "A ->B;A<-B; A>B; A --> B
			A<->B"
			# remove newlines, replacing with ;
			allOneLine = gsub("\n+", ";", a, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?^<-?>[ \t]?", "->", allOneLine, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?-?>[ \t]?", "<-", regularizedArrows, ignore.case = TRUE)

			# TODO remove duplicate ; 
			pathList = umx_explode(";", allOneLine)
			for (aPath in pathList) {
				if(length(umx_explode("<->", aPath))==3){
					# bivariate
					parts = umx_explode("<->", aPath)
					# not finished, obviously...
					mxPath(from = umx_trim(parts[1]))
				} else if(length(umx_explode("->", aPath))==3){
					# from to
				} else if(length(umx_explode("<-", aPath))==3){
					# to from
				}else{
					# bad line
				}
			}
			umx_explode("", a)
		}
	}
	n = 0

	for (i in list(with, cov, var, means, unique.bivariate, v.m. , v1m0, Cholesky)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n > 1){
		stop("At most one of with, cov, var, means, unique.bivariate, v1m0, v.m. or Cholesky can be set: Use at one time")
	} else if(n == 0){
		# check that from is set?
		if(is.null(from)){
			stop("You must set at least 'from'")
		}	
	} else {
		# n = 1
	}

	if(!is.null(Cholesky)){
		if(!(length(to) >= length(Cholesky))){
			stop("Must have at least as many 'to' vars as latents for Cholesky: you gave me ",
			length(to), " to vars and ", length(Cholesky), " Choleksy latents")
		}
		if(!is.na(labels)){
			stop("I don't yet support labels for Cholesky")
		}
		if(length(lbound) > 1){
			stop("I don't yet support multiple lbounds for Cholesky")
		}
		if(length(ubound) > 1){
			stop("I don't yet support multiple ubounds for Cholesky")
		}
		n = 1
		max_to = length(to)
		for(i in seq_along(Cholesky)) {
			a = mxPath(from = Cholesky[i], to = to[n:max_to], arrows = 1, free = free, lbound = lbound, ubound = ubound)
			if(n == 1){
				out = a
			} else if(n == 2) {
				out = list(out, a)
			} else {
				out = c(out, a)
			}
			n = n + 1
		}
		return(out)
	}
	if(!is.null(v1m0)){
		# TODO lbound ubound unlikely to be applied to two things, and can't affect result... error if they're not NULL?
		if(!is.na(lbound)&&is.na(ubound)){
			message("I lbounded var of ", v1m0, " @0")
		}
		a = mxPath(from = v1m0, arrows = 2, free = FALSE, values = 1, labels = labels, lbound = 0, ubound = ubound)
		b = mxPath(from = "one", to = v1m0, free = FALSE, values = 0, labels = labels, lbound = lbound, ubound = ubound)
		return(list(a,b))
	}

	if(!is.null(v.m.)){
		# TODO lbound ubound unlikely to be applied to two things. lbound for var should be 0
		if(!is.na(lbound)&&is.na(ubound)){
			message("I lbounded var of ", v.m. , " @0")
		}
		a = mxPath(from = v.m., arrows = 2, free = TRUE, values = 1, labels = labels, lbound = 0, ubound = ubound)
		b = mxPath(from = "one", to = v.m., free = TRUE, values = 0, labels = labels, lbound = lbound, ubound = ubound)
		return(list(a,b))
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
		if(!is.null(from) | !is.null(to)){
			stop("To use 'var = ', 'from' and 'to' should be empty")
		} else {
			from   = var
			to     = var
			arrows = 2
			connect = "single"
			if(is.na(lbound)){
				message("I lbounded var of ", omxQuotes(var), " @0")			
				lbound  = 0
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
	# ====================================
	# = From and to have now been set... =
	# ====================================

	# ==============================
	# = Handle fixedAt and firstAt =
	# ==============================
	if(sum(c(is.null(fixedAt), is.null(firstAt), is.null(freeAt))) < 2){
		stop("At most one of fixedAt freeAt and firstAt can be set: You seem to have tried to set more than one.")
	}

	# Handle firstAt
	if(!is.null(firstAt)){
		if(length(from) > 1 && length(to) > 1){
			# TODO think about this
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

# =====================================
# = Parallel helpers to be added here =
# =====================================

#' Helper Functions for Structural Equation Modelling in OpenMx
#'
#' umx allows you to more easily build, run, modify, and report models using OpenMx
#' with code. The core functions are linked below under \strong{See Also}
#'
#' The functions are organized into families: Have a read of these below, click to explore.
#' 
#' All the functions have explanatory examples, so use the help, even if you think it won't help :-)
#' Have a look, for example at \code{\link{umxRAM}}
#' 
#' Introductory working examples are below. You can run all demos with demo(umx)
#' When I have a vignette, it will be: vignette("umx", package = "umx")
#' 
#' There is a helpful blog at \url{http://tbates.github.io}
#' 
#' If you want the bleeding-edge version:
#' 
#' devtools::install_github("tbates/umx")
#' 
#' @family Model Building Functions
#' @family Reporting Functions
#' @family Modify or Compare Models
#' @family Super-easy helpers
#' @family Misc
#' @family Data Functions
#' @family Utility Functions
#' @family Stats Functions
#' @family File Functions
#' @family Twin Modeling Functions
#' @family zAdvanced Helpers
#' @references - \url{http://www.github.com/tbates/umx}
#' 
#' @examples
#' require("umx")
#' data(demoOneFactor)
#' myData = mxData(cov(demoOneFactor), type = "cov", numObs = nrow(demoOneFactor))
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = myData,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents  , fixedAt=1)
#' )
#' 
#' omxGetParameters(m1) # Wow! Now your model has informative labels, & better starts
#' 
#' # Let's get some journal-ready fit information
#' 
#' umxSummary(m1) 
#' umxSummary(m1, show = "std") #also display parameter estimates 
#' # You can get the coefficients of an MxModel with coef(), just like for lm etc.
#' coef(m1)
#' 
#' # ==================
#' # = Model updating =
#' # ==================
#' # Can we set the loading of X5 on G to zero?
#' m2 = omxSetParameters(m1, labels = "G_to_x1", values = 0, free = FALSE, name = "no_g_on_X5")
#' m2 = mxRun(m2)
#' # Compare the two models
#' umxCompare(m1, m2)
#' 
#' # Use umxModify to do the same thing in 1-line
#' m2 = umxModify(m1, "G_to_x1", name = "no_effect_of_g_on_X5", comparison = TRUE)
#' 
#' # =================================
#' # = Get some Confidence intervals =
#' # =================================
#' 
#' confint(m1, run = TRUE) # lots more to learn about ?confint.MxModel
#' 
#' # And make a Figure in .gv format!
#' 
#' # plot(m1, std = TRUE)
#' # If you just want the .dot code returned set file = NA
#' plot(m1, std = TRUE, file = NA)
#' @docType package
#' @name umx
NULL
