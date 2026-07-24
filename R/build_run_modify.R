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

# ===============================
# = Highlevel models (ACE, GxE) =
# ===============================

# ============================
# = Core Modeling Functions =
# ============================

#' Make a LISREL model analogous to umxRAM
#'
#' @description
#' `umxLISREL` is a wrapper for [OpenMx::mxModel()] with `type="LISREL"`.
#' It automatically partitions manifest and latent variables into endogenous and exogenous sets,
#' inserts data, handles raw/covariance types, adds means if raw data are used, handles ordinal variables,
#' runs the model, and displays summaries.
#'
#' @param model NA, a name string, or an existing LISREL [OpenMx::mxModel()] to update.
#' @param ... Path statements (`mxPath` or `umxPath`), matrices, etc., to add to the model.
#' @param data Data frame, matrix, or `mxData` to use.
#' @param manifestVars Optional list of endogenous/exogenous manifest variables, or character vector of manifest variables to partition.
#' @param latentVars Optional list of endogenous/exogenous latent variables, or character vector of latent variables to partition.
#' @param name Optional name for the model (defaults to the model's current name or 'm1').
#' @param group Column name in data to partition data for multi-group models.
#' @param group.equal Not implemented.
#' @param suffix Suffix to append to parameter labels.
#' @param comparison Compare the model to saturated models in summary (Default = TRUE).
#' @param type Data type: "Auto" (guesses), "FIML", "cov", "cor", "WLS", "DWLS", "ULS".
#' @param weight Weight column name.
#' @param allContinuousMethod Method for continuous variables when WLS is used.
#' @param autoRun Run the model (default = TRUE).
#' @param tryHard How to run the model: "no", "yes", "ordinal", "search".
#' @param std Standardize output (Default = FALSE).
#' @param refModels Reference models for summary comparison.
#' @param remove_unused_manifests Remove manifests not used in paths (Default = TRUE).
#' @param independent Whether the model is independent (Default = NA).
#' @param setValues Automatically set starting values (Default = TRUE).
#' @param optimizer Set the optimizer to use.
#' @param verbose Print diagnostic info.
#' @param std.lv Standardize latent variables (Default = FALSE).
#' @param printTab Print parameter table.
#' @return A LISREL [OpenMx::mxModel()]
#' @export
#' @family Core Model Building Functions
#' @seealso [umxRAM()], [plot.MxLISRELModel()], [xmu_standardize_LISREL()]
#' @references <https://github.com/tbates/umx>, <https://tbates.github.io>

#' @examples
#' \dontrun{
#' library(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' 
#' # 1. LISREL model with covariance data
#' m1 = umxLISREL("one_factor_cov", data = demoOneFactor, type = "cov",
#'                umxPath("G", to = manifests),
#'                umxPath(var = manifests),
#'                umxPath(var = "G", fixedAt = 1))
#' 
#' # 2. LISREL model with raw data (means are automatically added)
#' m2 = umxLISREL("one_factor_raw", data = demoOneFactor,
#'                umxPath("G", to = manifests),
#'                umxPath(var = manifests),
#'                umxPath(var = "G", fixedAt = 1))
#' 
#' # 3. Forced exogenous manifests using manifestVars list override
#' m3 = umxLISREL("forced_exogenous", data = demoOneFactor,
#'                manifestVars = list(endogenous = c("x4", "x5"), exogenous = c("x1", "x2")),
#'                latentVars = list(endogenous = "G", exogenous = "xi"),
#'                umxPath("xi", to = c("x1", "x2")),
#'                umxPath("G", to = c("x4", "x5")),
#'                umxPath("xi", to = "G"),
#'                umxPath(var = c("x1", "x2", "x4", "x5")),
#'                umxPath(var = "xi", fixedAt = 1),
#'                umxPath(var = "G"),
#'                umxPath(means = c("x1", "x2", "x4", "x5")))
#' }
umxLISREL <- function(model = NA, ..., data = NULL, manifestVars = NULL, latentVars = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), weight = NULL, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, printTab = FALSE) {
	dotItems = list(...) # grab all the dot items: mxPaths, etc...
	# Check for data/model objects passed in ... before unlist() flattens them
	for (item in dotItems) {
		thisIs = class(item)[1]
		if (thisIs %in% c("data.frame", "matrix", "MxData")) {
			stop("umxLISREL can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me a ", thisIs, " inside the path list. ",
				 "To include data in umxLISREL, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
		} else if (thisIs == "MxModel") {
			stop("umxLISREL can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me an MxModel inside the path list. ",
				 "umxLISREL does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
		}
	}
	dotItems = unlist(dotItems) # In case any dot items are lists of mxPaths, etc...
	type       = match.arg(type)
	tryHard    = match.arg(tryHard)
	allContinuousMethod = match.arg(allContinuousMethod)

	if(!is.null(weight)){
		message("Polite note: Weight feature has not been tested: Models may have spurious fit, consider this feature alpha quality")
	}
	# if data provided check it isn't a tibble
	if(!is.null(data)){
		# avoid ingesting tibbles
		if(inherits(data, "tbl")){
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
		if(!inherits(data, "data.frame")){
			stop("Currently, for multiple groups, data must be a raw data.frame so I can subset it into multiple groups. You gave me a ", omxQuotes(class(data)))
		}
	}

	# umxPath-based model
	if(typeof(model) == "character"){
		if(is.na(name)){
			name = model
		} else {
			stop("If model is set to a string, don't pass in name as well...")
		}
	} else {
		if(umx_is_LISREL(model)){
			# message("Updating existing model")
			if(is.na(name)){
				name = model$name
			}
			if(is.null(data)){
				newModel = mxModel(model, dotItems, name = name)
			} else {
				if(umx_is_MxData(data)){
					newModel = mxModel(model, dotItems, data, name = name)
				} else {
					stop("Polite note: I don't know how to convert raw data into mxData to update your model - can you please do that for me and try again?")
				}
			}
			newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
			return(newModel)
		} else {
			stop("First item must be either an existing LISREL model or a name string. You gave me a ", typeof(model))
		}
	}

	umx_check(!is.null(data), "stop", "In umxLISREL, you must set 'data = '. If you're building a model with no data, use mxModel")

	foundNames = c()
	defnNames = c()
	targets = c()
	for (thisItem in dotItems) {
		if(!is.list(thisItem)){
			# Sometimes we get a list, so expand everything to a list.
			thisItem = list(thisItem)
		}
		for (i in seq_along(thisItem)) {
			thisIs = class(thisItem[[i]])[1]
			if(thisIs == "MxPath"){
				foundNames = append(foundNames, c(thisItem[[i]]$from, thisItem[[i]]$to))
				if(thisItem[[i]]$arrows == 1){
					targets = append(targets, thisItem[[i]]$to)
				}
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
				} else if (isS4(thisItem[[i]]) && grepl("^Mx", thisIs) && !thisIs %in% c("MxModel", "MxData")) {
					# Valid OpenMx S4 object (MxConstraint, MxAlgebra, MxCI, etc.) - no path/matrix-label actions needed
				} else {
					if (thisIs %in% c("data.frame", "matrix", "MxData")) {
						stop("umxLISREL can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " inside the path list. ",
							 "To include data in umxLISREL, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
					} else if (thisIs == "MxModel") {
						stop("umxLISREL can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me an MxModel inside the path list. ",
							 "umxLISREL does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
					} else {
						stop("umxLISREL can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " which is not supported inside the LISREL path list.", call. = FALSE)
					}
				}
			}			
		}
	}

	# ============================
	# = All dotItems processed   =
	# ============================

	# ====================================
	# = Find latentVars and manifestVars =
	# ====================================
	# Omit NAs from found names (empty "to =" can generate these spuriously)
	foundNames = unique(na.omit(foundNames))
	defnNames  = unique(na.omit(defnNames))
	targets    = unique(na.omit(targets))

	if(length(defnNames) > 0){
		# check'm if you've got'm
		umx_check_names(defnNames, data = data, message = "note: used as definition variable, but not present in data")
	}

	# 1. Determine Manifests
	if (is.list(manifestVars)) {
		manifestList = manifestVars
		usedManifests = c(manifestList$endogenous, manifestList$exogenous)
		if (!is.null(weight)) {
			myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
				defnNames, verbose = verbose, weight = weight)
		} else {
			myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
				defnNames, verbose = verbose)
		}
	} else {
		if (is.null(manifestVars)) {
			manifestVarsPool = unique(na.omit(umx_names(data)))
		} else {
			manifestVarsPool = manifestVars
		}

		# List up used and un-used Manifests
		unusedManifests = setdiff(manifestVarsPool, c(foundNames, defnNames))
		if (!is.null(weight)) unusedManifests = setdiff(c(manifestVarsPool, weight), c(foundNames, defnNames))

		if(remove_unused_manifests & length(unusedManifests) > 0){
			usedManifests = setdiff(intersect(manifestVarsPool, foundNames), "one")
			if (!is.null(weight)) {
				myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
					defnNames, verbose = verbose, weight = weight)
			} else {
				myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
					defnNames, verbose = verbose)
			}
		} else {
			# keep everything
			usedManifests = setdiff(manifestVarsPool, defnNames)
			myData = xmu_make_mxData(data = data, type = type, verbose = verbose, manifests = usedManifests, fullCovs = 
				defnNames)
		}
	}

	# 2. Determine Latents
	if (is.list(latentVars)) {
		latentList = latentVars
		endogenousLatents = latentList$endogenous
		exogenousLatents = latentList$exogenous
		latentVarsPool = c(endogenousLatents, exogenousLatents)
	} else {
		if (is.null(latentVars)) {
			latentVarsPool = setdiff(foundNames, c(usedManifests, "one"))
		} else {
			latentVarsPool = latentVars
		}

		# Partition latentVarsPool into endogenous and exogenous
		# A latent variable is endogenous if it is targeted by another latent variable
		endogenousLatents = c()
		for (thisItem in dotItems) {
			if (inherits(thisItem, "MxPath") && thisItem$arrows == 1) {
				fromLatents = intersect(thisItem$from, latentVarsPool)
				toLatents   = intersect(thisItem$to, latentVarsPool)
				if (length(fromLatents) > 0 && length(toLatents) > 0) {
					endogenousLatents = union(endogenousLatents, toLatents)
				}
			}
		}
		exogenousLatents = setdiff(latentVarsPool, endogenousLatents)

		latentList = list()
		if(length(endogenousLatents) > 0) latentList$endogenous = endogenousLatents
		if(length(exogenousLatents) > 0) latentList$exogenous = exogenousLatents
	}

	# 3. Partition manifests if not already a list
	if (!is.list(manifestVars)) {
		# A manifest variable is endogenous if it is targeted by an endogenous latent or another manifest
		endogenousManifests = c()
		for (thisItem in dotItems) {
			if (inherits(thisItem, "MxPath") && thisItem$arrows == 1) {
				toManifests = intersect(thisItem$to, usedManifests)
				if (length(toManifests) > 0) {
					fromEndoLatents = intersect(thisItem$from, endogenousLatents)
					fromManifests   = intersect(thisItem$from, usedManifests)
					if (length(fromEndoLatents) > 0 || length(fromManifests) > 0) {
						endogenousManifests = union(endogenousManifests, toManifests)
					}
				}
			}
		}
		exogenousManifests = setdiff(usedManifests, endogenousManifests)

		manifestList = list()
		if(length(endogenousManifests) > 0) manifestList$endogenous = endogenousManifests
		if(length(exogenousManifests) > 0) manifestList$exogenous = exogenousManifests
	}

	# Report which latents were created
	nLatent = length(latentVarsPool)
	if (!umx_set_silent(silent=TRUE)) {
		if(nLatent == 0){
			# message("No latent variables were created.\n")
		} else if (nLatent == 1){
			message("A latent variable '", latentVarsPool[1], "' was created. ")
		} else {
			message(nLatent, " latent variables were created:", paste(latentVarsPool, collapse = ", "), ". ")
		}
	}

	# ==================
	# = Assemble model =
	# ==================

	newModel = do.call("mxModel", list(name = name, type = "LISREL",
		manifestVars = manifestList,
		latentVars  = latentList,
		independent = independent, dotItems)
	)
	# ============
	# = Add data =
	# ============
	if (inherits(myData, "character")){
		newModel = xmuLabel(newModel, suffix = suffix)
		if(is.null(group)){
			if(autoRun && umx_set_auto_plot(silent = TRUE)){
				plot(newModel)
			}
			return(newModel)
		}
	}else{
		newModel = mxModel(newModel, myData)
	}
	
	# ==========================
	# = Add means if necessary =
	# ==========================
	needsMeans = xmu_check_needs_means(data = myData, type = type, allContinuousMethod = allContinuousMethod)
	# Check if means matrices (TY or TX) exist. In LISREL, if we add one path, it creates the matrix.
	if(needsMeans && is.null(newModel$matrices$TX) && is.null(newModel$matrices$TY)){
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
		# For LISREL, set thresholds in the expectation
		newModel$expectation$thresholds = "threshMat"
		newModel = mxModel(newModel, umxThresholdMatrix(myData$observed, fullVarNames = usedManifests, verbose = TRUE))
	}

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
		newModel = mxModel(newModel, mxFitFunctionWLS(type= type, allContinuousMethod = allContinuousMethod) )
	}

	# =====================
	# = Handle group here =
	# =====================
	if(!is.null(group)){
		modelList = list()
		groupCol  = data[, group]
		levelsOfGroup = unique(groupCol)
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
				thisModel = umxSetParameters(thisModel, regex= "_GROUP$", newlabels= paste0("_", thisLevelOfGroup))
			}

			modelList = c(modelList, thisModel)
		}
		return(umxSuperModel(name = name, modelList, autoRun = autoRun, tryHard = tryHard, std = std))
	}

	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
	invisible(newModel)
}

#' Make a multi-group model
#'
#' @description
#' `umxSuperModel` takes 1 or more models and wraps them in a supermodel with a
#' [OpenMx::mxFitFunctionMultigroup()] fit function that minimizes the sum of the
#' fits of the sub-models.
#'
#' *note*: Any duplicate model-names are renamed to be unique by suffixing `_1` etc.
#'
#' @param name The name for the container model (default = 'super')
#' @param ...  Models forming the multiple groups contained in the supermodel.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param std Show standardized parameters, raw (default), or just the fit indices (null)
#' @return - [OpenMx::mxModel()]
#' @export
#' @family Core Model Building Functions
#' @seealso - [OpenMx::mxFitFunctionMultigroup()], [umxRAM()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @examples
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
		print(paste0("Polite note from umxSuperModel: I renamed sub-models with duplicate names, e.g. ", omxQuotes(dupes[1]), " -> ", omxQuotes(paste0(dupes[1], "_2"))))
	}
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, std = std)
	invisible(newModel)
}

#' umxModify: Add, set, or drop model paths by label.
#' 
#' umxModify allows you to modify, re-run and summarize an [OpenMx::mxModel()], all in one line of script.
#' 
#' @details
#' You can add paths, or other model elements, set path values (default is 0), or replace labels.
#' As an example, this one-liner drops a path labelled "Cs", and returns the updated model:
#' 
#' `fit2 = umxModify(fit1, update = "Cs", name = "newModelName", comparison = TRUE)`
#' 
#' Regular expressions are a powerful feature: they let you drop collections of paths by matching patterns
#' for instance, this would match labels containing either "Cs" or "Cr":
#' 
#' ```R
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
#' @param lastFit The [OpenMx::mxModel()] you wish to update and run.
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
#' @param intervals Whether to run confidence intervals (see [OpenMx::mxRun()])
#' @param verbose How much feedback to give
#' @return - [OpenMx::mxModel()]
#' @family Core Model Building Functions
#' @references - <https://github.com/tbates/umx>
#' @export

#' @examples
#' \dontrun{
#' require(umx)
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
	if(!is.null(name) && name == "guess"){
		name = update[1]
	}
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
			all_labels = names(omxGetParameters(newModel, free = NA))
			bad_labels = theLabels[!(theLabels %in% all_labels)]
			if(length(bad_labels) > 0){
				msg = paste0("Some labels were not found in the model: ", paste(omxQuotes(bad_labels), collapse = ", "), "\n")
				for(bad in bad_labels){
					suggestions = c()
					# Check for _with_ reversal
					if(grepl("_with_", bad)){
						parts = strsplit(bad, "_with_")[[1]]
						if(length(parts) == 2){
							reversed = paste0(parts[2], "_with_", parts[1])
							if(reversed %in% all_labels){
								suggestions = c(suggestions, reversed)
							}
						}
					}
					# Check for close matches (fuzzy matching)
					dists = as.vector(adist(bad, all_labels))
					close = all_labels[dists <= 2]
					suggestions = c(suggestions, close)
					suggestions = unique(suggestions)
					if(length(suggestions) > 0){
						msg = paste0(msg, "  * For ", omxQuotes(bad), ", did you mean: ", paste(omxQuotes(suggestions), collapse = " or "), "?\n")
					} else {
						msg = paste0(msg, "  * For ", omxQuotes(bad), ", no close matches were found.\n")
					}
				}
				msg = paste0(msg, "Use parameters(model) to see all available labels.")
				stop(msg, call. = FALSE)
			}
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
#' Should work with rotations provided in `libs("GPArotation")` and `libs("psych")`, e.g.,
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

#' @examples
#' \dontrun{
#' # Rotate a CP solution(param)
#' # Common pathway model rotation
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
#' @return - [OpenMx::mxModel()]
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()]

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
xmuRAM2Ordinal <- function(model, verbose = FALSE, name = NULL) {
	if(!umx_is_RAM(model)){
		stop("xmuRAM2Ordinal only works with RAM models, sorry.")
	}
	if(!is.null(name)){
		model = mxRename(model, name)
	}
	model$expectation$thresholds = "threshMat"
	
	# Threshold construction quiet by default; ID notes from xmu_threshold_id_RAM when parameters change
	model = mxModel(model, umxThresholdMatrix(model$data$observed, fullVarNames = model$manifestVars, verbose = verbose))
	model = xmu_threshold_id_RAM(model, action = "fix", verbose = TRUE)
	return(model)
}

#' Enforce or check Mehta/binary identification for threshold RAM models
#'
#' For binary manifests: latent mean fixed at 0 and residual variance fixed at 1.
#' For ordinal manifests (>2 levels, Mehta): mean and residual variance free.
#' Continuous manifests are left unchanged. Does not rewrite path labels.
#'
#' @param model An OpenMx RAM [OpenMx::mxModel()] with data and (typically) thresholds.
#' @param action `"fix"` (default) corrects free/values; `"check"` only warns.
#' @param verbose If TRUE (default), emit one line when parameters are fixed/freed or when check finds problems. Silent when already correct.
#' @return The model (modified if `action = "fix"`).
#' @export
#' @family xmu internal not for end user
#' @seealso [umxThresholdMatrix()], [xmuRAM2Ordinal()], [umxRAM()]
xmu_threshold_id_RAM <- function(model, action = c("fix", "check"), verbose = TRUE) {
	action = match.arg(action)
	if (!umx_is_RAM(model)) {
		stop("xmu_threshold_id_RAM only works with RAM models.")
	}
	if (is.null(model$data) || is.null(model$data$observed) || model$data$type != "raw") {
		return(model)
	}
	manifests = model$manifestVars
	if (length(manifests) < 1) {
		return(model)
	}
	obsCols = intersect(manifests, colnames(model$data$observed))
	if (length(obsCols) < 1) {
		return(model)
	}
	summaryObj = umx_is_ordered(model$data$observed[, obsCols, drop = FALSE], summaryObject = TRUE)
	binVars = intersect(summaryObj$binVarNames, manifests)
	ordVars = intersect(summaryObj$ordVarNames, manifests)

	if (length(binVars) < 1 && length(ordVars) < 1) {
		return(model)
	}

	fixedMean = character(0)
	fixedVar = character(0)
	freedMean = character(0)
	freedVar = character(0)
	problems = character(0)

	# ---------- Binary: mean@0, residual@1 ----------
	for (v in binVars) {
		if (is.null(model$M) || is.null(dimnames(model$M$values)) || !(v %in% colnames(model$M$values))) {
			problems = c(problems, paste0(v, " (binary: missing mean path)"))
		} else {
			meanOk = isFALSE(model$M$free[1, v]) && isTRUE(all.equal(as.numeric(model$M$values[1, v]), 0))
			if (!meanOk) {
				if (action == "fix") {
					model$M$free[1, v] = FALSE
					model$M$values[1, v] = 0
					fixedMean = c(fixedMean, v)
				} else {
					problems = c(problems, paste0(v, " (binary mean should be fixed at 0)"))
				}
			}
		}
		if (is.null(model$S) || is.null(dimnames(model$S$values)) || !(v %in% rownames(model$S$values))) {
			if (action == "fix") {
				model = mxModel(model, mxPath(from = v, arrows = 2, free = FALSE, values = 1))
				fixedVar = c(fixedVar, v)
			} else {
				problems = c(problems, paste0(v, " (binary residual missing; should be fixed at 1)"))
			}
		} else {
			varOk = isFALSE(model$S$free[v, v]) && isTRUE(all.equal(as.numeric(model$S$values[v, v]), 1))
			if (!varOk) {
				if (action == "fix") {
					model$S$free[v, v] = FALSE
					model$S$values[v, v] = 1
					fixedVar = c(fixedVar, v)
				} else {
					problems = c(problems, paste0(v, " (binary residual should be fixed at 1)"))
				}
			}
		}
	}

	# ---------- Ordinal (Mehta): free mean and residual ----------
	for (v in ordVars) {
		if (!is.null(model$M) && !is.null(dimnames(model$M$values)) && (v %in% colnames(model$M$values))) {
			if (!isTRUE(model$M$free[1, v])) {
				if (action == "fix") {
					model$M$free[1, v] = TRUE
					freedMean = c(freedMean, v)
				} else {
					problems = c(problems, paste0(v, " (ordinal mean should be free; Mehta)"))
				}
			}
		} else {
			problems = c(problems, paste0(v, " (ordinal: missing mean path)"))
		}
		if (!is.null(model$S) && !is.null(dimnames(model$S$values)) && (v %in% rownames(model$S$values))) {
			if (!isTRUE(model$S$free[v, v])) {
				if (action == "fix") {
					model$S$free[v, v] = TRUE
					freedVar = c(freedVar, v)
				} else {
					problems = c(problems, paste0(v, " (ordinal residual should be free; Mehta)"))
				}
			}
		} else {
			problems = c(problems, paste0(v, " (ordinal: missing residual path)"))
		}
	}

	if (verbose) {
		bits = character(0)
		if (length(fixedMean) > 0) {
			bits = c(bits, paste0("binary mean@0: ", paste(unique(fixedMean), collapse = ", ")))
		}
		if (length(fixedVar) > 0) {
			bits = c(bits, paste0("binary residual@1: ", paste(unique(fixedVar), collapse = ", ")))
		}
		if (length(freedMean) > 0) {
			bits = c(bits, paste0("freed ordinal mean: ", paste(unique(freedMean), collapse = ", ")))
		}
		if (length(freedVar) > 0) {
			bits = c(bits, paste0("freed ordinal residual: ", paste(unique(freedVar), collapse = ", ")))
		}
		if (length(bits) > 0) {
			message("umx note: ", paste(bits, collapse = "; "), " (see ?umxThresholdMatrix).")
		}
		if (action == "check" && length(problems) > 0) {
			warning("umx threshold ID: ", paste(problems, collapse = "; "), " (see ?umxThresholdMatrix).", call. = FALSE)
		}
	}
	return(model)
}

#' Verify twin-model threshold identification (means / binary Vtot constraint)
#'
#' Does not modify the model or rewrite twin mean labels. Emits at most one warning
#' if binary means are free, ordinal means are fixed, or the binary Vtot==1 machinery is missing.
#'
#' @param model A twin super-model with `top` (typically from [xmu_make_TwinSuperModel()]).
#' @param fullVars Character vector of full twin variable names (e.g. `wt_T1`, `wt_T2`).
#' @param verbose If TRUE, warn on problems; silent when OK.
#' @return Invisibly, a character vector of problem strings (empty if OK).
#' @export
#' @family xmu internal not for end user
xmu_threshold_id_twin_check <- function(model, fullVars, verbose = TRUE) {
	problems = character(0)
	if (is.null(model$top) || is.null(model$MZ$data) || is.null(model$MZ$data$observed)) {
		return(invisible(problems))
	}
	obs = model$MZ$data$observed
	useVars = intersect(fullVars, colnames(obs))
	if (length(useVars) < 1) {
		return(invisible(problems))
	}
	summaryObj = umx_is_ordered(obs[, useVars, drop = FALSE], summaryObject = TRUE)
	binVars = intersect(summaryObj$binVarNames, useVars)
	ordVars = intersect(summaryObj$ordVarNames, useVars)

	if (length(binVars) < 1 && length(ordVars) < 1) {
		return(invisible(problems))
	}

	if (!is.null(model$top$expMean)) {
		em = model$top$expMean
		meanNames = colnames(em$free)
		if (is.null(meanNames)) {
			meanNames = dimnames(em$free)[[2]]
		}
		for (v in binVars) {
			if (!is.null(meanNames) && v %in% meanNames) {
				if (isTRUE(em$free[1, v])) {
					problems = c(problems, paste0(v, " binary mean free (should be fixed)"))
				}
			}
		}
		for (v in ordVars) {
			if (!is.null(meanNames) && v %in% meanNames) {
				if (!isTRUE(em$free[1, v])) {
					problems = c(problems, paste0(v, " ordinal mean fixed (should be free; Mehta)"))
				}
			}
		}
	} else if (length(binVars) + length(ordVars) > 0) {
		problems = c(problems, "top.expMean missing for ordinal/binary model")
	}

	if (length(binVars) > 0) {
		hasBinId = !is.null(model$top$binLabels) ||
			(!is.null(model$top$matrices) && !is.null(model$top$matrices$binLabels)) ||
			!is.null(model$top$constrain_Bin_var_to_1)
		if (!hasBinId && !is.null(model$top$constraints)) {
			cn = names(model$top$constraints)
			hasBinId = any(cn == "constrain_Bin_var_to_1") || any(grepl("Bin_var|binLabels", cn, ignore.case = TRUE))
		}
		if (!hasBinId) {
			problems = c(problems, "binary Vtot==1 identification (binLabels / constrain_Bin_var_to_1) not found")
		}
	}

	if (verbose && length(problems) > 0) {
		warning("umx twin threshold ID: ", paste(problems, collapse = "; "), " (see ?umxThresholdMatrix).", call. = FALSE)
	}
	invisible(problems)
}

#' xmuValues: Set values in RAM model, matrix, or path
#'
#' For models to be estimated, it is essential that path values start at credible values. 
#' `xmuValues` takes on that task for you.
#' 
#' xmuValues can set start values for the free parameters in both RAM and Matrix [OpenMx::mxModel()]s. 
#' It can also take an mxMatrix as input.
#' It tries to be smart in guessing starts from the values in your data and the model type.
#' 
#' *note*: If you give xmuValues a numeric input, it will use obj as the mean, and return a 
#' list of length n, with sd = sd.
#'
#' @param obj The RAM or matrix [OpenMx::mxModel()], or [OpenMx::mxMatrix()] that you want to set start values for.
#' @param sd Optional Standard Deviation for start values
#' @param n Optional Mean for start values
#' @param onlyTouchZeros Don't alter parameters that have starts (useful to speed [umxModify()])
#' @return - [OpenMx::mxModel()] with updated start values
#' @export
#' @seealso - Core functions:
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>

#' @examples
#' \dontrun{
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
#' 
#' }
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

		varNames = dimnames(obj$matrices$S$values)[[1]]
		if (is.null(varNames)) varNames = c(manifests, latents)
		
		# Total variance map (latents default to 1.0)
		total_vars = rep(1.0, length(varNames))
		names(total_vars) = varNames
		
		# ==============
		# = Set means  =
		# ==============
		if(is.null(obj$matrices$M)){
			if(type == "raw"){
				covData = umx_var(df = theData[, manifests, drop = FALSE], format = "full", ordVar = 1, use = "pairwise.complete.obs", allowCorForFactorCovs=TRUE)
			} else if (type %in% c("cov", "cor")){
				covData = as.matrix(theData)
			} else if (identical(type, "summary")) {
				osCov = tryCatch(obj$data$observedStats$cov, error = function(e) NULL)
				if (!is.null(osCov) && is.matrix(osCov)) {
					covData = as.matrix(osCov)
				} else if (is.matrix(theData)) {
					covData = as.matrix(theData)
				} else {
					stop("xmuValues: type='summary' without observedStats$cov. Known types: raw, cov, cor, and summary WLS via observedStats.", call. = FALSE)
				}
			} else if (identical(type, "none") || identical(type, "acov")) {
				stop("xmuValues: type=", omxQuotes(type), " is not supported (legacy WLS data API removed). Use type='summary' with observedStats = list(cov=S, useWeight=W, asymCov=V), or type raw/cov/cor.", call. = FALSE)
			} else {
				message("xmuValues can't recognise data of type ", omxQuotes(type), ". I know raw, cov, cor, and summary WLS (observedStats$cov).")
				covData = as.matrix(theData)
			}
		} else {
			dataMeans = umx_means(theData[, manifests, drop = FALSE], ordVar = 0, na.rm = TRUE)
			freeManifestMeans = (obj$matrices$M$free[1, manifests] == TRUE)
			obj$M@values[1, manifests][freeManifestMeans] = dataMeans[freeManifestMeans]
			covData = umx_var(df = theData[, manifests, drop = FALSE], format = "full", ordVar = 1, use = "pairwise.complete.obs", allowCorForFactorCovs=TRUE)
		}
		
		# Populate manifest variances into total_vars map
		if (!is.null(covData) && is.matrix(covData) && nrow(covData) > 0) {
			covNames = rownames(covData)
			validNames = covNames[covNames %in% varNames]
			total_vars[validNames] = diag(covData)[validNames]
		}
		
		# ==========================================================
		# = Fill the S (symmetrical) matrix with good start values =
		# ==========================================================
		# Set S diagonal (variances)
		freeDiags = diag(obj$S$free) == TRUE
		if(onlyTouchZeros) freeDiags = freeDiags & (diag(obj$S$values) == 0)
		
		# Set manifest residuals to 50% of observed variance, latents stay at 1.0
		diag_scales = total_vars
		valid_manifests = manifests[manifests %in% varNames]
		diag_scales[valid_manifests] = 0.5 * diag_scales[valid_manifests]
		diag(obj$S@values)[freeDiags] = diag_scales[freeDiags]
		
		# Set S off-diagonal (covariances)
		if (onlyTouchZeros) {
			freeOff = (obj$S$free == TRUE) & (obj$S$values == 0) & (!diag(length(varNames)))
		} else {
			freeOff = (obj$S$free == TRUE) & (!diag(length(varNames)))
		}
		cov_matrix = outer(total_vars, total_vars, function(r, c) 0.3 * sqrt(r * c))
		obj$S@values[freeOff] = cov_matrix[freeOff]
		
		# ======================================================
		# = Put scaled starts into the asymmetric (one headed) =
		# ======================================================
		freePaths = obj$matrices$A$free == TRUE
		if(onlyTouchZeros) freePaths = freePaths & (obj$matrices$A$values == 0)
		
		# Scale path = 0.5 * sqrt(Var(Row) / Var(Col))
		scale_matrix = outer(total_vars, total_vars, function(r, c) 0.5 * sqrt(r / c))
		obj$A@values[freePaths] = scale_matrix[freePaths]
		
		return(obj)
	} else if (umx_is_LISREL(obj)) {
		# This is a LISREL Model: Set sane starting values
		if (length(obj$submodels) > 0) {
			stop("xmuValues cannot yet handle sub-models for LISREL.")
		}
		if (is.null(obj$data)) {
			stop("'model' does not contain any data")
		}
		
		theData = obj$data$observed
		type = obj$data$type
		
		manifestsY = if(!is.null(obj$LY)) rownames(obj$LY$values) else c()
		manifestsX = if(!is.null(obj$LX)) rownames(obj$LX$values) else c()
		latentsEta = if(!is.null(obj$LY)) colnames(obj$LY$values) else c()
		latentsXi  = if(!is.null(obj$LX)) colnames(obj$LX$values) else c()
		
		# TE: residual variance of Y manifests
		if(!is.null(obj$TE)) {
			freeTE = diag(obj$TE$free) == TRUE
			if(onlyTouchZeros) freeTE = freeTE & diag(obj$TE$values) == 0
			if(length(manifestsY) > 0) {
				varsY = umx_var(theData[, manifestsY, drop = FALSE], format = "diag", ordVar = 1, use = "pairwise.complete.obs")
				diag(obj$TE@values)[freeTE] = varsY[freeTE]
			}
		}
		
		# TD: residual variance of X manifests
		if(!is.null(obj$TD)) {
			freeTD = diag(obj$TD$free) == TRUE
			if(onlyTouchZeros) freeTD = freeTD & diag(obj$TD$values) == 0
			if(length(manifestsX) > 0) {
				varsX = umx_var(theData[, manifestsX, drop = FALSE], format = "diag", ordVar = 1, use = "pairwise.complete.obs")
				diag(obj$TD@values)[freeTD] = varsX[freeTD]
			}
		}
		
		# PS: residual variance of eta latents
		if(!is.null(obj$PS)) {
			freePS = diag(obj$PS$free) == TRUE
			if(onlyTouchZeros) freePS = freePS & diag(obj$PS$values) == 0
			diag(obj$PS@values)[freePS] = 1
		}
		
		# PH: variance of xi latents
		if(!is.null(obj$PH)) {
			freePH = diag(obj$PH$free) == TRUE
			if(onlyTouchZeros) freePH = freePH & diag(obj$PH$values) == 0
			diag(obj$PH@values)[freePH] = 1
		}
		
		# Path coefficients: LX, LY, BE, GA
		for(matName in c("LX", "LY", "BE", "GA")) {
			mat = obj[[matName]]
			if(!is.null(mat)) {
				freePaths = mat$free == TRUE
				if(onlyTouchZeros) freePaths = freePaths & mat$values == 0
				obj[[matName]]@values[freePaths] = 0.9
			}
		}
		
		# Means: TY, TX
		if(!is.null(obj$TY) && length(manifestsY) > 0) {
			meansY = umx_means(theData[, manifestsY, drop = FALSE], ordVar = 0, na.rm = TRUE)
			freeTY = obj$TY$free[manifestsY, 1] == TRUE
			if(onlyTouchZeros) freeTY = freeTY & obj$TY$values[manifestsY, 1] == 0
			obj$TY@values[manifestsY, 1][freeTY] = meansY[freeTY]
		}
		if(!is.null(obj$TX) && length(manifestsX) > 0) {
			meansX = umx_means(theData[, manifestsX, drop = FALSE], ordVar = 0, na.rm = TRUE)
			freeTX = obj$TX$free[manifestsX, 1] == TRUE
			if(onlyTouchZeros) freeTX = freeTX & obj$TX$values[manifestsX, 1] == 0
			obj$TX@values[manifestsX, 1][freeTX] = meansX[freeTX]
		}
		
		# Latent intercepts/means: AL, KA
		if(!is.null(obj$AL)) {
			freeAL = obj$AL$free == TRUE
			if(onlyTouchZeros) freeAL = freeAL & obj$AL$values == 0
			obj$AL@values[freeAL] = 0
		}
		if(!is.null(obj$KA)) {
			freeKA = obj$KA$free == TRUE
			if(onlyTouchZeros) freeKA = freeKA & obj$KA$values == 0
			obj$KA@values[freeKA] = 0
		}
		
		return(obj)
	} else {
		stop("'obj' must be an mxMatrix, a RAM model, or a simple number")
	}
}

#' xmuLabel: Add labels to a RAM model, matrix, or path
#'
#' xmuLabel adds labels to things, be it an: [OpenMx::mxModel()] (RAM or matrix based), an [OpenMx::mxPath()], or an [OpenMx::mxMatrix()]
#' This is a core function in umx: Adding labels to paths opens the door to [umxEquate()], as well as [OpenMx::omxSetParameters()]
#'
#' @param obj An [OpenMx::mxModel()] (RAM or matrix based), [OpenMx::mxPath()], or [OpenMx::mxMatrix()]
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
#' @return - [OpenMx::mxModel()]
#' @export
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>

#' @examples
#' \dontrun{
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
#' }
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
#' @return - [OpenMx::mxMatrix()]
#' @export
#' @family Core Model Building Functions
#' @seealso - [xmu_simplex_corner()], [OpenMx::mxMatrix()], [xmuLabel()], [umxRAM()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>

#' @examples
#' \dontrun{
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
#' }
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
#' @return - [OpenMx::mxAlgebra()]
#' @export
#' @family Advanced Model Building Functions	
#' @seealso - [umxMatrix()]

#' @examples
#' \dontrun{
#' A = umxMatrix("A", "Full", nrow = 3, ncol = 3, values=2)
#' B = umxAlgebra("B", A)
#' C = umxAlgebra(A + B, name = "C")
#' D = umxAlgebra(sin(C), name = "D")
#' m1 = mxRun(mxModel("AlgebraExample", A, B, C, D ))
#' mxEval(D, m1)
#' 	
#' x = umxAlgebra("circ", expression = 2 * pi)
#' class(x$formula)
#' x = mxAlgebra(name = "circ", 2 * pi)
#' class(x$formula) # "call"
#' }
umxAlgebra <- function(name = NA, expression, dimnames = NA, ..., joinKey=as.character(NA), joinModel=as.character(NA), verbose=0L, initial=matrix(as.numeric(NA),1,1), recompute=c('always','onDemand'), fixed = "deprecated_use_recompute") {
	message("umxAlgebra is not working yet: contribute here https://github.com/tbates/umx/issues/199 if you'd like this finished... ")
	if(!inherits(name, "character")){
		stop("In umxAlgebra, name comes first, not expression.")
	}

	formula = match.call()$expression
	x = mxAlgebra(formula, name = name, dimnames = dimnames, ..., joinKey=joinKey, joinModel=joinModel, verbose=verbose, initial=initial, recompute = recompute)
	return(x)
}


# =================================
# = Run Helpers =
# =================================

#' umxRun: Run an mxModel
#'
#' `umxRun` is a version of [OpenMx::mxRun()] which can run also set start values, labels, and run multiple times
#' It can also calculate the saturated and independence likelihoods necessary for most fit indices.
#' **Note** this is not needed for umxRAM models or twin models - it is just a convenience to get base OpenMx models to run.
#' @param model The [OpenMx::mxModel()] you wish to run.
#' @param tryHard  How to tryHard. Default = "yes". Alternatives "no", "ordinal", "search"
#' @param calc_sat Whether to calculate the saturated and independence models (for raw [OpenMx::mxData()] [OpenMx::mxModel()]s)
#' @param setValues Whether to set the starting values of free parameters (default = FALSE)
#' @param setLabels Whether to set the labels (default =  FALSE)
#' @param optimizer optional to set the optimizer.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE) intervals = FALSE
#' @param summary Whether to print summary or not (default = !umx_set_silent() )
#' @param comparison Comparison model (will be used to drive umxCompare() after umxRun
#' @return - [OpenMx::mxModel()]
#' @family Advanced Model Building Functions
#' @references - <https://github.com/tbates/umx>
#' @export

#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 = mxModel("fact", type="RAM", manifestVars=manifests, latentVars=latents,
#' 	mxPath(latents  , to = manifests),
#' 	mxPath(manifests, arrows = 2),
#' 	mxPath(latents  , arrows = 2, free = FALSE, values = 1),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs=500)
#' )
#'
#' m1 = umxRun(m1) # just run: will create saturated model if needed
#' m1 = umxRun(m1, setValues = TRUE, setLabels = TRUE) # set start values and label all parameters
#' umxSummary(m1, std = TRUE)
#' m1 = mxModel(m1, mxCI("G_to_x1")) # add one CI
#' m1 = mxRun(m1, intervals = TRUE)
#' residuals(m1, run = TRUE) # get CIs on all free parameters
#' confint(m1) # OpenMx's SE-based CIs
#' umxConfint(m1, run = TRUE) # get likelihood-based CIs on all free parameters
#' m1 = umxRun(m1, tryHard = "yes")
#' }
#' 
# type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"),
umxRun <- function(model, tryHard = c( "yes", "no", "ordinal", "search"), calc_sat = TRUE, setValues = FALSE, setLabels = FALSE, summary = !umx_set_silent(silent = TRUE), intervals = FALSE, optimizer = NULL, comparison = NULL){
	# TODO: umxRun: Return change in -2LL for models being re-run
	# TODO: umxRun: Stash saturated model for re-use
	# TODO: umxRun: Optimise for speed
	tryHard = match.arg(tryHard)

	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}

	if(setLabels){
		model = xmuLabel(model)
	}
	if(setValues){
		model = xmuValues(model)
	}
	
	was_run = umx_has_been_run(model)
	old_fit = NULL
	if(was_run){
		old_fit = tryCatch(model$output$fit, error = function(e) NULL)
	}

	model = xmu_safe_run_summary(model, autoRun = TRUE, intervals=intervals, summary = summary, tryHard =  tryHard)

	if(was_run && !is.null(old_fit)){
		new_fit = tryCatch(model$output$fit, error = function(e) NULL)
		if(!is.null(new_fit) && !is.na(old_fit) && !is.na(new_fit)){
			diff_fit = old_fit - new_fit
			if(xmu_is_wls(model)){
				message(sprintf("Change in WLS Chi-Square (old - new) = %.3f", diff_fit))
			} else {
				message(sprintf("Change in -2LL (old - new) = %.3f", diff_fit))
			}
		}
	}

	if(calc_sat){
		if(umx_has_been_run(model) && !xmu_is_wls(model)){
			if(umx_is_RAM(model)){
				if(model$data$type == "raw"){
					# If we have a RAM model with raw data, compute the saturated and independence models
					# message("computing saturated and independence models so you have access to absolute fit indices for this raw-data model")
					ref_models = mxRefModels(model, run = TRUE)
					model@output$IndependenceLikelihood = as.numeric(-2 * logLik(ref_models$Independence))
					model@output$SaturatedLikelihood    = as.numeric(-2 * logLik(ref_models$Saturated))
				}
			}
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
#' `umxSetParameters` is used to alter values, and other parameter properties in an [OpenMx::mxModel()].
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
#' and, because this can create duplicate labels, we also call [OpenMx::omxAssignFirstParameters()]
#' to equate the start values for parameters which now have identical labels.
#' 
#' @param model an [OpenMx::mxModel()] to set parameters in.
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
#' @return - [OpenMx::mxModel()]
#' @export
#' @family Model Summary and Comparison
#' @seealso - [umxModify()], [xmuLabel()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>

#' @examples
#' \dontrun{
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
#  # example showing all updated with an "m1_" in front
#' umxSetParameters(m1, regex = "^", newlabels= "m1_", test = TRUE)
#'
#' # Change path to x1 to x2, equating these two paths
#' m2 = umxSetParameters(m1, "G_to_x1", newlabels= "G_to_x2", test = FALSE)
#' m2 = umxRun(m2) # umxSetParameters does not re-run he model, so make sure you do!
#' parameters(m2)
#' 
#' }
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
#' @param model   An [OpenMx::mxModel()] within which to equate parameters listed in "a" with those in "b"
#' @param a  one or more labels to equate with those in the "b" set.
#' @param b  one or more labels to equate with those in the 'a' set. (if 'newlabels' is NULL, labels will be set to 'a' list).
#' @param newlabels (optional) list of new labels for the equated parameters.
#' @param free    Must the parameter(s) initially be free? (default = TRUE)
#' @param verbose Whether to give verbose feedback (default = TRUE)
#' @param name    name for the returned model (optional: Leave empty to leave name unchanged)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param master  synonym for 'a'
#' @param slave   synonym for 'b'
#' @return - [OpenMx::mxModel()]
#' @export
#' @seealso [umxModify()], [umxCompare()]
#' @family Model Summary and Comparison
#' @references - <https://github.com/tbates/umx>

#' @examples
#' \dontrun{
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
#' }
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
#' @param model an [OpenMx::mxModel()] within which to fix free parameters
#' @param verbose whether to mention how many paths were fixed (default is FALSE)
#' @param name optional new name for the model. if you begin with a _ it will be made a suffix
#' @param run  whether to fix and re-run the model, or just return it (defaults to FALSE)
#' @return - the fixed [OpenMx::mxModel()]
#' @export
#' @family Advanced Model Building Functions
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>

#' @examples
#' \dontrun{
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
#' 
#' }
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
	method = match.arg(method)
	if(method=="allFree"){
		verbose = FALSE
	}
	if(any(selDVs != "deprecated")){
		message("Polite note: please use fullVarNames instead of selDVs when calling umxThresholdMatrix")
		fullVarNames = selDVs
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
	if(droplevels){ stop("Not sure it's wise to drop levels... let me know if you have a case where this is legit") }
	
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
	} else if(verbose){
		message(threshMatName, ": ", nBinVars, " binary, ", nOrdVars, " ordinal (Mehta first-2 thresholds fixed when nThresh>1).")
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

	# Identification lectures removed: high-level builders call xmu_threshold_id_RAM / twin supers.
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
		tab = table(thisCol)/sum(table(thisCol)) # Simple table of % values occuring at each threshold
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
				if(length(zValues[!is.infinite(zValues)])==0){
					zValues[zValues == (Inf)] = seq(from = .1, by = .1, length.out = nPlusInf)
				} else {
					maxOK = max(zValues[!is.infinite(zValues)])
					padding = seq(from = (maxOK + .1), by = .1, length.out = nPlusInf)
					zValues[zValues == (Inf)] = padding
				}
			}
			if(nMinusInf){
				if(length(zValues[!is.infinite(zValues)])==0){
					zValues[zValues == (-Inf)] = seq(from = -.1, by = .1, length.out = nPlusInf)
				} else {
					minOK = min(zValues[!is.infinite(zValues)])
					padding = seq(from = (minOK - .1), by = (- .1), length.out = nMinusInf)
					zValues[zValues == (-Inf)] = padding
				}
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
#' @description This function is used to easily and compactly specify paths in models. In addition to
#' `from` and `to`, it adds specialised parameters for variances (var), two headed paths (with) and means (mean).
#' There are also new terms to describe fixing values: `fixedAt` and `fixFirst`.
#' To give a couple of the most common, time-saving examples:
#'
#' * `umxPath("A", with = "B",  fixedAt = 1)`
#' * `umxPath(var = c("A", "B"),  fixedAt = 1)`
#' * `umxPath(v.m. = manifests)`# free variance and mean estimation
#' * `umxPath(v1m0 = latents)` # fixed at var 1 mean 0
#' * `umxPath(means = manifests)`
#' * `umxPath(fromEach = c('A',"B","C"), to = c("y1","y2"))`
#' * `umxPath(unique.bivariate = c('A',"B","C"))`
#' * `umxPath("A", to = c("B","C","D"),  firstAt = 1)`
#'
#' @details
#' `umxPath` introduces the following new words to your path-defining vocabulary: `with`, `var`, `cov`, `means`, `v1m0`, 
#' `v0m0`, `v.m0`, `v.m`, `fixedAt`, `freeAt`, `firstAt`, `unique.bivariate`, `unique.pairs`, `fromEach`, `Cholesky`, `defn`, `forms`.
#'
#' `with` creates covariances (2-headed paths):
#' `umxPath(A, with = B)`
#' 
#' Specify a variance for A with
#' `umxPath(var = "A")`.
#' 
#' Of course you can use vectors anywhere:
#' `umxPath(var = c('N','E', 'O'))`
#' 
#' To specify a mean, you just say:
#' `umxPath(mean = "A")`, which is equivalent to `mxPath(from = "one", to = "A")`.
#' 
#' To fix a path at a value, you can say:
#' `umxPath(var = "A", fixedAt = 1)`
#' 
#' The common task of creating a variable with variance fixed at 1 and mean at 0 is done thus:
#' `umxPath(v1m0 = "A")`
#' 
#' For free variance and means use:
#' `umxPath(v.m. = "A")`
#' 
#' `umxPath` exposes `unique.bivariate` and `unique.pairs`, So to create paths A<->A, B<->B, 
#' and A->B, you would say:
#' `umxPath(unique.pairs = c('A',"B"))` 
#' 
#' To create paths A<->B, B<->C, and A<->C, you would say:
#' `umxPath(unique.bivariate = c('A',"B","C"))`
#'
#' Creates one-headed arrows on the all.bivariate pattern
#' `umxPath(fromEach = c('A',"B","C"))`
#'
#' Setting up a latent trait, you can scale with a fixed first path thus:
#'
#' `umxPath("A", to = c("B","C","D"),  firstAt = 1)`  
#'
#' To create Cholesky-pattern connections:
#' 
#' `umxPath(Cholesky = c("A1", "A2"), to c("var1", "var2"))`
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
#' @param v0m. variance fixed at 0, mean free.
#' @param fixedAt Equivalent to setting "free = FALSE, values = fixedAt"
#' @param freeAt Equivalent to setting "free = TRUE, values = freeAt"
#' @param firstAt First path is fixed at this value (free is ignored: warning if other than a single TRUE)
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
#' @return - 1 or more [OpenMx::mxPath()]s
#' @export
#' @family Core Model Building Functions
#' @seealso - [OpenMx::mxPath()]
#' @references - <https://tbates.github.io>

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
#' umxPath(defn = "mpg") # create latent called def_mpg, with var = 1 and label = "data.mpg"
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
#' # ======================================================================
#' # = Definition variable example. for a RAM model                       =
#' # = def vars are instantiated as dummy latents with data on the "mean" = 
#' # ======================================================================
#' library(umx); libs("MASS") # for mvrnorm()
#' # 1. Create Data
#' N = 500 # size of each group
#' Sigma  = matrix(c(1,.5,.5,1),2,2) # cov (.5)
#' group1 = MASS::mvrnorm(N, c(1,2), Sigma)
#' group2 = MASS::mvrnorm(N, c(0,0), Sigma)
#' # rbind groups and name cols "x" and "y"
#' xy = rbind(group1, group2)
#' dimnames(xy)[2]= list(c("x", "y"))
#' 
#' # Create a definition variable for group status
#' groupID = rep(c(1,0), each = N) 
#' df = data.frame(xy, groupID = groupID)
#'
#' # Make the model with a definition variable on means
#' m1 = umxRAM("Def Means", data = df,
#' 	umxPath(v.m. = c("x","y")),
#' 	umxPath("x", with = "y"),
#'  # create a unit latent called "def_groupID" with data "data.groupID"
#' 	umxPath(defn = "groupID"),
#'  # Add it to the x and y means
#' 	umxPath("def_groupID", to = c("x", "y"))
#' )
#' plot(m1)
#'
#' }
umxPath <- function(from = NULL, to = NULL, with = NULL, var = NULL, cov = NULL, means = NULL, v1m0 = NULL, v.m. = NULL, v0m0 = NULL, v.m0 = NULL, v0m. = NULL, fixedAt = NULL, freeAt = NULL, firstAt = NULL, unique.bivariate = NULL, unique.pairs = NULL, fromEach = NULL, forms = NULL, Cholesky = NULL, defn = NULL, connect = c("single", "all.pairs", "all.bivariate", "unique.pairs", "unique.bivariate"), arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA, hasMeans = NULL) {
	connect = match.arg(connect) # Set to single if not overridden by user.
	# xmu_string2path(from)
	n = 0
	for (i in list(with, cov, var, forms, means, fromEach, unique.bivariate, unique.pairs, v.m. , v1m0, v0m0, v.m0, v0m., defn, Cholesky)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n > 1){
		stop("At most one of with, cov, var, forms, means, fromEach, unique.bivariate, unique.pairs, v1m0, v.m., v0m0, v.m0, v0m., defn, or Cholesky can be set: Use at one time")
	} else if(n == 0){
		# check that from is set?
		if(is.null(from)){
			stop("You must set at least 'from'")
		}	
	}

	n = 0
	for (i in list(v.m. , v1m0, v0m0, v0m., v.m0)) {
		if(!is.null(i)){ n = n + 1}
	}
	if(n && !is.null(fixedAt)){
		stop("I stopped processing the model: When you use v.m. , v1m0, v0m0, v.m0, or v0m. you can't also set fixedAt")
	}
	if(n && !is.null(firstAt)){
		stop("I stopped processing the model: When you use v.m. , v1m0, v0m0, v.m0, or v0m. you can't also set firstAt")
	}

	if(!is.null(defn)){
		if(anyNA(labels)){
			labels = paste0("data.", defn)
			defn   = paste0("def_" , defn)
			nDef = length(defn)
			if(nDef == 1){
				message(nDef, " definition variable created: refer to it as: ", omxQuotes(defn))
			} else {
				message(nDef, " definition variables created: refer to them as: ", omxQuotes(defn))
			}
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
	if(!is.null(v0m.)){
		if(length(values)!=1 || !is.na(values)){
			if(length(values)==2){
				varValue = values[1]
				meanValue = values[2]
			} else if(length(values)==1){
				varValue = 0
				meanValue = values
			} else {
				stop("Managing which values apply to variances and which to means is error prone for more than one variable: Please do them 1 at a time\n")
			}
		}else{
			varValue  = 0
			meanValue = 0			
		}
		if(any(!is.na(labels))){
			if(length(labels)==2){
				a = mxPath(from = v0m., arrows = 2, free = FALSE, values = varValue, labels = labels[1])
				b = mxPath(from = "one", to = v0m., free = TRUE , values = meanValue, labels = labels[2])
			} else {
				stop("Managing which labels apply to the variances and which to the means is error prone:\n",
				"I suggest you call: umxPath(var) and umxPath(means=) separately"
				)
			}
		} else {
			a = mxPath(from = v0m., arrows = 2, free = FALSE, values = varValue)
			b = mxPath(from = "one", to = v0m., free = TRUE , values = meanValue)
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

