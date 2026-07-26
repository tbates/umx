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
#'		umxPath("G", to = manifests),
#'		umxPath(var = manifests),
#'		umxPath(var = "G", fixedAt = 1)
#' )
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
