#' Build and run path-based Double-entry SEM models including censored data
#'
#' @description
#' `umxRAM_DE` expedites creation of structural equation models which inlcude censored data, enabled by support for double-entry, 
#' style pairs of `var_cens` and `var_cont` columns. It handles the doubling of paths, all the complex threshold set up and equating of weights.
#' Users still just used [umxPath()] to specify the model.
#' 
#' Fully continuous variables may be mixed with double-entry pairs in `selDVs`
#' (e.g. `c("ht", "wt_cont", "wt_cens")`). Prepare the censored traits with
#' [umx_make_double_entry_data()]. At least one contiguous `_cont`/`_cens` pair is required;
#' for all-continuous models use [umxACE()].
#'
#' Here's a path example that models miles per gallon (mpg) as a function of weight (wt) and engine displacement (disp)
#' using the widely used `mtcars` data set.
#' 
#' ```
#' # 1: Create the censored data
#' # Just for a demo, we will create a censored litres column, censoring at 2.
#' mtcars$litres = mtcars$litres
#' tmp = umx_make_double_entry_data(mtcars, cols = list(litres= 2), sep="", nSib=1)
#' # 2: Create the censored data
#' # Just for a demo, we will create a censored litres column, censoring at 2.
#' m1 = umxRAM_DE("test", data = mtcars,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(v.m. = c("wt", "disp", "mpg"))
#' )
#' ```
#'
#' As you can see, most of the work is done by [umxPath()]. `umxRAM_DE` wraps these paths up, takes the `data =` input, and 
#' then internally doubles the paths for DE variables, equates them and sets everything up. By defual tit will also run it.
#' 
#' Try it, or one of the several models in the examples at the bottom of this page.
#' 
#' **Gotchas**
#' A common error is to include data in the main list, a bit like
#' saying `lm(y ~ x + df)` instead of `lm(y ~ x, data = df)`.
#' 
#' **nb**: Because it uses the presence of a variable in the data to detect if a variable is latent or not, `umxRAM` needs data at build time.
#'
#' If you are used to hacking a quick model with [lavaan string syntax][umxLav2RAM()], use [umxRAM()] at present. Likewise, if you are at the "sketching" stage of theory consideration, `umxRAM` supports that, umxRAM_DE does not.
#' 
#' @details
#' **WLS**
#' `umxRAM` supports WLS estimation via the `type` argument (`"WLS"`, `"DWLS"`, or `"ULS"`).
#'
#' **Important for ordinal data**: If your data contains ordered factors, `umxRAM` will
#' automatically create the necessary `mxThreshold` objects. You **do not** need to add them manually.
#'
#' For all-continuous data, use `allContinuousMethod` to control means modeling:
#' - `"cumulants"` (default): Faster. No means model.
#' - `"marginals"`: Includes means and supports missing data.
#'
#' @param model A model to update (or set to string to use as name for new model)
#' @param data data for the model. Can be an [OpenMx::mxData()] or a data.frame
#' @param ... umxPaths, mxThreshold objects, etc.
#' @param group (optional) Column name to use for a multi-group model (default = NULL)
#' @param group.equal In multi-group models, what to equate across groups (default = NULL: all free)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param suffix String to append to each label (useful if model will be used in a multi-group model)
#' @param name A friendly name for the model
#' @param type One of "Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param weight Passes weight values to mxData
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
#' @return - [OpenMx::mxModel()]
#' @export 
#' @seealso [umxPath()], [umxSummary()], [plot()], [parameters()], [umxSuperModel()], [umxLav2RAM()]
#' @family Core Model Building Functions
#' @references - <https://tbates.github.io>, <https://github.com/tbates/umx>

#' @examples
#' # ============================================
#' # = 1. Here's a simple example with raw data =
#' # ============================================
#' # make the double entry data for the appropriate columns (litres here)
#' data(mtcars)
#' mtcars$litres = mtcars$disp/61.02
#' tmp = umx_make_double_entry_data(mtcars, cols = list(litres= 2), sep="", nSib=1)
#' print(tmp)
#' # ## Double-Entry Data Summary (umxACE_DE)
#' # 
#' # * Dataset: 32 rows x 14 columns
#' # * Twin Structure: nSib = 1, separator = ""
#' # * Variable Suffixes: continuous = "_cont", censored = "_cens"
#' # 
#' # |Trait  |Continuous Col |Censored Col |Censor Rule |Side |Cut Value |Fixed Cut? |N Censored |% Censored |
#' # |:------|:--------------|:------------|:-----------|:----|:---------|:----------|:----------|:----------|
#' # |litres |litres_cont    |litres_cens  |2           |left |2         |Yes        |9          |28.1%      |
#' # 
#' # ---
#' #    litres litres_cont litres_cens
#' #  2.622091    2.622091        <NA>
#' #  2.622091    2.622091        <NA>
#' #  1.769912          NA    censored
#' #  4.228122    4.228122        <NA>
#' #  5.899705    5.899705        <NA>
#' #  3.687316    3.687316        <NA>
#'
#' m1 = umxRAM("testRAM_DE", data = mtcars,
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
#' # 3. Of course you can plot the model
#' plot(m1)
#' plot(m1, std=TRUE, means=FALSE)
#' plot(m1, std = TRUE, means=FALSE, strip= TRUE, resid = "line")
#'
umxRAM <- function(model = NA, ..., data = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), weight = NULL, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, lavaanMode = c("sem", "lavaan"), printTab = FALSE) {
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	# Check for data/model objects passed in ... before unlist() flattens them
	for (item in dot.items) {
		thisIs = class(item)[1]
		if (thisIs %in% c("data.frame", "matrix", "MxData")) {
			stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me a ", thisIs, " inside the path list. ",
				 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
		} else if (thisIs == "MxModel") {
			stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me an MxModel inside the path list. ",
				 "umxRAM does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
		}
	}
	dot.items = unlist(dot.items) # In case any dot items are lists of mxPaths, etc...
	type       = match.arg(type)
	tryHard    = match.arg(tryHard)
	lavaanMode = match.arg(lavaanMode)
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
				}
			}
			# if(setValues){
			# 	newModel = xmuValues(newModel)
			# }
			newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
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
		for (i in seq_along(thisItem)) {
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
				} else if (isS4(thisItem[[i]]) && grepl("^Mx", thisIs) && !thisIs %in% c("MxModel", "MxData")) {
					# Valid OpenMx S4 object (MxConstraint, MxAlgebra, MxCI, etc.) - no path/matrix-label actions needed
				} else {
					if (thisIs %in% c("data.frame", "matrix", "MxData")) {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " inside the path list. ",
							 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
					} else if (thisIs == "MxModel") {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me an MxModel inside the path list. ",
							 "umxRAM does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
					} else {
						stop("umxRAM can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " which is not supported inside the RAM path list.", call. = FALSE)
					}
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

  # Include weight if it is passed
  if (!is.null(weight)) unusedManifests = setdiff(c(manifestVars, weight), c(foundNames, defnNames))

	if(remove_unused_manifests & length(unusedManifests) > 0){
		usedManifests = setdiff(intersect(manifestVars, foundNames), "one")
    if (!is.null(weight)) {
        myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
            defnNames, verbose = verbose, weight = weight)
    } else {
        myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = 
            defnNames, verbose = verbose)
    }
	} else {
		# keep everything
		usedManifests = setdiff(manifestVars, defnNames)
		myData = xmu_make_mxData(data= data, type = type, verbose = verbose, manifests = usedManifests, fullCovs = 
            defnNames)
	}
	# ==========================================================
	# = Topologically sort manifestVars and latentVars         =
	# ==========================================================
	all_nodes = c(latentVars, usedManifests)
	if(length(all_nodes) > 1) {
		from_list = character(0)
		to_list   = character(0)
		for(item in dot.items) {
			if(inherits(item, "MxPath") && item@arrows == 1) {
				for(f in item@from) {
					for(t in item@to) {
						from_list = c(from_list, f)
						to_list   = c(to_list, t)
					}
				}
			}
		}
		if(length(from_list) > 0) {
			sorted_nodes = xmu_topo_sort(from_list, to_list, all_nodes)
			latentVars    = intersect(sorted_nodes, latentVars)
			usedManifests = intersect(sorted_nodes, usedManifests)
		}
	}

	# ==================
	# = Assemble model =
	# ==================
	if (type %in% c("WLS", "DWLS", "ULS") && !is.null(data) && inherits(data, "data.frame")) {
		summaryObj = umx_is_ordered(data[, usedManifests, drop = FALSE], summaryObject = TRUE)
		if (is.null(summaryObj$nFactors) || summaryObj$nFactors == 0) {
			message("*Polite note*: Your data are continuous. WLS is typically reserved only for categorical data. Perhaps you want robust fit statistics? In that case remove the type= parameter and use robust methods in the summary: umxSummary(model, ..., uncertainty = \"MLR\").")
		}
	}

	newModel = mxModel(name = name, type = "RAM",
		manifestVars = usedManifests,
		latentVars  = latentVars,
		independent = independent, dot.items
	)

	# ============
	# = Add data =
	# ============
	if (inherits(myData, "character")){
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
	needsMeans = xmu_check_needs_means(data = myData, type = type, allContinuousMethod = allContinuousMethod)
	if(needsMeans && is.null(newModel$matrices$M)){
		# Continuous + ordinal: free means. Binary: mean@0 and residual@1 (Mehta/binary ID).
		summaryObj = umx_is_ordered(myData$observed[, usedManifests, drop = FALSE], summaryObject = TRUE)
		binVars = intersect(summaryObj$binVarNames, usedManifests)
		nonBinVars = setdiff(usedManifests, binVars)
		noteBits = character(0)

		newPaths = list()
		if (length(nonBinVars) > 0) {
			newPaths[[length(newPaths) + 1]] = mxPath("one", to = nonBinVars)
			noteBits = c(noteBits, paste0("free means for ", paste(nonBinVars, collapse = ", ")))
		}
		if (length(binVars) > 0) {
			newPaths[[length(newPaths) + 1]] = mxPath("one", to = binVars, free = FALSE, values = 0)
			noteBits = c(noteBits, paste0("binary mean@0 for ", paste(binVars, collapse = ", ")))
		}
		if (length(newPaths) > 0) {
			newModel = mxModel(newModel, newPaths)
		}
		# Binary residual variance fixed at 1 when S cell exists or must be added
		if (length(binVars) > 0) {
			for (v in binVars) {
				if (!is.null(newModel$S) && !is.null(dimnames(newModel$S$values)) && v %in% rownames(newModel$S$values)) {
					if (isTRUE(newModel$S$free[v, v]) || !isTRUE(all.equal(as.numeric(newModel$S$values[v, v]), 1))) {
						newModel$S$free[v, v] = FALSE
						newModel$S$values[v, v] = 1
					}
				} else {
					newModel = mxModel(newModel, mxPath(from = v, arrows = 2, free = FALSE, values = 1))
				}
			}
			noteBits = c(noteBits, paste0("binary residual@1 for ", paste(binVars, collapse = ", ")))
		}
		if (length(noteBits) > 0) {
			message("umx note: no means model; added ", paste(noteBits, collapse = "; "), " (see ?umxThresholdMatrix).")
		}
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
		newModel = xmuRAM2Ordinal(newModel, verbose = FALSE)
	}

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if(type %in%  c('WLS', 'DWLS', 'ULS')) {
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
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
	invisible(newModel)
}
