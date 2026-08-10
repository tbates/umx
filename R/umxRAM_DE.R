#' Build and run path-based Double-entry SEM models including censored data
#'
#' @description
#' `umxRAM_DE` expedites creation of structural equation models which include censored data, enabled by support for double-entry,
#' style pairs of `var_cens` and `var_cont` columns. It handles the doubling of paths, all the complex threshold set up and equating of weights.
#' Users still just used [umxPath()] to specify the model.
#' 
#' Fully continuous variables may be mixed with double-entry pairs. Prepare the censored traits with
#' [umx_make_double_entry_data()]. At least one `DEvar` is required.
#'
#' As you can see from the examples below, most of the work is done by [umxPath()]. `umxRAM_DE` wraps these paths up, takes the `data =` input, and 
#' then internally doubles the paths for DE variables, equates them and sets everything up. By default it will also run it.
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
#' @param DEvars A character vector of base names for double-entry variables (e.g. `c("litres")` creates `litres_cont`/`litres_cens`). Base names only; do not include suffix.
#' @param doubleEntrySuffix Suffixes for the continuous and censored variables (default = c("_cont", "_cens")).
#' @param fixCensorThresholds One of `c("yes","auto","no")`. `"yes"` fix every DE pair from `censorCuts`/prep attr; `"auto"` fix only pairs with finite known cut; `"no"` free thresholds.
#' @param censorCuts Optional named numeric vector of known cuts on analysis scale. Names may be base (`"litres"`), `"_cont"` or `"_cens"` form.
#' @param sep Separator used in prep (default `NULL` infers from `attr(data,"umxDoubleEntry")$sep` or `""`).
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
#' @return - [OpenMx::mxModel()]
#' @export 
#' @seealso [umxPath()], [umxSummary()], [plot()], [parameters()], [umxSuperModel()], [umxLav2RAM()]
#' @family Core Model Building Functions
#' @references - <https://tbates.github.io>, <https://github.com/tbates/umx>
#' @examples
#' # Here's a path example that models miles per gallon (mpg) as a function of weight (wt) and engine displacement (disp)
#' # using the widely used `mtcars` data set.
#' 
#' # 1: Create the censored data
#' # Just for a demo, we will create a censored litres column, censoring at 2.
#' data(mtcars)
#' tmp = mtcars
#' tmp$litres = tmp$disp / 61.02
#' tmp = umx_make_double_entry_data(tmp, cols = list(litres= 2), sep="", nSib=1)
#' # 2: Create the RAM_DE model
#' m1 = umxRAM_DE("test", data = tmp, DEvars = c("litres"),
#' 	umxPath("litres", to = "mpg"),
#' 	umxPath("wt", to = "mpg"),
#' 	umxPath("wt", with = "litres"),
#' 	umxPath(v.m. = c("litres", "wt", "mpg"))
#' )
#'
umxRAM_DE <- function(model = NA, ..., data = NULL, DEvars = NULL, doubleEntrySuffix = c("_cont", "_cens"), fixCensorThresholds = c("yes", "auto", "no"), censorCuts = NULL, sep = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), weight = NULL, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, lavaanMode = c("sem", "lavaan"), printTab = FALSE) {
	dot.items = list(...) # grab all the dot items: mxPaths, etc...
	# Check for data/model objects passed in ... before unlist() flattens them
	for (item in dot.items) {
		thisIs = class(item)[1]
		if (thisIs %in% c("data.frame", "matrix", "MxData")) {
			stop("umxRAM_DE can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me a ", thisIs, " inside the path list. ",
				 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
		} else if (thisIs == "MxModel") {
			stop("umxRAM_DE can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
				 "You have given me an MxModel inside the path list. ",
				 "umxRAM does not support nesting MxModels directly. If you wanted a multi-group model, see ?umxSuperModel.", call. = FALSE)
		}
	}
	dot.items  = unlist(dot.items) # In case any dot items are lists of mxPaths, etc...
	type       = match.arg(type)
	tryHard    = match.arg(tryHard)
	lavaanMode = match.arg(lavaanMode)
	allContinuousMethod = match.arg(allContinuousMethod)
	fixCensorThresholds = match.arg(fixCensorThresholds)
	doubleEntrySuffix = as.character(doubleEntrySuffix)
	if (length(doubleEntrySuffix) != 2L) {
		stop("Polite note: doubleEntrySuffix must be length 2, e.g. c(\"_cont\",\"_cens\")")
	}
	sCont = doubleEntrySuffix[1]
	sCens = doubleEntrySuffix[2]

	if (!is.null(data)){
		# Preserve umxDoubleEntry attr across class coercions (data may be umx_double_entry_data or tbl)
		savedDEattr = attr(data, "umxDoubleEntry")
		if (inherits(data, "tbl")){
			data = as.data.frame(data)
		}
		if (inherits(data, "umx_double_entry_data")) {
			# xmu_make_mxData checks class(data)[[1]]=="data.frame" strictly; strip custom class but keep attr
			class(data) = "data.frame"
		}
		if (!is.null(savedDEattr)) attr(data, "umxDoubleEntry") = savedDEattr
	}
	if (!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
	if (!is.null(group)){
		if (!inherits(data, "data.frame")){
			stop("Currently, for multiple groups, data must be a raw data.frame so I can subset it into multiple groups. You gave me a ", omxQuotes(class(data)))
		}
	}
	# lavaan string mode not supported for DE currently: handle via umxRAM
	if (is.character(model) && length(model) == 1L && grepl(model, pattern = "(<|~|=~|~~|:=)")){
		stop("Polite note: umxRAM_DE does not yet support lavaan string syntax. Please use umxPath() syntax with DEvars.")
	}

	# umxPath-based model
	if (typeof(model) == "character"){
		if (is.na(name)){
			name = model
		} else {
			stop("If model is set to a string, don't pass in name as well...")
		}
	} else {
		if (umx_is_RAM(model)){
			if (is.na(name)){
				name = model$name
			}
			if (is.null(data)){
				newModel = do.call(mxModel, c(list(model), dot.items, list(name = name)))
			} else {
				if (umx_is_MxData(data)){
					newModel = do.call(mxModel, c(list(model), dot.items, list(data, name = name)))
				} else {
					stop("Polite note: I don't know how to convert raw data into mxData to update your model - can you please do that for me and try again?")
				}
			}
			newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
			return(newModel)
		} else {
			stop("First item must be either an existing model or a name string. You gave me a ", typeof(model))
		}
	}

	umx_check(!is.null(data), "stop", "In umxRAM_DE, you must set 'data = '. If you're building a model with no data, use mxModel")
	umx_check(!is.null(DEvars), "stop", "In umxRAM_DE, you must set 'DEvars = ' to a vector of base DE variable names (e.g. DEvars = c(\"wt\")).")
	if (!is.character(DEvars) || anyNA(DEvars) || any(!nzchar(DEvars))) {
		stop("Polite note: DEvars must be a character vector of base names (e.g. c(\"wt\")).")
	}
	# Enforce base-only: no suffix in DEvars
	for (b in DEvars) {
		if (endsWith(b, sCont) || endsWith(b, sCens)) {
			stop("Polite note: DEvars must be base names without suffix. Use '", sub(paste0(sCont, "$"), "", sub(paste0(sCens, "$"), "", b)), "' not '", b, "'.")
		}
	}
	DEvars = unique(DEvars)
	# sep inference
	if (is.null(sep)) {
		attrSep = NULL
		tmpAttr = attr(data, "umxDoubleEntry")
		if (!is.null(tmpAttr$sep)) attrSep = tmpAttr$sep
		if (!is.null(attrSep)) sep = attrSep else sep = ""
	}
	# Validate DE columns exist in data
	dataCols = colnames(data)
	for (b in DEvars) {
		contCol = paste0(b, sCont)
		censCol = paste0(b, sCens)
		if (!(contCol %in% dataCols)) {
			stop("Polite note: DEvars base '", b, "' requires column '", contCol, "' in data. Did you call umx_make_double_entry_data()?")
		}
		if (!(censCol %in% dataCols)) {
			stop("Polite note: DEvars base '", b, "' requires column '", censCol, "' in data. Did you call umx_make_double_entry_data()?")
		}
		if (!is.ordered(data[[censCol]]) && !is.factor(data[[censCol]])) {
			stop("Polite note: censored column '", censCol, "' must be an ordered factor from umx_make_double_entry_data().")
		}
	}
	# Type guard: WLS not supported with DE v1
	if (type %in% c("WLS", "DWLS", "ULS") && length(DEvars) > 0) {
		stop("Polite note: Fixed double-entry thresholds with type=\"", type, "\" are not supported in umxRAM_DE v1. Use type=\"Auto\"/\"FIML\".")
	}
	if (!is.null(group) && length(DEvars) > 0) {
		# Allow group only if not fixing? For simplicity hard error as per plan
		if (!identical(fixCensorThresholds, "no") || !is.null(censorCuts)) {
			stop("Polite note: umxRAM_DE with group and DE fixed thresholds not supported in v1.")
		}
	}

	foundNames = c()
	defnNames  = c()
	for (thisItem in dot.items) {
		if (!is.list(thisItem)){
			thisItem = list(thisItem)
		}
		for (i in seq_along(thisItem)) {
			thisIs = class(thisItem[[i]])[1]
			if (thisIs == "MxPath"){
				foundNames = append(foundNames, c(thisItem[[i]]$from, thisItem[[i]]$to))
				tmp = namez(thisItem[[i]]$labels, "data\\.")
				if (length(tmp) > 0){
					defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
				}
			} else {
				if (thisIs == "MxThreshold"){
					# MxThreshold detected
				} else if (umx_is_MxMatrix(thisItem[[i]])){
					tmp = namez(thisItem[[i]]$labels, "data\\.")
					if (length(tmp) > 0){
						defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement= "\\1"))
					}
				} else if (isS4(thisItem[[i]]) && grepl("^Mx", thisIs) && !thisIs %in% c("MxModel", "MxData")) {
					# Valid OpenMx S4 object
				} else {
					if (thisIs %in% c("data.frame", "matrix", "MxData")) {
						stop("umxRAM_DE can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
							 "You have given me a ", thisIs, " inside the path list. ",
							 "To include data in umxRAM, please use the 'data = yourData' parameter, not inside the path list.", call. = FALSE)
					} else if (thisIs == "MxModel") {
						stop("umxRAM_DE can only handle (u)mxPaths, (u)mxMatrices, mxConstraints, and mxThreshold() objects.\n",
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

	# Validate paths use base names only (no suffix leakage)
	for (nm in foundNames) {
		if (nm %in% c("one", NA)) next
		if (is.na(nm)) next
		if (endsWith(nm, sCont) || endsWith(nm, sCens)) {
			baseTry = sub(paste0(sCont, "$"), "", sub(paste0(sCens, "$"), "", nm))
			if (baseTry %in% DEvars) {
				stop("Polite note: Use base name '", baseTry, "' in umxPath(), not '", nm, "'. umxRAM_DE will expand it to '", paste0(baseTry, sCont), "' and '", paste0(baseTry, sCens), "' automatically.")
			}
		}
	}

	# Expand DE paths: Cartesian product for DE→DE = 4, one-sided DE = 2
	dot.items = xmu_ram_de_expand_paths(dot.items, DEvars, doubleEntrySuffix)

	# Recompute foundNames after expansion for manifest detection
	foundNames = c()
	for (thisItem in dot.items) {
		if (!is.list(thisItem)){
			thisItem = list(thisItem)
		}
		for (i in seq_along(thisItem)) {
			if (class(thisItem[[i]])[1] == "MxPath"){
				foundNames = append(foundNames, c(thisItem[[i]]$from, thisItem[[i]]$to))
			}
		}
	}

	# ============================
	# = All dot.items processed  =
	# ============================

	# ====================================
	# = Find latentVars and manifestVars =
	# ====================================
	manifestVars = unique(na.omit(umx_names(data)))
	foundNames = unique(na.omit(foundNames))
	defnNames  = unique(na.omit(defnNames))
	
	if (length(defnNames) > 0){
		umx_check_names(defnNames, data = data, message = "note: used as definition variable, but not present in data")
	}
	latentVars = setdiff(foundNames, c(manifestVars, "one"))


	nLatent = length(latentVars)
	if (!umx_set_silent(silent=TRUE)) {
    	if (nLatent == 1){
			message("A latent variable '", latentVars[1], "' was created. ")
	    } else if (nLatent > 1){
      	  message(nLatent, " latent variables were created:", paste(latentVars, collapse = ", "), ". ")
    	}
	}

	# ======================================
	# = List up used and un-used Manifests =
	# ======================================
	unusedManifests = setdiff(manifestVars, c(foundNames, defnNames))
	if (!is.null(weight)) unusedManifests = setdiff(c(manifestVars, weight), c(foundNames, defnNames))
	if (remove_unused_manifests & length(unusedManifests) > 0){
		usedManifests = setdiff(intersect(manifestVars, foundNames), "one")
		if (!is.null(weight)) {
			myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = defnNames, verbose = verbose, weight = weight)
		} else {
			myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = defnNames, verbose = verbose)
		}
	} else {
		usedManifests = setdiff(manifestVars, defnNames)
		myData = xmu_make_mxData(data= data, type = type, verbose = verbose, manifests = usedManifests, fullCovs = defnNames)
	}
	# Topologically sort manifestVars and latentVars
	all_nodes = c(latentVars, usedManifests)
	if (length(all_nodes) > 1) {
		from_list = character(0)
		to_list   = character(0)
		for (item in dot.items) {
			if (inherits(item, "MxPath") && item@arrows == 1) {
				for (f in item@from) {
					for (t in item@to) {
						from_list = c(from_list, f)
						to_list   = c(to_list, t)
					}
				}
			}
		}
		if (length(from_list) > 0) {
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

	newModel = do.call(mxModel, c(list(name = name, type = "RAM", myData,
		manifestVars = usedManifests,
		latentVars  = latentVars,
		independent = independent), dot.items)
	)

	# ==========================
	# = Add means if necessary =
	# ==========================
	needsMeans = xmu_check_needs_means(data = myData, type = type, allContinuousMethod = allContinuousMethod)
	if (needsMeans && is.null(newModel$matrices$M)){
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
	if (setValues){
		newModel = xmuValues(newModel, onlyTouchZeros = TRUE)
	}

	if (any(umx_is_ordered(myData$observed))){
		newModel = xmuRAM2Ordinal(newModel, verbose = FALSE)
	}

	# =========================================
	# = DE threshold / mean / variance fixes  =
	# =========================================
	# Early guard for type already done; now resolve cuts and apply RAM ID fixes
	deMeta = xmu_ram_de_parse_censor_meta(data, DEvars, doubleEntrySuffix, fixCensorThresholds, censorCuts)
	# Enforce hard error for WLS/group already, but also handle group expansion later
	if (length(deMeta$fixedCuts) > 0 && !is.null(group)) {
		stop("Polite note: Fixed double-entry thresholds with group not supported in umxRAM_DE v1.")
	}
	newModel = xmu_ram_de_apply_censor_thresholds(newModel, deMeta$fixedCuts, deMeta$contByCens, DEvars, doubleEntrySuffix, sep)
	# Tag DE metadata for summary/plot
	attr(newModel, "umxDE") = list(
		fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
		fixedCuts = deMeta$fixedCuts,
		contByCens = deMeta$contByCens,
		DEvars = DEvars,
		doubleEntrySuffix = doubleEntrySuffix,
		sideByCens = deMeta$sideByCens
	)

	# ==============================
	# = Add mxFitFunction to model =
	# ==============================
	if (type %in%  c('WLS', 'DWLS', 'ULS')) {
		newModel = mxModel(newModel, mxFitFunctionWLS(type= type, allContinuousMethod = allContinuousMethod) )
	}

	# =====================
	# = Handle group here =
	# =====================
	if (!is.null(group)){
		modelList = list()
		groupCol  = data[, group]
		levelsOfGroup = unique(groupCol)
		for (thisLevelOfGroup in levelsOfGroup) {
			thisSubset = data[groupCol == thisLevelOfGroup, ]
			if (remove_unused_manifests & length(unusedManifests) > 0){
				myData = xmu_make_mxData(data = thisSubset, type = type, manifests = c(usedManifests, defnNames), verbose = FALSE)
			} else {
				myData = xmu_make_mxData(data= thisSubset, type = type, verbose = FALSE)
			}
			thisModel = mxModel(newModel, myData, name= paste0(name, "_", thisLevelOfGroup))
			if (!is.null(group.equal)){
				message("sorry, haven't implemented group.equal yet")
			} else {
				thisModel = umxSetParameters(thisModel, regex= "_GROUP$", newlabels= paste0("_", thisLevelOfGroup))
			}
			modelList = c(modelList, thisModel)
		}
		return(umxSuperModel(name = name, modelList, autoRun = autoRun, tryHard = tryHard, std = std))
	}

	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
	# Re-attach DE attr if run replaced object (mxRun preserves but be safe)
	if (is.null(attr(newModel, "umxDE"))) {
		attr(newModel, "umxDE") = list(
			fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
			fixedCuts = deMeta$fixedCuts,
			contByCens = deMeta$contByCens,
			DEvars = DEvars,
			doubleEntrySuffix = doubleEntrySuffix,
			sideByCens = deMeta$sideByCens
		)
	}
	invisible(newModel)
}

#' Expand DE paths with Cartesian product and label equating
#'
#' Internal helper for [umxRAM_DE()]. For each [OpenMx::mxPath()] that references a
#' base name in `DEvars`, expands to 2 or 4 paths sharing the original labels
#' (Cartesian product when both `from` and `to` contain DE bases). Pure continuous
#' paths pass through unchanged.
#'
#' @param dot.items List of dot items (unlisted `mxPath` etc.).
#' @param DEvars Character vector of base DE names.
#' @param doubleEntrySuffix Suffix pair `c("_cont","_cens")`.
#' @return Expanded list of dot items.
#' @family xmu internal not for end user
xmu_ram_de_expand_paths <- function(dot.items, DEvars, doubleEntrySuffix = c("_cont","_cens")) {
	sCont = doubleEntrySuffix[1]
	sCens = doubleEntrySuffix[2]
	expanded = list()
	for (item in dot.items) {
		# Handle lists returned by umxPath(v.m.) etc that may have been flattened already
		items = list()
		if (is.list(item) && !inherits(item, "MxPath") && !isS4(item)) {
			# unlikely after unlist, but keep safe
			for (k in seq_along(item)) items[[length(items)+1]] = item[[k]]
		} else {
			items[[1]] = item
		}
		for (p in items) {
			if (!inherits(p, "MxPath")) {
				expanded[[length(expanded)+1]] = p
				next
			}
			fromVec = p@from
			toVec   = p@to
			arrows  = p@arrows
			# Disallow suffix leakage already checked, but keep safe
			# Detect DE involvement (base names only)
			fromIsDE = fromVec %in% DEvars
			toIsDE   = toVec %in% DEvars
			hasFromDE = any(fromIsDE)
			hasToDE   = any(toIsDE)
			if (!hasFromDE && !hasToDE) {
				expanded[[length(expanded)+1]] = p
				next
			}
			# Build expanded from/to combinations
			# Helper to map a vector element to its _cont/_cens form if DE else itself
			mapCont = function(vec) {
				out = character(length(vec))
				for (i in seq_along(vec)) {
					v = vec[i]
					if (v %in% DEvars) out[i] = paste0(v, sCont) else out[i] = v
				}
				return(out)
			}
			mapCens = function(vec) {
				out = character(length(vec))
				for (i in seq_along(vec)) {
					v = vec[i]
					if (v %in% DEvars) out[i] = paste0(v, sCens) else out[i] = v
				}
				return(out)
			}
			# Determine expansion count: Cartesian logic per your Q1
			# - one side DE -> 2 paths (cont + cens on that side)
			# - both sides DE -> 4 paths (cont/cens x cont/cens)
			# Also need to handle connect modes: mxPath connect expands from×to via OpenMx.
			# We preserve original p's connect semantics by emitting paths with same connect/arrows/free/labels
			# but with from/to vectors mapped to cont/cens forms.
			# For both-sides case, emit 4 separate MxPaths each with cont/cens combo.
			if (hasFromDE && !hasToDE) {
				pCont = p
				pCont@from = mapCont(fromVec)
				pCens = p
				pCens@from = mapCens(fromVec)
				pCens@labels = pCont@labels
				# ensure free/values/lbound/ubound shared (already same object copy)
				expanded[[length(expanded)+1]] = pCont
				expanded[[length(expanded)+1]] = pCens
			} else if (!hasFromDE && hasToDE) {
				pCont = p
				pCont@to = mapCont(toVec)
				pCens = p
				pCens@to = mapCens(toVec)
				pCens@labels = pCont@labels
				expanded[[length(expanded)+1]] = pCont
				expanded[[length(expanded)+1]] = pCens
			} else {
				# both sides DE -> 4 combos (or 2 for diagonal var case)
				isDiagVar = (arrows == 2 && identical(sort(fromVec), sort(toVec)) && length(fromVec) == length(toVec))
				# Check if from and to are same set and connect single implies diagonal variances only
				# For diagonal var, emit only cont_cont and cens_cens (2), sharing label
				if (isDiagVar) {
					pCC = p; pCC@from = mapCont(fromVec); pCC@to = mapCont(toVec)
					pRR = p; pRR@from = mapCens(fromVec); pRR@to = mapCens(toVec)
					lab = pCC@labels
					pRR@labels = lab
					expanded[[length(expanded)+1]] = pCC
					expanded[[length(expanded)+1]] = pRR
				} else {
					pCC = p; pCC@from = mapCont(fromVec); pCC@to = mapCont(toVec)
					pCR = p; pCR@from = mapCont(fromVec); pCR@to = mapCens(toVec)
					pRC = p; pRC@from = mapCens(fromVec); pRC@to = mapCont(toVec)
					pRR = p; pRR@from = mapCens(fromVec); pRR@to = mapCens(toVec)
					lab = pCC@labels
					pCR@labels = lab; pRC@labels = lab; pRR@labels = lab
					expanded[[length(expanded)+1]] = pCC
					expanded[[length(expanded)+1]] = pCR
					expanded[[length(expanded)+1]] = pRC
					expanded[[length(expanded)+1]] = pRR
				}
			}
		}
	}
	return(expanded)
}

#' Parse censor cuts for single-group RAM DE
#'
#' Mirrors `xmu_ace_de_parse_censor_meta` but for a single data.frame and `DEvars`.
#'
#' @param data Data frame with `umxDoubleEntry` attr.
#' @param DEvars Base names.
#' @param doubleEntrySuffix Suffixes.
#' @param fixCensorThresholds `c("yes","auto","no")`.
#' @param censorCuts Named numeric cuts or `NULL`.
#' @return List with `fixedCuts` (named by cens col), `contByCens`, `sideByCens`.
#' @family xmu internal not for end user
xmu_ram_de_parse_censor_meta <- function(data, DEvars, doubleEntrySuffix = c("_cont","_cens"), fixCensorThresholds = c("yes","auto","no"), censorCuts = NULL) {
	fixCensorThresholds = match.arg(fixCensorThresholds)
	sCont = doubleEntrySuffix[1]
	sCens = doubleEntrySuffix[2]
	fixedCuts = numeric(0)
	contByCens = character(0)
	sideByCens = character(0)
	if (!is.null(censorCuts) && identical(fixCensorThresholds, "no")) {
		stop("Polite note: censorCuts is set but fixCensorThresholds = \"no\". Set fixCensorThresholds to \"yes\" or \"auto\", or omit censorCuts.")
	}
	if (identical(fixCensorThresholds, "no") && is.null(censorCuts)) {
		return(list(fixedCuts = fixedCuts, contByCens = contByCens, sideByCens = sideByCens))
	}
	# Build contByCens for all DEvars
	for (b in DEvars) {
		cont = paste0(b, sCont)
		cens = paste0(b, sCens)
		contByCens[cens] = cont
	}
	attrList = attr(data, "umxDoubleEntry")
	attrByCens = list()
	if (!is.null(attrList$pairs)) {
		for (p in attrList$pairs) {
			attrByCens[[p$cens]] = p
			attrByCens[[p$base]] = p
			attrByCens[[p$cont]] = p
		}
	}
	resolveCutName = function(nm) {
		for (cens in names(contByCens)) {
			base = sub(paste0(sCens, "$"), "", cens)
			cont = contByCens[[cens]]
			if (nm %in% c(cens, cont, base)) return(cens)
		}
		return(NA_character_)
	}
	if (!is.null(censorCuts)) {
		if (is.null(names(censorCuts)) || any(names(censorCuts) == "")) {
			stop("Polite note: censorCuts must be a named numeric vector (e.g. c(litres = 0)).")
		}
		for (nm in names(censorCuts)) {
			cutVal = as.numeric(censorCuts[[nm]])
			if (!is.finite(cutVal)) stop("Polite note: censorCuts[\"", nm, "\"] must be a finite number.")
			cens = resolveCutName(nm)
			if (is.na(cens)) {
				warning("umx note: censorCuts name \"", nm, "\" matches no DE pair; ignored.", call. = FALSE)
				next
			}
			fixedCuts[cens] = cutVal
			p = attrByCens[[cens]]
			sideByCens[cens] = if (!is.null(p$side)) p$side else "left"
		}
		return(list(fixedCuts = fixedCuts, contByCens = contByCens[names(fixedCuts)], sideByCens = sideByCens))
	}
	missingFixable = character(0)
	for (cens in names(contByCens)) {
		p = attrByCens[[cens]]
		if (is.null(p)) {
			if (identical(fixCensorThresholds, "yes")) missingFixable = c(missingFixable, cens)
			next
		}
		if (isTRUE(p$fixable) && is.finite(p$cut)) {
			fixedCuts[cens] = as.numeric(p$cut)
			sideByCens[cens] = p$side
		} else if (identical(fixCensorThresholds, "yes")) {
			missingFixable = c(missingFixable, cens)
		}
	}
	if (length(missingFixable) > 0) {
		stop("Polite note: fixCensorThresholds = \"yes\" but no finite cut for: ", paste(missingFixable, collapse = ", "), ". Provide censorCuts or prep with umx_make_double_entry_data() using a known numeric bound.")
	}
	return(list(fixedCuts = fixedCuts, contByCens = contByCens[names(fixedCuts)], sideByCens = sideByCens))
}

#' Apply RAM DE threshold/mean/variance identification
#'
#' For each `cens` in `contByCens`: release binary `mean@0`/`resid@1` and
#' equate means/variances to `cont`. For `fixedCuts`, fix threshold at cut;
#' otherwise leave free with sensible start.
#'
#' @param model RAM model after `xmuRAM2Ordinal`.
#' @param fixedCuts Named numeric cuts (may be empty).
#' @param contByCens Named character cens->cont for all DE pairs (or at least fixed ones).
#' @param DEvars Base names.
#' @param doubleEntrySuffix Suffixes.
#' @param sep Separator (unused but kept for parity).
#' @return Modified model.
#' @family xmu internal not for end user
xmu_ram_de_apply_censor_thresholds <- function(model, fixedCuts, contByCens, DEvars, doubleEntrySuffix = c("_cont","_cens"), sep = "") {
	if (is.null(fixedCuts)) fixedCuts = numeric(0)
	if (length(contByCens) == 0 && length(fixedCuts) == 0) return(model)
	# contByCens may be only fixed subset; expand to all DEvars for mean/var handling
	sCont = doubleEntrySuffix[1]
	sCens = doubleEntrySuffix[2]
	fullContByCens = character(0)
	for (b in DEvars) fullContByCens[paste0(b, sCens)] = paste0(b, sCont)
	# For threshold fixing, need mapping for every DE cens
	# Thresholds: in RAM, matrix "threshMat" (th_1 x nFactors) with dimnames 2 = factorVarNames
	# Use omxSetParameters on threshold labels; also support direct threshMat edits
	# Thresholds live in deviations_for_thresh (RAM) or threshMat (legacy)
	threshMat = NULL
	if (!is.null(model$deviations_for_thresh)) threshMat = model$deviations_for_thresh
	else if (!is.null(model$threshMat)) threshMat = model$threshMat
	for (cens in names(fullContByCens)) {
		cont = fullContByCens[[cens]]
		threshLab = paste0(cens, "_thresh1")
		if (!is.null(threshMat) && !is.null(colnames(threshMat$labels)) && cens %in% colnames(threshMat$labels)) {
			lab = threshMat$labels[1, cens]
			if (!is.na(lab) && nzchar(lab)) threshLab = lab
		}
		allLabs = tryCatch(names(omxGetParameters(model)), error = function(e) character(0))
		if (!(threshLab %in% allLabs)) next
		if (cens %in% names(fixedCuts)) {
			model = omxSetParameters(model, labels = threshLab, free = FALSE, values = as.numeric(fixedCuts[[cens]]))
		} else {
			startTau = NA_real_
			if (!is.null(model$M) && cont %in% colnames(model$M$values)) {
				startTau = as.numeric(model$M$values[1, cont])
			}
			if (!is.finite(startTau)) startTau = 0
			model = omxSetParameters(model, labels = threshLab, free = TRUE, values = startTau)
		}
	}
	# Means and variances: free M and S for cens and equate to cont
	for (cens in names(fullContByCens)) {
		cont = fullContByCens[[cens]]
		if (!is.null(model$M) && !is.null(dimnames(model$M$values))) {
			if (cens %in% colnames(model$M$values) && cont %in% colnames(model$M$values)) {
				labCont = model$M$labels[1, cont]
				valCont = model$M$values[1, cont]
				if (is.na(labCont) || !nzchar(labCont)) {
					labCont = paste0(cont, "_mean")
					model$M$labels[1, cont] = labCont
				}
				model$M$free[1, cens] = TRUE
				model$M$labels[1, cens] = labCont
				model$M$values[1, cens] = valCont
			}
		}
		if (!is.null(model$S) && !is.null(dimnames(model$S$values))) {
			if (cens %in% rownames(model$S$values) && cont %in% rownames(model$S$values)) {
				labContVar = model$S$labels[cont, cont]
				valContVar = model$S$values[cont, cont]
				if (is.na(labContVar) || !nzchar(labContVar)) {
					labContVar = paste0(cont, "_with_", cont)
					model$S$labels[cont, cont] = labContVar
				}
				model$S$free[cens, cens] = TRUE
				model$S$labels[cens, cens] = labContVar
				if (is.finite(valContVar)) model$S$values[cens, cens] = valContVar
				model$S$values[cens, cens] = model$S$values[cont, cont]
				model$S$free[cens, cens] = TRUE
			}
			# Equate covariances involving DE: S[cont, v] == S[cens, v] for all v
			if (cens %in% rownames(model$S$values) && cont %in% rownames(model$S$values)) {
				rNames = rownames(model$S$labels)
				for (v in rNames) {
					if (v == cens || v == cont) next
					# S is symmetric, but check both orderings: row cont vs cens
					if (cont %in% rNames && v %in% rNames) {
						# Check if either entry exists as free or labelled (both should exist as 0)
						labContCov = model$S$labels[cont, v]
						if (is.na(labContCov) || !nzchar(labContCov)) labContCov = model$S$labels[v, cont]
						if (!is.na(labContCov) && nzchar(labContCov)) {
							model$S$labels[cens, v] = labContCov
							model$S$labels[v, cens] = labContCov
							model$S$free[cens, v] = model$S$free[cont, v]
							model$S$free[v, cens] = model$S$free[v, cont]
							model$S$values[cens, v] = model$S$values[cont, v]
							model$S$values[v, cens] = model$S$values[v, cont]
						} else {
							# If cont-v not yet labelled, check cens-v and copy to cont
							labCensCov = model$S$labels[cens, v]
							if (!is.na(labCensCov) && nzchar(labCensCov)) {
								model$S$labels[cont, v] = labCensCov
								model$S$labels[v, cont] = labCensCov
							}
						}
					}
					# Also handle case where both v and cont/cens are DE (both ends DE) – 4-way
					# For two DE bases, the covariance between their cont/cens combos should all share one label
					# This will be handled when iterating over second DE's cens
				}
			}
		}
	}
	# Equate directed paths in A for DE: cont and cens share label (Cartesian for DE→DE =4, one-sided=2)
	if (!is.null(model$A) && !is.null(dimnames(model$A$labels))) {
		rNames = rownames(model$A$labels)
		cNames = colnames(model$A$labels)
		for (cens in names(fullContByCens)) {
			cont = fullContByCens[[cens]]
			# For each cell, map cens variant to cont variant and copy label
			for (r in rNames) {
				for (cc in cNames) {
					rBase = r; cBase = cc
					if (r == cens) rBase = cont
					if (cc == cens) cBase = cont
					# Need to handle case where both r and cc are cens variants? Already mapped to cont
					# Only equate if current cell involves cens and the cont counterpart exists
					if ((r == cens || cc == cens) && rBase %in% rNames && cBase %in% cNames) {
						labCont = model$A$labels[rBase, cBase]
						if (!is.na(labCont) && nzchar(labCont)) {
							# Preserve original free/values but share label
							model$A$labels[r, cc] = labCont
							# Ensure free and values match cont counterpart
							model$A$free[r, cc] = model$A$free[rBase, cBase]
							model$A$values[r, cc] = model$A$values[rBase, cBase]
						}
					}
				}
			}
		}
	}
	return(model)
}
