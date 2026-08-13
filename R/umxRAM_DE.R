#' Build and run path-based Double-entry SEM models including censored data
#'
#' @description
#' `umxRAM_DE` expedites SEM with censored data. Each name in `DEvars` is a **latent** trait.
#' Prepared `_cont` / `_cens` columns are indicators of that latent (loading fixed at 1, residual fixed at 0).
#' Write ordinary [umxPath()] calls using the base name (e.g. `"litres"`). Do not Cartesian-expand to `_cont`/`_cens`.
#' 
#' Fully continuous variables may be mixed with double-entry pairs. Prepare the censored traits with
#' [umx_make_double_entry_data()]. At least one `DEvar` is required.
#'
#' As you can see from the examples below, most of the work is done by [umxPath()]. `umxRAM_DE` wraps these paths up, takes the `data =` input, and 
#' then builds the latent measurement model for each DE trait and sets thresholds. By default it will also run it.
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
#' **WLS / cov / cor**
#' `type = "WLS"`, `"DWLS"`, `"ULS"`, `"cov"`, or `"cor"` is not valid in `umxRAM_DE`.
#' Double-entry needs raw-data FIML (each row is either the continuous density or the
#' threshold CDF). Those types fit a correlation/moment structure and cannot use the
#' mutual-NA `_cont`/`_cens` pattern. For WLS/cov/cor on ordinary (non-DE) data, use [umxRAM()].
#'
#' @param model A model to update (or set to string to use as name for new model)
#' @param data data for the model. Can be an [OpenMx::mxData()] or a data.frame
#' @param ... umxPaths, mxThreshold objects, etc.
#' @param DEvars A character vector of base names for double-entry variables (e.g. `c("litres")` creates `litres_cont`/`litres_cens`). Base names only; do not include suffix.
#' @param doubleEntrySuffix Suffixes for the continuous and censored variables (default = c("_cont", "_cens")).
#' @param fixCensorThresholds One of `c("yes","auto","no")`. `"yes"` fix every DE pair from `censorCuts`/prep attr; `"auto"` fix only pairs with finite known cut; `"no"` free thresholds.
#' @param censorCuts Optional named numeric vector of known cuts on analysis scale. Names may be base (`"litres"`), `"_cont"` or `"_cens"` form.
#' @param sep Separator used in prep (default `NULL` infers from `attr(data,"umxDoubleEntry")$sep` or `""`).
#' @param group (optional) Column name to use for a multi-group model (default = NULL). Fixed DE thresholds (known cut) are applied in every group.
#' @param group.equal In multi-group models, what to equate across groups (default = NULL: all free)
#' @param comparison Compare the new model to the old (if updating an existing model: default = TRUE)
#' @param suffix String to append to each label (useful if model will be used in a multi-group model)
#' @param name A friendly name for the model
#' @param type One of `"Auto"` or `"FIML"`. `"WLS"`, `"DWLS"`, `"ULS"`, `"cov"`, and `"cor"` are rejected (use [umxRAM()]).
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
umxRAM_DE <- function(model = NA, ..., data = NULL, DEvars = NULL, doubleEntrySuffix = c("_cont", "_cens"), fixCensorThresholds = c("yes", "auto", "no"), censorCuts = NULL, sep = NULL, name = NA, group = NULL, group.equal = NULL, suffix = "", comparison = TRUE, type = c("Auto", "FIML"), weight = NULL, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), std = FALSE, refModels = NULL, remove_unused_manifests = TRUE, independent = NA, setValues = TRUE, optimizer = NULL, verbose = FALSE, std.lv = FALSE, lavaanMode = c("sem", "lavaan"), printTab = FALSE) {
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
	# Moment-structure types are not row-wise raw FIML; DE needs the latter.
	if (type %in% c("WLS", "DWLS", "ULS", "cov", "cor")) {
		stop("Polite note: type=\"", type, "\" is not valid for umxRAM_DE. Double-entry censoring needs raw-data FIML (each row contributes either the continuous density or the threshold CDF). WLS/DWLS/ULS/cov/cor fit a correlation/moment structure and cannot use that pattern (_cont and _cens are never jointly observed). For WLS, cov, or cor on ordinary (non-DE) data, use umxRAM().", call. = FALSE)
	}
	# group= is allowed with fixed τ: the known cut is on the analysis scale and is
	# the same in every group. group.equal (equating other parameters) is still unimplemented.

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
				stop("Polite note: Use base name '", baseTry, "' in umxPath(), not '", nm, "'. umxRAM_DE treats '", baseTry, "' as a latent; indicators '", paste0(baseTry, sCont), "' and '", paste0(baseTry, sCens), "' are created automatically.")
			}
		}
	}

	# Measurement model: each DEvar is a latent; _cont/_cens are perfect indicators.
	# User paths stay on the base name. Two DE traits share one latent–latent S cell
	# (not a 4-way indicator Cartesian product).
	for (b in DEvars) {
		contCol = paste0(b, sCont)
		censCol = paste0(b, sCens)
		dot.items = c(dot.items, list(
			mxPath(from = b, to = contCol, arrows = 1, free = FALSE, values = 1),
			mxPath(from = b, to = censCol, arrows = 1, free = FALSE, values = 1),
			mxPath(from = contCol, arrows = 2, free = FALSE, values = 0),
			mxPath(from = censCol, arrows = 2, free = FALSE, values = 0)
		))
	}

	# Recompute foundNames after injecting indicators
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
	# DEvars are latents even if a same-named raw column remains in the prep data
	latentVars = unique(c(DEvars, setdiff(foundNames, c(manifestVars, "one"))))


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
	unusedManifests = setdiff(manifestVars, c(foundNames, defnNames, DEvars))
	if (!is.null(weight)) unusedManifests = setdiff(c(manifestVars, weight), c(foundNames, defnNames, DEvars))
	if (remove_unused_manifests & length(unusedManifests) > 0){
		usedManifests = setdiff(intersect(manifestVars, foundNames), c("one", DEvars))
	} else {
		usedManifests = setdiff(manifestVars, c(defnNames, DEvars))
	}
	for (b in DEvars) {
		usedManifests = unique(c(usedManifests, paste0(b, sCont), paste0(b, sCens)))
	}
	if (!is.null(weight)) {
		myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = defnNames, verbose = verbose, weight = weight)
	} else {
		myData = xmu_make_mxData(data = data, type = type, manifests = usedManifests, fullCovs = defnNames, verbose = verbose)
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
	# Trait mean/var live on the DE latent (not on _cont/_cens indicators)
	for (b in DEvars) {
		hasVar = !is.null(newModel$S) && !is.null(dimnames(newModel$S$values)) && b %in% rownames(newModel$S$values) &&
			(isTRUE(newModel$S$free[b, b]) || (is.finite(newModel$S$values[b, b]) && newModel$S$values[b, b] != 0))
		if (!hasVar) {
			newModel = mxModel(newModel, mxPath(from = b, arrows = 2, free = TRUE, values = 1))
		}
		if (needsMeans) {
			hasMean = !is.null(newModel$M) && !is.null(dimnames(newModel$M$values)) && b %in% colnames(newModel$M$values)
			if (!hasMean) {
				newModel = mxModel(newModel, mxPath("one", to = b, free = TRUE, values = 0))
			}
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
	# Always map every DEvar (not only pairs with a fixed cut) so summary/plot and
	# fixCensorThresholds="no" still equate means, variances, and loadings.
	fullContByCens = character(0)
	for (b in DEvars) {
		fullContByCens[paste0(b, sCens)] = paste0(b, sCont)
	}
	newModel = xmu_ram_de_apply_censor_thresholds(newModel, deMeta$fixedCuts, fullContByCens, DEvars, doubleEntrySuffix, sep)
	# Tag DE metadata for summary/plot
	attr(newModel, "umxDE") = list(
		fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
		fixedCuts = deMeta$fixedCuts,
		contByCens = fullContByCens,
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
			# Re-assert DE measurement ID after clone/relabel (τ@cut is group-invariant)
			thisModel = xmu_ram_de_apply_censor_thresholds(thisModel, deMeta$fixedCuts, fullContByCens, DEvars, doubleEntrySuffix, sep)
			attr(thisModel, "umxDE") = list(
				fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
				fixedCuts = deMeta$fixedCuts,
				contByCens = fullContByCens,
				DEvars = DEvars,
				doubleEntrySuffix = doubleEntrySuffix,
				sideByCens = deMeta$sideByCens
			)
			modelList = c(modelList, thisModel)
		}
		mg = umxSuperModel(name = name, modelList, autoRun = autoRun, tryHard = tryHard, std = std)
		if (is.null(attr(mg, "umxDE"))) {
			attr(mg, "umxDE") = list(
				fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
				fixedCuts = deMeta$fixedCuts,
				contByCens = fullContByCens,
				DEvars = DEvars,
				doubleEntrySuffix = doubleEntrySuffix,
				sideByCens = deMeta$sideByCens
			)
		}
		return(mg)
	}

	newModel = omxAssignFirstParameters(newModel)
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, refModels = refModels, std = std)
	# Re-attach DE attr if run replaced object (mxRun preserves but be safe)
	if (is.null(attr(newModel, "umxDE"))) {
		attr(newModel, "umxDE") = list(
			fixedCensorThresholds = length(deMeta$fixedCuts) > 0,
			fixedCuts = deMeta$fixedCuts,
			contByCens = fullContByCens,
			DEvars = DEvars,
			doubleEntrySuffix = doubleEntrySuffix,
			sideByCens = deMeta$sideByCens
		)
	}
	invisible(newModel)
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

#' Apply RAM DE latent-trait identification
#'
#' After [xmuRAM2Ordinal()] (which may set binary `mean@0` / `resid@1`), enforce
#' the latent-trait measurement model: each `DEvars` base `x` has
#' `A[x_cont, x] = A[x_cens, x] = 1`, indicator residuals and means at 0,
#' trait mean/variance on the latent. Fix or free the `_cens` threshold.
#'
#' @param model RAM model after `xmuRAM2Ordinal`.
#' @param fixedCuts Named numeric cuts (may be empty).
#' @param contByCens Named character cens->cont (unused for equate; kept for call signature).
#' @param DEvars Base names (latents).
#' @param doubleEntrySuffix Suffixes.
#' @param sep Separator (unused but kept for parity).
#' @return Modified model.
#' @family xmu internal not for end user
xmu_ram_de_apply_censor_thresholds <- function(model, fixedCuts, contByCens, DEvars, doubleEntrySuffix = c("_cont","_cens"), sep = "") {
	if (is.null(fixedCuts)) fixedCuts = numeric(0)
	sCont = doubleEntrySuffix[1]
	sCens = doubleEntrySuffix[2]
	threshMat = NULL
	if (!is.null(model$deviations_for_thresh)) threshMat = model$deviations_for_thresh
	else if (!is.null(model$threshMat)) threshMat = model$threshMat
	allLabs = tryCatch(names(omxGetParameters(model, free = NA)), error = function(e) character(0))
	for (b in DEvars) {
		cont = paste0(b, sCont)
		cens = paste0(b, sCens)
		# Loadings latent → indicators @ 1
		if (!is.null(model$A) && !is.null(dimnames(model$A$values))) {
			if (cont %in% rownames(model$A$values) && b %in% colnames(model$A$values)) {
				model$A$free[cont, b] = FALSE
				model$A$values[cont, b] = 1
			}
			if (cens %in% rownames(model$A$values) && b %in% colnames(model$A$values)) {
				model$A$free[cens, b] = FALSE
				model$A$values[cens, b] = 1
			}
		}
		# Indicator residuals @ 0 (override binary resid@1). No A paths from indicators
		# (outcomes attach to the latent, not to _cont/_cens).
		if (!is.null(model$A) && !is.null(dimnames(model$A$values))) {
			rA = rownames(model$A$values)
			cA = colnames(model$A$values)
			for (ind in c(cont, cens)) {
				if (ind %in% cA) {
					for (rr in rA) {
						if (rr == ind) next
						model$A$free[rr, ind] = FALSE
						model$A$values[rr, ind] = 0
					}
				}
			}
		}
		# Indicator residuals @ 0; no residual cov among any DE indicators
		# (trait cov is S[latent_i, latent_j] only).
		if (!is.null(model$S) && !is.null(dimnames(model$S$values))) {
			if (cont %in% rownames(model$S$values)) {
				model$S$free[cont, cont] = FALSE
				model$S$values[cont, cont] = 0
			}
			if (cens %in% rownames(model$S$values)) {
				model$S$free[cens, cens] = FALSE
				model$S$values[cens, cens] = 0
			}
			indAll = character(0)
			for (bb in DEvars) {
				indAll = c(indAll, paste0(bb, sCont), paste0(bb, sCens))
			}
			sNames = rownames(model$S$values)
			for (ii in seq_along(indAll)) {
				for (jj in seq_along(indAll)) {
					a = indAll[ii]
					c2 = indAll[jj]
					if (!(a %in% sNames) || !(c2 %in% sNames)) next
					if (a == c2) next
					model$S$free[a, c2] = FALSE
					model$S$values[a, c2] = 0
				}
			}
		}
		# Indicator means @ 0 (trait mean is on the latent)
		if (!is.null(model$M) && !is.null(dimnames(model$M$values))) {
			if (cont %in% colnames(model$M$values)) {
				model$M$free[1, cont] = FALSE
				model$M$values[1, cont] = 0
			}
			if (cens %in% colnames(model$M$values)) {
				model$M$free[1, cens] = FALSE
				model$M$values[1, cens] = 0
			}
		}
		# Threshold on cens
		threshLab = paste0(cens, "_thresh1")
		if (!is.null(threshMat) && !is.null(colnames(threshMat$labels)) && cens %in% colnames(threshMat$labels)) {
			lab = threshMat$labels[1, cens]
			if (!is.na(lab) && nzchar(lab)) threshLab = lab
		}
		startTau = 0
		if (!is.null(model$M) && b %in% colnames(model$M$values)) {
			muLat = as.numeric(model$M$values[1, b])
			if (is.finite(muLat)) startTau = muLat
		}
		if (cens %in% names(fixedCuts)) {
			if (threshLab %in% allLabs) {
				model = omxSetParameters(model, labels = threshLab, free = FALSE, values = as.numeric(fixedCuts[[cens]]))
			} else if (!is.null(threshMat) && cens %in% colnames(threshMat$values)) {
				model$deviations_for_thresh$free[1, cens] = FALSE
				model$deviations_for_thresh$values[1, cens] = as.numeric(fixedCuts[[cens]])
			}
		} else {
			if (threshLab %in% allLabs) {
				model = omxSetParameters(model, labels = threshLab, free = TRUE, values = startTau)
			} else if (!is.null(threshMat) && cens %in% colnames(threshMat$values)) {
				model$deviations_for_thresh$free[1, cens] = TRUE
				model$deviations_for_thresh$values[1, cens] = startTau
			}
		}
		allLabs = tryCatch(names(omxGetParameters(model, free = NA)), error = function(e) allLabs)
	}
	return(model)
}
