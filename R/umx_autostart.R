#' Starting values with mxAutoStart that survive definition variables
#'
#' @description
#' [OpenMx::mxAutoStart()] cannot handle models with definition variables. `umx_autostart`
#' builds a clean clone of the model in which every definition-variable cell (label
#' `data.var`) is fixed at a numeric placeholder, runs [OpenMx::mxAutoStart()] on the clone,
#' and transfers the resulting starting values back to the original model by label.
#' Models without definition variables are passed straight to [OpenMx::mxAutoStart()].
#'
#' @details
#' Placeholders for a definition variable `var` are:
#' * `"row1"`: the value of `var` in row `row` of the raw data (mean if missing, else 0).
#' * `"mean"`: the mean of `var` (0 if all missing).
#' * `"zero"`: 0.
#'
#' Placeholders are looked up in each (sub)model's own raw data.
#'
#' @param model An [OpenMx::mxModel()].
#' @param type Estimation type passed to [OpenMx::mxAutoStart()]: "ULS" (default) or "DWLS".
#' @param placeholder How to fill definition-variable cells in the clone: "row1" (default), "mean", or "zero".
#' @param row Row of the data used when `placeholder = "row1"` (default 1).
#' @param alignEquated Whether to give equated parameters identical starts via [OpenMx::omxAssignFirstParameters()] (default TRUE).
#' @param verbose Whether to report progress (default FALSE).
#' @return The model with new starting values.
#' @export
#' @family Advanced Model Building Functions
#' @seealso - [OpenMx::mxAutoStart()], [umx_tidy()], [umx_glance()]
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' df = demoOneFactor
#' df$age = rnorm(nrow(df))
#' m1 = umxRAM("defvar", data = df, autoRun = FALSE,
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G"),
#' 	umxPath(defn = "def", labels = "data.age"),
#' 	umxPath("def", to = "x1")
#' )
#' m1 = umx_autostart(m1)
#' m1 = mxRun(m1)
#' }
umx_autostart <- function(model, type = c("ULS", "DWLS"), placeholder = c("row1", "mean", "zero"), row = 1, alignEquated = TRUE, verbose = FALSE) {
	type        = match.arg(type)
	placeholder = match.arg(placeholder)

	# Early exit: no definition variables -> plain mxAutoStart
	if(!imxHasDefinitionVariable(model)) {
		if(verbose) message("umx_autostart: no definition variables found; calling mxAutoStart() directly.")
		return(mxAutoStart(model, type = type))
	}
	if(verbose) message("umx_autostart: definition variables detected; building a clean clone for autostart.")

	# Raw data attached to a model (if any)
	getObserved = function(m) {
		if(!is.null(m$data) && m$data@type == "raw") m$data@observed else NULL
	}

	# Per-column cache of the placeholder row and column means
	buildPlaceholderCache = function(obs) {
		if(is.null(obs)) return(list(row_values = list(), means = list()))
		list(
			row_values = as.list(obs[row, , drop = FALSE]),
			means      = lapply(obs, function(col) mean(col, na.rm = TRUE))
		)
	}

	# Numeric placeholder for one definition-variable label
	defPlaceholder = function(defLabel, cache) {
		var = sub("^data\\.", "", defLabel)
		rv  = cache$row_values[[var]]
		mv  = cache$means[[var]]
		if(is.null(mv)) mv = NA
		if(placeholder == "row1") {
			val = if(!is.null(rv)) rv else mv
			if(is.na(val)) val = mv
			return(ifelse(is.na(val), 0, val))
		} else if(placeholder == "mean") {
			return(ifelse(is.na(mv), 0, mv))
		}
		0
	}

	# Fix every "data.var" cell in all matrices of a model and its submodels at its placeholder
	stripDefvars = function(m) {
		cache = buildPlaceholderCache(getObserved(m))
		for(mname in names(m@matrices)) {
			M    = m@matrices[[mname]]
			labs = M@labels
			if(is.null(labs)) next
			sel = !is.na(labs) & grepl("^data\\.", labs)
			if(!any(sel)) next

			uniq_labs = unique(labs[sel])
			lab2val   = stats::setNames(vapply(uniq_labs, defPlaceholder, numeric(1), cache = cache), uniq_labs)

			vals        = M@values
			vals[sel]   = lab2val[labs[sel]]
			M@labels[sel] = NA
			M@free[sel]   = FALSE
			M@values      = vals
			m@matrices[[mname]] = M
		}
		for(sname in names(m@submodels)) {
			m@submodels[[sname]] = stripDefvars(m@submodels[[sname]])
		}
		m
	}

	# 1) Clean clone without definition variables
	cleanModel = stripDefvars(mxModel(model))

	# 2) Autostart on the clean clone
	autoClean = mxAutoStart(cleanModel, type = type)

	# 3) Transfer starts back by label
	newStarts = omxGetParameters(autoClean, free = TRUE)
	if(verbose) message(sprintf("umx_autostart: transferring %d starting values via omxSetParameters()", length(newStarts)))

	model2 = omxSetParameters(model, labels = names(newStarts), values = as.numeric(newStarts), indep = TRUE, strict = FALSE)

	# Equal labels get identical starts
	if(alignEquated) model2 = omxAssignFirstParameters(model2)
	model2
}
