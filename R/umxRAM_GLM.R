#   Copyright 2026 Timothy C. Bates
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

#' Path-based RAM model with mxFitFunctionGLM
#'
#' @description
#' Standalone counterpart to [umxRAM()] for conditional exponential-family
#' manifests. Latents are anything in the paths that is not in `data`. Manifests
#' are anything in the paths (or in `families`) that is in `data`. Residual `S`
#' on family items is fixed at 0. An observed column used only as `from` (a
#' predictor) is not given residual `S` or a mean; the arrow uses that row's
#' value. Manifests not listed in `families` that have residual `S` are
#' Gaussian. Does not call [mxRefModels()] (no CFI yet).
#'
#' @param model a model name (character). Updating an existing model is not supported.
#' @param ... [umxPath()] / [mxPath()] objects, optional [mxMatrix()], [mxConstraint()], and [mxFamily()] objects. Not data.
#' @param data a data.frame or raw [mxData()]. Required (used to detect manifests vs latents and to set starts).
#' @param families named list of `stats::family` or [mxFamily()] objects. Names are manifest variables. Items not listed are Gaussian.
#' @param theta optional named numeric (fixed) or character (free-parameter label) for NB size / Gamma shape / inverse-Gaussian lambda.
#' @param nAGQ passed to [mxFitFunctionGLM()]. `NA` (default) uses OpenMx's rule: 5 for binomial with one latent, else 1.
#' @param name optional name; if `NA`, `model` is used.
#' @param autoRun whether to run (default [umx_set_auto_run()]).
#' @param tryHard [xmu_safe_run_summary()] tryHard options.
#' @param setValues if TRUE, set start values (means by family; Gaussian residual variances from the data; GLM-item `S` at 0).
#' @param suffix passed to [xmuLabel()].
#' @param verbose currently unused; kept for umxRAM-like calling.
#' @return an [MxModel()] (invisibly if autoRun prints a summary)
#' @family Advanced Model Building Functions
#' @export
#' @seealso [umxRAM()], [mxFamily()], [mxFitFunctionGLM()]
#' @md
#' @examples
#' \dontrun{
#' set.seed(1)
#' dat = data.frame(y = rpois(80, lambda = exp(0.3)))
#' m1 = umxRAM_GLM("poi",
#' 	umxPath("one", to = "y"),
#' 	data = dat,
#' 	families = list(y = poisson()),
#' 	autoRun = TRUE)
#' }
umxRAM_GLM <- function(model = NA, ..., data = NULL, families = NULL, theta = NULL, nAGQ = NA_integer_, name = NA, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), setValues = TRUE, suffix = "", verbose = FALSE) {
	tryHard = match.arg(tryHard)
	dot.items = list(...)
	for (item in dot.items) {
		thisIs = class(item)[1]
		if (thisIs %in% c("data.frame", "matrix", "MxData")) {
			stop("umxRAM_GLM: pass data with data=, not inside ...", call. = FALSE)
		}
		if (thisIs == "MxModel") {
			stop("umxRAM_GLM: do not nest MxModels in ...", call. = FALSE)
		}
	}
	dot.items = unlist(dot.items)

	if (typeof(model) != "character") {
		stop("umxRAM_GLM: first argument must be a model name (character). To update an existing RAM model, use mxModel().", call. = FALSE)
	}
	if (is.na(name)) name = model

	if (is.null(data)) {
		stop("umxRAM_GLM: you must set data= (a data.frame or raw mxData)", call. = FALSE)
	}
	if (inherits(data, "tbl")) data = as.data.frame(data)
	if (umx_is_MxData(data)) {
		if (data$type != "raw") {
			stop("umxRAM_GLM requires raw data (mxFitFunctionGLM does not take cov/cor)", call. = FALSE)
		}
		obs = data$observed
	} else if (is.data.frame(data)) {
		obs = data
	} else {
		stop("umxRAM_GLM: data must be a data.frame or raw mxData", call. = FALSE)
	}

	# ---- families: named list, plus any mxFamily in ... ----
	famList = list()
	if (!is.null(families)) {
		if (!is.list(families) || is.null(names(families)) || any(!nzchar(names(families)))) {
			stop("families= must be a named list (names are manifest variables)", call. = FALSE)
		}
		for (nm in names(families)) {
			item = families[[nm]]
			th = NULL
			if (!is.null(theta) && nm %in% names(theta)) th = theta[[nm]]
			if (is(item, "MxFamily")) {
				famList[[item@variable]] = item
			} else if (is.list(item) && !inherits(item, "family") && is(item[[1]], "MxFamily")) {
				for (k in 1:length(item)) {
					famList[[item[[k]]@variable]] = item[[k]]
				}
			} else {
				famList[[nm]] = mxFamily(nm, item, theta = th)
			}
		}
	}
	keepDots = list()
	for (i in seq_along(dot.items)) {
		thisIs = class(dot.items[[i]])[1]
		if (thisIs == "MxFamily") {
			famList[[dot.items[[i]]@variable]] = dot.items[[i]]
		} else if (is.list(dot.items[[i]]) && length(dot.items[[i]]) > 0 && is(dot.items[[i]][[1]], "MxFamily")) {
			for (k in 1:length(dot.items[[i]])) {
				famList[[dot.items[[i]][[k]]@variable]] = dot.items[[i]][[k]]
			}
		} else {
			keepDots[[length(keepDots) + 1]] = dot.items[[i]]
		}
	}
	if (length(famList) == 0) {
		stop("umxRAM_GLM: declare at least one non-Gaussian item with families= or mxFamily() in ... . For all-Gaussian models use umxRAM().", call. = FALSE)
	}
	famNames = names(famList)

	# ---- path names -> manifests / latents ----
	foundNames = c()
	fromNames = c()
	toNames = c()
	defnNames = c()
	for (thisItem in keepDots) {
		chunk = thisItem
		if (!is.list(chunk) || is(chunk, "MxPath")) chunk = list(chunk)
		for (j in seq_along(chunk)) {
			thisIs = class(chunk[[j]])[1]
			if (thisIs == "MxPath") {
				foundNames = append(foundNames, c(chunk[[j]]$from, chunk[[j]]$to))
				fromNames = append(fromNames, chunk[[j]]$from)
				toNames = append(toNames, chunk[[j]]$to)
				tmp = namez(chunk[[j]]$labels, "data\\.")
				if (length(tmp) > 0) {
					defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement = "\\1"))
				}
			} else if (umx_is_MxMatrix(chunk[[j]])) {
				tmp = namez(chunk[[j]]$labels, "data\\.")
				if (length(tmp) > 0) {
					defnNames = append(defnNames, namez(tmp, "data\\.(.*)", replacement = "\\1"))
				}
			} else if (isS4(chunk[[j]]) && grepl("^Mx", thisIs) && !thisIs %in% c("MxModel", "MxData")) {
				# mxConstraint, mxAlgebra, mxCI, ...
			} else {
				stop(paste("umxRAM_GLM does not accept", thisIs, "in ... (use umxPath / mxPath, mxMatrix, mxConstraint, mxFamily)"), call. = FALSE)
			}
		}
	}
	foundNames = unique(na.omit(foundNames))
	fromNames = unique(na.omit(fromNames))
	toNames = unique(na.omit(toNames))
	defnNames = unique(na.omit(defnNames))
	dataNames = unique(na.omit(umx_names(obs)))
	if (length(defnNames) > 0) {
		umx_check_names(defnNames, data = obs, message = "used as definition variable, but not present in data")
	}
	missingFam = setdiff(famNames, dataNames)
	if (length(missingFam) > 0) {
		stop(paste("families= names not in data:", paste(missingFam, collapse = ", ")), call. = FALSE)
	}
	latentVars = setdiff(foundNames, c(dataNames, "one"))
	usedManifests = unique(c(setdiff(intersect(dataNames, foundNames), "one"), famNames))
	usedManifests = setdiff(usedManifests, defnNames)
	predOnly = setdiff(intersect(fromNames, dataNames), c(toNames, famNames, "one", defnNames))
	predOnly = intersect(predOnly, usedManifests)
	if (length(usedManifests) < 1) {
		stop("umxRAM_GLM: no manifest variables found in paths or families", call. = FALSE)
	}

	myData = xmu_make_mxData(data = obs, type = "FIML", manifests = usedManifests, fullCovs = defnNames, verbose = verbose)

	newModel = do.call(mxModel, c(list(name = name, type = "RAM",
		manifestVars = usedManifests,
		latentVars = latentVars), keepDots, unname(famList)))
	newModel = mxModel(newModel, myData)

	# Means if missing (GLM linear predictors need them). Predictors are not outcomes.
	if (is.null(newModel$matrices$M)) {
		meanTo = setdiff(usedManifests, predOnly)
		if (length(meanTo) > 0) {
			newModel = mxModel(newModel, mxPath("one", to = meanTo))
		}
	}

	newModel = xmuLabel(newModel, suffix = suffix)
	if (setValues) {
		newModel = xmuValues(newModel, onlyTouchZeros = TRUE)
	}

	# Force GLM-item residual S to 0 (xmuValues would treat them as Gaussian)
	Snames = dimnames(newModel$S$values)[[1]]
	for (nm in famNames) {
		if (!nm %in% Snames) next
		newModel$S$values[nm, ] = 0
		newModel$S$values[, nm] = 0
		newModel$S$free[nm, ] = FALSE
		newModel$S$free[, nm] = FALSE
	}
	# Observed predictors: keep S at 0 (the arrow uses the data column)
	for (nm in predOnly) {
		if (!nm %in% Snames) next
		newModel$S$values[nm, ] = 0
		newModel$S$values[, nm] = 0
		newModel$S$free[nm, ] = FALSE
		newModel$S$free[, nm] = FALSE
	}
	# Gaussian leftovers need a free residual variance if the user did not add umxPath(var=)
	gaussNames = setdiff(usedManifests, c(famNames, predOnly))
	for (nm in gaussNames) {
		if (!nm %in% Snames) next
		if (!isTRUE(newModel$S$free[nm, nm])) {
			newModel$S$free[nm, nm] = TRUE
			if (setValues) {
				v = var(obs[[nm]], na.rm = TRUE)
				if (!is.finite(v) || v <= 0) v = 1
				newModel$S$values[nm, nm] = v
			} else if (newModel$S$values[nm, nm] == 0) {
				newModel$S$values[nm, nm] = 1
			}
		}
	}

	# Start means on the scale of the linear predictor
	if (!is.null(newModel$matrices$M) && setValues) {
		for (nm in usedManifests) {
			if (!isTRUE(newModel$M$free[1, nm])) next
			col = obs[[nm]]
			col = col[is.finite(col)]
			if (length(col) < 1) next
			if (nm %in% famNames) {
				fn = famList[[nm]]@family
				lk = famList[[nm]]@link
				mny = mean(col)
				if (fn %in% c("poisson", "negativebinomial", "gamma", "inversegaussian")) {
					newModel$M$values[1, nm] = log(max(mny, 1e-6))
				} else if (fn == "binomial" && lk == "logit") {
					p = min(max(mny, 0.01), 0.99)
					newModel$M$values[1, nm] = log(p / (1 - p))
				} else if (fn == "binomial" && lk == "probit") {
					p = min(max(mny, 0.01), 0.99)
					newModel$M$values[1, nm] = qnorm(p)
				}
			}
		}
	}
	# xmuValues uses Gaussian SEM starts (0.5 * sqrt(var_to/var_from)). That is
	# exp() overflow on a log/logit box. Observed arrows into family items start at 0.
	if (setValues && !is.null(newModel$matrices$A)) {
		Afrom = dimnames(newModel$A$values)[[2]]
		obsFrom = intersect(Afrom, dataNames)
		for (nm in famNames) {
			if (!nm %in% dimnames(newModel$A$values)[[1]]) next
			for (fromNm in obsFrom) {
				if (fromNm == nm) next
				if (isTRUE(newModel$A$free[nm, fromNm])) {
					newModel$A$values[nm, fromNm] = 0
				}
			}
		}
	}

	if (!is(newModel$fitfunction, "MxFitFunctionGLM")) {
		stop("umxRAM_GLM: expected mxFamily to install mxFitFunctionGLM; got ", class(newModel$fitfunction)[1], call. = FALSE)
	}
	if (length(nAGQ) != 1) stop("nAGQ must be a single integer or NA", call. = FALSE)
	newModel$fitfunction$nAGQ = as.integer(nAGQ)

	newModel = omxAssignFirstParameters(newModel)
	newModel = mxOption(newModel, "Calculate Hessian", "Yes")
	newModel = mxOption(newModel, "Standard Errors", "Yes")
	newModel = xmu_safe_run_summary(newModel, autoRun = autoRun, tryHard = tryHard, comparison = FALSE, refModels = FALSE, std = FALSE, summary = TRUE)
	invisible(newModel)
}
