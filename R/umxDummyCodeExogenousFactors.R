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

#' Replace unordered exogenous factors with glm dummy columns
#'
#' @description
#' Takes a single-group raw-data RAM model and returns a new model.
#' Each unordered factor that is used only as a path source is replaced by the dummy columns [stats::glm()] would build for that factor.
#' Ordered factors are left unchanged. An ordered factor is a threshold variable, not a set of dummies.
#'
#' The default contrast follows `glm`. A `contrasts` attribute already on the factor is kept. Otherwise the function uses `getOption("contrasts")[1]`, which is treatment contrasts in a default R session.
#' `contrasts = "treatment"` drops one level. `reference = NULL` drops the first level. The existing mean path from `one` is the intercept. Dummy columns are not given a free mean or a residual variance.
#'
#' A label on the original path is not copied. The same label on every dummy path would equate those shifts. Each new path is labeled `dummy_to_destination`. A label you set yourself is a stop.
#' Product columns for an interaction are not created. Expanding `outcome` and `treatment` matches `counts ~ outcome + treatment`.
#'
#' The returned model has not been run. A fit already stored on the input is dropped.
#'
#' @param model a single-group RAM [MxModel()] with raw data. Expectation must be RAM. The fit function must be `MxFitFunctionML` or `MxFitFunctionGLM`.
#' @param contrasts `NULL` (glm's rule), or one of `"treatment"`, `"sum"`, `"helmert"`, `"poly"`. A character value overrides the factor attribute and `getOption("contrasts")`.
#' @param reference `NULL`, one level name, or a named character vector of level names. Treatment contrasts only. `NULL` drops the first level. A single string is the reference for every recoded factor. A named vector sets the reference for the factors it names; any other recoded factor still drops its first level.
#' @return The rewritten [MxModel()]. Dummy columns replace the factor columns in the data and in `manifestVars`. The model has not been run.
#' @export
#' @seealso [umxRAM_GLM()], [stats::glm()], [stats::model.matrix()]
#' @md
#' @examples
#' \dontrun{
#' # Dobson (1990) page 93: Randomized Controlled Trial
#' counts = c(18, 17, 15, 20, 10, 20, 25, 13, 12)
#' outcome = gl(n = 3, k = 1, length = 9)
#' treatment = gl(n = 3, k = 3)
#' df = data.frame(counts = counts, outcome = outcome, treatment = treatment)
#' m1 = umxRAM_GLM("rct",
#' 	umxPath(c("outcome", "treatment"), to = "counts"),
#' 	umxPath("one", to = "counts"),
#' 	data = df,
#' 	families = list(counts = poisson()),
#' 	autoRun = FALSE)
#' m1 = umxDummyCodeExogenousFactors(m1)
#' m1 = mxRun(m1)
#' }
umxDummyCodeExogenousFactors <- function(model, contrasts = NULL, reference = NULL) {
	if (!isS4(model) || !is(model, "MxModel")) {
		stop("umxDummyCodeExogenousFactors needs an MxModel", call. = FALSE)
	}
	if (length(model@submodels) > 0) {
		stop("umxDummyCodeExogenousFactors does not handle submodels. Pass one group.", call. = FALSE)
	}
	if (!is(model$expectation, "MxExpectationRAM")) {
		stop("umxDummyCodeExogenousFactors needs a RAM expectation", call. = FALSE)
	}
	fitOk = is(model$fitfunction, "MxFitFunctionGLM") || is(model$fitfunction, "MxFitFunctionML")
	if (!fitOk) {
		stop("umxDummyCodeExogenousFactors needs MxFitFunctionML or MxFitFunctionGLM", call. = FALSE)
	}
	if (is.null(model$data) || !identical(model$data$type, "raw")) {
		stop("umxDummyCodeExogenousFactors needs raw data", call. = FALSE)
	}
	weight = model$data@weight
	if (!(is.null(weight) || (length(weight) == 1 && is.na(weight)))) {
		stop("umxDummyCodeExogenousFactors does not handle a row weight", call. = FALSE)
	}
	if (!is.null(contrasts)) {
		if (!is.character(contrasts) || length(contrasts) != 1 || is.na(contrasts)) {
			stop("contrasts must be NULL or one of treatment, sum, helmert, poly", call. = FALSE)
		}
		contrasts = match.arg(contrasts, c("treatment", "sum", "helmert", "poly"))
	}
	if (!is.null(reference)) {
		if (!is.character(reference) || length(reference) < 1 || anyNA(reference) || any(!nzchar(reference))) {
			stop("reference must be NULL, one level name, or a named character vector of level names", call. = FALSE)
		}
		if (is.null(names(reference))) {
			if (length(reference) != 1) {
				stop("reference must be one level name, or a named vector with one level per factor", call. = FALSE)
			}
		} else if (any(!nzchar(names(reference)))) {
			stop("every name in reference must be a factor column", call. = FALSE)
		}
	}
	if (!is.null(contrasts) && contrasts != "treatment" && !is.null(reference)) {
		stop("reference applies only to treatment contrasts", call. = FALSE)
	}
	if (is.null(model$A) || is.null(model$S)) {
		stop("umxDummyCodeExogenousFactors needs RAM matrices A and S", call. = FALSE)
	}

	observed = model$data$observed
	if (!is.data.frame(observed)) {
		stop("raw data must be a data.frame", call. = FALSE)
	}
	manifests = model@manifestVars
	varNames = dimnames(model$A$values)[[1]]
	if (is.null(varNames) || !identical(varNames, dimnames(model$A$values)[[2]])) {
		stop("A must have matching row and column names", call. = FALSE)
	}
	famNames = character()
	if (is(model$fitfunction, "MxFitFunctionGLM")) {
		famNames = names(model$fitfunction@family)
		if (is.null(famNames)) famNames = character()
	}
	labelPool = c(model$A$labels, model$S$labels)
	if (!is.null(model$M)) labelPool = c(labelPool, model$M$labels)

	orderedNames = character()
	factorNames = character()
	for (col in intersect(names(observed), manifests)) {
		if (is.ordered(observed[[col]])) {
			orderedNames = c(orderedNames, col)
		} else if (is.factor(observed[[col]])) {
			factorNames = c(factorNames, col)
		}
	}
	if (length(orderedNames) > 0) {
		message("Left ordered factors unchanged: ", omxQuotes(orderedNames))
	}
	if (length(factorNames) == 0) {
		return(model)
	}

	# Record every replacement before editing the model, so a stop leaves the model untouched.
	jobs = list()
	for (factorName in factorNames) {
		if (factorName %in% famNames) {
			stop(paste0(factorName, " is a GLM family item. Dummy coding is for exogenous predictors."), call. = FALSE)
		}
		if (any(labelPool == paste0("data.", factorName), na.rm = TRUE)) {
			stop(paste0(factorName, " is used as a definition variable (data.", factorName, ")."), call. = FALSE)
		}
		for (src in varNames) {
			srcFree = isTRUE(model$A$free[factorName, src])
			srcValue = model$A$values[factorName, src]
			if (srcFree || (is.finite(srcValue) && srcValue != 0)) {
				stop(paste0(factorName, " is not an exogenous predictor. It has an incoming path from ", omxQuotes(src), "."), call. = FALSE)
			}
		}
		for (other in varNames) {
			sFree = isTRUE(model$S$free[factorName, other]) || isTRUE(model$S$free[other, factorName])
			sValue = model$S$values[factorName, other]
			sValue2 = model$S$values[other, factorName]
			sNonZero = (is.finite(sValue) && sValue != 0) || (is.finite(sValue2) && sValue2 != 0)
			if (sFree || sNonZero) {
				stop(paste0(factorName, " is not exogenous. It has a free or non-zero residual covariance."), call. = FALSE)
			}
		}
		if (!is.null(model$M) && isTRUE(model$M$free[1, factorName])) {
			stop(paste0(factorName, " has a free mean, so it is not an exogenous predictor."), call. = FALSE)
		}

		f = factor(observed[[factorName]], levels = levels(observed[[factorName]]))
		nLev = nlevels(f)
		if (nLev < 2) {
			stop(paste0(factorName, " has fewer than two levels. There is no contrast."), call. = FALSE)
		}
		thisReference = NULL
		if (!is.null(reference)) {
			if (is.null(names(reference))) {
				thisReference = reference
			} else if (factorName %in% names(reference)) {
				thisReference = unname(reference[[factorName]])
			}
		}
		attrC = attr(f, "contrasts")
		useExistingMatrix = FALSE
		if (!is.null(contrasts)) {
			contrastKey = contrasts
		} else if (is.matrix(attrC)) {
			if (!is.null(thisReference)) {
				stop(paste0(factorName, " already has a contrast matrix. reference applies only when setting treatment contrasts."), call. = FALSE)
			}
			useExistingMatrix = TRUE
			contrastKey = "existing"
		} else if (is.character(attrC) && length(attrC) == 1 && !is.na(attrC)) {
			contrastKey = sub("^contr\\.", "", attrC)
		} else {
			contrastKey = sub("^contr\\.", "", getOption("contrasts")[[1]])
		}
		if (!useExistingMatrix && !contrastKey %in% c("treatment", "sum", "helmert", "poly")) {
			stop(paste0("contrasts for ", factorName, " must be treatment, sum, helmert, or poly. Got ", omxQuotes(contrastKey), "."), call. = FALSE)
		}
		if (contrastKey != "treatment" && !is.null(thisReference)) {
			stop("reference applies only to treatment contrasts", call. = FALSE)
		}

		one = data.frame(f, stringsAsFactors = FALSE)
		names(one) = factorName
		contrastLabel = NULL
		if (useExistingMatrix) {
			contrastLabel = "Contrasts already set on the factor"
		} else if (contrastKey == "treatment") {
			if (is.null(thisReference)) {
				thisReference = levels(f)[1]
				base = 1L
			} else {
				base = match(thisReference, levels(f))
				if (is.na(base)) {
					stop(paste0("reference ", omxQuotes(thisReference), " is not a level of ", factorName, "."), call. = FALSE)
				}
			}
			stats::contrasts(one[[factorName]]) = stats::contr.treatment(nLev, base = base)
			contrastLabel = "Treatment contrasts"
		} else if (contrastKey == "sum") {
			stats::contrasts(one[[factorName]]) = stats::contr.sum(nLev)
			contrastLabel = "Sum contrasts"
		} else if (contrastKey == "helmert") {
			stats::contrasts(one[[factorName]]) = stats::contr.helmert(nLev)
			contrastLabel = "Helmert contrasts"
		} else if (contrastKey == "poly") {
			stats::contrasts(one[[factorName]]) = stats::contr.poly(nLev)
			contrastLabel = "Polynomial contrasts"
		}
		mm = stats::model.matrix(stats::reformulate(factorName), data = one)
		if (colnames(mm)[1] != "(Intercept)" || ncol(mm) < 2) {
			stop(paste0("model.matrix did not return an intercept and dummy columns for ", factorName), call. = FALSE)
		}
		dummies = mm[, -1, drop = FALSE]
		dummyNames = colnames(dummies)
		for (dummyName in dummyNames) {
			if (!grepl("^[A-Za-z.][A-Za-z0-9._]*$", dummyName)) {
				stop(paste0("Dummy column ", omxQuotes(dummyName), " from ", factorName, " is not a legal matrix name. Rename that level."), call. = FALSE)
			}
		}

		destNames = character()
		oldFree = logical()
		oldValue = numeric()
		oldLbound = numeric()
		oldUbound = numeric()
		for (dest in varNames) {
			if (dest == factorName) next
			destFree = isTRUE(model$A$free[dest, factorName])
			destValue = model$A$values[dest, factorName]
			if (!(destFree || (is.finite(destValue) && destValue != 0))) next
			oldLabel = model$A$labels[dest, factorName]
			defaultLabel = paste0(factorName, "_to_", dest)
			if (!is.na(oldLabel) && oldLabel != defaultLabel) {
				stop(paste0("Path ", factorName, " -> ", dest, " has label ", omxQuotes(oldLabel), ". One label cannot be copied onto every dummy path."), call. = FALSE)
			}
			destNames = c(destNames, dest)
			oldFree = c(oldFree, destFree)
			oldValue = c(oldValue, destValue)
			oldLbound = c(oldLbound, model$A$lbound[dest, factorName])
			oldUbound = c(oldUbound, model$A$ubound[dest, factorName])
		}
		if (length(destNames) == 0) {
			stop(paste0(factorName, " has no outgoing path to replace."), call. = FALSE)
		}
		contrastMatrix = NULL
		if (useExistingMatrix) contrastMatrix = attrC
		levelCount = table(f)
		emptyLevels = names(levelCount)[levelCount == 0]
		jobs[[length(jobs) + 1]] = list(
			factorName = factorName,
			contrastKey = contrastKey,
			contrastLabel = contrastLabel,
			referenceLevel = thisReference,
			contrastMatrix = contrastMatrix,
			dummyNames = dummyNames,
			destNames = destNames,
			oldFree = oldFree,
			oldValue = oldValue,
			oldLbound = oldLbound,
			oldUbound = oldUbound,
			emptyLevels = emptyLevels
		)
	}

	dummyNames = character()
	for (job in jobs) dummyNames = c(dummyNames, job$dummyNames)
	if (length(unique(dummyNames)) != length(dummyNames)) {
		stop(paste0("Dummy columns are not unique: ", omxQuotes(dummyNames[duplicated(dummyNames)])), call. = FALSE)
	}
	taken = c(names(observed), model@latentVars)
	clash = intersect(dummyNames, taken)
	if (length(clash) > 0) {
		stop(paste0("Dummy column already exists: ", omxQuotes(clash)), call. = FALSE)
	}

	keepRows = stats::complete.cases(observed[, factorNames, drop = FALSE])
	droppedRows = sum(!keepRows)
	if (sum(keepRows) < 1) {
		stop("every row has NA in an exogenous factor", call. = FALSE)
	}
	wideData = observed[keepRows, , drop = FALSE]
	for (jobIndex in seq_along(jobs)) {
		job = jobs[[jobIndex]]
		# model.matrix on the unfiltered column was only for names and checks. The data columns come from the kept rows.
		f = factor(wideData[[job$factorName]], levels = levels(observed[[job$factorName]]))
		one = data.frame(f, stringsAsFactors = FALSE)
		names(one) = job$factorName
		if (!is.null(job$contrastMatrix)) {
			stats::contrasts(one[[job$factorName]]) = job$contrastMatrix
		} else if (job$contrastKey == "treatment") {
			base = match(job$referenceLevel, levels(f))
			stats::contrasts(one[[job$factorName]]) = stats::contr.treatment(nlevels(f), base = base)
		} else if (job$contrastKey == "sum") {
			stats::contrasts(one[[job$factorName]]) = stats::contr.sum(nlevels(f))
		} else if (job$contrastKey == "helmert") {
			stats::contrasts(one[[job$factorName]]) = stats::contr.helmert(nlevels(f))
		} else if (job$contrastKey == "poly") {
			stats::contrasts(one[[job$factorName]]) = stats::contr.poly(nlevels(f))
		}
		mm = stats::model.matrix(stats::reformulate(job$factorName), data = one)
		dummies = mm[, -1, drop = FALSE]
		colnames(dummies) = job$dummyNames
		levelCount = table(f)
		jobs[[jobIndex]]$emptyLevels = names(levelCount)[levelCount == 0]
		wideData = data.frame(wideData, dummies, check.names = FALSE, stringsAsFactors = FALSE)
	}

	# Data first. Adding manifests in the same mxModel call rebuilds F against the previous data, which does not yet contain the dummy columns.
	model = mxModel(model, mxData(observed = wideData, type = "raw"))
	model = mxModel(model, manifestVars = dummyNames)
	fromList = character()
	toList = character()
	freeList = logical()
	valueList = numeric()
	labelList = character()
	lboundList = numeric()
	uboundList = numeric()
	for (job in jobs) {
		for (d in seq_along(job$destNames)) {
			dest = job$destNames[d]
			for (dummyName in job$dummyNames) {
				fromList = c(fromList, dummyName)
				toList = c(toList, dest)
				freeList = c(freeList, job$oldFree[d])
				valueList = c(valueList, job$oldValue[d])
				labelList = c(labelList, paste0(dummyName, "_to_", dest))
				lboundList = c(lboundList, job$oldLbound[d])
				uboundList = c(uboundList, job$oldUbound[d])
			}
		}
	}
	model = mxModel(model, mxPath(from = fromList, to = toList, arrows = 1, free = freeList, values = valueList, labels = labelList, lbound = lboundList, ubound = uboundList))

	# mxModel(remove = TRUE) dies here: removeVariablesRAM reads Thresholds@values, and a GLM RAM model has no Thresholds object. It also leaves M at the old size.
	keepManifests = setdiff(model@manifestVars, factorNames)
	keepVars = c(keepManifests, model@latentVars)
	Aold = model$A
	Sold = model$S
	freeA = Aold$free[keepVars, keepVars, drop = FALSE]
	freeS = Sold$free[keepVars, keepVars, drop = FALSE]
	storage.mode(freeA) = "logical"
	storage.mode(freeS) = "logical"
	Anew = mxMatrix("Full",
		values = Aold$values[keepVars, keepVars, drop = FALSE],
		free = freeA,
		labels = Aold$labels[keepVars, keepVars, drop = FALSE],
		lbound = Aold$lbound[keepVars, keepVars, drop = FALSE],
		ubound = Aold$ubound[keepVars, keepVars, drop = FALSE],
		name = "A", byrow = FALSE,
		dimnames = list(keepVars, keepVars))
	Snew = mxMatrix("Symm",
		values = Sold$values[keepVars, keepVars, drop = FALSE],
		free = freeS,
		labels = Sold$labels[keepVars, keepVars, drop = FALSE],
		lbound = Sold$lbound[keepVars, keepVars, drop = FALSE],
		ubound = Sold$ubound[keepVars, keepVars, drop = FALSE],
		name = "S", byrow = FALSE,
		dimnames = list(keepVars, keepVars))
	nMan = length(keepManifests)
	nVar = length(keepVars)
	Fvalues = diag(nrow = nMan, ncol = nVar)
	Fnew = mxMatrix("Full", values = Fvalues, free = FALSE, name = "F", dimnames = list(keepManifests, keepVars))
	finalData = wideData[, keepManifests, drop = FALSE]
	model@manifestVars = keepManifests
	if (!is.null(model$M)) {
		Mold = model$M
		valuesM = Mold$values[, keepVars, drop = FALSE]
		freeM = Mold$free[, keepVars, drop = FALSE]
		labelsM = Mold$labels[, keepVars, drop = FALSE]
		storage.mode(freeM) = "logical"
		for (dummyName in dummyNames) {
			freeM[1, dummyName] = FALSE
			valuesM[1, dummyName] = 0
			labelsM[1, dummyName] = as.character(NA)
		}
		Mnew = mxMatrix("Full",
			values = valuesM,
			free = freeM,
			labels = labelsM,
			lbound = Mold$lbound[, keepVars, drop = FALSE],
			ubound = Mold$ubound[, keepVars, drop = FALSE],
			name = "M",
			dimnames = list(NULL, keepVars))
		model = mxModel(model, Anew, Snew, Mnew, Fnew, mxData(observed = finalData, type = "raw"))
	} else {
		model = mxModel(model, Anew, Snew, Fnew, mxData(observed = finalData, type = "raw"))
	}
	if (length(model@output) > 0) {
		model@output = list()
	}

	if (droppedRows > 0) {
		message("Dropped ", droppedRows, " row(s) with NA in an exogenous factor.")
	}
	toldMean = character()
	for (job in jobs) {
		if (length(job$emptyLevels) > 0) {
			message(job$factorName, " level ", omxQuotes(job$emptyLevels), " has no rows. The dummy column is all zero, as in glm.")
		}
		pathBits = character()
		replaceBits = character()
		for (dest in job$destNames) {
			replaceBits = c(replaceBits, paste0(job$factorName, " -> ", dest))
			for (dummyName in job$dummyNames) {
				pathBits = c(pathBits, paste0(dummyName, " -> ", dest))
			}
			if (!(dest %in% toldMean)) {
				hasMean = FALSE
				if (!is.null(model$M) && dest %in% colnames(model$M$values)) {
					if (isTRUE(model$M$free[1, dest])) hasMean = TRUE
					else if (is.finite(model$M$values[1, dest]) && model$M$values[1, dest] != 0) hasMean = TRUE
				}
				if (!hasMean) {
					message("No intercept path to ", dest, ". The reference cell keeps the current mean.")
				}
				toldMean = c(toldMean, dest)
			}
		}
		if (job$contrastKey == "treatment") {
			head = paste0(job$contrastLabel, ", reference level ", omxQuotes(job$referenceLevel))
		} else {
			head = job$contrastLabel
		}
		message(job$factorName, " is an unordered factor. ", head, ". Paths ", paste(pathBits, collapse = " and "), " replace ", paste(replaceBits, collapse = " and "), ".")
	}
	model
}
