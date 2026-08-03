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
		deMeta = attr(model, "umxDE")
		# DE pairs use free shared cont/cens means (not binary mean@0)
		deMeanBases = character(0)
		if (isTRUE(deMeta$freeVariance) || isTRUE(deMeta$equateMeansWithCont)) {
			deMeanBases = unique(c(names(deMeta$fixedCuts), deMeta$freeThresholdPairs, names(deMeta$contByCens)))
			deMeanBases = deMeanBases[!is.na(deMeanBases) & nzchar(as.character(deMeanBases))]
		}
		devFree = NULL
		if (!is.null(model$top$deviations_for_thresh)) {
			devFree = model$top$deviations_for_thresh$free
		}
		for (v in binVars) {
			if (!is.null(meanNames) && v %in% meanNames) {
				if (isTRUE(em$free[1, v])) {
					allowFreeMean = FALSE
					if (length(deMeanBases) > 0) {
						for (b in deMeanBases) {
							if (identical(v, b) || startsWith(v, paste0(b, "_")) || grepl(paste0("^", b, "[0-9]+$"), v)) {
								allowFreeMean = TRUE
								break
							}
						}
					}
					# Structure fallback: threshold fixed (known cut) ⇒ free mean OK
					if (!allowFreeMean && !is.null(devFree) && v %in% colnames(devFree)) {
						if (!isTRUE(devFree[1, v])) {
							allowFreeMean = TRUE
						}
					}
					if (!allowFreeMean) {
						problems = c(problems, paste0(v, " binary mean free (should be fixed)"))
					}
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
		# DE pairs free continuous variance (not V=1). Exempt DE binaries from V=1 requirement.
		needV1 = binVars
		deMeta = attr(model, "umxDE")
		deBases = character(0)
		if (isTRUE(deMeta$freeVariance)) {
			deBases = unique(c(names(deMeta$fixedCuts), deMeta$freeThresholdPairs, names(deMeta$contByCens)))
			deBases = deBases[!is.na(deBases) & nzchar(deBases)]
		}
		if (length(deBases) > 0) {
			for (v in binVars) {
				for (b in deBases) {
					if (identical(v, b) || startsWith(v, paste0(b, "_")) || grepl(paste0("^", b, "[0-9]+$"), v)) {
						needV1 = setdiff(needV1, v)
						break
					}
				}
			}
		}
		# Structure fallback: no binary V=1 constraint left
		if (length(needV1) > 0 && is.null(model$top$constrain_Bin_var_to_1)) {
			needV1 = character(0)
		}
		if (length(needV1) > 0) {
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