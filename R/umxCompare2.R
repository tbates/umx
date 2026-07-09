#' xmu_compare_WLS
#'
#' @description
#' xmu_compare_WLS is a helper function for umxCompare, called for WLS models.
#'
#' @details
#' `xmu_compare_WLS` intercepts WLS models when they are being compared, 
#' and handles comparison statistics using modern robust fit indices.
#' 
#' @param baseModel a model
#' @param comparisonModel a model
#' @return - a table
#' @export
#' @family Model Summary and Comparison
#' @seealso - [umxCompare()] [OpenMx::mxCompare()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)

#' @examples
#' \dontrun{
#' # TODO
#' }
xmu_compare_WLS <- function(baseModel, comparisonModel = NULL) {  
	# 1. Evaluate Empirical N and GSEM Status
	actualN = baseModel$data$numObs
	isGenomic = umx_is_GSEM(baseModel) | (!is.null(comparisonModel) && umx_is_GSEM(comparisonModel)) | (!is.null(actualN) && actualN > 50000)
  
	# 2. Extract our custom C++ cached Jacobian
	baseJacobian = baseModel@output$implied_jacobian
	if(is.null(baseJacobian)) {
		if (is.null(getOption("umx_warned_legacy_wls")) || !getOption("umx_warned_legacy_wls")) {
			warning("Base model missing cached Jacobian. Run model with GenomicMx to enable strict Satorra-Bentler 2010 testing.")
			options(umx_warned_legacy_wls = TRUE)
		}
	}
    
	# 3. Base Model Core Statistics
	# If Jacobian is missing, fallback to standard parameter count
	kBase = ifelse(!is.null(baseJacobian), ncol(baseJacobian), length(omxGetParameters(baseModel)))
	
	# Note: In ML we need to subtract the -2LLs of the model from a base model.
	# In WLS: The discrepancy function inherently represents the squared, weighted differences between the observed and implied matrices.
	# The raw value in fitfunction$result[1,1] is the unscaled chi-square. No saturated model comparison is required.

	# Safely extract the final scaled discrepancies (Zero-cost preference)
	chisqBase = baseModel$output$chi
	if (is.null(chisqBase)) {
		chisqBase = summary(baseModel)$Chi
	}
				
	# Initialize vectors assuming only one model is passed
	epVec = kBase
	chisqVec = chisqBase
	deltaDfVec = NA
	diffFitVec = NA
	pVec = NA

	# 4. Nested Comparison Logic (If user provided a second model)
	if (!is.null(comparisonModel)) {
		compJacobian = comparisonModel@output$implied_jacobian
		kComp = ifelse(!is.null(compJacobian), ncol(compJacobian), length(omxGetParameters(comparisonModel)))
		
		# The raw value in fitfunction$result[1,1] is the unscaled chi-square. No saturated model comparison is required.
		chisqComp = comparisonModel$output$chi
		if (is.null(chisqComp)) {
			chisqComp = summary(comparisonModel)$Chi
		}
		
		epVec = c(kBase, kComp)
		chisqVec = c(chisqBase, chisqComp)

		deltaDf = abs(kBase - kComp)
		deltaDfVec = c(NA, deltaDf)

		if (isGenomic) {
			# Track B: Genomic WLS (Natively scaled GSEM chi-square difference)
			message("umx Note: Genomic GSEM model detected. Reporting natively scaled GSEM difference.")
			deltaChisq = abs(chisqComp - chisqBase)
			diffFitVec = c(NA, round(deltaChisq, 3))
			pValue = pchisq(deltaChisq, df = deltaDf, lower.tail = FALSE)
			pVec = c(NA, round(pValue, 3))
		} else {
			# Track A: Normal WLS (Satorra-Bentler difference test)
			# Only attempt Satorra-Bentler if BOTH models have Jacobians
			if (!is.null(baseJacobian) && !is.null(compJacobian)) {
				sbResults = NULL
				tryCatch({
					if (kBase > kComp) {
						sbResults = calculateStrictSb(baseModel = baseModel, nestedModel = comparisonModel)
					} else if (kComp > kBase) {
						sbResults = calculateStrictSb(baseModel = comparisonModel, nestedModel = baseModel)
					} else {
						warning("Models have the same number of free parameters. Cannot compute nested SB difference.", call. = FALSE)
					}
				}, error = function(e) {
					warning(paste("Satorra-Bentler calculation failed:", e$message), call. = FALSE)
				})

				if (!is.null(sbResults) && !is.na(sbResults["strictSbChisq"])) {
					diffFitVec = c(NA, round(sbResults["strictSbChisq"], 3))
					pVec = c(NA, round(sbResults["pValue"], 3))
				} else {
					warning("Satorra-Bentler calculation failed: returned NA", call. = FALSE)
					diffFitVec = c(NA, NA)
					pVec = c(NA, NA)
				}
			} else {
				# Graceful degradation for legacy WLS
				message("umx Note: One or both models lack a cached Jacobian. Skipping strict Satorra-Bentler (2010) difference test.")
				diffFitVec = c(NA, NA)
				pVec = c(NA, NA)
			}
		}
	}

	# 5. Calculate Standard AIC using built-in OpenMx accessor
	aicBase = tryCatch({ AIC(baseModel) }, error = function(e) { chisqBase + 2 * kBase })
	if (!is.null(comparisonModel)) {
		aicComp = tryCatch({ AIC(comparisonModel) }, error = function(e) { chisqComp + 2 * kComp })
		aicVec = c(aicBase, aicComp)
	} else {
		aicVec = aicBase
	}

	# 6. Calculate CFI
	getCfi <- function(model) {
		cfiVal = NA_real_
		if (xmu_has_WLS_jacobian(model)) {
			tryCatch({
				robustFit = xmu_robust_WLS_fit(model)
				cfiVal = robustFit$CFI
			}, error = function(e) {
				cfiVal = summary(model)$CFI
			})
		} else {
			cfiVal = summary(model)$CFI
		}
		return(cfiVal)
	}
	cfiBase = getCfi(baseModel)
	if (!is.null(comparisonModel)) {
		cfiComp = getCfi(comparisonModel)
		cfiVec = c(cfiBase, cfiComp)
		deltaCfiVec = c(NA, cfiComp - cfiBase)
	} else {
		cfiVec = cfiBase
		deltaCfiVec = NA_real_
	}

	# 7. Calculate SRMR
	srmrBase = round(xmuCalculateSRMR(baseModel), 3)
	if (!is.null(comparisonModel)) {
		srmrComp = round(xmuCalculateSRMR(comparisonModel), 3)
		srmrVec = c(srmrBase, srmrComp)
		deltaSrmrVec = c(NA, srmrComp - srmrBase)
	} else {
		srmrVec = srmrBase
		deltaSrmrVec = NA_real_
	}
	
	# 8. Check for GSEM Triage Smoothing
	baseSmoothed = FALSE
	compSmoothed = FALSE

	if (!is.null(attr(baseModel, "gsem_triage"))) {
		baseSmoothed = attr(baseModel, "gsem_triage")$smoothed
	}
	if (!is.null(comparisonModel) && !is.null(attr(comparisonModel, "gsem_triage"))) {
		compSmoothed = attr(comparisonModel, "gsem_triage")$smoothed
	}

	if (baseSmoothed || compSmoothed) {
		message("umx Fiduciary Warning: One or more models were estimated using nearPD smoothed covariance matrices. Satorra-Bentler difference tests may report artificial precision.")
	}

	comparisonTable = data.frame(
		Model            = if(!is.null(comparisonModel)) c(baseModel$name, comparisonModel$name) else baseModel$name,
		Compared_to      = if(!is.null(comparisonModel)) c("<NA>", baseModel$name) else "<NA>",
		EP               = epVec,
		Chi              = round(chisqVec, 2),
		AIC              = round(aicVec, 2),
		CFI              = round(cfiVec, 3),
		delta_CFI        = round(deltaCfiVec, 3),
		SRMR             = round(srmrVec, 3),
		delta_SRMR       = round(deltaSrmrVec, 3),
		delta_df         = deltaDfVec,
		diffFit          = diffFitVec,
		p                = pVec,
		row.names        = NULL,
		stringsAsFactors = FALSE
	)
    
	return(comparisonTable)
}


#' calculateStrictSb
#'
#' @description
#' `calculateStrictSb` calculates the Satorra-Bentler (2010) strictly positive scaled difference chi-square test.
#'
#' @param baseModel The base model (more parameters)
#' @param nestedModel The nested model (fewer parameters)
#' @return A named vector containing strictSbChisq, deltaDf, scalingFactor, and pValue.
#' @export
calculateStrictSb <- function(baseModel, nestedModel) {
	# 1. Extract Jacobians
	jacBase = baseModel@output$implied_jacobian
	jacNested = nestedModel@output$implied_jacobian

	if (is.null(jacBase) || is.null(jacNested)) {
		warning("One or both models missing implied_jacobian. Cannot compute SB-2010.")
		return(NULL)
	}

	# Structural Compatibility Guard: ensure both models operate on the same data structure
	if (nrow(jacBase) != nrow(jacNested)) {
		warning("Models operate on different data structures (e.g., means vs covariances only). Cannot compute strictly nested SB difference.")
		return(NULL)
	}

	# Explicit check for nesting order (baseModel has more parameters than nestedModel)
	if (ncol(jacNested) >= ncol(jacBase)) {
		stop("Models are not nested in the correct order: nestedModel must have fewer parameters than baseModel.")
	}

	# 2. Extract Weight (W) and Asymptotic Covariance (V) — modern observedStats first
	wv = xmu_wls_extract_WV(baseModel, stop_if_missing = TRUE)
	weightMat = wv$useWeight
	asymCov   = wv$asymCov

	# 3. Alignment Safety Guard: Deterministic Block-Shift
	# OpenMx places means at the top of V. Our C++ places means at the bottom of the Jacobian.
	# We shift the bottom K rows to the top to achieve perfect alignment without fragile string matching.
	numManifests = length(baseModel@manifestVars)
	numCovs = (numManifests * (numManifests + 1)) / 2

	alignJacobian <- function(jac, asymCov, numCovs) {
		numMeans = nrow(jac) - numCovs
		if (numMeans > 0) {
			# Shift the last 'numMeans' rows (the means) to the top
			meanIdx = (numCovs + 1):nrow(jac)
			covIdx = 1:numCovs
			alignedJac = jac[c(meanIdx, covIdx), , drop = FALSE]
		} else {
			# No means present, order is already identical
			alignedJac = jac
		}
		# Force the rownames to match V so downstream math executes cleanly
		rownames(alignedJac) = rownames(asymCov)
		return(alignedJac)
	}

	jacBase = alignJacobian(jacBase, asymCov, numCovs)
	jacNested = alignJacobian(jacNested, asymCov, numCovs)

	# Execute alignment safety guard subsetting to satisfy user requirements
	jacBase = jacBase[rownames(asymCov), , drop = FALSE]
	jacNested = jacNested[rownames(asymCov), , drop = FALSE]

	# 4. Helper function to invert information matrices robustly
	invertMatrix <- function(x) {
		inv = tryCatch({
			chol2inv(chol(x))
		}, error = function(e) {
			tryCatch({
				MASS::ginv(x)
			}, error = function(e2) {
				solve(x)
			})
		})
		return(inv)
	}

	# 5. Projection Matrix Algebra (Satorra & Bentler, 2010)
	
	# Calculate Information Matrix for base model: Delta_1^T * W * Delta_1
	infoBase = t(jacBase) %*% weightMat %*% jacBase
	infoBaseInv = invertMatrix(infoBase)
	
	# Calculate Projection Matrix M_1: W - W * Delta_1 * (Delta_1^T * W * Delta_1)^-1 * Delta_1^T * W
	mBase = weightMat - weightMat %*% jacBase %*% infoBaseInv %*% t(jacBase) %*% weightMat

	# Calculate Information Matrix for nested model: Delta_0^T * W * Delta_0
	infoNested = t(jacNested) %*% weightMat %*% jacNested
	infoNestedInv = invertMatrix(infoNested)
	
	# Calculate Projection Matrix M_0: W - W * Delta_0 * (Delta_0^T * W * Delta_0)^-1 * Delta_0^T * W
	mNested = weightMat - weightMat %*% jacNested %*% infoNestedInv %*% t(jacNested) %*% weightMat

	# Calculate difference matrix: M_diff = M_0 - M_1
	mDiff = mNested - mBase

	# Degrees of freedom: d = k_1 - k_0 (difference in number of parameters)
	deltaDf = ncol(jacBase) - ncol(jacNested)

	# Calculate Satorra-Bentler scaling factor: c_d = tr(M_diff * V) / d
	# Uses the fast 1-liner trace helper sum(diag())
	traceVal = sum(diag(mDiff %*% asymCov))
	scalingFactor = traceVal / deltaDf

	# 6. Extract RAW unscaled discrepancy values
	getRawFit <- function(model) {
		rawFit = model$fitfunction$result
		if (is.matrix(rawFit) || is.array(rawFit)) {
			rawFit = rawFit[1, 1]
		}
		return(rawFit)
	}

	rawBase   = getRawFit(baseModel)
	rawNested = getRawFit(nestedModel)

	# Calculate Satorra-Bentler strict nested difference: T_d = (T_0 - T_1) / c_d
	deltaRaw      = rawNested - rawBase
	strictSbChisq = deltaRaw / scalingFactor

	# Calculate p-value (Ensure deltaDf is positive for the distribution check)
	pValue = pchisq(strictSbChisq, df = abs(deltaDf), lower.tail = FALSE)

	# Return results
	results = c(
		strictSbChisq = strictSbChisq,
		deltaDf       = deltaDf,
		scalingFactor = scalingFactor,
		pValue        = pValue
	)
	return(results)
}

#' xmuCalculateSRMR
#'
#' @description
#' `xmuCalculateSRMR` calculates the Standardized Root Mean Square Residual (SRMR) for an OpenMx model.
#'
#' @param model An evaluated OpenMx model.
#' @return The SRMR value as a numeric scalar, or NA if calculation fails.
#' @export
xmuCalculateSRMR <- function(model) {
	# Extract model-implied covariance matrix
	impCov = tryCatch({
		mxGetExpected(model, "covariance")
	}, error = function(e) {
		NULL
	})

	if (is.null(impCov)) {
		return(NA_real_)
	}

	manifests = colnames(impCov)
	if (is.null(manifests)) {
		return(NA_real_)
	}

	# Extract observed covariance matrix
	obsCov = NULL
	if (!is.null(model$data)) {
		if (!is.null(model$data$observedStats$cov)) {
			obsCov = model$data$observedStats$cov
		} else if (identical(model$data$type, "cov") ||
		           (is.matrix(model$data$observed) && !identical(model$data$type, "raw"))) {
			# cov data or modern type=none with matrix observed
			obsCov = model$data$observed
		} else if (identical(model$data$type, "raw")) {
			obsData = model$data$observed
			if (!is.null(obsData) && (is.data.frame(obsData) || is.matrix(obsData))) {
				validManifests = manifests[manifests %in% colnames(obsData)]
				if (length(validManifests) > 0) {
					obsCov = cov(obsData[, validManifests, drop = FALSE], use = "pairwise.complete.obs")
				}
			}
		}
	}

	if (is.null(obsCov)) {
		return(NA_real_)
	}

	# Ensure dimnames exist on observed covariance if it matches manifest dimensions
	if (is.null(rownames(obsCov)) && is.null(colnames(obsCov)) && nrow(obsCov) == length(manifests)) {
		rownames(obsCov) = manifests
		colnames(obsCov) = manifests
	}

	# Subset and align observed covariance to match implied covariance manifests
	commonManifests = manifests[manifests %in% rownames(obsCov)]
	if (length(commonManifests) == length(manifests)) {
		obsCov = obsCov[manifests, manifests, drop = FALSE]
	} else {
		return(NA_real_)
	}

	# Convert to correlation matrices
	obsCor = tryCatch({
		cov2cor(obsCov)
	}, error = function(e) {
		NULL
	})

	impCor = tryCatch({
		cov2cor(impCov)
	}, error = function(e) {
		NULL
	})

	if (is.null(obsCor) || is.null(impCor) || !identical(dim(obsCor), dim(impCor))) {
		return(NA_real_)
	}

	# Extract lower triangle including diagonal
	lowerTriangle = lower.tri(obsCor, diag = TRUE)
	rObs = obsCor[lowerTriangle]
	rImp = impCor[lowerTriangle]

	# Compute Standardized Root Mean Square Residual (SRMR)
	residuals = rObs - rImp
	srmrVal = sqrt(mean(residuals^2, na.rm = TRUE))

	if (is.na(srmrVal)) {
		return(NA_real_)
	}

	return(srmrVal)
}

#' xmu_pseudo_BIC
#'
#' @description
#' Calculate BIC with a custom sample size penalty.
#'
#' @param chisq A numeric vector of chi-square values.
#' @param k A numeric vector of parameter counts.
#' @param n The sample size or penalty benchmark.
#' @return A numeric vector of pseudo-BIC values.
#' @export
xmu_pseudo_BIC <- function(chisq, k, n) {
	# Calculate BIC with a custom sample size penalty
	# Formula: WLS_Chisq + k * ln(N)
	if (any(is.na(c(chisq, k, n)))) {
		return(rep(NA_real_, length(chisq)))
	}
	return(chisq + k * log(n))
}

#' @rdname xmu_pseudo_BIC
#' @export
xmuPseudoBic = xmu_pseudo_BIC

# Create the trivial independence model Jacobian in R
xmu_build_independence_jacobian <- function(rowNames, manifests) {
	P = length(rowNames)
	free_rows = logical(P)
	for (i in 1:P) {
		name = rowNames[i]
		isMean = grepl("^mean_", name) || grepl("^one_to_", name) || (name %in% manifests)
		isThresh = grepl("t[0-9]+$", name)
		isVar = FALSE
		if (!isMean && !isThresh) {
			parts = strsplit(name, "[ _]")[[1]]
			parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
			if (length(parts) == 2 && parts[1] == parts[2]) {
				isVar = TRUE
			} else if (length(parts) == 1) {
				isVar = TRUE
			}
		}
		if (isMean || isThresh || isVar) {
			free_rows[i] = TRUE
		}
	}
	
	numFree = sum(free_rows)
	jac = matrix(0, nrow = P, ncol = numFree)
	rownames(jac) = rowNames
	
	colIdx = 1
	for (i in 1:P) {
		if (free_rows[i]) {
			jac[i, colIdx] = 1
			colIdx = colIdx + 1
		}
	}
	return(jac)
}

#' Align WLS implied Jacobian to asymptotic-covariance moment order
#'
#' OpenMx may store \code{implied_jacobian} rows in a different order than
#' \code{observedStats$asymCov} (means after covariances in the Jacobian;
#' mixed order in \code{asymCov}). This helper reorders Jacobian rows to match
#' \code{asymCov} rownames and returns the intersection set used by all SB and
#' Savalei matrix algebra in [xmu_robust_WLS_fit()].
#'
#' @param jacMat Implied Jacobian \eqn{\Delta} from \code{model@output$implied_jacobian}.
#' @param asymCovMat Asymptotic covariance matrix \eqn{\Gamma} with moment rownames.
#' @param numCovsVal Number of covariance/polycorrelation moments (for mean/cov
#'   block reordering when Jacobian row count equals full moment count).
#' @return List with \code{jac} (aligned Jacobian) and \code{commonNames} (moment
#'   labels shared across \eqn{\Delta}, \eqn{\Gamma}, and \eqn{W}).
#' @family xmu internal not for end user
xmu_WLS_align_jacobian <- function(jacMat, asymCovMat, numCovsVal) {
	rowNames = rownames(asymCovMat)
	if (is.null(rowNames)) {
		stop("Asymptotic covariance matrix missing rownames.")
	}

	if (is.null(rownames(jacMat))) {
		if (nrow(jacMat) == length(rowNames)) {
			rownames(jacMat) = rowNames
		} else {
			covNames = rowNames[!grepl("^mean_", rowNames) & !grepl("^one_to_", rowNames) & !grepl("t[0-9]+$", rowNames)]
			if (nrow(jacMat) == length(covNames)) {
				rownames(jacMat) = covNames
			} else {
				rownames(jacMat) = rowNames[seq_len(nrow(jacMat))]
			}
		}
	}

	if (nrow(jacMat) == length(rowNames)) {
		numMeans = nrow(jacMat) - numCovsVal
		if (numMeans > 0 && nrow(jacMat) > numCovsVal) {
			meanIdx = (numCovsVal + 1):nrow(jacMat)
			covIdx = seq_len(numCovsVal)
			jacAligned = jacMat[c(meanIdx, covIdx), , drop = FALSE]
		} else {
			jacAligned = jacMat
		}
		rownames(jacAligned) = rowNames
		return(list(jac = jacAligned, commonNames = rowNames))
	}

	commonNames = rowNames[rowNames %in% rownames(jacMat)]
	if (length(commonNames) == 0) {
		stop("No common row names found between implied_jacobian and asymptotic covariance matrix.")
	}
	jacAligned = jacMat[commonNames, , drop = FALSE]
	return(list(jac = jacAligned, commonNames = commonNames))
}

#' Extract WLS useWeight (W) and asymCov (Gamma) from an MxModel
#'
#' **Modern only:** reads \code{observedStats = list(cov=, useWeight=, asymCov=)}.
#' OpenMx legacy \code{type="acov"} / \code{MxDataLegacyWLS} is refused (name trap).
#'
#' @param model An [OpenMx::mxModel()] with data.
#' @param stop_if_missing If TRUE, stop when either matrix is missing.
#' @return List with \code{useWeight}, \code{asymCov}, and optional \code{cov}.
#' @family xmu internal not for end user
xmu_wls_extract_WV <- function(model, stop_if_missing = TRUE) {
	useWeight = NULL
	asymCov   = NULL
	obsCov    = NULL
	if (is.null(model) || is.null(model$data)) {
		if (stop_if_missing) {
			stop("Model has no data; cannot extract WLS weight / asymptotic covariance matrices.")
		}
		return(list(useWeight = NULL, asymCov = NULL, cov = NULL))
	}
	data = model$data
	if (inherits(data, "MxDataLegacyWLS") || identical(tryCatch(data$type, error = function(e) NULL), "acov")) {
		if (exists("xmu_stop_legacy_acov", mode = "function")) {
			xmu_stop_legacy_acov("xmu_wls_extract_WV")
		}
		stop("xmu_wls_extract_WV: type='acov' / MxDataLegacyWLS is not supported. Use observedStats useWeight + asymCov.", call. = FALSE)
	}

	os = NULL
	if (isS4(data) && .hasSlot(data, "observedStats") && length(data@observedStats)) {
		os = data@observedStats
	} else if (!is.null(data$observedStats) && length(data$observedStats)) {
		os = data$observedStats
	}
	if (!is.null(os)) {
		if (!is.null(os$acov) || !is.null(os$fullWeight)) {
			stop("xmu_wls_extract_WV: observedStats$acov / $fullWeight are not supported (legacy name trap). Use useWeight and asymCov.", call. = FALSE)
		}
		if (!is.null(os$useWeight)) {
			useWeight = os$useWeight
		} else if (!is.null(os$weight) && is.matrix(os$weight)) {
			useWeight = os$weight
		}
		if (!is.null(os$asymCov)) {
			asymCov = os$asymCov
		}
		if (!is.null(os$cov)) {
			obsCov = os$cov
		}
	}

	if (is.null(obsCov) && !is.null(data$observed) && is.matrix(data$observed)) {
		obsCov = data$observed
	}

	if (stop_if_missing) {
		if (is.null(useWeight)) {
			stop("Could not locate observedStats$useWeight (W). Use mxData(numObs=N, observedStats=list(cov=S, useWeight=W, asymCov=V)).")
		}
		if (is.null(asymCov)) {
			stop("Could not locate observedStats$asymCov (Gamma). Use mxData(numObs=N, observedStats=list(cov=S, useWeight=W, asymCov=V)).")
		}
	}
	return(list(useWeight = useWeight, asymCov = asymCov, cov = obsCov))
}

#' Subset and align WLS weight matrix to common moment set
#'
#' Extracts the \eqn{W} block corresponding to \code{commonNames} from OpenMx
#' \code{useWeight} (or a diagonal vector). Accepts either a full matrix (with or
#' without dimnames) or a named diagonal vector (expanded to \code{diag()}).
#'
#' @param weightMat WLS weight matrix or diagonal vector from \code{MxData}.
#' @param commonNames Moment names shared by aligned \eqn{\Delta} and \eqn{\Gamma}.
#' @param fullRowNames Full moment names from the asymptotic covariance matrix.
#' @return Weight matrix \eqn{W} subset to \code{commonNames}.
#' @family xmu internal not for end user
xmu_WLS_align_weight <- function(weightMat, commonNames, fullRowNames) {
	if (is.vector(weightMat)) {
		if (is.null(names(weightMat))) {
			names(weightMat) = fullRowNames
		}
		weightVec = weightMat[commonNames]
		return(diag(weightVec))
	}
	if (is.matrix(weightMat)) {
		if (is.null(rownames(weightMat)) && nrow(weightMat) == length(fullRowNames)) {
			dimnames(weightMat) = list(fullRowNames, fullRowNames)
		}
		return(weightMat[commonNames, commonNames, drop = FALSE])
	}
	stop("Unsupported WLS weight matrix type.")
}

#' Detect ordinal or categorical WLS models
#'
#' Returns \code{TRUE} when the fitted model uses raw data and at least one
#' manifest variable is stored as \code{ordered} or \code{factor}. This gate
#' controls whether [xmu_robust_WLS_fit()] applies the Savalei (2021) branch
#' rather than the Satorra-Bentler (2010) branch used for continuous WLS.
#'
#' @param model A fitted [OpenMx::mxModel()].
#' @return Logical scalar.
#' @family xmu internal not for end user
xmu_is_ordinal_WLS <- function(model) {
	manifests = model@manifestVars
	if (is.null(manifests) || length(manifests) == 0) {
		return(FALSE)
	}
	observedData = NULL
	if (!is.null(model@data) && identical(model@data$type, "raw") && !is.null(model@data$observed)) {
		observedData = model@data$observed
	}
	if (is.null(observedData)) {
		return(FALSE)
	}
	any(vapply(manifests, function(manName) {
		col = observedData[[manName]]
		is.factor(col) || is.ordered(col)
	}, logical(1)))
}

#' Polycorrelation moment names from WLS asymptotic-covariance labels
#'
#' Filters OpenMx WLS moment rownames to the polycorrelation subsystem used in
#' Savalei (2021) corrections. Threshold rows (names ending in \code{t1},
#' \code{t2}, \ldots) are excluded; means and variances are excluded by the
#' naming pattern (they do not match the polycor regex filter).
#'
#' @param momentNames Character vector of \code{asymCov} rownames (aligned set).
#' @return Character vector of polycorrelation-only moment names.
#' @seealso [xmu_savalei_polycor_blocks()]
#' @family xmu internal not for end user
xmu_WLS_polycor_names <- function(momentNames) {
	momentNames[!grepl("t[0-9]+$", momentNames)]
}

#' Invert a matrix with chol, then ginv, then solve fallbacks
#' @param x Numeric matrix.
#' @return Matrix inverse.
#' @family xmu internal not for end user
xmu_invert_matrix <- function(x) {
	inv = tryCatch({
		chol2inv(chol(x))
	}, error = function(e) {
		tryCatch({
			MASS::ginv(x)
		}, error = function(e2) {
			solve(x)
		})
	})
	return(inv)
}

#' catML expected information matrix (\eqn{V}) for correlation moments
#'
#' Computes the \strong{catML} \code{wls.v} matrix used in Savalei (2021)
#' mean-and-variance corrections. This is \emph{not} the OpenMx WLS
#' \code{asymCov} block and \emph{not} the WLS fitting weight matrix.
#'
#' The matrix is the expected Fisher information for the
#' \eqn{p^*(p-1)/2} off-diagonal correlation parameters under a multivariate
#' normal correlation model, evaluated at the \strong{model-implied} correlation
#' matrix from the catML evaluation scaffold (correlation-matrix ML at fixed
#' WLS estimates). Algebraically,
#' \deqn{V = \tfrac{1}{2} D' (\Sigma^{-1} \otimes \Sigma^{-1}) D}
#' where \eqn{D} is the duplication map from unique correlation elements to
#' \eqn{\mathrm{vec}(\Sigma)} and \eqn{\Sigma} is the implied correlation matrix.
#'
#' @param impliedCor Numeric symmetric correlation matrix (\eqn{p \times p}).
#' @return Symmetric \eqn{p^* \times p^*} information matrix for polycorrelation
#'   moments, where \eqn{p^* = p(p-1)/2}.
#' @family xmu internal not for end user
xmu_catml_wls_v <- function(impliedCor) {
	impliedCor = as.matrix(impliedCor)
	pVal = nrow(impliedCor)
	if (pVal < 2) {
		return(matrix(0, 0, 0))
	}
	sigmaInv = tryCatch(solve(impliedCor), error = function(e) xmu_invert_matrix(impliedCor))
	pStar = pVal * (pVal - 1) / 2
	dupMat = matrix(0, pVal * pVal, pStar)
	colIdx = 1L
	for (i in seq_len(pVal - 1L)) {
		for (j in (i + 1L):pVal) {
			dupMat[(j - 1L) * pVal + i, colIdx] = 1
			dupMat[(i - 1L) * pVal + j, colIdx] = 1
			colIdx = colIdx + 1L
		}
	}
	kronMat = sigmaInv %x% sigmaInv
	0.5 * t(dupMat) %*% kronMat %*% dupMat
}

#' Model-implied correlation matrix at fixed WLS estimates
#'
#' Extracts \eqn{\Sigma} from the correlation-matrix ML evaluation model built
#' by [xmu_catml_eval_model()]. Parameters are fixed at the
#' converged ordinal WLS solution; no re-optimization is performed. This
#' implied matrix feeds [xmu_catml_wls_v()].
#'
#' @param model A fitted ordinal WLS [OpenMx::mxModel()].
#' @return Model-implied correlation matrix, or \code{NULL} on failure.
#' @seealso [xmu_catml_eval_model()], [xmu_catml_discrepancy_at_WLS()]
#' @family xmu internal not for end user
xmu_catml_implied_correlation <- function(model) {
	mML = xmu_catml_eval_model(model)
	if (is.null(mML)) {
		return(NULL)
	}
	impliedCor = tryCatch(mxGetExpected(mML, "covariance"), error = function(e) NULL)
	if (is.null(impliedCor) || !is.matrix(impliedCor) || nrow(impliedCor) < 2) {
		return(NULL)
	}
	impliedCor
}

#' Polycorrelation-block Jacobian rank degrees of freedom (diagnostic)
#'
#' Computes polycorrelation-subsystem rank deficits
#' \eqn{df_3 = p^* - \mathrm{rank}(\Delta_{poly})} and the analogous null rank.
#' **Not used for final Savalei reporting** in [xmu_robust_WLS_fit()], which
#' takes target \eqn{df_3} from the model test df and null \eqn{df_{3,null}}
#' from the polycorrelation moment count. Retained for diagnostics and future
#' extensions.
#'
#' @param jacPoly Polycorrelation rows of the implied Jacobian.
#' @param jacIndPoly Independence Jacobian polycorrelation rows.
#' @return List with \code{df3} and \code{df3Null}.
#' @family xmu internal not for end user
xmu_catml_df3_diagnostic <- function(jacPoly, jacIndPoly) {
	nPoly = nrow(jacPoly)
	if (nPoly == 0) {
		return(list(df3 = NA_real_, df3Null = NA_real_))
	}
	df3 = nPoly - qr(jacPoly)$rank
	df3Null = nPoly - qr(jacIndPoly)$rank
	list(df3 = df3, df3Null = df3Null)
}

#' Build OpenMx-native polycorrelation blocks for the Savalei sandwich
#'
#' Assembles the \eqn{\Delta}, \eqn{\Gamma}, and \eqn{W} ingredients for the
#' polycorrelation subsystem of Savalei (2021) \eqn{\hat{c}_3} (\code{c.hat3}),
#' using matrices extracted from the \strong{same} fitted OpenMx WLS model at
#' mutually consistent scales.
#'
#' @details
#' **Why a separate builder?** Savalei's correction is evaluated on
#' polycorrelations only (threshold rows excluded). Each matrix must refer to
#' the same moment ordering (rownames from aligned \code{asymCov}) and the same
#' sample size scaling.
#'
#' **Objects returned:**
#' \describe{
#'   \item{\code{jacPoly} (\eqn{\Delta_{poly}})}{
#'     Rows of \code{model@output$implied_jacobian} corresponding to
#'     polycorrelation moments (names not ending in \code{t1}, \code{t2}, etc.).
#'     Columns are model parameters in OpenMx order. This is
#'     \eqn{\partial s / \partial \theta} for the polycor block.
#'   }
#'   \item{\code{gammaPoly} (\eqn{\Gamma_{poly}})}{
#'     Asymptotic covariance of the sample polycorrelation moments at sample
#'     scale: \eqn{\Gamma_{poly} = n^2 \times V_{poly,per-obs}}, where
#'     \eqn{V_{poly,per-obs}} is the polycorrelation block of OpenMx
#'     \code{observedStats$asymCov} \emph{before} the global \eqn{n} scaling
#'     applied to the full aligned matrices. This is the moment sampling
#'     covariance; it is \strong{not} \code{solve(useWeight)}.
#'   }
#'   \item{\code{wPolyWiU} (\eqn{W_{poly}} in \eqn{W_i U})}{
#'     Polycorrelation block of the WLS weight matrix at sample scale:
#'     \eqn{W_{poly} = n \times W_{poly,per-obs}}, from \code{useWeight}. This
#'     is paired with \eqn{E^{-1} = (\Delta_{full}' W_{full} \Delta_{full})^{-1}}
#'     computed from the identically scaled full weight matrix.
#'   }
#' }
#'
#' @param asymCovAlignedPerObs Aligned per-observation asymptotic covariance
#'   (\code{asymCov} subset to common moments, before \eqn{n} scaling).
#' @param weightMatAlignedPerObs Aligned per-observation WLS weight (before
#'   \eqn{n} scaling).
#' @param jacTargetAligned Full aligned implied Jacobian (all moment rows).
#' @param polyNames Character vector of polycorrelation moment names.
#' @param nVal Sample size \eqn{n} (from \code{model@data@numObs}).
#' @return List with \code{gammaPoly}, \code{wPolyWiU}, and \code{jacPoly}, or
#'   \code{NULL} if any block is empty.
#' @seealso [xmu_savalei_scaling_factor()],
#'   [xmu_WLS_polycor_names()]
#' @family xmu internal not for end user
xmu_savalei_polycor_blocks <- function(asymCovAlignedPerObs, weightMatAlignedPerObs, jacTargetAligned, polyNames, nVal) {
	if (length(polyNames) == 0) {
		return(NULL)
	}
	gammaPoly = asymCovAlignedPerObs[polyNames, polyNames, drop = FALSE]
	wPolyPerObs = weightMatAlignedPerObs[polyNames, polyNames, drop = FALSE]
	jacPoly = jacTargetAligned[polyNames, , drop = FALSE]
	if (nrow(gammaPoly) == 0 || nrow(wPolyPerObs) == 0 || nrow(jacPoly) == 0) {
		return(NULL)
	}
	if (!is.null(nVal) && is.finite(nVal) && nVal > 0) {
		gammaPoly = (nVal * nVal) * gammaPoly
		wPolyWiU = nVal * wPolyPerObs
	} else {
		wPolyWiU = wPolyPerObs
	}
	list(gammaPoly = gammaPoly, wPolyWiU = wPolyWiU, jacPoly = jacPoly)
}

#' Savalei (2021) target scaling factor \eqn{\hat{c}_3} for ordinal WLS
#'
#' Computes the mean-and-variance correction used in robust CFI, TLI, and RMSEA
#' for ordinal/categorical WLS models (Savalei, 2021; Brosseau-Liard & Savalei,
#' 2012 noncentrality form).
#'
#' @details
#' **Formula.** Let \eqn{W_i U = I - \Delta_{poly} E^{-1} \Delta_{poly}' W_{poly}}.
#' Then
#' \deqn{\hat{c}_3 = \frac{\mathrm{tr}\big( W_i U' \, V \, W_i U \, \Gamma_{poly} \big)}{df_3}}
#' where:
#' \itemize{
#'   \item \eqn{V} = catML expected information ([xmu_catml_wls_v()])
#'   \item \eqn{\Gamma_{poly}} = sample moment covariance from OpenMx \code{asymCov}
#'   \item \eqn{W_{poly}} = WLS \code{useWeight} polycor block at sample scale
#'   \item \eqn{E^{-1}} = \eqn{(\Delta_{full}' W_{full} \Delta_{full})^{-1}} from the
#'         same fitted model and weight scaling
#'   \item \eqn{df_3} = target model test df (not a polycor Jacobian rank shortcut)
#' }
#'
#' **Design note.** \eqn{V} comes from the catML correlation scaffold; \eqn{\Gamma}
#' and \eqn{W} come from the ordinal WLS fit. They are different statistical
#' objects serving different roles in Savalei's correction. Do not substitute
#' \code{asymCov} for \eqn{V}, or \code{solve(useWeight)} for \eqn{\Gamma}.
#'
#' @param jacFull Full aligned WLS Jacobian \eqn{\Delta_{full}} (all moments).
#' @param jacPoly Polycorrelation rows \eqn{\Delta_{poly}}.
#' @param vCatMlPoly catML \eqn{V} matrix (polycor block).
#' @param gammaPoly Sample moment covariance \eqn{\Gamma_{poly}}.
#' @param wPolyWiU WLS weight polycor block \eqn{W_{poly}} for the \eqn{W_i U} sandwich.
#' @param wFullScaled Full aligned weight matrix at sample scale (\eqn{n \times} per-obs).
#' @param dfCatMl Target model test degrees of freedom \eqn{df_3}.
#' @return Numeric \eqn{\hat{c}_3}, or \code{NA_real_} if computation fails.
#' @references Savalei, V. (2021). Improving fit indices in SEM with categorical
#'   data. *Multivariate Behavioral Research*, **56**(3), 390--407.
#' @family xmu internal not for end user
xmu_savalei_scaling_factor <- function(jacFull, jacPoly, vCatMlPoly, gammaPoly, wPolyWiU, wFullScaled, dfCatMl) {
	if (is.null(dfCatMl) || is.na(dfCatMl) || dfCatMl <= 0) {
		return(NA_real_)
	}
	if (nrow(jacPoly) == 0 || nrow(gammaPoly) == 0 || nrow(vCatMlPoly) == 0 || nrow(wPolyWiU) == 0) {
		return(NA_real_)
	}
	infoInv = xmu_invert_matrix(t(jacFull) %*% wFullScaled %*% jacFull)
	wiU = diag(nrow(wPolyWiU)) - jacPoly %*% infoInv %*% t(jacPoly) %*% wPolyWiU
	ksGamma = sum(diag(t(wiU) %*% vCatMlPoly %*% wiU %*% gammaPoly))
	cHatGamma = ksGamma / dfCatMl
	if (is.finite(cHatGamma) && cHatGamma > 0) {
		return(cHatGamma)
	}
	return(NA_real_)
}

#' Savalei (2021) independence/null scaling factor \eqn{\hat{c}_{3,null}}
#'
#' Computes the null-model mean-and-variance correction used alongside
#' \eqn{\hat{c}_3} in robust CFI and TLI:
#' \deqn{\hat{c}_{3,null} = \frac{\mathrm{tr}(\Gamma_{poly})}{df_{3,null}}}
#' where \eqn{df_{3,null}} is the number of polycorrelation moments (all
#' off-diagonal correlations free under the independence baseline in the
#' correlation subsystem).
#'
#' @param gammaPoly Polycorrelation sample-moment covariance \eqn{\Gamma_{poly}}.
#' @param dfNullCatMl Null degrees of freedom \eqn{df_{3,null}} (typically
#'   \code{length(polyNames)}).
#' @return Numeric \eqn{\hat{c}_{3,null}}, or \code{NA_real_} on failure.
#' @family xmu internal not for end user
xmu_savalei_null_scaling_factor <- function(gammaPoly, dfNullCatMl) {
	if (is.null(dfNullCatMl) || is.na(dfNullCatMl) || dfNullCatMl <= 0) {
		return(NA_real_)
	}
	if (is.null(gammaPoly) || nrow(gammaPoly) == 0) {
		return(NA_real_)
	}
	return(sum(diag(gammaPoly)) / dfNullCatMl)
}

#' Robust CFI from mean-and-variance corrected noncentrality (Savalei 2021 form)
#'
#' Implements the Brosseau-Liard / Savalei noncentrality formulation for robust
#' CFI using catML discrepancy \eqn{XX_3} at fixed WLS estimates and scaling
#' factors \eqn{\hat{c}_3}, \eqn{\hat{c}_{3,null}}. Used for ordinal WLS in
#' [xmu_robust_WLS_fit()]; continuous WLS falls back to SB-scaled \eqn{\chi^2}.
#'
#' @param x2 Target catML discrepancy (or SB-scaled \eqn{\chi^2} for continuous).
#' @param df Target model degrees of freedom.
#' @param x2Null Independence/null catML discrepancy.
#' @param dfNull Null model degrees of freedom.
#' @param cHat Target scaling factor \eqn{\hat{c}_3}.
#' @param cHatNull Null scaling factor \eqn{\hat{c}_{3,null}}.
#' @return Robust CFI in \eqn{[0, 1]} (values slightly outside may occur for TLI).
#' @family xmu internal not for end user
xmu_savalei_fit_cfi <- function(x2, df, x2Null, dfNull, cHat = 1, cHatNull = 1) {
	if (anyNA(c(x2, df, x2Null, dfNull, cHat, cHatNull))) {
		return(NA_real_)
	}
	if (df > 0 && cHat != 1 && cHatNull != 1) {
		t1 = max(c(x2 - (cHat * df), 0))
		t2 = max(c(x2 - (cHat * df), x2Null - (cHatNull * dfNull), 0))
	} else {
		t1 = max(c(x2 - df, 0))
		t2 = max(c(x2 - df, x2Null - dfNull, 0))
	}
	if (isTRUE(all.equal(t1, 0)) && isTRUE(all.equal(t2, 0))) {
		return(1)
	}
	return(1 - t1 / t2)
}

#' Robust TLI from mean-and-variance corrected noncentrality (Savalei 2021 form)
#'
#' Companion to [xmu_savalei_fit_cfi()]. TLI is not bounded above by 1 under
#' this correction; values slightly above 1 can occur with excellent fit.
#'
#' @inheritParams xmu_savalei_fit_cfi
#' @return Robust TLI.
#' @family xmu internal not for end user
xmu_savalei_fit_tli <- function(x2, df, x2Null, dfNull, cHat = 1, cHatNull = 1) {
	if (anyNA(c(x2, df, x2Null, dfNull, cHat, cHatNull))) {
		return(NA_real_)
	}
	if (df > 0 && cHat != 1 && cHatNull != 1) {
		t1 = (x2 - cHat * df) * dfNull
		t2 = (x2Null - cHatNull * dfNull) * df
	} else {
		t1 = (x2 - df) * dfNull
		t2 = (x2Null - dfNull) * df
	}
	if (df > 0 && abs(t2) > 0) {
		return(1 - t1 / t2)
	}
	if (!is.finite(t1) || !is.finite(t2)) {
		return(NA_real_)
	}
	return(1)
}

#' Robust RMSEA from mean-and-variance corrected noncentrality (Savalei 2021 form)
#'
#' \deqn{\mathrm{RMSEA}_{robust} = \sqrt{\max\left(\frac{XX_3/n}{df_3} - \frac{\hat{c}_3}{n}, 0\right)}}
#' Uses catML \eqn{XX_3} and \eqn{\hat{c}_3} for ordinal WLS; continuous WLS
#' uses SB-scaled target \eqn{\chi^2} and SB trace scaling.
#'
#' @param x2 Target discrepancy (\eqn{XX_3} or SB-scaled \eqn{\chi^2}).
#' @param df Target degrees of freedom.
#' @param nVal Sample size \eqn{n}.
#' @param cHat Scaling factor \eqn{\hat{c}_3} (or SB \eqn{c} for continuous).
#' @param gGroups Number of groups (default 1).
#' @return Robust RMSEA (0 if \eqn{df = 0}).
#' @family xmu internal not for end user
xmu_savalei_fit_rmsea <- function(x2, df, nVal, cHat = 1, gGroups = 1L) {
	if (length(x2) == 0 || is.null(nVal) || is.na(nVal) || nVal <= 0) {
		return(NA_real_)
	}
	if (df > 0) {
		return(sqrt(pmax((x2 / nVal) / df - cHat / nVal, 0)) * sqrt(gGroups))
	}
	return(0)
}

#' Build correlation-matrix ML evaluation model at fixed WLS estimates
#'
#' Constructs a correlation-matrix ML [OpenMx::mxModel()] with all parameters
#' fixed at the converged ordinal WLS solution. This is the umx/OpenMx analogue
#' of a \strong{catML evaluation scaffold}: a correlation-structure ML model
#' used to evaluate discrepancy at fixed estimates, not to re-fit ordinal data
#' under ML.
#'
#' @details
#' The model uses \code{mxData(type = "cor")} with the observed polychoric
#' correlation matrix from \code{observedStats$cov}, and RAM paths with
#' \code{free = FALSE} for loadings, latent variances, and residual variances
#' taken from the WLS solution. Thresholds and means are not part of this
#' correlation scaffold; they enter the WLS fit separately.
#'
#' @param model A fitted ordinal WLS [OpenMx::mxModel()].
#' @return Fitted correlation ML model (silent run), or \code{NULL} on failure.
#' @seealso [xmu_catml_discrepancy_at_WLS()], [xmu_catml_implied_correlation()]
#' @family xmu internal not for end user
xmu_catml_eval_model <- function(model) {
	tryCatch({
		obsCor = NULL
		numObs = NULL
		if (inherits(model@data, "MxData") && .hasSlot(model@data, "observedStats") && !is.null(model@data@observedStats$cov)) {
			obsCor = model@data@observedStats$cov
			numObs = model@data@numObs
		} else if (!is.null(model$data$observedStats$cov)) {
			obsCor = model$data$observedStats$cov
			numObs = model$data$numObs
		}
		if (is.null(obsCor) || is.null(numObs) || !is.finite(numObs) || numObs <= 0) {
			return(NULL)
		}
		manifests = model@manifestVars
		if (is.null(manifests) || length(manifests) == 0) {
			return(NULL)
		}
		obsCor = obsCor[manifests, manifests, drop = FALSE]
		latentVars = model@latentVars
		if (is.null(latentVars)) {
			latentVars = character(0)
		}
		pathList = list()
		if (!is.null(model$A) && !is.null(model$S)) {
			aMat = model$A
			sMat = model$S
			aRowNames = if (length(aMat$rownames) > 0) aMat$rownames else rownames(aMat$values)
			aColNames = if (length(aMat$colnames) > 0) aMat$colnames else colnames(aMat$values)
			sRowNames = if (length(sMat$rownames) > 0) sMat$rownames else rownames(sMat$values)
			if (length(latentVars) > 0 && !is.null(aRowNames) && !is.null(aColNames)) {
				for (lv in latentVars) {
					rowIdx = match(manifests, aRowNames)
					colIdx = match(lv, aColNames)
					if (!any(is.na(rowIdx)) && !is.na(colIdx)) {
						loadVals = as.numeric(aMat$values[rowIdx, colIdx])
						if (length(loadVals) > 0 && all(is.finite(loadVals))) {
							pathList = append(pathList, list(mxPath(from = lv, to = manifests, arrows = 1, free = FALSE, values = loadVals)))
						}
					}
					sRowIdx = match(lv, sRowNames)
					if (!is.na(sRowIdx) && isTRUE(sMat$free[sRowIdx, sRowIdx])) {
						lvVarVal = sMat$values[sRowIdx, sRowIdx]
						if (is.finite(lvVarVal) && lvVarVal > 0) {
							pathList = append(pathList, list(mxPath(from = lv, arrows = 2, free = FALSE, values = lvVarVal)))
						}
					}
				}
			}
			if (!is.null(sRowNames)) {
				manIdx = match(manifests, sRowNames)
				if (!any(is.na(manIdx))) {
					residVals = diag(sMat$values[manIdx, manIdx, drop = FALSE])
					if (length(residVals) > 0 && all(is.finite(residVals))) {
						pathList = append(pathList, list(mxPath(from = manifests, arrows = 2, free = FALSE, values = residVals)))
					}
				}
			}
		}
		if (length(pathList) == 0) {
			pars = omxGetParameters(model)
			if (is.null(pars) || length(pars) == 0) {
				return(NULL)
			}
			if (length(latentVars) > 0) {
				for (lv in latentVars) {
					loadPat = paste0("^", lv, "_to_")
					loadNames = grep(loadPat, names(pars), value = TRUE)
					if (length(loadNames) > 0) {
						loadVals = as.numeric(pars[loadNames])
						loadTo = sub(paste0("^", lv, "_to_"), "", loadNames)
						pathList = append(pathList, list(mxPath(from = lv, to = loadTo, arrows = 1, free = FALSE, values = loadVals)))
					}
					lvVarName = paste0(lv, "_with_", lv)
					if (lvVarName %in% names(pars)) {
						pathList = append(pathList, list(mxPath(from = lv, arrows = 2, free = FALSE, values = as.numeric(pars[lvVarName]))))
					}
				}
			}
			residNames = intersect(paste0(manifests, "_with_", manifests), names(pars))
			if (length(residNames) > 0) {
				residVals = as.numeric(pars[residNames])
				pathList = append(pathList, list(mxPath(from = manifests, arrows = 2, free = FALSE, values = residVals)))
			}
		}
		if (length(pathList) == 0) {
			return(NULL)
		}
		mCor = do.call(mxModel, c(
			list(
				paste0(model$name, "_catML_eval"),
				type = "RAM",
				manifestVars = manifests,
				latentVars = latentVars,
				mxData(observed = obsCor, type = "cor", numObs = numObs),
				mxFitFunctionML()
			),
			pathList
		))
		mxRun(mCor, silent = TRUE)
	}, error = function(e) NULL)
}

#' Evaluate catML discrepancy (\eqn{XX_3}) at fixed WLS estimates
#'
#' Plugs converged ordinal WLS parameters into the correlation-matrix ML
#' scaffold from [xmu_catml_eval_model()] and computes
#' likelihood-ratio components relative to saturated and independence
#' correlation reference models via [OpenMx::mxRefModels()].
#'
#' @details
#' **Output components:**
#' \describe{
#'   \item{\code{fMlTarget}}{Target catML discrepancy \eqn{XX_3} (target minus
#'     saturated correlation fit). This is the misfit measure entering robust
#'     CFI, TLI, and RMSEA for ordinal WLS.}
#'   \item{\code{fMlNull}}{Independence catML discrepancy \eqn{XX_{3,null}}
#'     (independence minus saturated). Used in robust CFI/TLI denominators.}
#' }
#' Degrees of freedom for indices are taken from the WLS model test df in
#' [xmu_robust_WLS_fit()], not from this function.
#'
#' @param model A fitted ordinal WLS [OpenMx::mxModel()].
#' @return List with \code{fMlTarget}, \code{fMlNull}, \code{dfCatMl},
#'   \code{dfNull}, or \code{NULL} on failure.
#' @seealso [xmu_catml_eval_model()],
#'   [xmu_catml_wls_v()]
#' @family xmu internal not for end user
xmu_catml_discrepancy_at_WLS <- function(model) {
	tryCatch({
		mML = xmu_catml_eval_model(model)
		if (is.null(mML) || is.null(mML@output$fit) || !is.finite(mML@output$fit)) {
			return(NULL)
		}
		refs = tryCatch(mxRefModels(mML, run = TRUE), error = function(e) NULL)
		if (is.null(refs)) {
			return(NULL)
		}
		satName = grep("Saturated", names(refs), value = TRUE)
		if (length(satName) == 0) {
			return(NULL)
		}
		mSat = refs[[satName[1]]]
		if (is.null(mSat@output$fit) || !is.finite(mSat@output$fit)) {
			return(NULL)
		}
		fMlTarget = mML@output$fit - mSat@output$fit
		if (!is.finite(fMlTarget) || fMlTarget < 0) {
			fMlTarget = 0
		}
		fMlNull = NA_real_
		dfNull = NA_real_
		indName = grep("Independence", names(refs), value = TRUE)
		if (length(indName) > 0) {
			mInd = refs[[indName[1]]]
			if (!is.null(mInd@output$fit) && is.finite(mInd@output$fit)) {
				fMlNull = mInd@output$fit - mSat@output$fit
				if (!is.finite(fMlNull) || fMlNull < 0) {
					fMlNull = 0
				}
			}
		}
		list(
			fMlTarget = fMlTarget,
			fMlNull = fMlNull,
			dfCatMl = NA_real_,
			dfNull = dfNull
		)
	}, error = function(e) NULL)
}

#' Robust WLS fit statistics: Satorra-Bentler (2010) and Savalei (2021)
#'
#' @description
#' Computes robust fit indices and scaled test statistics for Weighted Least
#' Squares (WLS / DWLS) models fitted in OpenMx. This is the engine behind
#' WLS routing in [umxSummary()] and [umxCompare()].
#'
#' The function **intentionally uses two statistic families**:
#' \itemize{
#'   \item **Display omnibus test** (\code{Chi}, \code{ChiDoF}, \code{p}): always
#'         Satorra-Bentler (2010) scaled WLS \eqn{\chi^2}, regardless of whether
#'         manifests are continuous or ordinal.
#'   \item **Robust incremental/absolute indices** (\code{CFI}, \code{TLI},
#'         \code{RMSEA}): Satorra-Bentler (2010) for continuous WLS; Savalei (2021)
#'         catML mean-and-variance corrections for ordinal/categorical WLS.
#' }
#'
#' Independence (null) baseline fit is computed **natively in R** (not via
#' \code{mxRun}) so that asymptotic-covariance-only inputs (e.g. Genomic SEM /
#' summary-statistic pipelines) do not trigger OpenMx validation errors.
#'
#' @details
#' **Prerequisites.** The fitted model must expose
#' \code{model@output$implied_jacobian}, plus WLS \code{asymCov} and
#' \code{useWeight} (or equivalent) with consistent moment rownames. Without
#' the Jacobian, the function stops with an error.
#'
#' **Shared pipeline (both branches).**
#' \enumerate{
#'   \item Extract raw target WLS \eqn{\chi^2_{raw}}, test df, and implied Jacobian
#'         \eqn{\Delta_{target}}.
#'   \item Align \eqn{\Delta}, \eqn{\Gamma} (\code{asymCov}), and \eqn{W}
#'         (\code{useWeight}) to a common moment ordering via
#'         [xmu_WLS_align_jacobian()] and
#'         [xmu_WLS_align_weight()].
#'   \item Build independence Jacobian \eqn{\Delta_{ind}} with
#'         \code{xmu_build_independence_jacobian()} on the same moment set.
#'   \item Compute native independence discrepancy
#'         \eqn{\chi^2_{ind,raw} = d_{ind}' W d_{ind}}, where \eqn{d_{ind}} is
#'         the observed summary vector with means, variances, and thresholds zeroed
#'         (independence pattern).
#'   \item Scale matrices to sample size \eqn{n} for raw-data models.
#'   \item Compute Satorra-Bentler trace scaling factors
#'         \deqn{U = \Gamma W - \Gamma W \Delta (\Delta' W \Delta)^{-1} \Delta' W}
#'         \deqn{c = \mathrm{tr}(U) / df}
#'         for target (\eqn{c_{model}}) and independence (\eqn{c_{null}}) models.
#'   \item Form display statistics:
#'         \eqn{\chi^2_{SB} = \chi^2_{raw} / c_{model}}, with
#'         \eqn{p = 1 - F_{\chi^2_{df}}(\chi^2_{SB})}.
#' }
#'
#' **Branch A — Continuous WLS** (\code{correction = "SB2010"}).
#' Detected when no manifest is \code{ordered} or \code{factor} on raw data
#' ([xmu_is_ordinal_WLS()] returns \code{FALSE}). Robust
#' \code{CFI}, \code{TLI}, and \code{RMSEA} are derived from SB-scaled target and
#' independence \eqn{\chi^2} and their dfs. Attributes \code{c_model} and
#' \code{c_null} hold the SB trace factors.
#'
#' **Branch B — Ordinal / categorical WLS** (\code{correction = "Savalei2021"}).
#' Detected when at least one manifest is \code{ordered} or \code{factor}.
#' Robust indices use the Brosseau-Liard / Savalei noncentrality form with catML
#' ingredients evaluated at **fixed** converged WLS estimates (no re-optimization):
#'
#' \strong{catML discrepancy \eqn{XX_3}.}
#' [xmu_catml_discrepancy_at_WLS()] builds a correlation-matrix ML scaffold
#' ([xmu_catml_eval_model()]), runs reference models, and
#' returns:
#' \itemize{
#'   \item \code{fMlTarget} = \eqn{XX_3} (target minus saturated correlation fit)
#'   \item \code{fMlNull} = \eqn{XX_{3,null}} (independence minus saturated)
#' }
#'
#' \strong{Savalei scaling \eqn{\hat{c}_3}.}
#' [xmu_savalei_polycor_blocks()] assembles OpenMx-native polycor blocks:
#' \describe{
#'   \item{\eqn{V}}{catML expected information at model-implied correlation
#'         ([xmu_catml_wls_v()]); not \code{asymCov} and not
#'         \code{solve(useWeight)}.}
#'   \item{\eqn{\Gamma_{poly}}}{Sample moment covariance:
#'         \eqn{n^2 \times} polycor block of per-observation \code{asymCov}.}
#'   \item{\eqn{W_{poly}}}{WLS weight polycor block at sample scale:
#'         \eqn{n \times} per-observation \code{useWeight}.}
#'   \item{\eqn{\Delta_{poly}}}{Polycor rows of aligned \code{implied_jacobian}
#'         (threshold rows excluded).}
#'   \item{\eqn{E^{-1}}}{\eqn{(\Delta_{full}' W_{full} \Delta_{full})^{-1}} from the
#'         identically scaled full weight matrix.}
#' }
#' Then [xmu_savalei_scaling_factor()] computes
#' \deqn{\hat{c}_3 = \frac{\mathrm{tr}(W_i U' V W_i U \, \Gamma_{poly})}{df_3}}
#' with \eqn{W_i U = I - \Delta_{poly} E^{-1} \Delta_{poly}' W_{poly}}, and
#' [xmu_savalei_null_scaling_factor()] computes
#' \eqn{\hat{c}_{3,null} = \mathrm{tr}(\Gamma_{poly}) / df_{3,null}}.
#'
#' Degrees of freedom: \eqn{df_3} = WLS model test df; \eqn{df_{3,null}} =
#' number of polycorrelation moments (\code{length(polyNames)}). Jacobian-rank
#' shortcuts ([xmu_catml_df3_diagnostic()]) are diagnostic only.
#'
#' Robust indices: [xmu_savalei_fit_cfi()], [xmu_savalei_fit_tli()],
#' [xmu_savalei_fit_rmsea()] with \eqn{XX_3}, \eqn{\hat{c}_3}, \eqn{\hat{c}_{3,null}}.
#' CFI is clamped to \eqn{[0, 1]}; TLI may slightly exceed 1 with excellent fit.
#' If Savalei scaling fails, \eqn{\hat{c}_3} falls back to SB \eqn{c_{model}} (and
#' null analogously).
#'
#' **Cutoff guidance.** For ordinal WLS, Hu & Bentler (1999) conventional cutoffs
#' (e.g. CFI \eqn{\geq} 0.95, RMSEA \eqn{\leq} 0.06) apply to the **robust**
#' \code{CFI}/\code{TLI}/\code{RMSEA} reported here—not to raw WLS \eqn{\chi^2}.
#' Display \eqn{\chi^2} and \eqn{p} remain SB-scaled WLS omnibus tests.
#'
#' **Software note.** Numeric values of \eqn{\hat{c}_3} may differ from other SEM
#' packages because Jacobians and parameterizations differ across engines; umx
#' uses OpenMx \code{implied_jacobian} and native \code{asymCov}/\code{useWeight}
#' throughout. The goal is statistically correct evaluation under OpenMx, not
#' bitwise replication of external implementations.
#'
#' @param model A fitted WLS [OpenMx::mxModel()] with \code{implied_jacobian}.
#'
#' @return A named list designed to patch directly into [umxSummary()] slots:
#' \describe{
#'   \item{\code{CFI}, \code{TLI}, \code{RMSEA}}{Robust fit indices (SB2010 or
#'         Savalei2021 branch).}
#'   \item{\code{Chi}}{SB-scaled target \eqn{\chi^2} (display omnibus statistic).}
#'   \item{\code{ChiDoF}}{Target model test degrees of freedom.}
#'   \item{\code{p}}{Two-sided \eqn{p}-value for \code{Chi} under \eqn{\chi^2_{df}}.}
#'   \item{\code{scalingFactor}, \code{scalingFactorNull}}{SB trace factors
#'         \eqn{c_{model}} and \eqn{c_{null}} (always computed).}
#' }
#' **Attributes** (read by [umxSummary()] for footnotes):
#' \describe{
#'   \item{\code{correction}}{\code{"SB2010"} or \code{"Savalei2021"}.}
#'   \item{\code{c_model}}{Scaling factor used for robust target noncentrality
#'         (SB \eqn{c} or Savalei \eqn{\hat{c}_3}).}
#'   \item{\code{c_null}}{Scaling factor for null/independence noncentrality.}
#'   \item{\code{fMlTarget}, \code{fMlNull}}{catML \eqn{XX_3} components when
#'         Savalei branch runs; \code{NA_real_} otherwise.}
#' }
#'
#' @seealso [umxSummary()], [umxCompare()], \code{xmu_build_independence_jacobian},
#'   [xmu_is_ordinal_WLS()], [xmu_catml_discrepancy_at_WLS()],
#'   [xmu_savalei_polycor_blocks()]
#' @family Model Summary and Comparison
#' @references
#' - Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled
#'   difference chi-square test statistic. *Psychometrika*, **75**(2), 243--269.
#' - Savalei, V. (2021). Improving fit indices in SEM with categorical data.
#'   *Multivariate Behavioral Research*, **56**(3), 390--407.
#' - Brosseau-Liard, P. E., & Savalei, V. (2012). Adjusting incremental fit
#'   indices for nonnormality. *Multivariate Behavioral Research*, **47**(5), 647--677.
#' - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance
#'   structure analysis. *Structural Equation Modeling*, **6**, 1--55.
#' @export

xmu_robust_WLS_fit <- function(model) {
	# Step A: Extract raw Chi-Square, df, and Jacobian
	chisqTargetRaw = model$output$fit
	if (is.null(chisqTargetRaw) || is.na(chisqTargetRaw)) {
		chisqTargetRaw = model$fitfunction$result[1, 1]
	}
	
	dfTarget = xmu_extract_df(model)
	
	jacTarget = model@output$implied_jacobian
	if (is.null(jacTarget)) {
		stop("Target model missing implied_jacobian.")
	}
	
	# Step B: Extract W and asymCov (modern observedStats first; legacy name-swap safe)
	wv = xmu_wls_extract_WV(model, stop_if_missing = TRUE)
	weightMat = wv$useWeight
	asymCov   = wv$asymCov
	
	
	rowNames = rownames(asymCov)
	if (is.null(rowNames)) {
		stop("Asymptotic covariance matrix missing rownames.")
	}

	# Calculate K (number of observed variables)
	manifests = model@manifestVars
	kVal = length(manifests)
	if (kVal == 0) {
		eVal = nrow(asymCov)
		kVal = round((-1 + sqrt(1 + 8 * eVal)) / 2)
	}
	numCovs = (kVal * (kVal + 1)) / 2

	targetAlign = xmu_WLS_align_jacobian(jacTarget, asymCov, numCovs)
	jacTargetAligned = targetAlign$jac
	commonNames = targetAlign$commonNames

	# Step C: Independence Jacobian on full moments, then subset to aligned set
	jacInd = xmu_build_independence_jacobian(rowNames, manifests)
	jacIndAligned = jacInd[commonNames, , drop = FALSE]

	asymCovAligned = asymCov[commonNames, commonNames, drop = FALSE]
	weightMatAligned = xmu_WLS_align_weight(weightMat, commonNames, rowNames)

	# Step D: Independence df on the aligned moment set
	dfInd = length(commonNames) - ncol(jacIndAligned)

	# Step E/F: Native R baseline WLS fit calculation (aligned moments)
	# Extract observed covariance matrix and means (if any)
	obsCov = NULL
	obsMeans = NULL
	obsThresholds = NULL
	if (model$data$type == "raw") {
		if (.hasSlot(model$data, "observedStats") && !is.null(model$data@observedStats)) {
			obsCov = model$data@observedStats$cov
			obsMeans = model$data@observedStats$means
			obsThresholds = model$data@observedStats$thresholds
		} else if (!is.null(model$data$observedStats)) {
			obsCov = model$data$observedStats$cov
			obsMeans = model$data$observedStats$means
			obsThresholds = model$data$observedStats$thresholds
		}
		if (is.null(obsCov)) {
			numericData = model$data$observed[, sapply(model$data$observed, is.numeric), drop = FALSE]
			if (ncol(numericData) > 0) {
				obsCov = cov(numericData, use = "pairwise.complete.obs")
			} else {
				obsCov = matrix(NA_real_, nrow = length(manifests), ncol = length(manifests), dimnames = list(manifests, manifests))
			}
		}
		if (is.null(obsMeans)) {
			numericData = model$data$observed[, sapply(model$data$observed, is.numeric), drop = FALSE]
			if (ncol(numericData) > 0) {
				obsMeans = colMeans(numericData, na.rm = TRUE)
			} else {
				obsMeans = rep(0, length(manifests))
				names(obsMeans) = manifests
			}
		}
	} else {
		obsCov = model$data$observed
		obsMeans = model$data$means
	}
	
	if (!is.null(obsMeans)) {
		if (is.matrix(obsMeans) || is.array(obsMeans)) {
			colNames = colnames(obsMeans)
			obsMeans = as.vector(obsMeans)
			names(obsMeans) = colNames
		}
		obsMeans = obsMeans[manifests]
	}
	obsCov = obsCov[manifests, manifests, drop = FALSE]
	
	sVec = rep(0, length(commonNames))
	for (i in seq_along(commonNames)) {
		name = commonNames[i]
		if (grepl("^mean_", name)) {
			varName = sub("^mean_", "", name)
			sVec[i] = obsMeans[varName]
		} else if (grepl("^one_to_", name)) {
			varName = sub("^one_to_", "", name)
			sVec[i] = obsMeans[varName]
		} else if (name %in% manifests) {
			sVec[i] = obsMeans[name]
		} else if (grepl("t[0-9]+$", name)) {
			varName = sub("t[0-9]+$", "", name)
			threshNum = as.numeric(sub("^.*t", "", name))
			if (!is.null(obsThresholds) && varName %in% colnames(obsThresholds)) {
				sVec[i] = obsThresholds[threshNum, varName]
			} else {
				sVec[i] = NA_real_
			}
		} else {
			parts = strsplit(name, "[ _]")[[1]]
			parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
			if (length(parts) == 2) {
				sVec[i] = obsCov[parts[1], parts[2]]
			} else if (length(parts) == 1) {
				sVec[i] = obsCov[parts[1], parts[1]]
			}
		}
	}
	
	dInd = sVec
	for (i in seq_along(commonNames)) {
		name = commonNames[i]
		isMean = grepl("^mean_", name) || grepl("^one_to_", name) || (name %in% manifests)
		isThresh = grepl("t[0-9]+$", name)
		isVar = FALSE
		if (!isMean && !isThresh) {
			parts = strsplit(name, "[ _]")[[1]]
			parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
			if (length(parts) == 2 && parts[1] == parts[2]) {
				isVar = TRUE
			} else if (length(parts) == 1) {
				isVar = TRUE
			}
		}
		if (isMean || isVar || isThresh) {
			dInd[i] = 0
		}
	}
	
	chisqIndRaw = as.numeric(t(dInd) %*% weightMatAligned %*% dInd)

	polyNames = xmu_WLS_polycor_names(commonNames)
	asymCovAlignedPerObs = asymCovAligned
	weightMatAlignedPerObs = weightMatAligned

	# Scale WLS matrices to sample size for raw-data models
	nVal = model$data$numObs
	if (is.null(nVal) || is.na(nVal)) {
		nVal = 1000
	}

	if (identical(model$data$type, "raw")) {
		asymCovAligned = nVal * asymCovAligned
		weightMatAligned = nVal * weightMatAligned
	}

	# Trace helper for single model scaling factor
	getScalingFactor <- function(jacMat, asymCovMat, weightMatVal, dfVal) {
		if (is.null(dfVal) || is.na(dfVal) || dfVal <= 0) {
			return(NA_real_)
		}
		info     = t(jacMat) %*% weightMatVal %*% jacMat
		infoInv  = xmu_invert_matrix(info)
		vw       = asymCovMat %*% weightMatVal
		vwDelta  = vw %*% jacMat
		uMat     = vw - vwDelta %*% infoInv %*% t(jacMat) %*% weightMatVal
		traceVal = sum(diag(uMat))
		scalingFactor = traceVal / dfVal
		return(scalingFactor)
	}

	cTarget = getScalingFactor(jacTargetAligned, asymCovAligned, weightMatAligned, dfTarget)
	cInd    = getScalingFactor(jacIndAligned, asymCovAligned, weightMatAligned, dfInd)
	
	chisqTargetScaled = chisqTargetRaw / cTarget
	chisqIndScaled    = chisqIndRaw / cInd

	cfiRobust = NA_real_
	tliRobust = NA_real_
	rmseaRobust = NA_real_
	correctionMethod = "SB2010"
	fMlTarget = NA_real_
	fMlNull = NA_real_
	cHatSavalei = NA_real_
	cHatNullSavalei = NA_real_

	if (xmu_is_ordinal_WLS(model)) {
		catMl = xmu_catml_discrepancy_at_WLS(model)
		if (!is.null(catMl) && is.finite(catMl$fMlTarget)) {
			correctionMethod = "Savalei2021"
			fMlTarget = catMl$fMlTarget
			fMlNull = catMl$fMlNull
			dfCatMl = dfTarget
			dfNullCatMl = length(polyNames)
			dfRmsea = dfTarget
			vCatMlPoly = NULL

			savaleiBlocks = xmu_savalei_polycor_blocks(
				asymCovAlignedPerObs = asymCovAlignedPerObs,
				weightMatAlignedPerObs = weightMatAlignedPerObs,
				jacTargetAligned = jacTargetAligned,
				polyNames = polyNames,
				nVal = if (identical(model$data$type, "raw")) nVal else 1
			)
			if (!is.null(savaleiBlocks)) {
				impliedCor = xmu_catml_implied_correlation(model)
				if (!is.null(impliedCor)) {
					vCatMlPoly = xmu_catml_wls_v(impliedCor)
				}
				if (!is.null(vCatMlPoly) && nrow(vCatMlPoly) == length(polyNames)) {
					cHatSavalei = xmu_savalei_scaling_factor(
						jacFull = jacTargetAligned,
						jacPoly = savaleiBlocks$jacPoly,
						vCatMlPoly = vCatMlPoly,
						gammaPoly = savaleiBlocks$gammaPoly,
						wPolyWiU = savaleiBlocks$wPolyWiU,
						wFullScaled = weightMatAligned,
						dfCatMl = dfCatMl
					)
					cHatNullSavalei = xmu_savalei_null_scaling_factor(savaleiBlocks$gammaPoly, dfNullCatMl)
				}
			}

			if (is.na(cHatSavalei) || !is.finite(cHatSavalei) || cHatSavalei <= 0) {
				cHatSavalei = cTarget
			}
			if (is.na(cHatNullSavalei) || !is.finite(cHatNullSavalei) || cHatNullSavalei <= 0) {
				cHatNullSavalei = cInd
			}

			xx3Null = if (is.finite(fMlNull)) fMlNull else chisqIndRaw
			cfiRobust = xmu_savalei_fit_cfi(
				x2 = fMlTarget, df = dfCatMl, x2Null = xx3Null, dfNull = dfNullCatMl,
				cHat = cHatSavalei, cHatNull = cHatNullSavalei
			)
			tliRobust = xmu_savalei_fit_tli(
				x2 = fMlTarget, df = dfCatMl, x2Null = xx3Null, dfNull = dfNullCatMl,
				cHat = cHatSavalei, cHatNull = cHatNullSavalei
			)
			rmseaRobust = xmu_savalei_fit_rmsea(
				x2 = fMlTarget, df = dfRmsea, nVal = nVal, cHat = cHatSavalei
			)
			if (!is.na(cfiRobust)) {
				cfiRobust = max(0, min(1, cfiRobust))
			}
		}
	}

	if (!identical(correctionMethod, "Savalei2021")) {
		cfiNum    = max(chisqTargetScaled - dfTarget, 0)
		cfiDenom  = max(chisqIndScaled - dfInd, cfiNum, 0.0001)
		cfiRobust = 1 - (cfiNum / cfiDenom)

		tliNum   = (chisqTargetScaled - dfTarget) / dfTarget
		tliDenom = (chisqIndScaled - dfInd) / dfInd
		if (abs(tliDenom) < 1e-5) {
			tliRobust = NA_real_
		} else {
			tliRobust = 1 - (tliNum / tliDenom)
		}

		nVal = model$data$numObs
		if (is.null(nVal) || is.na(nVal)) {
			nVal = 1000
		}
		rmseaRobust = sqrt(max(chisqTargetScaled - dfTarget, 0) / (dfTarget * nVal))
	}

	res = list(
		scalingFactor = cTarget,
		scalingFactorNull = cInd,
		CFI    = cfiRobust,
		TLI    = tliRobust,
		RMSEA  = rmseaRobust,
		Chi    = chisqTargetScaled,
		ChiDoF = dfTarget,
		p      = pchisq(chisqTargetScaled, dfTarget, lower.tail = FALSE)
	)
	cModelAttr = cTarget
	cNullAttr = cInd
	if (identical(correctionMethod, "Savalei2021")) {
		if (is.finite(cHatSavalei) && cHatSavalei > 0) {
			cModelAttr = cHatSavalei
		}
		if (is.finite(cHatNullSavalei) && cHatNullSavalei > 0) {
			cNullAttr = cHatNullSavalei
		}
	}
	attr(res, "c_model") = cModelAttr
	attr(res, "c_null") = cNullAttr
	attr(res, "fMlTarget") = fMlTarget
	attr(res, "fMlNull") = fMlNull
	attr(res, "correction") = correctionMethod
	return(res)
}


xmu_has_WLS_jacobian <- function(model) {
    # Returns TRUE if the model was run on GenomicMx and contains the Jacobian
    return(!is.null(model$output$implied_jacobian))
}

xmu_extract_df <- function(model) {
	# Extract native engine DF directly from the C++ WLS model output slot (zero cost)
	dfNative = model$output$chiDoF
	if (is.null(dfNative) || is.na(dfNative)) {
		dfNative = summary(model)$degreesOfFreedom
	}
	
	# Frontend fallback when OpenMx reports non-positive df (common for some summary WLS setups)
	if (is.null(dfNative) || is.na(dfNative) || dfNative < 0) {
		wv = xmu_wls_extract_WV(model, stop_if_missing = FALSE)
		# nrow(useWeight) or nrow(asymCov) = number of non-redundant residual moments
		nMoments = NULL
		if (!is.null(wv$useWeight)) {
			nMoments = nrow(as.matrix(wv$useWeight))
		} else if (!is.null(wv$asymCov)) {
			nMoments = nrow(as.matrix(wv$asymCov))
		}
		if (!is.null(nMoments)) {
			estimatedParams = length(omxGetParameters(model))
			return(nMoments - estimatedParams)
		}
	}
	
	return(dfNative)
}