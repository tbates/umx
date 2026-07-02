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

	# 2. Extract Weight (W) and Asymptotic Covariance (V) matrices
	weightMat = NULL
	if (!is.null(baseModel$data$fullWeight)) {
		weightMat = baseModel$data$fullWeight
	} else if (!is.null(baseModel$data$useWeight)) {
		weightMat = baseModel$data$useWeight
	} else if (!is.null(baseModel$data$observedStats$weight)) {
		weightMat = baseModel$data$observedStats$weight
	} else if (!is.null(baseModel$data$observedStats$useWeight)) {
		weightMat = baseModel$data$observedStats$useWeight
	}
	
	if (is.null(weightMat) && inherits(baseModel$data, "MxData")) {
		if (.hasSlot(baseModel$data, "fullWeight") && !is.null(baseModel$data@fullWeight)) {
			weightMat = baseModel$data@fullWeight
		} else if (.hasSlot(baseModel$data, "useWeight") && !is.null(baseModel$data@useWeight)) {
			weightMat = baseModel$data@useWeight
		} else if (.hasSlot(baseModel$data, "observedStats")) {
			obsStats = baseModel$data@observedStats
			if (!is.null(obsStats$weight)) {
				weightMat = obsStats$weight
			} else if (!is.null(obsStats$useWeight)) {
				weightMat = obsStats$useWeight
			}
		}
		if (is.null(weightMat) && .hasSlot(baseModel$data, "weight") && is.matrix(baseModel$data@weight)) {
			weightMat = baseModel$data@weight
		}
	}
	
	asymCov = NULL
	if (!is.null(baseModel$data$acov)) {
		asymCov = baseModel$data$acov
	} else if (!is.null(baseModel$data$observedStats$asymCov)) {
		asymCov = baseModel$data$observedStats$asymCov
	}
	
	if (is.null(asymCov) && inherits(baseModel$data, "MxData")) {
		if (.hasSlot(baseModel$data, "acov") && !is.null(baseModel$data@acov)) {
			asymCov = baseModel$data@acov
		} else if (.hasSlot(baseModel$data, "observedStats")) {
			obsStats = baseModel$data@observedStats
			if (!is.null(obsStats$asymCov)) {
				asymCov = obsStats$asymCov
			}
		}
	}

	if (is.null(weightMat)) {
		stop("Could not locate the WLS Weight matrix (W) in baseModel.")
	}
	if (is.null(asymCov)) {
		stop("Could not locate the WLS Asymptotic Covariance matrix (V) in baseModel.")
	}

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
		} else if (model$data$type %in% c("cov", "acov")) {
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

#' Calculate Satorra-Bentler Robust Fit Indices for WLS Models
#'
#' @description
#' Calculates Satorra-Bentler (2010) robust fit indices (CFI, TLI, RMSEA) and scaled
#' chi-square statistics for Weighted Least Squares (WLS) models. This function
#' bypasses the OpenMx optimizer for the baseline model, calculating the 
#' unscaled fit natively in R to prevent validation errors with asymptotic 
#' covariance matrices (such as those generated in Genomic SEM).
#' 
#' @details
#' The function executes the following mathematical pipeline:
#' \enumerate{
#'   \item Extracts raw target \eqn{\chi^2}, degrees of freedom, and the implied Jacobian.
#'   \item Extracts the Asymptotic Covariance (\eqn{V}) and Weight (\eqn{W}) matrices.
#'   \item Builds the baseline independence Jacobian using \code{xmu_build_independence_jacobian}.
#'   \item Calculates the baseline fit natively as \eqn{d_{ind}^T W d_{ind}}, where \eqn{d_{ind}} 
#'         is the observed summary statistics vector with variances and means explicitly zeroed out.
#'   \item Computes the Satorra-Bentler scaling factor (\eqn{c}) using the trace matrix formula:
#'         \deqn{U = V W - V W \Delta (\Delta^T W \Delta)^{-1} \Delta^T W}
#'   \item Computes scaled \eqn{\chi^2} values: \eqn{\chi^2_{scaled} = \chi^2_{raw} / c}.
#'   \item Derives robust CFI, TLI, and RMSEA from these scaled values.
#' }
#' 
#' @param model A fitted WLS \code{MxModel}.
#' 
#' @return A named list of robust statistics designed to patch directly into 
#' the \code{umxSummary} output slots. Returns the following elements:
#' \itemize{
#'   \item \code{CFI}: Robust Comparative Fit Index
#'   \item \code{TLI}: Robust Tucker-Lewis Index
#'   \item \code{RMSEA}: Robust Root Mean Square Error of Approximation
#'   \item \code{Chi}: Satorra-Bentler scaled target Chi-square
#'   \item \code{ChiDoF}: Target model degrees of freedom
#'   \item \code{p}: P-value for the scaled target Chi-square
#' }
#' 
#' @seealso \code{\link{umxSummary}}, \code{xmu_build_independence_jacobian}
#' @family Model Summary and Comparison
#' @references - Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled difference chi-square test statistic. Psychometrika, 75(2), 243-269.
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
	
	# Step B: Extract raw Asymptotic Covariance (V) and Weight (W) matrices
	weightMat = NULL
	if (!is.null(model$data$fullWeight)) {
		weightMat = model$data$fullWeight
	} else if (!is.null(model$data$useWeight)) {
		weightMat = model$data$useWeight
	} else if (!is.null(model$data$observedStats$weight)) {
		weightMat = model$data$observedStats$weight
	} else if (!is.null(model$data$observedStats$useWeight)) {
		weightMat = model$data$observedStats$useWeight
	}
	
	if (is.null(weightMat) && inherits(model$data, "MxData")) {
		if (.hasSlot(model$data, "fullWeight") && !is.null(model$data@fullWeight)) {
			weightMat = model$data@fullWeight
		} else if (.hasSlot(model$data, "useWeight") && !is.null(model$data@useWeight)) {
			weightMat = model$data@useWeight
		} else if (.hasSlot(model$data, "observedStats")) {
			obsStats = model$data@observedStats
			if (!is.null(obsStats$weight)) {
				weightMat = obsStats$weight
			} else if (!is.null(obsStats$useWeight)) {
				weightMat = obsStats$useWeight
			}
		}
		if (is.null(weightMat) && .hasSlot(model$data, "weight") && is.matrix(model$data@weight)) {
			weightMat = model$data@weight
		}
	}
	
	asymCov = NULL
	if (!is.null(model$data$acov)) {
		asymCov = model$data$acov
	} else if (!is.null(model$data$observedStats$asymCov)) {
		asymCov = model$data$observedStats$asymCov
	}
	
	if (is.null(asymCov) && inherits(model$data, "MxData")) {
		if (.hasSlot(model$data, "acov") && !is.null(model$data@acov)) {
			asymCov = model$data@acov
		} else if (.hasSlot(model$data, "observedStats")) {
			obsStats = model$data@observedStats
			if (!is.null(obsStats$asymCov)) {
				asymCov = obsStats$asymCov
			}
		}
	}
	
	if (is.null(weightMat)) {
		stop("Could not locate WLS Weight matrix (W).")
	}
	if (is.null(asymCov)) {
		stop("Could not locate Asymptotic Covariance matrix (V).")
	}
	
	if (nrow(jacTarget) != nrow(asymCov)) {
		stop(sprintf("implied_jacobian row count (%d) does not match asymptotic covariance matrix row count (%d) (this occurs in ordinal WLS models).", nrow(jacTarget), nrow(asymCov)))
	}
	
	rowNames = rownames(asymCov)
	
	# Calculate K (number of observed variables)
	manifests = model@manifestVars
	kVal = length(manifests)
	if (kVal == 0) {
		eVal = nrow(asymCov)
		kVal = round((-1 + sqrt(1 + 8 * eVal)) / 2)
	}
	
	# Step C: Call xmu_build_independence_jacobian(rowNames, manifests)
	jacInd = xmu_build_independence_jacobian(rowNames, manifests)
	
	# Step D: Calculate Independence df
	dfInd = nrow(asymCov) - ncol(jacInd)
	
	# Step E/F: Native R baseline WLS fit calculation
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
	
	sVec = rep(0, nrow(asymCov))
	rowNames = rownames(asymCov)
	for (i in 1:length(rowNames)) {
		name = rowNames[i]
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
	for (i in 1:length(rowNames)) {
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
		if (isMean || isVar || isThresh) {
			dInd[i] = 0
		}
	}
	
	chisqIndRaw = as.numeric(t(dInd) %*% weightMat %*% dInd)
	
	# Align Jacobians
	numManifests = length(model@manifestVars)
	numCovs = (numManifests * (numManifests + 1)) / 2
	
	alignJacobian <- function(jacMat, asymCovMat, numCovsVal) {
		if (nrow(jacMat) == nrow(asymCovMat)) {
			alignedJac = jacMat
			if (is.null(rownames(alignedJac))) {
				rownames(alignedJac) = rownames(asymCovMat)
			}
			return(alignedJac)
		}
		numMeans = nrow(jacMat) - numCovsVal
		if (numMeans > 0) {
			meanIdx = (numCovsVal + 1):nrow(jacMat)
			covIdx = 1:numCovsVal
			alignedJac = jacMat[c(meanIdx, covIdx), , drop = FALSE]
		} else {
			alignedJac = jacMat
		}
		rownames(alignedJac) = rownames(asymCovMat)
		return(alignedJac)
	}
	
	jacTargetAligned = alignJacobian(jacTarget, asymCov, numCovs)
	if (!is.null(rownames(asymCov))) {
		targetNames = rownames(jacTargetAligned)
		asymNames = rownames(asymCov)
		if (!is.null(targetNames)) {
			cleanTargetNames = gsub("^(Cov|Mean):", "", targetNames)
			cleanAsymNames = gsub("^(Cov|Mean):", "", asymNames)
			cleanTargetNames = gsub("^(cov|mean)_", "", cleanTargetNames)
			cleanAsymNames = gsub("^(cov|mean)_", "", cleanAsymNames)
			cleanTargetNames = gsub("[._:]", " ", cleanTargetNames)
			cleanAsymNames = gsub("[._:]", " ", cleanAsymNames)
			
			idx = match(cleanAsymNames, cleanTargetNames)
			if (!any(is.na(idx)) && length(idx) == nrow(jacTargetAligned)) {
				jacTargetAligned = jacTargetAligned[idx, , drop = FALSE]
				rownames(jacTargetAligned) = rownames(asymCov)
			} else {
				rownames(jacTargetAligned) = rownames(asymCov)
			}
		} else {
			rownames(jacTargetAligned) = rownames(asymCov)
		}
	}
	
	jacIndAligned = jacInd
	if (!is.null(rownames(asymCov))) {
		rownames(jacIndAligned) = rownames(asymCov)
	}
	
	# Helper to invert matrix robustly
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
	
	# Trace helper for single model scaling factor
	getScalingFactor <- function(jacMat, asymCovMat, weightMatVal, dfVal) {
		if (is.null(dfVal) || is.na(dfVal) || dfVal <= 0) {
			return(NA_real_)
		}
		info     = t(jacMat) %*% weightMatVal %*% jacMat
		infoInv  = invertMatrix(info)
		vw       = asymCovMat %*% weightMatVal
		vwDelta  = vw %*% jacMat
		uMat     = vw - vwDelta %*% infoInv %*% t(jacMat) %*% weightMatVal
		traceVal = sum(diag(uMat))
		scalingFactor = traceVal / dfVal
		return(scalingFactor)
	}
	
	cTarget = getScalingFactor(jacTargetAligned, asymCov, weightMat, dfTarget)
	cInd    = getScalingFactor(jacIndAligned, asymCov, weightMat, dfInd)
	
	chisqTargetScaled = chisqTargetRaw / cTarget
	chisqIndScaled    = chisqIndRaw / cInd
	
	# Robust CFI
	cfiNum    = max(chisqTargetScaled - dfTarget, 0)
	cfiDenom  = max(chisqIndScaled - dfInd, cfiNum, 0.0001)
	cfiRobust = 1 - (cfiNum / cfiDenom)
	
	# Robust TLI
	tliNum   = (chisqTargetScaled - dfTarget) / dfTarget
	tliDenom = (chisqIndScaled - dfInd) / dfInd
	if (abs(tliDenom) < 1e-5) {
		tliRobust = NA_real_
	} else {
		tliRobust = 1 - (tliNum / tliDenom)
	}
	
	# Robust RMSEA
	nVal = model$data$numObs
	if (is.null(nVal) || is.na(nVal)) {
		nVal = 1000
	}
	rmseaRobust = sqrt(max(chisqTargetScaled - dfTarget, 0) / (dfTarget * nVal))
	
	return(list(
	    CFI    = cfiRobust, 
	    TLI    = tliRobust, 
	    RMSEA  = rmseaRobust,
	    Chi    = chisqTargetScaled,
	    ChiDoF = dfTarget,
	    p      = pchisq(chisqTargetScaled, dfTarget, lower.tail = FALSE)
	))
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
	
	# TODO: this is probably not necessary, as acov appears to be working in OpenMx now
	# Auto-Resolving Gate: Catch OpenMx acov bug (df <= 0)
	if (is.null(dfNative) || is.na(dfNative) || dfNative < 0) {
		if (!is.null(model$data) && identical(model$data$type, "acov")) {
			
			acovMat = NULL
			
			# 1. Safely probe for the standard acov slot (using S4 @ or list $)
			if (isS4(model$data) && .hasSlot(model$data, "acov")) {
				acovMat = model$data@acov
			} else if (!isS4(model$data) && !is.null(model$data$acov)) {
				acovMat = model$data$acov
			}
			
			# 2. Safely probe for the nested observedStats$asymCov slot
			if (is.null(acovMat)) {
				if (isS4(model$data) && .hasSlot(model$data, "observedStats") && !is.null(model$data@observedStats$asymCov)) {
					acovMat = model$data@observedStats$asymCov
				} else if (!isS4(model$data) && !is.null(model$data$observedStats$asymCov)) {
					acovMat = model$data$observedStats$asymCov
				}
			}
			
			# 3. Calculate and assert frontend math
			if (!is.null(acovMat)) {
				observedStats   = nrow(acovMat)
				estimatedParams = length(omxGetParameters(model))
				return(observedStats - estimatedParams)
			}
		}
	}
	
	return(dfNative)
}