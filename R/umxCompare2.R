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
#' @param bicPenaltyN The penalty-N to apply if a model has an extreme N (Like genomic SEM data) (default NULL)
#' @return - a table
#' @export
#' @family Model Summary and Comparison
#' @seealso - [umxCompare()] [OpenMx::mxCompare()]
#' @references - [tutorials](https://tbates.github.io), [tutorials](https://github.com/tbates/umx)
#' @md
#' @examples
#' \dontrun{
#' 
#' }

xmu_compare_WLS = function(baseModel, comparisonModel = NULL, bicPenaltyN = NULL) {  
  # 1. Detect WLS and evaluate empirical N
  isWls     = xmu_is_wls(baseModel)
  actualN   = baseModel$data$numObs
  isGenomic = umx_is_GSEM(baseModel) | (isWls && (!is.null(actualN) && actualN > 50000))
  
  if (isWls) {
    if (is.null(bicPenaltyN)) {
      # AUTOMATIC ROUTING
      if (isGenomic) {
        # The Genomic Rescue
        bicPenaltyN = 1000
		message("umx Fiduciary Note: WLS evaluated with GSEM data or similar massive N.")
		message("  * Traditional RMSEA is structurally optimistic and should be interpreted with caution.")
		message(sprintf("  * BIC calculated using a penalized benchmark (N = %d) to prevent infinite-power overfitting.", bicPenaltyN))
      } else {
        # Standard WLS Fallback (Silent, expected behavior)
        bicPenaltyN = actualN
      }
    } else {
      # SCENARIO A: USER MANUAL OVERRIDE
      message(sprintf("umx User Override: BIC calculated using custom penalty benchmark (N = %d).", bicPenaltyN))
      
      # Optional: Add a gentle warning if they are overriding a standard model
      if (!is.null(actualN) && bicPenaltyN != actualN && !isGenomic) {
        message(sprintf("  * Note: This diverges from the empirical model sample size (N = %d).", actualN))
      }
    }

    message("umxGSEM routing: Applying high-N robust WLS comparison metrics...")
    
    # 2. Extract our custom C++ cached Jacobian
    # Using @ syntax for strict S4 slot extraction
    baseJacobian = baseModel@output$implied_jacobian
    
    if(is.null(baseJacobian)) {
      warning("Base model missing cached Jacobian. Run model with umxGSEM() to enable strict Satorra-Bentler testing.")
      return(umxCompare(baseModel, comparisonModel)) 
    }
    
    # 3. Base Model Core Statistics
    kBase = ncol(baseJacobian)
    chisqBase = summary(baseModel)$Chi
    if (is.null(chisqBase)) chisqBase = baseModel$fitfunction$result[1,1]

    # Initialize vectors assuming only one model is passed
    epVec = kBase
    chisqVec = chisqBase
    deltaEpVec = NA
    strictSbVec = NA
    pVec = NA

    # 4. Nested Comparison Logic (If user provided a second model)
    if (!is.null(comparisonModel)) {
      compJacobian = comparisonModel@output$implied_jacobian
      
      if (is.null(compJacobian)) {
        warning("Comparison model missing cached Jacobian. Cannot compute SB-2010.")
        return(umxCompare(baseModel, comparisonModel)) 
      }
      
      kComp = ncol(compJacobian)
      chisqComp = summary(comparisonModel)$Chi
      if (is.null(chisqComp)) chisqComp = comparisonModel$fitfunction$result[1,1]
      
      # Expand vectors for the two-model table
      epVec = c(kBase, kComp)
      chisqVec = c(chisqBase, chisqComp)

      # Auto-Routing Guard: Ensure baseModel (H1) is the larger model for SB math
      sbResults = NULL
      if (kBase > kComp) {
        sbResults = calculateStrictSb(baseModel = baseModel, nestedModel = comparisonModel)
      } else if (kComp > kBase) {
        # User swapped order in console. Handle silently, math requires H1 > H0.
        sbResults = calculateStrictSb(baseModel = comparisonModel, nestedModel = baseModel)
      } else {
        warning("Models have the same number of free parameters. Cannot compute nested SB difference.")
      }

      # Extract results if math fired successfully
      if (!is.null(sbResults)) {
        deltaEpVec = c(NA, sbResults["deltaDf"])
        strictSbVec = c(NA, round(sbResults["strictSbChisq"], 3))
        pVec = c(NA, round(sbResults["pValue"], 3))
      } else {
        deltaEpVec = c(NA, NA)
        strictSbVec = c(NA, NA)
        pVec = c(NA, NA)
      }
    }
    
    # 5. Calculate Absolute Fit Metrics (Placeholders for next step)
    # srmrVec = calculate_srmr(baseModel, comparisonModel)
    # pseudoBicVec = calculate_pseudo_bic(chisqVec, epVec, bicPenaltyN)
    
    srmrVec = rep(NA, length(epVec)) # Dummy pending SRMR
    pseudoBicVec = rep(NA, length(epVec)) # Dummy pending Pseudo-BIC
    
    # 6. Construct the new, mathematically sound table
    comparisonTable = data.frame(
      Base = baseModel$name,
      Comparison = ifelse(is.null(comparisonModel), "<NA>", comparisonModel$name),
      ep = epVec,
      delta_ep = deltaEpVec,
      chisq_WLS = round(chisqVec, 2),
      Strict_SB = strictSbVec,
      p = pVec,
      SRMR = srmrVec,
      Pseudo_BIC = pseudoBicVec,
      stringsAsFactors = FALSE
    )
    
    return(comparisonTable)
  }
  
  # 7. Standard routing for non-WLS models
  return(umxCompare(baseModel, comparisonModel))
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
calculateStrictSb = function(baseModel, nestedModel) {
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

	alignJacobian = function(jac, asymCov, numCovs) {
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
	invertMatrix = function(x) {
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

	# 6. Extract unscaled chi-square values
	getChisq = function(model) {
		modelSummary = summary(model)
		chisqVal = modelSummary$Chi
		if (is.null(chisqVal) || is.na(chisqVal)) {
			chisqVal = model$fitfunction$result
			if (is.matrix(chisqVal) || is.array(chisqVal)) {
				chisqVal = chisqVal[1, 1]
			}
			numObs = model$data$numObs
			if (!is.null(numObs) && !is.na(numObs)) {
				chisqVal = chisqVal * numObs
			}
		}
		return(chisqVal)
	}

	chisqBase = getChisq(baseModel)
	chisqNested = getChisq(nestedModel)

	# Calculate strictly positive scaled difference chi-square: T_d = (chisq_0 - chisq_1) / c_d
	deltaChisq = chisqNested - chisqBase
	strictSbChisq = deltaChisq / scalingFactor
	
	# Calculate p-value
	pValue = pchisq(strictSbChisq, df = deltaDf, lower.tail = FALSE)

	# Return results
	results = c(
		strictSbChisq = strictSbChisq,
		deltaDf = deltaDf,
		scalingFactor = scalingFactor,
		pValue = pValue
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
xmuCalculateSRMR = function(model) {
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
xmu_pseudo_BIC = function(chisq, k, n) {
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
xmu_build_independence_jacobian = function(K) {
	e = K * (K + 1) / 2
	jac = matrix(0, nrow = e, ncol = K)
	idx = 1
	paramIdx = 1
	for (j in 1:K) {
		for (i in j:K) {
			if (i == j) {
				jac[idx, paramIdx] = 1
				paramIdx = paramIdx + 1
			}
			idx = idx + 1
		}
	}
	return(jac)
}

#' xmu_robust_WLS_fit
#'
#' @description
#' `xmu_robust_WLS_fit` calculates the Satorra-Bentler (2010) robust CFI, TLI, and RMSEA for a WLS model.
#'
#' @param model An evaluated OpenMx model.
#' @return A list containing CFI, TLI, and RMSEA.
#' @export
#' @family Model Summary and Comparison
#' @seealso - [umxSummary()]
#' @references - Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled difference chi-square test statistic. Psychometrika, 75(2), 243-269.
#' @md
xmu_robust_WLS_fit = function(model) {
	# Step A: Extract raw Chi-Square, df, and Jacobian
	chisqTargetRaw = model$output$fit
	if (is.null(chisqTargetRaw) || is.na(chisqTargetRaw)) {
		chisqTargetRaw = model$fitfunction$result[1, 1]
	}
	
	dfTarget = model$output$degreesOfFreedom
	if (is.null(dfTarget) || is.na(dfTarget)) {
		dfTarget = summary(model)$degreesOfFreedom
	}
	
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
	
	# Calculate K (number of observed variables)
	manifests = model@manifestVars
	kVal = length(manifests)
	if (kVal == 0) {
		eVal = nrow(asymCov)
		kVal = round((-1 + sqrt(1 + 8 * eVal)) / 2)
	}
	
	# Step C: Call xmu_build_independence_jacobian(kVal)
	jacInd = xmu_build_independence_jacobian(kVal)
	
	# Step D: Calculate Independence df
	dfInd = kVal * (kVal + 1) / 2 - kVal
	
	# Step E/F: Native R baseline WLS fit calculation
	# Extract observed covariance matrix and means (if any)
	if (model$data$type == "raw") {
		obsCov = cov(model$data$observed, use = "pairwise.complete.obs")
		obsMeans = colMeans(model$data$observed, na.rm = TRUE)
	} else {
		obsCov = model$data$observed
		obsMeans = model$data$means
	}
	obsCov = obsCov[manifests, manifests, drop = FALSE]
	if (!is.null(obsMeans)) {
		obsMeans = obsMeans[manifests]
	}
	
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
		isVar = FALSE
		if (!isMean) {
			parts = strsplit(name, "[ _]")[[1]]
			parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
			if (length(parts) == 2 && parts[1] == parts[2]) {
				isVar = TRUE
			} else if (length(parts) == 1) {
				isVar = TRUE
			}
		}
		if (isMean || isVar) {
			dInd[i] = 0
		}
	}
	
	chisqIndRaw = as.numeric(t(dInd) %*% weightMat %*% dInd)
	
	# Align Jacobians
	numManifests = length(model@manifestVars)
	numCovs = (numManifests * (numManifests + 1)) / 2
	
	alignJacobian = function(jacMat, asymCovMat, numCovsVal) {
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
	jacTargetAligned = jacTargetAligned[rownames(asymCov), , drop = FALSE]
	
	jacIndAligned = jacInd
	rownames(jacIndAligned) = rownames(asymCov)
	
	# Helper to invert matrix robustly
	invertMatrix = function(x) {
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
	getScalingFactor = function(jacMat, asymCovMat, weightMatVal, dfVal) {
		info = t(jacMat) %*% weightMatVal %*% jacMat
		infoInv = invertMatrix(info)
		vw = asymCovMat %*% weightMatVal
		vwDelta = vw %*% jacMat
		uMat = vw - vwDelta %*% infoInv %*% t(jacMat) %*% weightMatVal
		traceVal = sum(diag(uMat))
		scalingFactor = traceVal / dfVal
		return(scalingFactor)
	}
	
	cTarget = getScalingFactor(jacTargetAligned, asymCov, weightMat, dfTarget)
	cInd = getScalingFactor(jacIndAligned, asymCov, weightMat, dfInd)
	
	chisqTargetScaled = chisqTargetRaw / cTarget
	chisqIndScaled = chisqIndRaw / cInd
	
	# Robust CFI
	cfiNum = max(chisqTargetScaled - dfTarget, 0)
	cfiDenom = max(chisqIndScaled - dfInd, cfiNum, 0.0001)
	cfiRobust = 1 - (cfiNum / cfiDenom)
	
	# Robust TLI
	tliNum = (chisqTargetScaled - dfTarget) / dfTarget
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
	
	return(list(CFI = cfiRobust, TLI = tliRobust, RMSEA = rmseaRobust))
}
