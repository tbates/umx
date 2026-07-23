#' calculateStrictSb
#'
#' @description
#' `calculateStrictSb` calculates the Satorra-Bentler (2010) scaled difference
#' chi-square test (the "strictly positive" design relative to SB-2001, which
#' can yield a negative scaling constant \eqn{c_d}). The returned statistic is
#' only interpretable when both models share the same WLS moments, weight
#' matrix, and \eqn{N}, and when the freer model's optimized discrepancy is not
#' larger than the nested model's (\eqn{F_{nested} \ge F_{base}}).
#' Non-monotone \eqn{F} (typically failed optimization) returns `NA` with a warning.
#'
#' @param baseModel The base model (more parameters)
#' @param nestedModel The nested model (fewer parameters)
#' @return A named vector containing strictSbChisq, deltaDf, scalingFactor, and pValue.
#'   May carry attribute \code{nonMonotoneF = TRUE} when \eqn{\Delta F < 0}.
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

	# 2. Extract Weight (W) and Asymptotic Covariance (V) - modern observedStats;
	# multigroup: block-diagonal across groups (same as xmu_robust_WLS_fit).
	wv = xmu_wls_extract_WV(baseModel, stop_if_missing = TRUE)
	weightMat = wv$useWeight
	asymCov   = wv$asymCov
	nGroups   = if (!is.null(wv$nGroups)) wv$nGroups else 1L

	if (is.null(rownames(asymCov))) {
		stop("Asymptotic covariance matrix missing rownames; cannot align Jacobian for SB-2010.")
	}
	# DWLS useWeight often lacks dimnames - copy from asymCov when congruent
	if (is.null(rownames(weightMat)) && nrow(weightMat) == nrow(asymCov) && ncol(weightMat) == ncol(asymCov)) {
		dimnames(weightMat) = dimnames(asymCov)
	}

	# 3. Align Jacobians to V/W moment order
	# Multigroup / named residuals: align by name (stacked group.moment labels).
	# Single-group continuous: optional means block-shift when names missing.
	alignJacobian <- function(jac, asymCov, nGroups) {
		rn = rownames(asymCov)
		if (is.null(rownames(jac)) && nrow(jac) == length(rn)) {
			rownames(jac) = rn
		}
		if (!is.null(rownames(jac)) && all(rn %in% rownames(jac))) {
			return(jac[rn, , drop = FALSE])
		}
		# Single-group block-shift fallback (means at bottom of jac -> top of V)
		if (nGroups <= 1L) {
			numManifests = length(baseModel@manifestVars)
			if (numManifests < 1) {
				numManifests = round((-1 + sqrt(1 + 8 * nrow(jac))) / 2)
			}
			numCovs = (numManifests * (numManifests + 1)) / 2
			numMeans = nrow(jac) - numCovs
			if (numMeans > 0 && numCovs > 0 && (numMeans + numCovs) == nrow(jac)) {
				meanIdx = (numCovs + 1):nrow(jac)
				covIdx = 1:numCovs
				jac = jac[c(meanIdx, covIdx), , drop = FALSE]
			}
			if (nrow(jac) == length(rn)) {
				rownames(jac) = rn
				return(jac)
			}
		}
		stop("Cannot align implied_jacobian (", nrow(jac), " x ", ncol(jac),
			") to asymCov moments (", length(rn), ").")
	}

	jacBase = alignJacobian(jacBase, asymCov, nGroups)
	jacNested = alignJacobian(jacNested, asymCov, nGroups)

	commonNames = rownames(asymCov)
	weightMat = weightMat[commonNames, commonNames, drop = FALSE]
	asymCov = asymCov[commonNames, commonNames, drop = FALSE]
	jacBase = jacBase[commonNames, , drop = FALSE]
	jacNested = jacNested[commonNames, , drop = FALSE]

	# Scale raw-data V/W to sample units (single-group or per multigroup block)
	dataModels = xmu_wls_data_models(baseModel)
	if (nGroups == 1L && length(dataModels) == 1L) {
		d = dataModels[[1]]$data
		if (identical(d$type, "raw")) {
			nVal = if (!is.null(d$numObs) && is.finite(d$numObs)) as.numeric(d$numObs) else nrow(d$observed)
			if (is.finite(nVal) && nVal > 0) {
				weightMat = weightMat * nVal
				asymCov = asymCov * nVal
			}
		}
	} else if (nGroups > 1L) {
		pos = 0L
		for (sm in dataModels) {
			d = sm$data
			ng = if (!is.null(d$numObs) && is.finite(d$numObs)) as.numeric(d$numObs) else if (!is.null(d$observed)) nrow(d$observed) else NA_real_
			k = if (!is.null(d$observedStats$asymCov)) nrow(d$observedStats$asymCov) else nrow(d$observedStats$useWeight)
			if (!is.finite(ng) || ng <= 0 || is.null(k) || !is.finite(k)) next
			if (identical(d$type, "raw")) {
				idx = (pos + 1L):(pos + k)
				weightMat[idx, idx] = weightMat[idx, idx] * ng
				asymCov[idx, idx] = asymCov[idx, idx] * ng
			}
			pos = pos + k
		}
	}

	# 4-5. Projection matrix algebra (Satorra & Bentler, 2010)
	infoBase = t(jacBase) %*% weightMat %*% jacBase
	infoBaseInv = xmu_invert_matrix(infoBase)
	mBase = weightMat - weightMat %*% jacBase %*% infoBaseInv %*% t(jacBase) %*% weightMat

	infoNested = t(jacNested) %*% weightMat %*% jacNested
	infoNestedInv = xmu_invert_matrix(infoNested)
	mNested = weightMat - weightMat %*% jacNested %*% infoNestedInv %*% t(jacNested) %*% weightMat

	mDiff = mNested - mBase
	deltaDf = ncol(jacBase) - ncol(jacNested)

	traceVal = sum(diag(mDiff %*% asymCov))
	scalingFactor = traceVal / deltaDf

	# 6. RAW unscaled discrepancies (output$fit = N * r'Wr; same scale on both models)
	getRawFit <- function(model) {
		rawFit = model$output$fit
		if (is.null(rawFit) || is.na(rawFit)) {
			rawFit = model$fitfunction$result
			if (is.matrix(rawFit) || is.array(rawFit)) {
				rawFit = rawFit[1, 1]
			}
		}
		return(as.numeric(rawFit))
	}

	rawBase   = getRawFit(baseModel)
	rawNested = getRawFit(nestedModel)

	if (!is.finite(rawBase) || !is.finite(rawNested)) {
		warning("Non-finite WLS fit on base or nested model; cannot compute SB-2010 difference.", call. = FALSE)
		return(c(
			strictSbChisq = NA_real_,
			deltaDf       = deltaDf,
			scalingFactor = scalingFactor,
			pValue        = NA_real_
		))
	}

	deltaRaw = rawNested - rawBase
	# Nested-monotone under fixed s, W, N at global mins: deltaRaw >= 0.
	# Small negative noise -> 0; clear violation -> NA (do not report negative chi-square / p = 1).
	tol = 1e-6 * max(1, abs(rawBase), abs(rawNested))
	if (deltaRaw < -tol) {
		warning("WLS discrepancy F is smaller for the nested model than for the freer base (F_nested = ",
			signif(rawNested, 6), ", F_base = ", signif(rawBase, 6),
			"). Nested Satorra-Bentler Delta chi-square is not interpretable; check optimization or nesting (same useWeight, moments, and N).",
			call. = FALSE)
		res = c(
			strictSbChisq = NA_real_,
			deltaDf       = deltaDf,
			scalingFactor = scalingFactor,
			pValue        = NA_real_
		)
		attr(res, "nonMonotoneF") = TRUE
		return(res)
	}
	if (deltaRaw < 0) {
		deltaRaw = 0
	}

	if (!is.finite(scalingFactor) || scalingFactor <= 0) {
		warning("Non-positive or non-finite SB-2010 scaling factor (tr(M_diff * Gamma)/df); cannot scale Delta chi-square.",
			call. = FALSE)
		return(c(
			strictSbChisq = NA_real_,
			deltaDf       = deltaDf,
			scalingFactor = scalingFactor,
			pValue        = NA_real_
		))
	}

	strictSbChisq = deltaRaw / scalingFactor
	pValue = pchisq(strictSbChisq, df = abs(deltaDf), lower.tail = FALSE)

	c(
		strictSbChisq = strictSbChisq,
		deltaDf       = deltaDf,
		scalingFactor = scalingFactor,
		pValue        = pValue
	)
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


# Create the trivial independence model Jacobian in R
# rowNames may be multigroup-prefixed (e.g. "DWLS_1.var_mpg"); strip first "group." segment for typing.
xmu_build_independence_jacobian <- function(rowNames, manifests) {
	P = length(rowNames)
	free_rows = logical(P)
	for (i in 1:P) {
		name = rowNames[i]
		nameBare = sub("^[^.]+\\.", "", name)
		# If stripping removed nothing useful (no dot), nameBare == name
		isMean = grepl("^mean_", nameBare) || grepl("^one_to_", nameBare) || (nameBare %in% manifests) || grepl("^Mean:", nameBare)
		isThresh = grepl("t[0-9]+$", nameBare)
		isVar = FALSE
		if (!isMean && !isThresh) {
			if (grepl("^var_", nameBare) || grepl("^Cov:([^_]+)_\\1$", nameBare)) {
				isVar = TRUE
			} else {
				parts = strsplit(nameBare, "[ _]")[[1]]
				parts = parts[!parts %in% c("var", "poly", "cov", "with", "to", "Cov:")]
				if (length(parts) == 2 && parts[1] == parts[2]) {
					isVar = TRUE
				} else if (length(parts) == 1) {
					isVar = TRUE
				}
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
