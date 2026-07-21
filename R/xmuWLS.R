# ===========================================================================
# = WLS Helpers =============================================================
# ===========================================================================

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
			warning(
				"Base model missing cached WLS Jacobian. ",
				xmu_openmx_install_message("Strict Satorra-Bentler (2010) nested tests"),
				call. = FALSE
			)
			options(umx_warned_legacy_wls = TRUE)
		}
	}
    
	# 3. Base Model Core Statistics
	# If Jacobian is missing, fallback to standard parameter count
	kBase = ifelse(!is.null(baseJacobian), ncol(baseJacobian), length(omxGetParameters(baseModel)))
	
	# Note: In ML we need to subtract the -2LLs of the model from a base model.
	# In WLS: The discrepancy function inherently represents the squared, weighted differences between the observed and implied matrices.
	# Continuous WLS Chi for the table is SB-scaled output$fit (via xmu_wls_display_chi), not Browne output$chi.
	# GSEM track keeps output$chi (DWLS structural scale).

	if (isGenomic) {
		chisqBase = baseModel$output$chi
		if (is.null(chisqBase)) {
			chisqBase = summary(baseModel)$Chi
		}
		cfiBase = NA_real_
		tryCatch({
			dispBase = xmu_wls_display_chi(baseModel)
			cfiBase = dispBase$CFI
		}, error = function(e) NULL)
	} else {
		dispBase = xmu_wls_display_chi(baseModel)
		chisqBase = dispBase$Chi
		cfiBase = dispBase$CFI
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
		
		if (isGenomic) {
			chisqComp = comparisonModel$output$chi
			if (is.null(chisqComp)) {
				chisqComp = summary(comparisonModel)$Chi
			}
			cfiComp = NA_real_
			tryCatch({
				dispComp = xmu_wls_display_chi(comparisonModel)
				cfiComp = dispComp$CFI
			}, error = function(e) NULL)
		} else {
			dispComp = xmu_wls_display_chi(comparisonModel)
			chisqComp = dispComp$Chi
			cfiComp = dispComp$CFI
		}
		
		epVec = c(kBase, kComp)
		chisqVec = c(chisqBase, chisqComp)

		deltaDf = abs(kBase - kComp)
		deltaDfVec = c(NA, deltaDf)

		if (isGenomic) {
			# Track B: Genomic SEM — difference of DWLS chi-squares on the GSEM scale
			message("umx Note: Genomic SEM model detected. Nested diffFit uses the GSEM DWLS chi-square difference (not ML LRT). Prefer SRMR for absolute fit; de-emphasize CFI cutoffs (see ?umxCompare).")
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
					# Non-monotone F or failed algebra: leave NA (calculateStrictSb already warned when applicable)
					if (is.null(sbResults) || is.null(attr(sbResults, "nonMonotoneF")) || !isTRUE(attr(sbResults, "nonMonotoneF"))) {
						warning("Satorra-Bentler calculation failed: returned NA", call. = FALSE)
					}
					diffFitVec = c(NA, NA)
					pVec = c(NA, NA)
				}
			} else {
				# Graceful degradation for legacy WLS
				message(
					"umx Note: One or both models lack a cached Jacobian. Skipping strict Satorra-Bentler (2010) difference test.\n",
					xmu_openmx_install_message("Strict Satorra-Bentler nested WLS comparison")
				)
				diffFitVec = c(NA, NA)
				pVec = c(NA, NA)
			}
		}
	}

	# 5. AIC on the same Chi scale as the table (Chi + 2*EP)
	aicBase = as.numeric(chisqBase) + 2 * kBase
	if (!is.null(comparisonModel)) {
		aicComp = as.numeric(chisqComp) + 2 * kComp
		aicVec = c(aicBase, aicComp)
	} else {
		aicVec = aicBase
	}

	# 6. CFI from display path (already extracted with Chi above)
	if (!is.null(comparisonModel)) {
		cfiVec = c(cfiBase, cfiComp)
		deltaCfiVec = c(NA, as.numeric(cfiComp) - as.numeric(cfiBase))
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

xmu_has_WLS_jacobian <- function(model) {
    # Returns TRUE if the model was run on GenomicMx and contains the Jacobian
    return(!is.null(model$output$implied_jacobian))
}

#' Display chi-square and AFIs for a WLS model (shared by summary and compare)
#'
#' When \code{output$implied_jacobian} is present, returns Satorra–Bentler (2010)
#' scaled statistics from [xmu_robust_WLS_fit()] (including saturated df=0
#' convention: Chi=0, CFI/TLI=1, RMSEA=0). Otherwise falls back to OpenMx
#' \code{summary()} Browne residual chi-square.
#'
#' @param model A run MxModel (typically WLS/DWLS).
#' @return Named list with \code{Chi}, \code{ChiDoF}, \code{p}, \code{CFI},
#'   \code{TLI}, \code{RMSEA}, and \code{source} one of
#'   \code{"SB2010"}, \code{"saturated"}, \code{"OpenMx"}.
#' @family xmu internal not for end user
#' @seealso [xmu_robust_WLS_fit()], [xmu_compare_WLS()], [umxSummary()]
xmu_wls_display_chi <- function(model) {
	out = list(
		Chi = NA_real_,
		ChiDoF = NA_real_,
		p = NA_real_,
		CFI = NA_real_,
		TLI = NA_real_,
		RMSEA = NA_real_,
		source = "OpenMx"
	)
	if (xmu_has_WLS_jacobian(model)) {
		robustFit = tryCatch(xmu_robust_WLS_fit(model), error = function(e) NULL)
		if (!is.null(robustFit)) {
			out$Chi = robustFit$Chi
			out$ChiDoF = robustFit$ChiDoF
			out$p = robustFit$p
			out$CFI = robustFit$CFI
			out$TLI = robustFit$TLI
			out$RMSEA = robustFit$RMSEA
			correction = attr(robustFit, "correction")
			if (!is.null(correction) && identical(correction, "saturated")) {
				out$source = "saturated"
			} else {
				out$source = "SB2010"
			}
			return(out)
		}
	}
	modelSummary = tryCatch(summary(model), error = function(e) NULL)
	if (!is.null(modelSummary)) {
		out$Chi = modelSummary$Chi
		out$ChiDoF = if (!is.null(modelSummary$ChiDoF)) modelSummary$ChiDoF else modelSummary$degreesOfFreedom
		out$p = modelSummary$p
		out$CFI = modelSummary$CFI
		out$TLI = modelSummary$TLI
		out$RMSEA = modelSummary$RMSEA
	} else if (!is.null(model$output$chi)) {
		out$Chi = model$output$chi
		out$ChiDoF = model$output$chiDoF
	}
	out$source = "OpenMx"
	return(out)
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

#' Models that hold WLS data (top-level or multigroup children)
#'
#' Multigroup `umxRAM(..., group=)` containers often have `model$data = NULL`
#' with each group's raw/summary data on `model$submodels`.
#'
#' @param model An [OpenMx::mxModel()].
#' @return List of models that each have non-null `$data`.
#' @keywords internal
xmu_wls_data_models <- function(model) {
	if (is.null(model)) {
		return(list())
	}
	if (!is.null(model$data)) {
		return(list(model))
	}
	out = list()
	if (length(model$submodels) > 0) {
		for (sm in model$submodels) {
			out = c(out, xmu_wls_data_models(sm))
		}
	}
	out
}

#' Block-diagonal combine of square matrices (unique dimnames)
#'
#' @param mats List of square matrices.
#' @param namePrefix Optional character vector of prefixes (one per matrix).
#' @return One block-diagonal matrix.
#' @keywords internal
xmu_wls_block_diag <- function(mats, namePrefix = NULL) {
	mats = Filter(Negate(is.null), mats)
	if (length(mats) == 0) {
		return(NULL)
	}
	if (length(mats) == 1L) {
		return(mats[[1]])
	}
	sizes = vapply(mats, nrow, integer(1))
	n = sum(sizes)
	out = matrix(0, n, n)
	rn = character(n)
	pos = 0L
	for (i in seq_along(mats)) {
		M = as.matrix(mats[[i]])
		k = nrow(M)
		idx = (pos + 1L):(pos + k)
		out[idx, idx] = M
		rni = rownames(M)
		if (is.null(rni)) {
			rni = paste0("m", seq_len(k))
		}
		if (!is.null(namePrefix) && length(namePrefix) >= i && nzchar(namePrefix[i])) {
			rni = paste0(namePrefix[i], ".", rni)
		}
		rn[idx] = rni
		pos = pos + k
	}
	dimnames(out) = list(rn, rn)
	out
}

#' Extract WLS useWeight (W) and asymCov (Gamma) from an MxModel
#'
#' **Modern only:** reads \code{observedStats = list(cov=, useWeight=, asymCov=)}.
#' OpenMx legacy \code{type="acov"} / \code{"none"} / \code{MxDataLegacyWLS} is refused forever.
#'
#' For multigroup models with data only on submodels, returns **block-diagonal**
#' \code{useWeight} and \code{asymCov} across groups (row/column names prefixed
#' by group name when more than one group contributes).
#'
#' @param model An [OpenMx::mxModel()] with data (or multigroup children with data).
#' @param stop_if_missing If TRUE, stop when either matrix is missing.
#' @return List with \code{useWeight}, \code{asymCov}, optional \code{cov}, and
#'   \code{nGroups} (integer).
#' @family xmu internal not for end user
xmu_wls_extract_WV <- function(model, stop_if_missing = TRUE) {
	useWeight = NULL
	asymCov   = NULL
	obsCov    = NULL
	nGroups   = 1L

	dataModels = xmu_wls_data_models(model)
	if (length(dataModels) == 0) {
		if (stop_if_missing) {
			stop("Model has no data (including multigroup submodels); cannot extract WLS weight / asymptotic covariance matrices.")
		}
		return(list(useWeight = NULL, asymCov = NULL, cov = NULL, nGroups = 0L))
	}

	extract_one = function(m) {
		data = m$data
		if (exists("xmu_is_legacy_acov_data", mode = "function") && xmu_is_legacy_acov_data(data)) {
			if (exists("xmu_stop_legacy_acov", mode = "function")) {
				xmu_stop_legacy_acov("xmu_wls_extract_WV")
			}
			stop("xmu_wls_extract_WV: legacy WLS data API is not supported. Use type='summary' with observedStats useWeight + asymCov.", call. = FALSE)
		}
		dataType = tryCatch({
			if (isS4(data) && .hasSlot(data, "type")) data@type else data$type
		}, error = function(e) NULL)
		if (identical(dataType, "summary") && exists("xmu_require_summary_mxData", mode = "function")) {
			xmu_require_summary_mxData("xmu_wls_extract_WV")
		}
		os = NULL
		if (isS4(data) && .hasSlot(data, "observedStats") && length(data@observedStats)) {
			os = data@observedStats
		} else if (!is.null(data$observedStats) && length(data$observedStats)) {
			os = data$observedStats
		}
		uw = NULL; ac = NULL; oc = NULL
		if (!is.null(os)) {
			if (!is.null(os$acov) || !is.null(os$fullWeight)) {
				stop("xmu_wls_extract_WV: observedStats$acov / $fullWeight are not supported (legacy name trap). Use useWeight and asymCov.", call. = FALSE)
			}
			if (!is.null(os$useWeight)) {
				uw = os$useWeight
			} else if (!is.null(os$weight) && is.matrix(os$weight)) {
				uw = os$weight
			}
			if (!is.null(os$asymCov)) {
				ac = os$asymCov
			}
			if (!is.null(os$cov)) {
				oc = os$cov
			}
			# DWLS diagonal W often lacks dimnames; align to asymCov residual labels
			if (!is.null(uw) && !is.null(ac) && is.null(rownames(uw)) && !is.null(rownames(ac)) &&
			    nrow(uw) == nrow(ac) && ncol(uw) == ncol(ac)) {
				dimnames(uw) = dimnames(ac)
			} else if (!is.null(uw) && !is.null(ac) && is.null(rownames(uw)) && !is.null(colnames(ac)) &&
			           nrow(uw) == nrow(ac) && ncol(uw) == ncol(ac)) {
				dimnames(uw) = list(colnames(ac), colnames(ac))
			}
		}
		if (is.null(oc) && !is.null(data$observed) && is.matrix(data$observed)) {
			oc = data$observed
		}
		list(useWeight = uw, asymCov = ac, cov = oc, name = m$name)
	}

	if (length(dataModels) == 1L) {
		one = extract_one(dataModels[[1]])
		useWeight = one$useWeight
		asymCov = one$asymCov
		obsCov = one$cov
		nGroups = 1L
	} else {
		nGroups = length(dataModels)
		piecesW = list()
		piecesV = list()
		prefixes = character(nGroups)
		for (i in seq_along(dataModels)) {
			one = extract_one(dataModels[[i]])
			if (stop_if_missing) {
				if (is.null(one$useWeight)) {
					stop("Could not locate observedStats$useWeight (W) in group ", omxQuotes(dataModels[[i]]$name), ".")
				}
				if (is.null(one$asymCov)) {
					stop("Could not locate observedStats$asymCov (Gamma) in group ", omxQuotes(dataModels[[i]]$name), ".")
				}
			}
			piecesW[[i]] = one$useWeight
			piecesV[[i]] = one$asymCov
			prefixes[i] = dataModels[[i]]$name
		}
		useWeight = xmu_wls_block_diag(piecesW, namePrefix = prefixes)
		asymCov = xmu_wls_block_diag(piecesV, namePrefix = prefixes)
		# obsCov is group-specific; leave NULL for multigroup (robust path uses V/W)
		obsCov = NULL
	}

	if (stop_if_missing) {
		if (is.null(useWeight)) {
			stop("Could not locate observedStats$useWeight (W). Use mxData(numObs=N, observedStats=list(cov=S, useWeight=W, asymCov=V)).")
		}
		if (is.null(asymCov)) {
			stop("Could not locate observedStats$asymCov (Gamma). Use mxData(numObs=N, observedStats=list(cov=S, useWeight=W, asymCov=V)).")
		}
	}
	return(list(useWeight = useWeight, asymCov = asymCov, cov = obsCov, nGroups = nGroups))
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
	
	# Step B: Extract W and asymCov (modern observedStats; multigroup → block-diagonal)
	# (Saturated early-return comes after this so missing W/Γ still error.)
	wv = xmu_wls_extract_WV(model, stop_if_missing = TRUE)
	weightMat = wv$useWeight
	asymCov   = wv$asymCov
	nGroups   = if (!is.null(wv$nGroups)) wv$nGroups else 1L

	# Saturated / non-positive residual df: SB scaling is undefined.
	# Report SEM convention (Chi=0, perfect incremental AFIs) rather than NA.
	if (!is.null(dfTarget) && is.finite(dfTarget) && dfTarget <= 0) {
		rawF = as.numeric(chisqTargetRaw)
		if (is.finite(rawF) && rawF > 1e-4) {
			warning("Saturated residual df (ChiDoF <= 0) but WLS discrepancy F is not near zero (F = ",
				signif(rawF, 4), "). Optimizer may not have reached a global minimum; nested tests may be invalid.",
				call. = FALSE)
		}
		res = list(
			scalingFactor = NA_real_,
			scalingFactorNull = NA_real_,
			CFI = 1,
			TLI = 1,
			RMSEA = 0,
			Chi = 0,
			ChiDoF = dfTarget,
			p = NA_real_
		)
		attr(res, "c_model") = NA_real_
		attr(res, "c_null") = NA_real_
		attr(res, "fMlTarget") = NA_real_
		attr(res, "fMlNull") = NA_real_
		attr(res, "correction") = "saturated"
		return(res)
	}

	# Multigroup: require stacked Jacobian (GenomicMx stacks one block per WLS RAM group)
	if (nGroups > 1L) {
		nMoments = nrow(asymCov)
		if (nrow(jacTarget) != nMoments) {
			stop("Multigroup WLS robust CFI/TLI/RMSEA need a stacked implied_jacobian matching all groups' moments (got Jacobian ",
				nrow(jacTarget), " x ", ncol(jacTarget), " vs stacked moments ", nMoments,
				").\n", xmu_openmx_install_message("Multigroup WLS robust fit (stacked Jacobian)"))
		}
		if (is.null(rownames(jacTarget)) && !is.null(rownames(asymCov))) {
			rownames(jacTarget) = rownames(asymCov)
		}
	}
	
	rowNames = rownames(asymCov)
	if (is.null(rowNames)) {
		stop("Asymptotic covariance matrix missing rownames.")
	}

	# Calculate K (number of observed variables) — single-group heuristic
	manifests = model@manifestVars
	if ((is.null(manifests) || length(manifests) == 0) && nGroups > 1L) {
		# Take manifests from first data-bearing group
		dms = xmu_wls_data_models(model)
		if (length(dms) > 0) {
			manifests = dms[[1]]@manifestVars
		}
	}
	kVal = length(manifests)
	if (kVal == 0) {
		eVal = if (nGroups > 1L) nrow(asymCov) / nGroups else nrow(asymCov)
		kVal = round((-1 + sqrt(1 + 8 * eVal)) / 2)
	}
	numCovs = (kVal * (kVal + 1)) / 2
	if (nGroups > 1L) {
		# Alignment is by name only; numCovs reordering heuristic is single-group
		numCovs = nrow(asymCov)
	}

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
	# Extract observed covariance matrix and means (if any).
	# Modern summary WLS / GSEM: type is "summary" with cov in observedStats.
	# Multigroup: build a named stacked residual moment vector instead of one obsCov.
	obsCov = NULL
	obsMeans = NULL
	obsThresholds = NULL
	os = NULL
	sVecStacked = NULL
	dataIsRaw = FALSE
	nVal = NA_real_

	if (nGroups > 1L) {
		dms = xmu_wls_data_models(model)
		sList = list()
		nSum = 0
		anyRaw = FALSE
		for (i in seq_along(dms)) {
			sm = dms[[i]]
			d = sm$data
			osi = NULL
			if (isS4(d) && .hasSlot(d, "observedStats") && length(d@observedStats)) {
				osi = d@observedStats
			} else if (!is.null(d$observedStats)) {
				osi = d$observedStats
			}
			if (is.null(osi$cov) || !is.matrix(osi$cov)) {
				stop("Multigroup WLS robust fit: group ", omxQuotes(sm$name), " lacks observedStats$cov.")
			}
			# Residual moment order matching asymCov colnames for this group
			gNames = colnames(osi$asymCov)
			if (is.null(gNames)) gNames = colnames(osi$useWeight)
			if (is.null(gNames)) {
				stop("Multigroup WLS robust fit: group ", omxQuotes(sm$name), " asymCov/useWeight lack dimnames.")
			}
			sv = rep(0, length(gNames))
			names(sv) = paste0(sm$name, ".", gNames)
			covg = osi$cov
			meansg = osi$means
			for (j in seq_along(gNames)) {
				nm = gNames[j]
				if (grepl("^var_", nm)) {
					v = sub("^var_", "", nm)
					if (v %in% colnames(covg)) sv[j] = covg[v, v]
				} else if (grepl("^poly_", nm)) {
					rest = sub("^poly_", "", nm)
					parts = strsplit(rest, "_", fixed = TRUE)[[1]]
					if (length(parts) >= 2) {
						# poly_A_B : last token is second var; rest is first (handles multi-underscore rare)
						b = parts[length(parts)]
						a = paste(parts[-length(parts)], collapse = "_")
						if (a %in% colnames(covg) && b %in% colnames(covg)) {
							sv[j] = covg[a, b]
						}
					}
				} else if (grepl("^mean_|^one_to_", nm) && !is.null(meansg)) {
					v = sub("^mean_|^one_to_", "", nm)
					if (v %in% names(meansg)) sv[j] = meansg[v]
					else if (is.matrix(meansg) && v %in% colnames(meansg)) sv[j] = meansg[1, v]
				}
			}
			sList[[i]] = sv
			if (identical(d$type, "raw")) {
				anyRaw = TRUE
				nSum = nSum + if (!is.null(d$numObs) && is.finite(d$numObs)) d$numObs else nrow(d$observed)
			} else if (!is.null(d$numObs) && is.finite(d$numObs)) {
				nSum = nSum + d$numObs
			}
		}
		sVecStacked = unlist(sList)
		dataIsRaw = anyRaw
		nVal = if (nSum > 0) nSum else 1000
	} else if (!is.null(model$data)) {
		if (.hasSlot(model$data, "observedStats") && !is.null(model$data@observedStats)) {
			os = model$data@observedStats
		} else if (!is.null(model$data$observedStats)) {
			os = model$data$observedStats
		}
		if (!is.null(os)) {
			obsCov = os$cov
			obsMeans = os$means
			obsThresholds = os$thresholds
		}
		if (identical(model$data$type, "raw")) {
			dataIsRaw = TRUE
			if (is.null(obsCov) && !is.null(model$data$observed)) {
				numericData = model$data$observed[, sapply(model$data$observed, is.numeric), drop = FALSE]
				if (ncol(numericData) > 0) {
					obsCov = cov(numericData, use = "pairwise.complete.obs")
				}
			}
			if (is.null(obsMeans) && !is.null(model$data$observed)) {
				numericData = model$data$observed[, sapply(model$data$observed, is.numeric), drop = FALSE]
				if (ncol(numericData) > 0) {
					obsMeans = colMeans(numericData, na.rm = TRUE)
				} else {
					obsMeans = rep(0, length(manifests))
					names(obsMeans) = manifests
				}
			}
		} else {
			# cov / means data, or type "summary" with cov only in observedStats
			if (is.null(obsCov)) {
				obsCov = model$data$observed
			}
			if (is.null(obsMeans)) {
				obsMeans = model$data$means
			}
		}
		nVal = model$data$numObs
		if (is.null(nVal) || is.na(nVal)) {
			nVal = 1000
		}
	}
	if (is.null(sVecStacked) && (is.null(obsCov) || !is.matrix(obsCov))) {
		stop("Could not locate observed covariance matrix for robust WLS fit (need data$observed or observedStats$cov).")
	}
	
	if (!is.null(sVecStacked)) {
		# Multigroup: residual moment vector already named group.moment
		sVec = as.numeric(sVecStacked[commonNames])
		if (length(sVec) != length(commonNames) || any(is.na(sVec) & !is.na(match(commonNames, names(sVecStacked))))) {
			# fall back to positional match when names differ slightly
			sVec = as.numeric(sVecStacked[seq_along(commonNames)])
			if (length(sVec) != length(commonNames)) {
				stop("Multigroup WLS robust fit: could not align stacked residual moments to commonNames.")
			}
		}
	} else {
		if (!is.null(obsMeans)) {
			if (is.matrix(obsMeans) || is.array(obsMeans)) {
				colNames = colnames(obsMeans)
				obsMeans = as.vector(obsMeans)
				names(obsMeans) = colNames
			}
			obsMeans = obsMeans[manifests]
		}
		if (!all(manifests %in% colnames(obsCov)) || !all(manifests %in% rownames(obsCov))) {
			stop("Observed covariance dimnames do not cover model manifests: ", paste(manifests, collapse = ", "))
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
					if (all(parts %in% colnames(obsCov))) {
						sVec[i] = obsCov[parts[1], parts[2]]
					} else {
						sVec[i] = NA_real_
					}
				} else if (length(parts) == 1) {
					if (parts[1] %in% colnames(obsCov)) {
						sVec[i] = obsCov[parts[1], parts[1]]
					} else {
						sVec[i] = NA_real_
					}
				}
			}
		}
	}
	
	dInd = sVec
	for (i in seq_along(commonNames)) {
		name = commonNames[i]
		# Multigroup: strip group prefix before classifying moment type
		nameBare = sub("^[^.]+\\.", "", name)
		isMean = grepl("^mean_", nameBare) || grepl("^one_to_", nameBare) || (nameBare %in% manifests)
		isThresh = grepl("t[0-9]+$", nameBare)
		isVar = FALSE
		if (!isMean && !isThresh) {
			parts = strsplit(nameBare, "[ _]")[[1]]
			parts = parts[!parts %in% c("var", "poly", "cov", "with", "to")]
			if (length(parts) == 2 && parts[1] == parts[2]) {
				isVar = TRUE
			} else if (length(parts) == 1) {
				isVar = TRUE
			} else if (grepl("^var_", nameBare)) {
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

	# Scale WLS matrices to sample size for raw-data models (match OpenMx fit units).
	# Multigroup: scale each diagonal block by that group's N (already in blockdiag order).
	if (is.null(nVal) || is.na(nVal)) {
		nVal = 1000
	}

	if (nGroups == 1L && isTRUE(dataIsRaw)) {
		asymCovAligned = nVal * asymCovAligned
		weightMatAligned = nVal * weightMatAligned
	} else if (nGroups > 1L && isTRUE(dataIsRaw)) {
		dms = xmu_wls_data_models(model)
		pos = 0L
		for (sm in dms) {
			d = sm$data
			ng = if (!is.null(d$numObs) && is.finite(d$numObs)) as.numeric(d$numObs) else nrow(d$observed)
			k = nrow(d$observedStats$asymCov)
			if (is.null(k) || !is.finite(k)) {
				k = nrow(d$observedStats$useWeight)
			}
			if (!is.finite(ng) || ng <= 0 || is.null(k) || !is.finite(k)) next
			idx = (pos + 1L):(pos + k)
			asymCovAligned[idx, idx] = asymCovAligned[idx, idx] * ng
			weightMatAligned[idx, idx] = weightMatAligned[idx, idx] * ng
			pos = pos + k
		}
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

#' Determine if a dataset will need statistics for the means if used in a WLS model.
#'
#' Given either a data.frame or raw `mxData`, this function determines whether [OpenMx::mxFitFunctionWLS()]
#' will generate expectations for means.
#' 
#' All-continuous models processed using the "cumulants" method LACK means, while
#' all continuous processed with allContinuousMethod = "marginals" will HAVE means.
#' 
#' When data are not all continuous, means are modeled and `allContinuousMethod` is ignored.
#'
#' @param data The raw data being used in a [OpenMx::mxFitFunctionWLS()] model.
#' @param allContinuousMethod the method used to process data when all columns are continuous (default = "cumulants")
#' @param verbose Whether or not to report diagnostics.
#' @return - list describing the data.
#' @family xmu internal not for end user
#' @seealso - [OpenMx::mxFitFunctionWLS()], See OpenMx internals for `omxAugmentDataWithWLSSummary` (used in WLS).
#' @export

#' @examples
#'
#' # ====================================
#' # = All continuous, data.frame input =
#' # ====================================
#'
#' tmp =xmu_describe_data_WLS(mtcars, allContinuousMethod= "cumulants", verbose = TRUE)
#' tmp$hasMeans # FALSE - no means with cumulants
#' tmp =xmu_describe_data_WLS(mtcars, allContinuousMethod= "marginals") 
#' tmp$hasMeans # TRUE we get means with marginals
#'
#' # ==========================
#' # = mxData object as input =
#' # ==========================
#' tmp = mxData(mtcars, type="raw")
#' xmu_describe_data_WLS(tmp, allContinuousMethod= "cumulants", verbose = TRUE)$hasMeans # FALSE
#' xmu_describe_data_WLS(tmp, allContinuousMethod= "marginals")$hasMeans  # TRUE
#'
#' # =======================================
#' # = One var is a factor: Means modeled =
#' # =======================================
#' tmp = mtcars
#' tmp$cyl = factor(tmp$cyl)
#'xmu_describe_data_WLS(tmp, allContinuousMethod= "cumulants")$hasMeans # TRUE - always has means
#'xmu_describe_data_WLS(tmp, allContinuousMethod= "marginals")$hasMeans # TRUE
#' 
xmu_describe_data_WLS <- function(data, allContinuousMethod = c("cumulants", "marginals"), verbose=FALSE){
	allContinuousMethod = match.arg(allContinuousMethod)
	if(inherits(data, "data.frame")){
		# all good
	} else if(inherits(data, "MxDataStatic") && data$type == "raw"){
		data = data$observed
	}else{
		message("xmu_describe_data_WLS currently only knows how to process dataframes and mxData of type = 'raw'.\n",
		"You offered up an object of class: ", omxQuotes(class(data)))
	}

	if(all(sapply(data, FUN= is.numeric))){
		if(verbose){ print("all continuous") }

		if(allContinuousMethod == "cumulants"){
			return(list(hasMeans = FALSE))
		} else {
			return(list(hasMeans = TRUE))
		}
	}else{
		# Data with any non-continuous vars have means under WLS
		return(list(hasMeans = TRUE))
	}
}

#' Check if a model is a WLS model
#'
#' @description
#' `xmu_is_wls` is an internal function to check if a model (or any of its submodels) uses WLS/DWLS/ULS.
#'
#' @param model An [OpenMx::mxModel()]
#' @return - Boolean
#' @export
#' @family xmu internal functions

xmu_is_wls <- function(model) {
	if (!umx_is_MxModel(model)) {
		return(FALSE)
	}
	# 1. Check fit function
	if (!is.null(model$fitfunction) && inherits(model$fitfunction, "MxFitFunctionWLS")) {
		return(TRUE)
	}
	# 2. Modern summary WLS: observedStats with useWeight / asymCov (type = "summary")
	if (!is.null(model$data)) {
		if (xmu_is_legacy_acov_data(model$data)) {
			xmu_stop_legacy_acov("xmu_is_wls")
		}
		os = NULL
		if (isS4(model$data) && .hasSlot(model$data, "observedStats")) {
			os = model$data@observedStats
		} else if (!is.null(model$data$observedStats)) {
			os = model$data$observedStats
		}
		if (!is.null(os) && (!is.null(os$useWeight) || !is.null(os$asymCov))) {
			return(TRUE)
		}
	}
	# 3. Check submodels
	for (sub in model$submodels) {
		if (xmu_is_wls(sub)) {
			return(TRUE)
		}
	}
	return(FALSE)
}

xmu_subset_modern_wls_observedStats <- function(data, namesNeeded) {
	os = data$observedStats
	if (is.null(os)) {
		return(data)
	}
	if (!is.null(os$cov) && is.matrix(os$cov)) {
		have = intersect(namesNeeded, colnames(os$cov))
		if (length(have) == 0) {
			have = intersect(namesNeeded, rownames(os$cov))
		}
		if (length(have) > 0) {
			os$cov = os$cov[have, have, drop = FALSE]
			namesNeeded = have
		}
	}
	if (!is.null(data$observed) && is.matrix(data$observed) && all(namesNeeded %in% colnames(data$observed))) {
		data$observed = data$observed[namesNeeded, namesNeeded, drop = FALSE]
	}
	# Use strict OpenMx WLS residual names
	keep_omx = xmu_openmx_residual_names(namesNeeded)
	subset_mat = function(M) {
		if (is.null(M) || !is.matrix(M)) {
			return(M)
		}
		rn = rownames(M)
		if (is.null(rn)) {
			rn = colnames(M)
		}
		if (is.null(rn)) {
			return(M)
		}
		if (all(keep_omx %in% rn)) {
			return(M[keep_omx, keep_omx, drop = FALSE])
		}
		# partial intersection
		keep = rn[rn %in% keep_omx]
		if (length(keep) > 0) {
			return(M[keep, keep, drop = FALSE])
		}
		M
	}
	if (!is.null(os$useWeight)) {
		os$useWeight = subset_mat(os$useWeight)
	}
	if (!is.null(os$asymCov)) {
		os$asymCov = subset_mat(os$asymCov)
	}
	# Refuse deprecated observedStats keys (same footgun as type='acov')
	if (!is.null(os$acov) || !is.null(os$fullWeight)) {
		xmu_stop_legacy_acov("umx (observedStats$acov / $fullWeight)")
	}
	data$observedStats = os
	data
}
