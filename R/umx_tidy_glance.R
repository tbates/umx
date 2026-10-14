#' Tidy table of parameter estimates for an OpenMx model
#'
#' @description
#' Returns the free parameters of an [OpenMx::mxModel()] as a data.frame, one row
#' per parameter. Optionally returns standardized RAM paths with Wald CIs, and/or
#' cluster-robust (sandwich) standard errors.
#'
#' @details
#' With `std = FALSE` the table has `term`, `est`, `se`, `matrix`, `row`, and `col`.
#'
#' With `std = TRUE` estimates are standardized via [OpenMx::mxStandardizeRAMpaths()],
#' and the table has `term`, `std.est`, `std.se`, and `ci`, where `ci` is the
#' formatted Wald interval `estimate +/- z(1 - (1 - level)/2) * SE`. If
#' standardized SEs cannot be computed, a warning is given and SEs are omitted.
#'
#' When `cluster` names a column of the raw data, standard errors are replaced
#' by a cluster-robust sandwich (CR0 bread from [vcov()], meat from cluster sums of
#' row gradients via [OpenMx::imxRowGradients()]; with `small = TRUE` the meat
#' gets a CR1 `G/(G - 1)` correction, where G is the number of clusters). The
#' robust covariance is also passed to [OpenMx::mxStandardizeRAMpaths()] when `std = TRUE`.
#'
#' @param x An [OpenMx::mxModel()] that has been run.
#' @param std Whether to return standardized estimates with CIs (default FALSE).
#' @param digits Decimals for the formatted `ci` string (default 2).
#' @param level Confidence level for CIs (default .95).
#' @param cluster Optional name of a data column identifying clusters (e.g. family ID) for robust SEs.
#' @param small Whether to apply the CR1 small-sample correction to cluster-robust SEs (default TRUE).
#' @param ... Ignored.
#' @return A data.frame of parameter estimates.
#' @export
#' @family Reporting functions
#' @seealso - [umx_glance()], [umxSummary()], [OpenMx::mxStandardizeRAMpaths()]
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' umx_tidy(m1)
#' umx_tidy(m1, std = TRUE)
#' }
umx_tidy <- function(x, std = FALSE, digits = 2, level = 0.95, cluster = NULL, small = TRUE, ...) {
	if(!umx_is_MxModel(x)) stop("umx_tidy needs an MxModel. You gave me a ", class(x)[1])
	if(!is.logical(std) || length(std) != 1L) stop("`std` must be TRUE or FALSE")

	# Cluster-robust covariance if requested
	Vcr = NULL
	if(!is.null(cluster)) {
		obs = x@data@observed
		if(!(cluster %in% colnames(obs))) stop(sprintf("Cluster '%s' not found.", cluster))

		# Ensure vcov exists
		if(is.null(tryCatch(vcov(x), error = function(e) NULL))) {
			x = mxModel(x, mxComputeSequence(list(
				mxComputeNumericDeriv(checkGradient = FALSE, hessian = TRUE),
				mxComputeStandardError()
			)))
			x = mxRun(x, silent = TRUE)
		}

		# Free-parameter names in the model (canonical order)
		pnames = names(omxGetParameters(x))

		# Row gradients (may have no colnames: then they are in free-parameter order)
		G = imxRowGradients(x, robustSE = TRUE)
		stopifnot(nrow(G) == nrow(obs))
		if(is.null(colnames(G))) {
			colnames(G) = pnames
		} else {
			G = G[, pnames, drop = FALSE]
		}
		p = ncol(G)

		# Bread: naive covariance in the same order
		B = vcov(x)
		if(!is.null(rownames(B)) && !is.null(colnames(B))) {
			B = B[pnames, pnames, drop = FALSE]
		}
		stopifnot(nrow(B) == p, ncol(B) == p)

		# Meat: cluster sums of row gradients (on -2 log L scale, hence * 0.25)
		S_by_cluster = rowsum(G, obs[[cluster]])
		Gc = nrow(S_by_cluster)
		M  = 0.25 * crossprod(S_by_cluster)
		# CR1 small-sample correction (cluster count)
		if(small && Gc > 1) M = M * (Gc / (Gc - 1))

		Vcr = B %*% M %*% B
		dimnames(Vcr) = list(pnames, pnames)
	}

	# ---------- Unstandardized ----------
	if(!isTRUE(std)) {
		pars = summary(x)$parameters
		out = data.frame(term = pars$name, est = pars$Estimate, se = pars$Std.Error,
			matrix = pars$matrix, row = pars$row, col = pars$col, stringsAsFactors = FALSE)
		if(!is.null(Vcr)) {
			se_cr = sqrt(diag(Vcr))[out$term]
			out$se = ifelse(!is.na(se_cr), se_cr, out$se)
		}
		rownames(out) = NULL
		return(out)
	}

	# ---------- Standardized ----------
	empty = data.frame(term = character(), std.est = numeric(), std.se = numeric(), ci = character())
	std_raw = tryCatch({
		if(!is.null(Vcr)) {
			mxStandardizeRAMpaths(x, SE = TRUE, cov = Vcr)
		} else {
			mxStandardizeRAMpaths(x, SE = TRUE)
		}
	}, error = function(e) NULL)

	if(is.null(std_raw) || (is.data.frame(std_raw) && !nrow(std_raw))) {
		warning("mxStandardizeRAMpaths(SE=TRUE) failed; falling back to SE=FALSE.")
		std_raw = tryCatch(mxStandardizeRAMpaths(x, SE = FALSE), error = function(e) NULL)
		if(is.null(std_raw)) return(empty)
	}

	# Multigroup models return a list of data.frames: stack them
	std_df = if(is.data.frame(std_raw)) {
		std_raw
	} else if(is.list(std_raw)) {
		do.call(rbind, lapply(std_raw, function(d) d[, intersect(c("label", "row", "col", "Std.Value", "Std.SE"), names(d)), drop = FALSE]))
	} else {
		NULL
	}
	if(is.null(std_df) || !nrow(std_df)) return(empty)

	z_mult  = stats::qnorm(1 - (1 - level)/2)
	std.est = suppressWarnings(as.numeric(std_df$Std.Value))
	std.se  = if(is.null(std_df$Std.SE)) rep(NA_real_, nrow(std_df)) else suppressWarnings(as.numeric(std_df$Std.SE))
	label   = as.character(std_df$label)
	term    = ifelse(!is.na(label) & nzchar(label), label, paste0(std_df$row, "__", std_df$col))
	fmt     = function(v) formatC(round(v, digits), format = "f", digits = digits)
	ci      = ifelse(is.finite(std.se),
		paste0(fmt(std.est), " [", fmt(std.est - z_mult * std.se), ", ", fmt(std.est + z_mult * std.se), "]"),
		NA_character_)

	out = data.frame(term = term, std.est = std.est, std.se = std.se, ci = ci, stringsAsFactors = FALSE)
	out = out[order(out$term), , drop = FALSE]
	rownames(out) = NULL
	out
}

#' One-row summary of fit statistics for an OpenMx model
#'
#' @description
#' Returns a one-row data.frame of fit statistics (AIC, BIC, CFI, TLI, chi-square,
#' -2LL, df, N, effective N, and RMSEA with CI) for an [OpenMx::mxModel()].
#'
#' @details
#' [OpenMx::mxRefModels()] drops definition variables, so a model whose means depend
#' on them can beat its own "saturated" model (chi-square < 0, CFI > 1). When the
#' model has definition variables, `umx_glance` instead builds reference models that
#' keep them (see [xmu_ref_models_defvars()]).
#'
#' `eff_nobs` counts the rows that actually enter the likelihood (see [xmu_eff_nobs()]).
#'
#' @param x An [OpenMx::mxModel()] that has been run.
#' @param ... Ignored.
#' @return A one-row data.frame with `aic`, `bic`, `cfi`, `tli`, `chisq`, `chisq.df`,
#' `chisq.p`, `minus2LL`, `df`, `nobs`, `eff_nobs`, `rmsea`, `rmsea.lower`, `rmsea.upper`.
#' @export
#' @family Reporting functions
#' @seealso - [umx_tidy()], [umxSummary()], [OpenMx::mxRefModels()]
#' @examples
#' \dontrun{
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor,
#' 	umxPath("G", to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = "G")
#' )
#' umx_glance(m1)
#' }
umx_glance <- function(x, ...) {
	if(!umx_is_MxModel(x)) stop("umx_glance needs an MxModel. You gave me a ", class(x)[1])
	has_def = isTRUE(tryCatch(imxHasDefinitionVariable(x), error = function(e) FALSE))
	refs = if(has_def) xmu_ref_models_defvars(x) else mxRefModels(x, run = TRUE)
	s = suppressWarnings(summary(x, refModels = refs))
	na_if_null = function(v) if(is.null(v) || !length(v)) NA else v
	data.frame(
		aic         = na_if_null(s$AIC.Mx),
		bic         = na_if_null(s$BIC.Mx),
		cfi         = na_if_null(s$CFI),
		tli         = na_if_null(s$TLI),
		chisq       = na_if_null(s$Chi),
		chisq.df    = na_if_null(s$ChiDoF),
		chisq.p     = na_if_null(s$p),
		minus2LL    = na_if_null(s$Minus2LogLikelihood),
		df          = na_if_null(s$degreesOfFreedom),
		nobs        = na_if_null(s$numObs),
		eff_nobs    = xmu_eff_nobs(x),
		rmsea       = na_if_null(s$RMSEA),
		rmsea.lower = if(!is.null(s$RMSEACI)) round(s$RMSEACI[1], getOption("digits")) else NA,
		rmsea.upper = if(!is.null(s$RMSEACI)) round(s$RMSEACI[2], getOption("digits")) else NA,
		row.names   = NULL
	)
}

#' Reference models that keep the definition variables
#'
#' @description
#' [OpenMx::mxRefModels()] drops definition variables, so a model whose means
#' depend on them can beat its own "saturated" model (chi-square < 0, CFI > 1).
#' Here, per group, the saturated model has a free covariance and the independence
#' model a diagonal one; in both, every manifest mean is regressed on all definition
#' variables used in that group.
#'
#' Definition variables that act on the covariance (e.g. continuous-time intervals)
#' are only carried in the means, so the fitted model is not strictly nested in the
#' saturated reference; chi-square/CFI are then descriptive.
#'
#' @param x An [OpenMx::mxModel()] with raw data and definition variables.
#' @return A list with run `Saturated` and `Independence` models, suitable for `summary(x, refModels = )`.
#' @export
#' @family xmu internal not for end user
#' @seealso - [umx_glance()], [OpenMx::mxRefModels()]
xmu_ref_models_defvars <- function(x) {
	groups = xmu_model_groups(x)
	one_group = function(g, type) {
		m    = if(identical(g, x@name)) x else x[[g]]
		dat  = m$data$observed
		mans = xmu_model_manifests(m)
		labs = unlist(lapply(c(m@matrices, if(identical(g, x@name)) list() else x@matrices),
			function(M) as.vector(M$labels)))
		defs = unique(sub("^data\\.", "", grep("^data\\.", labs, value = TRUE)))
		p = length(mans)
		Mint = mxMatrix("Full", 1, p, free = TRUE, values = colMeans(dat[, mans, drop = FALSE], na.rm = TRUE),
			labels = paste0(g, "_int_", mans), name = "Mint")
		if(type == "Saturated") {
			S = mxMatrix("Symm", p, p, free = TRUE, values = diag(p) * 0.5, name = "S", dimnames = list(mans, mans))
		} else {
			S = mxMatrix("Diag", p, p, free = TRUE, values = 0.5, name = "S", dimnames = list(mans, mans))
		}
		expN = mxExpectationNormal(covariance = "S", means = "Mu", dimnames = mans)
		if(length(defs)) {
			B  = mxMatrix("Full", length(defs), p, free = TRUE, values = 0,
				labels = outer(defs, mans, function(d, v) paste0(g, "_b_", d, "_", v)), name = "B")
			D  = mxMatrix("Full", 1, length(defs), free = FALSE, labels = paste0("data.", defs), name = "D")
			Mu = mxAlgebra(Mint + D %*% B, name = "Mu", dimnames = list(NULL, mans))
			mxModel(g, mxData(dat, type = "raw"), Mint, B, D, Mu, S, expN, mxFitFunctionML())
		} else {
			Mu = mxAlgebra(Mint, name = "Mu", dimnames = list(NULL, mans))
			mxModel(g, mxData(dat, type = "raw"), Mint, Mu, S, expN, mxFitFunctionML())
		}
	}
	build_ref = function(type) {
		if(length(groups) == 1 && identical(groups, x@name)) {
			return(mxRun(mxModel(one_group(groups, type), name = paste0(type, "Cond")), silent = TRUE))
		}
		mxRun(mxModel(paste0(type, "Cond"), lapply(groups, one_group, type = type),
			mxFitFunctionMultigroup(groups)), silent = TRUE)
	}
	list(Saturated = build_ref("Saturated"), Independence = build_ref("Independence"))
}

#' Effective N of a model: rows that enter the likelihood
#'
#' @description
#' Counts rows with at least one manifest observed and no missing definition
#' variable, summed over groups. Rows failing either condition do not contribute
#' to the likelihood, so this can be smaller than `summary(x)$numObs`.
#'
#' @param x An [OpenMx::mxModel()] with raw data.
#' @return Integer count of rows entering the likelihood (NA for non-raw data).
#' @export
#' @family xmu internal not for end user
#' @seealso - [umx_glance()]
xmu_eff_nobs <- function(x) {
	groups = xmu_model_groups(x)
	sum(vapply(groups, function(g) {
		m   = if(identical(g, x@name)) x else x[[g]]
		if(is.null(m$data) || m$data$type != "raw") return(NA_real_)
		dat  = m$data$observed
		mans = xmu_model_manifests(m)
		labs = unlist(lapply(c(m@matrices, x@matrices), function(M) as.vector(M$labels)))
		defs = intersect(unique(sub("^data\\.", "", grep("^data\\.", labs, value = TRUE))), names(dat))
		keep = rowSums(!is.na(dat[, mans, drop = FALSE])) > 0
		if(length(defs)) keep = keep & stats::complete.cases(dat[, defs, drop = FALSE])
		sum(keep)
	}, numeric(1)))
}

# Names of the groups (submodels) contributing to the fit: the model itself unless multigroup
xmu_model_groups <- function(x) {
	if(inherits(x$fitfunction, "MxFitFunctionMultigroup")) {
		sub("\\.fitfunction$", "", x$fitfunction$groups)
	} else {
		x@name
	}
}

# Manifest names of one group: expectation dims, or manifestVars for RAM models
xmu_model_manifests <- function(m) {
	mans = m$expectation$dims
	if(is.null(mans) || all(is.na(mans))) mans = m@manifestVars
	mans
}
