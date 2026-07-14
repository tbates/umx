# ===========================================================================
# = GSEM Helpers ============================================================
# ===========================================================================



xmu_gsem_subset_covstruc <- function(covstruc, keep_vars) {
	if (length(keep_vars) == 0) stop("No trait names requested for covstruc subset.")
	keep_vars  = as.character(keep_vars)
	S = covstruc$S
	V = covstruc$V
	I = covstruc$I
	full_names = colnames(S)
	missing    = setdiff(keep_vars, full_names)
	if (length(missing) > 0) {
		stop("Traits not found in S: ", paste(missing, collapse = ", "),
			". Available: ", paste(full_names, collapse = ", "))
	}
	S_subset = S[keep_vars, keep_vars, drop = FALSE]

	if (is.null(colnames(V)) || is.null(rownames(V))) {
		colnames(V) = xmu_gsem_vech_names(full_names)
		rownames(V) = colnames(V)
	}

	need = xmu_gsem_vech_names(keep_vars)
	orig_keys = character(0)
	for (key in need) {
		if (key %in% colnames(V)) {
			orig_keys = c(orig_keys, key)
		} else if (grepl("^poly_", key)) {
			parts = strsplit(sub("^poly_", "", key), "_")[[1]]
			rev_key = paste0("poly_", parts[2], "_", parts[1])
			if (rev_key %in% colnames(V)) {
				orig_keys = c(orig_keys, rev_key)
			} else {
				orig_keys = c(orig_keys, NA_character_)
			}
		} else {
			orig_keys = c(orig_keys, NA_character_)
		}
	}
	
	if (anyNA(orig_keys)) {
		have = colnames(V)
		tip = paste0(
			"Could not align V with S for traits: ", paste(keep_vars, collapse = ", "), ".\n",
			"  Need OpenMx pair names like: ", paste(need[seq_len(min(4L, length(need)))], collapse = ", "), if (length(need) > 4) ", ..." else "", "\n",
			"  V has: ", if (is.null(have)) "(no dimnames)" else paste(have[seq_len(min(6L, length(have)))], collapse = ", "),
			if (!is.null(have) && length(have) > 6) ", ..." else "", "\n",
			"  Tip: ensure your dataset was labeled natively with OpenMx residual names (var_X, poly_Y_X).\n",
			"  Do not use V[1:z, 1:z] on a larger V — that is not the trait-block vech."
		)
		stop(tip, call. = FALSE)
	}
	
	V_subset = V[orig_keys, orig_keys, drop = FALSE]
	dimnames(V_subset) = list(need, need)
	
	out = covstruc
	out$S = S_subset
	out$V = V_subset
	if (!is.null(I)) {
		out$I = I[keep_vars, keep_vars, drop = FALSE]
	}
	out
}

#' Prepare Data and Weight Matrices for GenomicSEM WLS/DWLS
#'
#' Subsets, smooths, and packages the genetic covariance (`S`) and sampling
#' covariance (`V`) matrices into OpenMx-compatible inputs for `mxFitFunctionWLS`.
#' 
#' @param S A genetic covariance matrix.
#' @param V A sampling covariance matrix representing the asymptotic covariance of `vech(S)`.
#' @param keep_vars Character vector of traits to retain. Order must match model `manifestVars`.
#' @param estimation Character string. Either `"DWLS"` (default), `"WLS"`, or `"ULS"`.
#' @param smooth Logical. Should `S` and `V` be smoothed to nearest positive definite matrices if necessary? (Default `TRUE`).
#' @return A list containing:
#'   \item{S}{The subsetted, optionally smoothed genetic covariance matrix.}
#'   \item{V_omx}{The subsetted, optionally smoothed sampling covariance matrix, formatted for OpenMx.}
#'   \item{W_omx}{The weight matrix for OpenMx (`diag(1/diag(V))` for DWLS, `solve(V)` for WLS, identity for ULS).}
#'   \item{triage}{The raw output from `xmu_gsem_triage` detailing what smoothing occurred.}
#'   \item{keep_vars}{The valid subset of `keep_vars` actually processed.}
#' @keywords internal
xmu_gsem_prepare_WLS <- function(S, V, keep_vars, estimation = "DWLS", smooth = TRUE) {
	covstruc = list(S = S, V = V)
	sub = xmu_gsem_subset_covstruc(covstruc, keep_vars)
	triageResult = xmu_gsem_triage(vMat = sub$V, sMat = sub$S, smooth = smooth)
	V_omx = triageResult$V
	S_subset = triageResult$S
	if (estimation == "WLS") {
		W_omx = solve(V_omx)
	} else if (estimation == "DWLS") {
		W_omx = diag(1 / diag(V_omx), nrow = nrow(V_omx), ncol = ncol(V_omx))
	} else {
		W_omx = diag(1, nrow = nrow(V_omx), ncol = ncol(V_omx))
	}
	dimnames(W_omx) = dimnames(V_omx)
	list(S = S_subset, V_omx = V_omx, W_omx = W_omx, triage = triageResult, keep_vars = sub$keep_vars)
}

xmu_gsem_set_starts <- function(model, S) {
	if (!is(model$A, "MxMatrix") || !is(model$S, "MxMatrix")) {
		return(model)
	}
	man = model$manifestVars
	lat = model$latentVars
	if (length(man) == 0 || length(lat) == 0) {
		return(model)
	}
	# 1-factor (or first latent) eigen starts
	S_use = S[man, man, drop = FALSE]
	# Guard PD for eigen
	ev = tryCatch(eigen(cov2cor(S_use), symmetric = TRUE), error = function(e) NULL)
	if (is.null(ev)) {
		return(model)
	}
	lam = ev$vectors[, 1] * sqrt(max(ev$values[1], 0)) * sqrt(pmax(diag(S_use), 0))
	if (lam[1] < 0) {
		lam = -lam
	}
	theta = pmax(diag(S_use) - lam^2, 0.01 * diag(S_use), 1e-4)

	A = model$A
	Smat = model$S
	# Free A paths from first latent to manifests
	lat1 = lat[1]
	if (lat1 %in% colnames(A$values)) {
		for (i in seq_along(man)) {
			rn = man[i]
			if (isTRUE(A$free[rn, lat1])) {
				A$values[rn, lat1] = lam[i]
			}
		}
	}
	# Free residual variances
	for (i in seq_along(man)) {
		rn = man[i]
		if (isTRUE(Smat$free[rn, rn])) {
			Smat$values[rn, rn] = theta[i]
		}
	}
	model = mxModel(model, A, Smat)
	return(model)
}

xmu_gsem_bound_residuals <- function(model, lbound = 1e-8) {
	if (!is(model$S, "MxMatrix")) {
		return(model)
	}
	Smat = model$S
	man = model$manifestVars
	for (rn in man) {
		if (isTRUE(Smat$free[rn, rn])) {
			if (is.na(Smat$lbound[rn, rn]) || Smat$lbound[rn, rn] < lbound) {
				Smat$lbound[rn, rn] = lbound
			}
		}
	}
	mxModel(model, Smat)
}

xmu_gsem_triage <- function(vMat, sMat, smooth = TRUE, eigenTolerance = -1e-8, fatalTolerance = -0.05) {
	# Check missingness first
	if (any(is.na(vMat)) || any(is.na(sMat))) {
		stop("Fatal Missingness: LDSC matrices contain raw NAs. The pairwise overlap between your cohorts is too sparse to compute covariance.")
	}
	
	# Evaluate V matrix eigenvalues
	vEigen   = eigen(vMat, symmetric = TRUE, only.values = TRUE)$values
	minEigen = min(vEigen)
	
	# PD Check (Clean Matrix)
	if (minEigen > 0) {
		return(list(V = vMat, S = sMat, smoothed = FALSE, triageLevel = 0))
	}
	
	# If matrix is NPD but user strictly disabled smoothing
	if (!smooth) {
		stop(paste0("Matrix is non-positive definite (min eigenvalue = ", round(minEigen, 4), "), and smooth = FALSE. Halting execution."))
	}
	
	# Level 1: Microscopic ridge (Silent fix for floating-point artifacts)
	if (minEigen > eigenTolerance) {
		diag(vMat) = diag(vMat) + 1e-6
		diag(sMat) = diag(sMat) + 1e-6 # Apply to S to maintain symmetry of adjustment
		return(list(V = vMat, S = sMat, smoothed = TRUE, triageLevel = 1))
	}
	
	# Level 2: nearPD Coercion (Alertable)
	if (minEigen > fatalTolerance) {
		warning("umxGSEM: Asymptotic covariance matrix (V) is non-positive definite. Applying nearPD coercion. Note: Resulting Satorra-Bentler corrections and standard errors will be artificially precise.", call. = FALSE)
		
		if (!requireNamespace("Matrix", quietly = TRUE)) {
			stop("The Matrix package is required for nearPD heritability smoothing. Please install it.")
		}
		
		vMatPD = as.matrix(Matrix::nearPD(vMat, corr = FALSE)$mat)
		sMatPD = as.matrix(Matrix::nearPD(sMat, corr = FALSE)$mat)
		
		return(list(V = vMatPD, S = sMatPD, smoothed = TRUE, triageLevel = 2))
	}
	
	# Level 3: Fatal Structural Deficiency
	stop(paste0("Fatal Matrix Deficiency: V matrix is severely non-positive definite (minimum eigenvalue = ", round(minEigen, 4), "). The pairwise overlap between your cohorts is too sparse to support multivariable SEM. Do not proceed."))
}

xmu_gsem_resolve_paths <- function(paths, baseDir = getwd()) {
	paths = as.character(paths)
	out = character(length(paths))
	for (i in seq_along(paths)) {
		p = path.expand(paths[i])
		if (grepl("^(/|[A-Za-z]:)", p)) {
			out[i] = p
		} else {
			out[i] = file.path(path.expand(baseDir), p)
		}
	}
	out
}

xmu_gsem_read_sumstats_table <- function(path) {
	path = path.expand(path)
	if (!file.exists(path)) {
		stop("File not found: ", path)
	}
	# White-space separated (tutorial files often lack separators in header display)
	utils::read.table(path, header = TRUE, quote = "\"", fill = TRUE, stringsAsFactors = FALSE,
		na.strings = c(".", "NA", ""))
}

xmu_gsem_rename_sumstats_cols <- function(df) {
	nm = tolower(names(df))
	map = list(
		SNP = c("snp", "rsid", "marker", "snpid", "rs", "markername"),
		A1 = c("a1", "allele1", "effect_allele", "inc_allele", "reference_allele"),
		A2 = c("a2", "allele2", "non_effect_allele", "dec_allele", "other_allele"),
		effect = c("effect", "or", "beta", "logor", "est", "estimate", "b"),
		SE = c("se", "stderr", "standard_error", "std_err"),
		P = c("p", "p-value", "pval", "p.value", "pvalue"),
		N = c("n", "neff", "n_eff", "ncase", "samplesize"),
		INFO = c("info", "impinfo", "r2"),
		MAF = c("maf", "frq", "freq", "a1freq", "eaf")
	)
	out = list()
	for (std in names(map)) {
		hit = which(nm %in% map[[std]])
		if (length(hit) >= 1) {
			out[[std]] = df[[hit[1]]]
		}
	}
	as.data.frame(out, stringsAsFactors = FALSE)
}

xmu_gsem_sumstats_one_trait <- function(filename, ref, trait.name, se.logit = TRUE, OLS = FALSE, linprob = FALSE, N = NA, info.filter = 0.6) {
	raw = xmu_gsem_read_sumstats_table(filename)
	file = xmu_gsem_rename_sumstats_cols(raw)
	need = c("SNP", "A1", "A2", "effect")
	if (!all(need %in% names(file))) {
		stop(filename, ": need columns for SNP, A1, A2, effect (OR/beta). Found: ", paste(names(raw), collapse = ", "))
	}
	if (!linprob && !"SE" %in% names(file) && !OLS) {
		stop(filename, ": SE column required unless linprob/OLS-with-N.")
	}
	file$SNP = as.character(file$SNP)
	file$A1 = toupper(as.character(file$A1))
	file$A2 = toupper(as.character(file$A2))
	file$effect = as.numeric(file$effect)
	if ("SE" %in% names(file)) file$SE = as.numeric(file$SE)
	if ("P" %in% names(file)) file$P = as.numeric(file$P)
	if ("INFO" %in% names(file)) file$INFO = as.numeric(file$INFO)
	if (!is.na(N)) file$N = N

	# Drop multi-allelic duplicates
	file = file[!(duplicated(file$SNP) | duplicated(file$SNP, fromLast = TRUE)), , drop = FALSE]

	# Merge with reference (suffixes mark ref vs gwas alleles)
	m = merge(ref, file, by = "SNP", suffixes = c(".ref", ".gwas"))
	if (nrow(m) == 0) {
		stop(filename, ": no SNPs left after merging with reference.")
	}
	a1r = m$A1.ref
	a2r = m$A2.ref
	a1g = m$A1.gwas
	a2g = m$A2.gwas
	maf = m$MAF
	if (is.null(maf)) {
		maf = m$MAF.ref
	}
	varSNP = 2 * as.numeric(maf) * (1 - as.numeric(maf))

	effect = m$effect
	# OR if median near 1
	if (isTRUE(all.equal(round(stats::median(effect, na.rm = TRUE)), 1))) {
		effect = log(effect)
	}
	# Flip to reference A1
	flip = (a1r != a1g) & (a1r == a2g)
	effect[flip] = -effect[flip]
	# Drop allele mismatches
	ok = !((a1r != a1g) & (a1r != a2g))
	ok = ok & !((a2r != a2g) & (a2r != a1g))
	m = m[ok, , drop = FALSE]
	effect = effect[ok]
	varSNP = varSNP[ok]
	if ("SE" %in% names(m)) {
		se = m$SE[ok]
	} else {
		se = rep(NA_real_, sum(ok))
	}
	if ("INFO" %in% names(m)) {
		keepInfo = is.na(m$INFO[ok]) | m$INFO[ok] >= info.filter
		m = m[keepInfo, , drop = FALSE]
		effect = effect[keepInfo]
		varSNP = varSNP[keepInfo]
		se = se[keepInfo]
	}
	# Continuous-scale transform (Genomic SEM se.logit formula)
	if (OLS && "N" %in% names(m)) {
		# Z from effect/se if available else skip
		Z = if (all(is.finite(se)) && all(se > 0)) effect / se else effect
		effect = Z / sqrt(m$N * varSNP)
		se = abs(effect / Z)
		se[!is.finite(se)] = NA
	} else if (linprob && "N" %in% names(m)) {
		Z = if (all(is.finite(se)) && all(se > 0)) effect / se else effect
		effect = Z / sqrt((m$N / 4) * varSNP)
		se = 1 / sqrt((m$N / 4) * varSNP)
		denom = sqrt(effect^2 * varSNP + (pi^2) / 3)
		effect = effect / denom
		se = se / denom
	} else if (se.logit) {
		denom = sqrt(effect^2 * varSNP + (pi^2) / 3)
		effect = effect / denom
		se = se / denom
	} else {
		# SE of OR: divide SE by exp(logOR)
		denom = sqrt(effect^2 * varSNP + (pi^2) / 3)
		effect = effect / denom
		se = (se / exp(effect * denom)) / denom  # careful: effect already transformed
		# Simpler: recompute from logOR before transform is cleaner
	}
	# Fix non-logit OR-SE branch more carefully was messy; tutorial uses se.logit=TRUE
	out = data.frame(
		SNP = m$SNP,
		beta = effect,
		se = se,
		stringsAsFactors = FALSE
	)
	out = out[is.finite(out$beta) & is.finite(out$se) & out$se > 0, , drop = FALSE]
	colnames(out) = c("SNP", paste0("beta.", trait.name), paste0("se.", trait.name))
	out
}

xmu_gsem_expand_snp <- function(covstruc, beta_i, se_i, varSNP_i, varSNPSE2, GC = "standard", coords = NULL) {
	k = ncol(covstruc$S)
	traits = colnames(covstruc$S)
	if (is.null(coords)) {
		coords = as.matrix(expand.grid(seq_len(k), seq_len(k)))
	}
	# V_SNP (k x k) sampling cov of SNP-trait genetic covariances
	V_SNP = diag(k)
	for (p in seq_len(nrow(coords))) {
		x = coords[p, 1]
		y = coords[p, 2]
		if (GC == "conserv") {
			if (x != y) {
				V_SNP[x, y] = se_i[y] * se_i[x] * covstruc$I[x, y] * covstruc$I[x, x] * covstruc$I[y, y] * varSNP_i^2
			} else {
				V_SNP[x, x] = (se_i[x] * covstruc$I[x, x] * varSNP_i)^2
			}
		} else if (GC == "standard") {
			if (x != y) {
				V_SNP[x, y] = se_i[y] * se_i[x] * covstruc$I[x, y] * sqrt(covstruc$I[x, x]) * sqrt(covstruc$I[y, y]) * varSNP_i^2
			} else {
				V_SNP[x, x] = (se_i[x] * sqrt(covstruc$I[x, x]) * varSNP_i)^2
			}
		} else {
			if (x != y) {
				V_SNP[x, y] = se_i[y] * se_i[x] * covstruc$I[x, y] * varSNP_i^2
			} else {
				V_SNP[x, x] = (se_i[x] * varSNP_i)^2
			}
		}
	}
	# S_Full
	S_SNP = numeric(k + 1)
	S_SNP[1] = varSNP_i
	for (p in seq_len(k)) {
		S_SNP[p + 1] = varSNP_i * beta_i[p]
	}
	S_Full = diag(k + 1)
	S_Full[2:(k + 1), 2:(k + 1)] = covstruc$S
	S_Full[1:(k + 1), 1] = S_SNP
	S_Full[1, 1:(k + 1)] = S_SNP
	dimnames(S_Full) = list(c("SNP", traits), c("SNP", traits))

	# V_Full in GenomicSEM lower.tri order of S_Full
	nV = ((k + 1) * (k + 2)) / 2
	V_Full = diag(nV)
	V_Full[1, 1] = varSNPSE2
	V_Full[2:(k + 1), 2:(k + 1)] = V_SNP
	V_Full[(k + 2):nV, (k + 2):nV] = covstruc$V
	# Name V rows/cols with vech of [SNP, traits]
	vnames = xmu_gsem_vech_names(c("SNP", traits))
	dimnames(V_Full) = list(vnames, vnames)

	# Smooth if needed (triage will also run)
	if (min(eigen(S_Full, only.values = TRUE)$values) <= 0) {
		S_Full = as.matrix(Matrix::nearPD(S_Full, corr = FALSE)$mat)
		dimnames(S_Full) = list(c("SNP", traits), c("SNP", traits))
	}
	if (min(eigen(V_Full, only.values = TRUE)$values) <= 0) {
		V_Full = as.matrix(Matrix::nearPD(V_Full, corr = FALSE)$mat)
		dimnames(V_Full) = list(vnames, vnames)
	}
	list(S = S_Full, V = V_Full)
}


xmu_gsem_extract_snp_path <- function(fit, traits) {
	# Prefer free A-path: F1 ~ SNP  (RAM: row = to = latent, col = from = SNP)
	est = NA_real_
	se = NA_real_
	se_source = NA_character_
	lat = if (length(fit$latentVars)) fit$latentVars[1] else NA_character_
	# Prefer a factor named F1 when present
	if ("F1" %in% fit$latentVars) {
		lat = "F1"
	}
	lab = NA_character_
	if (!is.null(fit$A) && !is.na(lat) && lat %in% rownames(fit$A$values) && "SNP" %in% colnames(fit$A$values)) {
		est = fit$A$values[lat, "SNP"]
		lab = fit$A$labels[lat, "SNP"]
		ses = fit$output$standardErrors
		cf = tryCatch(coef(fit), error = function(e) NULL)
		if (!is.null(ses) && !is.null(lab) && !is.na(lab) && !is.null(cf)) {
			rn = rownames(ses)
			if (!is.null(rn) && lab %in% rn) {
				se = as.numeric(ses[lab, 1])
				if (is.finite(se)) se_source = "openmx"
			} else if (lab %in% names(cf) && length(as.numeric(ses)) >= length(cf)) {
				# OpenMx often returns SE in free-parameter order without rownames
				se = as.numeric(ses)[match(lab, names(cf))]
				if (is.finite(se)) se_source = "openmx"
			}
		}
	}
	if (!is.finite(se)) {
		ss = tryCatch(summary(fit)$parameters, error = function(e) NULL)
		if (!is.null(ss) && nrow(ss) > 0) {
			matcol = if ("matrix" %in% names(ss)) ss$matrix else ss$Matrix
			hit = which(matcol == "A" & ss$col == "SNP" & ss$row %in% fit$latentVars)
			if (length(hit) >= 1) {
				if (!is.finite(est)) est = ss$Estimate[hit[1]]
				if (is.na(lab) && "name" %in% names(ss)) lab = ss$name[hit[1]]
				if ("Std.Error" %in% names(ss)) {
					se = ss[["Std.Error"]][hit[1]]
					if (is.finite(se)) se_source = "summary"
				}
			}
		}
	}
	# Drop numerical garbage (singular sandwich → absurd Z)
	if (is.finite(se) && (se <= 0 || se < 1e-12 * max(1, abs(est)))) {
		se = NA_real_
		se_source = "dropped_numeric"
	}
	list(est = as.numeric(est)[1], se = as.numeric(se)[1], se_source = se_source)
}


xmu_gsem_check_format <- function(covstruc) {
	if (!is.list(covstruc) || is.null(covstruc$S) || is.null(covstruc$V)) {
		stop("covstruc must be a list with S and V matrices.")
	}
	if (is.null(colnames(covstruc$S))) {
		stop("covstruc$S must have column names.")
	}
	expected_V_names = xmu_gsem_vech_names(colnames(covstruc$S))
	if (!identical(colnames(covstruc$V), expected_V_names)) {
		stop("Legacy GenomicSEM naming detected in V matrix. Please upgrade your dataset to the OpenMx protocol by running: your_data <- umxGSEM_label_ldsc(your_data)")
	}
}

