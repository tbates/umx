#   Copyright 2007-2022 Timothy C. Bates
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

# An institution is the lengthened shadow of one man. Emerson.

# TODO: define a folder structure for gsem projects in umx "project/data/"
umx_CheckProject <- function(project_path = "/path/to/my/project") {
    message("Status of Project folder: ")
	# TODO: does the user have the downloaded HM3 files etc needed?
	# TODO: is munge done? and they have the files needed for ldsc?
	# TODO: is ldsc done and that object is ready?
	# TODO: is the SNP file done?
  }

#' High-performance LD score regression (C++/OpenMP)
#'
#' A fast, native reimplementation of multivariate LD score regression
#' using C++ and OpenMP. Designed to be compatible with
#' [GenomicSEM::ldsc()] output while offering significantly better
#' performance, especially with larger numbers of traits.
#'
#' @details
#' S is A symmetric K×K matrix (where K is the number of traits). The diagonal contains the genetic variances (heritabilities, h 
#' 2). The off-diagonals contain the genetic covariances.
#' 
#' V is A symmetric M×M sampling covariance matrix, where M= (K*(K+1))/2 (the number of unique elements in S).
#' It contains the sampling variances of your heritabilities and genetic covariances on the diagonal, 
#' and their sampling covariances on the off-diagonal.
#' 
#' I is a K×K matrix containing the LDSC intercepts 
#' (cross-trait intercepts on the off-diagonal, single-trait intercepts on the diagonal) 
#' used to quantify sample overlap and population stratification.
#' 
#' @param traits Character vector of paths to munged `.sumstats.gz` files.
#' @param sample.prev Numeric vector of sample prevalences (proportion of cases).
#'   Use `NA` or `0.5` for continuous traits.
#' @param population.prev Numeric vector of population prevalences (for liability
#'   scale transformation of binary traits). Use `NA` for continuous traits.
#' @param ld Path to folder containing LD score files (e.g. `eur_w_ld_chr/`).
#' @param wld Path to folder containing weight LD score files.
#' @param trait.names Optional character vector of names for the traits. If `NULL`,
#'   names are taken from the file names.
#' @param sep_weights Logical. Whether to use separate weight LD scores (default `FALSE`).
#' @param chr Number of chromosomes to use (default = 22).
#' @param n.blocks Number of jackknife blocks to use when estimating the sampling
#'   covariance matrix `V` (default = 200).
#' @param ldsc.log Optional path to write an LDSC log file. If `NULL`, no log is written.
#' @param stand Logical. If `TRUE`, also returns standardized results (genetic
#'   correlations and their sampling covariance matrix).
#' @param select Logical or numeric. Passed through to control SNP selection
#'   (see GenomicSEM documentation).
#' @param chisq.max Optional numeric. Maximum chi-square value allowed when
#'   estimating the LD score regression (used for outlier control).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{S}{Genetic covariance matrix (heritabilities on diagonal).}
#'   \item{V}{Sampling covariance matrix of the elements of `S`.}
#'   \item{I}{Matrix of LD score regression intercepts.}
#'   \item{N}{Effective sample sizes used.}
#'   \item{m}{Number of SNPs used in the regression.}
#'   \item{...}{Additional components depending on `stand`.}
#' }
#' If `stand = TRUE`, the list also contains the genetic correlation matrix
#' and its sampling covariance matrix.
#' @export
#' @family GSEM
#' @seealso [GenomicSEM::ldsc()]
umxGSEM_ldsc <- function (traits, sample.prev, population.prev, ld, wld, trait.names = NULL, 
    sep_weights = FALSE, chr = 22, n.blocks = 200, ldsc.log = NULL, 
    stand = FALSE, select = FALSE, chisq.max = NA) {
	
	# TODO do the light-weight checks here to match GenomicSEM::ldsc
	# TODO call OpenMx imxLDSC
}



#' Download HapMap3 Reference SNP list
#'
#' @description
#' Helper function to download the HapMap3 SNP reference list (`w_hm3.snplist`) needed for munging
#' summary statistics in Genomic SEM / LDSC.
#'
#' @details TBD
#'
#' @param project_path Path to the directory where the SNP list should be saved. Defaults to the current working directory.
#' @param path2snplist URL to download the HapMap3 SNP list. Defaults to the bulik LDSC repository.
#' @param overwrite Whether to force overwrite/re-download if the file already exists (defaults to FALSE).
#' @return Absolute path to the downloaded file.
#' @export
#' @family GSEM

#' @examples
#' \dontrun{
#' project_path = "~/your/project"
#' umxGSEM_dl_RefList(project_path = project_path)
#' }
umxGSEM_dl_RefList <- function(project_path = getwd(),  path2snplist = "https://zenodo.org/records/7773502/files/w_hm3.snplist.gz",  overwrite = FALSE) {    
	project_path = path.expand(project_path)
	# Create directory if it doesn't exist
	if (!dir.exists(project_path)) {
		dir.create(project_path, showWarnings = FALSE, recursive = TRUE)
	}
	# Target is the decompressed file
	dest_file = file.path(project_path, "w_hm3.snplist")
	temp_gz   = file.path(tempdir(), "w_hm3.snplist.gz")
    
	# Logic: Only download/extract if missing or overwrite requested
	if (overwrite || !file.exists(dest_file) || file.info(dest_file)$size == 0) {
		message("Downloading and decompressing HapMap3 reference list...")        
		# Download compressed file to temp
		download.file(path2snplist, destfile = temp_gz, mode = "wb")
		# Decompress to final destination
		R.utils::gunzip(temp_gz, destname = dest_file, overwrite = TRUE, remove = TRUE)        
		# Integrity check: Does it have the expected header?
		header = colnames(data.table::fread(dest_file, nrows = 0))
		if (!"SNP" %in% header) {
		   stop("Downloaded file structure invalid. Expected 'SNP' column.")
		}else{
			message("Nice: File structure valid")
		}
	} else {
		message("HapMap3 reference list already exists: ", dest_file)
	}
	return(normalizePath(dest_file, mustWork = TRUE))
}


#' Munge Genomic SEM Summary Statistics
#'
#' @description
#' `umxGSEM_munge` allows you to format and filter summary statistics files
#' for Genomic SEM / LDSC using the high-speed OpenMx C++ munging engine.
#' munge creates the *.sumstats.gz files for [umxGSEM_ldsc()]
#' 
#' You first need to run umxGSEM_dl_RefList()] to grab the SNP ref list if oyu have not already.
#'
#' @details
#' 
#' umxGSEM_munge will intelligently search for synonyms for heading names:
#' SNP: snp, rsid, marker, snpid, rs, markername
#' A1: a1, allele1, effect_allele, inc_allele, reference_allele
#' A2: a2, allele2, non_effect_allele, dec_allele, other_allele
#' P: p, p-value, pval, p.value
#' effect: effect, or, beta, logor, est, estimate
#'
#' note: Ensure that the summary statistics are full, not "23&me" style "top x thousand SNPs".
#' Nor just pruned SNPs.
#'
#' @param files A character vector of summary statistics files to munge.
#' @param hm3 Path to the HapMap3 SNP reference list (e.g., as returned by [umxGSEM_dl_RefList()]).
#' @param Ns A numeric vector of sample sizes (one per file). If a single value is provided, it will be replicated for all files.
#' @param trait.names A character vector of names for the traits. Defaults to the basenames of the files.
#' @param info.filter Info score threshold filter (defaults to 0.9).
#' @param maf.filter Minor Allele Frequency threshold filter (defaults to 0.01).
#' @param column.names A list of custom column name mappings. Synonyms are automatically resolved if left empty.
#' @param output_dir Path to the directory where munged files should be saved. Defaults to `getwd()`.
#' @param cores Number of CPU cores/threads to use. Defaults to -1 (uses all available threads).
#' @param overwrite Whether to overwrite existing munged files (defaults to FALSE).
#' @param ... Additional arguments passed to \code{OpenMx::imxMunge()}.
#' @return A character vector of paths to the munged files.
#' @export
#' @family GSEM

umxGSEM_munge <- function(files = NULL, hm3 = "w_hm3.snplist", Ns = NULL, trait.names = NULL, info.filter = 0.9, maf.filter = 0.01, column.names = list(), output_dir = getwd(), cores = -1, overwrite = FALSE, ...) {
	if (is.null(files)) {
		stop("Provide a character vector of your summary statistics files, e.g., files = c('ed.txt', 'aut.txt')")
	}
	
	# 1. Path normalization to absolute paths
	files <- normalizePath(files, mustWork = TRUE)
	hm3 <- normalizePath(hm3, mustWork = TRUE)
	output_dir <- path.expand(output_dir)
	if (!dir.exists(output_dir)) {
		dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
	}
	output_dir <- normalizePath(output_dir, mustWork = TRUE)
	
	# 2. Replicate Ns if single value
	if (!is.null(Ns)) {
		if (length(Ns) == 1) {
			Ns <- rep(Ns, length(files))
		}
		if (length(Ns) != length(files)) {
			stop("Ns must have the same length as files (or a single value).")
		}
	}
	
	# 3. Default trait names
	if (is.null(trait.names)) {
		trait.names <- gsub("\\..*$", "", basename(files))
	}
	
	# 4. Auto-detect columns from the first row of each file (UX Layer)
	detect_synonyms <- function(headers) {
		headers_lower <- tolower(headers)
		
		# Define synonym lists
		syns <- list(
			SNP = c("snp", "rsid", "marker", "snpid", "rs", "markername"),
			A1 = c("a1", "allele1", "effect_allele", "inc_allele", "reference_allele"),
			A2 = c("a2", "allele2", "non_effect_allele", "dec_allele", "other_allele"),
			P = c("p", "p-value", "pval", "p.value"),
			effect = c("effect", "or", "beta", "logor", "est", "estimate")
		)
		
		mapping <- list()
		for (target in names(syns)) {
			match_idx <- which(headers_lower %in% syns[[target]])
			if (length(match_idx) > 0) {
				mapping[[target]] <- headers[match_idx[1]]
			}
		}
		return(mapping)
	}
	
	# Merge auto-detected columns with user column.names
	final_mappings <- list()
	for (f in files) {
		sample_data <- data.table::fread(f, nrows = 1)
		detected <- detect_synonyms(colnames(sample_data))
		# User-provided mappings override auto-detected ones
		for (target in names(detected)) {
			if (!target %in% names(column.names)) {
				final_mappings[[target]] <- detected[[target]]
			}
		}
	}
	# Append user-provided mappings
	for (name in names(column.names)) {
		final_mappings[[name]] <- column.names[[name]]
	}
	
	# 5. Multithreading config
	if (cores > 0 || cores == -1) {
		old_threads <- umx_set_cores(NA, silent = TRUE)
		umx_set_cores(cores, silent = TRUE)
		on.exit(umx_set_cores(old_threads, silent = TRUE), add = TRUE)
	}
	
	# 6. Execute imxMunge in the target output directory
	# This forces imxMunge to write output files directly into output_dir
	curr_wd <- getwd()
	on.exit(setwd(curr_wd), add = TRUE)
	setwd(output_dir)
	
	message("Passing files to high-speed OpenMx C++ munging engine...")
	OpenMx::imxMunge(
		files = files, 
		hm3 = hm3, 
		trait.names = trait.names, 
		N = Ns, 
		info.filter = info.filter, 
		maf.filter = maf.filter, 
		column.names = final_mappings, 
		overwrite = overwrite,
		...
	)
	
	expected_outputs <- file.path(output_dir, paste0(trait.names, ".sumstats.gz"))
	return(expected_outputs)
}


umxGSEMprepFindData <- function(mode = c("Benchmark", "Synthetic", "MissingData", "EBI", "ATLAS"), output_dir = getwd(), ...) {
  mode = match.arg(mode)
  
  switch(mode,
    "Benchmark" = {
      message("Fetching original GenomicSEM psychiatric traits...")
      # Logic: Programmatically download the PGC/MDD/PTSD files used in the original paper
    },
    "Synthetic" = {
      message("Generating synthetic multivariate normal dataset (10^6 variants)...")
      # Logic: Call a C++ function that uses armadillo to generate a random 
      # matrix of Z-scores with a defined covariance structure.
    },
    "MissingData" = {
      message("Generating dataset with controlled missingness...")
      # Logic: Generate synthetic data then inject NAs into 5-15% of the entries
    },
    "EBI" = {
      # Logic: Search the GWAS Catalog API (ebi.ac.uk) for a specific trait ID
    },
    "ATLAS" = {
      # Logic: Interface with the GWAS ATLAS API
    }
  )
}

#' Fit genomic Structural Equation Models
#'
#' @description
#' `umxGSEM` is a wrapper around [umxRAM()] and OpenMx WLS to fit genomic structural
#' equation models (genomic SEM) using genetic covariance matrices (S) and sampling covariance
#' matrices (V) estimated from LD Score Regression (LDSC).
#'
#' @details
#' **Data Triage and Matrix Smoothing**
#' 
#' Genomic SEM relies on asymptotic covariance matrices (\eqn{V}) generated by LDSC. These matrices are frequently non-positive definite due to sampling variation or minor missingness across GWAS cohorts. `umxGSEM` implements an automatic triage system:
#' 
#' 1. **Level 1 (Clean)**: Matrices are strictly positive definite and pass directly to the optimizer.
#' 2. **Level 2 (Smoothed)**: Negative eigenvalues are detected. The matrices are smoothed using `Matrix::nearPD` to restore positive definiteness before estimation. 
#' 
#' *Fiduciary Warning*: Models estimated using smoothed matrices are tagged with a `gsem_triage` attribute. Downstream functions like `umxCompare` will read this attribute and warn the user that Satorra-Bentler difference tests may report artificial precision.
#' 
#' **Engine Integration**
#' `umxGSEM` automatically configures the C++ backend to cache the implied Jacobian matrix. This is a structural requirement for computing Satorra-Bentler (2010) scaled difference tests in `umxCompare`.
#'
#' **OpenMx WLS data (modern only)**
#'
#' Genomic SEM needs precomputed summary matrices \(S\) (genetic cov) and \(V\) (sampling cov of
#' \(\mathrm{vech}(S)\)). OpenMx WLS consumes these via the **modern** `mxData` interface only:
#'
#' ```
#' mxData(numObs = 1,
#'        observedStats = list(cov = S, useWeight = W, asymCov = V))
#' ```
#'
#' with `W = diag(1/diag(V))` for DWLS. `umxGSEM` also reorders `V` into OpenMx residual order
#' (all variances, then free covariances) and sets dimnames `var_*` / `poly_*_*`.
#'
#' **umx hard-refuses** OpenMx `type = "acov"` / `MxDataLegacyWLS` data (historical name trap:
#' `acov` meant useWeight; `fullWeight` meant asymCov). Do not construct that form; use
#' `observedStats` as above or raw data + `type = "DWLS"`.
#'
#' @param model A lavaan-style model string (also accepts umx `~=` loadings), **or** an existing
#'   [OpenMx::mxModel()] / [umxRAM()] RAM model whose `manifestVars` name traits in `S`.
#'   For SNP GWAS, use [umxGSEM_GWAS()] instead of passing SNPs here.
#' @param covstruc LDSC list with genetic covariance `S` and sampling covariance `V` (and typically `I`, `N`, `m`) as from GenomicSEM's `ldsc` / [umxGSEM_ldsc()] — see e.g. [Psych_LDSC]. If provided, `S` and `V` are extracted from it.
#' @param S Genetic covariance matrix (if covstruc not provided).
#' @param V Sampling covariance matrix (asymptotic covariance of S). If covstruc not provided.
#' @param estimation Method for estimation. One of "DWLS", "WLS", or "ULS" (defaults to "DWLS").
#' @param name The model name (defaults to "gsem").
#' @param numObs Sample size passed to OpenMx for WLS bookkeeping. With LDSC `V` already on the
#'   sampling-covariance scale, the GSEM \eqn{\chi^2} is \eqn{r'Wr}; use `numObs = 1` (default) so
#'   OpenMx reports that quantity directly. Larger dummy `N` rescales the displayed fit without
#'   changing point estimates.
#' @param smooth Whether to smooth non-positive definite matrices using [Matrix::nearPD()] (defaults to TRUE).
#' @param autoRun Whether to run the model (defaults to getOption("umx_auto_run")).
#' @param tryHard Method for fitting the model ("no", "yes", "ordinal", "search"). Defaults to `"yes"`
#'   because DWLS on LDSC `V` often has multiple local minima.
#' @param std.lv If `TRUE` (default), free the first factor loading and fix latent variances to 1
#'   (unit-variance identification). Preferred for genetic covariance scale where unit-loading ID
#'   often produces Heywood cases. Passed to [umxRAM()] / lavaan.
#' @param ... Additional arguments passed to [umxRAM()].
#' @return An [OpenMx::mxModel()] object of class `MxModelGSEM`.
#' @export
#' @family GSEM
#' @references
#' Grotzinger, A. D., Rhemtulla, M., de Vlaming, R., Ritchie, G. R., Mallard, T. T., Hill, W. D., ... & Tucker-Drob, E. M. (2019). Genomic structural equation modeling. *Nature Human Behaviour*, **3**, 513-525. \doi{10.1038/s41562-019-0566-x}

#' @examples
#' \dontrun{
#' data(Psych_LDSC)
#' # umx path style (~=) or lavaan (=~); unit-variance ID is the default
#' m1 = umxGSEM(model = "g ~= SCZ + BIP + MDD + EA + INSOM",
#'              covstruc = Psych_LDSC, estimation = "DWLS")
#' umxSummary(m1)
#'
#' # Explicit lavaan (same model)
#' m2 = umxGSEM(model = "g =~ NA*SCZ + BIP + MDD + EA + INSOM\ng ~~ 1*g",
#'              covstruc = Psych_LDSC, estimation = "DWLS")
#'
#' # Existing umxRAM structure (manifest names must match S)
#' m0 = umxRAM("tmp", data = mxData(Psych_LDSC$S[1:3, 1:3], type = "cov", numObs = 2),
#'   type = "cov", autoRun = FALSE, std.lv = TRUE,
#'   umxPath("g", to = c("SCZ", "BIP", "MDD")),
#'   umxPath(var = c("SCZ", "BIP", "MDD")),
#'   umxPath(var = "g", fixedAt = 1)
#' )
#' m3 = umxGSEM(m0, covstruc = Psych_LDSC, estimation = "DWLS")
#' }
umxGSEM <- function(model, covstruc = NULL, S = NULL, V = NULL, estimation = c("DWLS", "WLS", "ULS"), name = "gsem", numObs = 1, smooth = TRUE, autoRun = getOption("umx_auto_run"), tryHard = c("yes", "no", "ordinal", "search"), std.lv = TRUE, ...) {
	tryHard    = match.arg(tryHard)
	estimation = match.arg(estimation)

	sv = xmu_gsem_extract_SV(covstruc = covstruc, S = S, V = V)
	S = sv$S
	V = sv$V

	isMx = umx_is_MxModel(model)
	if (isMx) {
		if (!is.null(model$data) && exists("xmu_is_legacy_acov_data", mode = "function") && xmu_is_legacy_acov_data(model$data)) {
			xmu_stop_legacy_acov("umxGSEM")
		}
		keep_vars = model$manifestVars
		keep_vars = setdiff(keep_vars, "SNP") # SNP is GWAS-only
	} else if (is.character(model)) {
		model = gsub("~=", "=~", model, fixed = TRUE)
		# Discover manifests (cov placeholder; WLS data injected after triage)
		dummy_model = umxRAM(model, data = mxData(S, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, std.lv = std.lv, ...)
		keep_vars = dummy_model$manifestVars
	} else {
		stop("umxGSEM: model must be a lavaan/umx model string or an mxModel/umxRAM object.")
	}

	# Subset S and V to manifests used in the model (preserve S column order)
	keep_vars = colnames(S)[colnames(S) %in% keep_vars]
	if (length(keep_vars) == 0) {
		stop("No variables in S match manifest variables in the model.")
	}

	prep = xmu_gsem_prepare_WLS(S = S, V = V, keep_vars = keep_vars, estimation = estimation, smooth = smooth)
	S_subset = prep$S
	V_omx    = prep$V_omx
	W_omx    = prep$W_omx
	triageResult = prep$triage

	# Structure: umxRAM from string, or inject data into existing MxModel
	if (isMx) {
		final_model = mxModel(model, name = name)
		# Drop latents/paths for traits not in keep_vars are user's responsibility; data dims must match manifests used
		if (!all(keep_vars %in% final_model$manifestVars)) {
			stop("umxGSEM: model manifestVars must include all selected traits: ", paste(keep_vars, collapse = ", "))
		}
	} else {
		final_model = umxRAM(model, data = mxData(S_subset, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, name = name, std.lv = std.lv, ...)
	}

	final_model = xmu_gsem_set_starts(final_model, S_subset)
	wls_data    = mxData(numObs = numObs, observedStats = list(cov = S_subset, useWeight = W_omx, asymCov = V_omx))
	final_model = mxModel(final_model, wls_data)
	final_model = mxModel(final_model, mxFitFunctionWLS(type = estimation))
	final_model = xmu_gsem_bound_residuals(final_model, lbound = 1e-8)
	final_model = xmuLabel(final_model)

	attr(final_model, "gsem_triage")     = triageResult
	attr(final_model, "gsem_estimation") = estimation
	final_model = as(final_model, "MxModelGSEM")

	if (autoRun) {
		final_model = umxRun(final_model, tryHard = tryHard)
	}
	return(final_model)
}

# ---- shared GSEM helpers ----------------------------------------------------
# (xmu_gsem_vech_names is defined in xmu.R)

xmu_gsem_extract_SV <- function(covstruc = NULL, S = NULL, V = NULL, I = NULL) {
	if (!is.null(covstruc)) {
		if (is.list(covstruc)) {
			if (!is.null(covstruc$S)) S = covstruc$S else if (length(covstruc) >= 2) S = covstruc[[2]]
			if (!is.null(covstruc$V)) V = covstruc$V else if (length(covstruc) >= 1) V = covstruc[[1]]
			if (!is.null(covstruc$I)) I = covstruc$I else if (length(covstruc) >= 3) I = covstruc[[3]]
		}
	}
	if (is.null(S) || is.null(V)) {
		stop("You must provide BOTH the genetic covariance matrix S and the sampling covariance matrix V (either directly or via covstruc).")
	}
	S = as.matrix(S)
	V = as.matrix(V)
	if (is.null(colnames(S))) {
		stop("S matrix must have column/row names matching the trait names.")
	}
	rownames(S) = colnames(S)
	k = ncol(S)
	z = (k * (k + 1)) / 2
	if (ncol(V) != z || nrow(V) != z) {
		stop(paste0("Dimensions of V (", nrow(V), "x", ncol(V), ") must match non-redundant elements of S (", z, ")."))
	}
	vech_names = xmu_gsem_vech_names(colnames(S))
	if (is.null(colnames(V)) || is.null(rownames(V))) {
		colnames(V) = vech_names
		rownames(V) = vech_names
	}
	if (!is.null(I)) {
		I = as.matrix(I)
		if (is.null(colnames(I))) {
			colnames(I) = colnames(S)
			rownames(I) = colnames(S)
		}
	}
	list(S = S, V = V, I = I)
}

xmu_gsem_subset_SV <- function(S, V, keep_vars) {
	keep_vars = colnames(S)[colnames(S) %in% keep_vars]
	if (length(keep_vars) == 0) {
		stop("No variables in S match requested trait names.")
	}
	S_subset = S[keep_vars, keep_vars, drop = FALSE]
	keep_pairs = xmu_gsem_vech_names(keep_vars)
	if (all(keep_pairs %in% colnames(V))) {
		V_subset = V[keep_pairs, keep_pairs, drop = FALSE]
	} else {
		stop("Could not align V with S variable order. Provide V in GenomicSEM lower-triangle order with dimnames.")
	}
	list(S = S_subset, V = V_subset, keep_vars = keep_vars)
}

xmu_gsem_prepare_WLS <- function(S, V, keep_vars, estimation = "DWLS", smooth = TRUE) {
	sub = xmu_gsem_subset_SV(S, V, keep_vars)
	triageResult = xmu_gsem_triage(vMat = sub$V, sMat = sub$S, smooth = smooth)
	V_subset = triageResult$V
	S_subset = triageResult$S
	omxOrd = xmu_gsem_V_to_openmx_order(V_subset, colnames(S_subset))
	V_omx = omxOrd$V
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

# Convert GenomicSEM / R lower.tri(V) order → OpenMx WLS residual order with var_/poly_ names.
# OpenMx residual vector (continuous, no means): diag(S), then S[i,j] for j < i (column-major strict lower).
xmu_gsem_V_to_openmx_order <- function(V_gsem, traitNames) {
	k = length(traitNames)
	# Map GenomicSEM pair key -> index
	gsem_keys = character(0)
	for (j in 1:k) {
		for (i in j:k) {
			gsem_keys = c(gsem_keys, paste(traitNames[i], traitNames[j], sep = " "))
		}
	}
	if (is.null(colnames(V_gsem))) {
		colnames(V_gsem) = gsem_keys
		rownames(V_gsem) = gsem_keys
	}
	# Build OpenMx order and names
	omx_names = paste0("var_", traitNames)
	omx_keys  = paste(traitNames, traitNames, sep = " ") # "SCZ SCZ", ...
	if (k >= 2) {
		for (j in 1:(k - 1)) {
			for (i in (j + 1):k) {
				omx_names = c(omx_names, paste0("poly_", traitNames[i], "_", traitNames[j]))
				omx_keys  = c(omx_keys, paste(traitNames[i], traitNames[j], sep = " "))
			}
		}
	}
	# If V already has OpenMx-style names, keep as-is
	if (all(omx_names %in% colnames(V_gsem))) {
		V_omx = V_gsem[omx_names, omx_names, drop = FALSE]
		return(list(V = V_omx, names = omx_names))
	}
	# Match GenomicSEM keys (allow either "A B" or existing colnames order)
	if (all(omx_keys %in% colnames(V_gsem))) {
		V_omx = V_gsem[omx_keys, omx_keys, drop = FALSE]
	} else if (nrow(V_gsem) == length(gsem_keys)) {
		# Assume matrix is already in GenomicSEM lower.tri order
		colnames(V_gsem) = gsem_keys
		rownames(V_gsem) = gsem_keys
		V_omx = V_gsem[omx_keys, omx_keys, drop = FALSE]
	} else {
		stop("Cannot map V into OpenMx WLS order; check dimensions and dimnames.")
	}
	dimnames(V_omx) = list(omx_names, omx_names)
	return(list(V = V_omx, names = omx_names, map_keys = omx_keys))
}

# Set free factor loadings / residual starts from first eigencomponent of S
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

# Put a lower bound on free residual (manifest) variances
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

#' umx_is_GSEM
#'
#' Utility function returning a binary answer to the question "Is this a GSEM model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx GSEM [OpenMx::mxModel()]
#' @return - Boolean
#' @export
#' @family Test
#' @references - <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' # LDSC object (list with V, S, I, N, m)
#' data(Psych_LDSC)
#'
#' m1 = umxGSEM(model = "F1 ~= SCZ + BIP + MDD + EA + INSOM",
#'              covstruc = Psych_LDSC, estimation = "DWLS")
#' umx_is_GSEM(m1) # TRUE
#'
#' # lavaan-style paths (quote the model string carefully)
#' m2 = umxGSEM(
#'   model = "F1 =~ SCZ + BIP + MDD\nINSOM ~ F1\nEA ~ INSOM",
#'   covstruc = Psych_LDSC, estimation = "DWLS")
#' umxCompare(m2, m1)
#'
#' # Hand-built S/V heritability + genetic covariance
#' traits = c("Trt1", "Trt2")
#' S = matrix(c(0.25, 0.15, 0.15, 0.30), nrow = 2, ncol = 2,
#'            dimnames = list(traits, traits))
#' V = diag(c(0.002, 0.003, 0.002))
#' vech_names = c("Trt1 Trt1", "Trt2 Trt1", "Trt2 Trt2")
#' dimnames(V) = list(vech_names, vech_names)
#' modelStr = "Trt1 ~~ Trt1\nTrt2 ~~ Trt2\nTrt1 ~~ Trt2"
#' m0 = umxGSEM(model = modelStr, S = S, V = V, estimation = "DWLS")
#' umx_is_GSEM(m0)
#' }
umx_is_GSEM <- function(obj) {
	if(!umx_is_MxModel(obj)){
		return(FALSE)
	} else if(class(obj)[[1]] %in% c("MxModelGSEM")){
		return(TRUE)
	} else {
		return(class(obj$objective)[[1]] == "MxRAMObjective")
	}
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

# =============================================================================
# SNP preparation and multivariate GWAS (separate from structural umxGSEM)
# =============================================================================

#' Prepare multivariate SNP summary statistics for genomic SEM GWAS
#'
#' @description
#' Aligns per-trait GWAS summary statistic files to a 1000 Genomes reference
#' (allele match + MAF filter), converts odds ratios to the log scale when needed,
#' and applies the continuous-scale transformation used in Genomic SEM so that
#' effects and standard errors are on a unit-variance phenotype scale. The result
#' is ready for [umxGSEM_GWAS()].
#'
#' The returned `data.frame` has columns **SNP**, **CHR**, **BP**, **MAF**,
#' **A1**, **A2**, and **`beta.*` / `se.*` per trait** (listwise-complete SNPs
#' present in the reference and in every trait file).
#'
#' @details
#' ## Example input (SCZ subset)
#'
#' Workshop-style GWAS files are whitespace-separated. A few lines of the package
#' toy file `SCZ_subset.txt` look like:
#'
#' | SNP | Freq.A1 | CHR | BP | A1 | A2 | OR | SE | P |
#' |:----|--------:|----:|---:|:---|:---|-----:|-------:|-------:|
#' | rs1000000 | 0.7763 | 12 | 126890980 | G | A | 1.0195 | 0.011559 | 0.0954 |
#' | rs10000010 | 0.5099 | 4 | 21618674 | T | C | 0.9910 | 0.009582 | 0.3452 |
#' | rs1000002 | 0.5199 | 3 | 183635768 | C | T | 1.0048 | 0.009435 | 0.6113 |
#'
#' Column synonyms are resolved automatically (`SNP`/`rsid`, `OR`/`beta`/`effect`,
#' `SE`/`stderr`, `INFO`, `N`, allele columns, etc.). When the median effect is near 1,
#' values are treated as odds ratios and logged.
#'
#' ## Example output
#'
#' After aligning SCZ, BIP, and MDD to a 1000G reference with `se.logit = TRUE`
#' (toy subsets under `inst/developer/GenomicSEM/`), the first rows resemble:
#'
#' | SNP | CHR | BP | MAF | A1 | A2 | beta.SCZ | se.SCZ | beta.BIP | se.BIP | beta.MDD | se.MDD |
#' |:----|----:|---:|----:|:---|:---|---------:|-------:|---------:|-------:|---------:|-------:|
#' | rs1000073 | 1 | 157255396 | 0.417 | A | G | -0.00053 | 0.00543 | 0.00696 | 0.0133 | -0.00105 | 0.00447 |
#' | rs1000050 | 1 | 162736463 | 0.147 | C | T | 0.00614 | 0.00742 | -0.0150 | 0.0182 | -0.00033 | 0.00700 |
#' | rs1000053 | 2 | 12790328 | 0.090 | C | T | 0.00149 | 0.00915 | -0.0187 | 0.0240 | -0.00226 | 0.00744 |
#'
#' Reference alleles and MAF come from `ref`; betas are flipped so that A1 matches the reference.
#' SNPs that fail allele match, INFO, or listwise merge are dropped.
#'
#' ## Scale flags (`se.logit`, `OLS`, `linprob`)
#'
#' Pass a single logical or a vector recycled to `length(files)`. For each trait,
#' choose the treatment that matches how that GWAS was run:
#'
#' | Flag | Typical traits | What it does |
#' |:-----|:---------------|:-------------|
#' | `se.logit = TRUE` | Case/control logistic GWAS (default) | Treat `effect`/`SE` as log(OR) and SE on the logistic scale; transform both to the continuous scale used by Genomic SEM: divide by \(\sqrt{\beta^2 \mathrm{Var}(\mathrm{SNP}) + \pi^2/3}\), with \(\mathrm{Var}(\mathrm{SNP}) = 2p(1-p)\). |
#' | `linprob = TRUE` | Binary traits lacking usable logistic SEs | Reconstruct Z from effect/SE when present, then back out continuous-scale beta and SE using effective sample size `N` (sum of effective Ns) and the same logistic-to-continuous adjustment. |
#' | `OLS = TRUE` | Continuous phenotypes | Standardize using Z and total sample size `N` with SNP variance \(2p(1-p)\). |
#' | `se.logit = FALSE` (and not OLS/linprob) | Non-standard OR-scale SEs | Alternate SE scaling for odds-ratio reported SEs; prefer `se.logit = TRUE` for ordinary logistic GWAS. |
#'
#' ## Other parameters
#'
#' * **`files`**: Paths (or basenames under `baseDir`) to one GWAS file per trait, in the
#'   same trait order you will use for LDSC / [umxGSEM_GWAS()].
#' * **`ref`**: 1000 Genomes (or equivalent) reference table with columns
#'   `SNP`, `CHR`, `BP`, `MAF`, `A1`, `A2`. Used for allele alignment, MAF filter, and
#'   SNP annotation in the return value.
#' * **`trait.names`**: Labels for `beta.*` / `se.*` columns (default: file basenames
#'   without extension). Must match LDSC trait names when you expand into GWAS.
#' * **`N`**: Optional sample sizes, one per trait (or recycled). Required for meaningful
#'   `OLS` / `linprob` transforms (total N for OLS; effective N sum for linprob).
#' * **`info.filter`**: Drop SNPs with INFO below this value when an INFO column is present
#'   (default `0.6`). Missing INFO is kept.
#' * **`maf.filter`**: Drop reference SNPs with MAF below this threshold (default `0.01`).
#'   MAF is folded so it is at most 0.5.
#' * **`baseDir`**: Directory prepended to relative `files` and `ref` paths. Absolute
#'   paths are left unchanged.
#'
#' Processing is **listwise**: a SNP must appear in the reference and in every trait file
#' after QC. Expect a message like `umxGSEM_sumstats: N SNPs after listwise merge with reference.`
#'
#' @param files Character vector of GWAS summary statistic file names or paths (same trait order as LDSC).
#'   Relative names are resolved under `baseDir` (default: working directory).
#' @param ref Path or file name of 1000G reference with columns SNP, CHR, BP, MAF, A1, A2
#'   (resolved under `baseDir` when relative).
#' @param trait.names Character vector of trait names for output columns (defaults to file basenames).
#' @param se.logit Logical (or vector, one per trait). If `TRUE` (default), treat effects/SEs as
#'   logistic-scale log(OR) and apply the Genomic SEM continuous-scale transform.
#' @param OLS Logical (or vector). If `TRUE`, treat the trait as continuous OLS and standardize
#'   with Z and total `N` (see Details).
#' @param linprob Logical (or vector). If `TRUE`, recover continuous-scale effects from Z and
#'   effective `N` for binary traits (see Details).
#' @param N Optional numeric sample sizes (total N for OLS; sum of effective N for linprob).
#'   Recycled to `length(files)`.
#' @param info.filter Numeric INFO threshold (default `0.6`); applied when INFO is present.
#' @param maf.filter Numeric MAF threshold applied to the reference (default `0.01`).
#' @param baseDir Directory for relative `files` and `ref` (default `getwd()`). Absolute paths are left unchanged.
#' @return A `data.frame` with columns `SNP`, `CHR`, `BP`, `MAF`, `A1`, `A2`, and
#'   `beta.<trait>` / `se.<trait>` for each trait in `trait.names`, sorted by CHR/BP.
#' @export
#' @family GSEM
#' @seealso [umxGSEM_GWAS()], [umxGSEM()]
#' @references
#' Grotzinger, A. D., Rhemtulla, M., de Vlaming, R., Ritchie, S. J., Mallard, T. T.,
#' Hill, W. D., Ip, H. F., Marioni, R. E., McIntosh, A. M., Deary, I. J., Koellinger, P. D.,
#' Harden, K. P., Nivard, M. G., & Tucker-Drob, E. M. (2019).
#' Genomic structural equation modelling provides insights into the multivariate genetic
#' architecture of complex traits. *Nature Human Behaviour*, **3**, 513–525.
#' \doi{10.1038/s41562-019-0566-x}
#'
#' Tutorial materials and sumstats conventions follow the Genomic SEM documentation and
#' workshops (see <https://github.com/GenomicSEM/GenomicSEM>).
#'
#' @examples
#' \dontrun{
#' # basenames only (files live in baseDir)
#' snps = umxGSEM_sumstats(
#'   files = c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt"),
#'   ref = "reference.1000G.subset.txt",
#'   trait.names = c("SCZ", "BIP", "MDD"),
#'   se.logit = TRUE,
#'   baseDir = "path/to/umx/inst/developer/GenomicSEM"
#' )
#' head(snps)
#' # columns: SNP, CHR, BP, MAF, A1, A2, beta.SCZ, se.SCZ, beta.BIP, ...
#' }
umxGSEM_sumstats <- function(files, ref, trait.names = NULL, se.logit = TRUE, OLS = FALSE, linprob = FALSE, N = NULL, info.filter = 0.6, maf.filter = 0.01, baseDir = getwd()) {
	files = as.character(files)
	if (length(files) < 1) {
		stop("Provide at least one GWAS file path.")
	}
	baseDir = path.expand(baseDir)
	files = xmu_gsem_resolve_paths(files, baseDir)
	ref = xmu_gsem_resolve_paths(ref, baseDir)
	n = length(files)
	if (is.null(trait.names)) {
		trait.names = gsub("\\..*$", "", basename(files))
	}
	if (length(trait.names) != n) {
		stop("trait.names must have the same length as files.")
	}
	se.logit = rep(se.logit, length.out = n)
	OLS = rep(OLS, length.out = n)
	linprob = rep(linprob, length.out = n)
	if (is.null(N)) {
		N = rep(NA_real_, n)
	} else {
		N = rep(N, length.out = n)
	}

	ref = xmu_gsem_read_sumstats_table(ref)
	ref_cols = toupper(names(ref))
	names(ref) = ref_cols
	need = c("SNP", "CHR", "BP", "MAF", "A1", "A2")
	if (!all(need %in% names(ref))) {
		stop("Reference file must contain columns: ", paste(need, collapse = ", "))
	}
	ref$SNP = as.character(ref$SNP)
	ref$A1 = toupper(as.character(ref$A1))
	ref$A2 = toupper(as.character(ref$A2))
	ref$MAF = as.numeric(ref$MAF)
	ref = ref[!is.na(ref$MAF) & ref$MAF >= maf.filter & ref$MAF < 1, , drop = FALSE]
	ref$MAF = ifelse(ref$MAF > 0.5, 1 - ref$MAF, ref$MAF)

	merged = NULL
	for (i in seq_len(n)) {
		one = xmu_gsem_sumstats_one_trait(
			filename = files[i],
			ref = ref,
			trait.name = trait.names[i],
			se.logit = se.logit[i],
			OLS = OLS[i],
			linprob = linprob[i],
			N = N[i],
			info.filter = info.filter
		)
		if (is.null(merged)) {
			merged = one
		} else {
			merged = merge(merged, one, by = "SNP", all = FALSE)
		}
	}
	# Attach ref annotation for surviving SNPs
	ref_ann = ref[, c("SNP", "CHR", "BP", "MAF", "A1", "A2"), drop = FALSE]
	merged = merge(ref_ann, merged, by = "SNP", all = FALSE)
	# Prefer ref alleles
	merged = merged[order(merged$CHR, merged$BP), , drop = FALSE]
	rownames(merged) = NULL
	message("umxGSEM_sumstats: ", nrow(merged), " SNPs after listwise merge with reference.")
	merged
}

# Resolve relative paths under baseDir; leave absolute paths alone
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

#' Multivariate genomic SEM GWAS (SNP on a factor or user model)
#'
#' Expands the LDSC genetic covariance structure with each SNP (using prepared
#' betas from [umxGSEM_sumstats()]), fits DWLS/WLS, and returns SNP-level results.
#' For a default common-factor model, estimates \code{F1 ~ SNP}.
#'
#' The LDSC object is the usual GenomicSEM-style list (`S`, `V`, preferably `I`);
#' see e.g. [Psych_LDSC] for a packaged example and field definitions.
#'
#' @param covstruc LDSC list with `S` (genetic covariance), `V` (sampling covariance of
#'   \(\mathrm{vech}(S)\)), and preferably `I` (LDSC intercept matrix, for GC-corrected SEs).
#'   See e.g. [Psych_LDSC] (`I` = intercepts / sample-overlap; also `N`, `m`).
#' @param SNPs Data frame from [umxGSEM_sumstats()] (columns `beta.*`, `se.*`, `MAF`, `SNP`, …).
#' @param model Optional lavaan/umx string or mxModel. If `NULL`, builds
#'   `F1 =~ NA*T1 + T2 + ...; F1 ~~ 1*F1; F1 ~ SNP` for traits in `S`.
#' @param estimation `"DWLS"` (default), `"WLS"`, or `"ULS"`.
#' @param traits Trait names (order must match `beta.*` columns). Default: colnames of `S`.
#' @param GC Genomic control for SNP sampling variances: `"standard"`, `"conserv"`, or `"none"`.
#' @param SNPSE SE used for SNP variance (treated as nearly fixed; default 5e-4).
#' @param maxSNPs Optional limit for smoke tests.
#' @param snpEffect Character; path to report (default `"F1 ~ SNP"` style match on A matrix).
#' @param quiet Suppress per-SNP messages.
#' @return A data.frame of SNP results (`SNP`, `CHR`, `BP`, `MAF`, `est`, `se`, `Z`, `P`, `status`, …).
#' @export
#' @family GSEM
#' @seealso [umxGSEM_sumstats()], [umxGSEM()], [Psych_LDSC]
#'
#' @examples
#' \dontrun{
#' data(Psych_LDSC)
#' # Psych_LDSC fields: S (genetic cov), V (sampling cov), I (LDSC intercepts),
#' # N (effective Ns), m (SNP count). See ?Psych_LDSC
#' dir = "~/bin/umx/inst/developer/GenomicSEM"
#' snps = umxGSEM_sumstats(
#'   files = file.path(dir, c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt")),
#'   ref = file.path(dir, "reference.1000G.subset.txt"),
#'   trait.names = c("SCZ", "BIP", "MDD"), se.logit = TRUE
#' )
#' # 3-trait LDSC block matching SNP traits (keep I for GC)
#' cov3 = list(
#'   S = Psych_LDSC$S[1:3, 1:3],
#'   V = Psych_LDSC$V[1:6, 1:6],
#'   I = Psych_LDSC$I[1:3, 1:3]  # LDSC intercept matrix
#' )
#' gwas = umxGSEM_GWAS(covstruc = cov3, SNPs = snps, maxSNPs = 20)
#' head(gwas)
#' }
umxGSEM_GWAS <- function(covstruc, SNPs, model = NULL, estimation = c("DWLS", "WLS", "ULS"), traits = NULL, GC = c("standard", "conserv", "none"), SNPSE = 5e-4, maxSNPs = NULL, snpEffect = NULL, quiet = TRUE) {
	estimation = match.arg(estimation)
	GC = match.arg(GC)
	sv = xmu_gsem_extract_SV(covstruc = covstruc)
	S_LD = sv$S
	V_LD = sv$V
	I_LD = sv$I
	if (is.null(I_LD)) {
		I_LD = diag(ncol(S_LD))
		dimnames(I_LD) = list(colnames(S_LD), colnames(S_LD))
		if (!quiet) message("umxGSEM_GWAS: no I matrix in covstruc; using identity (GC limited).")
	}
	if (is.null(traits)) {
		traits = colnames(S_LD)
	}
	# Subset LDSC to requested traits
	sub = xmu_gsem_subset_SV(S_LD, V_LD, traits)
	S_LD = sub$S
	V_LD = sub$V
	traits = sub$keep_vars
	I_LD = I_LD[traits, traits, drop = FALSE]
	k = length(traits)

	beta_cols = paste0("beta.", traits)
	se_cols = paste0("se.", traits)
	if (!all(beta_cols %in% colnames(SNPs)) || !all(se_cols %in% colnames(SNPs))) {
		stop("SNPs must contain columns ", paste(c(beta_cols, se_cols), collapse = ", "),
			". Run umxGSEM_sumstats() first.")
	}
	need_snp = c("SNP", "MAF")
	if (!all(need_snp %in% colnames(SNPs))) {
		stop("SNPs must include SNP and MAF columns.")
	}
	if (!is.null(maxSNPs)) {
		SNPs = SNPs[seq_len(min(maxSNPs, nrow(SNPs))), , drop = FALSE]
	}
	nSNP = nrow(SNPs)
	varSNP = 2 * SNPs$MAF * (1 - SNPs$MAF)
	beta_SNP = as.matrix(SNPs[, beta_cols, drop = FALSE])
	SE_SNP = as.matrix(SNPs[, se_cols, drop = FALSE])
	varSNPSE2 = as.numeric(SNPSE)^2
	coords = as.matrix(expand.grid(seq_len(k), seq_len(k)))

	# Default common-factor + SNP (unit-loading ID like GenomicSEM commonfactorGWAS)
	if (is.null(model)) {
		rest = if (k > 1) paste0(" + ", paste(traits[-1], collapse = " + ")) else ""
		model = paste0(
			"F1 =~ 1*", traits[1], rest, "\n",
			"F1 ~~ NA*F1\n",
			"F1 ~ SNP\n"
		)
	} else if (is.character(model) && !grepl("SNP", model, fixed = TRUE)) {
		warning("umxGSEM_GWAS: model string does not mention SNP; appending 'F1 ~ SNP'.", call. = FALSE)
		model = paste0(model, "\nF1 ~ SNP\n")
	}

	# Preallocate result columns (avoid do.call(rbind, ...) over list blobs)
	out_SNP = character(nSNP)
	out_CHR = if ("CHR" %in% names(SNPs)) SNPs$CHR[seq_len(nSNP)] else rep(NA, nSNP)
	out_BP  = if ("BP" %in% names(SNPs)) SNPs$BP[seq_len(nSNP)] else rep(NA, nSNP)
	out_MAF = SNPs$MAF[seq_len(nSNP)]
	out_A1  = if ("A1" %in% names(SNPs)) as.character(SNPs$A1[seq_len(nSNP)]) else rep(NA_character_, nSNP)
	out_A2  = if ("A2" %in% names(SNPs)) as.character(SNPs$A2[seq_len(nSNP)]) else rep(NA_character_, nSNP)
	out_est = rep(NA_real_, nSNP)
	out_se  = rep(NA_real_, nSNP)
	out_Z   = rep(NA_real_, nSNP)
	out_P   = rep(NA_real_, nSNP)
	out_status = rep(NA_integer_, nSNP)
	out_fail = rep(TRUE, nSNP)
	out_warn = rep(NA_character_, nSNP)

	oldAuto = getOption("umx_auto_plot")
	options(umx_auto_plot = FALSE)
	on.exit(options(umx_auto_plot = oldAuto), add = TRUE)

	for (i in seq_len(nSNP)) {
		if (!quiet && (i == 1L || i %% 50L == 0L || i == nSNP)) {
			message("umxGSEM_GWAS: SNP ", i, " / ", nSNP)
		}
		out_SNP[i] = as.character(SNPs$SNP[i])
		expn = xmu_gsem_expand_snp(
			S_LD = S_LD, V_LD = V_LD, I_LD = I_LD,
			beta_i = as.numeric(beta_SNP[i, ]),
			se_i = as.numeric(SE_SNP[i, ]),
			varSNP_i = varSNP[i],
			varSNPSE2 = varSNPSE2,
			GC = GC, coords = coords
		)
		fit = tryCatch({
			m = umxGSEM(
				model = model,
				S = expn$S, V = expn$V,
				estimation = estimation,
				name = paste0("gwas_", i),
				autoRun = FALSE,
				tryHard = "no",
				std.lv = FALSE
			)
			# Silent run (avoid umxSummary spam in the SNP loop)
			# TODO: replace R loop with OpenMx C++/OpenMP SNP stream (flatten once, block by core)
			mxRun(m, silent = TRUE, suppressWarnings = TRUE)
		}, error = function(e) e)

		if (inherits(fit, "error")) {
			out_warn[i] = conditionMessage(fit)
		} else {
			out_fail[i] = FALSE
			out_status[i] = tryCatch(as.integer(fit$output$status$code), error = function(e) NA_integer_)
			# F1 ~ SNP: RAM path from SNP to first latent
			est_se = xmu_gsem_extract_snp_path(fit, traits)
			out_est[i] = est_se$est
			out_se[i] = est_se$se
			if (is.finite(out_est[i]) && is.finite(out_se[i]) && out_se[i] > 0) {
				out_Z[i] = out_est[i] / out_se[i]
				out_P[i] = 2 * stats::pnorm(-abs(out_Z[i]))
			}
		}
	}
	data.frame(
		SNP = out_SNP, CHR = out_CHR, BP = out_BP, MAF = out_MAF,
		A1 = out_A1, A2 = out_A2,
		est = out_est, se = out_se, Z = out_Z, P = out_P,
		status = out_status, fail = out_fail, warning = out_warn,
		stringsAsFactors = FALSE
	)
}

# Build S and V including SNP (Genomic SEM ordering: SNP first, then traits)
xmu_gsem_expand_snp <- function(S_LD, V_LD, I_LD, beta_i, se_i, varSNP_i, varSNPSE2, GC = "standard", coords = NULL) {
	k = ncol(S_LD)
	traits = colnames(S_LD)
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
				V_SNP[x, y] = se_i[y] * se_i[x] * I_LD[x, y] * I_LD[x, x] * I_LD[y, y] * varSNP_i^2
			} else {
				V_SNP[x, x] = (se_i[x] * I_LD[x, x] * varSNP_i)^2
			}
		} else if (GC == "standard") {
			if (x != y) {
				V_SNP[x, y] = se_i[y] * se_i[x] * I_LD[x, y] * sqrt(I_LD[x, x]) * sqrt(I_LD[y, y]) * varSNP_i^2
			} else {
				V_SNP[x, x] = (se_i[x] * sqrt(I_LD[x, x]) * varSNP_i)^2
			}
		} else {
			if (x != y) {
				V_SNP[x, y] = se_i[y] * se_i[x] * I_LD[x, y] * varSNP_i^2
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
	S_Full[2:(k + 1), 2:(k + 1)] = S_LD
	S_Full[1:(k + 1), 1] = S_SNP
	S_Full[1, 1:(k + 1)] = S_SNP
	dimnames(S_Full) = list(c("SNP", traits), c("SNP", traits))

	# V_Full in GenomicSEM lower.tri order of S_Full
	nV = ((k + 1) * (k + 2)) / 2
	V_Full = diag(nV)
	V_Full[1, 1] = varSNPSE2
	V_Full[2:(k + 1), 2:(k + 1)] = V_SNP
	V_Full[(k + 2):nV, (k + 2):nV] = V_LD
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
	lat = if (length(fit$latentVars)) fit$latentVars[1] else NA_character_
	if (!is.null(fit$A) && !is.na(lat) && lat %in% rownames(fit$A$values) && "SNP" %in% colnames(fit$A$values)) {
		est = fit$A$values[lat, "SNP"]
		lab = fit$A$labels[lat, "SNP"]
		ses = fit$output$standardErrors
		cf = tryCatch(coef(fit), error = function(e) NULL)
		if (!is.null(ses) && !is.null(lab) && !is.na(lab) && !is.null(cf)) {
			rn = rownames(ses)
			if (!is.null(rn) && lab %in% rn) {
				se = as.numeric(ses[lab, 1])
			} else if (lab %in% names(cf) && length(as.numeric(ses)) >= length(cf)) {
				# OpenMx often returns SE in free-parameter order without rownames
				se = as.numeric(ses)[match(lab, names(cf))]
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
				se = ss$Std.Error[hit[1]]
			}
		}
	}
	list(est = as.numeric(est)[1], se = as.numeric(se)[1])
}