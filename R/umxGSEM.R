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
#' Genomic SEM needs precomputed summary matrices S (genetic cov) and V (sampling cov of
#' vech(S)). OpenMx WLS consumes these via the **modern** `mxData` interface only:
#'
#' ```
#' mxData(numObs = 1, type = "summary",
#'        observedStats = list(cov = S, useWeight = W, asymCov = V))
#' ```
#'
#' with `W = diag(1/diag(V))` for DWLS. `umxGSEM` also reorders `V` into OpenMx residual order
#' (all variances, then free covariances) and sets dimnames `var_*` / `poly_*_*`.
#'
#' **umx hard-refuses forever** the removed OpenMx WLS data interface
#' (`type = "acov"` / `"none"`, `MxDataLegacyWLS`, top-level `acov=`/`fullWeight=`, or
#' `observedStats$acov`/`$fullWeight` — historical name trap: `acov` meant useWeight).
#' Use `observedStats` as above or raw data + `type = "DWLS"`.
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

	xmu_require_summary_mxData("umxGSEM")

	if (is.null(covstruc)) {
		if (is.null(S) || is.null(V)) {
			stop("You must provide BOTH the genetic covariance matrix S and the sampling covariance matrix V (either directly or via covstruc).")
		}
		covstruc = list(S = S, V = V)
	} else {
		if (!is.null(S) || !is.null(V)) {
			warning("covstruc provided; ignoring separate S and V arguments.", call. = FALSE)
		}
	}

	# Retain "SNP" as a manifest when it is present in S (umxGSEM_GWAS expand).
	# Only drop SNP for pure structural GSEM (S has traits only).
	snpInS = "SNP" %in% colnames(covstruc$S)

	isMx = umx_is_MxModel(model)
	if (isMx) {
		if (!is.null(model$data) && exists("xmu_is_legacy_acov_data", mode = "function") && xmu_is_legacy_acov_data(model$data)) {
			xmu_stop_legacy_acov("umxGSEM")
		}
		# Keep model manifest order (OpenMx WLS matches cov to F by position; wrong order segfaults).
		keep_vars = model$manifestVars
		if (!snpInS) {
			keep_vars = setdiff(keep_vars, "SNP")
		}
	} else if (is.character(model)) {
		model = gsub("~=", "=~", model, fixed = TRUE)
		# Discover manifests (cov placeholder; WLS data injected after triage)
		dummy_model = umxRAM(model, data = mxData(covstruc$S, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, std.lv = std.lv, ...)
		keep_vars = dummy_model$manifestVars
		if (!snpInS) {
			keep_vars = setdiff(keep_vars, "SNP")
		}
	} else {
		stop("umxGSEM: model must be a lavaan/umx model string or an mxModel/umxRAM object.")
	}

	if (length(keep_vars) == 0) {
		stop("No manifest variables found in the model.")
	}
	missing_traits = setdiff(keep_vars, colnames(covstruc$S))
	if (length(missing_traits) > 0) {
		stop("Model manifests not found in S/covstruc: ", paste(missing_traits, collapse = ", "),
			". S traits: ", paste(colnames(covstruc$S), collapse = ", "))
	}
	# Do NOT reorder keep_vars to colnames(covstruc$S): WLS cov must match model$manifestVars / F order.

	prep = xmu_gsem_prepare_WLS(covstruc = covstruc, keep_vars = keep_vars, estimation = estimation, smooth = smooth)
	S_subset = prep$S
	V_omx    = prep$V_omx
	W_omx    = prep$W_omx
	triageResult = prep$triage

	# Structure: umxRAM from string, or inject data into existing MxModel
	if (isMx) {
		final_model = mxModel(model, name = name)
		man_now = final_model$manifestVars
		if (!snpInS) {
			man_now = setdiff(man_now, "SNP")
		}
		if (!identical(as.character(man_now), as.character(keep_vars))) {
			stop("umxGSEM: internal mismatch between model manifestVars and WLS cov order.")
		}
	} else {
		final_model = umxRAM(model, data = mxData(S_subset, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, name = name, std.lv = std.lv, ...)
	}

	final_model = xmu_gsem_set_starts(final_model, S_subset)
	# Align cov dimnames with model manifests (guard OpenMx backend)
	if (!identical(colnames(S_subset), final_model$manifestVars)) {
		# String-built models should already match; reindex if needed
		man = final_model$manifestVars
		if (!all(man %in% colnames(S_subset))) {
			stop("umxGSEM: WLS cov traits do not cover model manifests: ", paste(man, collapse = ", "))
		}
		S_subset = S_subset[man, man, drop = FALSE]
		# V/W are in OpenMx residual order for S_subset's previous trait order — rebuild if reordered
		prep = xmu_gsem_prepare_WLS(covstruc = covstruc, keep_vars = man, estimation = estimation, smooth = smooth)
		S_subset = prep$S
		V_omx = prep$V_omx
		W_omx = prep$W_omx
		triageResult = prep$triage
	}
	wls_data    = xmu_mxData_summary(numObs = numObs, observedStats = list(cov = S_subset, useWeight = W_omx, asymCov = V_omx))
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




# Set free factor loadings / residual starts from first eigencomponent of S

# Put a lower bound on free residual (manifest) variances

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
#' | `se.logit = TRUE` | Case/control logistic GWAS (default) | Treat `effect`/`SE` as log(OR) and SE on the logistic scale; transform both to the continuous scale used by Genomic SEM: divide by sqrt(beta^2 * Var(SNP) + pi^2/3), with Var(SNP) = 2p(1-p). |
#' | `linprob = TRUE` | Binary traits lacking usable logistic SEs | Reconstruct Z from effect/SE when present, then back out continuous-scale beta and SE using effective sample size `N` (sum of effective Ns) and the same logistic-to-continuous adjustment. |
#' | `OLS = TRUE` | Continuous phenotypes | Standardize using Z and total sample size `N` with SNP variance 2p(1-p). |
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
#'   vech(S)), and preferably `I` (LDSC intercept matrix, for GC-corrected SEs).
#'   See e.g. [Psych_LDSC] (`I` = intercepts / sample-overlap; also `N`, `m`).
#' @param SNPs Data frame from [umxGSEM_sumstats()] (columns `beta.*`, `se.*`, `MAF`, `SNP`, …).
#' @param model Optional lavaan/umx string or mxModel. If `NULL`, builds
#'  A RAM model with one latent (F1) loading on all the traits, and  "SNP" loading on F1
#' @param estimation `"DWLS"` (default), `"WLS"`, or `"ULS"`.
#' @param traits Trait names. Default: colnames of `covstruc$S`.
#' @param GC Genomic control for SNP sampling variances: `"standard"`, `"conserv"`, or `"none"`.
#' @param uncertainty Whether to compute the robust sandwich SE (`"robustSE"`, default) or naive SE (`"SE"`).
#' @param SnpSamplingError SE used for SNP variance (treated as nearly fixed; default 5e-4).
#' @param maxSNPs Optional limit for smoke tests.
#' @param snpEffect Character; path label to extract from the model (default \code{"SNP_to_F1"}).
#' @param quiet Suppress per-SNP messages.
#' @param fix_measurement Logical; if TRUE (default), fixes the non-SNP measurement model parameters to their base optimized values when evaluating each SNP (only applies if analytic engine is bypassed).
#' @param force_fallback Logical; internal use for testing to force the non-vectorized analytic path (default `FALSE`).
#' @return A data.frame of SNP results (`SNP`, `CHR`, `BP`, `MAF`, `est`, `se`, `Z`, `P`, `status`, …).
#' @export
#' @family GSEM
#' @seealso [umxGSEM_sumstats()], [umxGSEM()], [Psych_LDSC]
#'
#' @examples
#' \dontrun{
#' 1. load data
#' data(Psych_LDSC)
#' # Have a quick look if you like
#' # Psych_LDSC fields: S, V, I, N, m (see ?Psych_LDSC). V pairs are named var_SCZ, poly_BIP_SCZ, ...
#' umxSummary(Psych_LDSC)
#'
#' #' # 2. build the snp data structure from your SNP data txt files, e.g.
#' dir = "~/bin/umx/inst/developer/GenomicSEM"
#' snps = umxGSEM_sumstats(
#'   files = file.path(dir, c("SCZ_subset.txt", "BIP_subset.txt", "MDD_subset.txt")),
#'   ref = file.path(dir, "reference.1000G.subset.txt"),
#'   trait.names = c("SCZ", "BIP", "MDD"), se.logit = TRUE
#' )
#'
#' # 3. Run the GWAS on your trait measurement model adding SNP effects into the latent structure
#' GWAS = umxGSEM_GWAS(covstruc = Psych_LDSC, SNPs = snps,
#'   traits = c("SCZ", "BIP", "MDD"), maxSNPs = 20)
#'
#' # 4. Examine results and make a new drug or something useful!
#' head(GWAS)
#' # Manhattan Plot (default)
#' plot(gwas_res)
#' plot(example_gwas, chromosomes= 1, label_top_n= 4, title="IQ GWAS") + theme_plos()
#' volcano_plot(example_gwas, size_by = "AF", label_top_n = 3)
#' 
#' # Q-Q Plot
#' plot(gwas_res, type = "qq")
#' 
#' # ================
#' # = Another demo =
#' # ================
#' # 1. Load the reference LDSC covariance structure
#' data(Psych_LDSC, package = "umx")
#' 
#' # 2. Generate a million simulated SNPs
#' traits = c("SCZ", "BIP", "MDD")
#' t0 = Sys.time()
#' snps = umxGSEM_sim_snps(n = 1e6, traits = traits)
#' print(Sys.time() - t0)
#' 
#' # 3. Run the GWAS and time it
#' t0 = Sys.time()
#' gwas_res = umxGSEM_GWAS(covstruc = Psych_LDSC, SNPs = snps, traits = traits)
#' print(Sys.time() - t0)
#' 
#' # 4. View the results
#' head(gwas_res)
#' # Manhattan Plot (default)
#' plot(gwas_res)
#' plot(example_gwas, chromosomes= 1, label_top_n= 4, title="IQ GWAS") + theme_plos()
#' volcano_plot(example_gwas, size_by = "AF", label_top_n = 3)
#' 
#' # Q-Q Plot
#' plot(gwas_res, type = "qq")
#' 
#' }
umxGSEM_GWAS <- function(covstruc, SNPs, model = NULL, estimation = c("DWLS", "WLS", "ULS"), traits = NULL, GC = c("standard", "conserv", "none"), uncertainty = c("robustSE", "SE"), SnpSamplingError = 5e-4, maxSNPs = NULL, snpEffect = "SNP_to_F1", quiet = TRUE, fix_measurement = TRUE, force_fallback = FALSE) {
	# Fail before SNP loop if summary WLS is requested without fork OpenMx
	estimation = match.arg(estimation)
	GC = match.arg(GC)
	uncertainty = match.arg(uncertainty)
	if (estimation %in% c("DWLS", "WLS")) {
		xmu_require_summary_mxData("umxGSEM_GWAS")
	}
	if (!is.list(covstruc) || is.null(covstruc$S) || is.null(covstruc$V)) {
		stop("covstruc must be a list containing at least matrices S and V.")
	}
	if (is.null(covstruc$I)) {
		covstruc$I = diag(ncol(covstruc$S))
		dimnames(covstruc$I) = list(colnames(covstruc$S), colnames(covstruc$S))
		if (!quiet) message("umxGSEM_GWAS: no I matrix in covstruc; using identity (GC limited).")
	}
	if (is.null(traits)) {
		traits = colnames(covstruc$S)
	}
	covstruc = xmu_gsem_subset_covstruc(covstruc, traits)
	k      = length(traits)

	beta_cols = paste0("beta.", traits)
	se_cols   = paste0("se.", traits)
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
	nSNP      = nrow(SNPs)
	varSNP    = 2 * SNPs$MAF * (1 - SNPs$MAF)
	beta_SNP  = as.matrix(SNPs[, beta_cols, drop = FALSE])
	SE_SNP    = as.matrix(SNPs[, se_cols, drop = FALSE])
	varSNPSE2 = as.numeric(SnpSamplingError)^2
	coords    = as.matrix(expand.grid(seq_len(k), seq_len(k)))

	# Pre-flight: Build template model and optimize starting values
	if (!quiet) message("umxGSEM_GWAS: Generating GWAS template and optimizing starting values...")
	expn_dummy = xmu_gsem_expand_snp(covstruc, as.numeric(beta_SNP[1, ]), as.numeric(SE_SNP[1, ]), varSNP[1], varSNPSE2, GC, coords)
	
	# Default common-factor + SNP
	if (is.null(model)) {
		model = umxRAM("gwas_template",
			umxPath(from = "F1", to = traits),       # All factor loadings free
			umxPath(var = traits),                   # Free trait residual variances
			umxPath(var = "F1", fixedAt = 1),        # Fix factor variance to 1
			umxPath(from = "SNP", to = "F1"),        # SNP effect
			data = mxData(expn_dummy$S, type = "cov", numObs = 1),
			type = "cov", autoRun = FALSE
		)
	} else if (is.character(model)) {
		if (!grepl("SNP", model, fixed = TRUE)) {
			stop("umxGSEM_GWAS: 'SNP' not found in your custom model string. Please explicitly specify the path (e.g., 'F1 ~ SNP' or 'Trait ~ SNP') in your model string.")
		}
		model = umxRAM(model = model, name = "gwas_template", data = mxData(expn_dummy$S, type = "cov", numObs = 1), type = "cov", autoRun = FALSE)
	}

	if (!"SNP" %in% model$manifestVars) {
		stop("umxGSEM_GWAS: the model must include 'SNP' as a manifest variable.")
	}

	# Preallocate result columns (avoid do.call(rbind, ...) over list blobs)
	out_SNP = as.character(SNPs$SNP[seq_len(nSNP)])
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
	out_Q_SNP = rep(NA_real_, nSNP)
	out_Q_SNP_p = rep(NA_real_, nSNP)
	out_se_source = rep(NA_character_, nSNP)

	oldAuto = getOption("umx_auto_plot")
	options(umx_auto_plot = FALSE)
	on.exit(options(umx_auto_plot = oldAuto), add = TRUE)
	oldSilent = umx_set_silent(TRUE, silent = TRUE)
	on.exit(umx_set_silent(oldSilent, silent = TRUE), add = TRUE)

	# umxGSEM natively handles WLS preparation when estimation = "DWLS" or "WLS"
	m_template_err = NULL
	m_template = tryCatch(
		umxGSEM(model = model, S = expn_dummy$S, V = expn_dummy$V, estimation = estimation, autoRun = FALSE, quiet = TRUE, std.lv = FALSE),
		error = function(e) { m_template_err <<- conditionMessage(e); NULL }
	)
	
	if (is.null(m_template)) {
		msg = if (is.null(m_template_err) || !nzchar(m_template_err)) "unknown error" else m_template_err
		stop("Failed to build GWAS template model: ", msg, call. = FALSE)
	}
	m_template = suppressWarnings(mxTryHard(m_template, extraTries = 5, silent = TRUE, checkHess = FALSE, bestInitsOutput = FALSE))

	# Pre-flight for Analytic WLS
	analytic_ok = FALSE
	H_raw = NULL; valid_targets = NULL
	if (estimation %in% c("DWLS", "WLS")) {
		# Extract measurement model directly from m_template by dropping 'SNP'
		keep = c(m_template$latentVars, traits)
		A     = mxMatrix("Full", nrow = length(keep)  , ncol = length(keep), values = m_template$A$values[keep, keep]  , free = m_template$A$free[keep, keep], labels = m_template$A$labels[keep, keep], dimnames = list(keep, keep), name = "A")
		S_mat = mxMatrix("Symm", nrow = length(keep)  , ncol = length(keep), values = m_template$S$values[keep, keep]  , free = m_template$S$free[keep, keep], labels = m_template$S$labels[keep, keep], dimnames = list(keep, keep), name = "S")
		F_mat = mxMatrix("Full", nrow = length(traits), ncol = length(keep), values = m_template$F$values[traits, keep], free = FALSE, dimnames = list(traits, keep), name = "F")
		
		# 1. Fit measurement model on trait-only covstruc (no umxSummary: autoRun=FALSE + silent tryHard)
		m_meas = mxModel("meas_model", type = "RAM", manifestVars = traits, latentVars = m_template$latentVars, 
			mxData(covstruc$S, type = "cov", numObs = 1), 
			A, 
			S_mat, 
			F_mat, 
			mxExpectationRAM("A", "S", "F")
		)
		
		m_meas = tryCatch({
			mm = umxGSEM(model = m_meas, covstruc = covstruc, estimation = estimation, autoRun = FALSE, tryHard = "no", quiet = TRUE, std.lv = FALSE)
			# Point estimates suffice for H_raw; do not require a usable Hessian
			suppressWarnings(mxTryHard(mm, extraTries = 5, silent = TRUE, checkHess = FALSE, bestInitsOutput = FALSE))
		}, error = function(e) NULL)
		
		# 2. Extract targets using a dummy template model
		if (!is.null(m_meas) && inherits(m_meas, "MxModel")) {
			
			# Inject the pure measurement model parameters into m_template
			# This ensures the fallback engine freezes the model at the true base estimates,
			# not at the estimates perturbed by the dummy SNP during mxTryHard
			if (!is.null(m_template$A) && !is.null(m_meas$A)) m_template$A$values[rownames(m_meas$A), colnames(m_meas$A)] = m_meas$A$values
			if (!is.null(m_template$S) && !is.null(m_meas$S)) m_template$S$values[rownames(m_meas$S), colnames(m_meas$S)] = m_meas$S$values
			if (!is.null(m_template$M) && !is.null(m_meas$M)) m_template$M$values[, colnames(m_meas$M)] = m_meas$M$values
			
			if (!is.null(m_template) && "SNP" %in% colnames(m_template$A$free)) {
				targets = rownames(m_template$A$free)[m_template$A$free[, "SNP"]]
				valid_targets = targets[targets %in% rownames(m_meas$A)]
				if (length(valid_targets) > 0 && length(valid_targets) == length(targets)) {
					L = matrix(0, nrow = nrow(m_meas$A), ncol = length(valid_targets))
					for (j in seq_along(valid_targets)) {
						L[which(rownames(m_meas$A) == valid_targets[j]), j] = 1
					}
					# Total effects of target on manifests
					H_raw = m_meas$F$values %*% solve(diag(nrow(m_meas$A)) - m_meas$A$values) %*% L
					rownames(H_raw) = m_meas$manifestVars
					H_raw = H_raw[traits, , drop = FALSE]
					if (!force_fallback) {
						analytic_ok = TRUE
						if (!quiet) message("umxGSEM_GWAS: Enabled Sub-10s Analytic WLS Engine.")
					} else {
						if (!quiet) message("umxGSEM_GWAS: Analytic engine bypassed due to force_fallback = TRUE.")
					}
				}
			}
		}
	}

	vectorized_ok = FALSE
	if (analytic_ok && length(valid_targets) == 1) {
		if (!quiet) message("umxGSEM_GWAS: Vectorizing math for ", nSNP, " SNPs.")
		diag_I_LD = diag(covstruc$I)
		
		# Vectorized computation of robust sandwich SE and Q_SNP
		# Construct constant sampling correlation/covariance matrix M
		k = length(traits)
		M = matrix(0, k, k)
		for (x in 1:k) {
			for (y in 1:k) {
				if (GC == "conserv") {
					if (x != y) M[x,y] = covstruc$I[x,y] * covstruc$I[x,x] * covstruc$I[y,y]
					else M[x,x] = covstruc$I[x,x]^2
				} else if (GC == "standard") {
					if (x != y) M[x,y] = covstruc$I[x,y] * sqrt(covstruc$I[x,x]) * sqrt(covstruc$I[y,y])
					else M[x,x] = covstruc$I[x,x]
				} else {
					M[x,y] = covstruc$I[x,y]
				}
			}
		}
		
		# Bread of sandwich (H2_W_sum corresponds to bread inverse)
		v_snp_diag_mat = sweep(SE_SNP^2, 2, diag(M), "*") * varSNP^2
		W_mat = 1 / v_snp_diag_mat
		H_mat = sweep(matrix(1, nSNP, length(traits)), 2, as.numeric(H_raw), "*") * varSNP
		
		H2_W_sum = rowSums(H_mat^2 * W_mat)
		S_obs_mat = sweep(beta_SNP, 1, varSNP, "*")
		H_W_beta_sum = rowSums(H_mat * W_mat * S_obs_mat)
		
		out_est_vec = H_W_beta_sum / H2_W_sum
		
		# Robust SE (Sandwich)
		if (uncertainty == "robustSE") {
			G_mat = sweep(1 / SE_SNP, 2, as.numeric(H_raw) / diag(M), "*")
			meat_vec = rowSums((G_mat %*% M) * G_mat)
			out_se_vec = sqrt(meat_vec) / H2_W_sum
			out_se_source = rep("analytic_robust", nSNP)
		} else {
			out_se_vec = sqrt(1 / H2_W_sum)
			out_se_source = rep("analytic", nSNP)
		}
		
		# Q_SNP Heterogeneity Statistic
		R_mat = (beta_SNP - outer(out_est_vec, as.numeric(H_raw))) / SE_SNP
		invM = solve(M)
		out_Q_SNP_vec = rowSums((R_mat %*% invM) * R_mat)
		df_Q = k - length(valid_targets)
		out_Q_SNP_p_vec = stats::pchisq(out_Q_SNP_vec, df = df_Q, lower.tail = FALSE)
		
		out_est = out_est_vec
		out_se = out_se_vec
		out_Q_SNP = out_Q_SNP_vec
		out_Q_SNP_p = out_Q_SNP_p_vec
		out_status = rep(0L, nSNP)
		out_fail = rep(FALSE, nSNP)
		
		# Deal with bad SEs/NAs (where sum W -> 0 or NA)
		bad_idx = is.na(out_est) | is.na(out_se) | !is.finite(out_est) | out_se <= 0
		if (any(bad_idx)) {
			out_est[bad_idx] = NA_real_
			out_se[bad_idx] = NA_real_
			out_Q_SNP[bad_idx] = NA_real_
			out_Q_SNP_p[bad_idx] = NA_real_
			out_status[bad_idx] = 3L
			out_fail[bad_idx] = TRUE
		}
		
		valid_Z = is.finite(out_est) & is.finite(out_se) & out_se > 0
		out_Z[valid_Z] = out_est[valid_Z] / out_se[valid_Z]
		out_P[valid_Z] = 2 * stats::pnorm(-abs(out_Z[valid_Z]))
		
		vectorized_ok = TRUE
	}

	cpp_loop_ok = FALSE
	hasCppLoop = exists("mxComputeGSEMLoop", mode = "function")
	if (!vectorized_ok && estimation %in% c("DWLS", "WLS") && hasCppLoop) {
		if (!quiet) message("umxGSEM_GWAS: OpenMx C++ SNP loop for ", nSNP, " SNPs.")
		
		expn1 = xmu_gsem_expand_snp(covstruc, as.numeric(beta_SNP[1, ]), as.numeric(SE_SNP[1, ]), varSNP[1], varSNPSE2[1], GC = GC, coords = coords)
		
		wls_full = xmu_gsem_prepare_WLS(S = expn1$S, V = expn1$V, keep_vars = c(traits, "SNP"), estimation = estimation)
		
		# Vectorize S_obs and W_obs
		S_obs_k = as.matrix(beta_SNP)
		S_obs = sweep(S_obs_k, 1, varSNP, "*")
		
		k_traits = length(traits)
		M = matrix(0, k_traits, k_traits)
		for (x in 1:k_traits) {
			for (y in 1:k_traits) {
				if (GC == "conserv") {
					if (x != y) M[x,y] = covstruc$I[x,y] * covstruc$I[x,x] * covstruc$I[y,y]
					else M[x,x] = covstruc$I[x,x]^2
				} else if (GC == "standard") {
					if (x != y) M[x,y] = covstruc$I[x,y] * sqrt(covstruc$I[x,x]) * sqrt(covstruc$I[y,y])
					else M[x,x] = covstruc$I[x,x]
				} else {
					M[x,y] = covstruc$I[x,y]
				}
			}
		}
		M_inv = solve(M)
		se_mat = as.matrix(SE_SNP)
		inv_se = 1 / se_mat
		
		W_obs = matrix(0, nrow=nSNP, ncol=k_traits*k_traits)
		for(j in 1:k_traits) {
			for(l in 1:k_traits) {
				col_idx = l + (j - 1) * k_traits
				W_obs[, col_idx] = M_inv[l, j] * inv_se[, l] * inv_se[, j] / (varSNP^2)
			}
		}
		
		W_varSNP = 1 / varSNPSE2
		if (length(W_varSNP) == 1) W_varSNP = rep(W_varSNP, nSNP)
		
		m = m_template
		wls_data = xmu_mxData_summary(
			numObs = covstruc$N[1],
			observedStats = list(
				cov = wls_full$S,
				useWeight = wls_full$W_omx,
				asymCov = wls_full$V_omx
			)
		)
		m = mxModel(m, wls_data, mxFitFunctionWLS(type = estimation))

		
		if (fix_measurement) {
			if (!is.null(m$A)) {
				is_snp_col = (colnames(m$A) == "SNP")
				is_snp_row = (rownames(m$A) == "SNP")
				if (any(is_snp_col) || any(is_snp_row)) {
					snp_free_A = m$A$free
					snp_free_A[!is_snp_row, !is_snp_col] = FALSE
					m$A$free = snp_free_A
					
					m$A$values[is_snp_row, ] = 0
					m$A$values[, is_snp_col] = 0
				}
			}
			if (!is.null(m$S)) {
				is_snp_col = (colnames(m$S) == "SNP")
				is_snp_row = (rownames(m$S) == "SNP")
				if (any(is_snp_col) || any(is_snp_row)) {
					snp_free_S = m$S$free
					snp_free_S[!is_snp_row, !is_snp_col] = FALSE
					m$S$free = snp_free_S
					
					if (any(is_snp_row) && any(is_snp_col)) {
						if (!is.null(wls_full$S) && "SNP" %in% rownames(wls_full$S)) {
							m$S$values[is_snp_row, is_snp_col] = wls_full$S["SNP", "SNP"]
						}
					}
				}
			}
			if (!is.null(m$M)) {
				is_snp_col = (colnames(m$M) == "SNP")
				if (any(is_snp_col)) {
					snp_free_M = m$M$free
					snp_free_M[, !is_snp_col] = FALSE
					m$M$free = snp_free_M
				}
			}
		}
		
		subplan = mxComputeSequence(list(
			mxComputeGradientDescent(),
			mxComputeStandardError()
		))
		
		target_params = rep(NA_character_, length(valid_targets))
		if (!is.null(m$A) && "SNP" %in% colnames(m$A)) {
			for (kk in seq_along(valid_targets)) {
				lab = m$A$labels[valid_targets[kk], "SNP"]
				if (!is.na(lab) && nzchar(lab)) {
					target_params[kk] = lab
				} else {
					target_params[kk] = valid_targets[kk]
				}
			}
		} else {
			target_params = valid_targets
		}

		loopArgs = list(
			step = subplan,
			S_obs = S_obs,
			W_obs = W_obs,
			W_varSNP = W_varSNP,
			targets = target_params
		)
		if ("varSNP" %in% names(formals(mxComputeGSEMLoop))) {
			loopArgs$varSNP = as.numeric(varSNP)
		}
		m = mxModel(m, do.call(mxComputeGSEMLoop, loopArgs))
		
		fit = tryCatch(mxRun(m, silent = TRUE, suppressWarnings = TRUE), error = function(e) e)
		
		if (!inherits(fit, "error") && !is.null(fit$compute$output$gsem_est)) {
			est_mat = fit$compute$output$gsem_est
			se_mat = fit$compute$output$gsem_se
			status_vec = fit$compute$output$gsem_status
			
			out_est = est_mat[, 1]
			out_se = se_mat[, 1]
			out_status = status_vec
			out_fail = (out_status != 0L & out_status != 1L)
			out_se_source = rep("cpp_loop", nSNP)
			
			bad_se = is.finite(out_status) & out_fail & (is.finite(out_se) & is.finite(out_est) & out_se < 1e-3 * pmax(1e-6, abs(out_est)))
			out_se[bad_se] = NA_real_
			out_se_source[bad_se] = "dropped_bad_status"
			
			valid_Z = is.finite(out_est) & is.finite(out_se) & out_se > 0
			out_Z[valid_Z] = out_est[valid_Z] / out_se[valid_Z]
			out_P[valid_Z] = 2 * stats::pnorm(-abs(out_Z[valid_Z]))
			cpp_loop_ok = TRUE
		} else if (!quiet) {
			msg = if (inherits(fit, "error")) conditionMessage(fit) else "C++ loop returned no gsem_est"
			message("umxGSEM_GWAS: C++ SNP loop unavailable (", msg, "); using per-SNP R path.")
		}
	} else if (!vectorized_ok && estimation %in% c("DWLS", "WLS") && !hasCppLoop && !quiet) {
		message("umxGSEM_GWAS: mxComputeGSEMLoop not in this OpenMx build; using per-SNP R path.")
	}

	for (i in seq_len(nSNP)) {
		if (vectorized_ok || cpp_loop_ok) next
		
		if (!quiet && (i == 1L || i %% 5000L == 0L || i == nSNP)) {
			message("umxGSEM_GWAS: SNP ", i, " / ", nSNP)
		}
		out_SNP[i] = as.character(SNPs$SNP[i])
		expn = xmu_gsem_expand_snp(
			covstruc = covstruc,
			beta_i = as.numeric(beta_SNP[i, ]),
			se_i = as.numeric(SE_SNP[i, ]),
			varSNP_i = varSNP[i],
			varSNPSE2 = varSNPSE2,
			GC = GC, coords = coords
		)
		
		if (analytic_ok) {
			wls_data = xmu_gsem_prepare_WLS(S = expn$S, V = expn$V, keep_vars = c("SNP", traits), estimation = estimation)
			all_names = colnames(wls_data$V_omx)
			snp_cov_names = all_names[grepl("poly_", all_names) & grepl("SNP", all_names)]
			
			S_obs_snp = wls_data$S[traits, "SNP"]
			W_snp = wls_data$W_omx[snp_cov_names, snp_cov_names]
			if (!is.matrix(W_snp)) { W_snp = diag(W_snp, nrow=length(W_snp)) }
			
			H = H_raw * varSNP[i]
			H_t_W = t(H) %*% W_snp
			H_t_W_H = H_t_W %*% H
			
			res = tryCatch({
				b = solve(H_t_W_H) %*% H_t_W %*% S_obs_snp
				if (uncertainty == "robustSE") {
					meat = H_t_W %*% wls_data$V_omx[snp_cov_names, snp_cov_names] %*% t(H_t_W)
					cov_sandwich = solve(H_t_W_H) %*% meat %*% solve(H_t_W_H)
					se = sqrt(diag(cov_sandwich))
					se_source = "analytic_robust"
				} else {
					se = sqrt(diag(solve(H_t_W_H)))
					se_source = "analytic"
				}
				
				# Q_SNP Heterogeneity Statistic
				residuals = S_obs_snp - (H %*% b)
				V_snp = wls_data$V_omx[snp_cov_names, snp_cov_names]
				if (!is.matrix(V_snp)) { V_snp = diag(V_snp, nrow=length(V_snp)) }
				q_snp_val = as.numeric(t(residuals) %*% solve(V_snp) %*% residuals)
				df_q = length(traits) - length(valid_targets)
				q_snp_pval = stats::pchisq(q_snp_val, df = df_q, lower.tail = FALSE)
				
				# Extract target logic (usually "F1") matching xmu_gsem_extract_snp_path
				target_idx = 1
				if ("F1" %in% valid_targets) target_idx = which(valid_targets == "F1")
				
				list(est = b[target_idx, 1], se = se[target_idx], q_snp = q_snp_val, q_snp_p = q_snp_pval, se_src = se_source, status = 0L)
			}, error = function(e) list(est = NA_real_, se = NA_real_, q_snp = NA_real_, q_snp_p = NA_real_, se_src = NA_character_, status = 3L))
			
			out_est[i] = res$est
			out_se[i] = res$se
			out_Q_SNP[i] = res$q_snp
			out_Q_SNP_p[i] = res$q_snp_p
			out_status[i] = res$status
			out_fail[i] = FALSE
			out_se_source[i] = res$se_src
			
			if (is.finite(out_est[i]) && is.finite(out_se[i]) && out_se[i] > 0) {
				out_Z[i] = out_est[i] / out_se[i]
				out_P[i] = 2 * stats::pnorm(-abs(out_Z[i]))
			}
		} else {
			fit = tryCatch({
				# Always start from clean template (never reuse a failed C++-loop model)
				m = mxModel(m_template, name = paste0("gwas_", i))
				m@compute = NULL
				if (estimation %in% c("DWLS", "WLS")) {
					wls_i = xmu_gsem_prepare_WLS(S = expn$S, V = expn$V, keep_vars = m_template$manifestVars, estimation = estimation)
					# Ensure DWLS weight is strictly diagonal (OpenMx rejects off-diagonals)
					W_use = wls_i$W_omx
					if (!is.null(W_use) && estimation == "DWLS") {
						W_use = diag(diag(as.matrix(W_use)), nrow = nrow(W_use), ncol = ncol(W_use))
						dimnames(W_use) = dimnames(wls_i$W_omx)
					}
					if (!is.null(W_use)) {
						wls_data = xmu_mxData_summary(numObs = covstruc$N[1], observedStats = list(cov = wls_i$S, useWeight = W_use, asymCov = wls_i$V_omx))
					} else {
						wls_data = xmu_mxData_summary(numObs = covstruc$N[1], observedStats = list(cov = wls_i$S, asymCov = wls_i$V_omx))
					}
					m = mxModel(m, wls_data, mxFitFunctionWLS(type = estimation))
				} else {
					S_subset = expn$S[m_template$manifestVars, m_template$manifestVars, drop=FALSE]
					m = mxModel(m, mxData(S_subset, type = "cov", numObs = covstruc$N[1]))
				}
				
				# Emulate the analytic engine: Freeze all measurement model parameters
				# Only unfreeze paths directly involving the SNP, and reset their starting values to 0
				if (fix_measurement) {
					if (!is.null(m$A)) {
						is_snp_col = (colnames(m$A) == "SNP")
						is_snp_row = (rownames(m$A) == "SNP")
						if (any(is_snp_col) || any(is_snp_row)) {
							snp_free_A = m$A$free
							snp_free_A[!is_snp_row, !is_snp_col] = FALSE
							m$A$free = snp_free_A
							
							# Reset SNP path starting values to 0 to escape local minima inherited from m_template
							m$A$values[is_snp_row, ] = 0
							m$A$values[, is_snp_col] = 0
						}
					}
					if (!is.null(m$S)) {
						is_snp_col = (colnames(m$S) == "SNP")
						is_snp_row = (rownames(m$S) == "SNP")
						if (any(is_snp_col) || any(is_snp_row)) {
							snp_free_S = m$S$free
							snp_free_S[!is_snp_row, !is_snp_col] = FALSE
							m$S$free = snp_free_S
							
							# Fix SNP variance to the observed variance
							if (any(is_snp_row) && any(is_snp_col)) {
								if (!is.null(wls_i$S) && "SNP" %in% rownames(wls_i$S)) {
									m$S$values[is_snp_row, is_snp_col] = wls_i$S["SNP", "SNP"]
								}
							}
						}
					}
					if (!is.null(m$M)) {
						is_snp_col = (colnames(m$M) == "SNP")
						if (any(is_snp_col)) {
							snp_free_M = m$M$free
							snp_free_M[, !is_snp_col] = FALSE
							m$M$free = snp_free_M
						}
					}
				}
				
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
				est_se = xmu_gsem_extract_snp_path(fit, traits, snpEffect)
				out_est[i] = est_se$est
				out_se[i] = est_se$se
				out_se_source[i] = if (!is.null(est_se$se_source)) est_se$se_source else NA_character_
				# Unreliable SE when optimizer did not converge cleanly
				if (is.finite(out_status[i]) && !(out_status[i] %in% c(0L, 1L))) {
					if (is.finite(out_se[i]) && is.finite(out_est[i]) && out_se[i] < 1e-3 * max(1e-6, abs(out_est[i]))) {
						out_se[i] = NA_real_
						out_se_source[i] = "dropped_bad_status"
					}
				}
				if (is.finite(out_est[i]) && is.finite(out_se[i]) && out_se[i] > 0) {
					out_Z[i] = out_est[i] / out_se[i]
					out_P[i] = 2 * stats::pnorm(-abs(out_Z[i]))
				}
			}
		}
	}
	res = data.frame(
		SNP = out_SNP, CHR = out_CHR, BP = out_BP, MAF = out_MAF,
		A1 = out_A1, A2 = out_A2,
		est = out_est, se = out_se, Z = out_Z, P = out_P,
		Q_SNP = out_Q_SNP, Q_SNP_p = out_Q_SNP_p,
		status = out_status, fail = out_fail, warning = out_warn,
		se_source = out_se_source,
		stringsAsFactors = FALSE
	)
	class(res) = c("umx_GWAS", "data.frame")
	res
}


#' Simulate GWAS Summary Statistics for Genomic SEM
#'
#' @description
#' Generates a simulated summary statistics dataset (SNPs) suitable for testing \code{\link{umxGSEM_GWAS}}.
#'
#' @param n Number of SNPs to simulate. Default is 1,000,000.
#' @param traits A character vector of traits to generate betas and SEs for.
#' @param seed Random seed for reproducibility.
#'
#' @return A `data.frame` with columns for SNP, CHR, BP, A1, A2, MAF, and `beta.[trait]`/`se.[trait]` for each trait.
#' @export
#' @family Data Functions
#' @md
#' @examples
#' snps = umxGSEM_sim_snps(n = 1000, traits = c("SCZ", "BIP", "MDD"))
#' head(snps)
#'
umxGSEM_sim_snps <- function(n = 1000000, traits = c("SCZ", "BIP", "MDD"), seed = 42) {
	if (!is.null(seed)) {
		if (exists(".Random.seed", envir = .GlobalEnv)) {
			old_seed <- get(".Random.seed", envir = .GlobalEnv)
			on.exit(assign(".Random.seed", old_seed, envir = .GlobalEnv))
		} else {
			on.exit(rm(".Random.seed", envir = .GlobalEnv))
		}
		set.seed(seed)
	}
	
	snps = data.frame(
		SNP = paste0("rs", seq_len(n)),
		CHR = sample(1:22, n, replace = TRUE),
		BP  = sample(1:100000000, n, replace = TRUE),
		A1  = sample(c("A", "C", "G", "T"), n, replace = TRUE),
		A2  = sample(c("A", "C", "G", "T"), n, replace = TRUE),
		MAF = runif(n, 0.01, 0.5),
		stringsAsFactors = FALSE
	)
	
	# Prevent A1 and A2 from being identical
	same_allele = snps$A1 == snps$A2
	if (any(same_allele)) {
		alts = c("A", "C", "G", "T")
		snps$A2[same_allele] = vapply(snps$A1[same_allele], function(a) {
			sample(setdiff(alts, a), 1)
		}, FUN.VALUE = character(1))
	}
	
	for (t in traits) {
		# Tiny jitter so SNPs are mathematically unique
		snps[[paste0("beta.", t)]] = rnorm(n, mean = 0, sd = 0.05)
		snps[[paste0("se.", t)]] = abs(rnorm(n, mean = 0.01, sd = 0.005)) + 0.001
	}
	
	return(snps)
}

#' Label or Assemble S, V, I, N into an LDSC-style covstruc list
#'
#' Takes an existing `covstruc` list (from GenomicSEM) OR loose `S` and `V` matrices,
#' validates them, and applies strict OpenMx WLS-compliant dimnames (`var_X`, `poly_Y_X`).
#' This guarantees that external GenomicSEM data correctly aligns with OpenMx's WLS engine.
#'
#' @param covstruc An optional list with at least matrix `S` (genetic covariance).
#' @param S A genetic covariance matrix (if assembling from loose matrices).
#' @param V An asymptotic covariance matrix (if assembling from loose matrices).
#' @param I An optional intercept matrix.
#' @param N An optional sample size matrix or vector.
#' @param traits Character vector of trait names. Default: `colnames(S)`, else
#'   `rownames(S)`, else `paste0("T", seq_len(ncol(S)))`.
#' @param overwrite If `FALSE` (default), only fill missing dimnames; if `TRUE`,
#'   replace existing dimnames.
#' @return A list with matrices `S`, `V`, (and optionally `I`, `N`) properly labelled.
#' @export
#' @family GSEM
#' @seealso [Psych_LDSC], [umxGSEM()]
#' @examples
#' data(Psych_LDSC)
#' # Re-apply labels safely
#' Psych_LDSC = umxGSEM_label_ldsc(Psych_LDSC)
#' colnames(Psych_LDSC$V)[1:3]
umxGSEM_label_ldsc <- function(covstruc = NULL, S = NULL, V = NULL, I = NULL, N = NULL, traits = NULL, overwrite = FALSE) {
	if (is.null(covstruc)) {
		if (is.null(S) || is.null(V)) stop("umxGSEM_label_ldsc: You must provide either a 'covstruc' list, or both 'S' and 'V' matrices.")
		covstruc = list(S = S, V = V)
		if (!is.null(I)) covstruc$I = I
		if (!is.null(N)) covstruc$N = N
	}
	if (!is.list(covstruc) || is.null(covstruc$S)) stop("covstruc must be a list with a genetic covariance matrix S.")
	
	S_mat = as.matrix(covstruc$S)
	k = ncol(S_mat)
	if (is.null(traits)) {
		traits = colnames(S_mat)
		if (is.null(traits)) traits = rownames(S_mat)
		if (is.null(traits)) traits = paste0("T", seq_len(k))
	}
	traits = as.character(traits)
	if (length(traits) != k) stop("traits must have length ncol(S) = ", k, ".")
	
	need_S = overwrite || is.null(colnames(S_mat)) || is.null(rownames(S_mat))
	if (need_S) {
		colnames(S_mat) = rownames(S_mat) = traits
		covstruc$S = S_mat
	} else {
		traits = colnames(S_mat)
	}
	
	if (exists("omxNameWLS_V", where = asNamespace("OpenMx"), mode = "function")) {
		vech = OpenMx::omxNameWLS_V(traits)
	} else {
		k_len <- length(traits)
		vech <- character(0)
		if (k_len >= 1) {
			for (j in seq_len(k_len)) {
				for (i in j:k_len) {
					if (i == j) vech <- c(vech, paste0("var_", traits[i]))
					else vech <- c(vech, paste0("poly_", traits[i], "_", traits[j]))
				}
			}
		}
	}
	z = length(vech)

	align_vech_matrix <- function(mat, vech, is_vector = FALSE) {
		if (is_vector || (is.matrix(mat) && nrow(mat) == 1)) {
			if (is.matrix(mat)) {
				if (overwrite || is.null(colnames(mat))) { colnames(mat) = vech; return(mat) }
				mat_names = colnames(mat)
			} else {
				if (overwrite || is.null(names(mat))) { names(mat) = vech; return(mat) }
				mat_names = names(mat)
			}
		} else {
			if (overwrite || is.null(colnames(mat))) { colnames(mat) = rownames(mat) = vech; return(mat) }
			mat_names = colnames(mat)
		}
		orig_keys = character(0)
		for (key in vech) {
			if (key %in% mat_names) { orig_keys = c(orig_keys, key) }
			else if (grepl("^poly_", key)) {
				parts = strsplit(sub("^poly_", "", key), "_")[[1]]
				rev_key = paste0("poly_", parts[2], "_", parts[1])
				if (rev_key %in% mat_names) orig_keys = c(orig_keys, rev_key)
				else {
					l1 = paste0(parts[1], "_", parts[2])
					l2 = paste0(parts[2], "_", parts[1])
					if (l1 %in% mat_names) orig_keys = c(orig_keys, l1)
					else if (l2 %in% mat_names) orig_keys = c(orig_keys, l2)
					else orig_keys = c(orig_keys, NA_character_)
				}
			} else if (grepl("^var_", key)) {
				l = sub("^var_", "", key)
				if (l %in% mat_names) orig_keys = c(orig_keys, l)
				else orig_keys = c(orig_keys, NA_character_)
			} else orig_keys = c(orig_keys, NA_character_)
		}
		if (anyNA(orig_keys)) {
			if (is_vector || (is.matrix(mat) && nrow(mat) == 1)) {
				if (is.matrix(mat)) colnames(mat) = vech
				else names(mat) = vech
			} else colnames(mat) = rownames(mat) = vech
			return(mat)
		}
		if (is_vector) {
			mat = mat[orig_keys]
			names(mat) = vech
		} else if (is.matrix(mat) && nrow(mat) == 1) {
			mat = mat[, orig_keys, drop = FALSE]
			colnames(mat) = vech
		} else {
			mat = mat[orig_keys, orig_keys, drop = FALSE]
			colnames(mat) = rownames(mat) = vech
		}
		mat
	}

	if (!is.null(covstruc$I) && is.matrix(covstruc$I)) {
		I_mat = as.matrix(covstruc$I)
		if (overwrite || is.null(colnames(I_mat)) || is.null(rownames(I_mat))) {
			if (nrow(I_mat) == k && ncol(I_mat) == k) {
				colnames(I_mat) = rownames(I_mat) = traits
				covstruc$I = I_mat
			}
		}
	}
	if (!is.null(covstruc$V) && is.matrix(covstruc$V)) {
		V_mat = as.matrix(covstruc$V)
		if (nrow(V_mat) == z && ncol(V_mat) == z) {
			covstruc$V = align_vech_matrix(V_mat, vech, is_vector = FALSE)
		} else warning("V is ", nrow(V_mat), "x", ncol(V_mat), " but k(k+1)/2 = ", z, "; left V dimnames unchanged.", call. = FALSE)
	}
	if (!is.null(covstruc$N)) {
		N_mat = covstruc$N
		if (is.matrix(N_mat) && ncol(N_mat) == z) covstruc$N = align_vech_matrix(N_mat, vech, is_vector = FALSE)
		else if (is.vector(N_mat) && length(N_mat) == z) covstruc$N = align_vech_matrix(N_mat, vech, is_vector = TRUE)
	}
	if (!is.null(covstruc$S_Stand) && is.matrix(covstruc$S_Stand)) {
		Ss = as.matrix(covstruc$S_Stand)
		if (overwrite || is.null(colnames(Ss))) {
			if (ncol(Ss) == k) { colnames(Ss) = rownames(Ss) = traits; covstruc$S_Stand = Ss }
		}
	}
	if (!is.null(covstruc$V_Stand) && is.matrix(covstruc$V_Stand)) {
		Vs = as.matrix(covstruc$V_Stand)
		if (nrow(Vs) == z && ncol(Vs) == z) covstruc$V_Stand = align_vech_matrix(Vs, vech, is_vector = FALSE)
	}
	covstruc
}

# =============================================================================
# umxSummary for GSEM/LDSC covstruc lists and WLS mxData (observedStats)
# =============================================================================

#' Test whether an object is a Genomic SEM / LDSC covariance structure list
#'
#' @param x Object to test.
#' @return Logical: TRUE if `x` is a list with matrix elements `S` and `V`.
#' @keywords internal
xmu_is_gsem_covstruc <- function(x) {
	is.list(x) && !is.null(x$S) && is.matrix(x$S) && !is.null(x$V) && is.matrix(x$V)
}

#' Format a numeric matrix for [umx_print]
#'
#' @param M Matrix or vector.
#' @param rowName Header for the row-name column (default `" "`).
#' @return data.frame with an explicit first column of row labels.
#' @keywords internal
xmu_matrix_for_print <- function(M, rowName = " ") {
	if (is.null(M)) {
		return(NULL)
	}
	if (is.vector(M) && !is.matrix(M)) {
		nm = names(M)
		M = matrix(as.numeric(M), nrow = 1L)
		if (!is.null(nm)) {
			colnames(M) = nm
		}
		rownames(M) = "value"
	}
	M = as.matrix(M)
	rn = rownames(M)
	if (is.null(rn)) {
		rn = as.character(seq_len(nrow(M)))
	}
	cn = colnames(M)
	if (is.null(cn)) {
		cn = as.character(seq_len(ncol(M)))
	}
	df = data.frame(rn, M, check.names = FALSE, stringsAsFactors = FALSE)
	names(df) = c(rowName, cn)
	rownames(df) = NULL
	df
}

#' Print one named matrix block for covstruc / WLS data summaries
#'
#' @param M Matrix.
#' @param title Caption / section title.
#' @param digits Rounding digits.
#' @param report `"markdown"` or `"html"`.
#' @param rowName First-column header for row labels.
#' @keywords internal
xmu_umxSummary_print_matrix <- function(M, title, digits = 3, report = c("markdown", "html"), rowName = " ") {
	report = match.arg(report)
	if (is.null(M)) {
		return(invisible(NULL))
	}
	df = xmu_matrix_for_print(M, rowName = rowName)
	if (is.null(df)) {
		return(invisible(NULL))
	}
	# Round numeric columns only
	for (j in seq_along(df)) {
		if (is.numeric(df[[j]])) {
			df[[j]] = round(df[[j]], digits)
		}
	}
	umx_print(df, digits = digits, caption = title, report = report, na.print = "NA", zero.print = "0")
	invisible(df)
}

#' Summarize a Genomic SEM / LDSC covariance structure (covstruc)
#'
#' Pretty-prints the trait list and the main LDSC/GSEM matrices (`S`, `I`, `V`,
#' `N`, and scalar `m`) for objects like [Psych_LDSC] or the return value of
#' [umxGSEM_ldsc()] / [imxLDSC()]. Tables use markdown or HTML via [umx_print].
#'
#' @param model A covstruc list with at least matrices `S` (genetic cov) and
#'   `V` (sampling cov of vech(S)). Optional: `I`, `N`, `m`, `S_Stand`, `V_Stand`.
#' @param digits Rounding for printed cells (default 3).
#' @param report `"markdown"` (default) or `"html"` (browser-friendly kable).
#' @param matrices Character vector of which components to show (default
#'   `c("S", "I", "V", "N")`). Unknown names are ignored. Scalar `m` is always
#'   mentioned in the header when present.
#' @param traits Optional character vector of trait names to subset (default = NULL, which shows all).
#' @param ... Not used (S3 compatibility).
#' @return Invisibly returns the input `model` (covstruc).
#' @export
#' @method umxSummary list
#' @family GSEM
#' @family Model Summary
#' @seealso [umxSummary.MxDataStatic()], [Psych_LDSC], [umxGSEM()], [umx_print]
#' @examples
#' data(Psych_LDSC)
#' umxSummary(Psych_LDSC)
#' \dontrun{
#' umxSummary(Psych_LDSC, traits= c("MDD", "SCZ"), digits=5)
#' umxSummary(Psych_LDSC, report = "html")
#' }
umxSummary.list <- function(model, digits = 3, report = c("markdown", "html"), matrices = c("S", "I", "V", "N"), traits = NULL, ...) {
	report = match.arg(report)
	if (!xmu_is_gsem_covstruc(model)) {
		stop(
			"umxSummary() for lists expects a GSEM/LDSC covstruc: a list with matrices S and V\n",
			"  (e.g. data(Psych_LDSC); umxSummary(Psych_LDSC)).\n",
			"  This list has names: ", paste(names(model), collapse = ", "),
			call. = FALSE
		)
	}

	if (!is.null(traits)) {
		model = xmu_gsem_subset_covstruc(model, keep_vars = traits)
	}

	S = as.matrix(model$S)
	traits = colnames(S)

	if (is.null(traits)) {
		traits = rownames(S)
	}
	if (is.null(traits)) {
		traits = paste0("T", seq_len(ncol(S)))
	}
	k = length(traits)
	z = k * (k + 1L) / 2L

	cat("\n")
	cat("### Genomic SEM / LDSC covariance structure\n\n")
	cat(sprintf("- **Traits (k = %d):** %s\n", k, paste(traits, collapse = ", ")))
	cat(sprintf("- **vech(S) length:** %d unique elements (lower triangle including diagonal)\n", z))
	if (!is.null(model$m)) {
		cat(sprintf("- **SNPs used in LDSC (m):** %s\n", paste(as.character(model$m), collapse = ", ")))
	}
	cat("\n")
	cat("These matrices feed OpenMx WLS via modern `mxData`:\n\n")
	cat("```r\n")
	cat("mxData(numObs = N, type = \"summary\",\n")
	cat("       observedStats = list(cov = S, useWeight = W, asymCov = V))\n")
	cat("# DWLS: W = diag(1/diag(V));  WLS: W = solve(V)\n")
	cat("```\n\n")

	want = unique(as.character(matrices))
	# Always print S first if requested
	if ("S" %in% want && !is.null(model$S)) {
		xmu_umxSummary_print_matrix(
			model$S, digits = digits, report = report, rowName = "Trait",
			title = "S: Genetic covariance matrix (LDSC; diagonal ≈ SNP heritability on LDSC scale)"
		)
		cat("\n")
	}
	if ("I" %in% want && !is.null(model$I)) {
		xmu_umxSummary_print_matrix(
			model$I, digits = digits, report = report, rowName = "Trait",
			title = "I: LDSC intercept matrix (diag ≈ 1; off-diag = sample overlap / confounding)"
		)
		cat("\n")
	}
	if ("V" %in% want && !is.null(model$V)) {
		xmu_umxSummary_print_matrix(
			model$V, digits = digits, report = report, rowName = "vech",
			title = "V: Sampling covariance of vech(S) (asymptotic cov / asymCov for WLS; residual-pair labels)"
		)
		cat("\n")
	}
	if ("N" %in% want && !is.null(model$N)) {
		xmu_umxSummary_print_matrix(
			model$N, digits = digits, report = report, rowName = " ",
			title = "N: Effective sample size for each unique S element (vech order)"
		)
		cat("\n")
	}
	if ("S_Stand" %in% want && !is.null(model$S_Stand)) {
		xmu_umxSummary_print_matrix(
			model$S_Stand, digits = digits, report = report, rowName = "Trait",
			title = "S_Stand: Standardized genetic covariance / correlation (if provided)"
		)
		cat("\n")
	}
	if ("V_Stand" %in% want && !is.null(model$V_Stand)) {
		xmu_umxSummary_print_matrix(
			model$V_Stand, digits = digits, report = report, rowName = "vech",
			title = "V_Stand: Sampling covariance of standardized S elements (if provided)"
		)
		cat("\n")
	}

	invisible(model)
}

#' Summarize WLS / summary-statistic [OpenMx::mxData] objects
#'
#' Explains modern OpenMx WLS data for users who have not seen `type = "summary"`
#' and `observedStats` (`cov`, `useWeight`, `asymCov`). Works for
#' `MxDataStatic` from [OpenMx::mxData] (including GSEM-style summary data).
#'
#' @param model An [OpenMx::mxData] object (`MxDataStatic`).
#' @param digits Rounding for printed cells (default 3).
#' @param report `"markdown"` (default) or `"html"`.
#' @param matrices Which observedStats blocks to print: any of
#'   `c("cov", "useWeight", "asymCov", "means", "thresholds")`.
#'   Default prints `cov`, `useWeight`, and `asymCov` when present.
#' @param ... Not used (S3 compatibility).
#' @return Invisibly returns the input `model`.
#' @export
#' @method umxSummary MxDataStatic
#' @family GSEM
#' @family Model Summary
#' @seealso [umxSummary.list()], [mxData], [mxFitFunctionWLS], [umxGSEM]
#' @examples
#' data(Psych_LDSC)
#' S = Psych_LDSC$S[1:3, 1:3]
#' V = Psych_LDSC$V[1:6, 1:6]
#' W = diag(1 / diag(V))
#' dimnames(W) = dimnames(V)
#' d = mxData(numObs = 1, type = "summary",
#'            observedStats = list(cov = S, useWeight = W, asymCov = V))
#' umxSummary(d)
umxSummary.MxDataStatic <- function(model, digits = 3, report = c("markdown", "html"),
	matrices = c("cov", "useWeight", "asymCov"), ...) {
	report = match.arg(report)
	dtype = tryCatch(model$type, error = function(e) NA_character_)
	numObs = tryCatch(model$numObs, error = function(e) NA_real_)
	os = tryCatch(model$observedStats, error = function(e) NULL)
	if (is.null(os) || length(os) == 0) {
		os = list()
	}

	cat("\n")
	cat("### OpenMx WLS / summary data (`mxData`)\n\n")
	cat(sprintf("- **type:** `%s`\n", dtype))
	if (is.finite(numObs)) {
		cat(sprintf("- **numObs:** %s\n", format(numObs, scientific = FALSE)))
	}

	# Trait / residual labels
	traits = NULL
	if (!is.null(os$cov) && is.matrix(os$cov)) {
		traits = colnames(os$cov)
		if (is.null(traits)) {
			traits = rownames(os$cov)
		}
	}
	if (is.null(traits) && !is.null(model$observed) && is.matrix(model$observed)) {
		traits = colnames(model$observed)
	}
	if (!is.null(traits) && length(traits) > 0) {
		cat(sprintf("- **Variables / traits (k = %d):** %s\n", length(traits), paste(traits, collapse = ", ")))
	}

	hasW = !is.null(os$useWeight)
	hasV = !is.null(os$asymCov)
	hasCov = !is.null(os$cov)
	cat(sprintf("- **observedStats present:** cov=%s, useWeight=%s, asymCov=%s",
		hasCov, hasW, hasV))
	if (!is.null(os$means)) cat(", means=TRUE")
	if (!is.null(os$thresholds)) cat(", thresholds=TRUE")
	cat("\n\n")

	cat("**How to read this structure (modern WLS route)**\n\n")
	cat("| Slot | Role |\n|------|------|\n")
	cat("| `observedStats$cov` | Observed (or genetic) covariance of variables |\n")
	cat("| `observedStats$useWeight` | WLS weight **W** (DWLS: diagonal; WLS: dense) |\n")
	cat("| `observedStats$asymCov` | Sampling covariance of residual moments (often called **V** / NACOV) |\n")
	cat("| `type = \"summary\"` | No raw rows; moments only (GSEM / LDSC style) |\n\n")
	cat("Legacy `type=\"acov\"` / top-level `acov=` formals are removed; do not use them.\n\n")

	want = unique(as.character(matrices))
	titles = c(
		cov = "observedStats$cov: observed / genetic covariance of variables",
		useWeight = "observedStats$useWeight: WLS weight matrix W (estimation metric)",
		asymCov = "observedStats$asymCov: asymptotic sampling cov of residual moments (V)",
		means = "observedStats$means: observed means (if modelled)",
		thresholds = "observedStats$thresholds: ordinal thresholds in the data (if present)"
	)

	for (key in want) {
		mat = os[[key]]
		if (is.null(mat)) {
			next
		}
		ttl = if (!is.null(titles[[key]])) titles[[key]] else paste0("observedStats$", key)
		rowLab = if (key %in% c("useWeight", "asymCov")) "moment" else "var"
		xmu_umxSummary_print_matrix(mat, title = ttl, digits = digits, report = report, rowName = rowLab)
		cat("\n")
	}

	# If type is cov/cor with observed matrix but no observedStats cov printed
	if (!hasCov && !is.null(model$observed) && is.matrix(model$observed) && "cov" %in% want) {
		xmu_umxSummary_print_matrix(
			model$observed,
			title = paste0("observed: type='", dtype, "' covariance/correlation matrix"),
			digits = digits, report = report, rowName = "var"
		)
		cat("\n")
	}

	invisible(model)
}

# Alias if class is plain MxData in some builds
#' @export
#' @method umxSummary MxData
umxSummary.MxData <- umxSummary.MxDataStatic


