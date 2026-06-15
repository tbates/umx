# An institution is the lengthened shadow of one man. Emerson.

# TODO: define a folder structure for gsem projects in umx "project/data/"
umx_CheckProject <- function(project_path = "/path/to/my/project") {
    message("Status of Project folder: ")
	# TODO: does the user have the downloaded HM3 files etc needed?
	# TODO: is munge done? and they have the files needed for ldsc?
	# TODO: is ldsc done and that object is ready?
	# TODO: is the SNP file done?
  }

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
#' @family Genomic SEM Functions
#' @md
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
#' @details TBD
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
#' @family Genomic SEM Functions
#' @md
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


umxGSEM_FindData <- function(mode = c("Benchmark", "Synthetic", "MissingData", "EBI", "ATLAS"), output_dir = getwd(), ...) {
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
#' @param model An OpenMx model, a list of umxPath's, or a lavaan-style model string.
#' @param covstruc A list containing S (genetic covariance matrix) and V (sampling covariance matrix) as output by GenomicSEM's `ldsc` function. If provided, `S` and `V` are extracted from it.
#' @param S A genetic covariance matrix.
#' @param V A sampling covariance matrix (asymptotic covariance of S).
#' @param estimation The estimation method. One of "DWLS", "WLS", or "ULS" (defaults to "DWLS").
#' @param name The name of the model (defaults to "gsem").
#' @param numObs The dummy/actual sample size for WLS (defaults to 2, as commonly used in GenomicSEM DWLS).
#' @param smooth whether to smooth non-positive definite matrices using [Matrix::nearPD()] (defaults to TRUE).
#' @param autoRun Whether to run the model (defaults to getOption("umx_auto_run")).
#' @param tryHard Method for fitting the model ("no", "yes", "ordinal", "search").
#' @param ... Additional arguments passed to [umxRAM()].
#' @return An [OpenMx::mxModel()] object.
#' @export
#' @family Genomic SEM Functions
#' @references
#' Grotzinger, A. D., Rhemtulla, M., de Vlaming, R., Ritchie, G. R., Mallard, T. T., Hill, W. D., ... & Tucker-Drob, E. M. (2019). Genomic structural equation modeling. *Nature Human Behaviour*, **3**, 513-525. \doi{10.1038/s41562-019-0566-x}
#' @md
#' @examples
#' \dontrun{
#' # A simple heritability and genetic correlation example using simulated S and V
#' traits <- c("T1", "T2")
#' S <- matrix(c(0.25, 0.15, 0.15, 0.30), nrow = 2, ncol = 2, dimnames = list(traits, traits))
#' V <- matrix(c(0.002, 0.001, 0.001,
#'               0.001, 0.003, 0.001,
#'               0.001, 0.001, 0.002), nrow = 3, ncol = 3)
#' vech_names <- c("T1 T1", "T2 T1", "T2 T2")
#' dimnames(V) <- list(vech_names, vech_names)
#'
#' # Fit a simple bivariate correlation model
#' model <- "
#' T1 ~~ T1
#' T2 ~~ T2
#' T1 ~~ T2
#' "
#' fit <- umxGSEM(model = model, S = S, V = V, estimation = "DWLS")
#' umxSummary(fit)
#' }
umxGSEM <- function(model, covstruc = NULL, S = NULL, V = NULL, estimation = c("DWLS", "WLS", "ULS"), name = "gsem", numObs = 2, smooth = TRUE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), ...) {
	estimation = match.arg(estimation)
	tryHard = match.arg(tryHard)
	
	# Extract S and V from covstruc if provided
	if (!is.null(covstruc)) {
		if (is.list(covstruc)) {
			if (!is.null(covstruc$S)) {
				S <- covstruc$S
			} else if (length(covstruc) >= 2) {
				S <- covstruc[[2]]
			}
			if (!is.null(covstruc$V)) {
				V <- covstruc$V
			} else if (length(covstruc) >= 1) {
				V <- covstruc[[1]]
			}
		}
	}
	
	if (is.null(S) || is.null(V)) {
		stop("You must provide BOTH the genetic covariance matrix S and the sampling covariance matrix V (either directly or via covstruc).")
	}
	
	S <- as.matrix(S)
	V <- as.matrix(V)
	
	if (is.null(colnames(S))) {
		stop("S matrix must have column/row names matching the trait names.")
	}
	rownames(S) <- colnames(S)
	
	# Ensure V has the correct dimensions for S
	k <- ncol(S)
	z <- (k * (k + 1)) / 2
	if (ncol(V) != z || nrow(V) != z) {
		stop(paste0("Dimensions of V (", nrow(V), "x", ncol(V), ") must match the number of non-redundant elements of S (", z, ")."))
	}
	
	# Internal helper to construct lower-triangular pair names (column-major vech)
	get_vech_names <- function(S_names) {
		k <- length(S_names)
		pairs <- c()
		for (j in 1:k) {
			for (i in j:k) {
				pairs <- c(pairs, paste(S_names[i], S_names[j], sep = " "))
			}
		}
		return(pairs)
	}
	
	# Label V rows and columns if not already labeled
	vech_names <- get_vech_names(colnames(S))
	if (is.null(colnames(V)) || is.null(rownames(V))) {
		colnames(V) <- vech_names
		rownames(V) <- vech_names
	}
	
	# Smooth S and V to near positive definite if needed/requested
	if (smooth) {
		if (!requireNamespace("Matrix", quietly = TRUE)) {
			stop("The Matrix package is required for nearPD heritability smoothing. Please install it.")
		}
		if (eigen(S)$values[k] <= 0) {
			message("S is not positive definite. Smoothing S using nearPD.")
			S <- as.matrix(Matrix::nearPD(S, corr = FALSE)$mat)
		}
		if (eigen(V)$values[z] <= 0) {
			message("V is not positive definite. Smoothing V using nearPD.")
			V <- as.matrix(Matrix::nearPD(V, corr = FALSE)$mat)
		}
	}
	
	# Create a dummy model to discover manifest variables used in the model
	dummy_model <- umxRAM(model, data = mxData(S, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, ...)
	keep_vars <- dummy_model$manifestVars
	
	# Subset S and V to keep only the manifest variables in the model
	keep_vars <- colnames(S)[colnames(S) %in% keep_vars]
	if (length(keep_vars) == 0) {
		stop("No variables in S match manifest variables in the model.")
	}
	
	S_subset <- S[keep_vars, keep_vars, drop = FALSE]
	keep_pairs <- get_vech_names(keep_vars)
	V_subset <- V[keep_pairs, keep_pairs, drop = FALSE]
	
	# Create the final model with subsetted heritabilities/covariances
	final_model <- umxRAM(model, data = mxData(S_subset, type = "cov", numObs = numObs), type = "cov", autoRun = FALSE, name = name, ...)
	
	# Compute weight matrix based on estimation type
	if (estimation == "WLS") {
		W <- solve(V_subset)
	} else if (estimation == "DWLS") {
		W <- diag(1 / diag(V_subset), nrow = nrow(V_subset), ncol = ncol(V_subset))
	} else if (estimation == "ULS") {
		W <- diag(1, nrow = nrow(V_subset), ncol = ncol(V_subset))
	}
	dimnames(W) <- dimnames(V_subset)

	# Replace standard covariance data with WLS acov data
	wls_data <- mxData(observed = S_subset, type = "acov", acov = V_subset, fullWeight = W, numObs = numObs)
	final_model$data <- wls_data
	
	# Set the fit function to WLS
	final_model <- mxModel(final_model, mxFitFunctionWLS(type = estimation))
	
	# Run model if autoRun is TRUE
	if (autoRun) {
		final_model <- umxRun(final_model, tryHard = tryHard)
	}
	
	class(final_model) <- "MxModelGSEM"
	return(final_model)
}
