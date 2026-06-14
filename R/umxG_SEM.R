#' Fit genomic Structural Equation Models
#'
#' @description
#' `umxG_SEM` is a wrapper around [umxRAM()] and OpenMx WLS to fit genomic structural
#' equation models (genomic SEM) using genetic covariance matrices (S) and sampling covariance
#' matrices (V) estimated from LD Score Regression (LDSC).
#'
#' @param model An OpenMx model, a list of `umxPath`s, or a lavaan-style model string.
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
#' fit <- umxG_SEM(model = model, S = S, V = V, estimation = "DWLS")
#' umxSummary(fit)
#' }
umxG_SEM <- function(model, covstruc = NULL, S = NULL, V = NULL, estimation = c("DWLS", "WLS", "ULS"), name = "gsem", numObs = 2, smooth = TRUE, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search"), ...) {
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
		stop("You must provide both the genetic covariance matrix S and the sampling covariance matrix V (either directly or via covstruc).")
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
	
	return(final_model)
}
