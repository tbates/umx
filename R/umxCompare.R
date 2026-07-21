#' Print a comparison table of two or more [OpenMx::mxModel()]s, nicely formatted.
#'
#' @description
#' umxCompare compares two or more [OpenMx::mxModel()]s. It has several nice features:
#' 
#' 1. Direct control of precision via digits=. p-values use APA style.
#' 2. Report = directs publication to your chosen format: markdown, html, latex.
#' 3. Columns arranged to make for easy visual comparison.
#' 4. report = 'inline', includes a summary sentence suitable for your report.
#' 5. report = "html" opens a web table in your browser to paste into a word processor.
#' 
#'
#' **Interpreting fit under WLS/DWLS and Genomic SEM**
#'
#' Hu & Bentler (1999) style cutoffs (e.g. CFI > .95, RMSEA < .06) were developed for
#' maximum likelihood with continuous data. They do **not** transfer to WLS/DWLS
#' (Shi et al., 2020), and still less to Genomic SEM (DWLS on LDSC genetic
#' covariances with an estimated sampling covariance, often with bookkeeping `numObs = 1`).
#'
#' * **Incremental indices (CFI, TLI, NFI, …)** compare the model to an independence
#'   baseline under the same weight matrix. That baseline behaves differently than under ML.
#'   Report CFI/TLI only **descriptively**; do **not** apply conventional cutoffs.
#' * **Absolute fit: SRMR** (and residual matrices) is preferred. SRMR lives on a
#'   standardized residual metric and is less hostage to weight-matrix / N scaling.
#'   Rough guide: SRMR < 0.10 suggests acceptable residual structure (not a hard law).
#' * **RMSEA** folds in chi-square scaling and N; it is often misleading for WLS and
#'   especially for GSEM. Do not use the ML cutoff of < .06.
#' * **Nested model building:** prefer scaled chi-square difference tests and ΔSRMR
#'   over ΔCFI alone (see Details for what `umxCompare` computes).
#'
#' @details
#' **What `umxCompare` does for WLS**
#'
#' * Refuses mixed WLS vs ML comparisons (same engine required).
#' * **Continuous WLS** with cached `implied_jacobian`:
#'   - Table **Chi** is the same Satorra–Bentler (2010) scaled omnibus as [umxSummary()]
#'     (\eqn{F/c} from `output$fit`), **not** OpenMx Browne residual `output$chi`
#'     (those two differ under DWLS). Saturated residual df reports Chi = 0, CFI = 1, RMSEA = 0.
#'   - Nested **diffFit** is SB-2010 scaled \eqn{\Delta F = F_{nested} - F_{base}}
#'     (requires both models at comparable minima of the same WLS objective: same moments,
#'     `useWeight`, and \eqn{N}). If \eqn{F_{nested} < F_{base}} (often multigroup
#'     optimization), diffFit is **NA** with a warning — not a negative chi-square with p = 1.
#' * **Genomic SEM** (`MxModelGSEM`): nested difference on the **GSEM DWLS chi-square
#'   scale** (same discrepancy as the structural fit; not a substitute for ML LRT).
#'   If LDSC matrices were `nearPD`-smoothed, a fiduciary warning notes that difference
#'   tests may look artificially precise.
#' * Table also reports **SRMR / ΔSRMR** (preferred absolute residual summary) and
#'   **CFI / ΔCFI** (descriptive only). AIC may appear but is not a primary GSEM decision
#'   rule under asymptotic / bookkeeping N.
#'
#' **Ordinal WLS** is a separate track: robust CFI/TLI/RMSEA use Savalei (2021) catML
#' corrections in [umxSummary()]; Hu–Bentler cutoffs can apply to **those** robust indices.
#' That exception does **not** apply to continuous WLS or Genomic SEM.
#'
#' Best practice: report estimates with SEs, SRMR (and residuals), nested SB/GSEM
#' difference tests, and avoid single-number “good fit” claims from CFI or RMSEA.
#'
#' @param base The base [OpenMx::mxModel()] for comparison
#' @param comparison The model (or list of models) which will be compared for fit with the base model (can be empty)
#' @param all Whether to make all possible comparisons if there is more than one base model (defaults to T)
#' @param digits rounding for p-values etc.
#' @param report "markdown" (default), "inline" (a sentence suitable for inclusion in a paper), or "html".
#' create a web table and open your default browser.
#' (handy for getting tables into Word, and other text systems!)
#' @param file file to write html too if report = "html" (defaults to "tmp.html")
#' @param compareWeightedAIC Show the Wagenmakers AIC weighted comparison (default = FALSE)
#' @param silent (don't print, just return the table as a dataframe (default = FALSE)
#' @param uncertainty What type of parameter uncertainty to report: "SE" (standard ML standard errors), "RobustSE" (robust standard errors), "CI" (profile likelihood confidence intervals), or "none" (none).
#' @family Model Summary and Comparison
#' @seealso - [umxSummary()], [umxRAM()],[umxCompare()]
#' @references - <https://github.com/tbates/umx>
#' @export

#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
#' umxCompare(m1, m2, report = "html") # Open table in browser
#'
#' # Two comparison models
#' m3 = umxModify(m2, update = "G_to_x3", name = "drop_path_2_x2_and_3")
#' 
#' umxCompare(m1, c(m2, m3))
#' umxCompare(m1, c(m2, m3), compareWeightedAIC = TRUE)
#' umxCompare(c(m1, m2), c(m2, m3), all = TRUE)
#'
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("WLS", data = demoOneFactor, type = "DWLS",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#'
#' m2 = umxModify(m1, update = "G_to_x2", name = "drop_path_2_x2")
#' umxCompare(m1, m2)
#' umxCompare(m1, m2, report = "inline") # Add English-sentence descriptions
#' umxCompare(m1, m2, report = "html") # Open table in browser
#' }
umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, digits = 3, report = c("markdown", "html", "inline"), compareWeightedAIC = FALSE, silent = FALSE, file = "tmp.html", uncertainty = c("none", "SE", "RobustSE", "CI")) {
	report = match.arg(report)
	uncertainty = match.arg(uncertainty)
	if(umx_is_MxModel(all)){
		stop("Provide all comparison models as a c() (You provided a model as input to 'all', and I'm guessing that's a mistake)")
	}
	if(is.null(comparison)){
		comparison = base
	} else if (is.null(base)) {
		stop("You must provide at least a base model for umxCompare")
	}
	if(length(base) == 1) {
		if(typeof(base) == "list"){
			base = base[[1]]
		}
		if(!umx_has_been_run(base)){
			warning("Base model not run yet!")		
		}
	}
	if (umx_is_MxModel(comparison)) {
		comparison = list(comparison)
	}
	if (!is.list(comparison)) {
		stop("comparison must be a list or an MxModel")
	}
	for (comp in comparison) {
		if (!umx_has_been_run(comp)) {
			stop("Comparison model has not been run!")
		}
	}

	baseIsWLS  = xmu_is_wls(base)
	baseHasJac = xmu_has_WLS_jacobian(base)

	if (baseIsWLS) {
		# 1. Enforce Homogeneity Constraint across all comparison models
		for (comp in comparison) {
			if (!xmu_is_wls(comp)) {
				stop("Engine Mismatch: Cannot compare a WLS model with an ML model.")
			}
			if (baseHasJac != xmu_has_WLS_jacobian(comp)) {
				stop("Engine Mismatch: Cannot compare a legacy OpenMx WLS model with a GenomicMx WLS model. Both models must use the same engine.")
			}
		}

		# 1. Initialize an empty data frame to hold the master table
		finalTable = data.frame()

		# 2. Iterate explicitly through each model in the comparison list
		for (i in seq_along(comparison)) {
	
			comp = comparison[[i]]
	
			# 3. Generate the table for this specific base vs. comparison pair
			compTable = xmu_compare_WLS(baseModel = base, comparisonModel = comp)
	
			# DIAGNOSTIC: Print the isolated output before it gets smashed into the master table
			# print(paste("Rows returned by xmu_compare_WLS for comparison", i, ":", nrow(compTable)))
			# print(compTable)
	
			# 4. Bind it to the master table
			if (nrow(finalTable) == 0) {
				finalTable = compTable
			} else {
				finalTable = rbind(finalTable, compTable)
			}
		}

		# TODO match the output as much as possible with what we do for ML (see further down)
		# names(tablePub) = c("Model", "EP", "\u0394 Fit" , "\u0394 df" , "p", "AIC", "\u0394 AIC", "Compare with Model", "Fit units")
		if (!silent) {
			umx_print(finalTable, digits = digits, zero.print = "0", caption = "Table of Model Comparisons", report = report)
			units_str = summary(base)$fitUnits
			if (is.null(units_str) || length(units_str) == 0) {
				units_str = "r'wr"
			}
			actualN = base$data$numObs
			isGenomic = umx_is_GSEM(base) | (!is.null(actualN) && actualN > 50000)
			for (comp in comparison) {
				if (umx_is_GSEM(comp)) {
					isGenomic = TRUE
				}
			}

			cat(paste0("\n*Note*: EP = free parameters; delta_df = change in df.\n"))
			if (isGenomic) {
				cat("  - Chi: GSEM/DWLS chi-square scale for the model (OpenMx residual/structural chi).\n")
			} else {
				cat("  - Chi: Satorra-Bentler (2010) scaled WLS discrepancy (same as umxSummary; not Browne output$chi).\n")
			}
			cat("  - SRMR / delta_SRMR: preferred absolute residual summary and its change.\n")
			cat("  - CFI / delta_CFI: descriptive only (no conventional cutoffs under WLS/GSEM).\n")
			cat("  - AIC: Chi + 2*EP on the same Chi scale (convenience only for GSEM).\n")
			if (isGenomic) {
				cat("  - diffFit: nested difference on the GSEM DWLS chi-square scale.\n")
				cat("\n*Statistical Note*: Genomic SEM — absolute fit: prefer SRMR (roughly < 0.10) and residual inspection. Nested models: use diffFit (DWLS chi-square difference). De-emphasize CFI/TLI/RMSEA; do not apply Hu-Bentler cutoffs. See ?umxCompare.\n")
			} else {
				cat("  - diffFit: Satorra-Bentler (2010) scaled nested Delta chi-square when Jacobians are available and F is nested-monotone.\n")
				cat("\n*Statistical Note*: Continuous WLS/DWLS — conventional CFI/TLI/RMSEA cutoffs do not apply. Prefer SRMR for absolute fit; nested comparisons use Strict Satorra-Bentler (2010) Delta chi-square (diffFit). See ?umxCompare.\n")
			}
		}
		return(invisible(finalTable))
	} else {
		# Base is ML. Ensure no comparison models are WLS to prevent reverse-mismatch.
		for (comp in comparison) {
			if (xmu_is_wls(comp)) {
				stop("Engine Mismatch: Cannot compare an ML model with a WLS model.")
			}
		}
	}

	robustScalingFactors = list()
	tableOut = mxCompare(base = base, comparison = comparison, all = all)
	tableOut = as.data.frame(tableOut)

	tablePub = tableOut[, c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "base", "fitUnits")]

	# Subtract row-1 AIC from all values and place the resulting deltaAIC column after AIC 
	tablePub$deltaAIC = tablePub[, "AIC"] - tablePub[1, "AIC"]
	tablePub = tablePub[,c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "deltaAIC", "base", "fitUnits")]

	# Compute and patch robust Satorra-Bentler difference statistics if requested
	if (uncertainty == "RobustSE") {
		for (comp in comparison) {
			resSb = xmu_compare_robust_ML(base, comp)
			if (!is.null(resSb)) {
				compName = comp$name
				rowIdx = which(tablePub$comparison == compName)
				if (length(rowIdx) == 1) {
					tablePub[rowIdx, "diffFit"] = resSb$diffFit
					tablePub[rowIdx, "p"] = resSb$p
					robustScalingFactors[[compName]] = resSb$scalingFactor
				}
			}
		}
	}

	# c("1: Comparison", "2: Base", "3: EP", "4: AIC", "5: &Delta; -2LL", "6: &Delta; df", "7: p")
	# U+2206 = math delta
	# Fix problem where base model has compare set to its own name, and name set to NA
	nRows = dim(tablePub)[1]
	for (i in 1:nRows) {
		if(is.na(tablePub[i, "comparison"])){
			tablePub[i, "comparison"] = tablePub[i, "base"]
			tablePub[i, "base"] = NA
		}
	}
	tablePub[, "p"] = umx_APA_pval(tablePub[, "p"], min = (1/ 10^3), digits = digits, addComparison = NA)
	if(report == "inline"){
		n_rows = dim(tablePub)[1]
		for (i in 1:n_rows) {
			thisPValue = tableOut[i, "p"]
			if(!is.na(thisPValue) && !is.nan(thisPValue)){
				if(tableOut[i, "p"] < .05){
					this = ". This caused a significant loss of fit "
				} else {
					this = ". This did not lower fit significantly "
				}
				inlineMsg = paste0("The hypothesis that ", omxQuotes(tablePub[i, "comparison"]), 
				" was tested by dropping ", tablePub[i, "comparison"],
				" from ", omxQuotes(tablePub[i, "base"]), 
				this, "(\u03C7\u00B2(", tablePub[i, "diffdf"], ") = ", round(tablePub[i, "diffFit"], 2), # \u03A7 = Chi \u00B2 = superscript 2
				", p = ", tablePub[i, "p"], ": AIC = ", round(tablePub[i, "AIC"], digits), " change in AIC = ", round(tablePub[i, "deltaAIC"], digits), ")."
				)
				if(!silent){
					cat(inlineMsg)
				}
			}
		}
	}
	
	# Rename for printing
	names(tablePub) = c("Model", "EP", "\u0394 Fit" , "\u0394 df" , "p", "AIC", "\u0394 AIC", "Compare with Model", "Fit units")

	if(report == "inline"){ report= "markdown"}
	
	# Check if any model in the comparison is a WLS model
	anyWLS = FALSE
	checkModels = list()
	if (is.list(base) && !umx_is_MxModel(base)) {
		checkModels = c(checkModels, base)
	} else if (!is.null(base)) {
		checkModels = c(checkModels, list(base))
	}
	if (is.list(comparison) && !umx_is_MxModel(comparison)) {
		checkModels = c(checkModels, comparison)
	} else if (!is.null(comparison)) {
		checkModels = c(checkModels, list(comparison))
	}
	
	for (m in checkModels) {
		if (umx_is_MxModel(m) && xmu_is_wls(m)) {
			anyWLS = TRUE
			break
		}
	}

	if(!silent){
		umx_print(tablePub, digits = digits, zero.print = "0", caption = "Table of Model Comparisons", report = report)
		if (anyWLS) {
			units_val = unique(tablePub$`Fit units`[!is.na(tablePub$`Fit units`)])
			if (length(units_val) == 0) {
				units_str = "WLS discrepancy function"
			} else {
				units_str = paste(units_val, collapse = ", ")
			}
			cat(paste0("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394 Fit = change in fit (units: ", units_str, "); \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n"))
			cat("\n*Statistical Note*: WLS/GSEM — conventional CFI/TLI/RMSEA cutoffs do not apply. Prefer SRMR for absolute fit; nested comparisons use scaled Delta chi-square (diffFit). De-emphasize incremental indices. See ?umxCompare.\n")

		} else {
			if (length(robustScalingFactors) > 0) {
				factorsStr = paste(sapply(names(robustScalingFactors), function(n) sprintf("%s (c_d = %.3f)", n, robustScalingFactors[[n]])), collapse = ", ")
				cat(sprintf("\n*Statistical Note*: Robust ML difference tests use Satorra-Bentler (2001) scaled difference formula: %s.\n", factorsStr))
			} else {
				cat("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394-2LL = change in -2 \u00D7 Log-Likelihood of the model; \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n")
			}
		}
	}

	if(compareWeightedAIC){
		modelList = c(base, comparison)
		# get list of AICs
		AIClist = c()
		for (i in modelList) {
			AIClist = c(AIClist, AIC(i))
		}
		whichBest = which.min(AIClist)
		bestModel = modelList[[whichBest]]
		# Probabilities according to AIC MuMIn::Weights (Wagenmakers et al https://pubmed.ncbi.nlm.nih.gov/15117008/ )
		aic.weights = round(Weights(AIClist), 2)
		if(!silent){
			cat("The ", omxQuotes(bestModel$name), " model is the best fitting model according to AIC.")
			cat("AIC weight-based  {Wagenmakers, 2004, 192-196} conditional probabilities of being the best model for ", 
				omxQuotes(namez(modelList)), " respectively are: ", 
				omxQuotes(aic.weights), " Using MuMIn::Weights(AIC()).")	
		}
		
	}
	if (length(robustScalingFactors) > 0) {
		attr(tablePub, "robustScalingFactors") = robustScalingFactors
		attr(tablePub, "c_d") = robustScalingFactors
	}
	invisible(tablePub)
}
