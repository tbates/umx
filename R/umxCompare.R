#' Print a comparison table of one or more [OpenMx::mxModel()]s, formatted nicely.
#'
#' @description
#' umxCompare compares two or more [OpenMx::mxModel()]s. It has several nice features:
#' 
#' 1. It supports direct control of rounding, and reports p-values rounded to APA style.
#' 2. It reports the table in your preferred format (default is markdown, options include latex).
#' 3. Table columns are arranged to make for easy comparison for readers.
#' 4. report = 'inline', will provide an English sentence suitable for a paper.
#' 5. report = "html" opens a web table in your browser to paste into a word processor.
#' 
#' 
#' **Interpreting fit statistics under WLS/DWLS**
#' 
#' Simulation studies show that CFI and TLI behave differently under DWLS/WLS than under ML, and conventional cutoffs (e.g., CFI > 0.95) do not transfer well (Shi et al., 2020). Incremental fit indices (CFI, TLI, NFI, etc.) rely on a comparison to the independence (null) model.
#' Under WLS the weighting changes how that baseline behaves, so the usual interpretation breaks down.
#' We advise de-emphasizing incremental fit indices (CFI, TLI), and to not use conventional cutoffs.
#' Best practice is to report multiple indices and inspect residuals rather than relying on any single number.
#' Absolute indices are preferred. SRMR tends to perform reasonably. RMSEA can be biased depending on model and sample sizes, and degree of misspecification. The RMSEA cutoff of <.06 developed for ML does not work the same way.
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
#' @family Model Summary and Comparison
#' @seealso - [umxSummary()], [umxRAM()],[umxCompare()]
#' @references - <https://github.com/tbates/umx>
#' @export
#' @md
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
umxCompare <- function(base = NULL, comparison = NULL, all = TRUE, digits = 3, report = c("markdown", "html", "inline"), compareWeightedAIC = FALSE, silent = FALSE, file = "tmp.html") {
	report = match.arg(report)
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
			compIsWLS = xmu_is_wls(comp)
			compHasJac = xmu_has_WLS_jacobian(comp)

			if (!compIsWLS) {
				stop("Engine Mismatch: Cannot compare a WLS model with an ML model.")
			}
			if (baseHasJac != compHasJac) {
				stop("Engine Mismatch: Cannot compare a legacy OpenMx WLS model (no Jacobian) with a GenomicMx WLS model. Both models must use the same engine.")
			}
		}

		# 2. Route based on Engine
		if (baseHasJac) {
			# Modern GenomicMx Route
			message("umxCompare: GenomicMx WLS models detected. Routing to Satorra-Bentler (2010) robust comparison engine...")
			finalTableList = lapply(comparison, function(comp) {
				xmu_compare_WLS(baseModel = base, comparisonModel = comp)
			})
			finalTable = do.call(rbind, finalTableList)
			
			if (!silent) {
				umx_print(finalTable, digits = digits, zero.print = "0", caption = "Table of Model Comparisons", report = report)
				units_str = summary(base)$fitUnits
				if (is.null(units_str) || length(units_str) == 0) {
					units_str = "r'wr"
				}
				cat(paste0("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394 Fit = change in fit (units: ", units_str, "); \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n"))
				cat("Note: For WLS/DWLS models, conventional fit index cutoffs do not apply. See ?umxCompare for details.\n")
			}
			return(finalTable)
			
		} else {
			# Legacy OpenMx Route
			warning("Legacy OpenMx WLS engine detected (missing Jacobian). Chi-square difference tests are naive unadjusted subtractions and are statistically unreliable. Install GenomicMx for accurate Satorra-Bentler difference testing.", call. = FALSE)
			# Do NOT return. Let execution fall through to standard mxCompare table logic below.
		}
		
	} else {
		# Base is ML. Ensure no comparison models are WLS to prevent reverse-mismatch.
		for (comp in comparison) {
			if (xmu_is_wls(comp)) {
				stop("Engine Mismatch: Cannot compare an ML model with a WLS model.")
			}
		}
	}

	tableOut = mxCompare(base = base, comparison = comparison, all = all)
	tableOut = as.data.frame(tableOut)

	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf |p     |fit       |fitUnits |diffFit |chisq     |SBchisq |
	# |:--------|:--------------|---:|:---------|:----|---------:|:---------|:-------|:-----|:---------|:--------|:-------|:---------|:-------|
	# |DWLS     |               |  6 |          |0    | 12.00000 |          |        |      |0         |r'Wr     |        |0         |        |
	# |DWLS     |drop_l2mpg     |  5 |          |1    | 14.49542 |          |1       |      |4.4954186 |r'Wr     |        |4.4954186 |        |
	
	# | base    | comparison    | ep | minus2LL | df  | AIC      | diffLL   | diffdf | p    |
	#    1            2           3     4          5     6          7          8        9     
	# | twinSat | <NA>          | 13 | 333.0781 | 149 | 35.07809 | NA       | NA     | NA   |
	# | twinSat | betaSetToZero | 10 | 351.6486 | 152 | 47.64858 | 18.57049 | 3      | 0.01 |

	tablePub = tableOut[, c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "base", "fitUnits")]

	# Subtract row-1 AIC from all values and place the resulting deltaAIC column after AIC 
	tablePub$deltaAIC = tablePub[, "AIC"] - tablePub[1, "AIC"]
	tablePub = tablePub[,c("comparison", "ep", "diffFit", "diffdf", "p", "AIC", "deltaAIC", "base", "fitUnits")]

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
			units_val <- unique(tablePub$`Fit units`[!is.na(tablePub$`Fit units`)])
			if (length(units_val) == 0) {
				units_str <- "WLS discrepancy function"
			} else {
				units_str <- paste(units_val, collapse = ", ")
			}
			cat(paste0("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394 Fit = change in fit (units: ", units_str, "); \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n"))
			cat("Note: For WLS/DWLS models, conventional fit index cutoffs do not apply. See ?umxCompare for details.\n")
		} else {
			cat("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394-2LL = change in -2 \u00D7 Log-Likelihood of the model; \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n")
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
	invisible(tablePub)
}
