#' Shows a compact, publication-style, summary of a RAM model
#'
#' Report the fit of a model in a compact form suitable for a journal. 
#' It reports parameters in a markdown or html table (optionally standardized), and fit indices
#' RMSEA (an absolute fit index, comparing the model to a perfect model) and CFI and TLI (incremental fit indices comparing a model with the worst fit).
#' 
#' `umxSummary` alerts you when model fit is worse than accepted criterion (TLI >= .95 and RMSEA <= .06; (Hu & Bentler, 1999; Yu, 2002).
#' 
#' Note: For some (multi-group) models, you will need to fall back on [summary()]
#' 
#' **Robust Fit Statistics for WLS/DWLS (The GenomicMx Ecosystem)**
#' 
#' Historically, simulation studies showed that CFI and TLI behaved differently under DWLS/WLS than under ML (Shi et al., 2020). Incremental fit indices rely on a comparison to the independence (null) model. Under legacy WLS implementations, the weighting matrix changed how that baseline behaved, causing conventional cutoffs (e.g., CFI > 0.95) to break down.
#' 
#' *Solution:* To resolve this, `umxSummary` (as part of the upcoming **GenomicMx** ecosystem) now automatically intercepts WLS models and computes **Satorra-Bentler (2010) robust fit indices** (Robust CFI, TLI, and RMSEA) natively. 
#' 
#' By calculating the unscaled baseline fit natively and applying Satorra-Bentler trace matrix scaling, `umx` restores the conventional interpretation of absolute and incremental fit metrics for summary-statistics and heritability models. You no longer need to strictly de-emphasize CFI/TLI when using WLS. (Note: While robust indices greatly improve comparability, best practice remains reporting multiple indices and inspecting residuals).
#' 
#' **CIs and Identification**
#' This function uses the standard errors reported by OpenMx to produce the CIs you see in umxSummary.
#' These are used to derive confidence intervals based on the formula 95%CI = estimate +/- 1.96*SE.
#' 
#' Sometimes SEs appear NA. This may reflect a model which is not identified (see <http://davidakenny.net/cm/identify.htm>).
#' This can include empirical under-identification - for instance two factors
#' that are essentially identical in structure. Use [OpenMx::mxCheckIdentification()] to check identification.
#' 
#' Solutions: If there are paths estimated at or close to zero, this suggests that fixing one or two of 
#' these to zero may fix the standard error calculation.
#' 
#' If factor loadings can flip sign and provide identical fit, this creates another form of 
#' under-identification and can break confidence interval estimation.
#' *Solution*: Fixing a factor loading to 1 and estimating factor variances can help here.
#'
#' @aliases umxSummary.MxModel umxSummary.MxRAMModel
#' @param model The [OpenMx::mxModel()] whose fit will be reported
#' @param std If TRUE, model is standardized (Default FALSE, NULL means "don't show").
#' @param digits How many decimal places to report (Default 2)
#' @param report If "html", then show results in browser (default = "markdown")
#' @param SE Whether to compute SEs... defaults to TRUE. In rare cases, you might need to turn off to avoid errors.
#' @param means Whether to include means in the summary (TRUE)
#' @param residuals Whether to include residuals in the summary (TRUE)
#' @param filter whether to show significant paths (SIG) or NS paths (NS) or all paths (ALL)
#' @param RMSEA_CI Whether to compute the CI on RMSEA (Defaults to FALSE)
#' @param refModels Saturated models if needed for fit indices (see example below:
#'  If NULL will be computed on demand. If FALSE will not be computed.
#' @param ... Other parameters to control model summary
#' @param matrixAddresses Whether to show "matrix address" columns (Default = FALSE)
#' @family Summary functions
#' @seealso - [umxRAM()], [xmu_robust_WLS_fit()]
#' @references - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance 
#'  structure analysis: Conventional criteria versus new alternatives. *Structural Equation Modeling*, **6**, 1-55. 
#'
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models
#'  with binary and continuous outcomes. University of California, Los Angeles.
#'  Retrieved from <https://www.statmodel.com/download/Yudissertation.pdf>
#'  
#'  - Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled difference chi-square test statistic. *Psychometrika*, **75**(2), 243-248.
#' 
#' <https://tbates.github.io>
#' 
#' @export
#' @import OpenMx
#' @return - parameterTable returned invisibly, if estimates requested
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#'  umxPath("G", to = manifests),
#'  umxPath(var = manifests),
#'  umxPath(var = "G", fixedAt = 1)
#' )
#' umxSummary(m1, std = TRUE)
#' # output as latex
#' umx_set_table_format("latex")
#' umxSummary(m1, std = TRUE)
#' umx_set_table_format("markdown")
#' # output as raw
#' umxSummary(m1, std = FALSE)
#' 
#' # switch to a raw data model
#' m1 = umxRAM("One Factor", data = demoOneFactor[1:100, ],
#'  umxPath("G", to = manifests),
#'  umxPath(v.m. = manifests),
#'  umxPath(v1m0 = "G")
#' )
#' umxSummary(m1, std = TRUE, filter = "NS")
#' }
#'
umxSummary.MxModel <- function(model, refModels = NULL, std = FALSE, digits = 2, report = c("markdown", "html"), means= TRUE, residuals= TRUE, SE = TRUE, filter = c("ALL", "NS", "SIG"), RMSEA_CI = FALSE, ..., matrixAddresses = FALSE){
	# TODO make table take lists of models...
	commaSep = paste0(umx_set_separator(silent = TRUE), " ")
	report   = match.arg(report)
	filter   = match.arg(filter)
	
	reportedWLS = FALSE
	message("?umxSummary options: std=T|F', digits=, report= 'html', filter= 'NS' & more")
	
	# If the filter is not default, user must want something: Assume it's what would have been the default...
	if( filter != "ALL" & is.null(std) ) {
		std = FALSE
	}else if(!is.null(std)){
		 if(SE == FALSE){
			 # message("SE must be TRUE to show std, overriding to set SE = TRUE")
			 SE = TRUE
		 }
	}
	umx_has_been_run(model, stop = TRUE)

	if(is.null(refModels)) {
		# SaturatedModels not passed in from outside, so get them from the model
		modelSummary = summary(model)
		if(is.na(modelSummary$SaturatedLikelihood)){
			# no SaturatedLikelihood, compute refModels
			refModels = tryCatch({
				refModels = mxRefModels(model, run = TRUE)
			}, warning = function(x) {
			    print("Warning calling mxRefModels: mxRefModels can't handle all designs https://github.com/OpenMx/OpenMx/issues/184")
			}, error = function(x) {
			    print("Error calling mxRefModels: mxRefModels can't handle all designs https://github.com/OpenMx/OpenMx/issues/184")
			}, finally={
			    # print("cleanup-code")
			})

			if(!inherits(refModels, "list")){
				modelSummary = summary(model)
			} else {
				modelSummary = summary(model, refModels = refModels)
			}
		}
		if(is.null(model$data)){
			# TODO model with no data - no saturated solution?
			# message("Top model doesn't contain data. You may get extra information from summary() rather than umxSummary()")
		}
	} else if (refModels == FALSE){
		modelSummary = summary(model) # Don't use or generate refModels		
	}else{
		modelSummary = summary(model, refModels = refModels) # Using refModels supplied by user
	}

	isWLS = xmu_is_wls(model)
	if (isWLS) {
	    # Check for GenomicMx engine via our new helper
	    if (xmu_has_WLS_jacobian(model)) {
	        message("umxSummary: Modern WLS model with Jacobian detected. Applying SB-2010 robust metrics...")
	        robustFit = NULL
	        tryCatch({
	            robustFit = xmu_robust_WLS_fit(model)
	        }, error = function(e) {
	            message(paste("umxSummary Note: Frontend Satorra-Bentler calculation skipped:", e$message))
	        })
        
	        if (!is.null(robustFit)) {
	            # Patch incremental indices
	            modelSummary$CFI   = robustFit$CFI
	            modelSummary$TLI   = robustFit$TLI
	            modelSummary$RMSEA = robustFit$RMSEA
            
	            # Patch the base Chi-square statistics
	            modelSummary$Chi    = robustFit$Chi
	            modelSummary$ChiDoF = robustFit$ChiDoF
	            modelSummary$p      = robustFit$p
	        }
        
	    } else {
	        # Legacy catch: Warn the user, leave modelSummary unadjusted
	        warning("Legacy OpenMx WLS engine detected (missing Jacobian). Fit statistics are unadjusted and unreliable. Install GenomicMx for accurate Satorra-Bentler WLS fit reporting.", call. = FALSE)
	    }
	}
	
	# DisplayColumns show
	if(!is.null(std)){
		# n.b: mxStandardizeRAMpaths returns the raw paths as well, so two birds, one stone.
		parameterTable = mxStandardizeRAMpaths(model, SE = SE) # Compute standard errors
		
		# Helper to recursively flatten lists of dataframes returned by mxStandardizeRAMpaths
		flatten_pTable <- function(x) {
			if (is.data.frame(x)) {
				return(x)
			} else if (is.list(x)) {
				if (length(x) == 0) return(NULL)
				dfs = lapply(x, flatten_pTable)
				dfs = dfs[vapply(dfs, is.data.frame, logical(1))]
				if (length(dfs) > 0) {
					return(do.call(rbind, dfs))
				}
			}
			return(NULL)
		}
		
		parameterTable = flatten_pTable(parameterTable)

		if (!is.null(parameterTable) && nrow(parameterTable) > 0) {
			#          name    label  matrix   row         col    Raw.Value  Raw.SE   Std.Value    Std.SE
			# 1  Dep.A[6,1]    age    A        mean_sdrr   age   -0.37       0.0284   -0.372350    .028
			# Raw.SE is new
			names(parameterTable) = c("label", "name", "matrix", "row", "col", "Estimate", "SE", "Std.Estimate", "Std.SE")

			if(matrixAddresses){
				naming = c("name", "matrix", "row", "col")
			} else {
				naming = c("name")
			}
			# TODO: umxSummary add p value, perhaps CI?
			# TODO: umxSummary block table into latents/resid/means etc.
			
			if(std == TRUE){
				# TODO: should CI be here?
				namesToShow = c(naming, "Std.Estimate", "Std.SE", "CI")
			}else{ # must be raw
				namesToShow = c(naming, "Estimate", "SE")					
			}

			if("CI" %in% namesToShow){
				parameterTable$sig = TRUE
				parameterTable$CI  = ""
				for(i in 1:dim(parameterTable)[1]) {
					# TODO we only show SE-based CI for std estimates so far
					est   = parameterTable[i, "Std.Estimate"]
					CI95  = parameterTable[i, "Std.SE"] * 1.96
					bounds = c(est - CI95, est + CI95)

					if(any(is.na(bounds))) {
						# protect cases with SE == NA from evaluation for significance
					} else {
						if (any(bounds <= 0) & any(bounds >= 0)){
							parameterTable[i, "sig"] = FALSE
						}
						if(est < 0){
							parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), commaSep, round(est + CI95, digits), "]")
						} else {
							parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(est - CI95, digits), commaSep, round(est + CI95, digits), "]")
						}
					}
				}
			}
			if(filter == "NS") {
				toShow = parameterTable[parameterTable$sig == FALSE, namesToShow]
			} else if(filter == "SIG") {
				toShow = parameterTable[parameterTable$sig == TRUE, namesToShow]
			} else {
				toShow = parameterTable[, namesToShow]
			}
			toShow = xmu_summary_RAM_group_parameters(model, toShow,  means= means, residuals = residuals)
			toShow = unique.data.frame(toShow[,c(namesToShow, "type")])
			umx_print(toShow, digits = digits, report = report, caption = paste0("Parameter loadings for model ", omxQuotes(model$name)), na.print = "", zero.print = "0", justify = "none")
		}
	}
	if (!reportedWLS) {
		hasFitIndices = !is.null(modelSummary$TLI) && !is.null(modelSummary$CFI) && !is.null(modelSummary$RMSEA) && !is.null(modelSummary$Chi)
		if(hasFitIndices) {
			with(modelSummary, {
				if(!is.finite(TLI)){
					TLI_OK = "OK"
				} else {
					if(TLI > .95) {
						TLI_OK = "OK"
					} else {
						TLI_OK = "bad"
					}
				}
				if(!is.finite(RMSEA)) {
					RMSEA_OK = "OK"
				} else {
					if(RMSEA < .06){
					RMSEA_OK = "OK"
					} else {
						RMSEA_OK = "bad"
					}
				}
				if(report == "table"){
					x = data.frame(cbind(model$name, round(Chi,2), formatC(p, format="g"), round(CFI,3), round(TLI,3), round(RMSEA, 3)))
					names(x) = c("model","\u03C7","p","CFI", "TLI","RMSEA") # \u03A7 is unicode for chi
					print(x)
				} else {
					if(RMSEA_CI){
						RMSEA_CI = RMSEA(modelSummary)$txt
					} else {
						RMSEA_CI = paste0("RMSEA = ", round(RMSEA, 3))
					}
					fitMsg = paste0("\nModel Fit: \u03C7\u00B2(", ChiDoF, ") = ", round(Chi, 2), # was A7
						# "Chi2(", ChiDoF, ") = ", round(Chi, 2), # was A7
						", p "      , umx_APA_pval(p, .001, 3, addComparison = TRUE),
						"; CFI = "  , round(CFI, 3),
						"; TLI = "  , round(TLI, 3),
						"; ", RMSEA_CI
					)
					message(fitMsg)
					if (xmu_is_wls(model)) {
						if (umx_is_GSEM(model)) {
							cat("\n*Statistical Note*: For GSEM models, evaluate absolute fit using SRMR (< 0.10) and CFI. Evaluate model improvements using change in CFI and change in SRMR (see ?umxCompare for details).\n")
						} else {
							cat("\n*Statistical Note*: For WLS models, due to weight-matrix and N-inflation, conventional cutoffs for CFI, TLI, and RMSEA are not valid. Evaluate absolute fit using SRMR (< 0.10), and nested comparisons using the Strict Satorra-Bentler \u0394 \u03C7\u00B2. (see ?umxCompare for details).\n")
						}
					} else {
						if(TLI_OK   != "OK"){ message("TLI is worse than desired (>.95)") }
						if(RMSEA_OK != "OK"){ message("RMSEA is worse than desired (<.06)")}
					}
				}
			})
		} else {
			# Fallback if fit indices are not available
			minus2LL = modelSummary$Minus2LogLikelihood
			df = modelSummary$degreesOfFreedom
			estimatedParameters = modelSummary$estimatedParameters
			
			# If estimatedParameters or minus2LL is not NULL/NA, calculate AIC
			if (!is.null(minus2LL) && !is.na(minus2LL) && !is.null(estimatedParameters)) {
				aic = minus2LL + 2 * estimatedParameters
				fitMsg = paste0("\nModel Fit: -2LL = ", round(minus2LL, 2), 
				                ", df = ", df, 
				                ", AIC = ", round(aic, 2))
			} else {
				fitMsg = "\nModel Fit: Fit statistics not available (model may not have run successfully)."
			}
			message(fitMsg)
		}
	}
	# TODO: umxSummary.MxRAMModel integrate interval printing into summary table
	if(!is.null(model$output$confidenceIntervals)){
		print(model$output$confidenceIntervals)
	}
	
	xmu_print_algebras(model)
	if(!is.null(std) && !is.null(parameterTable) && nrow(parameterTable) > 0){ # return these as  invisible for the user to filter, sort etc.
		if(filter == "NS"){
			invisible(parameterTable[parameterTable$sig == FALSE, namesToShow])
		}else if(filter == "SIG"){
			invisible(parameterTable[parameterTable$sig == TRUE, namesToShow])
		}else{
			invisible(parameterTable[,namesToShow])
		}
	} else {
		invisible(NULL)
	}
}

#' @export
umxSummary.MxRAMModel <- umxSummary.MxModel

#' @export
umxSummary.MxModelGSEM = umxSummary.MxModel
