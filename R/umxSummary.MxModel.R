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
#' **Robust inference (`uncertainty = "RobustSE"`)**
#'
#' This option means **engine-appropriate robust inference**, not one sandwich formula for every fit type:
#' \itemize{
#'   \item **ML / FIML (raw continuous):** casewise score sandwich SEs via [OpenMx::imxRobustSE()], plus robust CFI/TLI/RMSEA via [xmu_robust_ML_fit()] (Yuan–Bentler / Brosseau-Liard & Savalei scaling).
#'   \item **WLS / DWLS / ULS:** do **not** use the ML casewise sandwich. Parameter SEs are the usual **WLS/GMM moment sandwich** already returned by OpenMx (\eqn{W}, \eqn{\Delta}, \eqn{\Gamma}). Robust CFI/TLI/RMSEA are applied when \code{output$implied_jacobian} is present via [xmu_robust_WLS_fit()] (Satorra–Bentler 2010 for continuous WLS; Savalei 2021 catML for ordinal WLS). A statistical note states that SEs are WLS asymptotic (moment sandwich).
#' }
#'
#' **Robust Fit Statistics for ML**
#' Standard Maximum Likelihood (ML) is powerful and efficient, with χ2, CFI, TLI, and RMSEA measures of fit. Non-normality and heteroscedasticity distort the sampling distribution of the scores and the ML test statistic itself, necessitating changes to SEs and fit statistics. For ML, `uncertainty = "RobustSE"` implements the sandwich estimator for SEs and MLR-style fit scaling (Yuan–Bentler / Savalei 2018-style robust indices).
#'
#' **Robust Fit Statistics for WLS/DWLS (The GenomicMx Ecosystem)**
#' 
#' Historically, simulation studies showed that CFI and TLI behaved differently under DWLS/WLS than under ML (Shi et al., 2020). Incremental fit indices rely on a comparison to the independence (null) model. Under legacy WLS implementations, the weighting matrix changed how that baseline behaved, causing conventional cutoffs (e.g., CFI > 0.95) to break down.
#' 
#' *Solution:* `umxSummary` intercepts WLS models (including summary-statistic / Genomic SEM inputs) and routes them through [xmu_robust_WLS_fit()] when a Jacobian is available. That function computes an independence baseline natively in R and applies trace-matrix scaling corrections so that robust indices are interpretable alongside conventional ML practice. Parameter SEs remain the OpenMx WLS moment sandwich (see `uncertainty = "RobustSE"` note above).
#' 
#' **Two statistic families (by design).** WLS output deliberately separates omnibus testing from incremental/absolute fit:
#' \itemize{
#'   \item **Displayed \eqn{\chi^2} and \eqn{p}-value** — always Satorra-Bentler (2010) scaled WLS test statistics, for both continuous and ordinal models.
#'   \item **Displayed CFI, TLI, RMSEA** — robust indices corrected for WLS sampling behavior; branch depends on variable type (below).
#' }
#' 
#' **Continuous WLS** (all manifests numeric, not `ordered`/`factor`): robust CFI, TLI, and RMSEA use the Satorra-Bentler (2010) scaled \eqn{\chi^2} pipeline. The printed `*Statistical Note*` reminds users that raw WLS cutoffs do not apply; evaluate nested comparisons with Strict Satorra-Bentler \eqn{\Delta\chi^2} via [umxCompare()].
#' 
#' **Ordinal / categorical WLS** (at least one manifest `ordered` or `factor` on raw data): robust CFI, TLI, and RMSEA use **Savalei (2021)** catML mean-and-variance corrections (\eqn{XX_3} discrepancy at fixed WLS estimates plus \eqn{\hat{c}_3} sandwich scaling from OpenMx \code{asymCov}, \code{useWeight}, and \code{implied_jacobian}). The printed `*Statistical Note*` reports `correction = Savalei2021` with \eqn{\hat{c}_{model}} and \eqn{\hat{c}_{null}}; **Hu & Bentler (1999) conventional cutoffs apply to these robust indices** (CFI \eqn{\geq} 0.95, RMSEA \eqn{\leq} 0.06, etc.). Display \eqn{\chi^2} and \eqn{p} remain SB-scaled WLS omnibus tests—not catML-scaled.
#' 
#' Requires OpenMx WLS models with `implied_jacobian` populated (standard after `mxRun` on WLS). If robust computation fails, `umxSummary` falls back to OpenMx `summary()` indices and emits a note.
#' 
#' (Best practice: report multiple indices, inspect residuals, and use [umxCompare()] for nested model comparison.)
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
#' @param uncertainty What type of uncertainty/inference packaging to report: `"SE"` (default SEs from the fit), `"RobustSE"` (engine-appropriate robust inference: ML casewise sandwich SEs + robust AFIs; for WLS, moment-sandwich SEs + robust WLS AFIs when available), `"CI"` (profile likelihood CIs), or `"none"`.
#' @param means Whether to include means in the summary (TRUE)
#' @param residuals Whether to include residuals in the summary (TRUE)
#' @param filter whether to show significant paths (SIG) or NS paths (NS) or all paths (ALL)
#' @param RMSEA_CI Whether to compute the CI on RMSEA (Defaults to FALSE)
#' @param refModels Saturated models if needed for fit indices (see example below:
#'  If NULL will be computed on demand. If FALSE will not be computed.
#' @param matrixAddresses Whether to show "matrix address" columns (Default = FALSE)
#' @param SE Deprecated. Please use `uncertainty` instead.
#' @param ... Other parameters to control model summary
#' @family Summary functions
#' @seealso - [umxRAM()], [xmu_robust_WLS_fit()]
#' @references - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance 
#'  structure analysis: Conventional criteria versus new alternatives. *Structural Equation Modeling*, **6**, 1-55. 
#'  - Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the scaled difference chi-square test statistic. *Psychometrika*, **75**(2), 243-248.
#'  - Satorra, A. and Bentler P. M. (1994). Corrections to test statistics and standard errors in covariance structure analysis. Latent variables analysis: Applications for developmental research. A. von Eye and C. C. Clogg, Sage: 399-419.
#'  - Savalei, V. (2018). On the computation of the RMSEA and CFI from the mean-and-variance corrected test statistic with nonnormal data in SEM. *Multivariate Behavioral Research*, **53**(3), 419--429.
#'  - Savalei, V. (2021). Improving fit indices in SEM with categorical data. *Multivariate Behavioral Research*, **56**(3), 390--407.
#'  - Brosseau-Liard, P. E., & Savalei, V. (2012). Adjusting incremental fit indices for nonnormality. *Multivariate Behavioral Research*, **47**(5), 647--677.
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models
#'  with binary and continuous outcomes. University of California, Los Angeles.
#'  Retrieved from <https://www.statmodel.com/download/Yudissertation.pdf>
#'  - Yuan, K.-H. and P. M. Bentler (2000). "5. Three Likelihood-Based Methods for Mean and Covariance Structure Analysis with Nonnormal Missing Data." Sociological Methodology 30(1): 165-200.
#' 
#' <https://tbates.github.io>
#' 
#' @export
#' @import OpenMx
#' @return - parameterTable returned invisibly, if estimates requested
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
umxSummary.MxModel <- function(model, refModels = NULL, std = FALSE, digits = 2, report = c("markdown", "html"), means= TRUE, residuals= TRUE, uncertainty = c("SE", "RobustSE", "CI", "none"), filter = c("ALL", "NS", "SIG"), RMSEA_CI = FALSE, SE = TRUE, matrixAddresses = FALSE, ...){
	# TODO make table take lists of models...
	commaSep = paste0(umx_set_separator(silent = TRUE), " ")
	report   = match.arg(report)
	filter   = match.arg(filter)
	
	# Handle deprecated SE parameter and map it to uncertainty
	if (missing(uncertainty)) {
		if (!missing(SE)) {
			if (is.logical(SE)) {
				uncertainty = ifelse(SE, "SE", "none")
			} else {
				uncertainty = SE
			}
		} else {
			uncertainty = "SE"
		}
	} else {
		uncertainty = match.arg(uncertainty)
	}

	# RobustSE: engine-appropriate robust inference (not one sandwich for all fit types)
	isWLS = xmu_is_wls(model)
	if (uncertainty == "RobustSE") {
		if (isWLS) {
			# Plan A: keep OpenMx WLS/GMM moment-sandwich SEs (already in output$vcov).
			# Do not call imxRobustSE (casewise ML sandwich is wrong for WLS).
			if (is.null(model$output$vcov)) {
				warning("WLS model has no parameter covariance matrix in output; SEs unavailable. Run the model with standard errors enabled.", call. = FALSE)
			}
		} else {
			if (is.null(model$data) || is.null(model$data$observed) || identical(model$data$type, "cov")) {
				stop("Robust standard errors for ML require raw data. The model has covariance or missing data.")
			}
			resRobust = imxRobustSE(model, details = TRUE)
			model@output$vcov = resRobust$cov
			model@output$standardErrors = resRobust$SE
		}
	}

	SE = (uncertainty %in% c("SE", "RobustSE", "CI")) && !is.null(model$output$vcov)

	reportedWLS = FALSE
	message("?umxSummary options: std=T|F', digits=, report= 'html', uncertainty = \"RobustSE\", filter= 'NS' & more")
	
	# If the filter is not default, user must want something: Assume it's what would have been the default...
	if( filter != "ALL" & is.null(std) ) {
		std = FALSE
	}else if(!is.null(std)){
		 if(SE == FALSE && !is.null(model$output$vcov)){
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

	if (isWLS) {
	    # Check for GenomicMx engine via our new helper
	    if (xmu_has_WLS_jacobian(model)) {
	        message("umxSummary: Modern WLS model with Jacobian detected. Applying robust WLS fit metrics...")
	        robustFit = NULL
	        tryCatch({
	            robustFit = xmu_robust_WLS_fit(model)
	        }, error = function(e) {
	            # Avoid raw R errors ("replacement has length zero") in user-facing notes
	            msg = conditionMessage(e)
	            if (grepl("replacement has length zero|Could not locate observed covariance|dimnames do not cover", msg)) {
	            	message("umxSummary Note: robust CFI/TLI/RMSEA (Satorra-Bentler) could not be computed for this WLS model; reporting unadjusted chi-square. Parameter estimates and SEs are unaffected. For genomic SEM, prefer SRMR and nested comparisons (see ?umxCompare).")
	            } else {
	            	message("umxSummary Note: robust CFI/TLI/RMSEA (Satorra-Bentler) skipped: ", msg)
	            }
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

	            attr(modelSummary, "robustScalingFactors") = list(
	            	cModel = attr(robustFit, "c_model"),
	            	cNull = attr(robustFit, "c_null"),
	            	correction = attr(robustFit, "correction")
	            )
	        }
        
	    } else {
	        # Legacy catch: Warn the user (once per session), leave modelSummary unadjusted
	        if (is.null(getOption("umx_warned_legacy_wls")) || !getOption("umx_warned_legacy_wls")) {
	            warning("Legacy OpenMx WLS engine detected (missing Jacobian). Fit statistics are unadjusted and unreliable. Install GenomicMx for accurate Satorra-Bentler WLS fit reporting.", call. = FALSE)
	            options(umx_warned_legacy_wls = TRUE)
	        }
	    }
	} else if (uncertainty == "RobustSE") {
		robustFit = NULL
		tryCatch({
			robustFit = xmu_robust_ML_fit(model, refModels = refModels)
		}, error = function(e) {
			message(paste("umxSummary Note: Frontend robust ML calculation skipped:", e$message))
		})

		if (!is.null(robustFit)) {
			modelSummary$CFI   = robustFit$CFI
			modelSummary$TLI   = robustFit$TLI
			modelSummary$RMSEA = robustFit$RMSEA

			modelSummary$Chi    = robustFit$Chi
			modelSummary$ChiDoF = robustFit$ChiDoF
			modelSummary$p      = robustFit$p
			
			attr(modelSummary, "robustScalingFactors") = list(
				cModel = robustFit$scalingFactor,
				cNull = robustFit$scalingFactorNull
			)
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
				if (uncertainty == "none") {
					namesToShow = c(naming, "Std.Estimate")
				} else if (uncertainty == "CI") {
					namesToShow = c(naming, "Std.Estimate", "CI")
				} else {
					namesToShow = c(naming, "Std.Estimate", "Std.SE", "CI")
				}
			}else{ # must be raw
				if (uncertainty == "none") {
					namesToShow = c(naming, "Estimate")
				} else if (uncertainty == "CI") {
					namesToShow = c(naming, "Estimate", "CI")
				} else {
					namesToShow = c(naming, "Estimate", "SE", "CI")
				}
			}

			parameterTable$sig = TRUE
			if("CI" %in% namesToShow){
				parameterTable$CI  = ""
				cis = model$output$confidenceIntervals
				hasCIs = !is.null(cis) && (nrow(cis) > 0)
				for(i in 1:dim(parameterTable)[1]) {
					pName = parameterTable[i, "name"]
					est   = parameterTable[i, ifelse(std, "Std.Estimate", "Estimate")]
					
					if (uncertainty == "CI" && hasCIs && (pName %in% rownames(cis))) {
						lbound = cis[pName, "lbound"]
						ubound = cis[pName, "ubound"]
						parameterTable[i, "CI"] = paste0(round(est, digits), " [", round(lbound, digits), commaSep, round(ubound, digits), "]")
						if (!is.na(lbound) && !is.na(ubound)) {
							if (lbound <= 0 && ubound >= 0) {
								parameterTable[i, "sig"] = FALSE
							}
						}
					} else {
						seVal = suppressWarnings(as.numeric(parameterTable[i, ifelse(std, "Std.SE", "SE")]))
						CI95  = seVal * 1.96
						bounds = c(est - CI95, est + CI95)

						if(any(is.na(bounds))) {
							# protect cases with SE == NA from evaluation for significance
						} else {
							if (any(bounds <= 0) & any(bounds >= 0)){
								parameterTable[i, "sig"] = FALSE
							}
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
						robustScaling = attr(modelSummary, "robustScalingFactors")
						if (umx_is_GSEM(model)) {
							cat("\n*Statistical Note*: For GSEM models, evaluate absolute fit using SRMR (< 0.10) and CFI. Evaluate model improvements using change in CFI and change in SRMR (see ?umxCompare for details).\n")
						} else if (!is.null(robustScaling) && identical(robustScaling$correction, "Savalei2021")) {
							cat(sprintf("\n*Statistical Note*: Ordinal WLS robust indices use Savalei (2021) cML corrections with catML scaling (c_model = %.3f, c_null = %.3f). Conventional Hu & Bentler (1999) cutoffs apply to these robust CFI/TLI/RMSEA values.\n", robustScaling$cModel, robustScaling$cNull))
						} else {
							cat("\n*Statistical Note*: For WLS models, due to weight-matrix and N-inflation, conventional cutoffs for CFI, TLI, and RMSEA are not applicable.\nEvaluate absolute fit using SRMR (< 0.10), and nested comparisons using the Strict Satorra-Bentler \u0394 \u03C7\u00B2. (see ?umxCompare for details).\n")
						}
						if (identical(uncertainty, "RobustSE")) {
							cat("*SEs*: WLS asymptotic (GMM moment sandwich using the weight matrix and asym. cov. of summary statistics). These are the usual OpenMx WLS SEs—not ML casewise sandwich SEs.\n")
						}
					} else {
						robustScaling = attr(modelSummary, "robustScalingFactors")
						if (!is.null(robustScaling)) {
							isFiml = FALSE
							if (!is.null(model$data) && model$data$type == "raw" && !is.null(model$data$observed)) {
								manifests = model$manifestVars
								if (is.null(manifests) || length(manifests) == 0) {
									manifests = colnames(model$data$observed)
								}
								isFiml = any(is.na(model$data$observed[, manifests, drop = FALSE]))
							}
							if (isFiml) {
								cat(sprintf("\n*Statistical Note*: FIML robust indices use Yuan-Bentler trace scaling + Brosseau-Liard/Savalei adjustment (c_model = %.3f, c_null = %.3f).\n", robustScaling$cModel, robustScaling$cNull))
							} else {
								cat(sprintf("\n*Statistical Note*: Robust ML indices use sandwich-based scaling + Brosseau-Liard & Savalei (2012) adjustments to CFI/TLI/RMSEA (c_model = %.3f, c_null = %.3f).\n", robustScaling$cModel, robustScaling$cNull))
							}
						} else {
							if(TLI_OK   != "OK"){ message("TLI is worse than desired (>.95)") }
							if(RMSEA_OK != "OK"){ message("RMSEA is worse than desired (<.06)")}
						}
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
		robustScaling = attr(modelSummary, "robustScalingFactors")
		if (!is.null(robustScaling)) {
			attr(parameterTable, "robustScalingFactors") = robustScaling
			attr(parameterTable, "c_model") = robustScaling$cModel
			attr(parameterTable, "c_null") = robustScaling$cNull
		}
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
