#' Pearson-Aitken selection models in umxRAM
#'
#' @description
#' When a sample is selected on a variable, bare correlations mislead: selecting
#' high scorers on education (caused by IQ and C) induces a spurious negative
#' IQ-C correlation (collider bias). Pearson-Aitken selection models recover the
#' population moments given the known population variance of the selection
#' variable. umx supports two patterns:
#'
#' * **Correction** (non-degenerate): `selection = list(varNames=, popCov=)`
#'   adds free `arrows=0` selected (co)variance paths plus a constraint equating
#'   the implied population moments to `popCov` (the identifying assumption).
#'   `popCov` is a scalar for one variable, a matrix or length-k*k vector for a
#'   selected SET (personnel battery hired on several tests), recovering each
#'   predictor's population validity (Schmidt & Hunter (1998) case, with a
#'   runnable battery demo in the examples below).
#'   Use [umx_make_selection_data()] to simulate the IQ/C/EA demo below.
#' 
#' * **Assortment** (degenerate): hand-written `arrows=0` cross-covariances, as
#'   in the Maes et al. (1998) spouse model ([maes1998]). Here the selection
#'   set covers every variable, so no Aitken adjustment happens: the paths are
#'   free covariances drawn without arrowheads.
#'
#' **Note**: Means stay free but are saturated nuisance under selection, not population
#' quantities: interpret covariances, not means.
#'
#' The multi-variable form declares the whole selection set at once in a list:
#' First `varNames`: the hiring battery, then `popCov` the known population covariances
#'
#' ```r
#' selection = list(varNames = c("COG", "INT", "BIO"), popCov = applicantCov)
#' ```
#' **Notes:**
#' `varNames` may be omitted when `popCov` is a named matrix (var names are inferred);
#' If you just offer a length-k*k vector for popCov `varNames` is needed to disambiguate it.
#'
#' @name umx_concept_selection
#' @aliases umxRAM-selection
#' @concept selection models
#' @concept Pearson-Aitken
#' @family Concepts
#' @seealso [umxRAM()], [umx_make_selection_data()], [umxPath()]
#' @references
#' Schmidt, F. L., & Hunter, J. E. (1998). The validity and utility of
#'   selection methods in personnel psychology: Practical and theoretical
#'   implications of 85 years of research findings. *Psychological Bulletin*,
#'   **124**, 262-274 \doi{10.1037/0033-2909.124.2.262}.
#'
#' Maes, H. H., Neale, M. C., Kendler, K. S., Hewitt, J. K.,
#'   Silberg, J. L., Foley, D. L., ... & Eaves, L. J. (1998). Assortative
#'   mating for major psychiatric diagnoses in two population-based samples.
#'   *Psychological Medicine*, **28**, 1389-1401 \doi{10.1017/s0033291798007326}.
#' @examples
#' \dontrun{
#' 
#'
#' # ===========================
#' # = 1. Correction pattern:  =
#' # ===========================
#' #' # Recover r(IQ, C) = 0 from a selected sample
#' 
#' d = umx_make_selection_data()
#' 
#' m = umxRAM("selDemo", data = d$sel,
#'	selection = list(varNames = "EA", popCov = 1.0),
#'	umxPath(v.m. = c("IQ", "C", "EA")),
#' 	umxPath(from = c("IQ", "C"), to = "EA"),
#' 	umxPath("IQ", with = "C")
#' )
#' 
#' # ============================
#' # = #2.  Assortment pattern: =
#' # ============================
#' # free cross-spouse covariances (full model in ?maes1998)
#' 
#' data(maes1998)
#' 
#' # ================================
#' # = # 3. Schmidt-Hunter battery: =
#' # ================================
#' #' hired on 3 tests, recover population validities
#' 
#' # Create the data
#' set.seed(100)
#' Rpop = matrix(c(1.00, 0.30, 0.50, 0.51,
#'                 0.30, 1.00, 0.25, 0.51,
#'                 0.50, 0.25, 1.00, 0.35,
#'                 0.51, 0.51, 0.35, 1.00), 4, 4)
#' colnames(Rpop) = rownames(Rpop) = c("COG", "INT", "BIO", "PERF")
#' pop = as.data.frame(matrix(rnorm(6000 * 4), ncol = 4) %*% chol(Rpop))
#' names(pop) = c("COG", "INT", "BIO", "PERF")
#'
#' # a. The erroneous result, ignoring selection
#' comp = pop$COG + pop$INT + pop$BIO
#' hired = pop[comp > quantile(comp, 0.7), ]
#' tests = c("COG", "INT", "BIO")
#' # Correlation with performance
#' round(cor(hired[, tests], hired$PERF), 3) # naive: .23, .28, -.01 (attenuated)
#'
#' # b. The Correct result by Pearson-Aitken model of selection.
#' m = umxRAM("battery", data = hired, autoRun = TRUE,
#' 	selection = list(varNames = tests, popCov = Rpop[tests, tests]),
#'	umxPath(v.m. = c(tests, "PERF")),
#' 	umxPath(unique.bivariate = c(tests, "PERF"))
#' )
#' cc = coef(m); S = m$S$values
#' c(COG = cc[["COG_with_PERF"]], INT = cc[["INT_with_PERF"]], BIO = cc[["BIO_with_PERF"]]) /
#' 	sqrt(diag(S)[tests] * S["PERF", "PERF"])
#' # recovered: .50, .50, .34 (truth .51, .51, .35)
#' }
NULL
