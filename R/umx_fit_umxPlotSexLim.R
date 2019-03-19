# TODO: umxPlotSexLim Add SEstyle code from plotCP

#' Draw and display a graphical figure of a Sex limitation model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' # @aliases plot.MxModelCP
#' @param x \code{\link{mxModel}} to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param SEstyle report "b (se)" instead of "b [lower, upper]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - \code{\link{umxCP}}
#' @family Plotting functions
#' @references - \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' require(umx)
#' umx_set_optimizer("SLSQP")
#' data("us_skinfold_data")
#' # Rescale vars
#' us_skinfold_data[, c('bic_T1', 'bic_T2')] = us_skinfold_data[, c('bic_T1', 'bic_T2')]/3.4
#' us_skinfold_data[, c('tri_T1', 'tri_T2')] = us_skinfold_data[, c('tri_T1', 'tri_T2')]/3
#' us_skinfold_data[, c('caf_T1', 'caf_T2')] = us_skinfold_data[, c('caf_T1', 'caf_T2')]/3
#' us_skinfold_data[, c('ssc_T1', 'ssc_T2')] = us_skinfold_data[, c('ssc_T1', 'ssc_T2')]/5
#' us_skinfold_data[, c('sil_T1', 'sil_T2')] = us_skinfold_data[, c('sil_T1', 'sil_T2')]/5
#'
#' # Data for each of the 5 twin-type groups
#' mzmData = subset(us_skinfold_data, zyg == 1)
#' mzfData = subset(us_skinfold_data, zyg == 2)
#' dzmData = subset(us_skinfold_data, zyg == 3)
#' dzfData = subset(us_skinfold_data, zyg == 4)
#' dzoData = subset(us_skinfold_data, zyg == 5)
#'
#' # ==========================
#' # = Run univariate example =
#' # ==========================
#' m1 = umxSexLim(selDVs = "bic", sep = "_T", A_or_C = "A", autoRun= FALSE,
#'		mzmData = mzmData, dzmData = dzmData, 
#'		mzfData = mzfData, dzfData = dzfData, 
#'		dzoData = dzoData
#')
#' m1 = mxTryHard(m1)
#' umxPlotSexLim(m1)
#' plot(m1) # no need to remember a special name: plot works fine!
#' }
umxPlotSexLim <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	# TODO All this is CP code
	# New plot functions no longer dependent on labels. This means they need to know about the correct matrices to examine.
	# 1. a_cp_matrix = A latent (and correlations among latents)
	# 	* These go from a_cp n=row TO common n= row
	# 	* Or for off diag, from a_cp n=col TO a_cp n= row
	# 2. Same again for c_cp_matrix, e_cp_matrix
	# 3. cp_loadings common factor loadings

	format = match.arg(format)
	model = x # just to emphasize that x has to be a model 
	umx_check_model(model, "MxModelSexLim", callingFn= "umxPlotSexLim")

	if(std){ model = umx_standardize_SexLim(model) }

	facCount = dim(model$top$a_cp$labels)[[1]]
	varCount = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(varCount)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())
	# Process x_cp matrices
	# 1. Collect latents on the diag
	# from   = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_dot_mat2dot(model$top$a_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$c_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$e_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)

	# 2. On the lower
	# from = "<name><rowNum>"; target = "<name><colNum>"
	out = umx_dot_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_dot_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_dot_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", p = out)

	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_dot_mat2dot(model$top$cp_loadings, cells= "any", toLabel= selDVs, from= "cols", fromLabel= "common", fromType= "latent", p= out)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)

	# Process "as" matrix
	out = umx_dot_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_dot_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = umx_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", p = out)
	}
	preOut  = umx_dot_define_shapes(latents = out$latents, manifests = selDVs[1:varCount])
	top     = umx_dot_rank(out$latents, "^[ace]_cp", "min")
	bottom  = umx_dot_rank(out$latents, "^[ace]s[0-9]+$", "max")
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, top, bottom, out$str, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	}
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	# TODO umxPlotCP could tabulate thresholds?
	# Process "_dev" (where are these?)
	# cat(out$str)
}

#' @export
plot.MxModelSexLim <- umxPlotSexLim
