# TODO: umxPlotCPnew Add SEstyle code from plotCP
# TODO: umxPlotCPnew Add new label trimming code if necessary
#' Draw and display a graphical figure of Common Pathway model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' # @aliases plot.MxModelCP
#' @param x The Common Pathway \code{\link{mxModel}} to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param SEstyle report "b (se)" instead of b CI95[l, u] (Default = FALSE)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - \code{\link{umxCP}}
#' @family Plotting functions
#' @family Twin Reporting Functions
#' @references - \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' require(umx)
#' umx_set_optimizer("SLSQP")
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
# # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP("new", selDVs = selDVs, sep = "_T", 
#' 	dzData = dzData, mzData = mzData, nFac = 3
#' )
#' # m1 = mxTryHardOrdinal(m1)
#' umxPlotCPnew(m1)
#' plot(m1) # No need to remember a special name: plot works fine!
#' }
umxPlotCPnew <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	# TODO Check I am handling nFac > 1 properly!!
	# New plot functions no longer dependent on labels. This means they need to know about the correct matrices to examine.
	# 1. a_cp_matrix = A latent (and correlations among latents)
	# 	* These go from a_cp n=row TO common n= row
	# 	* Or for off diag, from a_cp n=col TO a_cp n= row
	# out = umx_dot_from_matrix(a_cp_matrix, from = "rows", cells = "diag", type = "latent")
	# out = umx_dot_from_matrix(a_cp_matrix, from = "rows", cells = "lower", arrows = "both", type = "latent", strIn = out)
	# 2. Same again for c_cp_matrix, e_cp_matrix
	# 3. cp_loadings common factor loadings
	# No longer used: parameterKeyList = omxGetParameters(model)

	umx_check_model(m1, "MxModelCP", calling="umxPlotCP")

	format = match.arg(format)
	model = x # just to emphasise that x has to be a model 
	if(std){ model = umx_standardize_CP(model) }

	facCount = dim(model$top$a_cp$labels)[[1]]
	varCount = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(varCount)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())
	# Process x_cp matrices
	# 1. Collect latents on the diag
	# from = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_graphviz_mat2dot(model$top$a_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)
	out = umx_graphviz_mat2dot(model$top$c_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)
	out = umx_graphviz_mat2dot(model$top$e_cp, cells = "diag", from = "rows", toLabel = "common", fromType = "latent", p = out)

	# 2. Factor correlations on the lower
	# from = "<name><rowNum>"; target = "<name><colNum>"
	out = umx_graphviz_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_graphviz_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_graphviz_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", p = out)

	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_graphviz_mat2dot(model$top$cp_loadings, cells= "any", toLabel= selDVs, from= "cols", fromLabel= "common", fromType= "latent", p= out)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)

	# Process "as" matrix
	out = umx_graphviz_mat2dot(model$top$as, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_graphviz_mat2dot(model$top$cs, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)
	out = umx_graphviz_mat2dot(model$top$es, cells = "any", toLabel = selDVs, from = "rows", fromType = "latent", p = out)

	# Process "expMean" 1 * nVar matrix
	if(means){
		# from = "one"; target = selDVs[c]
		out = umx_graphviz_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", p = out)
	}
	preOut  = umx_graphviz_define_shapes(latents = out$latents, manifests = selDVs[1:varCount])
	top     = umx_graphviz_rank(out$latents, "^[ace]_cp", "min")
	bottom  = umx_graphviz_rank(out$latents, "^[ace]s[0-9]+$", "max")
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, top, bottom, out$str, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	}
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
	# TODO umxPlotCP could tabulate thresholds?
	# Process "_dev" (where are these?)
	# cat(out$str)
}