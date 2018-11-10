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
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
# # These will be expanded into "gff_T1" "gff_T2" etc.
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP("new", selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, 
#' 	nFac = 3, correlatedA = TRUE
#' )
#' m1 = mxTryHardOrdinal(m1)
#' umxPlotCPnew(m1)
#' plot(m1) # no need to remember a special name: plot works fine!
#' }
umxPlotCPnew <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, ...) {
	if(!class(x) == "MxModelCP"){
		stop("The first parameter of umxPlotCP must be a CP model, you gave me a ", class(x))
	}

	# new plot functions no longer dependend on labels. This means they need to know about the correct
	# matrices to examine.
	# In this case, would need to examine:
	# 1. a_cp_matrix = A latent (and correlations among latents)
	# 	* these go from a_cp n=row TO common n= row
	# 	* or for off diag, from a_cp n=col TO a_cp n= row
	# out = umx_dot_from_matrix(a_cp_matrix, from = "rows", cells = "diag", type = "latent")
	# out = umx_dot_from_matrix(a_cp_matrix, from = "rows", cells = "lower", arrows = "both", type = "latent", strIn = out)
	# 2 same again for c_cp_matrix, e_cp_matrix
	# 3. cp_loadings common factor loadings

	# no longer used: parameterKeyList = omxGetParameters(model)

	format = match.arg(format)
	model = x # just to emphasise that x has to be a model 
	if(std){
		model = umx_standardize_CP(model)
	}
	# TODO Check I am handling nFac > 1 properly!!
	facCount = dim(model$top$a_cp$labels)[[1]]
	varCount = dim(model$top$as$values)[[1]]
	selDVs   = dimnames(model$MZ$data$observed)[[2]]
	selDVs   = selDVs[1:(varCount)]
	selDVs   = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())
	# Looks at matrices, not labels	
	# Process a_cp matrix: latents into common paths
	# 1. On the diag
	# from   = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_mat2dot(model$top$a_cp, cells = "diag", from = "rows", toLabel = "common", type = "latent", p = out)
	out = umx_mat2dot(model$top$c_cp, cells = "diag", from = "rows", toLabel = "common", type = "latent", p = out)
	out = umx_mat2dot(model$top$e_cp, cells = "diag", from = "rows", toLabel = "common", type = "latent", p = out)
	# GOOD

	# 2. On the lower
	# from   = "<name><rowNum>"
	# target = "<name><colNum>"
	out = umx_mat2dot(model$top$a_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_mat2dot(model$top$c_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	out = umx_mat2dot(model$top$e_cp, cells = "lower", from = "cols", arrows = "both", p = out)
	# Process "cp_loadings" nManifests * nFactors matrix: latents into common paths.
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_mat2dot(model$top$cp_loadings, cells= "any", selDVs= selDVs, from= "cols", fromLabel= "common", type= "latent", p= out)
	# from    = "common<c>"
	# target  = selDVs[row]
	# latents = append(latents, from)
	# Process "as" matrix
	# out = list(str = "", latents = c(), manifests = c())
	out = umx_mat2dot(model$top$as, cells = "any", selDVs = selDVs, from = "rows", type = "latent", p = out)
	out = umx_mat2dot(model$top$cs, cells = "any", selDVs = selDVs, from = "rows", type = "latent", p = out)
	out = umx_mat2dot(model$top$es, cells = "any", selDVs = selDVs, from = "rows", type = "latent", p = out)
	# target  = selDVs[as.numeric(rowNum)]
	# Process "expMean" 1 * nVar matrix
	if(means){
		# out = list(str = "", latents = c(), manifests = c())
		out = umx_mat2dot(model$top$expMean, cells = "left", selDVs = selDVs, from = "rows", fromLabel = "one", type = "latent", p = out)
		# from = "one"
		# target = selDVs[c]
	}
	# Process "_dev" (where are these?)
	# TODO umxPlotCP could tabulate thresholds?
	# "_dev[0-9]"
	# cat(out$str)
		
	preOut = "# Latents\n"
	latents = unique(out$latents) # TODO unique already done
	for(var in latents) {
		if(var == "one"){
			preOut = paste0(preOut, "\t", var, " [shape = triangle];\n")
		} else {
			preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}
	}
	preOut = paste0(preOut, "\n# Manifests\n")
	for(n in c(1:varCount)) {
	   preOut = paste0(preOut, "\t", selDVs[n], " [shape = square];\n")
	}
	bottom = umx_graphviz_rank(out$latents, "^[ace]s[0-9]+$", "max")
	top = umx_graphviz_rank(out$latents, "^[ace]_cp", "min")
	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", preOut, top, bottom, out, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	}
	xmu_dot_maker(model, file, digraph)
}