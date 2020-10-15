#' @export
umxPlotIP2 <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	format = match.arg(format)
	model = x # Just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelIP", callingFn = "umxPlotIP")
	
	if(std){
		model = xmu_standardize_IP(model)
	}

	# 1. get vars from rows of as matrix
	nFac   = dim(model$top$ai$labels)[[2]] # added! (but won't work, right? can have different numbers of a c, e )
	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end

	out = list(str = "", latents = c(), manifests = c())

	# TODO Check I am handling nFac > 1 properly!!

	# from = <name><rowNum>; target = common<colNum>; latents = append(latents, from)
	# out = list(str = "", latents = c(), manifests = c())

	# 1. Collect ai (the independent latent factors)
	out = xmu_dot_mat2dot(model$top$ai, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$ci, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$ei, cells = "any", from = "cols", fromType = "latent", toLabel = selDVs, showFixed = showFixed, p = out)

	# 2 collect as (the specific latent factors)
	out = xmu_dot_mat2dot(model$top$as, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$cs, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(model$top$es, cells = "diag", toLabel = selDVs, from = "rows", fromType = "latent", showFixed = showFixed, p = out)


	# Process "expMean" 1 * nVar matrix e.g. "expMean_gff_T1"
	if(means){
		out = xmu_dot_mat2dot(model$top$expMean, cells = "left", toLabel = selDVs, from = "rows", fromLabel = "one", fromType = "latent", showFixed = showFixed, p = out)
	}
	
	# TODO: Could extract thresholds? "_dev[0-9]+$"

	# TODO Add CIs to parameter values
	# this code picks out the CIs if available... Now would need to be embedded in xmu_dot_mat2dot() now?
	# CIstr = xmu_get_CI(model, label = thisParam, prefix = "top.", suffix = "_std", digits = digits, SEstyle = SEstyle, verbose = FALSE)
	# CIstr = xmu_get_CI(model= tmp, label = "S[1,1]", prefix = "Holzinger_and_Swineford_1939.", SEstyle = TRUE, digits = 3)

	# ==============
	# = up to here =
	# ==============

	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
		if(var == "one"){
			preOut = paste0(preOut, "\t", var, " [shape = triangle];\n")
		} else {
			preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}
	}
	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = selDVs[1:nVar])
	top     = xmu_dot_rank(out$latents, "^[ace]i", "min")
	bottom  = xmu_dot_rank(out$latents, "^[ace]s", "max")

	label = model$name
	splines = "FALSE"

	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, 
		top, 
		bottom, 
		out$str, "\n}"
	)

	if(format != "current"){ umx_set_plot_format(format) }
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

	
