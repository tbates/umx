#' Draw a graphical figure for a Independent Pathway model
#'
#' Options include digits (rounding), showing means or not, standardization, and which output format is desired.
#'
#' @param x The [umxIP()] model to plot
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR")
#' @param SEstyle Report "b (se)" instead of "b \[lower, upper\]" (Default)
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)
#' @param ... Optional additional parameters
#' @return - optionally return the dot code
#' @seealso - [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @seealso - [umxIP()]
#' @family umx deprecated
#' @references - <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData = subset(GFF, zyg_2grp == "MZ")
#' dzData = subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' xmuOldPlotIP(model, file = NA)
#' }
xmuOldPlotIP <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, format = c("current", "graphviz", "DiagrammeR"), SEstyle = FALSE, strip_zero = TRUE, ...) {
	format = match.arg(format)
	model = x # Just to emphasise that x has to be a model 
	umx_check_model(model, "MxModelIP", callingFn = "umxPlotIP")
	
	if(std){
		model = xmu_standardize_IP(model)
	}
	# TODO Check I am handling nFac > 1 properly!!
	nVar   = dim(model$top$ai$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs) # trim "_Tn" from end
	
	parameterKeyList = omxGetParameters(model, free = TRUE);
	cSpecifics = c();
	latents = c()
	out = "";
	for(thisParam in names(parameterKeyList) ) {
		if( grepl("^[ace]i_r[0-9]", thisParam)) {
			# top level a c e
			# "ai_r1c1" note: c1 = factor1, r1 = variable 1
			grepStr = '^([ace]i)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1_\\3', thisParam, perl = TRUE);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl=T));
			target  = selDVs[as.numeric(targetindex)]
			latents = append(latents, from);
		} else if (grepl("^[ace]s_r[0-9]", thisParam)) { # specific
			grepStr = '([ace]s)_r([0-9]+)c([0-9]+)'
			from    = sub(grepStr, '\\1\\3', thisParam, perl = T);
			targetindex = as.numeric(sub(grepStr, '\\2', thisParam, perl = T));
			target  = selDVs[as.numeric(targetindex)]
			cSpecifics = append(cSpecifics,from);
			latents = append(latents,from);
		} else if (grepl("^expMean", thisParam)) { # means probably "expMean_gff_T1" (was "expMean_r1c1")
			grepStr = '^expMean_(.*_T1)'
			from    = "one";
			target = sub(grepStr, '\\1', thisParam, perl = TRUE)
			if(means){
				latents = append(latents, from)
			}
		} else if (grepl("_dev[0-9]+$", thisParam)) { # probably a threshold
			# grepStr = '^expMean_(.*_T1)'
			# from    = "one";
			# target = sub(grepStr, '\\1', thisParam, perl = TRUE)
			# if(means){
			# 	latents = append(latents, from)
			# }
		} else {
			message("While making the plot, I found a path labeled ", thisParam, "I don't know where that goes.\n",
			"If you are using umxModify to make newLabels, instead of making up a new label, use, say, the first label in update as the newLabel to help plot()")
		}

		# look for CIs if they exist...
		if(!means & from == "one"){
			# not adding means...
		} else {
			# look for standardized values to replace the raw ones...
			CIstr = xmu_get_CI(model, label = thisParam, prefix = "top.", suffix = "_std", digits = digits, SEstyle = SEstyle, verbose = FALSE)
			if(is.na(CIstr)){
				val = round(parameterKeyList[thisParam], digits)
			}else{
				val = CIstr
			}
			out = paste0(out, ";\n", from, " -> ", target, " [label=\"", val, "\"]")
		}
	}

	preOut = "\t# Latents\n"
	latents = unique(latents)
	for(var in latents) {
		if(var == "one"){
			preOut = paste0(preOut, "\t", var, " [shape = triangle];\n")
		} else {
			preOut = paste0(preOut, "\t", var, " [shape = circle];\n")
		}
	}
	preOut = paste0(preOut, "\n\t# Manifests\n")
	for(n in c(1:nVar)) {
	   preOut = paste0(preOut, "\n", selDVs[n], " [shape=square];\n")
	}

	ranks = paste(cSpecifics, collapse = "; ");
	ranks = paste0("{rank=sink; ", ranks, "}");

	label = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut,
		ranks,
		out, "\n}"
	)

	if(format != "current"){ umx_set_plot_format(format) }
	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)

}

	
