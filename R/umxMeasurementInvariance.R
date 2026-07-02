#' Test measurement invariance of a confirmatory factor analysis model
#'
#' @description
#' `umxMeasurementInvariance` runs a sequence of multi-group CFA models
#' to test for configural, weak (metric), strong (scalar), and strict invariance
#' across groups.
#'
#' @param model A lavaan syntax string defining the CFA model.
#' @param data A data frame containing the data.
#' @param group Character string, the name of the grouping variable.
#' @param type Character vector, which tests to run. Default is "all", which runs configural, weak, strong, and strict models.
#' @param compare Character string, how to compare the models: "both", "sequential", or "configural". Default is "both".
#' @param silent Boolean, whether to suppress output and messages. Default is FALSE.
#' @param ... Additional arguments passed to [umxRAM()].
#' @return A model comparison table (data frame). If `compare` is "both" (default) or "sequential", the sequential comparison table is returned, and the configural/baseline comparison table is attached as the `configural` attribute. If `compare` is "configural", the baseline comparison table is returned.
#' @export
#' @family Model Comparison
#' @seealso - [umxRAM()], [umxCompare()]

#' @examples
#' \dontrun{
#' data("HS.ability.data", package = "OpenMx")
#' df = umx_scale(HS.ability.data[, c("visual", "cubes", "flags", "school")])
#' model = "spatial =~ visual + cubes + flags"
#' umxMeasurementInvariance(model, data = df, group = "school")
#' }
umxMeasurementInvariance <- function(model, data = NULL, group = NULL, type = "all", compare = c("both", "sequential", "configural"), silent = FALSE, ...) {
	if (is.null(group)) {
		stop("You must specify a 'group' variable to test measurement invariance.")
	}

	compare = match.arg(compare)

	all_types = c("configural", "weak", "strong", "strict")
	if (length(type) == 1 && type == "all") {
		run_types = all_types
	} else {
		run_types = match.arg(type, all_types, several.ok = TRUE)
	}

	models = list()

	# 1. Configural
	if ("configural" %in% run_types) {
		if (!silent) message("Fitting Configural Model...")
		models$Configural = umxRAM(model, data = data, group = group, ...)
	}

	# 2. Metric (Weak)
	if ("weak" %in% run_types) {
		if (!silent) message("Fitting Metric (Weak) Model...")
		models$Metric = umxRAM(model, data = data, group = group, group.equal = "loadings", ...)
	}

	# 3. Scalar (Strong)
	if ("strong" %in% run_types) {
		if (!silent) message("Fitting Scalar (Strong) Model...")
		models$Scalar = umxRAM(model, data = data, group = group, group.equal = c("loadings", "intercepts"), ...)
	}

	# 4. Strict
	if ("strict" %in% run_types) {
		if (!silent) message("Fitting Strict Model...")
		models$Strict = umxRAM(model, data = data, group = group, group.equal = c("loadings", "intercepts", "residuals"), ...)
	}

	# Rename models for clear comparison presentation
	if ("configural" %in% run_types) {
		models$Configural = mxRename(models$Configural, "Configural")
	}
	if ("weak" %in% run_types) {
		models$Metric = mxRename(models$Metric, "Metric")
	}
	if ("strong" %in% run_types) {
		models$Scalar = mxRename(models$Scalar, "Scalar")
	}
	if ("strict" %in% run_types) {
		models$Strict = mxRename(models$Strict, "Strict")
	}

	model_list = unlist(models)
	if (length(model_list) <= 1) {
		if (length(model_list) == 1) {
			comparison = summary(model_list[[1]])
			attr(comparison, "models") <- models
			return(comparison)
		} else {
			stop("No models were fitted.")
		}
	}

	# Extract report and digits if they are in ... or use defaults
	dots = list(...)
	report = if (!is.null(dots$report)) dots$report else "markdown"
	digits = if (!is.null(dots$digits)) dots$digits else 3

	configural_compare = NULL
	sequential_compare = NULL

	# Compute comparisons
	if (compare %in% c("both", "configural")) {
		configural_compare = umxCompare(model_list[[1]], model_list[2:length(model_list)], silent = TRUE, digits = digits, report = report)
	}

	if (compare %in% c("both", "sequential")) {
		# First comparison: model_list[[1]] vs model_list[[2]]
		comp_first = umxCompare(model_list[[1]], model_list[[2]], silent = TRUE, digits = digits, report = report)
		sequential_compare = comp_first
		
		if (length(model_list) > 2) {
			for (i in 2:(length(model_list) - 1)) {
				comp_pair = umxCompare(model_list[[i]], model_list[[i+1]], silent = TRUE, digits = digits, report = report)
				model_name = names(model_list)[i+1]
				row_idx = which(comp_pair$Model == model_name)
				if (length(row_idx) == 0) {
					row_idx = 2
				}
				comp_row = comp_pair[row_idx, , drop = FALSE]
				sequential_compare = rbind(sequential_compare, comp_row)
			}
		}
	}

	# Printing
	if (!silent) {
		if (compare == "both") {
			umx_print(sequential_compare, digits = digits, zero.print = "0", caption = "Table of Sequential Model Comparisons", report = report)
			cat("\n")
			umx_print(configural_compare, digits = digits, zero.print = "0", caption = "Table of Baseline Model Comparisons", report = report)
		} else if (compare == "sequential") {
			umx_print(sequential_compare, digits = digits, zero.print = "0", caption = "Table of Sequential Model Comparisons", report = report)
		} else if (compare == "configural") {
			umx_print(configural_compare, digits = digits, zero.print = "0", caption = "Table of Baseline Model Comparisons", report = report)
		}
		cat("\n*Note*: EP = Estimated (i.e. free) parameters; \u0394-2LL = change in -2 \u00D7 Log-Likelihood of the model; \u0394 df = Change in degrees of freedom with respect to the comparison model; \u0394 AIC = Change in Akaike Information Criterion; 'Compared to' = The baseline model for this comparison.\n")
	}

	# Return structure
	if (compare == "configural") {
		res = configural_compare
	} else {
		res = sequential_compare
		if (compare == "both") {
			attr(res, "configural") <- configural_compare
		}
	}

	attr(res, "models") <- models
	return(res)
}
