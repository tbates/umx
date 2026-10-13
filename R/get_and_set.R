# ==============================
# = Get and set OpenMx options =
# ==============================

#' Display umx options
#'
#' Show the umx options. Useful for beginners to discover, or people like me to remember :-)
#'
#' @return - message
#' @export
#' @family Get and set
#' @examples
#' umx_get_options()
umx_get_options <- function() {
	umx_set_auto_plot()
	umx_set_plot_format()
	umx_set_plot_file_suffix()
	umx_set_table_format()
	umx_set_optimizer()
	umx_set_auto_run() 
	umx_set_condensed_slots()
	message(umx_set_cores(silent = TRUE), " cores will be used")	
}


#' Set output suffix used in umx SEM diagram files saved to disk.
#'
#' `umx` SEM diagram files can have a suffix of "gv" (default) or  "dot".
#' Interrogate the setting by calling with no value: it will return the current setting. 
#' To change the setting call with "gv" or "dot". Or use TRUE to toggle the setting.
#'
#' @param umx.plot.suffix The suffix for plot files (if empty current value is returned). "TRUE", toggles setting.
#' @param silent If TRUE, no message will be printed.
#' @return - Current setting
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' umx_set_plot_file_suffix() # print current state
#' old = umx_set_plot_file_suffix(silent = TRUE) # store current value
#' umx_set_plot_file_suffix("dot")
#' umx_set_plot_file_suffix("gv")
#' umx_set_plot_file_suffix(old) # reinstate
umx_set_plot_file_suffix <- function(umx.plot.suffix = NULL, silent = FALSE) {
	if(is.null(umx.plot.suffix)) {
		if(!silent){
			message("Current format is", 
				omxQuotes(getOption("umx.plot.suffix")),
				". Valid options are 'gv' or 'dot'. Use TRUE to toggle"
			)
		}
		invisible(getOption("umx.plot.suffix"))
	} else {
		if(umx.plot.suffix == TRUE){
			# if T then toggle
			if(getOption("umx.plot.suffix") == "gv"){
				umx.plot.suffix = "dot"
			} else {
				umx.plot.suffix = "gv"
			}
		} else {
			umx_check(umx.plot.suffix %in% c("gv", "dot"), "stop", "valid options are 'gv' or 'dot'. Use TRUE to toggle)")
		}
		options("umx.plot.suffix" = umx.plot.suffix)
	}
}

#' Set output format of plots (structural diagrams) in umx
#'
#' Set output format of plots (default = "[DiagrammeR::DiagrammeR()]", alternatives are graphviz, svg, png, pdf). If you call this with no
#' value, it will return the current setting. If you call it with TRUE, it toggles the setting.
#'
#' @param umx.plot.format format for plots (if empty, returns the current value of umx.plot.format). If "TRUE", then toggles
#' @param silent If TRUE, no message will be printed.
#' @return - Current umx.plot.format setting
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' umx_set_plot_format() # print current state
#' old = umx_set_plot_format(silent = TRUE) # store current value
#' umx_set_plot_format("graphviz")
#' umx_set_plot_format("DiagrammeR")
#' umx_set_plot_format("png")
#' umx_set_plot_format("pdf")
#' umx_set_plot_format(old) # reinstate
umx_set_plot_format <- function(umx.plot.format = NULL, silent = FALSE) {
	if(is.null(umx.plot.format)) {
		if(!silent){
			message("Current format is ", 
				omxQuotes(getOption("umx.plot.format")),
				". Valid options are 'DiagrammeR', 'pdf', 'png', 'svg', or 'graphviz'. TRUE toggles between DiagrammeR and graphviz"
			)
		}
		invisible(getOption("umx.plot.format"))
	} else {
		if(umx.plot.format == TRUE){
			# if T then toggle
			if(getOption("umx.plot.format") == "graphviz"){
				umx.plot.format = "DiagrammeR"
			} else {
				umx.plot.format = "graphviz"
			}
		} else {
			umx_check(umx.plot.format %in% c("DiagrammeR", "pdf", "png", 'svg', "graphviz"), "stop", "valid options are 'DiagrammeR', 'pdf', 'png', 'svg', 'graphviz'. TRUE toggles between DiagrammeR and graphviz)")
		}
		options("umx.plot.format" = umx.plot.format)
	}
}


#' Accumulate scale reliabilities and report them as a table
#'
#' [umx_score_scale()] with `alpha = TRUE` prints reliability coefficients.
#' `umx_scale_reliabilities()` collects these across scales, then reports them together.
#'
#' To use, call with `"init"`, score your scales, then call with `"show"`.
#' 
#' @param action One of `"init"` (start or clear the store), `"add"` (append one row), or `"show"` (print and return the table). `"print"` is accepted as an alias of `"show"`. Default `"show"`.
#' @param scale_name Name of the scale (single string, required when `action = "add"`).
#' @param reliability Reliability value (single numeric, required when `action = "add"`).
#' @param type Type of reliability. When `action = "add"`, a single string (required), e.g. `"alpha"`, `"omega_t"`, or `"omega_h"`. When `action = "show"`, an optional filter: one or more types to report (`NULL`, the default, reports all rows).
#' @param report Format for the printed table: `"markdown"` or `"html"`.
#' @param digits Decimal places for the printed table (default 2). Display only: stored values keep full precision.
#' @return - invisible `data.frame` with columns `scale`, `reliability`, and `type`
#' @export
#' @family Get and set
#' @examples
#' umx_scale_reliabilities("init") # start with an empty store
#' umx_scale_reliabilities("add", scale_name = "Agreeableness", reliability = 0.72, type = "alpha")
#' umx_scale_reliabilities("add", scale_name = "Agreeableness", reliability = 0.75, type = "omega_t")
#' umx_scale_reliabilities("show")
#' umx_scale_reliabilities("show", type = "alpha") # just the alphas
#' umx_scale_reliabilities("init") # clear the store
#' \dontrun{
#' data(bfi, package = "psychTools")
#' umx_scale_reliabilities("init")
#' bfi = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data = bfi, name = "A", alpha = TRUE)
#' umx_scale_reliabilities("show")
#' }
umx_scale_reliabilities <- function(action = "show", scale_name = NULL, reliability = NULL, type = NULL, report = c("markdown", "html"), digits = 2) {
	action = xmu_match.arg(action, c("init", "add", "show", "print"))
	if(action == "print"){ action = "show" }
	if(action == "init"){
		relTable = data.frame(scale = character(), reliability = numeric(), type = character(), stringsAsFactors = FALSE)
		options("umx_scale_reliabilities" = relTable)
		invisible(relTable)
	} else if(action == "add"){
		if(is.null(scale_name) || length(scale_name) != 1L || !is.character(scale_name) || is.na(scale_name) || !nzchar(scale_name)){
			stop("When action = 'add', scale_name must be a single non-empty string.")
		}
		if(is.null(reliability) || length(reliability) != 1L || !is.numeric(reliability)){
			stop("When action = 'add', reliability must be a single numeric value.")
		}
		if(is.null(type) || length(type) != 1L || !is.character(type) || is.na(type) || !nzchar(type)){
			stop("When action = 'add', type must be a single non-empty string, e.g. 'alpha', 'omega_t', or 'omega_h'.")
		}
		relTable = getOption("umx_scale_reliabilities")
		if(is.null(relTable)){
			relTable = data.frame(scale = character(), reliability = numeric(), type = character(), stringsAsFactors = FALSE)
		}
		relTable = relTable[!(relTable$scale == scale_name & relTable$type == type), , drop = FALSE]
		newRow = data.frame(scale = scale_name, reliability = reliability, type = type, stringsAsFactors = FALSE)
		relTable = rbind(relTable, newRow)
		rownames(relTable) = NULL
		options("umx_scale_reliabilities" = relTable)
		invisible(relTable)
	} else {
		if(!is.null(type)){
			if(!is.character(type) || length(type) < 1L || any(is.na(type)) || any(!nzchar(type))){
				stop("When action = 'show', type must be one or more non-empty strings, e.g. 'alpha'.")
			}
		}
		relTable = getOption("umx_scale_reliabilities")
		if(!is.null(relTable) && !is.null(type)){
			relTable = relTable[relTable$type %in% type, , drop = FALSE]
		}
		if(is.null(relTable) || nrow(relTable) == 0L){
			if(is.null(type)){
				message("No scale reliabilities stored: call umx_scale_reliabilities('init'), score scales with alpha = TRUE, then umx_scale_reliabilities('show').")
			} else {
				message("No stored reliabilities of type ", paste(sQuote(type), collapse = ", "), ".")
			}
			relTable = data.frame(scale = character(), reliability = numeric(), type = character(), stringsAsFactors = FALSE)
			invisible(relTable)
		} else {
			umx_print(relTable, report = report, digits = digits)
			invisible(relTable)
		}
	}
}

#' Set the separator
#'
#' Set umx_default_separator (used in CI\[low sep high\] ). Default = ","
#'
#' @param umx_default_separator separator for CIs etc. (if empty, returns the current value)
#' @param silent If TRUE, no message will be printed.
#' @return - Current umx_default_separator
#' @export
#' @family Get and set
#' @examples
#' library(umx)
#' umx_set_separator() # show current state
#' old = umx_set_separator(silent=TRUE) # store existing value
#' umx_set_separator("|")
#' umxAPA(.3, .2)
#' umx_set_separator(old)    # reinstate
umx_set_separator <- function( umx_default_separator = NULL, silent = FALSE) {
	if(is.null( umx_default_separator)) {
		if(!silent){
			message("Current separator is", omxQuotes(getOption("umx_default_separator")) )
		}
		invisible(getOption("umx_default_separator"))		
	} else {
			options("umx_default_separator" =  umx_default_separator)
	}
} # end umx_set_separator

#' umx_set_table_format
#'
#' Set knitr.table.format default (output style for tables). Legal values are 
#' "latex", "html", "markdown", "pandoc", and "rst".
#'
#' @param knitr.table.format format for tables (if empty, returns the current value of knitr.table.format)
#' @param silent If TRUE, no message will be printed.
#' @return - Current knitr.table.format setting
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' umx_set_table_format() # show current state
#' old = umx_set_table_format() # store existing value
#' umx_set_table_format("latex")
#' umx_set_table_format("html")
#' umx_set_table_format("markdown")
#' umx_set_table_format("") # get available options
#' umx_set_table_format(old)    # reinstate
umx_set_table_format <- function(knitr.table.format = NULL, silent = FALSE) {
	legal = c('latex', 'html', 'pipe', 'simple', 'markdown', 'rst')
	if(is.null(knitr.table.format)) {
		if(!silent){
			message("Current format is", omxQuotes(getOption("knitr.table.format")), 
				". Valid options are ", omxQuotes(legal)
			)
		}
		invisible(getOption("knitr.table.format"))		
	} else {
		if(!knitr.table.format %in% legal){
			message("legal options are ", omxQuotes(legal))
		} else {
			options("knitr.table.format" = knitr.table.format)
		}
	}
} # end umx_set_table_format


#' umx_set_auto_plot
#'
#' Set autoPlot default for models like umxACE umxGxE etc.
#'
#' @param autoPlot If TRUE, sets the umx_auto_plot option. Else returns the current value of umx_auto_plot
#' @param silent If TRUE, no message will be printed.
#' @return - Current umx_auto_plot setting
#' @export
#' @family Get and set
#' @return - existing value
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' umx_set_auto_plot() # print current state
#' old = umx_set_auto_plot(silent = TRUE) # store existing value
#' old
#' umx_set_auto_plot(TRUE)   # set to on (internally stored as "name")
#' umx_set_auto_plot(FALSE)  # set to off (internally stored as NA)
#' umx_set_auto_plot(old)    # reinstate
umx_set_auto_plot <- function(autoPlot = NULL, silent = FALSE) {	
	oldAutoPlot = getOption("umx_auto_plot")
	if(is.null(oldAutoPlot)){
		# not initialised yet
		options("umx_auto_plot" = NA)
		oldAutoPlot = NA
	}
	if(is.na(oldAutoPlot)){
		oldAutoPlot = FALSE
	} else {
		oldAutoPlot = TRUE
	}
	if(is.null(autoPlot)){
		# just asking...
		if(!silent){
			message("Current plot format is ", omxQuotes(oldAutoPlot), "\n Use TRUE to turn on, FALSE to turn off.")
		}
	}else{
		if(is.na(autoPlot) || autoPlot %in% FALSE){
			# Set to NA (which is interpreted as FALSE
			options("umx_auto_plot" = NA)		
		} else if(autoPlot == 'name' || autoPlot){
			options("umx_auto_plot" = "name")
		}
	}
	invisible(oldAutoPlot)
}

#' umx_set_data_variance_check
#'
#' Set default for data checking in models like umxACE umxGxE etc.
#'
#' @param minVar Set the threshold at which to warn user about variables with too-small variance. Else returns the current value of umx_minVar
#' @param maxVarRatio Set the option for threshold at which to warn user variances differ too much. Else returns the current value of umx_maxVarRatio
#' @param silent If TRUE, no message will be printed.
#' @return - list of umx_minVar and umx_maxVarRatio settings
#' @export
#' @seealso xmu_check_variance which uses these to check sanity in the variances of a data frame.
#' @family Get and set
#' @examples
#' library(umx)
#' umx_set_data_variance_check() # print current state
#' old = umx_set_data_variance_check(silent = TRUE) # store existing value
#' umx_set_data_variance_check(minVar = .01)
#' umx_set_data_variance_check(maxVarRatio = 500)
#' umx_set_data_variance_check(minVar = old$minVar, maxVarRatio = old$maxVarRatio) # reinstate
umx_set_data_variance_check <- function(minVar = NULL, maxVarRatio = NULL, silent = FALSE) {
	if(is.null(minVar)){
		minVar = getOption("umx_minVar")
		if(!silent){
			message("Current threshold for small variance warning in umx functions is ", omxQuotes(getOption("umx_minVar")))
		}
	}else{
		options("umx_minVar" = minVar)		
	}
	if(is.null(maxVarRatio)){
		maxVarRatio = getOption("umx_maxVarRatio")
		if(!silent){
			message("Current threshold for excess ratio among variances warning in umx functions is ", omxQuotes(getOption("umx_maxVarRatio")))
		}
	}else{
		options("umx_maxVarRatio" = maxVarRatio)		
	}
	invisible(list(minVar = minVar, maxVarRatio = maxVarRatio))
}

#' Turn off most console and summary output from umx
#'
#' Running multiple analyses or simulations, it can be handy to turn off the automatic summary, 
#' graphing, and printing that umx does to help interactive sessions. `umx_set_silent` does this.
#' Summary and graph output, as well as progress and durable console output will be suppressed.
#' 
#' Not every function knows about silent, but most, like [umx::umxRAM()] etc do.
#' 
#' @details
#' Under the hood, `umx_set_silent` sets options("umx_silent"). This can be set to either `TRUE` or `FALSE`.
#' If TRUE, then the progress messages from model runs are suppressed. Useful for power simulations etc.
#'
#' @param value Boolean stating if umx Models should run silently (TRUE).
#' @param silent If TRUE, this function itself will just return the state of the option, with no user message.
#' @return - Current silent value
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>, <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' old = umx_set_silent() # print & store existing value
#' umx_set_silent(FALSE, silent = TRUE) # set to FALSE
#' umx_set_silent(old)   # reinstate
#' umx_set_silent() # print existing value
umx_set_silent <- function(value = NA, silent = FALSE) {
	# initialize if needed
	oldValue = getOption("umx_silent")
	if(is.null(oldValue)) {
		oldValue = FALSE
		options("umx_silent" = FALSE)
	}
	if(is.na(value)) {
		# get value
		if(!silent){
			message("Current silent setting is ", omxQuotes(oldValue),".\nValid options are TRUE or FALSE.")
		}
	} else {
		umx_check(value %in% c(TRUE, FALSE), "stop")
		options("umx_silent" = value)
	}
	invisible(oldValue)
}

#' Automatically run models?
#'
#' Set `autoRun` default for models like [umxRAM()], [umxACE()] etc.
#'
#' @param autoRun If TRUE or FALSE, sets the umx_auto_run option. Else returns the current value of umx_auto_run
#' @param silent If TRUE, no message will be printed.
#' @return - Current umx_auto_run setting
#' @export
#' @family Get and set
#' @examples
#' library(umx)
#' umx_set_auto_run() # print existing value
#' old = umx_set_auto_run(silent = TRUE) # store existing value
#' umx_set_auto_run(FALSE)  # set to FALSE
#' umx_set_auto_run(old)    # reinstate
umx_set_auto_run <- function(autoRun = NA, silent = FALSE) {
	oldValue = getOption("umx_auto_run")
	if(is.null(oldValue)) {
		oldValue = FALSE
		options("umx_auto_run" = TRUE)
	}
	
	if(is.na(autoRun)) {
		if(!silent){
			message("Current auto-run setting is ", omxQuotes(oldValue),".\nValid options are TRUE or FALSE.")
		}
		invisible(getOption("umx_auto_run"))
	} else {
		umx_check(autoRun %in% c(TRUE, FALSE), "stop")
		options("umx_auto_run" = autoRun)
	}
}

#' umx_set_condensed_slots
#'
#' Sets whether newly-created mxMatrices are to be condensed (set to NULL if not being used) or not.
#'
#' @param state what state (TRUE or FALSE) to set condensed slots (default NA returns current value).
#' @param silent If TRUE, no message will be printed.
#' @return - current value of condensed slots
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' umx_set_condensed_slots() # print
#' old = umx_set_condensed_slots(silent = TRUE) # store the existing state
#' umx_set_condensed_slots(TRUE) # update globally
#' umx_set_condensed_slots(old) # set back
umx_set_condensed_slots <- function(state = NA, silent = FALSE) {
	if(is.na(state)){
		if(!silent){
			message("mxCondenseMatrixSlots is currently: ",
				omxQuotes(getOption('mxCondenseMatrixSlots'))
			)
		}
		invisible(getOption('mxCondenseMatrixSlots'))
	} else {
		if(!is.logical(state)){
			stop("mxCondenseMatrixSlots must be TRUE or FALSE you tried ", omxQuotes(state))
		}else{
			options(mxCondenseMatrixSlots = state)			
		}
	}
}

#' Set options that affect optimization in OpenMx
#'
#' `umx_set_optimization_options` provides access to get and set options affecting optimization.
#' 
#' *note*: For `mvnRelEps`, values between .0001 to .01 are conventional. Smaller values slow optimization.
#'
#' @param opt default returns current values of the options listed. Currently
#' "mvnRelEps", "mvnMaxPointsA", and "Parallel diagnostics".
#' @param value If not NULL, the value to set the opt to (can be a list of length(opt))
#' @param silent If TRUE, no message will be printed.
#' @param model A model for which to set the optimizer. Default (NULL) sets the optimizer globally.
#' @return - current values if no value set.
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' # show current value for selected or all options
#' umx_set_optimization_options() # print the existing state(s)
#' umx_set_optimization_options("mvnRelEps") 
#' \dontrun{
#' umx_set_optimization_options("mvnRelEps", .01) # update globally
#' umx_set_optimization_options("Parallel diagnostics", value = "Yes") 
#' }
umx_set_optimization_options <- function(opt = c("mvnRelEps", "mvnMaxPointsA", "Parallel diagnostics"), value = NULL, model = NULL, silent = FALSE) {
	if(is.null(value)){
		# print current values for each item in opt
		for (this in opt) {			
			if(is.null(model)){
				o = mxOption(NULL, this)
			} else {
				o = mxOption(model, this)
			}
			message(paste0("Current ", this , " is: ", omxQuotes(o)))
		}
		invisible(o)
	} else {
		# Set options
		if(length(opt)!=length(value)){
			stop("For safe coding, please match opt and value lengths")
		} else {
			i = 1
			for (this in opt) {
				if(is.null(model)){
					o = mxOption(NULL, this, value[i])
				} else {
					o = mxOption(model, this, value[i])
				}
				i = i + 1
			}
		}
	}
}

#' Set the optimizer in OpenMx
#'
#' `umx_set_optimizer` provides an easy way to get and set the default optimizer.
#' `umx` can use several optimizers: `SLSQP` (the default), `CSOLNP`, or `NPSOL`.
#' `SLSQP` is often a good choice. `CSOLNP`often works well for ordinal models. 
#' `NPSOL` is proprietary. It is not in the CRAN version of OpenMx.
#' However it can outperform other optimizers, esp. on CIs.
#'
#' A bare call simply shows the current optimizer:
#'
#' ```r
#' umx_set_optimizer()
#' > Current Optimizer is: 'SLSQP'. Options are: 'CSOLNP', 'SLSQP', and 'NPSOL'
#' ```
#' 
#' Set the optimizer by name. note: the old optimizer is returned,
#' so you can stash it:
#' 
#' ```r
#' oldOpt = umx_set_optimizer("CSOLNP")
#' # ...
#' umx_set_optimizer(oldOpt)
#' ```
#' @param opt default (NA) returns current value. Current alternatives are
#' "NPSOL" "SLSQP" and "CSOLNP".
#' @param model A model for which to set the optimizer. Default (NULL) sets the optimizer globally.
#' @param silent If TRUE, no message will be printed.
#' @return - current optimizer if nothing requested to be set.
#' @export
#' @family Get and set
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' library(umx)
#' umx_set_optimizer() # print the existing state
#' old = umx_set_optimizer(silent = TRUE) # store the existing state
#' umx_set_optimizer("SLSQP") # update globally
#' umx_set_optimizer(old) # set back
umx_set_optimizer <- function(opt = NA, model = NULL, silent = FALSE) {
	if(is.na(opt)){
		if(is.null(model)){
			o = mxOption(NULL, "Default optimizer")
		} else {
			o = mxOption(model, "Default optimizer")
		}
		if(!silent){
			quoteOptions = omxQuotes(mxAvailableOptimizers())
			message("Current Optimizer is: ", omxQuotes(o), ". Options are: ", quoteOptions)
		}
		invisible(o)
	} else {
		if(!opt %in% mxAvailableOptimizers()){
			stop("The Optimizer ", omxQuotes(opt), " is not legal. Legal values (from mxAvailableOptimizers() ) are:",
			omxQuotes(mxAvailableOptimizers()))
		}
		if(is.null(model)){
			mxOption(NULL, "Default optimizer", opt)	
		} else {
			stop(paste0("'Default optimizer' is a global option and cannot be set on models. just say:\n",
			"umx_set_optimizer(", omxQuotes(opt), ")"))
		}
	}
}

#' Detect number of CPU cores (including Performance cores)
#'
#' Detects the number of cores on the local machine. On hybrid architectures
#' (like Apple Silicon or Intel Alder Lake+), it prioritizes detecting only
#' Performance (P) cores to avoid E-core bottlenecks in parallel loops.
#'
#' A detector, not a decider: every probe is guarded, so this always returns a
#' positive integer and never errors — worst case it falls back to
#' [parallel::detectCores()].
#'
#' Method: on macOS, tiers are selected by NAME (`hw.perflevelN.name`), not rank:
#' fast cores are tiers named "Super" or "Performance". Ranks shift yearly (L0 was
#' Performance through M4, Super from M5 Pro/Max & M6 (h/t Brian Ripley, CRAN)),
#' and M5/6 machines can have zero Efficiency cores, so rank-based rules
#' undercount there. On Linux there is no sysfs P-core flag, so P-cores are the
#' online cores whose max frequency is the highest on the box (true for Intel
#' hybrid and ARM big.LITTLE; degrades to all cores on homogeneous machines).
#'
#' @param logical Whether to count logical cores (hyperthreaded) or physical cores. Default is FALSE (physical).
#' @param p_cores Whether to prioritize detecting Performance (P) cores only on hybrid architectures. Default is TRUE.
#' @return Integer count of cores.
#' @export
#' @family Get and set
#' @examples
#' umx_detect_cores() # physical cores (P-cores if hybrid)
#' umx_detect_cores(logical = TRUE, p_cores = FALSE) # all logical cores
umx_detect_cores = function(logical = FALSE, p_cores = TRUE) {
	umx_check(logical %in%	c(TRUE, FALSE), "stop", "logical must be TRUE or FALSE")
	umx_check(p_cores %in%	c(TRUE, FALSE), "stop", "p_cores must be TRUE or FALSE")

	# Safe fallback: whatever R itself can see; never NA, never < 1, never an error
	fallback = tryCatch(parallel::detectCores(logical = logical), error = function(e) 1L)
	if (is.na(fallback) || fallback < 1) fallback = 1L

	# macOS probe as a closure: early return()s stay local, and the outer tryCatch
	# turns any surprise into NULL. The sysctl probes themselves can never throw:
	# launch failure, unknown keys and unparsable output all degrade to NA.
	macos_p_cores = function() {
		sysctl_int = function(key) {
			res = tryCatch(suppressWarnings(system(paste("sysctl -n", key), intern = TRUE, ignore.stderr = TRUE)), error = function(e) character(0))
			if (!length(res)) return(NA_integer_)
			val = suppressWarnings(as.integer(res[[1]]))
			if (is.na(val)) NA_integer_ else val
		}
		sysctl_str = function(key) {
			res = tryCatch(suppressWarnings(system(paste("sysctl -n", key), intern = TRUE, ignore.stderr = TRUE)), error = function(e) character(0))
			if (!length(res)) return(NA_character_)
			trimws(res[[1]])
		}
		kind = if (logical) "logicalcpu" else "physicalcpu"
		# Tier ranks shift yearly (L0 was Performance, now Super on M5/6),
		# and M5/6 machines can have zero Efficiency cores: select by NAME.
		nlevels = sysctl_int("hw.nperflevels")
		if (is.na(nlevels) || nlevels < 1) return(NULL) # no perflevels: Intel Mac or unknown box
		n = 0L
		for (i in seq_len(nlevels)) {
			nm = sysctl_str(paste0("hw.perflevel", i - 1L, ".name"))
			if (is.na(nm) || !(nm %in% c("Performance", "Super"))) next
			count = sysctl_int(paste0("hw.perflevel", i - 1L, ".", kind))
			if (!is.na(count) && count > 0) n = n + count
		}
		if (n < 1) NULL else n
	}

	# Linux probe as a closure. No sysfs flag marks P-cores, so use max frequency:
	# on Intel hybrid and ARM big.LITTLE the P-cores are the ones clocked highest.
	linux_p_cores = function() {
		cpu_dirs = list.dirs("/sys/devices/system/cpu", recursive = FALSE, full.names = TRUE)
		cpu_dirs = grep("/cpu[0-9]+$", cpu_dirs, value = TRUE)
		if (!length(cpu_dirs)) return(NULL)
		online = readLines("/sys/devices/system/cpu/online", warn = FALSE) # e.g. "0-7" or "0-3,8-11"
		online_ids = integer(0)
		for (part in strsplit(online, ",", fixed = TRUE)[[1]]) {
			bounds = suppressWarnings(as.integer(strsplit(part, "-", fixed = TRUE)[[1]]))
			online_ids = c(online_ids, if (length(bounds) == 2L) seq.int(bounds[1], bounds[2]) else bounds)
		}
		cpu_dirs = cpu_dirs[as.integer(sub(".*/cpu", "", cpu_dirs)) %in% online_ids]
		if (!length(cpu_dirs)) return(NULL)
		max_freq = vapply(cpu_dirs, function(d) {
			f = file.path(d, "cpufreq", "cpuinfo_max_freq")
			if (!file.exists(f)) return(NA_real_)
			suppressWarnings(as.numeric(readLines(f, warn = FALSE)[1]))
		}, numeric(1))
		if (all(is.na(max_freq))) return(NULL) # no frequency info (some VMs/containers)
		p_dirs = cpu_dirs[!is.na(max_freq) & max_freq == max(max_freq, na.rm = TRUE)]
		if (logical) return(length(p_dirs))
		ids = vapply(p_dirs, function(d) {
			core = file.path(d, "topology", "core_id")
			pkg  = file.path(d, "topology", "physical_package_id")
			if (!file.exists(core) || !file.exists(pkg)) return(NA_character_)
			paste(readLines(pkg, warn = FALSE)[1], readLines(core, warn = FALSE)[1], sep = ":")
		}, character(1))
		if (any(is.na(ids))) return(NULL)
		length(unique(ids))
	}

	if (p_cores) {
		sysname = Sys.info()[["sysname"]]
		if (sysname == "Darwin") {
			n = tryCatch(macos_p_cores(), error = function(e) NULL)
			if (!is.null(n)) return(n)
		}
		if (sysname == "Linux") {
			n = tryCatch(linux_p_cores(), error = function(e) NULL)
			if (!is.null(n)) return(n)
		}
	}
	return(fallback)
}

#' Set the number of cores your models will use.
#'
#' *note*: This function is intelligent: it will default to all performance cores
#' (low power cores are typically so slow its not worth forcing the power
#' cores to wait for them.
#'
#' @param cores number of cores to use. NA (the default) returns current value. "-1" will set to `umx_detect_cores()`.
#' @param model an (optional) model to set. If left NULL, the global option is updated.
#' @param silent If TRUE, no message will be printed.
#' @return - number of cores
#' @export
#' @family Get and set
#' @seealso - [umx_time()]
#' @examples
#' \dontrun{
#' library(umx)
#' manifests = c("mpg", "disp", "gear")
#' m1 = umxRAM("ind", data = mxData(mtcars[, manifests], type = "raw"),
#' 	umxPath(v.m. = manifests)
#' )
#' umx_set_cores() # print current value
#' # 5/5 (P-cores / total: 11)
#' oldCores = umx_set_cores(silent = TRUE)  # store existing value
#' umx_set_cores(omxDetectCores()) # set to max
#' umx_set_cores(-1); umx_set_cores() # set to max (performance cores)
#' m1 = umx_set_cores(1, m1)  # set m1 usage to 1 core
#' umx_set_cores(model = m1)  # show new value for m1
#' umx_set_cores(oldCores)    # reinstate old global value
#' umx_set_cores(3) # Request use of 3 cores
#' }
umx_set_cores <- function(cores = NA, model = NULL, silent = FALSE) {
	max_p_cores   = umx_detect_cores(p_cores = TRUE)
	max_all_cores = omxDetectCores()

	if(is.na(cores)){
		n = mxOption(model, "Number of Threads") # get the old value
		if(!silent){
			message(n, "/", max_p_cores, " (P-cores / total: ", max_all_cores, ")")
		}
		return(n)
	} else if(umx_is_MxModel(cores)) {
		stop("Call this as umx_set_cores(cores, model), not the other way around")
	}else{
		if(!is.numeric(cores)){
			stop("cores must be a number. You gave me ", cores)
		}
		umx_check(isTRUE(all.equal(cores, as.integer(cores))), message = paste0("cores must be an integer. You gave me: ", cores))
		if(cores > max_all_cores ){
			message("cores set to maximum available (request (", cores, ") exceeds number possible: ", max_all_cores)
			cores = max_all_cores
		} else if (cores < 1){
			cores = max_p_cores
		}
		mxOption(model, "Number of Threads", cores)		
	}
}

#' Set the checkpoint status for a model or global options
#'
#' @param interval How many units between checkpoints: Default =  1.
#' A value of zero sets always to 'No' (i.e., do not checkpoint all models during optimization)
#' @param units units to count in: Default unit is 'evaluations' ('minutes' is also legal)
#' @param prefix string prefix to add to all checkpoint filenames (default = "")
#' @param directory a directory, i.e "~/Desktop" (defaults to getwd())
#' @param model (optional) model to set options in (default = NULL)
#' @return - mxModel if provided
#' @export
#' @family Get and set
#' @examples
#' 
#' \dontrun{
#' umx_set_checkpoint(interval = 1, "evaluations", dir = "~/Desktop/")
#' # Turn off checkpointing with interval = 0
#' umx_set_checkpoint(interval = 0)
#' umx_set_checkpoint(2, "evaluations", prefix="SNP_1")
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' m1 = umx_set_checkpoint(model = m1)
#' m1 = mxRun(m1)
#' umx_checkpoint(0)
#' }
umx_set_checkpoint <- function(interval = 1, units = c("evaluations", "iterations", "minutes"), prefix = "", directory = getwd(), model = NULL) {
	if(umx_is_MxModel(interval)){
		stop("You passed in a model as the first parameter. You probably want:\n",
		"umx_set_checkpoint(model=yourModel)")
	}
	units = match.arg(units)
	if(interval == 0){
		always = "No"
	} else {
		always = "Yes"
	}
	if(is.null(model)){
		# Whether to checkpoint all models during optimization.
		mxOption(NULL, "Always Checkpoint"   , always)

		# The number of units between checkpoint intervals
		mxOption(NULL, "Checkpoint Count"    , interval)

		# The type of units for checkpointing: 'minutes', 'iterations', or 'evaluations'.
		mxOption(NULL, "Checkpoint Units"    , units)	

		# The string prefix to add to all checkpoint filenames.
		mxOption(NULL, "Checkpoint Prefix"   , prefix)

		# the directory into which checkpoint files are written.
		mxOption(NULL, "Checkpoint Directory", directory)
	} else {
		model = mxOption(model, "Always Checkpoint"   , always)
		model = mxOption(model, "Checkpoint Count"    , interval)
		model = mxOption(model, "Checkpoint Units"    , units)
		model = mxOption(model, "Checkpoint Prefix"   , prefix)
		model = mxOption(model, "Checkpoint Directory", directory)
		return(model)
	}
}

#' @rdname umx_set_checkpoint
#' @export
umx_checkpoint <- umx_set_checkpoint

#' Get or set checkpointing for a model
#'
#' Get the checkpoint status for a model or global options
#'
#' @param model an optional model to get options from
#' @return None
#' @export
#' @family Get and set
#' @examples
#' \dontrun{
#' umx_get_checkpoint() # current global default
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' umx_get_checkpoint(model = m1)
#' }
#' 
umx_get_checkpoint <- function(model = NULL) {
	message("Always Checkpoint: "    , mxOption(model, "Always Checkpoint") )
	message("Checkpoint  Count: "    , mxOption(model, "Checkpoint Count" ) )
	message("Checkpoint  Units: "    , mxOption(model, "Checkpoint Units" ) )
	message("Checkpoint  Prefix: "   , mxOption(model, "Checkpoint Prefix" ) )	
	message("Checkpoint  Directory: ", mxOption(model, "Checkpoint Directory" ) )
}
