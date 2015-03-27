# find non-ascii = grep this '[^\x00-\x7F]'
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::check("~/bin/umx");
# devtools::load_all()
# devtools::dev_help("umxX")
# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin"); 
# show_news()
# utility naming convention: "umx_" prefix, lowercase, and "_" not camel case for word boundaries
# so umx_swap_a_block()

# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools

# ======================================
# = how to check the thresholds matrix =
# ======================================
# umx_check_thresh_matrix()<- function(model or thresh) {
# 	x = data.frame(m1$top$threshMat$values)
# 	x[x == 0] = NA
# 	diffRows = dim(x)[1] -1
# 	y = umx_apply(umx_rot, of_DF = x, by = "columns")
# 	y[1:diffRows,] - x[1:diffRows,]
# 	# some threshold differences are zero
# 	any(y[1:diffRows,] - x[1:diffRows,] == 0, na.rm = T)
# 	# some threshold differences are inverted!
# 	any(y[1:diffRows,] - x[1:diffRows,] < 0, na.rm = T)
# }

# Use this or not?
umxFIML <- function(covariance, means, dimnames = NA, thresholds = NA, threshnames = dimnames, vector = FALSE) {
    # replaces mxFIMLObjective
	mxExp = mxExpectationNormal(covariance= covariance, means = means, dimnames = dimnames, thresholds)
	mxFit = mxFitFunctionML(vector = vector)
	return(list(mxExp, mxFit))
}

#' umx_check_multi_core
#'
#' Shows how many cores you are using, and runs a test script so user can check CPU usage
#'
#' @return - NULL
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' model = umx_check_multi_core(model)
#' }
umx_check_multi_core <- function() {
	oldCores = umx_get_cores()
	message("You are using ", oldCores, " of ", parallel::detectCores(), " available cores (0 means all)")
	message("I will now set cores to max (they will be reset after) and run a script that hits multiple cores if possible.\n",
	"Check CPU while it's running and see if R is pegging the processor.")
	umx_set_cores(parallel::detectCores())
	source("~/bin/OpenMx/models/nightly/3LatentMultiRegWithContinuousModerator-c.R")
	umx_set_cores(oldCores)
}

# ============================
# = OpenMx-related functions =
# ============================
#' umx_get_optimizer
#'
#' get the optimizer in OpenMx
#'
#' @param model (optional) model to get from. If left NULL, the global option is returned
#' @return - the optimizer  - a string
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' oldOpt = umx_get_optimizer()
#' m1 <- umxRAM("ind", data = mxData(mtcars[,manifests], type = "raw"),
#' 	umxPath(var = manifests),
#' 	umxPath(means = manifests)
#' )
#' umx_set_optimizer(opt = "CSOLNP")
#' m2 = mxRun(mxModel(m1, name = "CSOLNP")); umx_time(m2)
#' umx_set_optimizer(opt = "NPSOL")
#' m3 = mxRun(mxModel(m1, name = "NPSOL")); umx_time(m3)
#' # umx_set_optimizer(opt = "NLOPT")
#' # m4 = mxRun(mxModel(m1, name = "NLOPT")); umx_time(m4)
#' umx_set_optimizer(oldOpt)
umx_get_optimizer <- function(model = NULL) {
	if(is.null(model)){
		mxOption(NULL, "Default optimizer")
	} else {
		mxOption(model, "Default optimizer")
	}
}

#' umx_set_optimizer
#'
#' set the optimizer in OpenMx
#'
#' @param opt defaults to "NPSOL". Current alternatives are "NLOPT" and "CSOLNP"
#' @return - 
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' library(umx)
#' old = umx_get_optimizer() # get the existing state
#' umx_set_optimizer("NPSOL") # update globally
#' umx_set_optimizer(old) # set back
#' 
#' manifests = c("mpg", "disp", "gear")
#' oldOpt = umx_get_optimizer()
#' m1 <- umxRAM("ind", data = mxData(mtcars[,manifests], type = "raw"),
#' 	umxPath(var = manifests),
#' 	umxPath(means = manifests)
#' )
#' umx_set_optimizer(opt = "CSOLNP")
#' m2 = mxRun(mxModel(m1, name="CSOLNP")); umx_time(m2)
#' umx_set_optimizer(opt = "NPSOL")
#' m3 = mxRun(mxModel(m1, name="NPSOL")); umx_time(m3)
#' # umx_set_optimizer(opt = "NLOPT")
#' # m4 = mxRun(mxModel(m1, name="NLOPT")); umx_time(m4)
#' umx_set_optimizer(oldOpt)
#' \dontrun{
#' m1@@runstate$compute$steps[1][[1]]$engine # NPSOL
#' }
umx_set_optimizer <- function(opt = c("NPSOL", "NLOPT", "CSOLNP")) {
	opt = umx_default_option(opt, c("NPSOL", "NLOPT", "CSOLNP"), check = FALSE)
	# opt = match.arg(opt)
	oldOpt = umx_get_optimizer()
	# set the new choice
	mxOption(NULL, "Default optimizer", opt)
	
	# try it out
	# manifests = c("disp", "mpg")
	# m1 <- umxRAM("sat", data = mxData(mtcars[, manifests], type = "raw"),
	# 	umxPath(var   = manifests),
	# 	umxPath(cov   = manifests),
	# 	umxPath(means = manifests)
	# )
	#
	# tryCatch(
	# 	mxRun(m1),
	# 	error = function(err) {
	# 		umx_set_optimizer(oldOpt)
	#     	message(err)
	#     	message("remained with old optimizer", oldOpt)
	# 	}
	# )
	
	# if(opt == "NPSOL"){
	# 	# mxOption(model, 'mvnAbsEps', 1.e-6) # default is .001
	# 	# mxOption(model, 'mvnMaxPointsC', 5e+5) # default is 5000
	# }
}

#' umx_set_cores
#'
#' set the number of cores (threads) used by OpenMx
#'
#' @param cores number of cores to use (defaults to max - 1 to preserve UI responsiveness)
#' @param model an (optional) model to set. If left NULL, the global option is updated.
#' @return - NULL
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[, manifests], type = "raw")
#' )
#' oldCores <- umx_get_cores() # get global value
#' umx_set_cores() # set to default (max - 1)
#' umx_get_cores() # show new value
#' umx_set_cores(1, m1)  # set m1 useage to 1 core
#' umx_get_cores(model = m1)  # show new value
umx_set_cores <- function(cores = parallel::detectCores() - 1, model = NULL) {
	if(umx_is_MxModel(cores)){
		stop("Call this as umx_set_cores(cores, model), not the other way around")
	}
	mxOption(model, "Number of Threads", cores)
}

#' umx_get_cores
#'
#' Gets the number of cores (threads) used by OpenMx.
#'
#' @param model an (optional) model to get from. If left NULL, the global option is returned
#' @return - number of cores
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[, manifests], type = "raw")
#' )
#' oldCores = umx_get_cores()   # get current default value
#' umx_set_cores(model = m1)    # set to default (max - 1)
#' umx_get_cores(model = m1)    # show new value
#' umx_set_cores() # set to default (max - 1)
#' umx_get_cores()              # show new value
#' umx_set_cores(oldCores)      # reset to old value
umx_get_cores <- function(model = NULL) {
	n = mxOption(model, "Number of Threads")
	message(n, "/", parallel::detectCores())
	invisible(n)
	
}

#' umx_set_checkpoint
#'
#' Set the checkpoint status for a model or global options
#'
#' @aliases umx_set_checkpoint umx_checkpoint
#' @param interval How many units between checkpoints: Default =  1.
#' A value of zero sets always to 'No' (i.e., do not checkpoint all models during optimization)
#' @param units units to count in: Default unit is 'evaluations' ('minutes' is also legal)
#' @param prefix string prefix to add to all checkpoint filenames (default = "")
#' @param directory a directory, i.e "~/Desktop" (defaults to getwd())
#' @param model (optional) model to set options in (default = NULL)
#' @return - mxModel if provided
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_set_checkpoint(interval = 1, "evaluations", dir = "~/Desktop/")
#' # turn off checkpointing with interval = 0
#' umx_set_checkpoint(interval = 0)
#' umx_set_checkpoint(2, "evaluations", prefix="SNP_1")
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' m1 = umx_set_checkpoint(model = m1)
#' m1 = mxRun(m1)
#' umx_checkpoint(0)
umx_set_checkpoint <- function(interval = 1, units = c("evaluations", "iterations", "minutes"), prefix = "", directory = getwd(), model = NULL) {
	if(umx_is_MxModel(interval)){
		stop("You passed in a model as the first parameter. You probably want:\n",
		"umx_is_MxModel(model=yourModel)")
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

#' @export
umx_checkpoint <- umx_set_checkpoint

#' umx_get_checkpoint
#'
#' get the checkpoint status for a model or global options
#'
#' @param model an optional model to get options from
#' @return - NULL
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_get_checkpoint() # current global default
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor", mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1.0)
#' )
#' m1 = umx_set_checkpoint(interval = 2, model = m1)
#' umx_get_checkpoint(model = m1)
umx_get_checkpoint <- function(model = NULL) {
	message("Always Checkpoint: "    , mxOption(model, "Always Checkpoint") )
	message("Checkpoint  Count: "    , mxOption(model, "Checkpoint Count" ) )
	message("Checkpoint  Units: "    , mxOption(model, "Checkpoint Units" ) )
	message("Checkpoint  Prefix: "   , mxOption(model, "Checkpoint Prefix" ) )	
	message("Checkpoint  Directory: ", mxOption(model, "Checkpoint Directory" ) )
}

#' umx_update_OpenMx
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  A Boolean determining whether to request the beta (TRUE) or relase version (defaults to FALSE)
#' @param loadNew A Boolean parameter determining whether to load the library after (optionally) updating
#' @param anyOK The minimum version to accept without updating
#' @family Miscellaneous Functions
#' @export
#' @examples
#' \dontrun{
#' umx_update_OpenMx()
#' }

umx_update_OpenMx <- function(bleedingEdge = FALSE, loadNew = TRUE, anyOK = FALSE) {
	if( "OpenMx" %in% .packages() ){
		oldV = mxVersion();
		if(anyOK){
			message("You have version", oldV, "and that's fine")
			return()
		}
		detach(package:OpenMx); # unload existing version
		message("existing version \"" ,oldV, "\" was detached")
	}	
	if (bleedingEdge){
		install.packages('OpenMx', repos = 'http://www.github.com/tbates/umx/testing/');
	} else {
		if (.Platform$OS.type == "windows") {
			if (!is.null(.Platform$r_arch) && .Platform$r_arch == "x64") {
				stop(paste("OpenMx is not yet supported on 64-bit R for Windows.",
				"Please use 32-bit R in the interim."), call. = FALSE)
			}
			repos <- c('http://www.github.com/tbates/umx/packages/')
			install.packages(pkgs=c('OpenMx'), repos=repos)
		} else {
			if (Sys.info()["sysname"] == "Darwin") {
				darwinVers <- as.numeric(substr(Sys.info()['release'], 1, 2))
				if (darwinVers > 10) {
					msg <- paste("We have detected that you are running on OS X 10.7 or greater",
					"whose native version of gcc does not support the OpenMP API.", 
					"As a result your default installation has been set to single-threaded.",
					"If you have installed the mac ports version of gcc to address this issue",
					"please choose the multi-threaded installation option.")
					cat(msg)
					cat("1. single-threaded [default]\n")
					cat("2. multi-threaded \n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 1
					} 

				} else {
					cat("1. single-threaded\n")
					cat("2. multi-threaded [default]\n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 2
					}
				}
				} else {
					cat("1. single-threaded\n")
					cat("2. multi-threaded [default]\n")
					select <- readline("Which version of OpenMx should I install? ")

					if (select == "") {
						select <- 2
					}
				}

				if (!(select %in% c(1,2))) {
					stop("Please enter '1' or '2'", call. = FALSE)
				}
  
				if (select == 1) {
					repos <- c('http://www.github.com/tbates/umx/sequential/')
					install.packages(pkgs=c('OpenMx'), repos=repos, 
					configure.args=c('--disable-openmp'))
				} else if (select == 2) {
					repos <- c('http://www.github.com/tbates/umx/packages/')
					install.packages(pkgs=c('OpenMx'), repos=repos)
				} else {
					stop(paste("Unknown installation type", select))
				}
		}
	}
	if(loadNew){
		# detach(package:OpenMx); # unload existing version
		require("OpenMx")
		newV = mxVersion();
		if(!is.na(oldV)){
			message("Woot: installed the latest and greatest \"", newV, "\" of OpenMx!")
		} else {
			message("Woot: you have upgraded from version \"" ,oldV, "\" to the latest and greatest \"", newV, "\"!")
		}
	}
}

# How long did that take?
#' umx_time
#'
#' A function to compactly report how long a model took to execute. User can set the format, with some
#' preset styles, or c-style string formatting.
#'
#' The default is "simple", which gives only the biggest unit used. i.e., "x seconds" for times under 1 minute.
#' "std" shows time in the format adopted in OpenMx 2.0 e.g. "Wall clock time (HH:MM:SS.hh): 00:00:01.16"
#' If a list of models is provided, time deltas will also be reported.
#'
#' @param model An \code{\link{mxModel}} from which to get the elapsed time
#' @param formatStr A format string, defining how to show the time (defaults to human readable)
#' @param tz time zone in which the model was executed (defaults to "GMT")
#' @export
#' @seealso - \code{\link{summary}}, \code{\link{umxRun}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @family Reporting Functions
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umx_time(m1)
#' m2 = umxRun(m1)
#' umx_time(c(m1, m2))
umx_time <- function(model, formatStr = c("simple", "std", "custom %H %M %OS3"), tz = "GMT"){
	# TODO output a nicely formated table
	formatStr = umx_default_option(formatStr, c("simple", "std", "custom %H %M %OS3"), check = FALSE)
	for(i in 1:length(model)) {			
		if(length(model) > 1) {
			m = model[[i]]
		}else{
			m = model
		}
		if(!umx_has_been_run(m)){
			message("You must run the model before asking for the elapsed run time")
		}else{
			thisTime = m$output$wallTime
			if(i == 1){
				lastTime = thisTime
				timeDelta = ""
			} else {
				timeDelta = paste0("(\u2206: ", round(thisTime - lastTime, 3), ")")
			}
			if(formatStr == "std"){
				formatStr = "Wall clock time (HH:MM:SS.hh): %H:%M:%OS2"
			} else if(formatStr == "simple"){
				if(thisTime > (3600 * 2)-1){ # hours
					formatStr = "%H hours, %M minute(s), %OS2 seconds"
				} else if(thisTime > 3600){ # hours
					formatStr = "%H hour, %M minute(s), %OS2 seconds"
				} else if(thisTime > 60){ # minutes
					if(thisTime > 119){ # minutes
						formatStr = "%M minutes,  %OS2 seconds"
					}else{
						formatStr = "%M minute,  %OS2 seconds"	
					}					
				} else { # seconds
					formatStr = "%OS2 seconds"
				}
			}
			message(format(.POSIXct(thisTime, tz), paste0(m$name, ": ", formatStr, timeDelta)))
		}
	}
}

#' umx_print
#'
#' A helper to aid the interpretability of printed tables from OpenMx (and elsewhere).
#' Its most useful characteristic is allowing you to change how NA and zero appear.
#' By default, Zeros have the decimals suppressed, and NAs are suppressed altogether.
#'

#' @param x A data.frame to print
#' @param digits  The number of decimal places to print (defaults to getOption("digits")
#' @param quote  Parameter passed to print (defaults to FALSE)
#' @param na.print String to replace NA with (default to blank "")
#' @param zero.print String to replace 0.000 with  (defaults to "0")
#' @param justify Parameter passed to print (defaults to "none")
#' @param file whether to write to a file (defaults to NA (no file). Use "tmp.html" to open as tables in browser.
#' @param suppress minimum numeric value to print (default =  NULL, print all values, no matter how small)
#' @param ... Optional parameters for print
#' @export
#' @family Miscellaneous Functions
#' @family Reporting Functions
#' @seealso - \code{\link{print}}
#' @examples
#' umx_print(mtcars[1:10,], digits = 2, zero.print = ".", justify = "left")
#' \dontrun{
#' umx_print(model)
#' umx_print(mtcars[1:10,], file = "Rout.html")
#' }

umx_print <- function (x, digits = getOption("digits"), quote = FALSE, na.print = "", zero.print = "0", justify = "none", file = c(NA,"tmp.html"), suppress = NULL, ...){
	# TODO: Options for file = c("Rout.html","cat","return")
	file = umx_default_option(file, c(NA,"tmp.html"), check = FALSE)
	if(!is.null(suppress)){
		x[abs(x) < suppress] = 0
		zero.print = "."
	}
	x <- umx_round(x, digits = digits, coerce = FALSE)
    if (any(ina <- is.na(x))) 
        x[ina] <- na.print
	i0 <- !ina & x == 0
    if (zero.print != "0" && any(i0)) 
        x[i0] <- zero.print
    if (is.numeric(x) || is.complex(x)){
        print(x, quote = quote, right = TRUE, ...)
	} else if(!is.na(file)){
		R2HTML::HTML(x, file = file, Border = 0, append = FALSE, sortableDF= TRUE); 
		system(paste0("open ", file))
		print("Table opened in browser")
    }else{
		print(x, quote = quote, ...)	
    }
    invisible(x)
}

# ===============
# = RAM helpers =
# ===============
#' umx_is_exogenous
#'
#' Return a list of all the exogenous variables (variables with no incoming single-arrow path) in a model. 
#'
#' @param model an \code{\link{mxModel}} from which to get exogenous variables
#' @param manifests_only Whether to check only manifests (default = TRUE)
#' @return - list of exogenous variables
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	mxPath(from = "g", to = names(demoOneFactor))
#' )
#' umx_is_exogenous(m1, manifests_only = TRUE)
#' umx_is_exogenous(m1, manifests_only = FALSE)
umx_is_exogenous <- function(model, manifests_only = TRUE) {
	umx_check_model(model, type = "RAM")
	checkThese = model@manifestVars
	if(!manifests_only){
		checkThese = c(checkThese, model@latentVars)
	}
	if(length(checkThese) < 1){
		return(c())
	}
	exog = c()
	n = 1
	for (i in checkThese) {
		if(!any(model@matrices$A@free[i, ])){
			exog[n] = i
			n = n + 1
		}
	}
	return(exog)
}

#' umx_is_endogenous
#'
#' Return a list of all the endogenous variables (variables with at least one incoming single-arrow path) in a model.
#'
#' @param model an \code{\link{mxModel}} from which to get endogenous variables
#' @param manifests_only Whether to check only manifests (default = TRUE)
#' @return - list of endogenous variables
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m1 <- umxRAM("One Factor", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	mxPath(from = "g", to = names(demoOneFactor))
#' )
#' umx_is_endogenous(m1, manifests_only = TRUE)
#' umx_is_endogenous(m1, manifests_only = FALSE)
umx_is_endogenous <- function(model, manifests_only = TRUE) {
	# has_no_incoming_single_arrow
	umx_check_model(model, type = "RAM")
	checkThese = model@manifestVars
	if(!manifests_only){
		checkThese = c(checkThese, model@latentVars)
	}
	if(length(checkThese) < 1){
		return(c())
	}
	endog = c()
	n = 1
	for (i in checkThese) {
		# any free paths in this row?
		if(any(model$matrices$A$free[i, ])){
			endog[n] = i
			n = n + 1
		}
	}
	return(endog)
}

#' umx_add_variances
#'
#' Convenience function to save the user specifying mxPaths adding variance to each variable
#'
#' @param model an \code{\link{mxModel}} to add variances to
#' @param add.to = List of variables to create variance for
#' @param free = List of variables to create variance for (default = NULL)
#' @param values = List of values (default = NULL)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM",
#'  manifestVars = names(demoOneFactor),
#'  latentVars = "g",
#' 	mxPath(from = "g", to = names(demoOneFactor), values= .1),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_show(m1, matrices = "S") # variables lack variance :-(
#' m1 = umx_add_variances(m1, add.to = names(demoOneFactor))
#' m1 = umx_add_variances(m1, add.to = "g", FALSE, 1)
#' umx_show(m1, matrices = "S") 
#' # Note: latent g has been treated like the manifests...
#' # umxFixLatents() will take care of this for you...
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxSummary(m1)
umx_add_variances <- function(model, add.to, values = NULL, free = NULL) {
	umx_check_model(model, type = "RAM")
	theList = c(model@latentVars, model@manifestVars)
	if(!all(add.to %in% theList)){
		stop("not all names found in model")
	}
	for (i in add.to) {
		model@matrices$S@free[i, i] = TRUE
		model@matrices$S@values[i, i] = .1
	}
	return(model)
}

#' umx_fix_latents
#'
#' Fix the variance of all, or selected, exogenous latents at selected values. This function adds a variance to the factor if it does not exist.
#'
#' @param model an \code{\link{mxModel}} to set
#' @param latents (If NULL then all latentVars)
#' @param exogenous.only only touch exogenous latents (default = TRUE)
#' @param at (Default = 1)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Model Building Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM",
#'  manifestVars = names(demoOneFactor),
#'  latentVars = "g",
#' 	mxPath(from = "g", to = names(demoOneFactor)),
#' 	mxPath(from = names(demoOneFactor), arrows = 2),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_show(m1, matrices = "S") # variance of g is not set
#' m1 = umx_fix_latents(m1)
#' umx_show(m1, matrices = "S") # variance of g is fixed at 1
umx_fix_latents <- function(model, latents = NULL, exogenous.only = TRUE, at = 1) {
	if(is.null(latents)){
		latenVarList = model@latentVars
	} else {
		latenVarList = latents
	}
	exogenous_list = umx_is_exogenous(model, manifests_only = FALSE)
	for (i in latenVarList) {
		if(!exogenous.only | i %in% exogenous_list){
			model@matrices$S@free[i, i]   = FALSE
			model@matrices$S@values[i, i] = at
		}
	}
	return(model)
}

#' umx_fix_first_loadings
#'
#' Fix the loading of the first path from each latent at selected value (default = 1).
#'
#' @param model an \code{\link{mxModel}} to set
#' @param latents (If NULL then all latentVars in model)
#' @param at (Default = 1)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Model Building Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM",
#'  manifestVars = names(demoOneFactor),
#'  latentVars = "g",
#' 	mxPath(from = "g", to = names(demoOneFactor)),
#' 	mxPath(from = names(demoOneFactor), arrows = 2),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umx_fix_first_loadings(m1)
#' umx_show(m1) # variance of g is fixed at 1
umx_fix_first_loadings <- function(model, latents = NULL, at = 1) {
	# TODO: SHould this apply when the first loading is a latents?
	# TODO: Must not apply this twice
	umx_check_model(model, type = "RAM")
	if(is.null(latents)){
		latenVarList = model@latentVars
	} else {
		latenVarList = latents
	}
	for (i in latenVarList) {
		# i = "ind60"
		firstFreeRow = which(model@matrices$A@free[,i])[1]
		# check that there is not already a factor fixed prior to this one
		if(firstFreeRow == 1){
			# must be ok
			model@matrices$A@free[firstFreeRow, i]   = FALSE
			model@matrices$A@values[firstFreeRow, i] = at
		} else {
			if(any(model@matrices$A@values[1:(firstFreeRow-1), i] == at)){
				message("I skipped factor '", i, "'. It looks like it already has a loading fixed at ", at)
			} else {
				model@matrices$A@free[firstFreeRow, i]   = FALSE
				model@matrices$A@values[firstFreeRow, i] = at				
			}
		}
	}
	return(model)
}

umxFormativeVarianceMess <- function(model){
	a = mxPath(from = correlatedManifests, arrows = 2, free = TRUE, values = 1)
	# And allow them to covary (formative)
	b = mxPath(from = correlatedManifests, connect = "unique.bivariate", arrows = 2)
	return(list(a,b))
}


#' umx_apply
#'
#' Tries to make apply more readable. Other functions to think of include
#' \code{\link{cumsum}}, \code{\link{rowSums}}, \code{\link{colMeans}}, etc.
#'
#' @param FUN the function to apply
#' @param of_DF the dataframe to work with
#' @param by What to apply the function to: columns or rows (default = "columns")
#' @param ... optional arguments to FUN, i.e., na.rm = T
#' @return - \code{\link{mxModel}}
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_apply(mean, mtcars, by = "columns")
#' umx_apply(mean, of_DF = mtcars, by = "columns")
#' umx_apply(mean, by = "rows", of_DF = mtcars, na.rm = TRUE)
umx_apply <- function(FUN, of_DF, by = "columns", ...) {
	if(! (by %in% c("columns", "rows"))){
		stop(paste("by must be either 'columns' or 'rows', you gave me", by))
	} else if (by == "rows") {
		by = 1
	} else {
		by = 2		
	}
	apply(of_DF, by, FUN, ...)

	# TODO fix all this...
	# lapply if list
	# tapply if vector?
	# out = c()
	# if(fromEach == "column"){
	# 	for(n in 1:ncol(of_DF)){
	# 		out[n] = nlevels(of_DataFrame[,n])
	# 	}
	# } else {
	# 	for(n in 1:nrow(of_DF)){
	# 		out[n] = nlevels(of_DataFrame[n,])
	# 	}
	# }
	# return(out)
}

# ====================
# = Parallel Helpers =
# ====================

eddie_AddCIbyNumber <- function(model, labelRegex = "") {
	# eddie_AddCIbyNumber(model, labelRegex="[ace][1-9]")
	args     = commandArgs(trailingOnly=TRUE)
	CInumber = as.numeric(args[1]); # get the 1st argument from the cmdline arguments (this is called from a script)
	CIlist   = umxGetParameters(model ,regex= "[ace][0-9]", verbose= FALSE)
	thisCI   = CIlist[CInumber]
	model    = mxModel(model, mxCI(thisCI) )
	return (model)
}

# ===================================
# = Ordinal/Threshold Model Helpers =
# ===================================

#' umx_RAM_ordinal_objective
#'
#' umx_RAM_ordinal_objective builds an appropriate thresholds matrix
#' It also sets latent means and variances to 0 and 1 respectively.
#' 
#' TODO: more detail about what we are doing here
#'
#' @param df Dataframe to make a threshold matrix for
#' @param deviationBased whether to use the deviation system to ensure order thresholds (default = TRUE)
#' @param droplevels whether to also drop unused levels (default = TRUE)
#' @param verbose whether to say what the function is doing (default = FALSE)
#' @return - \code{\link{mxModel}}
#' @export
#' @family Advanced Helpers
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model = umx_RAM_ordinal_objective(model)
#' }
umx_RAM_ordinal_objective <- function(df, deviationBased = TRUE, droplevels = TRUE, verbose = FALSE) {
	# (This is a nice place to check, as we have the df present...)
	if(!any(umx_is_ordered(df))){
		stop("No ordinal variables in dataframe: no need to call umx_RAM_ordinal_objective")
	} 
	pt1 = umxPath(means = umx_is_ordered(df, names = TRUE), fixedAt = 0)
	pt2 = umxPath(var   = umx_is_ordered(df, names = TRUE), fixedAt = 1)
	return(list(pt1, pt2, umxThresholdMatrix(df, deviationBased = TRUE, droplevels = TRUE, verbose = FALSE)))
}


# ===========
# = Utility =
# ===========

#' umx_find_object
#'
#' Find objects a certain class, whose name matches a search string.
#' The string (pattern) is grep-enabled, so you can match wild-cards
#'
#' @param pattern the pattern that matching objects must contain
#' @param requiredClass the class of object that will be matched
#' @return - a list of objects matching the class and name
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - 
#' @family Miscellaneous Utility Functions
#' @examples
#' \dontrun{
#' umx_find_object("^m[0-9]") # mxModels beginning "m1" etc.
#  umx_find_object("", "MxModel") # all MxModels
#' }


umx_find_object <- function(pattern = ".*", requiredClass = "MxModel") {
	# Use case: umxFindObject("Chol*", "MxModel")
	matchingNames = ls(envir = sys.frame(-1), pattern = pattern) # envir
	matchingObjects = c()
	for (obj in matchingNames) {
		if(class(get(obj))[1] == requiredClass){
			matchingObjects = c(matchingObjects, obj)
		}
	}
	return(matchingObjects)
}

#' umx_rename_file
#'
#' rename files. On OS X, the function can access the current frontmost Finder window.
#' The file renaming is fast and, because you can use regular expressions, powerful
#'
#' @param findStr The (regex) string to find, i.e., "c[ao]t"
#' @param replaceStr The (regex) replacement string "\1 are not dogs"
#' @param baseFolder  The folder to search in. If set to "Finder" (and you are on OS X) it will use the current frontmost Finder window. If it is blank, a choose folder dialog will be thrown.
#' @param listPattern A pre-filter for files
#' @param test Boolean determining whether to change files on disk, or just report on what would have happened (Defaults to test = TRUE)
#' @param overwrite Boolean determining if an existing file will be overwritten (Defaults to the safe FALSE)
#' @family Miscellaneous Utility Functions
#' @return - 
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umx_rename_file(baseFolder = "~/Downloads/", findStr = "", replaceStr = "", test = TRUE)
#' umx_rename_file("[Ss]eason +([0-9]+)", replaceStr="S\1", baseFolder = "Finder", test = TRUE)
#' }
umx_rename_file <- function(findStr = NA, replaceStr = NA, baseFolder = "Finder", listPattern = NA, test = TRUE, overwrite = FALSE) {
	# TODO: add recursive support to rename
	# cd "/Users/tim/Desktop/"
	# find "The Strain" -name "*.mp4"  -exec mv {} "The Strain" \;
	if(is.na(replaceStr)){
		stop("Please set a replacement string")
	}

	# uppercase = u$1
	if(baseFolder == "Finder"){
		baseFolder = system(intern = TRUE, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
		message("Using front-most Finder window:", baseFolder)
	} else if(baseFolder == "") {
		baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep = "") ## choose a directory
		message("Using selected folder:", baseFolder)
	}
	if(is.na(listPattern)){
		listPattern = findStr
	}
	a = list.files(baseFolder, pattern = listPattern)
	message("found ", length(a), " possible files")
	changed = 0
	for (fn in a) {
		findB = grepl(pattern = findStr, fn) # returns 1 if found
		if(findB){
			fnew = gsub(findStr, replacement = replaceStr, fn) # replace all instances
			if(test){
				message("would change ", fn, " to ", fnew)
			} else {
				if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
					message("renaming ", fn, "to", fnew, "failed as already exists. To overwrite set T")
				} else {
					file.rename(paste0(baseFolder, fn), paste0(baseFolder, fnew))
					changed = changed + 1;
				}
			}
		}else{
			if(test){
				# message(paste("bad file",fn))
			}
		}
	}
	if(test & changed==0){
		message("add test = FALSE to actually change files.")
	} else {
		umx_msg(changed)
	}
}

# ===========================
# = File handling functions =
# ===========================


#' dl_from_dropbox
#'
#' Download a file from Dropbox, given either the url, or the name and key
#'
#' Improvements would include error handling...
#' @param x Either the file name, or full dropbox URL (see example below)
#' @param key the code after s/ and before the file name in the dropbox url
#' @return - NULL
#' @export
#' @family Miscellaneous Utility Functions
#' @references - \url{http://thebiobucket.blogspot.kr/2013/04/download-files-from-dropbox.html}
#' @examples
#' \dontrun{
#' dl_from_dropbox("https://dl.dropboxusercontent.com/s/7kauod48r9cfhwc/tinytwinData.rda")
#' dl_from_dropbox("tinytwinData.rda", key = "7kauod48r9cfhwc")
#' }
dl_from_dropbox <- function(x, key){
	require(RCurl)
	if(is.null(key)){
		bin <- RCurl::getBinaryURL(x, ssl.verifypeer = FALSE)
		x = sub("^.+/(.*)$", "\\1", x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE)
	} else {
		# user has provided key and file name, so concatenate with https...
		bin <- RCurl::getBinaryURL(paste0("https://dl.dropboxusercontent.com/s/", key, "/", x), ssl.verifypeer = FALSE)
	}
	con <- file(x, open = "wb")
	writeBin(bin, con)
	close(con)
	message(noquote(paste(x, "read into", getwd())))                        
}


#' umx_pb_note
#'
#' Use the pushbullet service to push a note. You can also initialise this
#' service by providing your autho_key one time
#'
#' If you supply auth_key, It will be writen to "~/.pushbulletkey"
#' once it exists there, you dont need to store it in code, so code is sharable.
#' 
#' You can get your autho code at \url{https://www.pushbullet.com/account}
#'
#' @param title of the note
#' @param body of the note
#' @param auth_key optional authkey
#' @export
#' @family Miscellaneous Utility Functions
#' @seealso - \code{\link{umx_msg}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' \dontrun{
#' umx_pb_note("done!", umx_time(m1))
#' }
umx_pb_note <- function(title="test", body="default body", auth_key=NULL) {
	auth_key_file = "~/.pushbulletkey"
	if(is.null(auth_key)){
		auth_key = read.table(auth_key_file, stringsAsFactors=FALSE)[1,1]
	} else {
		fileConn <- file(auth_key_file)
		writeLines(auth_key, fileConn)
		close(fileConn)
	}
	# umx_check(exists(auth_key_file)
	cmd = paste0("curl -s --header 'Authorization: Bearer ", auth_key, "'", 
	" -X POST https://api.pushbullet.com/v2/pushes ",
	"--header 'Content-Type: application/json' ",
    "--data-binary '{\"type\": \"note\", \"title\": \"",title, "\", \"body\": \"", body, "\"}'"
	)
	invisible(system(cmd, intern=TRUE))
}

#' umx_move_file
#'
#' move files. On OS X, the function can access the current frontmost Finder window.
#' The file moves are fast and, because you can use regular expressions, powerful
#'
#' @param baseFolder  The folder to search in. If set to "Finder" (and you are on OS X) it will use the current frontmost Finder window. If it is blank, a choose folder dialog will be thrown.
#' @param findStr = regex string select files to move (WARNING: NOT IMPLEMENTED YET)
#' @param fileNameList List of files to move
#' @param destFolder Folder to move files into
#' @param test Boolean determining whether to change the names, or just report on what would have happened
#' @param overwrite Boolean determining whether to overwrite files or not (default = FALSE (safe))
#' @return - 
#' @family Miscellaneous Utility Functions
#' @export
#' @seealso - \code{\link{umx_rename_file}}, \code{\link{file.rename}}
#' @examples
#' \dontrun{
#' base = "/Users/tim/Music/iTunes/iTunes Music/"
#' dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
#' umx_move_file(baseFolder = base, fileNameList = toMove, destFolder = dest, test= FALSE)
#' }
umx_move_file <- function(baseFolder = NA, findStr = NULL, fileNameList = NA, destFolder = NA, test = TRUE, overwrite = FALSE) {
	# TODO implement findStr
	if(!is.null(findStr)){
		stop("Have not implemented findStr yet")
	}

	if(is.na(destFolder)){
		stop("destFolder can't be NA")
	}
	if(baseFolder == "Finder"){
		baseFolder = system(intern = TRUE, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
		message("Using front-most Finder window:", baseFolder)
	} else if(baseFolder == "") {
		baseFolder = paste(dirname(file.choose(new = FALSE)), "/", sep="") ## choose a directory
		message("Using selected folder:", baseFolder)
	}
	moved = 0
	for (fn in fileNameList) {
		if(test){
			message("would move ", fn, " to ", destFolder)	
			moved = moved + 1;
		} else {
			if((!overwrite) & file.exists(paste0(destFolder, fn))){
				message("moving ", fn, "to", destFolder, "failed as already exists. To overwrite set T")
			} else {
				file.rename(paste0(baseFolder, fn), paste0(destFolder, fn))
				moved = moved + 1;
			}
		}
	}
	message("moved (or would have moved)", moved)
}

#' umx_cor
#'
#' Report correlations and their p-values
#'
#' @param X a matrix or dataframe
#' @param df the degrees of freedom for the test
#' @param use how to handle missing data
#' @param digits rounding of answers
#' @return - matrix of correlations and p-values
#' @family Miscellaneous Utility Functions
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_cor(myFADataRaw[1:8,])
umx_cor <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {
	# see also
	# hmisc::rcorr( )
	warning("don't use this until we are computing n properly")
	# nvar    = dim(x)[2]
	# nMatrix = diag(NA, nrow= nvar)
	# for (i in 1:nvar) {
	# 	x[,i]
	# }
	R <- cor(X, use = use)
	above <- upper.tri(R)
	below <- lower.tri(R)
	r2 <- R[above]^2
	Fstat <- r2 * df/(1 - r2)
	R[row(R) == col(R)] <- NA # NA on the diagonal
	R[above] <- pf(Fstat, 1, df, lower.tail = FALSE)
	R[below] = round(R[below], digits)
	R[above] = round(R[above], digits)
	# R[above] = paste("p=",round(R[above], digits))
	message("lower tri  = correlation; upper tri = p-value")
	return(R)
}

# Return the maximum value in a row
rowMax <- function(df, na.rm = TRUE) {
	tmp = apply(df, MARGIN = 1, FUN = max, na.rm = na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

rowMin <- function(df, na.rm= TRUE) {
	tmp = apply(df, MARGIN = 1, FUN = min, na.rm = na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

#' umx_as_numeric
#' 
#' Convert each column of a dataframe to numeric
#'
#' @param df a \code{\link{data.frame}} to convert
#' @param force whether to force conversion to numeric for non-numeric columns (defaults to FALSE)
#' @return - data.frame
#' @family Miscellaneous Utility Functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' df = mtcars
#' # make one variable non-numeric
#' df$mpg = c(letters,letters[1:6]); str(df)
#' df = umx_as_numeric(df)
umx_as_numeric <- function(df, force = FALSE) {
	# TODO handle case of not being a data.frame...
	colsToConvert = names(df)
	if(!force){
		# just the numeric names
		colsToConvert = colsToConvert[umx_is_numeric(df)]
	}
	for (i in colsToConvert) {
		df[ ,i] = as.numeric(df[ ,i])
	}
	return(df)
}

#' umx_swap_a_block
#'
#' Swap a block of rows of a datset between two lists variables (typically twin 1 and twin2)
#'
#' @param theData a data frame to swap within
#' @param rowSelector rows to swap amongst columns
#' @param T1Names the first set of columns
#' @param T2Names the second set of columns
#' @return - dataframe
#' @family Miscellaneous Utility Functions
#' @export
#' @seealso - \code{\link{subset}}
#' @examples
#' test = data.frame(
#' a = paste0("a", 1:10),
#' b = paste0("b", 1:10),
#' c = paste0("c", 1:10),
#' d = paste0("d", 1:10), stringsAsFactors = FALSE)
#' umx_swap_a_block(test, rowSelector = c(1,2,3,6), T1Names = "b", T2Names = "c")
#' umx_swap_a_block(test, rowSelector = c(1,2,3,6), T1Names = c("a","c"), T2Names = c("b","d"))
#'
umx_swap_a_block <- function(theData, rowSelector, T1Names, T2Names) {
	theRows = theData[rowSelector,]
	old_BlockTwo = theRows[,T2Names]
	theRows[,T1Names] -> theRows[, T2Names]
	theRows[,T1Names] <- old_BlockTwo
	theData[rowSelector,] <- theRows
	return(theData)
}

#' umx_rename
#'
#' Returns a dataframe with variables renamed as desired.
#' Unlike some functions, it checks that the variables exist, and that the new names are not already used.
#'
#' note: to use replace list, you must say c(old = "new"), not c(old -> "new")
#' @param x the dataframe in which to rename variables
#' @param replace If used alone, a named collection of c(oldName = "newName") pairs
#' OR, if "old" is a list of existing names, the list of new names)
#' OR, if "grep" is a regular expression, the replace string)
#' @param old Optional list of old names that will be found and replaced by the contents of replace. Defaults to NULL
#' @param grep Optional grep string. Matches will be replaced using replace as the replace string. Defaults to NULL
#' @param test whether to report a "dry run" - and not actually change anything (defaults to false)
#' @return - dataframe with columns renamed.
#' @export
#' @family Miscellaneous Functions
#' @examples
#' # Re-name "cyl" to "cylinder"
#' x = mtcars
#' x = umx_rename(x, replace = c(cyl = "cylinder"))
#' # alternate style
#' x = umx_rename(x, old = c("disp"), replace = c("displacement"))
#' umx_check_names("displacement", data = x, die = TRUE)
#' # This will warn that "disp" doesn't exist (anymore)
#' x = umx_rename(x, old = c("disp"), replace = c("displacement"))
#' x = umx_rename(x, grep = "lacement", replace = "") # use grep to set it back
#' umx_names(x, "^d")
umx_rename <- function(x, replace = NULL, old = NULL, grep = NULL, test = FALSE) {
	# See also gdate::rename.vars(data, from, to)	
	if(!is.null(old) && !is.null(grep)){
		stop("Only one of old and grep can be used")
	}
	if(!is.null(grep)){
		if(is.null(replace)){
			stop("Please set replace to a valid replacement string!")
		}
	    nameVector = names(x)
	    if (is.null(nameVector)) {
	        stop(paste0("umx_names requires a dataframe or something else with names(), ", 
	            umx_object_as_str(x), " is a ", typeof(x)))
	    }
		new_names = gsub(grep, replace, nameVector)
		if(test){
			message("The following changes would be made (set test =FALSE to actually make them)")
			message("Was")
			print(nameVector)
			message("New:")
			print(new_names)
		} else {
			names(x) = new_names
		}
		invisible(x)		
	} else {
		if(!is.null(old)){
		# message("replacing old with replace")
		if(length(old) != length(replace)){
			stop("You are trying to replace ", length(old), " old names with ", length(replace), "new names: Lengths must match")
		}
		names_to_replace <- old
		new_names_to_try <- replace
		} else {
			names_to_replace <- names(replace)
			new_names_to_try <- unname(replace)
		}
		old_names <- names(x)

		if(!all(names_to_replace %in% old_names)) {
			warning("The following names did not appear in the dataframe:", 
			paste(names_to_replace[!names_to_replace %in% old_names], collapse=", "), "\nperhaps you already updated them")
		}

		if(anyDuplicated(names_to_replace)) {
		  err <- paste("You are trying to update the following names more than once:", 
		           paste(names_to_replace[duplicated(names_to_replace)], collapse=", "))
		  stop(err)
		}

		if(anyDuplicated(new_names_to_try)) {
		  err <- paste("You have the following duplicates in your replace list:", 
		         	paste(new_names_to_try[duplicated(new_names_to_try)], collapse=", ")
		)
		  stop(err)
		}
		new_names <- new_names_to_try[match(old_names, names_to_replace)]  
		if(test){
			message("The following changes would be made (set test =FALSE to actually make them")
			message("Names to be replaced")
			print(names_to_replace)
			message("replacement names:")
			print(new_names)
			invisible(x)
		} else {
			names(x) = new_names
			setNames(x, ifelse(is.na(new_names), old_names, new_names)) # also returns the new object
		}
	}
}

#' umx_grep
#'
#' Search for text. Will search names if given a data.frame, or strings if given a vector of strings. 
#' NOTE: Handy feature is that this can search the labels of data imported from SPSS
#'
#' @param df The \code{\link{data.frame}} or string to search
#' @param grepString the search string
#' @param output the column name, the label, or both (default)
#' @param ignore.case whether to be case sensitive or not (default TRUE = ignore case)
#' @param useNames whether to search the names as well as the labels (for SPSS files with label metadata)
#' @return - list of matched column names and/or labels
#' @family Miscellaneous Utility Functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_grep(mtcars, "hp", output="both", ignore.case= TRUE)
#' umx_grep(c("hp", "ph"), "hp")
#' umx_grep(mtcars, "^h.*", output="both", ignore.case= TRUE)
#' \dontrun{
#' umx_grep(spss_df, "labeltext", output = "label") 
#' umx_grep(spss_df, "labeltext", output = "name") 
#' }
umx_grep <- function(df, grepString, output = c("both", "label", "name"), ignore.case=TRUE, useNames= FALSE) {
	output = match.arg(output)
	# if(length(grepString > 1)){
	# 	for (i in grepString) {
	# 		umx_grep_labels(df, i, output=output, ignore.case=ignore.case, useNames=useNames)
	# 	}
	if(is.data.frame(df)){
		vLabels = attr(df, "variable.labels") # list of descriptive labels?
		a       = names(df) 
		if(is.null(vLabels)){
			# message("No labels found")
			return(grep(grepString, names(df), value=TRUE, ignore.case= ignore.case))
		}
		if(useNames) {
			findIndex = grep(grepString,a, value=F, ignore.case=ignore.case)
			return( as.matrix(vLabels[findIndex]))
		} else {
			# need to cope with finding nothing
			findIndex = grep(grepString,vLabels, value=F, ignore.case=ignore.case)
			if(output=="both") {
				theResult <- as.matrix(vLabels[findIndex])
			} else if(output=="label"){
				vLabels= as.vector(vLabels[findIndex])
				theResult <- (vLabels)
			} else if(output=="name"){
				theResult <- names(vLabels)[findIndex]
			}else{
				stop(paste("bad choice of output:", output))
			}
			if(dim(theResult)[1]==0 |is.null(theResult)){
				cat("using names!!!!\n")
				findIndex = grep(grepString,a, value=F, ignore.case=ignore.case)
				return(as.matrix(vLabels[findIndex]))
			} else {
				return(theResult)
			}
		}
	} else {
		# TODO	assume this is just a string or vector of strings
		return(grep(grepString, df, value = TRUE, ignore.case = ignore.case))
	}
}


# =====================
# = Utility functions =
# =====================

#' umx_msg
#'
#' Helper function to make dumping  "ObjectName has the value: <objectvalue>" easy
#'
#' @param  x the thing you want to print
#' @return - NULL
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' a = "brian"
#' umx_msg(a)
#' a = c("brian", "sally", "jane")
#' umx_msg(a)

umx_msg <- function(x) {
    nm = deparse(substitute(x) )
	if(length(x) > 1) {
		message(nm, " = ", omxQuotes(x))	
	} else {
		message(nm, " = ", x)	
	}
}

#' umx_paste_names
#'
#' Helper to add suffixes to names: useful for expanding twin vars like "bmi" into c("bmi_T1", "bmi_T2")
#' Use textConstant to turning add a constant"E_T1", by adding "_T" and 1.
#'
#' @param varNames a list of base names, e.g c("bmi", "IQ")
#' @param textConstant The suffix added to all names, e.g. "_T" (the default)
#' @param suffixes a list of terminal suffixes differentiating the var names (e.g c("1", "2"))
#' @return - vector of suffixed var names, i.e., c("a_T1", "b_T1", "a_T2", "b_T2")
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' umx_paste_names("bmi", "_T", 1:2)
#' umx_paste_names("bmi", suffixes = c("_T1", "_T2"))
#' varNames = umx_paste_names(c("N", "E", "O", "A", "C"), "_T", 1:2)
umx_paste_names <- function(varNames, textConstant = "", suffixes) {
	nameList = c()
	for (ID in suffixes) {
		nameList = c(nameList, paste0(varNames, textConstant, ID))
	}
	return(nameList)
}

#' umx_merge_CIs
#'
#' if you compute some CIs in one model and some in another (copy of the same model, perhaps to get some parallelism),
#' this is a simple helper to cludge them together.
#'
#' @param m1 first copy of the model
#' @param m2 second copy of the model
#' @return - \code{\link{mxModel}}
#' @family Miscellaneous Utility Functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umx_merge_CIs(m1, m2)
#' # TODO remove duplicates...
#' # TODO (check they are the same as well!)
#' # TODO Support arbitrarily long list of input models with ...
#' # TODO check the models are the same, with same fit
#' # TODO check the models have CIs
#' }
umx_merge_CIs <- function(m1, m2) {
	# cludge together
	a  = m1@output$confidenceIntervals
	b  = m2@output$confidenceIntervals
	a_names = attr(a, "dimnames")[[1]]
	b_names = attr(b, "dimnames")[[1]]
	all_names = c(a_names, b_names)
	all_CIs = rbind(a,b)
	if(any(duplicated(all_names))){
		message("Some CIs appear to be duplicates...")
		message("I dropped these from the list:")
		cat(duplicated(all_names))
		cat(all_names[duplicated(all_names)])
		cat(all_CIs[duplicated(all_names), ])
	}

	m1@output$confidenceIntervals = all_CIs
	return(m1)
	# return(all_CIs)
}

#' umx_round
#'
#' A version of round() which works on dataframes that contain non-numeric data (or data that cannot be coerced to numeric)
#' Helpful for dealing with table output that mixes numeric and string types.
#'
#' @param df a dataframe to round in
#' @param digits how many digits to round to (defaults to getOption("digits"))
#' @param coerce whether to make the column numeric if it is not (default = FALSE)
#' @return - \code{\link{mxModel}}
#' @family Miscellaneous Utility Functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' head(umx_round(mtcars, coerce = FALSE))
#' head(umx_round(mtcars, coerce = TRUE))

umx_round <- function(df, digits = getOption("digits"), coerce = FALSE) {
	if(is.matrix(df)){
		df = data.frame(df)
	}

	if(!is.data.frame(df)){
		stop("df input for umx_round must be a dataframe")
	}
	# for each column, if numeric, round
	rows = dim(df)[1]
	cols = dim(df)[2]
	for(c in 1:cols) {
		if(coerce){
			for(r in 1:rows) {
				df[r, c] = round(as.numeric(df[r, c]), digits)
			}
		} else {
			if(is.numeric(df[1, c])){
				df[ , c] = round(df[ , c], digits)
			}
		}
	}
	return(df)
}

specify_decimal <- function(x, k){
	format(round(x, k), nsmall = k)
}

# extracted from Rcmdr: TODO: fix URL for RCmdr reference
#' reliability
#'
#' Compute and report Coefficient alpha
#'
#' @param S A square, symmetric, numeric covariance matrix
#' @return - 
#' @export
#' @family Miscellaneous Stats Helpers
#' @seealso - \code{\link{cov}}
#' @references - \url{http://Rcmdr}
#' @examples
#' # treat vehicle aspects as items of a test
#' reliability(cov(mtcars))
reliability <-function (S){
     reliab <- function(S, R) {
         k <- dim(S)[1]
         ones <- rep(1, k)
         v <- as.vector(ones %*% S %*% ones)
         alpha <- (k/(k - 1)) * (1 - (1/v) * sum(diag(S)))
         rbar <- mean(R[lower.tri(R)])
         std.alpha <- k * rbar/(1 + (k - 1) * rbar)
         c(alpha = alpha, std.alpha = std.alpha)
     }
     result <- list()
     if ((!is.numeric(S)) || !is.matrix(S) || (nrow(S) != ncol(S)) || any(abs(S - t(S)) > max(abs(S)) * 1e-10) || nrow(S) < 2)
         stop("argument must be a square, symmetric, numeric covariance matrix")
     k <- dim(S)[1]
     s <- sqrt(diag(S))
     R <- S/(s %o% s)
     rel <- reliab(S, R)
     result$alpha <- rel[1]
     result$st.alpha <- rel[2]
     if (k < 3) {
         warning("there are fewer than 3 items in the scale")
         return(invisible(NULL))
     }
     rel <- matrix(0, k, 3)
     for (i in 1:k) {
         rel[i, c(1, 2)] <- reliab(S[-i, -i], R[-i, -i])
         a <- rep(0, k)
         b <- rep(1, k)
         a[i] <- 1
         b[i] <- 0
         cov <- a %*% S %*% b
         var <- b %*% S %*% b
         rel[i, 3] <- cov/(sqrt(var * S[i, i]))
     }
     rownames(rel) <- rownames(S)
     colnames(rel) <- c("Alpha", "Std.Alpha", "r(item, total)")
     result$rel.matrix <- rel
     class(result) <- "reliability"
     result
}

print.reliability <- function (x, digits = 4, ...){
     cat(paste("Alpha reliability = ", round(x$alpha, digits), "\n"))
     cat(paste("Standardized alpha = ", round(x$st.alpha, digits), "\n"))
     cat("\nReliability deleting each item in turn:\n")
     print(round(x$rel.matrix, digits))
     invisible(x)
}

#' umxAnova
#'
#' Generate a text-format report of the F values in an ANOVA.
#' Like umxAnova(model) # --> "F(495,1) = 0.002"
#'
#' @param model an \code{\link{lm}} model to report F for.
#' @param digits how many numbers after the decimal for F (p-value is APA-style)
#' @return - F in  text format
#' @family Miscellaneous Functions
#' @family Miscellaneous Stats Helpers
#' @export
#' @seealso - \code{\link{lm}}, \code{\link{anova}}, \code{\link{summary}}
#' @examples
#' m1 = lm(mpg~ cyl + wt, data = mtcars)
#' umxAnova(m1)
#' m2 = lm(mpg~ cyl, data = mtcars)
#' umxAnova(m2)
#' umxAnova(anova(m1, m2))

umxAnova <- function(model, digits = 2) {
	if(is(digits,"lm")){
		stop(paste0(
		"digits looks like an lm model: ",
		"You probably said:\n    umxAnova(m1, m2)\n",
		"when you meant\n",
		"   umxAnova(anova(m1, m2))"
		))
	}
	tmp = class(model)
	if(all(tmp=="lm")){
		a = summary(model)
		dendf = a$fstatistic["dendf"]
		numdf = a$fstatistic["numdf"]
		value = a$fstatistic["value"]
		return(paste0(
				   "F(", dendf, ",", numdf, ") = " , round(value, digits),
					", p = ", umx_APA_pval(pf(value, numdf, dendf, lower.tail = FALSE))
				)
		)
	} else {
		if(model[2, "Res.Df"] > model[1, "Res.Df"]){
			message("Have you got the models the right way around?")
		}
		paste0(
			"F(", 
			round(model[2, "Res.Df"]), ",",
			round(model[2,"Df"]), ") = ",
			round(model[2,"F"], digits), ", p = ",
			umx_APA_pval(model[2,"Pr(>F)"])
		)
	}	
}

# =====================
# = Statistical tools =
# =====================


#' Stouffer.test
#'
#' Runs a Stouffer.test
#'
#' @param p A list of p values, i.e., p(.4, .3, .6, .01)
#' @family Miscellaneous Stats Helpers
#' @seealso - 
#' @references - \url{http://imaging.mrc-cbu.cam.ac.uk/statswiki/FAQ/CombiningPvalues}
#' Stouffer, Samuel A., Edward A. Suchman, Leland C. DeVinney, Shirley A. Star, 
#' and Robin M. Williams, Jr. (1949). Studies in Social Psychology in World War II: 
#' The American Soldier. Vol. 1, Adjustment During Army Life. Princeton: Princeton University Press.
#' 
#' Bailey TL, Gribskov M (1998). Combining evidence using p-values: application to sequence
#' homology searches. Bioinformatics, 14(1) 48-54.
#' Fisher RA (1925). Statistical methods for research workers (13th edition). London: Oliver and Boyd.
#' Manolov R and Solanas A (2012). Assigning and combining probabilities in single-case studies.
#' Psychological Methods 17(4) 495-509. Describes various methods for combining p-values including
#' Stouffer and Fisher and the binomial test.
#' \url{http://www.burns-stat.com/pages/Working/perfmeasrandport.pdf}
#' @export
#' @examples
#' Stouffer.test(p = c(.01, .2, .3))

Stouffer.test <- function(p = NULL) {
	pl <- length(p)
	if (is.null(p) | pl < 2) {
		stop("There was an empty array of p-values")
	}
	erf <- function(x) {
		2 * pnorm(2 * x/ sqrt(2)) - 1
	}
	erfinv <- function(x) {
		qnorm( (x + 1) / 2 ) / sqrt(2)
	}
	pcomb <- function(p) {
		(1 - erf(sum(sqrt(2) * erfinv(1 - 2 * p)) / sqrt(2 * length(p))))/2
	}
	pcomb(p)
}


# Test differences in Kurtosis and Skewness
kurtosisDiff <- function(x, y, B = 1000){
	require(psych)
	kx <- replicate(B, kurtosi(sample(x, replace = TRUE)))
	ky <- replicate(B, kurtosi(sample(y, replace = TRUE)))
	return(kx - ky)	
}
# Skew
skewnessDiff<- function(x, y, B = 1000){
	require(psych)
	sx <- replicate(B, skew(sample(x, replace = TRUE)))
	sy <- replicate(B, skew(sample(y, replace = TRUE)))
	return(sx - sy)	
}

# get the S3 and its parameter list with
# seq.default()


#' @title umx_pp33
#' @description
#' A utility function to compute the critical value of student (xc) that
#' gives p = .05 when df = df_xc = qt(p = .975, df = target_df)
#' 
#' @details
#' Find noncentrality parameter (ncp) that leads 33% power to obtain xc
#' The original is published at p-curve
#' \url{http://www.p-curve.com/Supplement/R/pp33.r} 
#' 
#' Find noncentrality parameter (ncp) that leads 33% power to obtain xc
#'
#' @param target_df the... TODO
#' @param x the... TODO
#' @return - value
#' @family Miscellaneous Stats Helpers
#' @export
#' @seealso - \code{\link{pnorm}}
#' @references - \url{http://www.p-curve.com/Supplement/R/pp33.r}
#' @examples
#' # To find the pp-value for 33% power for a t(38)=2.4, execute 
#' umx_pp33(target_df = 38, x = 2.4)

umx_pp33 <- function(target_df, x) {
	f <- function(delta, pr, x, df){
		pt(x, df = df, ncp = delta) - pr
	}
	# Find critical value of student (xc) that gives p=.05 when df = target_df
	xc = qt(p = .975, df = target_df)		
	# Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	out <- uniroot(f, c(0, 37.62), pr =2/3, x = xc, df = target_df)	
	ncp_ = out$root	
	# Find probability of getting x_ or larger given ncp
	p_larger = pt(x, df = target_df, ncp = ncp_)
	# Condition on p < .05 (i.e., get pp-value)
	pp = 3 * (p_larger - 2/3)
	# Print results
	return(pp)
}

umxCovData = function(df, columns = manifests, use = "pairwise.complete.obs") {
	if(!all(manifests %in%  names(df))){
		message("You asked for", length(manifests), "variables. Of these the following were not in the dataframe:")
		stop(paste(manifests[!(manifests %in% names(df))], collapse=", "))
	} else {
		return(mxData(cov( df[,manifests], use = use), type = "cov", numObs = nrow(df)))
	}	
}


#' umxCov2cor
#'
#' Just a version of cov2cor that forces the upper and lower triangles to be identical (rather than really similar)
#'
#' @param x something that cov2cor can work on (matrix, df, etc.)
#' @return - a correlation matrix
#' @export
#' @family Miscellaneous Stats Helpers
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' x_cor = umxCov2cor(x)
#' }

umxCov2cor <- function(x) {
	x = cov2cor(x)
	x[lower.tri(x)] <- t(x)[lower.tri(t(x))]
	return(x)
}

# ===========================================
# = Functions to check kind: return Boolean =
# ===========================================

#' umx_has_been_run
#'
#' check if an mxModel has been run or not
#'
#' @param model The \code{\link{mxModel}} you want to check has been run
#' @param stop  Whether to stop if the model has not been run (defaults to FALSE)
#' @return - boolean
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umx_has_been_run(m1)
umx_has_been_run <- function(model, stop = FALSE) {
	output <- model@output
	if (is.null(output)){
		if(stop){
			stop("Provided model has no objective function, and thus no output to process further")
		}else{
			return(FALSE)
		}
	} else if (length(output) < 1){
		if(stop){
			stop("Provided model has no output. , and thus no output to process further")
		} else {
			return(FALSE)
		}
	}
	return(TRUE)
}

#' umx_check
#'
#' Check that a test evaluates to TRUE. If not, stop, warn, or message the user
#'
#' @param boolean.test test evaluating to TRUE or FALSE
#' @param action One of "stop" (the default), "warning", or "message"
#' @param message what to tell the user when boolean.test is FALSE
#' @return - boolean
#' @export
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_check(length(1:3)==3, "stop", "item must have length == 3")
umx_check <- function(boolean.test, action = c("stop", "warning", "message"), message = "check failed"){
	action = match.arg(action)
	if(!boolean.test){
		if(action == "stop"){
			stop(message)
		} else if(action == "warning"){
			warning(message)
		}else{
			message(message)			
		}
	}
	return(boolean.test)
}

#' umx_check_names
#'
#' Check if a list of names are in the names() of a dataframe (or the of a matrix)
#'
#' @param namesNeeded list of variable names to find
#' @param data data.frame (or matrix) to search in for names
#' @param die whether to die if the check fails (defaults to TRUE)
#' @param no_others Whether to test that the data contain no columns in addition to those in namesNeeded (defaults to FALSE)
#' @return - boolean
#' @export
#' @family Miscellaneous Building Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor) # "x1" "x2" "x3" "x4" "x5"
#' umx_check_names(c("x1", "x2"), demoOneFactor)
#' umx_check_names(c("x1", "x2"), as.matrix(demoOneFactor))
#' umx_check_names(c("x1", "x2"), cov(demoOneFactor[,c("x1","x2")]))
#' umx_check_names(c("z1", "x2"), data = demoOneFactor, die = FALSE)
#' umx_check_names(c("x1", "x2"), data = demoOneFactor, die = FALSE, no_others = TRUE)
#' umx_check_names(c("x1","x2","x3","x4","x5"), data = demoOneFactor, die = FALSE, no_others = TRUE)
#' \dontrun{
#' umx_check_names(c("bad_var_name", "x2"), data = demoOneFactor, die = TRUE)
#' }
umx_check_names <- function(namesNeeded, data, die = TRUE, no_others = FALSE){
	if(is.data.frame(data)){
		namesInData = names(data)
	}else if(is.matrix(data)){
		namesInData = dimnames(data)[[2]]
	} else {
		stop("data has to be a dataframe or matrix. You gave me a", typeof(data))
	}
	namesFound = (namesNeeded %in% namesInData)
	if(any(!namesFound)){
		if(die){
			# print(namesInData[namesFound])
			stop("Not all required names were found in the data. Missing were:\n",
				paste(namesNeeded[!namesFound], collapse = "; ")
			)
		} else {
			return(FALSE)
		}
	} else if(no_others & !setequal(namesInData, namesNeeded)){
		if(die){
			stop("Data contains columns other than those needed. Superfluous columns were:\n", 
				paste(namesInData[!namesInData %in% namesNeeded], collapse = "; "))
		} else {
			return(FALSE)
		}
	} else {
		return(TRUE)
	}
}

#' umx_cov_diag
#'
#' Helper to get variances from a df that might contain some non-numeric columns. Non numerics are set to whatever ordVar=
#'
#' @param df a dataframe of raw data from which to get variances.
#' @param ordVar The value to return at any ordinal columns found, i.e., (defaults to 1, i.e., z
#' @param format to return: options are c("diag", "Full", "Lower"). Defaults to diag: a vector of variances
#' @param use passed to \code{\link{cov}} - defaults to "complete.obs" (other options are in the function )
#' @return - \code{\link{mxModel}}
#' @export
#' @family Miscellaneous Building Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' tmp = mtcars[,1:4]
#' tmp$cyl = ordered(mtcars$cyl) # ordered factor
#' tmp$hp  = ordered(mtcars$hp)  # binary factor
#' umx_cov_diag(tmp, ordVar = 1, use = "pair")
#' tmp2 = tmp[, c(1,3)]
#' umx_cov_diag(tmp2)
#' umx_cov_diag(tmp2, format = "Full")
umx_cov_diag <- function(df, ordVar = 1, format = c("diag", "Full", "Lower"), use = c("complete.obs", "pairwise.complete.obs", "everything", "all.obs", "na.or.complete")){
	format = match.arg(format)
	use    = match.arg(use)
	if(any(umx_is_ordered(df))){
		nCol = dim(df)[2]
		starts = diag(ordVar, nCol, nCol)
		cont = umx_is_ordered(df, continuous.only = TRUE)
		if(any(cont)){
			for(i in which(cont)) {
				starts[i,i] = var(df[,i], use = use)
			}
		}
		starts = diag(starts)
	} else {
		starts = diag(var(df, use = use))
	}
	if(format == "diag"){
		return(starts)
	} else {
		message("only var list implemented")
		return(starts)	
	}
}

#' umx_means
#'
#' Helper to get means from a df that might contain ordered  data. Factor means are set to "ordVar"
#'
#' @param df a dataframe of raw data from which to get variances.
#' @param ordVar value to return for the means of factor data = 0
#' @param na.rm passed to mean - defaults to "na.rm"
#' @return - frame of means
#' @export
#' @family Miscellaneous Stats Helpers
#' @examples
#' tmp = mtcars[,1:4]
#' tmp$cyl = ordered(mtcars$cyl) # ordered factor
#' tmp$hp  = ordered(mtcars$hp)  # binary factor
#' umx_means(tmp, ordVar = 0, na.rm = TRUE)
umx_means <- function(df, ordVar = 0, na.rm = TRUE) {
	if(any(umx_is_ordered(df))){
		# Set the default outcome
		means = rep(ordVar, times = dim(df)[2])
		# Get variables where mean makes snes
		cont = umx_is_ordered(df, continuous.only = TRUE)
		if(any(cont)){
			for(i in which(cont)) {
				means[i] = mean(df[, i], na.rm = na.rm)
			}
		}
	} else {
		means = umx_apply(mean, df, by = "columns", na.rm = TRUE)
	}
	return(means)
}

#' umx_is_ordered
#'
#' Return the names of any ordinal variables in a dataframe
#'
#' @param df an \code{\link{data.frame}} to look in for ordinal variables
#' @param names whether to return the names of ordinal variables, or a binary (T,F) list (default = FALSE)
#' @param strict whether to stop when unordered factors are found (default = TRUE)
#' @param binary.only only count binary factors (2-levels) (default = FALSE)
#' @param ordinal.only only count ordinal factors (3 or more levels) (default = FALSE)
#' @param continuous.only use with names = TRUE to get the names of the continuous variables
#' @return - vector of variable names or Booleans
#' @export
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' tmp = mtcars
#' tmp$cyl = ordered(mtcars$cyl) # ordered factor
#' tmp$vs = ordered(mtcars$vs) # binary factor
#' umx_is_ordered(tmp) # numeric indices
#' umx_is_ordered(tmp, names = TRUE)
#' umx_is_ordered(tmp, names = TRUE, binary.only = TRUE)
#' umx_is_ordered(tmp, names = TRUE, ordinal.only = TRUE)
#' umx_is_ordered(tmp, names = TRUE, continuous.only = TRUE)
#' umx_is_ordered(tmp, continuous.only = TRUE)
#' isContinuous = !umx_is_ordered(tmp)
#' tmp$gear = factor(mtcars$gear) # unordered factor
#' # nb: Factors are not necessarily ordered! By default unordered factors cause an message...
#' \dontrun{
#' tmp$cyl = factor(mtcars$cyl)
#' umx_is_ordered(tmp, names=TRUE)
#' }

umx_is_ordered <- function(df, names = FALSE, strict = TRUE, binary.only = FALSE, ordinal.only = FALSE, continuous.only = FALSE) {
	if(sum(c(binary.only, ordinal.only, continuous.only)) > 1){
		stop("Only one of binary.only ordinal.only and continuous.only can be TRUE")
	}
	if(!is.data.frame(df)){
		stop("df argument to umx_is_ordered must be a dataframe. Perhaps this is one column selected from a data frame without [r,c, drop=FALSE]? ")
	}
	nVar = ncol(df);
	# Which are ordered factors?
	isFactor  = rep(FALSE, nVar)
	isOrdered = rep(FALSE, nVar)
	for(n in 1:nVar) {
		if(is.ordered(df[, n])) {
			thisLevels  = length(levels(df[, n]))
			if(binary.only & (2 == thisLevels) ){
				isOrdered[n] = TRUE
			} else if(ordinal.only & (thisLevels > 2) ){
				isOrdered[n] = TRUE	
			} else if(!binary.only & !ordinal.only) {
				isOrdered[n] = TRUE
			}
		}
		if(is.factor(df[,n])) {
			thisLevels = length(levels(df[,n]))
			if(binary.only & (2 == thisLevels) ){
				isFactor[n] = TRUE
			} else if(ordinal.only & (thisLevels > 2) ){
				isFactor[n] = TRUE	
			} else if(!binary.only & !ordinal.only) {
				isFactor[n] = TRUE
			}
		}
	}
	if(any(isFactor & ! isOrdered) & strict){
		message("Dataframe contains at least 1 unordered factor. Set strict = FALSE to allow this.\n",
			  omxQuotes(names(df)[isFactor & ! isOrdered])
		)
	}

	if(continuous.only){
		isOrdered = !isOrdered
		isFactor  = !isFactor
	}

	if(names){
		if(strict){
			return(names(df)[isOrdered])
		} else {
			return(names(df)[isFactor])
		}
	} else {
		if(strict){
			return(isOrdered)
		} else {
			return(isFactor)
		}
	}
}

#' umx_is_RAM
#'
#' Utility function returning a binary answer to the question "Is this a RAM model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx RAM \code{\link{mxModel}}
#' @return - Boolean
#' @export
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umxSummary(m1, show = "std")
#' if(umx_is_RAM(m1)){
#' 	message("nice RAM model!")
#' }
#' if(!umx_is_RAM(m1)){
#' 	msg("model must be a RAM model")
#' }
umx_is_RAM <- function(obj) {
	# return((class(obj$objective)[1] == "MxRAMObjective" | class(obj$expectation)[1] == "MxExpectationRAM"))
	if(!umx_is_MxModel(obj)){
		return(F)
	} else if(class(obj)[1] == "MxRAMModel"){
		return(T)
	} else {
		return(class(obj$objective)[1] == "MxRAMObjective")
	}
}

#' umx_is_MxModel
#'
#' Utility function returning a binary answer to the question "Is this an OpenMx model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx \code{\link{mxModel}}
#' @return - Boolean
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{mxModel}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' m1 = mxModel("test")
#' if(umx_is_MxModel(m1)){
#' 	message("nice OpenMx model!")
#' }
umx_is_MxModel <- function(obj) {
	isS4(obj) & is(obj, "MxModel")	
}

#' umx_is_MxMatrix
#'
#' Utility function returning a binary answer to the question "Is this an OpenMx mxMatrix?"
#'
#' @param obj an object to be tested to see if it is an OpenMx \code{\link{mxMatrix}}
#' @return - Boolean
#' @export
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' x = mxMatrix(name = "expMean", type = "Full", nrow = 3, ncol = 3, free = TRUE, values = .3, byrow = TRUE)
#' if(umx_is_MxMatrix(x)){
#' 	message("nice OpenMx matrix!")
#' }
umx_is_MxMatrix <- function(obj) {
	isS4(obj) & is(obj, "MxMatrix")	
}

#' umx_is_cov
#'
#' test if a data frame or matrix is cov or cor data, or is likely to be raw...
#' @param data dataframe to test
#' @param boolean whether to return the type ("cov") or a boolean (default = string)
#' @param verbose How much feedback to give (default = FALSE)
#' @return - "raw", "cor", or "cov", or, if boolean= T, then T | F
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' df = cov(mtcars)
#' umx_is_cov(df)
#' df = cor(mtcars)
#' umx_is_cov(df)
#' umx_is_cov(df, boolean = TRUE)
#' umx_is_cov(mtcars, boolean = TRUE)

umx_is_cov <- function(data = NULL, boolean = FALSE, verbose = FALSE) {
	if(is.null(data)) { stop("Error in umx_is_cov: You have to provide the data = that you want to check...\n",
		"Or as Jack Nicholson says, 'No ticky, no laundry' :-) ") }

	if( nrow(data) == ncol(data)) {
		if(all(data[lower.tri(data)] == t(data)[lower.tri(t(data))])){
			if(all(diag(data) == 1)){
				isCov = "cor"
				if(verbose){
					message("treating data as cor")
				}
			} else {
				isCov = "cov"
				if(verbose){
					message("treating data as cov")
				}
			}
		} else {
			isCov = "raw"
			if(verbose){
				message("treating data as raw: it's a bit odd that it's square, however")
			}
		}
	} else {
		isCov = "raw"
		if(verbose){
			message("treating data as raw")
		}
	}
	if(boolean){
		return(isCov %in%  c("cov", "cor"))
	} else {
		return(isCov)
	}
}

#' umx_has_means
#'
#' A utility function to return a binary answer to the question "does this \code{\link{mxModel}} have a means model?" 
#'
#' @param model The \code{\link{mxModel}} to check for presence of means
#' @return - TRUE or FALSE
#' @export
#' @family Miscellaneous Functions
#' @references - http://www.github.com/tbates/umx/
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_has_means(m1)
#' m1 <- mxModel(m1,
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(demoOneFactor, type = "raw")
#' )
#' umx_has_means(m1)
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umx_has_means(m1)
umx_has_means <- function(model) {
	# TODO check for type? check for run
	if(!umx_is_RAM(model)){
		stop("Can only run on RAM models so far")
	}
	return(!is.null(model@matrices$M))
	# expMeans = attr(model@output$algebras[[paste0(model$name, ".fitfunction")]], "expMean")
	# return(!all(dim(expMeans) == 0))
}

#' umx_has_CIs
#'
#' A utility function to return a binary answer to the question "does this \code{\link{mxModel}} have confidence intervals?" 
#'
#' @param model The \code{\link{mxModel}} to check for presence of CIs
#' @param check What to check for: "intervals" requested, "output" present, or "both". Defaults to "both"
#' @return - TRUE or FALSE
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{mxCI}}, \code{\link{umxCI}}, \code{\link{umxRun}}
#' @references - http://www.github.com/tbates/umx/
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umx_has_CIs(m1) # FALSE: no CIs and no output
#' m1 = mxModel(m1, mxCI("G_to_x1"))
#' umx_has_CIs(m1, check = "intervals") # TRUE intervals set
#' umx_has_CIs(m1, check = "output")  # FALSE not yet run
#' m1 = mxRun(m1)
#' umx_has_CIs(m1, check = "output")  # Still FALSE: Set and Run
#' m1 = mxRun(m1, intervals = TRUE)
#' umx_has_CIs(m1, check = "output")  # TRUE: Set, and Run with intervals = T
umx_has_CIs <- function(model, check = c("both", "intervals", "output")) {
	check = umx_default_option(check, c("both", "intervals", "output"), check=F)
	if(is.null(model@intervals)){
		thisModelHasIntervals = FALSE
	}else{
		thisModelHasIntervals = length(names(model@intervals)) > 0
	}
	if(is.null(model@output$confidenceIntervals)){
		thisModelHasOutput = FALSE
	} else {
		thisModelHasOutput = dim(model@output$confidenceIntervals)[1] > 0
	}
	# do logic of returning a value
	if(check == "both"){
		return(thisModelHasIntervals & thisModelHasOutput)
	} else if(check == "intervals"){
		return(thisModelHasIntervals)
	}else{
		return(thisModelHasOutput)
	}
}


#' umx_check_model
#'
#' Check an OpenMx model
#'
#' @param obj an object to check
#' @param type what type the model must be, i.e., "RAM", "LISREL", etc. (defaults to not checking NULL)
#' @param hasData whether the model should have data or not (defaults to not checking NULL)
#' @param beenRun whether the model has been run or not (defaults to not checking NULL)
#' @param hasMeans whether the model should have a means model or not (defaults to not checking NULL)
#' @param checkSubmodels whether to check submodels (not implemented yet) (default = FALSE)
#' @return - boolean
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umx}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_check_model(m1)
#' umx_check_model(m1, type = "RAM") # equivalent to umx_is_RAM()
#' umx_check_model(m1, hasData = TRUE)
#' \dontrun{
#' umx_check_model(m1, hasMeans = TRUE)
#' umx_check_model(m1, beenRun = FALSE)
#' }
umx_check_model <- function(obj, type = NULL, hasData = NULL, beenRun = NULL, hasMeans = NULL, checkSubmodels = FALSE) {
	# TODO hasSubmodels = FALSE
	# TODO fix all these so they respect true and false...
	if (!umx_is_MxModel(obj)) {
		stop("'model' must be an mxModel")
	}
	if(is.null(type)){
		#no check
	}else if(type == "RAM"){
		if (!umx_is_RAM(obj)) {
			stop("'model' must be an RAMModel")
		}
	} else {
		message(paste("type", sQuote(type), "not handled yet"))
	}
	if(checkSubmodels){
		if (length(obj@submodels) > 0) {
			message("Cannot yet handle models with submodels")
		}
	}
	if(!is.null(hasData)){
		if (hasData & is.null(obj@data@observed)) {
			stop("'model' does not contain any data")
		}
	}
	if(!is.null(beenRun)){
		if(!(umx_has_been_run(obj) == beenRun)){
			stop("'model' run state != ", beenRun)		
		}
	}
	if(!is.null(hasMeans)){
		if (!(hasMeans == umx_has_means(obj))) {
			stop("'model' does or does not have means")
		}
	}
	return(TRUE)
}

#' umxJiggle
#'
#' umxJiggle takes values in a matrix and jiggles them
#'
#' @param matrixIn an \code{\link{mxMatrix}} to jiggle the values of
#' @param mean the mean value to add to each value
#' @param sd the sd of the jiggle noise
#' @param dontTouch A value, which, if found, will be left as-is (defaults to 0)
#' @return - \code{\link{mxMatrix}}
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' \dontrun{
#' mat1 = umxJiggle(mat1)
#' }

umxJiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {
	mask = (matrixIn != dontTouch);
	newValues = mask;
	matrixIn[mask == TRUE] = matrixIn[mask == TRUE] + rnorm(length(mask[mask == TRUE]), mean = mean, sd = sd);
	return (matrixIn);
}


#' umx_APA_pval
#'
#' round a p value so you get < .001 instead of .000000002 or .134E-16
#'
#' @param p A p-value to round
#' @param min Threshold to say < min
#' @param rounding Number of decimal to round to 
#' @param addComparison Whether to return the bare number, or to add the appropriate comparison symbol (= <)
#' @family Miscellaneous Functions
#' @family Reporting Functions
#' @return - a value
#' @export
#' @seealso - \code{\link{round}}
#' @examples
#' umx_APA_pval(.052347)
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(1.23E-4)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = TRUE)

umx_APA_pval <- function(p, min = .001, rounding = 3, addComparison = NA) {
	# addComparison can be NA to only add when needed
	if(length(p) > 1){
		o = rep(NA, length(p))
		for(i in seq_along(p)) {
		   o[i] = umx_APA_pval(p[i], min = min, rounding = rounding, addComparison = addComparison)
		}
		return(o)
	} else {
		if(is.nan(p) | is.na(p)){
			if(is.na(addComparison)){
				return(p)
			}else if(addComparison){
				return(paste0("= ", p))
			} else {
				return(p)
			}
		}
		if(p < min){
			if(is.na(addComparison)){
				return(paste0("< ", min))
			}else if(addComparison){
				return(paste0("< ", min))
			} else {
				return(min)
			}
		} else {
			if(is.na(addComparison)){
				return(format(round(p, rounding), scientific = FALSE, nsmall = rounding))
			}else if(addComparison){				
				return(paste0("= ", format(round(p, rounding), scientific = FALSE, nsmall = rounding)))
			} else {
				return(round(p, rounding))
			}
		}	
	}
}

#' umx_get_CI_as_APA_string
#'
#' Look up CIs for free parameters in a model, and return as APA-formatted text string
#'
#' @param model an \code{\link{mxModel}} to get CIs from
#' @param cellLabel the label of the cell to interogate for a CI, e.g. "ai_r1c1"
#' @param prefix This submodel to look in (i.e. "top.")
#' @param suffix The suffix for algebras ("_std")
#' @param digits = 2
#' @param verbose = FALSE
#' @return - the CI string, e.g. ".73 [-.2, .98]"
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' umx_get_CI_as_APA_string(fit_IP, cellLabel = "ai_r1c1", prefix = "top.", suffix = "_std")
#' }
umx_get_CI_as_APA_string <- function(model, cellLabel, prefix = "top.", suffix = "_std", digits = 2, verbose= FALSE){
	if(!umx_has_CIs(model)){
		if(verbose){
			message("no CIs")
		}
		return(NA)
	} else {
		# we want "top.ai_std[1,1]" from "ai_r1c1"
		result = tryCatch({
			grepStr = '^(.*)_r([0-9]+)c([0-9]+)$' # 1 = matrix names, 2 = row, 3 = column
			mat = sub(grepStr, '\\1', cellLabel, perl = TRUE);
			row = sub(grepStr, '\\2', cellLabel, perl = TRUE);
			col = sub(grepStr, '\\3', cellLabel, perl = TRUE);
		
			z = model$output$confidenceIntervals
			dimIndex = paste0(prefix, mat, suffix, "[", row, ",", col, "]")

			intervalNames = dimnames(z)[[1]]
			
			
			APAstr = paste0(
				umx_APA_pval(z[dimIndex, "estimate"], min = -1, rounding = digits),
				" [",
				umx_APA_pval(z[dimIndex, "lbound"], min = -1, rounding = digits),
				", ",
				umx_APA_pval(z[dimIndex, "ubound"], min = -1, rounding = digits),
				"]"
			)
		    return(APAstr) 
		}, warning = function(cond) {
			if(verbose){
				message(paste0("warning ", cond, " for CI ", omxQuotes(cellLabel)))
			}
		    return(NA) 
		}, error = function(cond) {
			if(verbose){
				message(paste0("error: ", cond, " for CI ", omxQuotes(cellLabel), "\n",
				"dimIndex = ", dimIndex))
				print(intervalNames)
			}
		    return(NA) 
		}, finally = {
		    # cleanup-code
		})
		return(result)
	}
	# if estimate differs...
}


#' umxAnovaReport
#'
#' umxAnovaReport is a convenience function to format results for journals. There are others. But I made this one.
#' If you give it the output of an lm, it runs anova() and QuantPsyc::lm.beta(), and puts that together in a regression table...
#' Alternatively if you fill in the optional second model, it compares them (just like \code{\link{umxCompare}})
#' @param model1 An \code{\link{lm}} model to make a table from 
#' @param model2 An (optional) second \code{\link{lm}} model to compare to model 1
#' @param raw Should the raw table also be output? (allows checking that nothing crazy is going on)
#' @param format String or markdown format?
#' @param printDIC A Boolean toggle whether you want AIC-type fit change table printed
#' @family Miscellaneous Functions
#' @family Reporting Functions
#' @seealso - \code{\link{umxSummary}}, \code{\link{umxCompare}}, \code{\link{anova}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' model = lm(mpg ~ cyl + disp, data = mtcars)
#' umxAnovaReport(model)

umxAnovaReport <- function(model1, model2 = NULL, raw = TRUE, format = "string", printDIC = FALSE) {
	# TODO merge with anova.report.F, deprecate the latter
	# TODO replace lm.beta with normalizing the variables?
	if(!is.null(model2)){
		# F(-2, 336) =  0.30, p = 0.74
		a = anova(model1, model2)
		if(raw){
			print(a)
		}
		if(format == "string"){
			print( paste0("F(", a[2,"Df"], ",", a[2,"Res.Df"], ") = ",
					round(a[2,"F"],2), ", p ", umx_APA_pval(a[2,"Pr(>F)"])
				)
			)
		} else {
			print( paste0("| ", a[2,"Df"], " | ", a[2,"Res.Df"], " | ", 
				round(a[2,"F"],2), " | ", umx_APA_pval(a[2,"Pr(>F)"]), " | ")
			)
		}		

	} else {
		a = anova(model1);
		if(require(QuantPsyc, quietly = TRUE)){
			a$beta = c(QuantPsyc::lm.beta(model1), NA);
		} else {
			a$beta = NA
			message("To include beta weights\ninstall.packages(\"QuantPsyc\")")
		}

		x <- c("Df", "beta", "F value", "Pr(>F)");
		a = a[,x]; 
		names(a) <- c("df", "beta", "F", "p"); 
		ci = confint(model1)
		a$lowerCI = ci[,1]
		a$upperCI = ci[,2]
		a <- a[,c("df", "beta", "lowerCI", "upperCI", "F", "p")]; 
		print(a)
		if(printDIC){
			a = drop1(model1); 
			a$DIC = round(a$AIC - a$AIC[1], 2); 
			print(a)	
		}
	}
}

#' umx_reorder
#'
#' Reorder the variables in a correlation matrix. Can also remove one or more variables from a matrix using this function
#'
#' @param old a square matrix of correlation or covariances to reorder
#' @param newOrder The order you'd like the variables to be in
#' @return - the re-ordered (and/or resized) matrix
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' oldMatrix = cov(mtcars)
#' umx_reorder(oldMatrix, newOrder = c("mpg", "cyl", "disp")) # first 3
#' umx_reorder(oldMatrix, newOrder = c("hp", "disp", "cyl")) # subset and reordered
umx_reorder <- function(old, newOrder) {
	dim_names = dimnames(old)[[1]]
	if(!all(newOrder %in% dim_names)){
		stop("All variable names must appear in the matrix")
	}
	numVarsToRetain = length(newOrder)
	new = old[1:numVarsToRetain, 1:numVarsToRetain]
	dimnames(new) = list(newOrder, newOrder)
	for(r in newOrder) {
		for(c in newOrder) {
			new[r, c] <- old[r, c]
		}
	}
	return(new)
}

#' umx_recode_deciles
#'
#' Recode a variable into decile levels, returning an mxFactor.
#' 
#' \strong{Note}: redundant deciles are merged. i.e., if the same score identifies the
#' 0, 10th, 20th, and 30th decile, then these are merged into one category.
#'
#' @param var a variable to recode as deciles
#' @param verbose report the min, max, and decile cuts used (default = FALSE)
#' @return - recoded mxFactor
#' @export
#' @family Miscellaneous Utility Functions
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
#' @examples
#' str(umx_recode_deciles(rnorm(1000), TRUE))
umx_recode_deciles <- function(var, verbose=FALSE){
	# var = ocd$OCI_TOTAL
	decileCuts = quantile(var, seq(0, 1, length = 11), type = 5, na.rm=T)
	decileCuts = unique(decileCuts)
	decileLabels = decileCuts[(-length(decileCuts))]
	var = cut(var, breaks = decileCuts, labels = decileLabels); # str(var); levels(var); length(levels(var)); length(decileCuts); 
	var = mxFactor(var, levels = decileLabels)
	if(verbose){
		message("Scores ranged from ", decileCuts[1], " to ", decileCuts[length(decileCuts)], ". Cuts made at ", omxQuotes(decileLabels), ". ", decileCuts[length(decileCuts)])
	}
	return(var)
}

#' umx_has_square_brackets
#'
#' Helper function, checking if a label has sqaure brackets
#'
#' @param input The label to check for square brackets (string input)
#' @return - boolean
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_has_square_brackets("[hello]")
#' umx_has_square_brackets("goodbye")

umx_has_square_brackets <- function (input) {
    match1 <- grep("[", input, fixed = TRUE)
    match2 <- grep("]", input, fixed = TRUE)
    return(length(match1) > 0 && length(match2) > 0)
}


#' umx_string_to_algebra
#'
#' This is useful because it lets you use paste() and rep() to quickly and easily insert values from R variables into the string, then parse the string as an mxAlgebra argument. The use case this time was to include a matrix exponent (that is A %*% A %*% A %*% A...) with a variable exponent. 
#'
#' @param algString a string to turn into an algebra
#' @param name of the returned algebra
#' @param dimnames of the returned algebra
#' @return - \code{\link{mxAlgebra}}
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' alg = umx_string_to_algebra(paste(rep("A", nReps), collapse = " %*% "), name = "test_case")
#' }
umx_string_to_algebra <- function(algString, name = NA, dimnames = NA) {
	eval(substitute(mxAlgebra(tExp, name=name, dimnames=dimnames), list(tExp = parse(text=algString)[[1]])))
}

#' umx_object_as_str
#'
#' Utility to return an object's name as a string
#'
#' @param x an object
#' @return - name as string
#' @export
#' @family Miscellaneous Functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_object_as_str(mtcars) # "mtcars"
umx_object_as_str<- function(x) {
  deparse(substitute(x))
}

#' umxEval
#'
#' Takes an expression as a string, and evaluates it as an expression in model, optionally computing the result.
#' # TODO Currently broken...
#'
#' @param expstring an expression string, i.e, "a + b"
#' @param model an \code{\link{mxModel}} to evaluate in
#' @param compute Whether to compute the result or not (default = FALSE)
#' @param show Whether to show??? (default = FALSE)
#' @return - an openmx algebra (formula)
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' m1 = mxModel("fit",
#'		mxMatrix("Full", nrow = 1, ncol = 1, free = TRUE, values = 1, name = "a"), 
#'		mxMatrix("Full", nrow = 1, ncol = 1, free = TRUE, values = 2, name = "b"), 
#'		mxAlgebra(a %*% b, name = "ab"), 
#'		mxConstraint(ab == 35, name = "maxHours"), 
#'		mxAlgebraObjective(algebra = "ab", numObs= NA, numStats = NA)
#'	)
#' m1 = mxRun(m1)
#' mxEval(list(ab = ab), m1)
umxEval <- function(expstring, model, compute = FALSE, show = FALSE) {
	return(eval(substitute(mxEval(x, model, compute, show), list(x = parse(text=expstring)[[1]]))))
}

#' umx_scale
#'
#' Scale data columns, skipping ordinal
#'
#' @param df a dataframe to scale
#' @param varsToScale (leave blank for all)
#' @param coerce Whether to coerce non-numerics to numeric (Defaults to FALSE)
#' @return - new dataframe with scaled variables
#' @export
#' @family Miscellaneous Utility Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' data(twinData) 
#' df = umx_scale(twinData, varsToScale = NULL)
#' plot(wt1 ~ wt2, data= df)

umx_scale <- function(df, varsToScale = NULL, coerce = FALSE){
	if(!is.data.frame(df)){
		stop(paste0("umx_round takes a dataframe as its first argument. ", quote(df), " isn't a dataframe"))
	}
	# For each column, if numeric, scale
	if(is.null(varsToScale)){
		varsToScale = names(df)
	}
	if(coerce){
		stop("coerce not implemented yet")
	}
	varsToScale = varsToScale[umx_is_numeric(df[,varsToScale])]
	message("Vars I will scale are:", paste(varsToScale, ", "))
	df[ ,varsToScale] = scale(df[ ,varsToScale])
	return(df)
}

umx_is_numeric <- function(df, cols = TRUE){
	if(cols != TRUE){
		stop(paste0("Can't handle anything by columns yet"))
	}
	if(!is.data.frame(df)){
		stop(paste0("First argument should be a dataframe as its first argument. ", quote(df), " isn't a dataframe"))
	}
	colNames = names(df)
	bIsNumeric = rep(F, length(colNames))
	i = 1
	for (n in colNames) {
		bIsNumeric[i] = is.numeric(df[,n])
		i = i + 1
	}
	return(bIsNumeric)
}

#' umx_residualize
#'
#' @description Return one or more variables residualised against covs.
#' @details This is the same as 
#' 
#' \code{tmp <- residuals(lm(var ~ cov1 + cov2, data = data, na.action = na.exclude))}
#'
#' Optionally, this also works on wide (ie., twin) data. Just supply suffixes to identify
#' the paired-wide columns (see examples)
#'
#' @param var The base name of the variable you want to residualize. Alternatively, a 
#' regression \code{\link{formula}} containing var on the lhs, and covs on the rhs
#' @param covs Covariates to residualize on.
#' @param suffixes Suffixes that identify the variable for each twin, i.e. c("_T1", "_T2")
#' Up to you to check all variables are present!
#' @param data The dataframe containing all the variables
#' @return - dataframe with var residualized in place (i.e under its original column name)
#' @export
#' @family Miscellaneous Utility Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' tmp = mtcars
#' # Residualise mpg on cylinders and displacement
#' r1 = umx_residualize("mpg", c("cyl", "disp"), data = tmp)$mpg
#' r2 = residuals(lm(mpg ~ cyl + disp, data = tmp, na.action = na.exclude))
#' all(r1 == r2)
#' # plot(r1~r2)
#' # formula interface
#' r1 = umx_residualize(mpg ~ cyl + I(cyl^2) + disp, data = tmp)$mpg
#' # Same again, but now on wide data (i.e. with family data on each row)
#' tmp$mpg_T1  = tmp$mpg_T2  = tmp$mpg
#' tmp$cyl_T1  = tmp$cyl_T2  = tmp$cyl
#' tmp$disp_T1 = tmp$disp_T2 = tmp$disp
#' umx_residualize("mpg", c("cyl", "disp"), c("_T1", "_T2"), data = tmp)
umx_residualize <- function(var, covs = NULL, suffixes = NULL, data){
	# Check names
	# TODO remove dependency on formula.tools
	if(class(var) == "formula"){
		umx_check(is.null(covs), "stop", "when using formula, leave covs empty")
		require(formula.tools)
		form <- var
		var  = all.vars(lhs(form))
		covs = all.vars(rhs(form))
	}else{
		form = NULL # so we catch this and create it below
	}
	
	if(is.null(suffixes)){
		vars = c(var, covs)
	} else {
		# Wide vars provided: expand names
		vars = umx_paste_names(c(var, covs), suffixes = suffixes)
	}
	umx_check_names(vars, data = data, die = TRUE)
	nVar = length(c(var, covs))

	if(!is.null(suffixes)){
		# Make a long version of the vars we want
		for (i in 1:length(suffixes)) {
			vars = umx_paste_names(c(var, covs), suffixes = suffixes[i])
			if(i == 1){
				tmp = data[,vars]
				names(tmp) = c(var, covs)
			} else {
				tmp2 = data[,vars]
				names(tmp2) = c(var, covs)
				tmp = rbind(tmp, tmp2)
			}
		}
	} else {
		tmp = data[,vars]
	}
	oldNAs = sum(is.na(tmp[,var]))
	# If formula not provided, construct it from var and covs
	if(is.null(form)){
		form = paste0(var, " ~ ", paste(covs, collapse = " + "))
		form = as.formula(form)
	}
	tmp <- residuals(lm(form, data = tmp, na.action = na.exclude))
	newNAs = sum(is.na(tmp))
	if(newNAs > oldNAs){
		message(newNAs - oldNAs, " cases of var ", omxQuotes(var), "lost due to missing covariates")
	}
	if(!is.null(suffixes)){
		i = 1
		nRows = nrow(data)
		for (suff in suffixes) {
			data[, paste0(var, suff)] = tmp[i:(i+nRows-1)]
			i = i + nRows
		}
	} else {
		data[, var] = tmp
	}
	return(data)
}

#' umx_scale_wide_twin_data
#'
#' Scale wide data across all cases: currently twins
#'
#' @param varsToScale the base names of the variables ("weight" etc)
#' @param suffixes the suffix that distinguishes each case (T1, T2 etc.)
#' @param df a wide dataframe
#' @return - new dataframe with scaled variables
#' @export
#' @family Miscellaneous Utility Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' data(twinData) 
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), suffixes = c("1","2") )
#' plot(wt1 ~ wt2, data = df)

umx_scale_wide_twin_data <- function(varsToScale, suffixes, df) {
	if(length(suffixes) != 2){
		stop("I need two suffixes, you gave me ", length(suffixes))
	}
	namesNeeded = c(paste0(varsToScale, suffixes[1]), paste0(varsToScale, suffixes[2]))
	umx_check_names(namesNeeded, df)
	t1Traits = paste0(varsToScale, suffixes[1])
	t2Traits = paste0(varsToScale, suffixes[2])
	for (i in 1:length(varsToScale)) {
		T1 = df[,t1Traits[i]]
		T2 = df[,t2Traits[i]]
		totalMean = mean(c(T1, T2), na.rm = TRUE)
		totalSD   =   sd(c(T1, T2), na.rm = TRUE)
		T1 = (T1 - totalMean)/totalSD
		T2 = (T2 - totalMean)/totalSD
		df[,t1Traits[i] ] = T1
		df[,t2Traits[i] ] = T2
	}
	return(df)
}

#' umx_default_option
#'
#' Handle parameter options given as a default list in a function.
#' This is just a flexible version of match.arg(x)
#'
#' @param x the value chosen (may be a selection, or the default list of options)
#' @param option_list TODO fix this documentation
#' @param check Whether to check that single items are in the list. Set false to accept abbreviations (defaults to TRUE) 
#' @return - the option
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{match.arg}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' option_list = c("default", "par.observed", "empirical")
#' umx_default_option("par.observed", option_list)
#' umx_default_option("bad", option_list)
#' umx_default_option("allow me", option_list, check = FALSE)
#' umx_default_option(option_list, option_list)
#' option_list = c(NULL, "par.observed", "empirical")
#' umx_default_option(option_list, option_list) # fails with NULL!!!!!
#' option_list = c(NA, "par.observed", "empirical")
#' umx_default_option(option_list, option_list) # use NA instead
#' option_list = c(TRUE, FALSE, NA)
#' umx_default_option(option_list, option_list) # works with non character
#' }
umx_default_option <- function(x, option_list, check = TRUE){
	# often the R-built in code will work...
	# filter = match.arg(filter)

	if (identical(x, option_list)) {
	    x = option_list[1]
	}
	if (length(x) != 1){
	    stop(paste("argument must be ONE of ", paste(sQuote(option_list),collapse=", "), "you tried:", paste(sQuote(x), collapse = ", ")))
	}else if (!(x %in% option_list) & check) {
	    stop(paste("argument must be one of ", paste(sQuote(option_list),collapse=", ")))
	} else {
		return(x)
	}
}


#' umx_fake_data
#'
#' This function takes as argument an existing dataset, which 
#' must be either a matrix or a data frame. Each column of the 
#' dataset must consist either of numeric variables or ordered 
#' factors. When one or more ordered factors are included, 
#' then a heterogeneous correlation matrix is computed using 
#' John Fox's polycor package. Pairwise complete observations 
#' are used for all covariances, and the exact pattern of 
#' missing data present in the input is placed in the output,
#' provided a new sample size is not requested. Warnings from
#' the polycor::hetcor function are suppressed.
#'
#' Author:   Ryne Estabrook
#' Created:  17 Aug 2010
#'
#' @param dataset The original dataset you want to make a simulacrum of
#' @param digits = 2
#' @param n = NA
#' @param use.names = T
#' @param use.levels = T
#' @param use.miss = T
#' @param mvt.method = "eigen"
#' @param het.ML = F
#' @param het.suppress = T
#' @return - new dataframe
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @examples
#' fakeCars = umx_fake_data(mtcars)
umx_fake_data <- function(dataset, digits = 2, n = NA, use.names = TRUE, use.levels = TRUE, use.miss = TRUE, mvt.method = "eigen", het.ML = FALSE, het.suppress = TRUE){
  require(mvtnorm)
  require(polycor)
  # requires data frame or matrix
  if((is.data.frame(dataset)+is.matrix(dataset))==0){
    warning("Data must be a data frame or matrix")
  }
  # organization
  row <- dim(dataset)[1] # number of rows
  if(is.na(n))(n <- row) # sets unspecified sample size to num rows
  col <- dim(dataset)[2] # number of columns
  del <- is.na(dataset)  # records position of NAs in dataset
  if(n != row){
    select <- round(runif(n, 0.5, row+.49),0)
    del    <- del[select,]
  }
  num <- rep(NA, col)    # see what's not a factor
  ord <- rep(NA, col)    # see what's an ordered factor

  # which columns are numeric (the others are factors)?
  for (i in 1:col){
    num[i] <- is.numeric(dataset[,i])
    ord[i] <- is.ordered(dataset[,i])
  }

  # check for unordered factors
  location <- !(num|ord)
  unorder  <- sum(location)

  if(unorder>0)warning(
    paste("Unordered factor detected in variable(s):", 
      names(dataset)[location]
    )
  )

  # if everything is numeric, don't invoke polycor
  if(sum(!num) == 0){
    # generate data with rmvnorm
    fake <- mvtnorm::rmvnorm(n, apply(dataset, 2, mean, na.rm = TRUE),
		cov(dataset, use = "pairwise.complete.obs"), mvt.method)

    # round the data to the requested digits
    fake <- round(fake, digits)

    # insert the missing data, if so requested
    if(use.miss == TRUE)(fake[del] <- NA)

    # give the variables names, if so requested
    if(use.names == TRUE)(names(fake) <- names(dataset))

    # return the new data
    return(fake)
  }

  # if there are factors, we start here

  # find the variable means (constrain to zero for factors)
  mixedMeans <- rep(0, col)
  mixedMeans[num] <- apply(dataset[, num], 2, mean, na.rm = TRUE)

  # estimate a heterogeneous correlation matrix
  if (het.suppress == TRUE){
	  suppressWarnings(het <- polycor::hetcor(dataset, ML = het.ML))
  } else {
	  het <- polycor::hetcor(dataset, ML = het.ML)	
  }
  mixedCov <- het$correlations

  # make a diagonal matrix of standard deviations to turn the 
  # correlation matrix into a covariance matrix
  stand <- matrix(0, col, col)
  diag(stand) <- rep(1, col)
  diag(stand)[num] <- apply(dataset[,num], 2, sd, na.rm=TRUE)
  # pre and post multiply hetero cor matrix by diagonal sd matrix
  mixedCov <- stand %*% mixedCov %*% stand

  # generate the data
  fake <- as.data.frame(rmvnorm(row, mixedMeans, mixedCov, mvt.method))

  # insert the missing data, if so requested
  if(use.miss == TRUE)(fake[del] <- NA)

  # turn the required continuous variables into factors
  for (i in (1:col)[!num]){
    # the original data for this column
    old <- dataset[,i]
   
    # the new data for this column, omiting NAs
    new <- fake[!is.na(fake[,i]),i]

    # what are the levels of the original factor?
    lev <- levels(old)

    # establish cutpoints in new variable from cdf of old factor
    cut <- cumsum(table(old))/(sum(!is.na(old)))

    # put continuous variable into a matrix, repeating value across columns
    wide <- matrix(new, length(new), length(lev))

    # put the cutpoints in a matrix, repeating the cut point values across rows
    crit <- matrix(quantile(new, cut), length(new), length(lev), byrow=TRUE)

    # for each value (row of the wide matrix), 
    # how many cutpoints is the value greater than?
    # number of cutpoints surpassed=category
    fake[!is.na(fake[,i]),i] <- apply(wide>crit, 1, sum)

    # make it a factor
    fake[,i] <- factor(fake[,i], ordered=TRUE)

    # give the new factor the same levels as the old variable
    if(length(levels(fake[,i]))!=length(lev))message(
      paste("Fewer categories in simulated variable", 
      names(fake)[i], "than in input variable", names(dataset)[i]))
    if(use.levels==TRUE&(length(levels(fake[,i]))==length(lev))){
      levels(fake[,i]) <- lev} else (levels(fake[,i]) <- 1:length(lev))
  }

  # Round the data to the requested digits
  fake[,num] <- round(fake[,num], digits)

  # Give the variables names, if so requested
  if(use.names==TRUE)(names(fake) <- names(dataset))
  
  # Return the new data
  return(fake)
}

#' qm
#'
#' Quickmatrix function
#'
#' @param ... the components of your matrix
#' @param rowMarker mark the end of each row
#' @return - matrix
#' @family Miscellaneous Utility Functions
#' @references \url{http://www.sumsar.net/blog/2014/03/a-hack-to-create-matrices-in-R-matlab-style}
#' @export
#' @examples
#' # simple example
#' qm(0, 1 |
#'    2, NA)
#' \dontrun{
#' # clever example
#' M1 = M2 = diag(2)
#' qm(M1,c(4,5) | c(1,2),M2 | t(1:3))
#' }
qm <- function(..., rowMarker = "|") {
	# Short hard to read version that allows some of the more advanced Matlab capabilities like Matrices as arguments:
	# turn ... into string
	args<-deparse(substitute(rbind(cbind(...))))
	# create "rbind(cbind(.),cbind(.),.)" construct
	sep = paste0("\\", rowMarker)
	args<-gsub(sep, "), cbind(", args)
	# eval
	eval(parse(text = args))
}

# easier to read variant that doesn't accept matrices as arguments...
# qm <- function(..., colsep = "|") {
# 	# Get the arguments as a list
# 	arg <- eval(substitute(alist(...)))
# 	out <- strsplit(as.character(arg), split = colsep, fixed = TRUE)
# 	ns <- sapply(out, length)
# 	ncol <- if(any(ns > 1)){min(which(ns>1))}else{length(ns)}
# 	matrix(as.numeric(unlist(out)), ncol = ncol, byrow = TRUE)
# }

#  tic()
# 
#  toc()
# 
# tic <- function(gcFirst = TRUE, type=c("elapsed", "user.self", "sys.self")){
#    type <- match.arg(type)
#    assign(".type", type, envir=baseenv())
#    if(gcFirst) gc(FALSE)
#    tic <- proc.time()[type]         
#    assign(".tic", tic, envir=baseenv())
#    invisible(tic)
# }
# 
# toc <- function(){
#    type <- get(".type", envir=baseenv())
#    toc <- proc.time()[type]
#    tic <- get(".tic", envir=baseenv())
#    print(toc - tic)
#    invisible(toc)
# }
# 
# library(rbenchmark)
# # Example 1
# # Benchmarking the allocation of one 10^6-element numeric vector,
# # by default replicated 100 times
# benchmark(1:10^6)
# # simple test functions used in subsequent examples
# random.array = function(rows, cols, dist=rnorm)
# array(dist(rows*cols), c(rows, cols))
# random.replicate = function(rows, cols, dist=rnorm)
# replicate(cols, dist(rows))
# 
# library("microbenchmark")
# library("ggplot2")
# tm <- microbenchmark(
# 	rchisq(100, 0),
# 	rchisq(100, 1),
# 	rchisq(100, 2),
# 	rchisq(100, 3),
# 	rchisq(100, 5), times=1000
# )
# boxplot(tm)
# autoplot(tm)
# summary(tm)
# tm <- microbenchmark(1:10^6); autoplot(tm)

# ====================
# = php type helpers =
# ====================
#' umx_explode - like php's explode function
#'
#' Takes a string and returns each character as an item in an array
#'
#' @param delimiter what to break the string on. Default is empty string ""
#' @param string an character string, e.g. "dog"
#' @return - a collection of characters, e.g. c("d", "o", "g")
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://www.php.net/}
#' @examples
#' umx_explode("", "dog") # "d" "o" "g"
#' umx_explode(" ", "cats and dogs") # [1] "cats" "and"  "dogs"
umx_explode <- function(delimiter = character(), string) { 
	strsplit(string, split = delimiter)[[1]] 
}

#' umx_names
#'
#' Convenient equivalent of grep("fa[rl].*", names(df), value=T, ignore.case=T)
#'
#' @param df dataframe to get names from
#' @param pattern = "find.*"
#' @param ignore.case default = TRUE (opposite default to grep)
#' @param perl = FALSE
#' @param value = default = TRUE (opposite default to grep)
#' @param fixed = FALSE
#' @param useBytes = FALSE
#' @param invert = FALSE
#' @return - vector of matches
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' umx_names(mtcars, "mpg") #"mpg" "cyl" "disp" "hp" "drat" "wt" "qsec" "vs" "am" "gear" "carb"
#' umx_names(mtcars, "^d") # "disp", drat
#' umx_names(mtcars, "r[ab]") # "drat", "carb"
umx_names <- function(df, pattern = ".*", ignore.case = TRUE, perl = FALSE, value = TRUE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
	nameVector = names(df)
	if(is.null(nameVector)){
		stop(paste0("umx_names requires a dataframe or something else with names(), ", umx_object_as_str(df), " is a ", typeof(df)))
	}
	grep(pattern = pattern, x = names(df), ignore.case = ignore.case, perl = perl, value = value,
	     fixed = fixed, useBytes = useBytes, invert = invert)
}

#' umx_trim
#'
#' returns string w/o leading or trailing whitespace
#'
#' @param string to trim
#' @return - string
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_trim(" dog") # "dog"
#' umx_trim("dog ") # "dog"
#' umx_trim("\t dog \n") # "dog"

umx_trim <- function(string) {
	# http://www.php.net/manual/en/function.trim.php
	return(gsub("^\\s+|\\s+$", "", string))
	# returns string w/o leading whitespace
	 # trim.leading <- function (x)  sub("^\\s+", "", x)

	# returns string w/o trailing whitespace
	 # sub("\\s+$", "", x)
}
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");


#' umx_rot
#'
#' rotate a vector (default, rotate by 1)
#'
#' @param vec vector to rotate
#' @return - \code{\link{mxModel}}
#' @export
#' @family Miscellaneous Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_rot(1:10)
#' umx_rot(c(3,4,5,6,7))
#' # [1] 4 5 6 7 3
umx_rot <- function(vec){
	ind = (1:length(vec) %% length(vec)) + 1
	vec[ind]
} 

#' demand a package
#'
#' This loads the package, installing it if needed
#'
#' @param package The package name as a string.
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/drknexus/repsych/blob/master/R/glibrary.r}
#' @examples
#' \dontrun{
#' demand("numDeriv")
#' }
demand <- function(package) {
	if(FALSE == package %in% rownames(installed.packages() ) ) {
		m <- getCRANmirrors(all = FALSE, local.only = FALSE)
		URL <- m[grepl("Cloud",m$Name),"URL"][1] # get the first repos with "cloud" in the name
		install.packages(package, repos = URL)
	}
	library(package = package, character.only = TRUE)
}


#' Turn a cov matrix into raw data
#'
#' Turn a cov matrix into raw data :-)
#'
#' @param myCovariance a covariance matrix
#' @param n how many rows of data to return
#' @param means the means of the raw data (defaults to 0)
#' @return - data.frame
#' @export
#' @seealso - \code{\link{cov2cor}}
#' @family Miscellaneous Utility Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}

#' @examples
#' covData <- matrix(nrow=6, ncol=6, byrow=TRUE, dimnames=list(paste0("v", 1:6), paste0("v", 1:6)),
#' data = c(0.9223099, 0.1862938, 0.4374359, 0.8959973, 0.9928430, 0.5320662,
#'            0.1862938, 0.2889364, 0.3927790, 0.3321639, 0.3371594, 0.4476898,
#'            0.4374359, 0.3927790, 1.0069552, 0.6918755, 0.7482155, 0.9013952,
#'            0.8959973, 0.3321639, 0.6918755, 1.8059956, 1.6142005, 0.8040448,
#'            0.9928430, 0.3371594, 0.7482155, 1.6142005, 1.9223567, 0.8777786,
#'            0.5320662, 0.4476898, 0.9013952, 0.8040448, 0.8777786, 1.3997558))
#' myData = umx_cov2raw(covData, n = 100, means = 1:6)
umx_cov2raw <- function(myCovariance, n, means = 0) {
	if(!umx_is_cov(myCovariance, boolean = TRUE)){
		stop("myCovariance must be a covariance matrix")
	}
	if(length(means) == 0){
		means = rep(means, dim(myCovariance)[2])
	} else {
		if(length(means) != dim(myCovariance)[2]){
			stop("means must have the same length as the matrix columns. You gave me ", dim(myCovariance)[2], 
			 " columns of cov matrix, but ", length(means), " means.")
		}
	}
	out = MASS::mvrnorm (n = n, mu = means, Sigma = myCovariance);
	out = data.frame(out);  names(out) <- colnames(myCovariance);
	return(out)
}

# ==========================
# = Data Prep and Cleaning =
# ==========================

#' umx_make_bin_cont_pair_data
#'
#' Takes a dataframe of left-censored variables (vars with a floor effect) and does two things to it:
#' 1. It creates new binary (1/0) copies of each column (with the suffix "bin"). These contain 0 where
#'    the variable is below the minimum and NA otherwise.
#' 2. In each existing variable, it sets all instances of min for that var to NA
#' 
#' @param df a \code{\link{data.frame}} to convert
#' @return - copy of the dataframe with new binary variables and censoring
#' @export
#' @family Miscellaneous Utility Functions
#' @seealso - \code{\link{umxACE}}
#' @references - \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' df = umx_make_bin_cont_pair_data(mtcars, vars = c("mpg"))
#' str(df)
#' df[order(df$mpg), c(1,12)]
#' # Introduce a floor effect
#' tmp = mtcars; tmp$mpg[tmp$mpg<=15]=15
#' tmp$mpg_T1 = tmp$mpg_T2 = tmp$mpg
#' df = umx_make_bin_cont_pair_data(tmp, vars = c("mpg"), suffixes = c("_T1", "_T2"))
#' df[order(df$mpg), 12:15]
umx_make_bin_cont_pair_data <- function(data, vars = NULL, suffixes=NULL){
	if(!is.null(suffixes)){
		umx_check(length(suffixes) < 3, "stop", "suffixes must have length == 2")
		longVars = umx_paste_names(vars, suffixes = suffixes)
	}else{
		longVars = vars
	}
	umx_check_names(longVars, data = data, die = TRUE)
	if(!is.null(suffixes)){
		# Get minimum scores from a long version of the vars
		for (i in 1:length(suffixes)) {
			vars_Ti = umx_paste_names(vars, suffixes = suffixes[i])
			if(i == 1){
				tmp = data[, vars_Ti, drop = FALSE]
				names(tmp) = vars
			} else {
				tmp2 = data[, vars_Ti, drop = FALSE]
				names(tmp2) = vars
				tmp = rbind(tmp, tmp2)
			}
		}
		listOfMins = umx_apply(min, tmp, by = "columns", na.rm = TRUE)
	} else {
		listOfMins = umx_apply(min, data[, vars, drop = FALSE], by = "columns", na.rm = TRUE)
	}
	# blank suffix to make this work when there is none
	if(is.null(suffixes)){ suffixes = ""}
	var_i = 1
	for (var in vars) {
		for (thisSuffix in suffixes) {
			thisVarName = paste0(var, thisSuffix)
			thisBinName = paste0(var, "bin", thisSuffix)
			data[,thisBinName] = (data[, thisVarName] <= listOfMins[var_i])
			data[,thisBinName] = mxFactor(data[, thisBinName], c(TRUE, FALSE), c("low", "high"))

			# Set NA if FALSE
			lowScores = data[,thisBinName] == "low"
			data[lowScores , thisVarName] = NA
			data[!lowScores, thisBinName] = NA
		}
		var_i = var_i + 1
	}
	return(data)
}

#' umxHetCor
#'
#' umxHetCor Helper to return just the correlations from John Fox's polycor::hetcor function
#'
#' @param data A \code{\link{data.frame}} of columns for which to compute heterochoric correlations
#' @param ML Whether to use Maximum likelihood computation of correlations (default = FALSE)
#' @param use How to delete missing data: "complete.obs", "pairwise.complete.obs" Default is pairwise.complete.obs
#' @param treatAllAsFactor Whether to treat all columns as factors, whether they are currently or not.
#' @param verbose How much to tell the user about what was done.
#' @return - A matrix of correlations
#' @family Miscellaneous Utility Functions
#' @export
#' @seealso - \code{\link[polycor]{hetcor}}
#' @references - 
#' @examples
#' \dontrun{
#' cor_df = umxHetCor(df)
#' cor_df = umxHetCor(df, ML = FALSE, use="pairwise.complete.obs")
#' }

umxHetCor <- function(data, ML = FALSE, use = "pairwise.complete.obs", treatAllAsFactor=F, verbose= FALSE){
	if(treatAllAsFactor){
		n = ncol(data)
		for (i in 1:n) {
			data[,i] = factor(data[,i])
		}
	}
	if(require(polycor)){
		hetc = polycor::hetcor(data, ML = ML, use = use, std.err = FALSE)
		if(verbose){
			print(hetc)
		}
		return(hetc$correlations)
	} else {
		# TODO add error message if polycor not found
		stop("To run umxHetCor, you must install the polycor package\ninstall.packages('polycor')")
	}
}

#' umx_lower2full
#'
#' Take a lower triangle of data (either from a "lower" \code{\link{mxMatrix}}, or as you might typed in a journal article) 
#' and turn it into a full matrix.
#' 
#' @param lower.data An \code{\link{mxMatrix}}
#' @param diag A boolean noting whether the lower matrix includes the diagonal
#' @param byrow Whether the matrix is to be filled by row or by column
#' @return - \code{\link{mxMatrix}}
#' @family Miscellaneous Utility Functions
#' 
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' 
#' tmpn = c("ROccAsp", "REdAsp", "FOccAsp", "FEdAsp", "RParAsp", 
#'          "RIQ", "RSES", "FSES", "FIQ", "FParAsp")
#' tmp = matrix(nrow = 10, ncol = 10, byrow = TRUE, dimnames = list(tmpn,tmpn), data = 
#' 	c(1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.6247, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.3269, 0.3669, 1.0000, 0.0000, 0.0000, 0.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.4216, 0.3275, 0.6404, 1.0000, 0.0000, 0.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.2137, 0.2742, 0.1124, 0.0839, 1.0000, 0.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.4105, 0.4043, 0.2903, 0.2598, 0.1839, 1.0000, 0.0000,  0.0000, 0.0000, 0,
#' 	0.3240, 0.4047, 0.3054, 0.2786, 0.0489, 0.2220, 1.0000,  0.0000, 0.0000, 0,
#' 	0.2930, 0.2407, 0.4105, 0.3607, 0.0186, 0.1861, 0.2707,  1.0000, 0.0000, 0,
#' 	0.2995, 0.2863, 0.5191, 0.5007, 0.0782, 0.3355, 0.2302,  0.2950, 1.0000, 0,
#' 	0.0760, 0.0702, 0.2784, 0.1988, 0.1147, 0.1021, 0.0931, -0.0438, 0.2087, 1)
#' )
#' umx_lower2full(tmp)
#' tmp = c(
#' 	c(1.0000, 
#' 	0.6247, 1.0000,
#' 	0.3269, 0.3669, 1.0000,
#' 	0.4216, 0.3275, 0.6404, 1.0000,
#' 	0.2137, 0.2742, 0.1124, 0.0839, 1.0000,
#' 	0.4105, 0.4043, 0.2903, 0.2598, 0.1839, 1.0000,
#' 	0.3240, 0.4047, 0.3054, 0.2786, 0.0489, 0.2220, 1.0000,
#' 	0.2930, 0.2407, 0.4105, 0.3607, 0.0186, 0.1861, 0.2707,  1.0000,
#' 	0.2995, 0.2863, 0.5191, 0.5007, 0.0782, 0.3355, 0.2302,  0.2950, 1.0000,
#' 	0.0760, 0.0702, 0.2784, 0.1988, 0.1147, 0.1021, 0.0931, -0.0438, 0.2087, 1)
#' )
#' umx_lower2full(tmp)
#' tmp = c(
#' 	c(0.6247,
#' 	0.3269, 0.3669,
#' 	0.4216, 0.3275, 0.6404,
#' 	0.2137, 0.2742, 0.1124, 0.0839,
#' 	0.4105, 0.4043, 0.2903, 0.2598, 0.1839,
#' 	0.3240, 0.4047, 0.3054, 0.2786, 0.0489, 0.2220,
#' 	0.2930, 0.2407, 0.4105, 0.3607, 0.0186, 0.1861, 0.2707, 
#' 	0.2995, 0.2863, 0.5191, 0.5007, 0.0782, 0.3355, 0.2302,  0.2950,
#' 	0.0760, 0.0702, 0.2784, 0.1988, 0.1147, 0.1021, 0.0931, -0.0438, 0.2087)
#' )
#' umx_lower2full(tmp, diag = FALSE)
umx_lower2full <- function(lower.data, diag = FALSE, byrow = TRUE) {
	if(is.matrix(lower.data)){
		# Copy the transpose of the lower triangle to the
		# upper triangle
		x = lower.data
		x[upper.tri(x)] <- t(x)[upper.tri(x)]
		return(x)
	} else {
		len = length(lower.data)
		if(diag) {
			# len*2 = ((x+.5)^2)-.25
			size = len * 2
			size = size + .25
			size = sqrt(size)
			size = size - .5; size
		}else{
			# len = (x*((x+1)/2))-x	
			# .5*(x-1)*x
			size = len * 2
			# (x-.5)^2 - .25
			size= size + .25
			size = sqrt(size)
			size = size + .5; size
		}
		mat = diag(size)
		if(byrow){
			# put  data into upper triangle, then transform to lower
			mat[upper.tri(mat, diag = diag)] <- lower.data;
			mat[lower.tri(mat, diag = FALSE)] <- mat[upper.tri(mat, diag = FALSE)]
		}else{
			mat[lower.tri(mat, diag = diag)] <- lower.data;
			mat[upper.tri(mat, diag = FALSE)] <-mat[lower.tri(mat, diag = FALSE)]
		}
		return(mat)
	}
}
#' umxPadAndPruneForDefVars
#'
#' Replaces NAs in definition slots with the mean for that variable ONLY where all data are missing for that twin
#'
#' @param df the dataframe to process
#' @param varNames list of names of the variables being analysed
#' @param defNames list of covariates
#' @param suffixes suffixes that map names on columns in df (i.e., c("T1", "T2"))
#' @param highDefValue What to replace missing definition variables (covariates) with. Default = 99
#' @param rm = how to handle missing values in the varNames. Default is "drop_missing_def", "pad_with_mean")
#' @return - dataframes
#' @export
#' @family Miscellaneous Utility Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' \dontrun{
#' df = umxPadAndPruneForDefVars(df, "E", "age", c("_T1", "_T2"))
#' }
umxPadAndPruneForDefVars <- function(df, varNames, defNames, suffixes, highDefValue = 99, rm = c("drop_missing_def", "pad_with_mean")) {
	# df = twinData
	# varNames = varNames
	# defNames = covNames
	# suffixes = suffixes
	# highDefValue = -100000
	# rm = "pad_with_mean"

	numTwinsPerFamily = length(suffixes)
	message("Working with ", numTwinsPerFamily, " twins per family:", paste(suffixes, collapse = ", "))
	message("Checking varnames: ", paste(varNames, collapse = ", "))
	# get mean values for each definition Variable
	meanDefVarValues = colMeans(df[, paste0(defNames, suffixes[1]), drop=F], na.rm = TRUE)
	numRows = dim(df)[1]

	for (i in 1:numTwinsPerFamily) {
		# i = 1
		# for twin i
		defVars = paste0(defNames, suffixes[i])
		defData = df[, defVars, drop = F]
		Vars    = paste0(varNames, suffixes[i])
		varData = df[, Vars, drop = F]
		allDataMissing = rep(FALSE, numRows)
		missingDefVars = rep(FALSE, numRows)
		for (n in 1:numRows) {
			# n = 1
			allDataMissing[n] = all(is.na(varData[n,]))
			defsMissing = is.na(defData[n,])
			missingDefVars[n] = any(defsMissing)
			if(allDataMissing[n]){
				if(missingDefVars[n]){
					df[n, defVars] = highDefValue
				}
			} else {
				if(missingDefVars[n]){
					df[n, defVars[defsMissing]] = meanDefVarValues[defsMissing]
				}
			}
		}
		message(numRows, " families found")
		message(sum(allDataMissing), " missing all DVs", " for twin ", i, " (", sum(!allDataMissing), " had at least one datapoint).")
		message("Of these, ", sum(allDataMissing & missingDefVars), " were NA for at least one definition variable and for these subjects, all definition vars were set to highDefValue (", highDefValue, ")")
		message(sum(!allDataMissing & missingDefVars), " were NA for at least one definition variable but had some measured data.\n")
		message(" for these subjects, definition vars were set to the mean for the dataset... not perfect but likely adequate response.")
		warning("I am not yet checking for ordinal vars etc.")
	}
	return(df)
}
# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");

#' get mat[r,c] style cell address from an mxMatrix
#'
#' Sometimes you want these :-) This also allows you to change the matrix name: useful for using mxMatrix addresses in an mxAlgebra.
#'
#' @param mat an mxMatrix to get address labels from
#' @param free how to filter on free (default = NA: take all)
#' @param newName = NA
#' @return - a list of bracket style labels
#' @export
#' @family Miscellaneous Functions
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_get_bracket_addresses(m1$matrices$A, free= TRUE)
# "stdA[1,6]" "stdA[2,6]" "stdA[3,6]" "stdA[4,6]" "stdA[5,6]"
umx_get_bracket_addresses <- function(mat, free = NA, newName = NA) {
	# c("stdS[6,7]", "stdS[7,7]")
	if(is.na(newName)){
		matName = mat$name
	} else {
		matName = newName
	}
	rows <- nrow(mat@free)
	cols <- ncol(mat@free)
	d1 <- expand.grid(matName, "[", 1:rows, ",", 1:cols, "]", stringsAsFactors = FALSE)	
	addys = c()
	for (i in 1:(rows*cols)) {
		addys = c(addys, paste(d1[i,], collapse = ""))
	}
	addys = matrix(addys, rows,cols)
	if(is.na(free) ){
		return(addys)
	} else if (free == TRUE){
		return(addys[mat@free == TRUE])
	} else if (free == FALSE){
		return(addys[mat@free == TRUE])
	} else {
		stop("free must be one of NA TRUE or FALSE")	
	}
}


#' umx_show
#'
#' Show matrix contents. The user can select  values, free, and/or labels, and which matrices to display
#'
#' @param model an \code{\link{mxModel}} to show data from
#' @param what  legal options are "values" (default), "free", or "labels")
#' @param matrices to show  (default is c("S", "A"))
#' @param digits precision to report, defaults to rounding to 2 decimal places
#' @return - \code{\link{mxModel}}
#' @export
#' @family Reporting Functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{http://tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
#' umx_show(m1)
#' umx_show(m1, digits = 3)
#' umx_show(m1, matrices = "S")
#' umx_show(m1, what = "free")
#' umx_show(m1, what = "labels")
#' umx_show(m1, what = "free", "A")
umx_show <- function(model, what = c("values", "free", "labels", "nonzero_or_free"), matrices = c("S", "A"), digits = 2) {
	if(!umx_is_RAM(model)){
		stop("Only RAM models by default: what would you like me to do with this type of model?")
	}
	what = match.arg(what)
	for (w in matrices) {
		message("Showing ", what, " for:", w, " matrix:")
		if(what == "values"){
			umx_print(data.frame(model$matrices[[w]]$values), zero.print = ".", digits = digits)		
		}else if(what == "free"){
			umx_print(data.frame(model$matrices[[w]]$free) , zero.print = ".", digits = digits)
		}else if(what == "labels"){
			umx_print(data.frame(model$matrices[[w]]$labels) , zero.print = ".", digits = digits)
		}else if(what == "nonzero_or_free"){
			message("99 means the value is fixed, but is non-zero")
			values = model$matrices[[w]]$values
			Free   = model$matrices[[w]]$free
			values[!Free & values !=0] = 99
			umx_print(data.frame(values) , zero.print = ".", digits = digits)
		}
	}
}

# Poems you should know by heart
# https://en.wikipedia.org/wiki/O_Captain!_My_Captain!
# https://en.wikipedia.org/wiki/The_Second_Coming_(poem)
# https://en.wikipedia.org/wiki/Invictus
# http://www.poetryfoundation.org/poem/173698get