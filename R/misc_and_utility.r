# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# devtools::document("~/bin/umx.twin"); devtools::install("~/bin/umx.twin"); 

# setwd("~/bin/umx"); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()
# utility naming convention: "umx_" prefix, lowercase, and "_" not camel case for word boundaries
# so umx_swap_a_block()

# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools


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
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' umx_set_optimizer(opt = "CSOLNP")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[,manifests], type="raw")
#' )
#' oldOpt = umx_get_optimizer()
#' umx_get_optimiser(m1)
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
#' @param model an (optional) model to set. If left NULL, the global option is updated.
#' @return - \code{\link{mxModel}} (if you provided one in x)
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[, manifests], type = "raw")
#' )
#' umx_set_optimiser("NPSOL") # set globally
#' m1 = umx_set_optimiser(m1, opt = "NPSOL")
#' m1 = mxRun(m1)
#' \dontrun{
#' m1@@runstate$compute$steps[1][[1]]$engine # NPSOL
#' }
umx_set_optimizer <- function(opt = c("NPSOL","NLOPT","CSOLNP"), model = NULL) {
	opt = umx_default_option(opt, c("NPSOL","NLOPT","CSOLNP"))
	mxOption(model, "Default optimizer", opt)
	if(opt == "NPSOL"){
		mxOption(model, 'mvnAbsEps', 1.e-6) # default is .001
		mxOption(model, 'mvnMaxPointsC', 5e+5) # default is 5000
	}
}

#' umx_set_cores
#'
#' set the number of cores (threads) used by OpenMx
#'
#' @param cores number of cores to use (defaults to max - 1 to preserve UI responsiveness)
#' @param model an (optional) model to set. If left NULL, the global option is updated.
#' @return - NULL
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[, manifests], type = "raw")
#' )
#' oldCores = umx_get_cores() # get global value
#' umx_set_cores(model = m1) # set to default (max - 1)
#' umx_get_cores() # show new value
#' umx_set_cores(omxDetectCores()) # set to default (max - 1)
#' umx_get_cores() # show new value
#' umx_set_cores(oldCores) $ reset to old value
umx_set_cores <- function(cores = omxDetectCores() - 1, model = NULL) {
	mxOption(model, "Number of Threads", cores)
}

#' umx_get_cores
#'
#' get the number of cores (threads) used by OpenMx
#'
#' @param model an (optional) model to get from. If left NULL, the global option is returned
#' @return - number of cores
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' library(OpenMx)
#' manifests = c("mpg", "disp", "gear")
#' m1 <- mxModel("ind", type = "RAM",
#' 	manifestVars = manifests,
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(mtcars[, manifests], type = "raw")
#' )
#' oldCores = umx_get_cores() # get current default value
#' umx_set_cores(model = m1) # set to default (max - 1)
#' umx_get_cores(model = m1) # show new value
#' umx_set_cores(omxDetectCores()) # set to default (max - 1)
#' umx_get_cores() # show new value
#' umx_set_cores(oldCores) $ reset to old value
umx_get_cores <- function(cores = omxDetectCores() - 1, model = NULL) {
	mxOption(model, "Number of Threads")
}

#' umx_set_checkpointing
#'
#' Set the checkpoint status for a model or global options
#'
#' @param always defaults to "Yes"
#' @param count default 1
#' @param units default "evaluations"
#' @param model an optional model to get options from (default = NULL)
#' @return - NULL
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_set_checkpointing()
umx_set_checkpointing <- function(always = c("Yes", "No"), count = 1, units = "evaluations", model = NULL) {
	legalAge = c("Yes", "No")
	always = umx_default_option(always, legalAge)
	mxOption(model, "Always Checkpoint", always)
	mxOption(model, "Checkpoint Count" , count)
	mxOption(model, "Checkpoint Units" , units)	
}

#' umx_get_checkpointing
#'
#' get the checkpoint status for a model or global options
#'
#' @param model an optional model to get options from
#' @return - NULL
#' @export
#' @family umx core functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_get_checkpointing(model)
umx_get_checkpointing <- function(model = NULL) {
	message("Always Checkpoint: ", mxOption(model, "Always Checkpoint") )
	message("Checkpoint  Count: ", mxOption(model, "Checkpoint Count" ) )
	message("Checkpoint  Units: ", mxOption(model, "Checkpoint Units" ) )
}


#' umx_update_OpenMx
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  A Boolean determining whether to request the beta (TRUE) or relase version (defaults to FALSE)
#' @param loadNew A Boolean parameter determining whether to load the library after (optionally) updating
#' @param anyOK The minimum version to accept without updating
#' @export
#' @examples
#' \dontrun{
#' umx_update_OpenMx()
#' }

umx_update_OpenMx <- function(bleedingEdge = F, loadNew = T, anyOK = F) {
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
#' umx_get_time
#'
#' A function to compactly report how long a model took to execute
#'
#' @param model An \code{\link{mxModel}} from which to get the elapsed time
#' @param formatStr A format string, defining how to show the time
#' @param tz The time zone in which the model was executed
#' @export
#' @seealso - \code{\link{summary}}, \code{\link{umxRun}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @family umx reporting functions
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' umx_get_time(m1)

umx_get_time <- function(model, formatStr= "%H hours, %M minutes, and %OS3 seconds", tz = "GMT"){
	lastTime = ""
	if(length(model)>1){
		for(i in 1:length(model)) {
			m = model[[i]]
			message(format(.POSIXct(m$output$wallTime,tz), paste0(m$name, ": ", formatStr)))
		}
	} else {
		format(.POSIXct(model$output$wallTime,tz), formatStr)
	}
}
umx_report_time <- umx_get_time

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
#' @family umx misc reporting functions
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
		R2HTML::HTML(x, file = file, Border = 0, append = F, sortableDF=T); 
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
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor",
#' 	mxPath(from = "g", to = names(demoOneFactor)),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_is_exogenous(model, manifests_only = TRUE)
#' umx_is_exogenous(model, manifests_only = FALSE)
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
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor",
#' 	mxPath(from = "g", to = names(demoOneFactor)),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_is_endogenous(model, manifests_only = TRUE)
#' umx_is_endogenous(model, manifests_only = FALSE)
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
		if(any(model@matrices$A@free[i, ])){
			endog[n] = i
			n = n + 1
		}
	}
	return(endog)
}

#' umx_add_variances
#'
#' Convenience function to save the user specifying mxpaths adding variance to each variable
#'
#' @param model an \code{\link{mxModel}} to add variances to
#' @param add.to = List of variables to create variance for
#' @param free = List of variables to create variance for (default = NULL)
#' @param values = List of values (default = NULL)
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
#' m1 = umx_add_variances(m1)
#' umx_show(m1, matrices = "S") 
#' # Note: latent g has been treated like the manifests...
#' # umxFixLatents() will take care of this for you...
#' m1 = umxRun(m1, setLabels = T, setValues = T)
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
#' @family umx build functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
#' @family umx build functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
	a = mxPath(from = correlatedManifests, arrows = 2, free = T, values = 1)
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
#' @param to_each What to apply the function to: columns or rows (default = "column")
#' @param of_DF the dataframe to work with
#' @param ... optional arguments to FUN, i.e., na.rm = T
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx core functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_apply(mean, to_each = "column", of_DF = mtcars, na.rm=T)
umx_apply <- function(FUN, to_each = "column", of_DF, ...) {
	# umx_apply(nlevels, to_each = "column", of_DF = myDF)
	if(! (to_each %in% c("column", "row"))){
		stop(paste("fromEach must be either 'column' or 'row', you gave me", to_each))
	} else if (to_each == "row") {
		to_each = 1
	} else {
		to_each = 2		
	}
	apply(of_DF, to_each, FUN, ...)

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
	CIlist   = umxGetParameters(model ,regex= "[ace][0-9]", verbose=F)
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
#' It uses \code{\link{umx_is_ordinal}} and \code{\link{umxMakeThresholdMatrix}} as helpers
#'
#' @param df Dataframe to make a threshold matrix for
#' @param deviationBased whether to use the deviation system to ensure order thresholds (default = T)
#' @param droplevels whether to also drop unused levels (default = T)
#' @param verbose whether to say what the function is doing (default = F)
#' @return - \code{\link{mxModel}}
#' @export
#' @family advanced umx helpers
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' model = umx_RAM_ordinal_objective(model)
#' }
umx_RAM_ordinal_objective <- function(df, deviationBased = TRUE, droplevels = TRUE, verbose = FALSE) {
	# TODO: means = zero & VAR = 1 for ordinal variables
	# (This is a nice place to check, as we have the df present...)
	if(!any(umx_is_ordinal(df))){
		stop("No ordinal variables in dataframe: no need to call umx_RAM_ordinal_objective")
	} 
	pt1 = umxPath(means = umx_is_ordinal(df, names = T), fixedAt = 0)
	pt2 = umxPath(var   = umx_is_ordinal(df, names = T), fixedAt = 1)
	return(list(pt1, pt2, umxMakeThresholdMatrix(df, deviationBased = T, droplevels = T, verbose = F)))
}

#' umx_RAM_thresh_Matrix
#'
#' The purpose of this function is to generate an mxRAMObjective. It is used by \code{\link{umx_RAM_ordinal_objective}}.
#' You likely want that, not this.
#'
#' @param df Dataframe for which to make a threshold matrix.
#' @param deviationBased whether to use the deviation system to ensure order thresholds (default = TRUE)
#' @param droplevels whether to also drop unused levels (default = TRUE)
#' @param verbose whether to say what the function is doing (default = FALSE)
#' @return - \code{\link{mxModel}}
#' @family advanced umx helpers
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_RAM_thresh_Matrix(mtcars, verbose = T)
umx_RAM_thresh_Matrix <- function(df, deviationBased = T, droplevels = F, verbose = F) {
	# mxRAMObjective(A = "A", S="S", F="F", M="M", thresholds = "thresh"), mxData(df, type="raw")
	# use case:  
	# TODO: Let the user know if there are any levels dropped...
	if(droplevels){
		df = droplevels(df)
	}
	if(deviationBased){
		return(xmuMakeDeviationThresholdsMatrices(df, droplevels, verbose))
	} else {
		return(xmuMakeThresholdsMatrices(df, droplevels, verbose))
	}
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
#' @family umx utility functions
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
#' @family umx utility functions
#' @return - 
#' @export
#' @seealso - \code{\link{grep}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' umx_rename_file(baseFolder = "~/Downloads/", findStr = "", replaceStr = "", test = T)
#' umx_rename_file("[Ss]eason +([0-9]+)", replaceStr="S\1", baseFolder = "Finder", test = T)
#' }
umx_rename_file <- function(findStr = NA, replaceStr = NA, baseFolder = "Finder", listPattern = NA, test = T, overwrite = F) {
	# uppercase = u$1
	if(baseFolder == "Finder"){
		baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
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
			fnew = gsub(findStr, replace = replaceStr, fn) # replace all instances
			if(test){
				message("would change ", fn, " to ", fnew)	
			} else {
				if((!overwrite) & file.exists(paste(baseFolder, fnew, sep = ""))){
					message("renaming ", fn, "to", fnew, "failed as already exists. To overwrite set T")
				} else {
					file.rename(paste(baseFolder, fn, sep = ""), paste(baseFolder, fnew, sep = ""))
					changed = changed + 1;
				}
			}
		}else{
			if(test){
				# message(paste("bad file",fn))
			}
		}
	}
	message("changed ", changed)
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
#' @family umx utility functions
#' @export
#' @seealso - \code{\link{umx_rename_file}}, \code{\link{file.rename}}
#' @examples
#' \dontrun{
#' base = "/Users/tim/Music/iTunes/iTunes Music/"
#' dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
#' umx_move_file(baseFolder = base, fileNameList = toMove, destFolder = dest, test=F)
#' }
umx_move_file <- function(baseFolder = NA, findStr = NULL, fileNameList = NA, destFolder = NA, test = T, overwrite = F) {
	# TODO implement findStr
	if(!is.null(findStr)){
		stop("Have not implemented findStr yet")
	}

	if(is.na(destFolder)){
		stop("destFolder can't be NA")
	}
	if(baseFolder == "Finder"){
		baseFolder = system(intern = T, "osascript -e 'tell application \"Finder\" to get the POSIX path of (target of front window as alias)'")
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
#' @family umx data helpers
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
	R[above] <- pf(Fstat, 1, df, lower.tail = F)
	R[below] = round(R[below], digits)
	R[above] = round(R[above], digits)
	# R[above] = paste("p=",round(R[above], digits))
	message("lower tri  = correlation; upper tri = p-value")
	return(R)
}

# Return the maximum value in a row
rowMax <- function(df, na.rm = T) {
	tmp = apply(df, MARGIN = 1, FUN = max, na.rm = na.rm)
	tmp[!is.finite(tmp)] = NA
	return(tmp)
}

rowMin <- function(df, na.rm=T) {
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
#' @family umx utility functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' df = mtcars
#' df$mpg = c(letters,letters[1:6]); str(df)
#' df = umx_as_numeric(df)
umx_as_numeric <- function(df, force = FALSE) {
	# TODO handle case of not being a data.frame...
	if(force){
		colsToConvert = names(df)
	} else {
		colsToConvert = umx_is_numeric(df)
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
#' @family umx utility functions
#' @export
#' @seealso - \code{\link{subset}}
#' @examples
#' test = data.frame(a=paste("a",1:10,sep=""),b=paste("b",1:10,sep=""), c=paste("c",1:10,sep=""),d=paste("d",1:10,sep=""),stringsAsFactors=F)
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
#' @param x the dataframe in which to rename variables
#' @param replace a named collection of c(oldName = "newName") pairs (OR, if using old, the list of new names)
#' @param old Optional list of old names that will be replaced by the contents of replace. defaults to NULL in which case replace must be paired list.
#' @return - dataframe with columns renamed.
#' @export
#' @family umx misc functions
#' @examples
#' # Re-name "cyl" to "cylinder"
#' x = mtcars
#' x = umx_rename(x, replace = c(cyl = "cylinder"))
#' # alternate style
#' x = umx_rename(x, old = c("disp"), replace = c("displacement"))
#' umx_check_names("displacement", data = x, die = T)
#' # This will warn that "disp" doesn't exist (anymore)
#' x = umx_rename(x, old = c("disp"), replace = c("displacement"))
umx_rename <- function (x, replace, old = NULL) {
	# See also gdate::rename.vars(data, from, to)	
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
	         	paste(new_names_to_try[duplicated(new_names_to_try)], collapse=", "))
	  stop(err)
	}
	new_names <- new_names_to_try[match(old_names, names_to_replace)]  
	setNames(x, ifelse(is.na(new_names), old_names, new_names))
}

#' umx_grep
#'
#' search the labels of an SPSS file
#'
#' @param df an \code{\link{data.frame}} to search the labels of
#' @param grepString the search string
#' @param output the column name, the label, or both (default)
#' @param ignore.case whether to be case sensitive or not (default TRUE)
#' @param useNames whether to search the names as well as the labels
#' @return - list of matched column name and labels
#' @family umx utility functions
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' umx_grep(mtcars, "hp", output="both", ignore.case=T)
#' umx_grep(mtcars, "^h.*", output="both", ignore.case=T)
#' \dontrun{
#' umx_grep(spss_df, "labeltext", output = "label") 
#' umx_grep(spss_df, "labeltext", output = "name") 
#' }
umx_grep <- function(df, grepString, output="both", ignore.case=T, useNames=F) {
	# output = "both", "label" or "name"
	# if(length(grepString > 1)){
	# 	for (i in grepString) {
	# 		umx_grep_labels(df, i, output=output, ignore.case=ignore.case, useNames=useNames)
	# 	}
	# } else {
		# need to check this exists
		vLabels = attr(df,"variable.labels") # list of descriptive labels?
		a       = names(df) 
		if(is.null(vLabels)){
			# message("No labels found")
			return(grep(grepString, names(df), value=T, ignore.case=T))
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
	# }
}


# =====================
# = Utility functions =
# =====================

#' umxMsg
#'
#' helper function to make dumping  message("thing has the value", thing's value) easy
#'
#' @param  x the thing you want to print
#' @return - NULL
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' a = "brian"
#' umxMsg(a)
#' a = c("brian", "sally", "jane")
#' umxMsg(a)

umxMsg <- function(x) {
    nm <-deparse(substitute(x))	
	if(length(x) > 1){
		message(nm, " = ", omxQuotes(x))	
	} else {
		message(nm, " = ", x)	
	}
}


#' umx_paste_names
#'
#' Helper to add suffixes to names: useful for expanding twin vars like "bm1" into c("bmi1", "bmi2")
#' Use textConstant to turning "E" into "E_T1", by adding "_T" and 1.
#'
#' @param varNames a list of base names, e.g c("bmi", "HRV")
#' @param textConstant The suffix that denotes "twin" (defaults to "_T")
#' @param suffixes a list of suffixes (e.g c("_T1", "_T2"))
#' @return - vector of names, i.e., c("a_T1","b_T1", "a_T2","b_T2")
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' umx_paste_names("bmi", c("_T1", "_T2"))
#' varNames = umx_paste_names(c("N", "E", "O", "A", "C"), "_T", 1:2)

umx_paste_names <- function(varNames, textConstant = "", suffixes) {
	nameList = c()
	for (ID in suffixes) {
		nameList = c(nameList, paste0(varNames, textConstant, ID))
	}
	return(nameList)
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
#' @family umx utility functions
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' head(umx_round(mtcars, coerce = F))
#' head(umx_round(mtcars, coerce = T))

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
#' @family umx misc reporting functions
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
					", p = ", umx_APA_pval(pf(value, numdf, dendf, lower.tail = F))
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
#' @family umx misc stats functions
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
#' @family umx misc stats functions
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
#' @family umx misc stats functions
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
#' @family umx misc functions
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' umx_has_been_run(m1)
umx_has_been_run <- function(model, stop = FALSE) {
	output <- model@output
	if (is.null(output)){
		if(stop){
			stop("Provided model has no objective function, and thus no output. I can only standardize models that have been run!")
		}else{
			return(FALSE)
		}
	} else if (length(output) < 1){
		if(stop){
			stop("Provided model has no output. I can only standardize models that have been run!")		
		}else{
			return(FALSE)
		}
	}
	return(TRUE)
}

#' umx_check_names
#'
#' check if a list of names are in the names() of a dataframe
#'
#' @param namesNeeded list of variable names to find
#' @param data data.frame to search in for names
#' @param die whether to die if the check fails (defaults to TRUE)
#' @param no_others Whether to test that the data contain no columns in addition to those in namesNeeded (defaults to F)
#' @return - boolean
#' @export
#' @family umx misc functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor) # "x1" "x2" "x3" "x4" "x5"
#' umx_check_names(c("x1", "x2"), demoOneFactor)
#' umx_check_names(c("z1", "x2"), data = demoOneFactor, die = F)
#' umx_check_names(c("x1", "x2"), data = demoOneFactor, die = F, no_others = T)
#' umx_check_names(c("x1","x2","x3","x4","x5"), data = demoOneFactor, die = F, no_others = T)
#' dontrun{
#' umx_check_names(c("not_in_the_frame", "x2"), data = demoOneFactor, die = T)
#' }
umx_check_names <- function(namesNeeded, data, die = TRUE, no_others = F){
	if(!is.data.frame(data)){
		stop("data has to be a dataframe")
	}
	namesInData = names(data)
	namesFound = (namesNeeded %in% namesInData)
	if(any(!namesFound)){
		if(die){
			print(namesFound)
			stop("Not all required names were found in the dataframe. Missing were:\n",
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

#' umx_is_ordinal
#'
#' Return the names of any ordinal variables in a dataframe
#'
#' @param df an \code{\link{data.frame}} to look in for ordinal variables
#' @param names whether to return the names of ordinal variables, or a binary (T,F) list (default = FALSE)
#' @param strict whether to stop when unordered factors are found (default = TRUE)
#' @param binary.only only count binary factors (2-levels) (default = FALSE)
#' @return - vector of variable names or Booleans
#' @export
#' @family umx misc functions
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' tmp = mtcars
#' tmp$cyl = ordered(mtcars$cyl) # ordered factor
#' tmp$vs = ordered(mtcars$vs) # binary factor
#' umx_is_ordinal(tmp)
#' umx_is_ordinal(tmp, names = TRUE)
#' umx_is_ordinal(tmp, names = TRUE, binary.only = TRUE)
#' isContinuous = !umx_is_ordinal(tmp)
#' tmp$gear = factor(mtcars$gear) # unordered factor
#' # nb: Factors are not necessarily ordered! By default unordered factors cause an error...
#' \dontrun{
#' tmp$cyl = factor(mtcars$cyl)
#' umx_is_ordinal(tmp)
#' }
umx_is_ordinal <- function(df, names = FALSE, strict = TRUE, binary.only = FALSE) {
	nVar = ncol(df);
	# Which are ordered factors?
	factorList  = rep(FALSE, nVar)
	orderedList = rep(FALSE, nVar)
	for(n in 1:nVar) {
		if(is.ordered(df[,n])) {
			thisLevels  = length(levels(df[,n]))
			if(binary.only & (2 == thisLevels) ){
				orderedList[n] = TRUE				
			} else if(!binary.only) {
				orderedList[n] = TRUE
			}
		}
		if(is.factor(df[,n])) {
			thisLevels = length(levels(df[,n]))
			if(binary.only & (2 == thisLevels) ){
				factorList[n] = TRUE
			} else if(!binary.only) {
				factorList[n] = TRUE
			}
		}
	}
	if(any(factorList & ! orderedList) & strict){
		stop("Dataframe contains at least 1 unordered factor. Set strict = FALSE to allow this.\n",
			  omxQuotes(names(df)[factorList & ! orderedList])
		)
	}
	if(names){
		return(names(df)[orderedList])
	} else {
		return(orderedList)
	}
}

#' umx_is_RAM
#'
#' Utility function returning a binary answer to the question "Is this a RAM model?"
#'
#' @param obj an object to be tested to see if it is an OpenMx RAM \code{\link{mxModel}}
#' @return - Boolean
#' @export
#' @family umx build functions
#' @seealso - \code{\link{mxModel}}
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
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
#' @family umx misc functions
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
#' @family umx misc functions
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_check_model(model)
umx_check_model <- function(obj, type = NULL, hasData = NULL, beenRun = NULL, hasMeans = NULL, checkSubmodels = FALSE) {
	# TODO hasSubmodels = F
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
	if(is.null(hasData)){
		# no check
	}else if (hasData & is.null(obj@data@observed)) {
		stop("'model' does not contain any data")
	}
	if(is.null(beenRun)){
		# no check
	}else {
		umx_has_been_run(obj)
	}
	if(is.null(hasMeans)){
		# no check
	}else {
		# TODO fix all these so they respect true and false...
		# currently assuming true
		umx_has_means(obj)
	}
	return(TRUE)
}

#' umx_is_cov
#'
#' test if a data frame or matrix is cov or cor data, or is likely to be raw...
#' @param data dataframe to test
#' @param boolean whether to return the type ("cov") or a boolean (default = string)
#' @param verbose How much feedback to give (default = FALSE)
#' @return - "raw", "cor", or "cov", or, if boolean= T, then T | F
#' @export
#' @family umx misc functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' df = cov(mtcars)
#' umx_is_cov(df)
#' df = cor(mtcars)
#' umx_is_cov(df)
#' umx_is_cov(df, boolean = T)
#' umx_is_cov(mtcars, boolean = T)

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
#' @family umx misc functions
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' umx_has_means(m1)
#' m1 <- mxModel(m1,
#' 	mxPath(from = "one", to = manifests),
#' 	mxData(demoOneFactor, type = "raw")
#' )
#' umx_has_means(m1)
#' m1 = umxRun(m1, setLabels = T, setValues = T)
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
#' @family umx misc functions
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
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' umx_has_CIs(m1) # FALSE: no CIs and no output
#' m1 = mxModel(m1, mxCI("G_to_x1"))
#' umx_has_CIs(m1, check = "intervals") # TRUE intervals set
#' umx_has_CIs(m1, check = "output")  # FALSE not yet run
#' m1 = mxRun(m1)
#' umx_has_CIs(m1, check = "output")  # Still FALSE: Set and Run
#' m1 = mxRun(m1, intervals = T)
#' umx_has_CIs(m1, check = "output")  # TRUE: Set, and Run with intervals = T
umx_has_CIs <- function(model, check = c("both", "intervals", "output")) {
	check = umx_default_option(check, c("both", "intervals", "output"))
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

#' umx_APA_pval
#'
#' round a p value so you get < .001 instead of .000000002 or .134E-16
#'
#' @param p A p-value to round
#' @param min Threshold to say < min
#' @param rounding Number of decimal to round to 
#' @param addComparison Whether to return the bare number, or to add the appropriate comparison symbol (= <)
#' @family umx misc reporting functions
#' @return - a value
#' @export
#' @seealso - \code{\link{round}}
#' @examples
#' umx_APA_pval(.052347)
#' umx_APA_pval(1.23E-3)
#' umx_APA_pval(1.23E-4)
#' umx_APA_pval(c(1.23E-3, .5))
#' umx_APA_pval(c(1.23E-3, .5), addComparison = T)

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
				return(format(round(p, rounding), scientific = F, nsmall = rounding))
			}else if(addComparison){				
				return(paste0("= ", format(round(p, rounding), scientific = F, nsmall = rounding)))
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
#' @param prefix This submodel to look in (i.e. "top.")
#' @param suffix The suffix for algebras ("_std")
#' @param digits = 2
#' @param verbose = FALSE
#' @return - the CI string, e.g. ".73 [-.2, .98]"
#' @export
#' @family umx  misc reporting functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' \dontrun{
#' umx_get_CI_as_APA_string(fit_IP, cellLabel = "ai_r1c1", prefix = "top.", suffix = "_std")
#' }
umx_get_CI_as_APA_string <- function(model, cellLabel, prefix = "top.", suffix = "_std", digits = 2, verbose=F){
	# umx_addCI_as_APA_string(fit_IP, celllabel = "ai_r1c1", prefix = "top.", suffix = "_std")
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
#' @family umx misc reporting functions
#' @seealso - \code{\link{umxSummary}}, \code{\link{umxCompare}}, \code{\link{anova}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @export
#' @examples
#' model = lm(mpg ~ cyl + disp, data = mtcars)
#' umxAnovaReport(model)

umxAnovaReport <- function(model1, model2 = NULL, raw = T, format = "string", printDIC = F) {
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
					round(a[2,"F"],2), ", p ", umx_u_APA_pval(a[2,"Pr(>F)"])
				)
			)
		} else {
			print( paste0("| ", a[2,"Df"], " | ", a[2,"Res.Df"], " | ", 
				round(a[2,"F"],2), " | ", umx_u_APA_pval(a[2,"Pr(>F)"]), " | ")
			)
		}		

	} else {
		a = anova(model1);
		if(require(QuantPsyc, quietly = T)){
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
#' @family umx misc functions
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

#' umx_has_square_brackets
#'
#' Helper function, checking if a label has sqaure brackets
#'
#' @param input The label to check for square brackets (string input)
#' @return - boolean
#' @export
#' @family umx misc functions
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
#' @family umx misc functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' alg = umx_string_to_algebra(paste(rep("A", nReps), collapse = " %*% "), name = "test_case")
#' }
umx_string_to_algebra <- function(algString, name = NA, dimnames = NA) {
	eval(substitute(mxAlgebra(tExp, name=name, dimnames=dimnames), list(tExp = parse(text=algString)[[1]])))
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
#' @family umx misc functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' m1 = mxModel("fit",
#'		mxMatrix("Full", nrow = 1, ncol = 1, free = T, values = 1, name = "a"), 
#'		mxMatrix("Full", nrow = 1, ncol = 1, free = T, values = 2, name = "b"), 
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
#' @family umx data helpers
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

#' umx_scale_wide_twin_data
#'
#' Scale wide data across all cases: currently twins
#'
#' @param df a wide dataframe
#' @param varsToScale the base names of the variables ("weight" etc)
#' @param suffixes the suffix that distinguishes each case (T1, T2 etc.)
#' @return - new dataframe with scaled variables
#' @export
#' @family umx data helpers
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' data(twinData) 
#' df = umx_scale_wide_twin_data(twinData, varsToScale = c("ht", "wt"), suffixes = c("1","2") )
#' plot(wt1 ~ wt2, data = df)

umx_scale_wide_twin_data <- function(df, varsToScale, suffixes) {
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
		totalMean = mean(c(T1, T2), na.rm = T)
		totalSD   =   sd(c(T1, T2), na.rm = T)
		T1 = (T1 - totalMean)/totalSD
		T2 = (T2 - totalMean)/totalSD
		df[,t1Traits[i] ] = T1
		df[,t2Traits[i] ] = T2
	}
	return(df)
}

#' umx_default_option
#'
#' handle parameter options given as a default list in a function
#'
#' @param x the value chosen (may be a selection, or the default list of options)
#' @param option_list TODO fix this documentation
#' @param check TRUE
#' @return - the option
#' @export
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' option_list = c("default", "par.observed", "empirical")
#' umx_default_option("par.observed", option_list)
#' umx_default_option("bad", option_list)
#' umx_default_option("allow me", option_list, check = F)
#' umx_default_option(option_list, option_list)
#' option_list = c(NULL, "par.observed", "empirical")
#' umx_default_option(option_list, option_list) # fails with NULL!!!!!
#' option_list = c(NA, "par.observed", "empirical")
#' umx_default_option(option_list, option_list) # use NA instead
#' }
umx_default_option <- function(x, option_list, check = T){
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
umx_fake_data <- function(dataset, digits = 2, n = NA, use.names = T, use.levels = T, use.miss = T, mvt.method = "eigen", het.ML = F, het.suppress = T){
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
  if(n!=row){
    select <- round(runif(n, 0.5, row+.49),0)
    del <- del[select,]
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
  unorder <- sum(location)

  if(unorder>0)warning(
    paste("Unordered factor detected in variable(s):", 
      names(dataset)[location]
    )
  )

  # if everything is numeric, don't invoke polycor
  if(sum(!num)==0){
    # generate data with rmvnorm
    fake <- rmvnorm(n, 
      apply(dataset, 2, mean, na.rm=TRUE),
      cov(dataset, use="pairwise.complete.obs"),
      mvt.method)

    # round the data to the requested digits
    fake <- round(fake, digits)

    # insert the missing data, if so requested
    if(use.miss==TRUE)(fake[del] <- NA)

    # give the variables names, if so requested
    if(use.names==TRUE)(names(fake) <- names(dataset))

    # return the new data
    return(fake)
  }

  # if there are factors, we start here

  # find the variable means (constrain to zero for factors)
  mixedMeans <- rep(0, col)
  mixedMeans[num] <- apply(dataset[,num], 2, mean, na.rm=TRUE)

  # estimate a heterogeneous correlation matrix
  if (het.suppress==TRUE){
    suppressWarnings(het <- polycor::hetcor(dataset, ML=het.ML))
  } else (het <- polycor::hetcor(dataset, ML=het.ML))
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
  if(use.miss==TRUE)(fake[del] <- NA)

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

  # round the data to the requested digits
  fake[,num] <- round(fake[,num], digits)

  # give the variables names, if so requested
  if(use.names==TRUE)(names(fake) <- names(dataset))
  
  # return the new data
  return(fake)
}

#' qm
#'
#' Quickmatrix function
#'
#' @param ... the components of your matrix
#' @param rowMarker mark the end of each row
#' @return - matrix
#' @family umx utility functions
#' @references \url{http://www.sumsar.net/blog/2014/03/a-hack-to-create-matrices-in-R-matlab-style}
#' @export
#' @examples
#' # simple example
#' qm(0, 1 |
#'    2, NA)
#' M <- N <- diag(2)
#' qm(M,c(4,5) | c(1,2),N | t(1:3))
#' matrix(1:16, 4)
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
# 	out <- strsplit(as.character(arg), split = colsep, fixed = T)
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
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://www.php.net/}
#' @examples
#' umx_explode("", "dog") # "d" "o" "g"
#' umx_explode(" ", "cats and dogs") # [1] "cats" "and"  "dogs"
umx_explode <- function(delimiter = character(), string) { 
	strsplit(string, split = delimiter)[[1]] 
}

#' umx_trim
#'
#' returns string w/o leading or trailing whitespace
#'
#' @param string to trim
#' @return - string
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
#' @param model an \code{\link{mxModel}} to WITH
#' @return - \code{\link{mxModel}}
#' @export
#' @family umx misc functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
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
#' @family umx misc functions
#' @references - \url{https://github.com/drknexus/repsych/blob/master/R/glibrary.r}
#' @examples
#' \dontrun{
#' demand("numderiv")
#' }
demand <- function(package = "") {
	if(FALSE == package %in% rownames(installed.packages() ) ) {
		m <- getCRANmirrors(all = FALSE, local.only = FALSE)
		URL <- m[grepl("Cloud",m$Name),"URL"][1] # get the first repos with "cloud" in the name
		install.packages(package, repos = URL)
	}
	require(pacakge)	
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
#' @family umx data helpers
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}

#' @examples
#' covData <- matrix(nrow=6, ncol=6, byrow=TRUE, dimnames=list(v1_6, v1_6),
#' data = c(0.9223099, 0.1862938, 0.4374359, 0.8959973, 0.9928430, 0.5320662,
#'            0.1862938, 0.2889364, 0.3927790, 0.3321639, 0.3371594, 0.4476898,
#'            0.4374359, 0.3927790, 1.0069552, 0.6918755, 0.7482155, 0.9013952,
#'            0.8959973, 0.3321639, 0.6918755, 1.8059956, 1.6142005, 0.8040448,
#'            0.9928430, 0.3371594, 0.7482155, 1.6142005, 1.9223567, 0.8777786,
#'            0.5320662, 0.4476898, 0.9013952, 0.8040448, 0.8777786, 1.3997558))
#' myData = umx_cov2raw(covData, n = 100, means = 1:6)
umx_cov2raw <- function(myCovariance, n, means = 0) {
	# TODO check cov is a cov matrix
	if(means == 0){
		means = rep(0,dim(myCovariance)[2])
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

#' umxHetCor
#'
#' umxHetCor Helper to return just the correlations from John Fox's polycor::hetcor function
#'
#' @param data A \code{\link{data.frame}} of columns for which to compute heterochoric correlations
#' @param ML Whether to use Maximum likelihood computation of correlations (default = F)
#' @param use How to delete missing data: "complete.obs", "pairwise.complete.obs" Default is pairwise.complete.obs
#' @param treatAllAsFactor Whether to treat all columns as factors, whether they are currently or not.
#' @param verbose How much to tell the user about what was done.
#' @return - A matrix of correlations
#' @family umx data helpers
#' @export
#' @seealso - \code{\link[polycor]{hetcor}}
#' @references - 
#' @examples
#' \dontrun{
#' cor_df = umxHetCor(df)
#' cor_df = umxHetCor(df, ML = F, use="pairwise.complete.obs")
#' }

umxHetCor <- function(data, ML = F, use = "pairwise.complete.obs", treatAllAsFactor=F, verbose=F){
	if(treatAllAsFactor){
		n = ncol(data)
		for (i in 1:n) {
			data[,i] = factor(data[,i])
		}
	}
	if(require(polycor)){
		hetc = polycor::hetcor(data, ML = ML, use = use, std.err = F)
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
#' @family umx data helpers
#' 
#' @export
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' 
#' tmpn = c("ROccAsp", "REdAsp", "FOccAsp", "FEdAsp", "RParAsp", "RIQ", "RSES", "FSES", "FIQ", "FParAsp")
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
#' umx_lower2full(tmp, diag = F)
umx_lower2full <- function(lower.data, diag = F, byrow = T) {
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
			mat[lower.tri(mat, diag = F)] <- mat[upper.tri(mat, diag = F)]
		}else{
			mat[lower.tri(mat, diag = diag)] <- lower.data;
			mat[upper.tri(mat, diag = F)] <-mat[lower.tri(mat, diag = F)]
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
#' @family umx data helpers
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
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
	meanDefVarValues = colMeans(df[, paste0(defNames, suffixes[1]), drop=F], na.rm = T)
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
#' @return - a list of bracket style labels
#' @export
#' @family umx misc functions
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
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
	d1 <- expand.grid(matName, "[", 1:rows, ",", 1:cols, "]", stringsAsFactors = F)	
	addys = c()
	for (i in 1:(rows*cols)) {
		addys = c(addys, paste(d1[i,], collapse = ""))
	}
	addys = matrix(addys, rows,cols)
	if(is.na(free) ){
		return(addys)
	} else if (free == T){
		return(addys[mat@free == TRUE])
	} else if (free == F){
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
#' @family umx reporting functions
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
#' @examples
#' require(OpenMx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents, to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
#' )
#' m1 = umxRun(m1, setLabels = T, setValues = T)
#' umx_show(m1)
#' umx_show(m1, digits = 3)
#' umx_show(m1, matrices = "S")
#' umx_show(m1, what = "free")
#' umx_show(m1, what = "labels")
#' umx_show(m1, what = "free", "A")
umx_show <- function(model, what = c("values", "free", "labels"), matrices = c("S", "A"), digits = 2) {
	what = umx_default_option(what, c("values", "free", "labels"), check = TRUE)
	for (w in matrices) {
		message("Showing ", what, " for:", w, " matrix:")
		if(what == "values"){
			umx_print(data.frame(model@matrices[[w]]@values), zero.print = ".", digits = digits)		
		}else if(what == "free"){
			umx_print(data.frame(model@matrices[[w]]@free), zero.print = ".", digits = digits)
		}
	}
}


# Poems you should know by heart
# https://en.wikipedia.org/wiki/O_Captain!_My_Captain!
# https://en.wikipedia.org/wiki/The_Second_Coming_(poem)
# https://en.wikipedia.org/wiki/Invictus
# http://www.poetryfoundation.org/poem/173698get