# ====================
# = Updating helpers =
# ====================

#' 	Update the OpenMx Library to latest version:
#'
#' This function automates the process of updating OpenMx while it is not a cran package
#'
#' @param bleedingEdge  binary factor determining whether to request the beta or relase version (F)
#' @param loadNew binary parameter determining whether to load the library after (optionally) updating
#' @param anyOK minimum version to accept without updating

#' @export
#' @examples
#' umxUpdateOpenMx()

umxUpdateOpenMx <- function(bleedingEdge = F, loadNew = T, anyOK = F) {
	# umxUpdateOpenMx()
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
		install.packages('OpenMx', repos='http://openmx.psyc.virginia.edu/testing/');
	} else {
		if (.Platform$OS.type == "windows") {
			if (!is.null(.Platform$r_arch) && .Platform$r_arch == "x64") {
				stop(paste("OpenMx is not yet supported on 64-bit R for Windows.",
				"Please use 32-bit R in the interim."), call. = FALSE)
			}
			repos <- c('http://openmx.psyc.virginia.edu/packages/')
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
					repos <- c('http://openmx.psyc.virginia.edu/sequential/')
					install.packages(pkgs=c('OpenMx'), repos=repos, 
					configure.args=c('--disable-openmp'))
				} else if (select == 2) {
					repos <- c('http://openmx.psyc.virginia.edu/packages/')
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
