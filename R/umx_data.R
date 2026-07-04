#' Show the datasets and vignettes in umx
#'
#' @description
#' `umx_data` lists the datasets and vignettes in `umx` with a 1-line description of each.
#'
#' @return None
#' @export
#' @family Miscellaneous Utility Functions
#' @md
#' @examples
#' umx_data()
umx_data <- function() {
	# Get datasets
	dataList = data(package = "umx")
	dataResults = dataList$results
	
	cat("Data sets in package 'umx': (access as data(\"name\", package=\"umx\"))\n")
	if (!is.null(dataResults) && nrow(dataResults) > 0) {
		for (i in 1:nrow(dataResults)) {
			cat(paste0(dataResults[i, "Item"], ": ", dataResults[i, "Title"], "\n"))
		}
	} else {
		cat("no datasets found\n")
	}
	
	cat("\n")
	
	# Get vignettes
	vignetteList = utils::vignette(package = "umx")
	vignetteResults = vignetteList$results
	
	cat("Vignettes in `umx` (access as vignette(\"name\", package=\"umx\"))\n")
	if (!is.null(vignetteResults) && nrow(vignetteResults) > 0) {
		for (i in 1:nrow(vignetteResults)) {
			cat(paste0(vignetteResults[i, "Item"], ": ", vignetteResults[i, "Title"], "\n"))
		}
	} else {
		cat("no vignettes found\n")
	}
}
