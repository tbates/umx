#' Convert a twin dataset into umx standard format.
#'
#' @description
#' `umx_nice_data` is a function to convert your twin data into a format used across `umx`. Specifically:
#'
#' 1. zygosity is stored in a column called "zygosity".
#' 2. sep is set to "_T"
# #' @details
#'
#' @param data a [data.frame()] to check/convert.
#' @param sep existing separator string (updated to "_T").
#' @param zygosity existing zygosity column name (updated to `zygosity`).
#' @return - [data.frame()]
#' @export
#' @family Twin Data functions Data Functions, 
#' @seealso - [umx_wide2long()], [umx_long2wide()], 
#' @references - [tutorials](https://tbates.github.io), [tbates/umx](https://github.com/tbates/umx)
#' @md
#' @examples
#' tmp = umx_nice_data(twinData, sep="", zyg="zygosity")
#' namez(tmp)
#' # m1 = umxACE("wt")
#'
umx_nice_data <- function(data, sep, zyg) {
	# if(zyg != "zygosity"){
	
	if(zyg != "zygosity"){
		if(!is.null(data$zygosity)){
			stop("A column called 'zygosity' already exists. please rename that column first, e.g. with\n",
			"data = umx_rename(data, old='zygosity', replace='old_zyg'")
		} else {
			data$zygosity = data[, zyg]
		}
	}
	# update twin names with new separator
	oldNames = namez(data, paste0(sep, "[0-9]$"))
	newNames = namez(oldNames, patt = paste0(sep, "([0-9])$"), replac = "_T\\1")
	data = umx_rename(data=data, old = oldNames, replace = newNames)
}