#' Convert a twin dataset into umx standard format.
#'
#' @description
#' `umx_make_twin_data_nice` is a function to convert your twin data into a format used across `umx`. Specifically:
#'
#' 1. Existing column for zygosity is renamed to "zygosity".
#' 2. `sep` is set to "_T"
#' 3. The twinID is is set to sequential digits, i.e. 1,2...
#' 
# #' @details
#' @param data a [data.frame()] to check/convert.
#' @param sep existing separator string (will be updated to "_T").
#' @param zygosity existing zygosity column name (will be renamed `zygosity`).
#' @param numbering existing twin sequence string (will be updated to _T1, _T2, _T3).
#' @param labelNumericZygosity If TRUE numeric zygosity levels will be set to labels.
#' @param levels legal levels of zygosity (ignored if labelNumericZygosity = FALSE (default 1:5)
#' @param labels labels for each zyg level c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS").
#' @return - [data.frame()]
#' @export
#' @family Twin Data functions
#' @seealso - [umx_wide2long()], [umx_long2wide()], 
#' @references - [tutorials](https://tbates.github.io), [tbates/umx](https://github.com/tbates/umx)
#' @md
#' @examples
#' data(twinData)
#' tmp = twinData
#' tmp$zygosity=NULL
#' tmp = umx_make_twin_data_nice(twinData, sep="", numbering = 1:5, zyg="zygosity")
#' namez(tmp, "zyg")
#' levels(tmp$zygosity)
#'
umx_make_twin_data_nice <- function(data, sep, zygosity, numbering, labelNumericZygosity = FALSE, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")){
	if(zygosity != "zygosity"){
		if(!is.null(data$zygosity)){
			stop("A column called 'zygosity' already exists. please rename that column first, e.g. with\n",
			"data = umx_rename(data, from='zygosity', to='old_zyg')")
		} else {
			data = umx_rename(data, from= zygosity, to= 'zygosity')
			# data$zygosity = data[, zygosity]
		}
	}
	# Update twin names with new separator.
	oldNames = namez(data, paste0(sep, "[0-9]$"))
	newNames = namez(oldNames, pattern = paste0(sep, "([0-9])$"), replacement = "_T\\1")
	data = umx_rename(data=data, from = oldNames, to = newNames)
	if(labelNumericZygosity){
		data$zygosity = factor(data$zygosity, levels= levels, labels = labels)
	}
	return(data)
}