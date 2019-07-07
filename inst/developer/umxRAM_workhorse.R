#' Make RAM model using lavaan syntax
#'
#' @description
#' Can detect lavaan string input.
#'
#' @param model A lavaan string
#' @param data Data for the model (optional)
#' @param lavaanMode = "sem" auto-options, or "lavaan" (no auto options)
#' @param name Name for model (optional)
#' @param std.lv = Whether to standardize latents (var=1, mean=0). Default = FALSE n.b. Toggles fix.first
#' @param group = Column to use for multi-group (default = NULL)
#' @param group.equal = what to equate across groups. Default (NULL) means no equates. Options that might be implemented (but not yet: c("loadings", "intercepts", "means", "regressions", "residuals", "covariances")
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param printTab Print the table (defaults to FALSE) # TODO just verbose
#' @return - [mxModel()]
#' @export
#' @family Super-easy helpers
#' @seealso - [umxLav2RAM()]
#' @md
#' @examples
#' m1 = umxRAM("y ~ x")
#' umxRAM("y is x") # not a lavaan string
#' namedStr = " 	# my name
#' 	y ~x"
#' m1 = umxRAM(namedStr) 
#'
#' # test for removal of bad chars from name
#' lav = " # Model 14 PROCESS Hayes + - '~', ':', and '= moderated mediation
#' gnt ~ a*cb
#' " 
#' m1 = umxRAM(lav) 
#'
umxRAM2 <- function(model, data = NULL, group = NULL, group.equal = NULL, std.lv = FALSE, name = NULL, lavaanMode = c("sem", "lavaan"), autoRun = TRUE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), printTab = FALSE){
	lavaanMode = match.arg(lavaanMode)
	if (is.character(model) && grepl(model, pattern = "(<|~|=~|~~|:=)")){
		# Process lavaanString
		model = umxLav2RAM(model = model, data = data, group = group, group.equal = group.equal, std.lv = std.lv, name = name, lavaanMode = lavaanMode, autoRun = autoRun, tryHard = tryHard, printTab = printTab)
		invisible(model)
	}else{
		message("Woot: that doesn't look like a lavaan string to me:")
	}
	return(model)
}

