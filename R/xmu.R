#   Copyright 2007-2019 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# ==================================================================================
# = Fns not used directly by users subject to arbitrary change and deprecation !!  =
# ==================================================================================


# ==========================
# = Run and Report helpers =
# ==========================

#' Show model logLik of model or print comparison table
#'
#' @description
#' Just a helper to show the logLik of a model or print a comparison table. 
#'
#' @param model an [mxModel()] to report on
#' @param comparison If not NULL, used as comparison model
#' @param digits (default = 2)
#' @return None
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxSummary()]
#' @md
#' @examples
#' \dontrun{
#' xmu_show_fit_or_comparison(model, comparison, digits=3)
#' }
#'
xmu_show_fit_or_comparison <- function(model, comparison = NULL, digits = 2) {
	if(is.null(comparison)){
		# \u00d7 = times sign
		message(paste0(model$name, " -2 \u00d7 log(Likelihood) = ", 
			round(-2 * logLik(model), digits = digits))
		)
	} else {
		message("Comparison of model with parent model:")
		umxCompare(comparison, model, digits = digits)
	}		
}

#' Safely run and summarize a model
#'
#' @description
#' The main benefit is that it returns the model, even if it can't be run.
#' 
#' The function will run the model if requested, wrapped in [tryCatch()] to avoid throwing an error.
#' If summary = TRUE then [umxSummary()] is requested (again, wrapped in try).
#' 
#' *note*: If `autoRun` is logical, then it over-rides `summary` to match `autoRun`. This is useful for easy use [umxRAM()] and twin models.
#'
#' @param model1 The model to attempt to run and summarize.
#' @param model2 Optional second model to compare with model1.
#' @param autoRun Whether to run or not (default = TRUE) Options are FALSE and "if needed".
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param summary Whether to print model summary (default = autoRun).
#' @param std What to print in summary (default FALSE) means raw, TRUE = standardize, null = omit parameter table.
#' @param comparison Toggle to allow not making comparison, even if second model is provided (more flexible in programming).
#' @param digits Rounding precision in tables and plots
#' @param show = "deprecated"
#' @return - [mxModel()]
#' @export
#' @family xmu internal not for end user
#' @seealso - [mxTryHard()]
#' @md
#' @examples
#' m1 = umxRAM("tim", data = mtcars,
#' 	umxPath(c("wt", "disp"), to = "mpg"),
#' 	umxPath("wt", with = "disp"),
#' 	umxPath(v.m. = c("wt", "disp", "mpg"))
#' )
#' m2 = umxModify(m1, "wt_to_mpg")
#'
#' # Summary ignored if run is false
#' xmu_safe_run_summary(m1, autoRun = FALSE, summary = TRUE)
#' # Run, no summary
#' xmu_safe_run_summary(m1, autoRun = TRUE, summary = FALSE)
#' # Default summary is just fit string
#' xmu_safe_run_summary(m1, autoRun = TRUE, summary = TRUE)
#' # Show std parameters
#' xmu_safe_run_summary(m1, autoRun = TRUE, summary = TRUE, std = TRUE)
#' # Run + Summary + comparison
#' xmu_safe_run_summary(m1, m2, autoRun = TRUE, summary = TRUE)
#' # Run + Summary + no comparison
#' xmu_safe_run_summary(m1, m2, autoRun = TRUE, summary = TRUE, std = TRUE, comparison= FALSE)
#'
xmu_safe_run_summary <- function(model1, model2 = NULL, autoRun = TRUE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), summary = !umx_set_silent(silent=TRUE), std = FALSE, comparison = TRUE, digits = 3, show = "deprecated") {
	# TODO xmu_safe_run_summary: Activate test examples
	tryHard = match.arg(tryHard)
	if(show != "deprecated"){
		stop("would be good is show was not passed to xmu_safe_run_summary")
	}

	if(tryHard == "yes"){
		tryHard = "mxTryHard"
	}
	if(!is.logical(autoRun)){
		if(autoRun == "if needed" && !umx_has_been_run(model1)){
			autoRun = FALSE
		}else{
			autoRun = TRUE
		}
	}
	if(!autoRun){
		summary = FALSE
	}
	if(autoRun){
		tryCatch({
			if(tryHard == "no"){
				model1 = mxRun(model1, beginMessage = !umx_set_silent(silent = TRUE), silent = umx_set_silent(silent = TRUE))
			} else if (tryHard == "mxTryHard"){
				model1 = mxTryHard(model1)
			} else if (tryHard == "mxTryHardOrdinal"){
				model1 = mxTryHardOrdinal(model1)
			} else if (tryHard == "mxTryHardWideSearch"){
				model1 = mxTryHardWideSearch(model1)
			}
		# }, warning = function(w){
		# 	if(tryHard == "no"){
		# 		message("Warning incurred trying to run model: mxTryHard might help?")
		# 	} else {
		# 		message("Warning incurred trying to run model")
		# 	}
		# 	message(w)
		}, error = function(e){
			if(tryHard == "no"){
				message("Error incurred trying to run model: model = mxTryHard(model) might help?")
			} else {
				message("Error incurred trying to run model")
			}
			message(e)
		})
	}
	if(!umx_has_been_run(model1)){
		# Didn't get run... don't try and summarize it (will error)
	} else if(summary){
		tryCatch({
			umxSummary(model1, std = std, digits = digits)
		# }, warning = function(w) {
		# 	message("Warning incurred trying to run umxSummary")
		# 	message(w)
		}, error = function(e) {
			message("Error incurred trying to run umxSummary")
			message(e)
		})

		tryCatch({
			if(!is.null(model2) && comparison){
				if(length(coef(model2)) > length(coef(model1))){
					umxCompare(model2, model1, digits = digits)
				} else {
					umxCompare(model1, model2, digits = digits)
				}
			}
		# }, warning = function(w) {
		# 	message("Warning incurred trying to run umxCompare")
		# 	message(w)
		}, error = function(e) {
			message("Error incurred trying to run umxCompare")
			message(e)
		})

	}
	invisible(model1)
}

# ===================================
# = Data and model checking helpers =
# ===================================

#' Check data to see if model needs means.
#'
#' @description
#' Check data to see if model needs means.
#'
#' @param data [mxData()] to check.
#' @param type of the data requested by the model.
#' @param allContinuousMethod How data will be processed if used for WLS.
#' @return - T/F
#' @export
#' @family xmu internal not for end user
#' @seealso - [xmu_make_mxData()]
#' @md
#' @examples
#' xmu_check_needs_means(mtcars, type = "Auto")
#' xmu_check_needs_means(mtcars, type = "FIML")
#' # xmu_check_needs_means(mtcars, type = "cov")
#' # xmu_check_needs_means(mtcars, type = "cor")
#'
#' # TRUE - marginals means means
#' xmu_check_needs_means(mtcars, type = "WLS", allContinuousMethod= "marginals")
#' xmu_check_needs_means(mtcars, type = "ULS", allContinuousMethod= "marginals")
#' xmu_check_needs_means(mtcars, type = "DWLS", allContinuousMethod= "marginals")
#'
#' # ================================
#' # = Provided as an mxData object =
#' # ================================
#' tmp = mxData(mtcars, type="raw")
#' xmu_check_needs_means(tmp, type = "FIML") # TRUE
#' xmu_check_needs_means(tmp, type = "ULS", allContinuousMethod= "cumulants") #FALSE
#' # TRUE - means with marginals
#' xmu_check_needs_means(tmp, type = "WLS", allContinuousMethod= "marginals")
#' tmp = mxData(cov(mtcars), type="cov", numObs= 100)
#' # Should catch this can't be type FIML
#' xmu_check_needs_means(tmp) # FALSE
#' tmp = mxData(cov(mtcars), means = umx_means(mtcars), type="cov", numObs= 100)
#' xmu_check_needs_means(tmp) # TRUE
#'
#' # =======================
#' # = One var is a factor =
#' # =======================
#' tmp = mtcars
#' tmp$cyl = factor(tmp$cyl)
#' xmu_check_needs_means(tmp, allContinuousMethod= "cumulants") # TRUE
#' xmu_check_needs_means(tmp, allContinuousMethod= "marginals") # TRUE - always has means
xmu_check_needs_means <- function(data, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), allContinuousMethod = c("cumulants", "marginals")) {
	# Add means if data are raw and means not requested by user
	type = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	# data must be mxData
	
	if(class(data) == "data.frame"){
		if(type %in% c("WLS", "DWLS", "ULS")){
			tmp =xmu_describe_data_WLS(data, allContinuousMethod = allContinuousMethod)
			return(tmp$hasMeans)
		}else if(type %in% c("cov", "cor")){
			warning("You passed in raw data, but requested type cov or cor. I can't tell yet if you will need means... make data into mxData first")
			return(FALSE)
		}else{
			# raw data needs means (can't tell if this would become cov with no means...)
			return(TRUE)
		}
	}else if(umx_is_MxData(data)){
		if(type %in% c('Auto', 'FIML') && (data$type == "raw")){
			return(TRUE)
			# Note, auto will be FIML not WLS
		}else if(type %in% c("WLS", "DWLS", "ULS")){
			tmp =xmu_describe_data_WLS(data, allContinuousMethod = allContinuousMethod)
			return(tmp$hasMeans)
		}else if(is.na(data$means[[1]])){
			# cov data no means
			return(FALSE)		
		}else{
			# cov data with means
			return(TRUE)
		}
	}else if(is.character(data)){
		return(FALSE)		
	}else{
		stop("I don't know what to do with data of type ", omxQuotes(class(data)))
	}
}

#' Check the minimum variance in data frame
#'
#' @description
#' Check that each variable exceeds a minimum variance and all are on compatible scales. Let the user know what to do if not.
#' @param data the data frame to check
#' @param minVar Minimum allowed variance in variables before warning user variances differ too much.
#' @param maxVarRatio Maximum allowed ratio of variance in data before warning user variances differ too much.
#' @return None 
#' @export
#' @family xmu internal not for end user
#' @examples
#' data(twinData)
#' xmu_check_variance(twinData[, c("wt1", "ht1", "wt2", "ht2")])
#' twinData[,c("ht1", "ht2")]= twinData[,c("ht1", "ht2")] * 100
#' xmu_check_variance(twinData[, c("wt1", "ht1", "wt2", "ht2")])
xmu_check_variance <- function(data, minVar = umx_set_data_variance_check(silent=T)$minVar, maxVarRatio = umx_set_data_variance_check(silent=T)$maxVarRatio){
	# data = twinData[, c("wt1","ht1", "wt2", "ht2")]; minVar = .1
	varList = umx_var(data, format = "diag")
	if(any(varList < minVar)){
		# At least 1 small
		message("The variance of variable(s) ", omxQuotes(names(which(varList < minVar))), " is < ", minVar, ".\n",
			"You might want to multiply to express the variable in smaller units, e.g. cm instead of metres.\n",
			"Alternatively umx_scale() for data already in long-format, or umx_scale_wide_twin_data for wide data might be useful.")
		
	}
	if(max(varList)/min(varList) > maxVarRatio){
		# At least 1 big difference in variance
		message("The variance of variable(s) ", omxQuotes(names(which.max(varList))), " is more than ", maxVarRatio, " times that of ", omxQuotes(names(which.min(varList))), ".\n",
			"You might want multiply to get variables into units on more similar scales.\n",
			"Alternatively, umx_scale() for data already in long-format, or umx_scale_wide_twin_data for wide data might be useful.")
		
	}

}

#' Upgrade a dataframe to an mxData type.
#'
#' @description
#' `xmu_make_mxData` is an internal function to upgrade a dataframe to `mxData`. It can also drop variables from the dataframe.
#'
#' The most common use will be to give it a dataframe, and get back an `mxData` object of type raw, cov, cor (WLS is just raw).
#'
#' @param data A [data.frame()] or [mxData()]
#' @param type What data type is wanted out c("Auto", "FIML", "cov", "cor", 'WLS', 'DWLS', 'ULS')
#' @param manifests If set, only these variables will be retained.
#' @param verbose If verbose, report on columns kept and dropped (default FALSE)
#' @return - [mxData()]
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' # =========================
#' # = Continuous ML example =
#' # =========================
#' manVars = c("mpg", "cyl", "disp")
#' tmp = xmu_make_mxData(data= mtcars, type = "Auto"); # class(tmp); # "MxDataStatic"
#' # names(tmp$observed) # "mpg"  "cyl"  "disp"
#' tmp = xmu_make_mxData(data= mtcars, type = "Auto", manifests = manVars); 
#' tmp$type == "raw" # TRUE
#'
#' # ==============================
#' # = All continuous WLS example =
#' # ==============================
#' tmp = xmu_make_mxData(data= mtcars, type = "WLS" , manifests = manVars, verbose= TRUE)
#' tmp$type == "raw" # TRUE (WLS is triggered by the fit function, not the data type)
#'
#' # ============================
#' # = Missing data WLS example =
#' # ============================
#' tmp = mtcars; tmp[1, "mpg"] = NA # add NA
#' tmp = xmu_make_mxData(data= tmp, type = "WLS", manifests = manVars, verbose= TRUE)
#'
#' # ==========================
#' # = already mxData example =
#' # ==========================
#'m1 = umxRAM("auto", data = mxData(mtcars, type = "raw"),
#'	umxPath(var= "wt"),
#'	umxPath(mean=  "wt")
#')
#'
#' # ========================
#' # = Cov and cor examples =
#' # ========================
#' tmp = xmu_make_mxData(data= mtcars, type = "cov")
#' tmp = xmu_make_mxData(data= mtcars, type = "cor")
#'
#' # =======================
#' # = Pass string through =
#' # =======================
#' xmu_make_mxData(data= c("a", "b", "c"), type = "Auto")
#' 
xmu_make_mxData <- function(data= NULL, type = c("Auto", "FIML", "cov", "cor", 'WLS', 'DWLS', 'ULS'), manifests = NULL, verbose = FALSE) {
	type = match.arg(type)
	if(is.null(data)){
		message("You must set data: either data = dataframe or data = mxData(yourData, type = 'raw|cov)', ...) or at least a list of variable names if using umxRAM in sketch mode)")
		stop("Did you perhaps just include the data among other functions instead of via data = ?")
	}else if(class(data) == "character"){
		# Pass strings through
		return(data)
	}

	if(is.null(manifests)){
		# Manifests not specified: retain all except illegal variables
		manifests = umx_names(data)
		if("one" %in% manifests){
			warning("You have a data column called 'one' which is illegal (it's the code used for setting means). I'll drop it!")
			manifests = manifests[!manifests %in% c("one")]
			unusedManifests = "one"
			dropColumns = TRUE
		}else{
			unusedManifests = c()
			dropColumns = FALSE
		}
	}else{
		# Manifests specified: mark all others as un-used
		# remove duplicates (e.g. caused by var in manifests and definition vars lists
		manifests = unique(manifests)
		unusedManifests = setdiff(umx_names(data), manifests)
		dropColumns = TRUE
	}

	if (class(data)[1] == "data.frame") {
		if(dropColumns){
			# Trim down the data to include only the requested columns
			data = data[, manifests, drop = FALSE]
			xmu_check_variance(data)
		}
		# Upgrade data.frame to mxData of desired type
		if(type %in% c("Auto", "FIML")){
			data = mxData(observed = data, type = "raw")
		}else	if(type == "cov"){
			# TODO xmu_make_mxData: could refuse to do this, as we don't know how to handle missingness...
			data = mxData(observed = cov(data), type = type, numObs = nrow(data))
		}else	if(type == "cor"){
			# TODO xmu_make_mxData: could refuse to do this, as we don't know how to handle missingness...
			data = mxData(observed = cor(data), type = type, numObs = nrow(data))
		} else if(type %in% c('WLS', 'DWLS', 'ULS')){
			if(any(umx_is_ordered(data))){
				# At least one non-continuous variable
			} else if(anyNA(data)){
				# All continuous and, some NA
				# message("Polite note from xmu_make_mxData: Missing data can't be handled in continuous-variable WLS.\n You might want to remove rows with missing values")
				# oldRows = nrow(data)
				# data = na.omit(data)
				# message("polite note from xmu_make_mxData: Missing data can't be handled in continuous-variable WLS.\n You might want to remove ", (nrow(data) - oldRows), " rows with missing values")
			}
			data = mxData(observed = data, type = "raw")
		}else{
			stop("I don't know how to create data of type ", omxQuotes(type))
		}
	}else if(umx_is_MxData(data)){
		# Already an mxData
		if(dropColumns){
			if(data$type %in% c("cov", "cor")){
				# Trim down the data to include only the requested columns				
				data$observed = umx_reorder(data$observed, manifests)
			} else if (data$type == "raw"){
				# Might be worth doing data = mxData(data = data$observed[, manifests], type=‘raw’)
				data$observed = data$observed[, manifests, drop = FALSE]
			} else {
				stop("You offered up an existing mxData and requested dropping unused variables: I can only do this for cov, cor, and raw data")
			}
		}
	}else if (class(data) == "matrix"){
		message("You gave me a matrix. umx needs to know the N for cov data. Rather than me assemble it,
			the easiest and least error-prone method is for you to pass in raw data, or else\n
			data = mxData(yourCov, type= 'cov', numObs= 100) # (or whatever your N is)")
	}else{
		stop("I was expecting a data frame or mxData  you gave me a ", omxQuotes(class(data)))	
	}
	if(verbose){
		msg_str = ""
		if(length(unusedManifests) > 0){
			if(length(unusedManifests) > 10){
				varList = paste0("First 10 were: ", paste(unusedManifests[1:10], collapse = ", "))
				msg_str = paste0(length(unusedManifests), " unused variables (", varList)
			} else if(length(unusedManifests) > 1){
				varList = paste(unusedManifests, collapse = ", ")
				msg_str = paste0(length(unusedManifests), " unused variables (", varList)
			} else {
				varList = unusedManifests
				msg_str = paste0(length(unusedManifests), " unused variable (", varList)
			}
		}
		
		# TODO clean up notes from old-style WLS
		# if("preferredFit" %in% names(data)){
		# 	message("Preferred fit for data = ", data$preferredFit)
		# } else {
		# 	message("Data type = ", data$type)
		# }
		# if(is.na(data$means)){
			# message("No means")
		# } else {
			# message("Has means")
		# }
		message("ManifestVars set to: ", paste(manifests, collapse = ", "), ". ", msg_str)
	}
	return(data)
}

# ==========================
# = Model building helpers =
# ==========================

#' Find name for model
#'
#' @description
#' Use name if provided. If first line contains a #, uses this line as name. Else use default.
#'
#' @param lavaanString A model string, possibly with # model name on line 1.
#' @param name A desired model name (optional).
#' @param default A default name if nothing else found.
#' @return - A name string
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxRAM()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' "m1" == xmu_name_from_lavaan_str("x~~x")
#' "bob" == xmu_name_from_lavaan_str(name = "bob")
#' "my_model" == xmu_name_from_lavaan_str("# my model")
#'
xmu_name_from_lavaan_str <- function(lavaanString = NULL, name = NA, default = "m1") {
	# Assume `name` should be used if !is.null(name)
	if(is.na(name)){
		# If first line contains a #, assume user wants it to be a name for the model
		line1 = strsplit(lavaanString, split="\\n", perl = TRUE)[[1]][1]
		if(grepl(x = line1, pattern = "#")){
			# line1 = "## my model ##"
			pat = "\\h*#+\\h*([^\\n#;]+).*" # remove leading #, trim
			name = gsub(x = line1, pattern = pat, replacement = "\\1", perl = TRUE);
			name = trimws(name)
			# Replace white space with  "_"
			name = gsub("(\\h+)", "_", name, perl = TRUE)
			# Delete illegal characters
			name = as.character(mxMakeNames(name))
		}else{
			# No name given in name or comment: use a default name
			name = default
		}
	}else{
		name = name
	}
	return(name)
}

#' Just a helper to cope with deprecated suffix lying around.
#'
#' @description
#' Returns either suffix or sep, with a deprecation warning if suffix is set.
#'
#' @param sep The separator (if suffix != 'deprecated', then this is returned).
#' @param suffix The suffix, defaults to 'deprecated'.
#' @return - sep
#' @export
#' @family xmu internal not for end user
#' @examples
#' xmu_set_sep_from_suffix(sep = "_T", suffix = "deprecated")
xmu_set_sep_from_suffix <- function(sep, suffix) {
	if (suffix != "deprecated"){
		warning("Just a message, but please use 'sep = ' instead of 'suffix = '. suffix will stop working in 2019")
		return(suffix)
	}else{
		return(sep)		
	}
}

#' Check basic aspects of input for twin models.
#'
#' @description
#' Check that DVs are in the data, that the data have rows, set the optimizer if requested.
#'
#' @param selDVs Variables used in the data.
#' @param dzData The DZ twin data.
#' @param mzData The MZ twin data.
#' @param sep Separator between base-name and numeric suffix when creating variable names, e.g. "_T"
#' @param nSib How many people per family? (Default = 2).
#' @param numObsMZ set if data are not raw.
#' @param numObsDZ set if data are not raw.
#' @param enforceSep Whether to require sep to be set, or just warn if it is not (Default = TRUE: enforce).
#' @param optimizer Set by name (if you want to change it).
#' @return None
#' @export
#' @family xmu internal not for end user
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' library(umx)
#' data(twinData)
#' mzData = subset(twinData, zygosity == "MZFF")
#' dzData = subset(twinData, zygosity == "MZFF")
#' xmu_twin_check(selDVs = c("wt", "ht"), dzData = dzData, mzData = mzData, 
#' 	sep = "", enforceSep = TRUE)
#' xmu_twin_check(selDVs = c("wt", "ht"), dzData = dzData, mzData = mzData, 
#' 	sep = "", enforceSep = FALSE)
#' xmu_twin_check(selDVs = c("wt", "ht"), dzData = dzData, mzData = mzData, 
#' 	sep = "", enforceSep = TRUE, nSib = 2, optimizer = NULL)
#' 
#' \dontrun{
#' # TODO xmu_twin_check: move to a test file:
#' # 1. stop on no rows
#' xmu_twin_check("Generativity", twinData[NULL,], twinData[NULL,], sep="_T")
#' # Error in xmu_twin_check("Generativity", twinData[NULL, ], twinData[NULL,  : 
#' #   Your DZ dataset has no rows!
#' 
#' # 2. Stop on a NULL sep  = NULL IFF enforceSep = TRUE
#' xmu_twin_check(selDVs = c("wt", "ht"), dzData = dzData, mzData = mzData, enforceSep = TRUE)
#' # 3. stop on a factor with sep = NULL
#' }
xmu_twin_check <- function(selDVs, dzData = dzData, mzData = mzData, sep = NULL, enforceSep = TRUE, nSib = 2, numObsMZ = NULL, numObsDZ = NULL, optimizer = NULL) {
	# 1. Check data has rows
	if(nrow(dzData) == 0){ stop("Your DZ dataset has no rows!") }
	if(nrow(mzData) == 0){ stop("Your MZ dataset has no rows!") }
	
	# 2. handle sep
	if(is.null(sep)){
		if(enforceSep){
			message("Please use sep. e.g. sep = '_T'. Set `selDVs` to the base variable names, and I will create the full variable names from that.")
			# strip the numbers off the ends
			namez(selDVs, "(_.)[0-9]$", replacement = "")
			nodigits = namez(selDVs, "[0-9]$", replacement = "")
			nodigits = unique(nodigits)
			if(length(namez(selDVs, "(_.)[0-9]$")) == 0){
				# no _ probably empty sep
				stop("In your case (I'm guessing) say: selDVs = c(", omxQuotes(nodigits), "), sep = '' ")
			} else {
				sepGuess = unique(namez(selDVs, ".*(_.*)[0-9]$", replacement="\\1"))
				stop("In your case (I'm guessing) say: selDVs = c(", omxQuotes(nodigits), "), sep = ", omxQuotes(sepGuess), " ")
			}
		} else {
			selVars = selDVs
			# Assume names are already expanded
		}
	} else if(length(sep) != 1){
		stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
		"i.e., set selDVs to 'obese', sep to '_T' and I look for 'obese_T1' and 'obese_T2' in the data...\n",
		"PS: variables have to end in 1 or 2, i.e  'example_T1' and 'example_T2'")
	}else{
		# 3. expand variable names
		selVars = umx_paste_names(selDVs, sep = sep, suffixes = 1:nSib)
	}

	# 4. Check all names in the data
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)


	# 6. Look for name conflicts
	badNames = umx_grep(selVars, grepString = "^[ACDEacde][0-9]*$")
	if(!identical(character(0), badNames)){
		stop("The data contain variables that look like parts of the a, c, e model, i.e., a1 is illegal.\n",
		"BadNames included: ", omxQuotes(badNames) )
	}
	dataType = umx_is_cov(dzData, boolean = FALSE)
	if(dataType == "raw"){
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
	# 5. Check data are legal
		if(!umx_is_class(mzData[, selVars], classes = c("integer", "double", "numeric", "factor", "ordered"), all = TRUE)) {
			bad = selVars[!umx_is_class(mzData[, selVars], classes = c("integer", "double", "numeric","factor", "ordered"), all = FALSE)]
			stop("variables must be integer, numeric or (possibly ordered) factor. The following are not: ", omxQuotes(bad))
		}
		# Drop unused columns from mzData and dzData
		mzData = mzData[, selVars]
		dzData = dzData[, selVars]
		isFactor = umx_is_ordered(mzData[, selVars])                      # T/F list of factor columns
		isOrd    = umx_is_ordered(mzData[, selVars], ordinal.only = TRUE) # T/F list of ordinal (excluding binary)
		isBin    = umx_is_ordered(mzData[, selVars], binary.only  = TRUE) # T/F list of binary columns
		nFactors = sum(isFactor)
		nOrdVars = sum(isOrd) # total number of ordinal columns
		nBinVars = sum(isBin) # total number of binary columns

		factorVarNames = names(mzData)[isFactor]
		ordVarNames    = names(mzData)[isOrd]
		binVarNames    = names(mzData)[isBin]
		contVarNames   = names(mzData)[!isFactor]
	} else {
		# Summary data
		isFactor = isOrd    = isBin    = c()
		nFactors = nOrdVars = nBinVars = 0
		factorVarNames = ordVarNames = binVarNames = contVarNames = c()
	}
	
	if(nFactors > 0 & is.null(sep)){
		stop("Please set 'sep'. e.g.: sep = '_T' \n",
		"Why: Your data include ordinal or binary variables.\n
		So I need to know which variables are for twin 1 and which for twin2.\n",
		"The way I do this is enforcing some naming rules. For example, if you have 2 variables:\n",
		" obesity and depression called: 'obesity_T1', 'dep_T1', 'obesity_T2' and 'dep_T2', you should call umxACE with:\n",
		"selDVs = c('obesity','dep'), sep = '_T' \n",
		"sep is just one word, appearing in all variables (e.g. '_T').\n",
		"This is assumed to be followed by '1' '2' etc...")
	}
	
	# Finally, set optimizer if we get to here
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}
}

#' xmu_check_levels_identical
#'
#' Just checks that the factor levels for twins 1 and 2 are the same
#'
#' @param df data.frame containing the data
#' @param selDVs base names of variables (without suffixes)
#' @param sep text-constant separating base variable names the twin index (1:2)
#' @param action if unequal levels found:  c("stop", "ignore")
#' @return None 
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' baseNames = c("bmi")
#' selDVs = umx_paste_names(baseNames, "", 1:2)
#' tmp = twinData[, selDVs]
#' tmp$bmi1[tmp$bmi1 <= 22] = 22
#' tmp$bmi2[tmp$bmi2 <= 22] = 22
#' xmu_check_levels_identical(umxFactor(tmp, sep = ""), selDVs = baseNames, sep = "")
#' \dontrun{
#' xmu_check_levels_identical(umxFactor(tmp), selDVs = baseNames, sep = "")
#' }
xmu_check_levels_identical <- function(df, selDVs, sep, action = c("stop", "ignore")){
	n = umx_explode_twin_names(df, sep)
	baseNames   = n$baseNames
	sep         = n$sep
	twinIndexes = n$twinIndexes
	selVars     = umx_paste_names(selDVs, sep = sep, suffixes = twinIndexes)
	umx_check_names(selVars, data = df, die = TRUE)
	nSib = length(twinIndexes)
	if(nSib != 2){
		stop("Sorry, Ask tim to implement handling more than two sibs")
	}
	allIdentical = TRUE
	for (thisVar in selDVs) {
		a = levels(df[,paste0(thisVar, sep, twinIndexes[1])])
		b = levels(df[,paste0(thisVar, sep, twinIndexes[2])])
		if(!identical(a, b)){
			if(action =="stop"){
				allIdentical = FALSE
				stop("levels of ", thisVar, " not identical for twin 1 and twin 2")
			} else {
				# alt.expr
			}
		}
	}
	return(allIdentical)
}



#' xmuLabel_MATRIX_Model (not a user function)
#'
#' This function will label all the free parameters in a (non-RAM) OpenMx [mxModel()]
#' nb: We don't assume what each matrix is for. Instead, the function just sticks labels like "a_r1c1" into each cell
#' i.e., matrix-name + _ + r + rowNumber + c + colNumber
#' 
#' End users should just call [umxLabel()]
#' 
#'
#' @param model a matrix-style mxModel to label
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @return - The labeled [mxModel()]
#' @family xmu internal not for end user
#' @export
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' m2 <- mxModel("One Factor",
#' 	mxMatrix("Full", 5, 1, values = 0.2, free = TRUE, name = "A"), 
#' 	mxMatrix("Symm", 1, 1, values = 1.0, free = FALSE, name = "L"), 
#' 	mxMatrix("Diag", 5, 5, values = 1.0, free = TRUE, name = "U"), 
#' 	mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
#' 	mxExpectationNormal("R", dimnames = names(demoOneFactor)),
#' 	mxFitFunctionML(),
#' 	demoOneFactor, type = "cov",
#' )
#' m3 = umx:::xmuLabel_MATRIX_Model(m2)
#' m4 = umx:::xmuLabel_MATRIX_Model(m2, suffix = "male")
#' # explore these with omxGetParameters(m4)
xmuLabel_MATRIX_Model <- function(model, suffix = "", verbose = TRUE) {
	if(!umx_is_MxModel(model) ){
		stop("xmuLabel_MATRIX_Model needs model as input")
	}
	if (umx_is_RAM(model)) {
		stop("xmuLabel_MATRIX_Model shouldn't be seeing RAM Models")
	}
	model = xmuPropagateLabels(model, suffix = "", verbose = verbose)
	return(model)
}

#' xmuLabel_RAM_Model (not a user function)
#'
#' This function will label all the free parameters in a RAM [mxModel()]
#' 
#' End users should just call [umxLabel()]
#'
#' @param model a RAM mxModel to label
#' @param suffix a string to append to each label
#' @param labelFixedCells Whether to labelFixedCells (Default TRUE)
#' @param overRideExisting Whether to overRideExisting (Default FALSE)
#' @param verbose how much feedback to give
#' @param name Add optional name parameter to rename returned model (default = leave it along)
#' @return - The labeled [mxModel()]
#' @family xmu internal not for end user
#' @export
#' @md
#' @examples
#' require(umx); data(demoOneFactor)
#' # raw but no means
#' m1 <- mxModel("One Factor", mxData(demoOneFactor, type = "raw"), type="RAM",
#' 	manifestVars = "x1", latentVars= "G",
#' 	umxPath("G", to = "x1"),
#' 	umxPath(var = "x1"),
#' 	umxPath(var = "G", fixedAt = 1)
#' )
#' xmuLabel_RAM_Model(m1)
#'
xmuLabel_RAM_Model <- function(model, suffix = "", labelFixedCells = TRUE, overRideExisting = FALSE, verbose = FALSE, name = NULL) {
	if (!umx_is_RAM(model)) {
		stop("'model' must be an OpenMx RAM Model")
	}
	freeA  = model$A$free
	namesA = dimnames(freeA)[[1]]

	freeS  = model$S$free
	namesS = dimnames(freeS)[[1]]

	# =========================
	# = Add asymmetric labels =
	# =========================
	theseNames = namesA
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeA[toRow, fromCol]){
			   thisLabel = paste0(theseNames[fromCol], "_to_", theseNames[toRow], suffix)			 	
				if(overRideExisting | is.na(model$A$labels[toRow,fromCol])){
					model$A$labels[toRow,fromCol] = thisLabel
			 	}
			}
		}
	}

	# =========================
	# = Add Symmetric labels =
	# =========================
	# Bivariate names are sorted alphabetically, makes it unambiguous...
	theseNames = namesS
	for(fromCol in seq_along(theseNames)) {
		for(toRow in seq_along(theseNames)) {
			if(labelFixedCells | freeS[toRow, fromCol]) {
			   orderedNames = sort(c(theseNames[fromCol], theseNames[toRow]))
			   thisLabel = paste0(orderedNames[1], "_with_", orderedNames[2], suffix)
 				if(overRideExisting | is.na(model$S$labels[toRow,fromCol])){
			   	 model$S$labels[toRow,fromCol] = thisLabel
 			 	}
			}
		}
	}
	model$S$labels[lower.tri(model$S$labels)] = t(model$S$labels[upper.tri(t(model$S$labels))])
	toGet = model$S$labels
	transpose_toGet = t(toGet)
	model$S$labels[lower.tri(toGet)] = transpose_toGet[lower.tri(transpose_toGet)]

	# ==============================
	# = Add means labels if needed =
	# ==============================

	if(!is.null(model$M)){
		meanLabels = paste0("one_to_", colnames(model$M$values), suffix)
		if(overRideExisting){
			model$M$labels[] = meanLabels
	 	}else{
			model$M$labels[is.na(model$M$labels)] = meanLabels[is.na(model$M$labels)]
	 	}
	}
	if(!is.null(name)){
		model = mxModel(model, name= name)
	}
	return(model)
}

#' Internal function to help building simplex models
#'
#' @description
#' internal function to help building simplex models is a function which 
# Sets the bottom corner free in a matrix, e.g. labels:
# FALSE, FALSE, FALSE, FALSE,
# TRUE , FALSE, FALSE, FALSE,
# FALSE, TRUE , FALSE, FALSE,
# FALSE, FALSE, TRUE , FALSE
# Values
# 0 , 0, 0, 0,
# .9, 0, 0, 0,
# 0 ,.9, 0, 0,
# 0 , 0,.9, 0
#'
#' @param x size of matrix, or an [umxMatrix()] of which to free the bottom triangle.
#' @param start a default start value for the freed items.
#' @return - [umxMatrix()]
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxMatrix()]
#' @md
#' @examples
#' x = umxMatrix('test', 'Full', nrow = 4, ncol = 4)
#' xmu_simplex_corner(x, start = .9)
#' # See how we have a diag free, but offset 1-down?
#' umx_print( xmu_simplex_corner(x, start = .9)$values, zero=".")
xmu_simplex_corner <- function(x, start = .9) {
	if(!umx_is_MxMatrix(x)){
		x = umxMatrix('test', 'Full', nrow = x, ncol = x)
	}
	nVar = dim(x)[1]
	if(length(start)==1){
		start = rep(start, nVar - 1)
	}
	start = c(NA, start)
	nVar_minus1 = nVar-1
	for(thisRow in 2:nVar) {
		x$free[thisRow, (thisRow-1)] = TRUE
		x$values[thisRow, (thisRow-1)] = start[thisRow]
	}
	return(x)
}

#' xmuLabel_Matrix (not a user function)
#'
#' This function will label all the free parameters in an [mxMatrix()]
#' 
#' End users should just call [umxLabel()]
#'
#' Purpose: label the cells of an mxMatrix
#' Detail: Defaults to the handy "name_r1c1" where name is the matrix name, and r1c1 = row 1 col 1.
#' Use case: You should not use this: call umxLabel
#' umx:::xmuLabel_Matrix(mxMatrix("Lower", 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 3, 3, values = 1, name = "a", byrow = TRUE));
#' umx:::xmuLabel_Matrix(mxMatrix("Symm" , 3, 3, values = 1, name = "a", byrow = TRUE), jiggle = .05, boundDiag = NA);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"));
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "data.a"), overRideExisting = TRUE);
#' umx:::xmuLabel_Matrix(mxMatrix("Full" , 1, 1, values = 1, name = "a", labels= "test"), overRideExisting = TRUE);
#' See also: fit2 = omxSetParameters(fit1, labels = "a_r1c1", free = FALSE, value = 0, name = "drop_a_row1_c1")
#' 
#' @param mx_matrix an mxMatrix
#' @param baseName A base name for the labels NA
#' @param setfree Whether to set free cells FALSE
#' @param drop What values to drop 0
#' @param jiggle = whether to jiggle start values
#' @param boundDiag set diagonal element lbounds to this numeric value (default = NA = ignore) 
#' @param suffix a string to append to each label
#' @param verbose how much feedback to give
#' @param labelFixedCells = FALSE
#' @param overRideExisting Whether to overRideExisting (Default FALSE)
#' @return - The labeled [mxMatrix()]
#' @family xmu internal not for end user
#' @md
#' @export
xmuLabel_Matrix <- function(mx_matrix = NA, baseName = NA, setfree = FALSE, drop = 0, jiggle = NA, boundDiag = NA, suffix = "", verbose = TRUE, labelFixedCells = FALSE, overRideExisting = FALSE) {
	if (!is(mx_matrix, "MxMatrix")){ # label a mxMatrix
		stop("I'm sorry Dave... xmuLabel_Matrix works on mxMatrix. You passed an ", class(mx_matrix), ". And why are you calling xmuLabel_Matrix() anyhow? You want umxLabel()")
	}
	type = class(mx_matrix)[1]; # Diag Full  Lower Stand Sdiag Symm Iden Unit Zero
	nrows = nrow(mx_matrix);
	ncols = ncol(mx_matrix);
	newLabels    = mx_matrix$labels;
	mirrorLabels = newLabels
	if(is.na(baseName)) { 
		baseName = mx_matrix$name
	}
	if(suffix != "") {
		baseName = paste(baseName, suffix, sep = "_")
	}

	if(any(grep("^data\\.", newLabels)) ) {
		if(verbose){
			message("matrix ", mx_matrix$name, " contains definition variables in the labels already... I'm leaving them alone")
		}
	}

	# Make a matrix of labels in the form "baseName_rRcC"	
	for (r in 1:nrows) {
		for (c in 1:ncols) {		
			if(grepl("^data\\.", newLabels[r, c])){
				# definition variable, leave it alone
			} else if (overRideExisting | is.na(newLabels[r, c])){
				newLabels[r, c] = paste0(baseName, "_r", r, "c", c)
				if(nrows == ncols) {
					# Used below if needed.
					# Should include all square forms type == "StandMatrix" | type == "SymmMatrix"
					mirrorLabels[c,r] = paste0(baseName, "_r", r, "c", c)
				}
			}			
		}
	}

	if(type == "DiagMatrix"){
		newLabels[lower.tri(newLabels, diag = FALSE)] = NA
		newLabels[upper.tri(newLabels, diag = FALSE)] = NA
	} else if(type == "FullMatrix"){
		# newLabels = newLabels
	} else if(type == "LowerMatrix"){
		newLabels[upper.tri(newLabels, diag = FALSE)] = NA 
	} else if(type == "SdiagMatrix"){
		newLabels[upper.tri(newLabels, diag = TRUE)] = NA
	} else if(type == "SymmMatrix"){
		newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = FALSE)] <- mirrorLabels[upper.tri(mirrorLabels, diag = FALSE)]
	} else if(type == "StandMatrix") {
		newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
		newLabels[upper.tri(newLabels, diag = FALSE)] <- mirrorLabels[upper.tri(mirrorLabels, diag = FALSE)]
		diag(newLabels) <- NA
	} else if(type == "IdenMatrix" | type == "UnitMatrix" | type == "ZeroMatrix") {
		# message("umxLabel Ignored ", type, " matrix ", mx_matrix$name, " - it has no free values!")
		return(mx_matrix)
	} else {
		return(paste0("You tried to set type ", "to ", omxQuotes(type)));
	}
	# Set labels
	mx_matrix$labels <- newLabels;
	if(setfree == FALSE) {
		# return("Matrix Specification not used: leave free as set in mx_matrix") 
	} else {
		newFree = mx_matrix$free
		# return(newFree)
		newFree[mx_matrix$values == drop] = FALSE;
		newFree[mx_matrix$values != drop] = TRUE;
		if(type=="StandMatrix") {
			newLabels[lower.tri(newLabels, diag = FALSE)] -> lower.labels;
			newLabels[upper.tri(newLabels, diag = FALSE)] <- lower.labels;
		} else {
			mx_matrix$free <- newFree
		}
		# newFree[is.na(newLabels)]=NA; # (validated by mxMatrix???)
	}
	if(!is.na(jiggle)){
		mx_matrix$values <- umxJiggle(mx_matrix$values, mean = 0, sd = jiggle, dontTouch = drop) # Expecting sd
	}
	# TODO this might want something to equate values after jiggling around equal labels?
	if(!is.na(boundDiag)){
		diag(mx_matrix$lbound) <- boundDiag # bound diagonal to be positive 
	}
	return(mx_matrix)
}

#' Make a deviation-based mxRAMObjective for ordinal models.
#'
#' Purpose: return a mxRAMObjective(A = "A", S = "S", F = "F", M = "M", thresholds = "thresh"), mxData(df, type = "raw")
#' use-case see: umxMakeThresholdMatrix
#'
#' @param df a dataframe
#' @param droplevels whether to droplevels or not
#' @param verbose how verbose to be
#' @return - list of matrices
#' @export
#' @family xmu internal not for end user
xmuMakeDeviationThresholdsMatrices <- function(df, droplevels, verbose) {
	# TODO xmuMakeDeviationThresholdsMatrices: Delete this function??
	isOrdinalVariable = umx_is_ordered(df) 
	if(sum(isOrdinalVariable) == 0){
		stop("no ordinal variables found")
	}
	ordinalColumns = df[,isOrdinalVariable, drop = FALSE]
	nOrdinal = ncol(ordinalColumns);
	ordNameList = names(ordinalColumns);
	levelList = rep(NA, nOrdinal)
	for(n in 1:nOrdinal) {
		levelList[n] = nlevels(ordinalColumns[, n])
	}
	maxThreshMinus1 = max(levelList) - 1
	# For Multiplication
	lowerOnes_for_thresh = mxMatrix(name = "lowerOnes_for_thresh", type = "Lower", nrow = maxThreshMinus1, ncol = maxThreshMinus1, free = FALSE, values = 1)
	# Threshold deviation matrix
	deviations_for_thresh = mxMatrix(name = "deviations_for_thresh", type = "Full", nrow = maxThreshMinus1, ncol = nOrdinal)
	initialLowerLim  = -1
	initialUpperLim  =  1
	# Fill first row of deviations_for_thresh with useful lower thresholds, perhaps -1 or .5 SD (nthresh/2)
	deviations_for_thresh$free[1,]   = TRUE
	deviations_for_thresh$values[1,] = initialLowerLim # Start with an even -2. Might spread this a bit for different levels, or centre on 0 for 1 threshold
	deviations_for_thresh$labels[1,] = paste("ThreshBaseline1", 1:nOrdinal, sep ="_")
	deviations_for_thresh$lbound[1,] = -7 # baseline limit in SDs
	deviations_for_thresh$ubound[1,] =  7 # baseline limit in SDs

	for(n in 1:nOrdinal){
		thisThreshMinus1 = levelList[n] -1
		stepSize = (initialUpperLim-initialLowerLim)/thisThreshMinus1
		deviations_for_thresh$values[2:thisThreshMinus1,n] = (initialUpperLim - initialLowerLim) / thisThreshMinus1
		deviations_for_thresh$labels[2:thisThreshMinus1,n] = paste("ThreshDeviation", 2:thisThreshMinus1, n, sep = "_")
		deviations_for_thresh$free  [2:thisThreshMinus1,n] = TRUE
		deviations_for_thresh$lbound[2:thisThreshMinus1,n] = .001
		if(thisThreshMinus1 < maxThreshMinus1) {
			# pad the shorter var's excess rows with fixed@99 so humans can see them...
			deviations_for_thresh$values[(thisThreshMinus1+1):maxThreshMinus1,n] <- (-99)

			deviations_for_thresh$labels[(thisThreshMinus1+1):maxThreshMinus1,n] <- paste("unusedThresh", min(thisThreshMinus1 + 1, maxThreshMinus1), n, sep = "_")
			deviations_for_thresh$free  [(thisThreshMinus1+1):maxThreshMinus1,n] <- F
		}
	}

	threshNames = paste0("Threshold", 1:maxThreshMinus1)
	thresholdsAlgebra = mxAlgebra(lowerOnes_for_thresh %*% deviations_for_thresh, dimnames = list(threshNames, ordNameList), name = "thresholdsAlgebra")
	if(verbose){
		cat("levels in each variable are:")
		print(levelList)
		print(paste("maxThresh - 1 = ", maxThreshMinus1))
	}
	return(list(lowerOnes_for_thresh, deviations_for_thresh, thresholdsAlgebra, mxRAMObjective(A="A", S="S", F="F", M="M", thresholds = "thresholdsAlgebra"), mxData(df, type = "raw")))
}


#' Make start values
#'
#' Purpose: Create startvalues for OpenMx paths
#' use cases
#' umx:::xmuStart_value_list(1)
#' umxValues(1) # 1 value, varying around 1, with sd of .1
#' umxValues(1, n=letters) # length(letters) start values, with mean 1 and sd .1
#' umxValues(100, 15)  # 1 start, with mean 100 and sd 15
#'
#' @param mean the mean start value
#' @param sd the sd of values
#' @param n how many to generate
#' @return - start value list
#' @export
#' @family xmu internal not for end user
#' @md
xmu_start_value_list <- function(mean = 1, sd = NA, n = 1) {
	# nb: bivariate length = n-1 recursive 1=0, 2=1, 3=3, 4=7 i.e., 
	if(is.na(sd)){
		sd = mean/6.6
	}
	if(length(n) > 1){
		n = length(n)
	}
	return(rnorm(n = n, mean = mean, sd = sd))
}

#' xmuPropagateLabels (not a user function)
#'
#' You should be calling [umxLabel()].
#' This function is called by xmuLabel_MATRIX_Model
#'
#' @param model a model to label
#' @param suffix a string to append to each label
#' @param verbose whether to say what is being done
#' @return - [mxModel()]
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 = mxModel("One Factor", type = "RAM", 
#' 	manifestVars = manifests, latentVars = latents, 
#' 	mxPath(from = latents  , to = manifests),
#' 	mxPath(from = manifests, arrows = 2),
#' 	mxPath(from = latents  , arrows = 2, free = FALSE, values = 1.0),
#' 	mxData(cov(demoOneFactor), type = "cov", numObs=500)
#' )
#'
#' m1 = umx:::xmuPropagateLabels(m1, suffix = "MZ")
xmuPropagateLabels <- function(model, suffix = "", verbose = TRUE) {
	model@matrices  <- lapply(model$matrices , xmuLabel_Matrix   , suffix = suffix, verbose = verbose)
	model@submodels <- lapply(model$submodels, xmuPropagateLabels, suffix = suffix, verbose = verbose)
	return(model)
}

#' xmuMI
#'
#' A function to compute and report modifications which would improve fit.
#' You will probably use [umxMI()] instead
#'
#' @param model an [mxModel()] to derive modification indices for
#' @param vector = Whether to report the results as a vector default = TRUE
#' @family xmu internal not for end user
#' @export
#' @md
xmuMI <- function(model, vector = TRUE) {
	# modification indices
	# v0.9: written Michael Culbertson
	# v0.91: up on github; added progress bar, Bates
	# https://openmx.ssri.psu.edu/thread/831
	# https://openmx.ssri.psu.edu/thread/1019
	# https://openmx.ssri.psu.edu/sites/default/files/mi-new.r
	steps <- 5
	bar <- txtProgressBar (min=0, max=steps, style=3)
    utils::setTxtProgressBar(bar, 1)
	accumulate <- function(A, B, C, D, d) {
		res <- matrix(0, d^2, d^2)    
		for (ii in 1:(d^2)){
			for (jj in ii:(d^2)){
				g <- 1 + (ii - 1) %% d
				h <- 1 + (ii - 1) %/% d
				i <- 1 + (jj - 1) %% d
				j <- 1 + (jj - 1) %/% d
				res[ii, jj] <- res[jj, ii] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
		}
		res
	}
	accumulate.asym <- function(A, B, C, D, d) {
		res <- matrix(0, d^2, d^2)    
		for (ii in 1:(d^2)){
			for (jj in 1:(d^2)){
				g <- 1 + (ii - 1) %% d
				h <- 1 + (ii - 1) %/% d
				i <- 1 + (jj - 1) %% d
				j <- 1 + (jj - 1) %/% d
				res[ii, jj] <- A[g, i] * B[h, j] + C[g, j] * D[h, i]
			}
		}
		res
	}
	A <- model$A$values
	P <- model$S$values
	S <- model$data$observed
	J <- model$F$values
	m <- dim(A)[1]
	which.free <- c(model$A$free, model$S$free & upper.tri(diag(m), diag= TRUE))
	vars       <- colnames(A)
	parNames   <- c(model$A$labels, model$S$labels)
	parNames[is.na(parNames)] <- c(outer(vars, vars, paste, sep=' <- '),
	outer(vars, vars, paste, sep=' <-> '))[is.na(parNames)]
	NM     <- model$data$numObs - 1
	I.Ainv <- solve(diag(m) - A) 
	C      <- J %*% I.Ainv %*% P %*% t(I.Ainv) %*% t(J)
	Cinv   <- solve(C)    
	AA     <- t(I.Ainv) %*% t(J)
	BB     <- J %*% I.Ainv %*% P %*% t(I.Ainv)
	correct <- matrix(2, m, m)
	diag(correct) <- 1
	grad.P <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% t(AA)
	grad.A <- correct * AA %*% Cinv %*% (C - S) %*% Cinv %*% BB 
	grad   <- c(grad.A, grad.P) * NM
	names(grad) <- parNames
	dF.dBdB <- accumulate(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% BB, AA %*% Cinv %*% BB, t(BB) %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 2)
	dF.dPdP <- accumulate(AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 3)
	dF.dBdP <- accumulate.asym(AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), AA %*% Cinv %*% t(AA), t(BB) %*% Cinv %*% t(AA), m)
    utils::setTxtProgressBar(bar, 4)
	correct.BB <- correct.PP <- correct.BP <- matrix(1, m^2, m^2)
	correct.BB[diag(m)==0, diag(m)==0] <- 2
	correct.PP[diag(m)==1, diag(m)==1] <- 0.5
	correct.PP[diag(m)==0, diag(m)==0] <- 2
	correct.BP[diag(m)==0, diag(m)==0] <- 2
	Hessian <- NM*rbind(cbind(dF.dBdB * correct.BB,    dF.dBdP * correct.BP),
	cbind(t(dF.dBdP * correct.BP), dF.dPdP * correct.PP))
	rownames(Hessian) <- parNames
	colnames(Hessian) <- parNames
	# list(gradient=grad[which.free], Hessian[which.free, which.free])

	hessian <- Hessian[which.free, which.free]
	E.inv <- solve(hessian)
	par.change <- mod.indices <- rep(0, 2*(m^2))                
	for (i in 1:(2*(m^2))) {
		k <- Hessian[i, i]
		d <- Hessian[i, which.free]
		par.change[i]  <- (-grad[i] / (k - d %*% E.inv %*% d))
		mod.indices[i] <- (-0.5 * grad[i] * par.change[i])
	}
	names(mod.indices) <- parNames
	names(par.change)  <- parNames
	if (vector) {
		which.ret <- c(!model$A$free & !diag(m), !model$S$free) # & upper.tri(diag(m), diag= TRUE))
		sel <- order(mod.indices[which.ret], decreasing= TRUE)
		ret <- list(mi=mod.indices[which.ret][sel], par.change=par.change[which.ret][sel])
	} else {
		mod.A <- matrix(mod.indices[1:(m^2)]   , m, m)
		mod.P <- matrix(mod.indices[-(1:(m^2))], m, m)
		par.A <- matrix(par.change[1:(m^2)]    , m, m)
		par.P <- matrix(par.change[-(1:(m^2))] , m, m)
		rownames(mod.A) <- colnames(mod.A) <- vars
		rownames(mod.P) <- colnames(mod.P) <- vars
		rownames(par.A) <- colnames(par.A) <- vars
		rownames(par.P) <- colnames(par.P) <- vars
		mod.A[model$A$free] <- NA
		par.A[model$A$free] <- NA
		diag(mod.A) <- NA
		diag(par.A) <- NA
		mod.P[model$S$free] <- NA
		par.P[model$S$free] <- NA
		ret <- list(mod.A=mod.A, par.A=par.A, mod.S=mod.P, par.S=par.P)
	}
    utils::setTxtProgressBar(bar, 5)
	close(bar)
	return(ret)
}

#' xmuHasSquareBrackets
#'
#' Tests if an input has square brackets
#'
#' @param input an input to test
#' @return - TRUE/FALSE
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmuHasSquareBrackets("A[1,2]")
xmuHasSquareBrackets <- function (input) {
    match1 <- grep("[", input, fixed = TRUE)
    match2 <- grep("]", input, fixed = TRUE)
    return(length(match1) > 0 && length(match2) > 0)
}

# ===================================
# = Ordinal/Threshold Model Helpers =
# ===================================

#' xmuMaxLevels
#'
#' Get the max levels from df
#'
#' @param df Dataframe to search through
#' @param what Either "value" or "name" ( of the max-level column)
#' @return - max number of levels in frame
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmuMaxLevels(mtcars) # NA = no ordinal vars
#' xmuMaxLevels(umxFactor(mtcars))
#' xmuMaxLevels(umxFactor(mtcars), what = "name")
xmuMaxLevels <- function(df, what = c("value", "name")) {
	what = match.arg(what)
	isOrd = umx_is_ordered(df)
	if(any(isOrd)){
		vars = names(df)[isOrd]
		nLevels = rep(NA, length(vars))
		j = 1
		for (i in vars) {
			nLevels[j] = length(levels(df[,i]))
			j = j + 1
		}	
		if(what == "value"){
			return(max(nLevels))
		} else {
			return(names(df)[which.max(nLevels)])
		}
	} else {
		return(NA)
	}
}

#' xmuMinLevels
#'
#' Get the min levels from df
#'
#' @param df Dataframe to search through
#' @param what Either "value" or "name" (of the min-level column)
#' @return - min number of levels in frame
#' @export
#' @family xmu internal not for end user
#' @md
#' @examples
#' xmuMinLevels(mtcars) # NA = no ordinal vars
#' xmuMinLevels(umxFactor(mtcars))
#' xmuMinLevels(umxFactor(mtcars), what = "name")
xmuMinLevels <- function(df, what = c("value", "name")) {
	what = match.arg(what)
	isOrd = umx_is_ordered(df)
	if(any(isOrd)){
		vars = names(df)[isOrd]
		nLevels = rep(NA, length(vars))
		j = 1
		for (i in vars) {
			nLevels[j] = length(levels(df[,i]))
			j = j + 1
		}
		if(what == "value"){
			return(min(nLevels))
		} else {
			return(names(df)[which.min(nLevels)])
		}
	} else {
		return(NA)
	}
}

# ===============
# = RAM helpers =
# ===============

#' Remove illegal characters from labels
#'
#' @description
#' Replaces . with _ in labels - e.g. from lavaan where . is common.
#'
#' @param label A label to clean.
#' @param replace character to replace . with (default = _)
#' @return - legal label string
#' @export
#' @family xmu internal not for end user
#' @seealso - [umxLabel()]
#' @md
#' @examples
#' xmu_clean_label("data.var", replace = "_")
#' xmu_clean_label("my.var.lab", replace = "_")
xmu_clean_label <- function(label, replace = "_") {
	if(length(namez(label, pattern = "^data\\.")) > 0){
		return(label) # defn var
	} else {
		return(namez(label, pattern = "\\.", replacement = replace, global = TRUE))
	}
}


#' xmuMakeTwoHeadedPathsFromPathList
#'
#' Make two-headed paths
#'
#' @param pathList A list of paths
#' @return - added items
#' @export
#' @family xmu internal not for end user
#' @md
xmuMakeTwoHeadedPathsFromPathList <- function(pathList) {
	a       = combn(pathList, 2)
	nVar    = dim(a)[2]
	toAdd   = rep(NA, nVar)
	n       = 1
	for (i in 1:nVar) {
		from = a[1,i]
		to   = a[2,i]
		if(match(to, pathList) > match(from, pathList)){
			labelString = paste0(to, "_with_", from)
		} else {
			labelString = paste0(from, "_with_", to)
		}
		toAdd[n] = labelString
		n = n+1
	}
	return(toAdd)
}

#' xmuMakeOneHeadedPathsFromPathList
#'
#' Make one-headed paths
#'
#' @param sourceList A sourceList
#' @param destinationList A destinationList
#' @return - added items
#' @export
#' @family xmu internal not for end user
#' @md
xmuMakeOneHeadedPathsFromPathList <- function(sourceList, destinationList) {
	toAdd   = rep(NA, length(sourceList) * length(destinationList))
	n       = 1
	for (from in sourceList) {
		for (to in destinationList) {
			labelString = paste0(from, "_to_", to)
			toAdd[n] = labelString
			n = n + 1
		}
	}
	return(toAdd)
}


# ====================
# = Graphviz helpers =
# ====================

#' Internal umx function to help plotting graphviz
#'
#' @description
#' Helper to print a digraph to file and open it
#' @param model An [mxModel()] to get the name from 
#' @param file Either "name" (use model name) or a file name
#' @param digraph Graphviz code for a model
#' @param strip_zero Whether to remove the leading "0." in digits in the diagram
#' @return - optionally returns the digraph text.
#' @family xmu internal not for end user
#' @family Graphviz
#' @md
xmu_dot_maker <- function(model, file, digraph, strip_zero= TRUE){
	if(strip_zero){
		# strip leading "0." (pad "0.5" to "50")
		# optionally negative number, with only 1 digit after the decimal
		digraph = umx_names(digraph, '(label ?= ?\\"-?)(0\\.)([0-9])\\"', replacement = "\\1\\30\"", global = TRUE)
		# optionally negative number, with only 1 or more digits after the decimal
		digraph = umx_names(digraph, '(label ?= ?\\"-?)(0\\.)([0-9]+)\\"', replacement = "\\1\\3\"", global = TRUE)
		# a1 -> ht1 [label = "0.92"];
	}

	if(!is.na(file)){
		if(file == "name"){
			file = paste0(model$name, ".", umx_set_plot_file_suffix(silent = TRUE))
		}
		cat(digraph, file = file) # write to file
		if(umx_set_plot_format(silent = TRUE) == "DiagrammeR"){
				# message("attempting plot")
				print(DiagrammeR::DiagrammeR(diagram = file, type = "grViz"))
		} else {
			if(umx_check_OS("OSX")){
				umx_open(file);
			} else if(umx_check_OS("Windows")){
				shell(paste0("dot -Tpdf -O ", shQuote(file)), "cmd.exe");
				umx_open(paste0(file, ".pdf"))
			} else {
				system(paste0("dot -Tpdf -O ", shQuote(file)));
				umx_open(paste0(file, ".pdf"))
			}
			# dot -Tpdf -O yourFilename.gv
			# creates "yourFilename.gv.pdf"
		}
	} else {
		return (cat(digraph));
	}
}


#' xmu_dot_move_ranks (not for end users)
#'
#' Variables will be moved from any existing rank to the new one. Setting a rank to "" will clear it.
#' @param min vars to group at top of plot
#' @param same vars to group at the same level
#' @param max vars to group at bottom of plot
#' @param old_min vars to group at top of plot
#' @param old_same vars to group at the same level
#' @param old_max vars to group at bottom of plot
#' @return - list(min=min, same=same, max=max)
#' @export
#' @family xmu internal not for end user
#' @family Graphviz
#' @md
#' @examples
#' old_min = c("min1", "min2")
#' old_same = c("s1", "s2")
#' old_max = paste0("x", 1:3)
#'
#' # Add L1 to min
#' xmu_dot_move_ranks(min = "L1", old_min= old_min, old_same= old_same, old_max= old_max)
#'
#' # Move min1 to max
#' xmu_dot_move_ranks(max = "min1", old_min= old_min, old_same= old_same, old_max= old_max)
#'
#' # Clear min
#' xmu_dot_move_ranks(min = "", old_min= old_min, old_same= old_same, old_max= old_max)
xmu_dot_move_ranks <- function(min = NULL, same = NULL, max = NULL, old_min, old_same, old_max) {
	# clear rank if set to ""
	if(identical(min , "")){ old_min  = c(); min  = c() }
	if(identical(same, "")){ old_same = c(); same = c() }
	if(identical(max , "")){ old_max  = c(); max  = c() }

	# Remove items in user's "max" from other lists...
	old_min  = setdiff(old_min,  max)
	old_max  = setdiff(old_max,  max)
	old_same = setdiff(old_same, max)
	# Remove items in user's "min" from other lists...
	old_min  = setdiff(old_min,  min)
	old_max  = setdiff(old_max,  min)
	old_same = setdiff(old_same, min)

	# Remove items in user's "same" from other lists...
	old_min  = setdiff(old_min,  same)
	old_max  = setdiff(old_max,  same)
	old_same = setdiff(old_same, same)

	# Append user to existing
	max  = c(old_max, max)
	min  = c(old_min, min)
	same = c(old_same, same)
	return(list(min=min, same=same, max=max))
}

#' xmu_dot_rank_str (not for end users)
#'
#'
#' @param min vars to group at top of plot
#' @param same vars to group at the same level
#' @param max vars to group at bottom of plot
#' @return - GraphViz rank string
#' @export
#' @family xmu internal not for end user
#' @family Graphviz
#' @examples
#' xmu_dot_rank_str(min = "L1", same = c("x1", "x2"), max = paste0("e", 1:3))
#' 
xmu_dot_rank_str <- function(min = NULL, same = NULL, max = NULL) {
	rankVariables = paste0("\t{rank=min; ", paste(min, collapse = "; "), "};\n")
	rankVariables = paste0(rankVariables, "\t{rank=same; ", paste(same, collapse = " "), "};\n")
	if(length(max) > 0){
		rankVariables = paste0(rankVariables, "\t{rank=max; ", paste(max, collapse = " "), "};\n")
	}
	return(rankVariables)
}


#' xmu_dot_make_residuals (not for end users)
#'
#'
#' @param mxMat An A or S mxMatrix 
#' @param latents Optional list of latents to alter location of circles (defaults to NULL)
#' @param fixed Whether to show fixed values or not
#' @param digits How many digits to report
#' @param resid How to show residuals and variances default is "circle". Other option is "line"
#' @return - list of variance names and variances
#' @export
#' @family xmu internal not for end user
#' @family Graphviz
xmu_dot_make_residuals <- function(mxMat, latents = NULL, fixed = TRUE, digits = 2, resid = c("circle", "line")) {
	mxMat_vals   = mxMat$values
	mxMat_free   = mxMat$free
	mxMat_labels = mxMat$labels
	mxMat_rows = dimnames(mxMat_free)[[1]]
	mxMat_cols = dimnames(mxMat_free)[[2]]

	variances = c()
	varianceNames = c()
	for(to in mxMat_rows ) { # rows
		lowerVars  = mxMat_rows[1:match(to, mxMat_rows)]
		for(from in lowerVars) { # columns
			thisPathLabel = mxMat_labels[to, from]
			thisPathFree  = mxMat_free[to, from]
			thisPathVal   = round(mxMat_vals[to, from], digits)

			if(thisPathFree){ prefix = "" } else { prefix = "@" }
			if(thisPathFree | (thisPathVal !=0 && fixed)) {
				if((to == from)) {
					if(resid =="circle"){
						if(from %in% latents){
							circleString = paste0(from, ' -> ', from, '[label="', prefix, thisPathVal, '", dir=both, headport=n, tailport=n]')
						} else {
							circleString = paste0(from, ' -> ', from, '[label="', prefix, thisPathVal, '", dir=both, headport=s, tailport=s]')
						}
						variances = append(variances, circleString)
					} else if(resid =="line"){
						varianceNames = append(varianceNames, paste0(from, '_var'))
						variances = append(variances, paste0(from, '_var [label="', prefix, thisPathVal, '", shape = plaintext]'))
					}					
				}
			}
		}
	}
	return(list(varianceNames = varianceNames, variances = variances))
}

#' xmu_dot_make_paths (not for end users)
#'
#' Makes graphviz paths
#'
#' @param mxMat An mxMatrix
#' @param stringIn Input string
#' @param heads 1 or 2 arrows (default NULL - you must set this)
#' @param fixed Whether show fixed values or not (defaults to TRUE)
#' @param comment A comment to include
#' @param showResiduals Whether to show residuals
#' @param labels show labels on the path? ("none", "labels", "both")
#' @param digits how many digits to report
#' @return - string
#' @export
#' @family xmu internal not for end user
#' @family Graphviz
xmu_dot_make_paths <- function(mxMat, stringIn, heads = NULL, fixed = TRUE, comment = "More paths", showResiduals = TRUE, labels = "labels", digits = 2) {
	if(is.null(heads)){
		stop("You must set 'heads' to 1 or 2 (was NULL)")
	}
	if(!heads %in% 1:2){
		stop("You must set 'heads' to 1 or 2: was ", heads)
	}
	mxMat_vals   = mxMat$values
	mxMat_free   = mxMat$free
	mxMat_labels = mxMat$labels
	mxMat_rows = dimnames(mxMat_free)[[1]]
	mxMat_cols = dimnames(mxMat_free)[[2]]
	if(!is.null(comment)){
		stringIn = paste0(stringIn, "\n\t# ", comment, "\n")
	}
	if(heads == 1){
		for(target in mxMat_rows ) {
			for(source in mxMat_cols) {
				thisPathLabel = mxMat_labels[target, source]
				thisPathFree  = mxMat_free[target, source]
				thisPathVal   = round(mxMat_vals[target, source], digits)

				labelStub = ' [label="'
				if(thisPathFree){
					prefix = "" 
				} else {
					prefix = "@"
				}

				if(thisPathFree | ((fixed & (thisPathVal != 0))) ) {
					# stringIn = paste0(stringIn, "\t", source, " -> ", target, labelStub, thisPathVal, '"];\n')
					if(labels == "both"){
						stringIn = paste0(stringIn, "\t", source, " -> ", target, labelStub, thisPathLabel, "=", prefix, thisPathVal, "\"];\n")
					} else if(labels == "labels"){
						stringIn = paste0(stringIn, "\t", source, " -> ", target, labelStub, thisPathLabel, "\"];\n")
					}else {
						# labels = "none"
						stringIn = paste0(stringIn, "\t", source, " -> ", target, labelStub, prefix, thisPathVal, "\"];\n")
					}
					
				}else{
					# not free and not non-0&&showfixed
					# print(paste0("thisPathFree = ", thisPathFree , "fixed =", fixed, "; thisPathVal = ", thisPathVal, "\n"))
				}
				
			}
		}
	} else {
		# heads = 2
		for(target in mxMat_rows ) { # rows
			lowerVars  = mxMat_rows[1:match(target, mxMat_rows)]
			for(source in lowerVars) { # columns
				thisPathLabel = mxMat_labels[target, source]
				thisPathFree  = mxMat_free[target, source]
				thisPathVal   = round(mxMat_vals[target, source], digits)

				if(thisPathFree){ prefix = "" } else { prefix = "@" }

				if(thisPathFree | ((fixed & (thisPathVal != 0))) ) {
					if(target == source) {
						if(showResiduals){
							stringIn = paste0(stringIn, "\t", source, "_var -> ", target, ";\n")
						}
					} else {
						if(labels == "both"){
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', thisPathLabel, "=", prefix, thisPathVal, "\"];\n")
						} else if(labels == "labels"){
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', thisPathLabel, "\"];\n")
						}else {
							# labels = "none"
							stringIn = paste0(stringIn, "\t", source, " -> ", target, ' [dir=both, label="', prefix, thisPathVal, "\"];\n")
						}
					}
				}
			}
		}
	}
	return(stringIn)
}

# handle sem-style strings

xmu_string2path <- function(from) {
	# TODO implement sem strings to umxPaths
	if(!is.null(from)){
		if(length(from) > 1){
			isSEMstyle = grepl("[<>]", x = from[1])	
		} else {
			isSEMstyle = grepl("[<>]", x = from)				
		}
		if(isSEMstyle){
			message("sem-style string syntax not yet implemented. In the mean time, try other features, such as fixedAt = , with, var, means = , fixFirst = ")
			# A with B; A to B
			if("from contains an arrow"){
				# parse into paths
			} else {
				if(!is.null(with)){
					to      = with
					arrows  = 2
					connect = "single"
				} else {
					to      = to
					arrows  = 1
					connect = "single"
				}
			}	
			a = "A ->B;A<-B; A>B; A --> B
			A<->B"
			# remove newlines, replacing with ;
			allOneLine = gsub("\n+", ";", a, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?^<-?>[ \t]?", "->", allOneLine, ignore.case = TRUE)
			# regularizedArrows = gsub("[ \t]?-?>[ \t]?", "<-", regularizedArrows, ignore.case = TRUE)
			pathList = umx_explode(";", allOneLine)
			for (aPath in pathList) {
				if(length(umx_explode("<->", aPath))==3){
					# bivariate
					parts = umx_explode("<->", aPath)
					# not finished, obviously...
					mxPath(from = umx_trim(parts[1]))
				} else if(length(umx_explode("->", aPath))==3){
					# from to
				} else if(length(umx_explode("<-", aPath))==3){
					# to from
				}else{
					# bad line
				}
			}
			umx_explode("", a)
		}
	}
}

#' Look up and report CIs for free parameters
#'
#' Look up CIs for free parameters in a model, and return as APA-formatted text string.
#' If std are available, then these are reported.
#'
#' @param model an [mxModel()] to get CIs from
#' @param label the label of the cell to interrogate for a CI, e.g. "ai_r1c1"
#' @param prefix The submodel to look in (default = "top.")
#' @param suffix The suffix for algebras when standardized (default = "_std")
#' @param SEstyle If TRUE, report "b(se)" instead of b CI95\[l,u\] (default = FALSE)
#' @param digits = 2
#' @param verbose = FALSE
#' @return - the CI string, e.g. ".73\[-.20, .98\]" or .73(.10)
#' @export
#' @family xmu internal not for end user
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
#' require(umx); data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#' m1 = umxCI(m1, run= "yes")
#' 
#' # Get CI by parameter label
#' xmu_get_CI(model= m1, "x1_with_x1")
#' xmu_get_CI(model= m1, "x1_with_x1", SEstyle=TRUE, digits=3)
#' 
#' # prefix (submodel) and suffix (e.g. std) are ignored if not needed
#' xmu_get_CI(model =m1, "x1_with_x1", prefix = "top.", suffix = "_std")
#' 
#' \dontrun{
#' xmu_get_CI(fit_IP, label = "ai_r1c1", prefix = "top.", suffix = "_std")
#' xmu_get_CI(fit_IP, label = "ai_r1c1", prefix = "top.", SEstyle = TRUE, suffix = "_std")
#' }
xmu_get_CI <- function(model, label, prefix = "top.", suffix = "_std", digits = 2, SEstyle = FALSE, verbose= FALSE){
	# xmu_get_CI ?
	# TODO xmu_get_CI: Look for CIs, if not found look for SEs, if not found compute with mxSE (high priority!)
	# TODO xmu_get_CI: Add choice of separator for CI (stash as preference) (easy)
	if(!umx_has_CIs(model)){
		if(verbose){
			message("no CIs")
		}
		return(NA)
	} else {
		# We want "top.ai_std[1,1]" from "ai_r1c1"
		result = tryCatch({
			CIlist = model$output$confidenceIntervals
			intervalNames = dimnames(CIlist)[[1]]
			if(label %in% intervalNames){
				# Easy case - the actual cell label was given, and will have been used by OpenMx to label the CI
				check = label
			}else{
				# Probably an auto-bracket-labelled CI e.g. "top.A_std[1,3]", in which case label would be "A_r1c3"
				grepStr = '^(.*)_r([0-9]+)c([0-9]+)$' # 1 = matrix names, 2 = row, 3 = column
				mat = sub(x = label, pattern = grepStr, replacement = '\\1', perl = TRUE);
				row = sub(x = label, pattern = grepStr, replacement = '\\2', perl = TRUE);
				col = sub(x = label, pattern = grepStr, replacement = '\\3', perl = TRUE);
				# prefix = "top."
				dimIndex    = paste0(prefix, mat, suffix, "[", row, ",", col, "]")
				dimNoSuffix = paste0(prefix, mat, "[", row, ",", col, "]")

				if(dimIndex %in% intervalNames){
					check = dimIndex
				} else {
					check = dimNoSuffix
				}
			}
			if(SEstyle){
				est = CIlist[check, "estimate"]
				if(is.na(CIlist[check, "lbound"])){
					# no lbound found: use ubound to form SE (SE not defined if ubound also NA :-(
					DIFF = (CIlist[check, "ubound"] - est)
				} else if (is.na(CIlist[check, "ubound"])){
					# lbound, but no ubound: use lbound to form SE
					DIFF = (est - CIlist[check, "lbound"])
				}else{
					# Both bounds present: average to get an SE
					DIFF = mean(c( (CIlist[check, "ubound"] - est), (est - CIlist[check, "lbound"]) ))
				}
			   APAstr = paste0(round(est, digits), " (", round(DIFF/(1.96 * 2), digits), ")")
			} else {
			   APAstr = paste0(
					umx_APA_pval(CIlist[check, "estimate"], min = -1, digits = digits), "[",
					umx_APA_pval(CIlist[check, "lbound"], min = -1, digits = digits)  , ",",
					umx_APA_pval(CIlist[check, "ubound"], min = -1, digits = digits)  , "]"
			   )
			}
		   return(APAstr) 
		}, warning = function(cond) {
			if(verbose){
				message(paste0("warning ", cond, " for CI ", omxQuotes(label)))
			}
		    return(NA) 
		}, error = function(cond) {
			if(verbose){
				message(paste0("error: ", cond, " for CI ", omxQuotes(label), "\n",
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
