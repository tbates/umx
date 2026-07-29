#   Copyright 2016-2026 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#        http://www.apache.org/licenses/LICENSE-2.0
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' Build a Double-Entry Censored Data ACE twin model
#'
#' @description
#' `umxACE_DE` implements a twin model for handling censored/floor-effect variables
#' using a "double-entry" strategy. Each censored variable is represented by a pair of columns:
#' one continuous (holding non-censored values) and one binary/ordinal (holding the censored indicator).
#' The path coefficients (A, C, E) are constrained to be equal across the two paired columns, 
#' and the specific factor loadings for the censored column are fixed to 0.
#'
#' Fully continuous variables may be mixed with double-entry pairs in `selDVs`
#' (e.g. `c("ht", "wt_cont", "wt_cens")`). Prep only the censored traits with
#' [umx_make_double_entry_data()]. At least one contiguous `_cont`/`_cens` pair is required;
#' for all-continuous models use [umxACE()].
#'
#' @details
#' Double-entry modeling represents a floor- or ceiling-censored trait using an adjacent pair of manifest columns:
#' one continuous (\code{_cont}) and one binary/ordinal factor (\code{_cens}).
#'
#' \strong{Likelihood Evaluation & Missingness Structure:}
#' Each individual contributes exactly one non-missing likelihood element for the censored trait:
#' \itemize{
#'   \item \strong{Non-censored cases} (\eqn{x > \textrm{cut}}): \code{_cont} contains the observed numeric value evaluated via the continuous normal density \eqn{f(x)}, while \code{_cens} is set to \code{NA}.
#'   \item \strong{Censored cases} (\eqn{x \le \textrm{cut}}): \code{_cont} is set to \code{NA}, while \code{_cens} contains the ordinal factor level evaluated via the cumulative threshold probability \eqn{P(Y \le \tau)}.
#' }
#' Leaving \code{_cens} non-missing for observed continuous rows would double-count the tail density (evaluating both \eqn{f(x)} and \eqn{P(Y > \tau)} for the same individual), introducing artificial covariance dependencies and inflating density estimates.
#'
#' @param name The name of the model (defaults to "ACE").
#' @param selDVs Base names of variables to model. Include fully observed continuous traits by base name (e.g. `"ht"`). Include each censored trait as an adjacent pair of prepped names (e.g. `"wt_cont", "wt_cens"`). Prep censored data with [umx_make_double_entry_data()] first.
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param doubleEntrySuffix Suffixes for the continuous and censored variables (default = c("_cont", "_cens")).
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param data If provided, dzData and mzData are treated as levels of zyg to select() MZ and DZ data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param nSib Number of siblings in a family (default = 2).
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param numObsDZ Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ Number of MZ twins: Set this if you input covariance data.
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param boundDiag Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @return - [OpenMx::mxModel()] of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umx_make_double_entry_data()], [umxACE()], [plot()], [umxSummary()], [umxModify()], [umxCompare()]
#' @examples
#' \donttest{
#' require(umx)
#' data(twinData)
#'
#' # Toy example: Height fully observed; weight recorded only for higher-BMI people (others coded 0).
#' # twinData BMI rarely exceeds classical overweight (25); use upper BMI quantile as threshold.
#' 
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10 # cms
#' bmiCut = quantile(c(twinData$bmi1, twinData$bmi2), probs = 0.8, na.rm = TRUE)
#' 
#' clinic = twinData
#' for (s in 1:2) {
#' 	bmiCol = paste0("bmi", s)
#' 	wtCol  = paste0("wt", s)
#' 	notWeighed = !is.na(clinic[[bmiCol]]) & clinic[[bmiCol]] < bmiCut
#' 	clinic[[wtCol]][notWeighed] = 0
#' }
#'
#' # Double-entry prep for weight (floor at 0) Creates "wt_cont*" and "wt_cens*"
#' prep = umx_make_double_entry_data(clinic, cols = list(wt = "<= 0"), sep = "")
#' 
#' mzData = prep[prep$zygosity %in% "MZFF", ]
#' dzData = prep[prep$zygosity %in% "DZFF", ]
#'
#' # 1. Correct mixed model: continuous height + double-entry censored weight
#' mDE = umxACE_DE(name = "htWtDE", mzData = mzData, dzData = dzData, sep = "",
#' 	selDVs = c("ht", "wt_cont", "wt_cens"), addCI = FALSE, tryHard = "yes"
#' )
#'
#' # Table: Standardized parameter estimates from DE Cholesky ACE
#' # |        |    a1|a2    |a3 |    c1|c2    |c3 |    e1|e2    |e3 |
#' # |:-------|-----:|:-----|:--|-----:|:-----|:--|-----:|:-----|:--|
#' # |ht      | 0.866|      |   |  0.35|      |   | 0.358|      |   |
#' # |wt_cont | 0.565|0.258 |   | -0.14|0.232 |   | 0.251|0.691 |   |
#' # |wt_cens | 0.565|0.258 |.  | -0.14|0.232 |.  | 0.251|0.691 |.  |
#' 
#' umxSummary(mDE, std = TRUE)
#'
#' # 2. Gold standard: uncensored bivariate ACE on true height and weight
#' mzTrue = twinData[twinData$zygosity %in% "MZFF", ]
#' dzTrue = twinData[twinData$zygosity %in% "DZFF", ]
#' mTrue  = umxACE("htWtTrue", selDVs = c("ht", "wt"), 
#'            mzData = mzTrue, dzData = dzTrue, sep = "", tryHard = "yes")
#' 
#' # Table: Standardized parameter estimates from 2-factor Cholesky ACE
#' # |   |    a1|a2    |     c1|c2 |    e1|e2    |
#' # |:--|-----:|:-----|------:|:--|-----:|:-----|
#' # |ht | 0.899|      |  0.252|   | 0.357|      |
#' # |wt | 0.419|0.766 | -0.049|.  | 0.074|0.479 |
#'
#' umxSummary(mTrue, std = TRUE)
#'
#' # 3. Mistaken analysis: treat 0 (not weighed) as a real continuous weight
#' mzNaive = clinic[clinic$zygosity %in% "MZFF", ]
#' dzNaive = clinic[clinic$zygosity %in% "DZFF", ]
#' mNaive = umxACE("htWtNaive0", selDVs= c("ht", "wt"), 
#'             mzData= mzNaive, dzData= dzNaive, sep ="",tryHard = "yes")
#' umxSummary(mNaive, std = TRUE)
#' # Naive mean(wt) pulled toward 0; prefer mDE when zeros mean censored.
#' }
umxACE_DE <- function(name = "ACE_DE", selDVs, selCovs = NULL, dzData = NULL, mzData = NULL, sep = "_T", data = NULL, zyg = "zygosity", type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, nSib = 2, dzAr = .5, dzCr = 1, weightVar = NULL, equateMeans = TRUE, addStd = TRUE, addCI = TRUE, doubleEntrySuffix = c("_cont", "_cens")) {
	tryHard = match.arg(tryHard)
	type    = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)

	if(dzCr == .25 & (name == "ACE_DE")){ name = "ADE_DE" }

	# if data provided create twin files 
	if(!is.null(data)){
		if(is.null(sep)){ sep = "_T" }
		# Avoid ingesting tibbles
		if("tbl" %in% class(data)){
			data = as.data.frame(data)
		}
		mzData = data[data[,zyg] %in% ifelse(is.null(mzData), "DZ", mzData), ]
		dzData = data[data[,zyg] %in% ifelse(is.null(dzData), "DZ", dzData), ]
	}else{
		# avoid ingesting tibbles
		if("tbl" %in% class(mzData)){
			mzData = as.data.frame(mzData)
			dzData = as.data.frame(dzData)
		}
	}

	# Find available columns in the datasets
	dataCols = NULL
	if (!is.null(data)) {
		dataCols = colnames(data)
	} else if (!is.null(mzData)) {
		dataCols = colnames(mzData)
	}

	# Parse selDVs: pure continuous base names and/or adjacent double-entry pairs
	s1 = doubleEntrySuffix[1]
	s2 = doubleEntrySuffix[2]
	doubleEntryPairs = list()
	i = 1
	while (i <= length(selDVs)) {
		v1 = selDVs[i]
		if (endsWith(v1, s1)) {
			# Continuous half of a double-entry pair: next name must be matching _cens
			if (i >= length(selDVs)) {
				stop("Polite note: Double-entry variable '", v1, "' needs an adjacent '", s2, "' partner (e.g. c(\"wt_cont\", \"wt_cens\")). Prep with umx_make_double_entry_data().")
			}
			v2 = selDVs[i + 1]
			if (!endsWith(v2, s2)) {
				stop("Polite note: '", v1, "' must be followed immediately by its '", s2, "' partner (got '", v2, "'). Prep with umx_make_double_entry_data().")
			}
			prefix1 = substr(v1, 1, nchar(v1) - nchar(s1))
			prefix2 = substr(v2, 1, nchar(v2) - nchar(s2))
			if (prefix1 != prefix2) {
				stop("Polite note: Double-entry pair prefixes must match: '", v1, "' and '", v2, "'. Prep with umx_make_double_entry_data().")
			}
			doubleEntryPairs[[length(doubleEntryPairs) + 1]] = c(v1, v2)
			i = i + 2
		} else if (endsWith(v1, s2)) {
			stop("Polite note: '", v1, "' must be preceded immediately by its '", s1, "' partner (e.g. c(\"wt_cont\", \"wt_cens\")). Prep with umx_make_double_entry_data().")
		} else {
			# Fully observed continuous trait (base name expanded via sep)
			i = i + 1
		}
	}
	if (length(doubleEntryPairs) < 1) {
		stop("Polite note: umxACE_DE requires at least one double-entry pair (e.g. c(\"wt_cont\", \"wt_cens\")). Prep censored traits with umx_make_double_entry_data(). For all-continuous ACE models use umxACE().")
	}

	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	selVars = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)

	model = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp   = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	nVar  = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL**

	if(nSib==2){
		expCovMZ = mxAlgebra(name = "expCovMZ", rbind(cbind(ACE,  AC), cbind( AC, ACE)), dimnames = list(selVars, selVars))
		expCovDZ = mxAlgebra(name = "expCovDZ", rbind(cbind(ACE, hAC), cbind(hAC, ACE)), dimnames = list(selVars, selVars))
	} else if (nSib==3) {
		expCovMZ = mxAlgebra(name="expCovMZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE,  AC, hAC),
			cbind(AC , ACE, hAC),
			cbind(hAC, hAC, ACE))
		)
		expCovDZ = mxAlgebra(name= "expCovDZ", dimnames = list(selVars, selVars), rbind(
			cbind(ACE, hAC, hAC),
			cbind(hAC, ACE, hAC),
			cbind(hAC, hAC, ACE))
		)
	}else{
		stop("3 sibs is experimental, but ", nSib, "? ... Maybe come back in 2030, best tim :-)")
	}
	
	top = mxModel(model$top,
		# Additive, Common, and Unique environmental paths				
		umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE),
		umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = tmp$varStarts, byrow = TRUE), 

		umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
		umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
		mxAlgebra(name = "A", a %*% t(a)), # Additive genetic variance
		mxAlgebra(name = "C", c %*% t(c)), # Common environmental variance
		mxAlgebra(name = "E", e %*% t(e)), # Unique environmental variance
		mxAlgebra(name = "ACE", A+C+E),
		mxAlgebra(name = "AC" , A+C  ),
		mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
		expCovMZ, expCovDZ
	)

	# Apply equality constraints for double-entry pairs in top model
	for (pair in doubleEntryPairs) {
		v1 = pair[1]
		v2 = pair[2]
		idx1 = which(selDVs == v1)
		idx2 = which(selDVs == v2)
		if (length(idx1) == 0 || length(idx2) == 0) next
		
		if (idx2 != idx1 + 1) {
			stop("Double entry variables in pair must be adjacent in selDVs: ", v1, " and ", v2)
		}
		
		for (matName in c("a", "c", "e")) {
			mat = top[[matName]]
			# Equate columns c <= idx1
			for (c in 1:idx1) {
				mat$labels[idx2, c] = mat$labels[idx1, c]
				mat$free[idx2, c] = mat$free[idx1, c]
			}
			# Fix element (idx2, idx2) to 0
			mat$free[idx2, idx2] = FALSE
			mat$values[idx2, idx2] = 0
			mat$labels[idx2, idx2] = as.character(NA)
			mat$lbound[idx2, idx2] = as.numeric(NA)
			mat$ubound[idx2, idx2] = as.numeric(NA)
			
			top[[matName]] = mat
		}
	}

	model = mxModel(model, top) 

	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be NULL, a value or a vector of values. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a$lbound = newLbound
			model$top$c$lbound = newLbound
			model$top$e$lbound = newLbound
			
			# Keep boundDiag off fixed 0 elements
			for (pair in doubleEntryPairs) {
				v2 = pair[2]
				idx2 = which(selDVs == v2)
				if (length(idx2) > 0) {
					model$top$a$lbound[idx2, idx2] = as.numeric(NA)
					model$top$c$lbound[idx2, idx2] = as.numeric(NA)
					model$top$e$lbound[idx2, idx2] = as.numeric(NA)
				}
			}
		}
	}
	if(addStd){
		newTop = mxModel(model$top,
			umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot", A + C+ E), # Total variance
			mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # total variance --> 1/SD
			mxAlgebra(name = "a_std", SD %*% a), # standardized a
			mxAlgebra(name = "c_std", SD %*% c), # standardized c
			mxAlgebra(name = "e_std", SD %*% e), # standardized e

			mxAlgebra(name = "A_std", SD %&% A), # standardized A
			mxAlgebra(name = "C_std", SD %&% C), # standardized C
			mxAlgebra(name = "E_std", SD %&% E)  # standardized E
		)
		model = mxModel(model, newTop)
	}
	if(addCI){
		if(addStd){
			model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
		}else{
			model = mxModel(model, mxCI(c('top.a', 'top.c', 'top.e')))
		}
	}
	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACE_DE") # set class so that S3 plot() and umxSummary dispatch
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE, intervals = intervals)
	return(model)
}

#' Prepare data for Double-Entry Censored Twin Models
#'
#' @description
#' Helper to split one or more variables in a twin dataset into a paired
#' continuous column (holding non-censored values) and an ordered factor column
#' (indicating censoring status), ready for \code{umxACE_DE}.
#'
#' @details
#' Double-entry data preparation creates paired continuous (\code{_cont}) and ordinal factor (\code{_cens}) columns for censored traits.
#'
#' To prevent likelihood double-counting during FIML estimation:
#' \itemize{
#'   \item Non-censored observations (\eqn{x > \textrm{cut}}) retain their numeric continuous score in \code{_cont}, while \code{_cens} is set to \code{NA}.
#'   \item Censored observations (\eqn{x \le \textrm{cut}}) have \code{_cont} set to \code{NA}, while \code{_cens} records the ordinal censored status level.
#' }
#' This ensures each case contributes exactly one mutually exclusive likelihood component (either continuous PDF or ordinal CDF threshold probability).
#'
#' @param data The dataframe to process.
#' @param cols A named list of variables and their censoring rules.
#' @param doubleEntrySuffix Suffixes for the continuous and censored columns (default = c("_cont", "_cens")).
#' @param sep Suffix/separator for twin indices (default = "_T").
#' @param nSib Number of siblings/twins (default = 2).
#' @param levels The factor levels for the censored column (default = c("continuous", "censored")).
#' @return The modified dataframe with expanded double-entry pairs.
#' @export
#' @family Twin Modeling Functions
#' @examples
#' data(twinData)
#' # Left-censor weight at 0 (or any floor): creates wt_cont1/2 and wt_cens1/2
#' prep = umx_make_double_entry_data(twinData, cols = list(wt = 0), sep = "")
#' # Then: umxACE_DE(selDVs = c("wt_cont", "wt_cens"), sep = "", ...)
#' # Mix with continuous traits: umxACE_DE(selDVs = c("ht", "wt_cont", "wt_cens"), ...)
umx_make_double_entry_data <- function(data, cols = NULL, doubleEntrySuffix = c("_cont", "_cens"), sep = "_T", nSib = 2, levels = c("cont", "censored")) {
	if (is.null(cols)) {
		return(data)
	}
	
	# Avoid ingesting tibbles
	if ("tbl" %in% class(data)) {
		data = as.data.frame(data)
	}
	
	for (varName in names(cols)) {
		rule = cols[[varName]]
		
		# Find if twin columns exist (e.g. v1_T1, v1_T2)
		twinColsFound = FALSE
		targetCols = character(0)
		suffixes = character(0)
		
		for (s in 1:nSib) {
			colName = paste0(varName, sep, s)
			if (colName %in% colnames(data)) {
				twinColsFound = TRUE
				targetCols = c(targetCols, colName)
				suffixes = c(suffixes, paste0(sep, s))
			}
		}
		
		# If no twin columns, fall back to single column (non-twin data)
		if (!twinColsFound && varName %in% colnames(data)) {
			targetCols = varName
			suffixes = ""
		}
		
		for (idx in seq_along(targetCols)) {
			colName = targetCols[idx]
			sSuffix = suffixes[idx]
			
			x = data[[colName]]
			cens = rep(FALSE, length(x))
			
			if (is.function(rule)) {
				cens = rule(x)
			} else if (is.numeric(rule)) {
				if (length(rule) == 1) {
					cens = (!is.na(x) & (x <= rule))
				} else if (length(rule) == 2) {
					cens = (!is.na(x) & (x >= rule[1] & x <= rule[2]))
				}
			} else if (is.character(rule)) {
				cleanRule = trimws(rule)
				if (grepl("\\bx\\b", cleanRule)) {
					cens = eval(parse(text = cleanRule))
				} else {
					cens = eval(parse(text = paste0("x ", cleanRule)))
				}
			}
			
			# Ensure NA values are preserved
			cens[is.na(x)] = NA
			
			# Generate continuous and censored column names
			if (sSuffix == "") {
				contCol = paste0(varName, doubleEntrySuffix[1])
				censCol = paste0(varName, doubleEntrySuffix[2])
			} else {
				# E.g. wt_cont_T1 and wt_cens_T1
				contCol = paste0(varName, doubleEntrySuffix[1], sSuffix)
				censCol = paste0(varName, doubleEntrySuffix[2], sSuffix)
			}
			
			# Create continuous column (NA if censored or missing)
			data[[contCol]] = ifelse(is.na(cens) | cens, NA_real_, x)
			
			# Create censored column (ordered factor: NA for continuous/non-censored rows)
			censFactor = ifelse(is.na(cens) | !cens, NA_character_, levels[2])
			data[[censCol]] = factor(censFactor, levels = levels, ordered = TRUE)
		}
	}
	
	return(data)
}

#' Plot a double-entry censored twin model (umxACE_DE)
#'
#' `umxPlotACE_DE` renders a GraphViz diagram for a [umxACE_DE()] model.
#' It automatically filters out the redundant continuous partner (`_cont`) of each double-entry pair,
#' plotting only the censored trait (`_cens`) and any unpaired continuous variables.
#'
#' @param x An [OpenMx::mxModel()] of class `MxModelACE_DE` to plot.
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model.
#' @param digits How many decimals to include in path loadings (default is 2).
#' @param means Whether to show means paths (default is FALSE).
#' @param std Whether to standardize the model (default is TRUE).
#' @param strip_zero Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE).
#' @param showFixed Whether to draw fixed parameters (default = FALSE).
#' @param ... Additional (optional) parameters passed to `xmu_dot_maker()`.
#' @return - optionally returns the dot code string.
#' @export
#' @family Plotting functions
#' @seealso - [plot()], [umxSummary()], [umxACE_DE()]
#' @references - <https://github.com/tbates/umx>
umxPlotACE_DE <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE, strip_zero = TRUE, showFixed = FALSE, ...) {
	model = x
	if(std){ model = xmu_standardize_ACE(model) }

	selDVs = dimnames(model$MZ$data$observed)[[2]]
	nVar   = dim(model$top$a$values)[[1]]
	allDVs = sub("(_T)?[0-9]$", "", selDVs[1:(nVar)])

	# Identify double-entry _cont variables to omit
	omitIdx = grep("_cont$", allDVs)
	if(length(omitIdx) > 0){
		keepIdx = setdiff(1:nVar, omitIdx)
	} else {
		keepIdx = 1:nVar
	}

	keepDVs = allDVs[keepIdx]

	nKeep = length(keepIdx)
	keepColIdx = 1:nKeep

	# Subset matrices to keepIdx (rows) and keepColIdx (cols) via umxMatrix
	aMat = umxMatrix("a", type = "Lower", nrow = nKeep, ncol = nKeep,
		free   = model$top$a$free[keepIdx, keepColIdx, drop = FALSE],
		values = model$top$a$values[keepIdx, keepColIdx, drop = FALSE],
		labels = model$top$a$labels[keepIdx, keepColIdx, drop = FALSE]
	)
	cMat = umxMatrix("c", type = "Lower", nrow = nKeep, ncol = nKeep,
		free   = model$top$c$free[keepIdx, keepColIdx, drop = FALSE],
		values = model$top$c$values[keepIdx, keepColIdx, drop = FALSE],
		labels = model$top$c$labels[keepIdx, keepColIdx, drop = FALSE]
	)
	eMat = umxMatrix("e", type = "Lower", nrow = nKeep, ncol = nKeep,
		free   = model$top$e$free[keepIdx, keepColIdx, drop = FALSE],
		values = model$top$e$values[keepIdx, keepColIdx, drop = FALSE],
		labels = model$top$e$labels[keepIdx, keepColIdx, drop = FALSE]
	)

	out = list(str = "", latents = c(), manifests = c())

	out = xmu_dot_mat2dot(aMat, cells = "lower_inc", from = "cols", toLabel = keepDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(cMat, cells = "lower_inc", from = "cols", toLabel = keepDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)
	out = xmu_dot_mat2dot(eMat, cells = "lower_inc", from = "cols", toLabel = keepDVs, fromType = "latent", toType = "manifest", arrows = "forward", showFixed = showFixed, p = out)

	if(means){
		if(!is.null(model$top$intercept)){
			interceptMat = umxMatrix("intercept", type = "Full", nrow = 1, ncol = nKeep,
				free   = model$top$intercept$free[1, keepIdx, drop = FALSE],
				values = model$top$intercept$values[1, keepIdx, drop = FALSE],
				labels = model$top$intercept$labels[1, keepIdx, drop = FALSE]
			)
			out = xmu_dot_mat2dot(interceptMat, cells = "left", toLabel = keepDVs, from = "rows", fromLabel = "one", fromType = "latent", toType = "manifest", showFixed = showFixed, p = out)
		}
	}


	preOut  = xmu_dot_define_shapes(latents = out$latents, manifests = out$manifests)
	same    = xmu_dot_rank(out$manifests, ".", rank = "same")
	top     = xmu_dot_rank(out$latents, "^a", rank = "min")
	bottom  = xmu_dot_rank(out$latents, "^[ce]", rank = "max")

	label   = model$name
	splines = "FALSE"
	digraph = paste0(
		"digraph G {\n\t",
		'label="', label, '";\n\t',
		"splines = \"", splines, "\";\n",
		preOut, out$str, same, top, bottom, "\n}"
	)

	xmu_dot_maker(model, file, digraph, strip_zero = strip_zero)
}

#' @rdname umxPlotACE_DE
#' @export
plot.MxModelACE_DE <- umxPlotACE_DE


#' Present results of a double-entry twin ACE model (umxACE_DE)
#'
#' Summarize a double-entry censored twin ACE model as returned by [umxACE_DE()].
#' Automatically filters out redundant `_cont` rows and fixed-zero factor columns from parameter tables,
#' while displaying all means (`_cont` mean and `_cens` threshold).
#'
#' @param model A fitted [OpenMx::mxModel()] of class `MxModelACE_DE` to summarize.
#' @param digits How many decimals to print (default = 2).
#' @param comparison Optional comparison model (default = NULL).
#' @param std Whether to standardize parameter estimates (default = TRUE).
#' @param showRg Whether to show genetic correlations (default = FALSE).
#' @param CIs Whether to compute and report confidence intervals (default = TRUE).
#' @param report Format to report tables: "markdown" or "html" (default = "markdown").
#' @param file The name of the dot file to write (default = getOption("umx_auto_plot")).
#' @param returnStd Whether to return standardized model (default = FALSE).
#' @param extended Whether to print raw path estimates alongside standardized (default = FALSE).
#' @param zero.print Character to print for zeroes (default = ".").
#' @param ... Additional arguments.
#' @return - optionally returns parameter estimates table dataframe.
#' @export
#' @family Reporting functions
#' @seealso - [umxACE_DE()], [umxPlotACE_DE()], [umxSummary()]
#' @references - <https://github.com/tbates/umx>
umxSummaryACE_DE <- function(model, digits = 2, comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")

	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryACE_DE(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, trim= TRUE, twinOneOnly= TRUE)
		nVar   = length(selDVs)

		# Identify double-entry _cont variables to omit
		omitIdx = grep("_cont$", selDVs)
		if(length(omitIdx) > 0){
			keepIdx = setdiff(1:nVar, omitIdx)
		} else {
			keepIdx = 1:nVar
		}
		nKeep      = length(keepIdx)
		keepColIdx = 1:nKeep
		keepDVs    = selDVs[keepIdx]

		a = mxEval(top.a, model) # Path coefficients
		c = mxEval(top.c, model)
		e = mxEval(top.e, model)
		A = mxEval(top.A, model) # Variances
		C = mxEval(top.C, model)
		E = mxEval(top.E, model)

		if(std){
			caption = paste0("Standardized parameter estimates from a ", nKeep, "-trait double-entry Cholesky ACE model. ")
			Vtot = A + C + E;            # Total variance
			I    = diag(nVar);           # nVar Identity matrix
			SD   = solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations

			a_std  = SD %*% a; # Standardized path coefficients
			c_std  = SD %*% c;
			e_std  = SD %*% e;
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			caption = paste0("Raw parameter estimates from a ", nKeep, "-trait double-entry Cholesky ACE model. ")
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA

		aSub = aClean[keepIdx, keepColIdx, drop = FALSE]
		cSub = cClean[keepIdx, keepColIdx, drop = FALSE]
		eSub = eClean[keepIdx, keepColIdx, drop = FALSE]

		Estimates = data.frame(cbind(aSub, cSub, eSub), row.names = keepDVs, stringsAsFactors = FALSE);

		if(model$top$dzCr$values == .25){
			colNames = c("a", "d", "e")
			caption = paste0(caption, "A: additive genetic; D: dominance effects; E: unique environment.")
		} else {
			colNames = c("a", "c", "e")
			caption = paste0(caption, "A: additive genetic; C: common environment; E: unique environment.")
		}
		names(Estimates) = paste0(rep(colNames, each = nKeep), rep(1:nKeep));
		umx_print(Estimates, digits = digits, caption = caption, report = report, zero.print = zero.print)
		xmu_twin_print_means(model = model, report = report)

		if(extended == TRUE) {
			aSubRaw = a[keepIdx, keepColIdx, drop = FALSE]
			cSubRaw = c[keepIdx, keepColIdx, drop = FALSE]
			eSubRaw = e[keepIdx, keepColIdx, drop = FALSE]
			aSubRaw[upper.tri(aSubRaw)] = NA
			cSubRaw[upper.tri(cSubRaw)] = NA
			eSubRaw[upper.tri(eSubRaw)] = NA
			unStandardizedEstimates = data.frame(cbind(aSubRaw, cSubRaw, eSubRaw), row.names = keepDVs);
			names(unStandardizedEstimates) = paste0(rep(colNames, each = nKeep), rep(1:nKeep));
			umx_print(unStandardizedEstimates, caption = "Unstandardized Cholesky ACE model path coefficients", digits = digits, zero.print = zero.print)
		}

		hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			message("Creating CI-based report!")
			CIlist = data.frame(model$output$confidenceIntervals)
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			CIlist$fullName = row.names(CIlist)

			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			a_CI = c_CI = e_CI = matrix(NA, rows, cols)

			labelList = imxGenerateLabels(model)
			rowCount  = dim(CIlist)[1]
			for(n in 1:rowCount) {
				thisName = row.names(CIlist)[n]
				if(!umx_has_square_brackets(thisName)) {
					nameParts = labelList[which(row.names(labelList) == thisName),]
					CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
				}
				fullName = CIlist$fullName[n]

				thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName)
				thisMatrixRow  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\1", x = fullName))
				thisMatrixCol  = as.numeric(sub(".*\\[(.*),(.*)\\]", replacement = "\\2", x = fullName))
				CIparts    = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString = paste0(CIparts[1], " [",CIparts[2], commaSep, CIparts[3], "]")

				if(grepl("^a", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^c", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^e", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				}
			}

			a_CISub = a_CI[keepIdx, keepColIdx, drop = FALSE]
			c_CISub = c_CI[keepIdx, keepColIdx, drop = FALSE]
			e_CISub = e_CI[keepIdx, keepColIdx, drop = FALSE]

			Estimates = data.frame(cbind(a_CISub, c_CISub, e_CISub), row.names = keepDVs, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nKeep), rep(1:nKeep));
			umx_print(Estimates, digits = digits, zero.print = zero.print, report=report, file = "tmpCI.html")
			xmu_twin_print_means(model, digits = digits, report = report)
		}
	}

	if(!is.na(file)) {
		umxPlotACE_DE(model, file = file, std = std)
	}
	if(returnStd) {
		xmu_standardize_ACE(model)
	}else{
		invisible(Estimates)
	}
}

#' @export
umxSummary.MxModelACE_DE <- umxSummaryACE_DE

