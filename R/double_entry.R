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
#' The path coefficients (A, C, E) are constrained to be equal across the two paired columns.
#'
#' Fully continuous variables may be mixed with double-entry pairs in `selDVs`
#' (e.g. `c("ht", "wt_cont", "wt_cens")`). Prepare the censored traits with
#' [umx_make_double_entry_data()]. At least one contiguous `_cont`/`_cens` pair is required;
#' for all-continuous models use [umxACE()].
#' 
#' **"Trust the Science" note**
#'
#' If you know the censoring value, keep fixCensorThresholds = "yes" / "auto" to lock that in.
#' Estimated-free threshold can "wander" very wildly. Reserve it only for unknown cut, and even then treat 
#' estimated thresholds with suspicion. Free threshold will "run" but it is **not great** at 
#' recovering the true floor/threshold.
#'
#' @details
#' Double-entry modeling represents a floor- or ceiling-censored trait using an adjacent pair of manifest columns:
#' one continuous (\code{_cont}) and one binary/ordinal factor (\code{_cens}).
#'
#' *Likelihood Evaluation & Missingness Structure:*
#' Each individual contributes exactly one non-missing likelihood element for the censored trait:
#' 
#'  * **Non-censored cases** (\eqn{x > \textrm{cut}}): \code{_cont} contains the observed numeric value evaluated via the continuous normal density \eqn{f(x)}, while \code{_cens} is set to \code{NA}.
#'  * **Censored cases** (\eqn{x \le \textrm{cut}}): \code{_cont} is set to \code{NA}, while \code{_cens} contains the ordinal factor level evaluated via the cumulative threshold probability \eqn{P(Y \le \tau)}.
#' }
#' Leaving `_cens` non-missing for observed continuous rows would double-count the tail density (evaluating both \eqn{f(x)} and \eqn{P(Y > \tau)} for the same individual), introducing artificial covariance dependencies and inflating density estimates.
#'
#' @param name The name of the model (defaults to "ACE").
#' @param selDVs Base names of variables to model. Include fully observed continuous traits by base name (e.g. `"ht"`). Include each censored trait as an adjacent pair of prepped names (e.g. `"wt_cont", "wt_cens"`). Prep censored data with [umx_make_double_entry_data()] first.
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param fixCensorThresholds One of \code{"yes"} (default), \code{"auto"}, or \code{"no"}.
#'  \code{"yes"}: fix every double-entry pair from \code{censorCuts} and/or prep attribute \code{umxDoubleEntry}. \code{"auto"}: fix only pairs with a finite known cut in \code{censorCuts} or the prep attribute.  \code{"no"}: free binary thresholds.
#' @param censorCuts Optional named numeric vector of known cuts on the \strong{analysis scale}
#'   (after any scaling). Names may be trait base (\code{"wt"}), continuous base (\code{"wt_cont"}),
#'   or censored base (\code{"wt_cens"}). When non-\code{NULL}, fixCensorThresholds must be \code{"yes"} or \code{"auto"};
#'   only the named pairs are fixed (partial \code{censorCuts} is allowed).
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
#' require(umx)
#'
#' ##################################
#' # Toy example 1: Threshold known #
#' ##################################
#'
#' # Weighing scales lowest value is 60kg, with lower values registering as 60kg.
#'
#' # NOTE: We first put height on a scale comparable to weight to ease estimation
#' data(twinData)
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] *100 # metre->cm
#' 
#' clinic = twinData
#' clinic$wt1[!is.na(twinData$wt1) & (twinData$wt1 <= 60)] = 60
#' clinic$wt2[!is.na(twinData$wt2) & (twinData$wt2 <= 60)] = 60
#'
#' # Double-entry prep for weight (floor at 0) Creates "wt_cont*" and "wt_cens*"
#' prep = umx_make_double_entry_data(clinic, cols = list(wt = "<= 60"), sep = "")
#' 
#' mzData = prep[prep$zygosity %in% "MZFF", ]
#' dzData = prep[prep$zygosity %in% "DZFF", ]
#'
#' \donttest{
#' # 1. Correct mixed model: continuous height + double-entry censored weight
#' # Default: free thresholds. For known LOD, use fixCensorThresholds = "auto" or "yes"
#' # with censorCuts = c(wt = 0) (cut on analysis scale; means cont=cens equated).
#' 
#' mDE = umxACE_DE(name = "htWtDE", mzData = mzData, dzData = dzData, sep = "",
#' 	selDVs = c("ht", "wt_cont", "wt_cens"), tryHard = "yes")
#'
#' # Table: Model Fit Summary for 'htWtDE'
#' # 
#' # |Model  | EP|  -2LL|   df|   AIC|      BIC|
#' # |:------|--:|-----:|----:|-----:|--------:|
#' # |htWtDE | 11| 37082| 7781| 37104| 37165.51|
#'
#'
#' # Table: Standardized parameter estimates from a 2-trait double-entry Cholesky ACE model.
#' # A: additive genetic; C: common environment; E: unique environment.
#' # 
#' # |        |    a1|a2    |    c1|c2    |     e1|e2    |
#' # |:-------|-----:|:-----|-----:|:-----|------:|:-----|
#' # |ht      | 0.874|      | 0.373|      |  0.312|      |
#' # |wt_cens | 0.437|0.381 | 0.079|0.301 | -0.148|0.738 |
#' # Double-entry thresholds fixed: wt_cens @ 60 (means equated to wt_cont).
#'
#'
#' # Table: Means (from model$top$expMean)
#' # 
#' # |          |     ht1| wt_cont1| wt_cens1|     ht2| wt_cont2| wt_cens2|
#' # |:---------|-------:|--------:|--------:|-------:|--------:|--------:|
#' # |intercept | 162.499|   56.961|   56.961| 162.499|   56.961|   56.961|
#'
#'
#' ####################################
#' # Toy example 2: Threshold unknown #
#' ####################################
#'
#' # GP only gets paid to record weight for higher-BMI people (~BMI 22 here).
#' # Others are coded "0").
#' # NOTE: We don't know what threshold 0 mapped to, so we *estimate* the threshold.
#' # NOTE: This example is contrived, and creates a nasty collider with height.
#' 
#' # Height fully observed; Weighing scales lowest value is 60kg, with all
#' # values beneath this registering as 60kg.
#'
#' # NOTE: First we get the height data on a comparable scale to weight to ease model estimation
#' data(twinData)
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 100 # metres -> cms
#'
#' bmiCut = 22 # about the 80th percentile in this sample.
#' 
#' clinic = twinData
#' clinic$wt1[!is.na(twinData$wt1) & (twinData$bmi1 <= 22)] = 0
#' clinic$wt2[!is.na(twinData$wt2) & (twinData$bmi2 <= 22)] = 0
#'
#' # Double-entry prep for weight (floor at 0) Creates "wt_cont*" and "wt_cens*"
#' prep = umx_make_double_entry_data(data = clinic, cols = list(wt = 0), sep = "")
#' 
#' mzData = prep[prep$zygosity %in% "MZFF", ]
#' dzData = prep[prep$zygosity %in% "DZFF", ]
#'
#' # 1. Correct mixed model: continuous height + double-entry censored weight
#' # Default: free thresholds. For known LOD, use fixCensorThresholds = "auto" or "yes"
#' # with censorCuts = c(wt = 0) (cut on analysis scale; means cont=cens equated).
#' 
#' # Note how we set fixCensorThresholds = "no"
#'
#' mDE = umxACE_DE(name = "htWtDE", mzData = mzData, dzData = dzData, sep = "",
#' 	selDVs = c("ht", "wt_cont", "wt_cens"), fixCensorThresholds="no", tryHard = "yes")
#'
#' # Table: Model Fit Summary for 'htWtDE'
#' # 
#' # |Model  | EP|     -2LL|   df|      AIC|      BIC|
#' # |:------|--:|--------:|----:|--------:|--------:|
#' # |htWtDE | 12| 32406.96| 7780| 32430.96| 32498.06|
#' # 
#' # 
#' # Table: Standardized parameter estimates from a 2-trait double-entry Cholesky ACE model. 
#' #   A: additive genetic; C: common environment; E: unique environment.
#' # 
#' # |        |    a1|a2    |    c1|c2    |    e1|e2    |
#' # |:-------|-----:|:-----|-----:|:-----|-----:|:-----|
#' # |ht      | 0.883|      | 0.268|      | 0.385|      |
#' # |wt_cens | 0.228|0.105 | 0.628|0.235 | 0.180|0.675 |
#' # 
#' # 
#' # Table: Means (from model$top$expMean)
#' # 
#' # |          |     ht1| wt_cont1|wt_cens1 |     ht2| wt_cont2|wt_cens2 |
#' # |:---------|-------:|--------:|:--------|-------:|--------:|:--------|
#' # |intercept | 162.918|   68.623|0        | 162.918|   68.623|0        |
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
#' #   A: additive genetic; C: common environment; E: unique environment.
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
#' 
#' # Naive mean(wt) pulled toward 0, covariance obscured; prefer mDE when zeros mean censored.
#' #
#' # Table: Standardized parameter estimates from a 2-factor Cholesky ACE model.
#' #   A: additive genetic; C: common environment; E: unique environment.
#' #
#' # |   |    a1|a2    |     c1|c2 |     e1|e2    |
#' # |:--|-----:|:-----|------:|:--|------:|:-----|
#' # |ht | 0.899|      |  0.252|   |  0.357|      |
#' # |wt | 0.057|0.717 | -0.238|.  | -0.072|0.649 |
#' #
#' #
#' # Table: Means (from model$top$expMean)
#' # 
#' # |          |     ht1|    wt1|     ht2|    wt2|
#' # |:---------|-------:|------:|-------:|------:|
#' # |intercept | 162.515| 20.984| 162.515| 20.984|
#' }
umxACE_DE <- function(name = "ACE_DE", selDVs, selCovs = NULL, dzData = NULL, mzData = NULL, sep = "_T", data = NULL, zyg = "zygosity", fixCensorThresholds = c("yes", "auto", "no"), censorCuts = NULL, doubleEntrySuffix = c("_cont", "_cens"), type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, nSib = 2, dzAr = .5, dzCr = 1, weightVar = NULL, equateMeans = TRUE, addStd = TRUE, addCI = TRUE) {
	tryHard = match.arg(tryHard)
	type    = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	fixCensorThresholds = match.arg(fixCensorThresholds)

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

	# Apply equality constraints for double-entry pairs in top model.
	# Contiguous pair (cont, cens): same loadings; no residual on cens; entire "cens"
	# factor column fixed at 0 (otherwise multi-DE leaves phantom free columns).
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
			# Equate cens row to cont row for free loadings into this pair
			for (c in 1:idx1) {
				mat$labels[idx2, c] = mat$labels[idx1, c]
				mat$free[idx2, c]   = mat$free[idx1, c]
				mat$values[idx2, c] = mat$values[idx1, c]
			}
			# Zero entire lower-triangular column idx2 (phantom factor for multi-DE)
			for (r in idx2:nVar) {
				mat$free[r, idx2] = FALSE
				mat$values[r, idx2] = 0
				mat$labels[r, idx2] = as.character(NA)
				mat$lbound[r, idx2] = as.numeric(NA)
				mat$ubound[r, idx2] = as.numeric(NA)
			}
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
			
			# Keep boundDiag off fixed-zero DE cens columns
			for (pair in doubleEntryPairs) {
				idx2 = which(selDVs == pair[2])
				if (length(idx2) > 0) {
					for (r in idx2:nVar) {
						model$top$a$lbound[r, idx2] = as.numeric(NA)
						model$top$c$lbound[r, idx2] = as.numeric(NA)
						model$top$e$lbound[r, idx2] = as.numeric(NA)
					}
				}
			}
		}
	}
	if(addStd){
		newTop = mxModel(model$top,
			umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
			mxAlgebra(name = "Vtot" , A + C+ E), # Total variance
			mxAlgebra(name = "SD"   , solve(sqrt(I * Vtot))), # total variance --> 1/SD
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
	# --- DE identification (post-supermodel; do not edit xmuTwinSuper_SomeBinary) ---
	# Stock binary packaging forces mean@0 and Vtot ==1 on _cens; path equating then
	# forces continuous partner variance to 1 (wrong for kg/cm). For every DE pair:
	#   - release Vtot==1 so trait variance is free
	#   - free shared mean cont=cens (one location in data units)
	#   - if known cut: fix threshold at c; else leave threshold free (estimate cut in data units)
	censorMeta = xmu_ace_de_parse_censor_meta(
		mzData = mzData,
		dzData = dzData,
		doubleEntryPairs = doubleEntryPairs,
		fixCensorThresholds = fixCensorThresholds,
		censorCuts = censorCuts,
		doubleEntrySuffix = doubleEntrySuffix
	)
	if (length(censorMeta$fixedCuts) > 0 && !is.null(selCovs)) {
		stop("Polite note: Fixed double-entry censor thresholds (fixCensorThresholds / censorCuts) are not supported with selCovs in this version. Fit without covariates, or leave thresholds free (fixCensorThresholds = \"no\").")
	}
	# Map every DE pair cont/cens bases for mean equate + V release
	allContByCens = character(0)
	allCensBases = character(0)
	for (pair in doubleEntryPairs) {
		allContByCens[pair[2]] = pair[1]
		allCensBases = c(allCensBases, pair[2])
	}
	# Fixed cuts only for named pairs; free tau for other DE pairs
	fixedCuts = censorMeta$fixedCuts
	# Optional free-tau starts from prep cuts (even when not fixing)
	freeTauStarts = numeric(0)
	attrList = attr(mzData, "umxDoubleEntry")
	if (is.null(attrList)) attrList = attr(dzData, "umxDoubleEntry")
	if (!is.null(attrList$pairs)) {
		for (p in attrList$pairs) {
			if (isTRUE(p$fixable) && is.finite(p$cut) && p$cens %in% names(allContByCens)) {
				freeTauStarts[p$cens] = as.numeric(p$cut)
			}
		}
	}
	model = xmu_ace_de_apply_censor_thresholds(
		model = model,
		fixedCuts = fixedCuts,
		contByCens = allContByCens,
		selDVs = selDVs,
		sep = sep,
		nSib = nSib,
		equateMeansWithCont = TRUE,
		freeVariance = TRUE,
		equateMeansForAllPairs = TRUE,
		freeTauStarts = freeTauStarts
	)
	for (censBase in names(fixedCuts)) {
		message("umx note: fixed double-entry threshold for ", censBase, " at ", fixedCuts[[censBase]], " (means cont=cens; trait variance free).")
	}
	freeThreshPairs = setdiff(allCensBases, names(fixedCuts))
	if (length(freeThreshPairs) > 0) {
		message("umx note: free double-entry threshold(s) for ", paste(freeThreshPairs, collapse = ", "), " (means cont=cens; trait variance free -- not binary V=1).")
	}

	# Trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelACE_DE") # set class so that S3 plot() and umxSummary dispatch
	# as() strips custom attributes -- set umxDE metadata only after cast
	attr(model, "umxDE") = list(
		fixedCensorThresholds = length(fixedCuts) > 0,
		fixedCuts = fixedCuts,
		contByCens = allContByCens,
		equateMeansWithCont = TRUE,
		freeVariance = TRUE,
		sideByCens = censorMeta$sideByCens,
		freeThresholdPairs = freeThreshPairs
	)
	xmu_threshold_id_twin_check(model, fullVars = selVars, verbose = TRUE)
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE, intervals = intervals)
	# Re-attach metadata if run replaced the object without attrs (mxRun usually preserves)
	if (is.null(attr(model, "umxDE"))) {
		attr(model, "umxDE") = list(
			fixedCensorThresholds = length(fixedCuts) > 0,
			fixedCuts = fixedCuts,
			contByCens = allContByCens,
			equateMeansWithCont = TRUE,
			freeVariance = TRUE,
			sideByCens = censorMeta$sideByCens,
			freeThresholdPairs = freeThreshPairs
		)
	}
	return(model)
}

#' Prepare data for Double-Entry Censored Twin Models
#'
#' @description
#' Helper to split one or more variables in a twin dataset into a paired
#' continuous column (holding non-censored values) and an ordered factor column
#' (indicating censoring status), ready for \code{umxACE_DE}.
#' 
#' The function has flexible rules for censoring. You give a `list()` of columns, with the censoring rule for each, and the function creates new matching `var_cens` and `var_cont` columns. A rule can be a simple numeric value to cut at, e.g., `list(wt=0)`) for cut column `wt` at zero. More complex expressions should be in quotes `list(wt="<= cut")`.
#' **Mutual-NA invariant enforced**: Each row contributes exactly *one* non-missing element per DE trait — continuous density `f(x)` or threshold CDF `P(Y≤τ)` — preventing double-counting. 
#' \eqn{P(Y <= \tau)}
#' On non-censored rows, the function sets the var_cont to the observed value and the var_cens value to NA. On censored rows, the continuous column is set to NA, and the correct value of an ordered factor `c("censored","observed")` is set in var_cens (integer 1). Right-censor `>=cut` flips levels to `c("observed","censored")` so censored = integer 2 (upper tail). `NA` in raw propagates to `NA` in both.
#'
#' @details
#' Double-entry data preparation creates paired continuous (\code{_cont}) and ordinal factor (\code{_cens}) columns for censored traits.
#'
#' To prevent likelihood double-counting during FIML estimation:
#'
#' * Non-censored observations (\eqn{x > \textrm{cut}}) retain their numeric continuous score in \code{_cont}, while \code{_cens} is set to \code{NA}.
#' * Censored observations (\eqn{x \le \textrm{cut}}) have \code{_cont} set to \code{NA}, while \code{_cens} records the ordinal censored status level.
#'
#' This ensures each case contributes exactly one mutually exclusive likelihood component (either continuous PDF or ordinal CDF threshold probability).
#'
#' @param data The dataframe to process.
#' @param cols A named list of variables and their censoring rules. A numeric scalar is left-censoring at that floor;
#'   length-2 numeric is interval; character comparison (e.g. \code{"<= 0"}, \code{">= 40"}) or a function are also allowed.
#'   Known finite bounds are stored in attribute \code{umxDoubleEntry} for optional use by [umxACE_DE()] with
#'   \code{fixCensorThresholds = "auto"} or \code{"yes"}.
#' @param doubleEntrySuffix Suffixes for the continuous and censored columns (default = c("_cont", "_cens")).
#' @param sep Suffix/separator for twin indices (default = "_T").
#' @param nSib Number of siblings/twins (default = 2).
#' @param levels Ordered factor levels for the censored column. \code{NULL} (default) chooses by censor side:
#'   left/interval/unknown use \code{c("censored", "observed")}; right uses \code{c("observed", "censored")}.
#'   If supplied, must include the level name \code{"censored"} (assigned by name when censored, never by index alone).
#' @return The modified dataframe with expanded double-entry pairs and attribute \code{umxDoubleEntry}
#'   listing per-trait cut, side, and whether the cut is fixable in a model.
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxACE_DE()], [umxACE()], [plot()], [umxSummary()], [umxModify()], [umxCompare()]
#' @examples
#' a = 2+2
#' data(twinData)
#' prep = umx_make_double_entry_data(twinData, cols = list(wt = 0), sep = "")
#' # attr(prep, "umxDoubleEntry")$pairs[[1]]$cut  # 0
#' 
#' \dontrun{
#' # Then
#' selDVs = c("ht", "wt_cont", "wt_cens")
#' # Known LOD
#' umxACE_DE(data = prep, selDVs = selDVs, sep = "")
#' # Un-known LOD
#' umxACE_DE(data = prep, selDVs = selDVs, sep = "", fixCensorThresholds = "no")
#' # Mix with continuous traits:
#' umxACE_DE(selDVs = c("ht", "wt_cont", "wt_cens"), ...)
#' }
umx_make_double_entry_data <- function(data, cols = NULL, doubleEntrySuffix = c("_cont", "_cens"), sep = "_T", nSib = 2, levels = NULL) {
	if (is.null(cols)) {
		return(data)
	}
	# Avoid ingesting tibbles
	if ("tbl" %in% class(data)) {
		data = as.data.frame(data)
	}

	userLevels = levels
	pairMeta = list()
	
	for (varName in names(cols)) {
		rule = cols[[varName]]
		parsed = xmu_ace_de_parse_censor_rule(rule)
		side = parsed$side

		if (is.null(userLevels)) {
			if (identical(side, "right")) {
				levelVec = c("observed", "censored")
			} else {
				# left, interval, unknown: censored as lower category for left-censor Tobit
				levelVec = c("censored", "observed")
			}
		} else {
			levelVec = userLevels
			if (!("censored" %in% levelVec)) {
				stop("Polite note: levels must include the name \"censored\" (got: ", paste(levelVec, collapse = ", "), ").")
			}
		}
		
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
			
			# Censored column: assign censored state by level name (not levels[2])
			censFactor = ifelse(is.na(cens) | !cens, NA_character_, "censored")
			data[[censCol]] = factor(censFactor, levels = levelVec, ordered = TRUE)
		}

		pairMeta[[length(pairMeta) + 1]] = list(
			base = varName,
			cont = paste0(varName, doubleEntrySuffix[1]),
			cens = paste0(varName, doubleEntrySuffix[2]),
			cut = parsed$cut,
			side = parsed$side,
			ruleRepr = parsed$ruleRepr,
			fixable = parsed$fixable
		)
	}

	attr(data, "umxDoubleEntry") = list(
		version = 1L,
		doubleEntrySuffix = doubleEntrySuffix,
		sep = sep,
		nSib = nSib,
		pairs = pairMeta
	)
	
	return(data)
}

#' Parse a double-entry censoring rule into cut, side, and fixable flag
#'
#' Character cuts use a strict regex grammar (no \code{eval}). Indicator evaluation for
#' arbitrary expressions remains in [umx_make_double_entry_data()].
#'
#' @param rule Numeric scalar/length-2, character comparison, or function.
#' @return list with \code{side}, \code{cut}, \code{fixable}, \code{ruleRepr}.
#' @family xmu internal not for end user
xmu_ace_de_parse_censor_rule <- function(rule) {
	ruleRepr = if (is.function(rule)) {
		"<function>"
	} else if (is.character(rule)) {
		paste(rule, collapse = " ")
	} else {
		paste(deparse(rule, width.cutoff = 500L), collapse = " ")
	}

	if (is.function(rule)) {
		return(list(side = "unknown", cut = NA_real_, fixable = FALSE, ruleRepr = ruleRepr))
	}
	if (is.numeric(rule)) {
		if (length(rule) == 1L) {
			cutVal = as.numeric(rule)
			return(list(side = "left", cut = cutVal, fixable = is.finite(cutVal), ruleRepr = ruleRepr))
		}
		if (length(rule) == 2L) {
			return(list(side = "interval", cut = NA_real_, fixable = FALSE, ruleRepr = ruleRepr))
		}
		return(list(side = "unknown", cut = NA_real_, fixable = FALSE, ruleRepr = ruleRepr))
	}
	if (is.character(rule) && length(rule) == 1L) {
		cleanRule = trimws(rule)
		# optional x, op, number  OR  number, op, optional x
		reLeft = "^\\s*(x\\s*)?(<=|>=|<|>)\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)\\s*$"
		reRight = "^\\s*([-+]?[0-9]*\\.?[0-9]+(?:[eE][-+]?[0-9]+)?)\\s*(<=|>=|<|>)\\s*(x)?\\s*$"
		m1 = regexec(reLeft, cleanRule, perl = TRUE)
		rm1 = regmatches(cleanRule, m1)[[1]]
		if (length(rm1) >= 4L) {
			op = rm1[3]
			num = as.numeric(rm1[4])
			side = if (op %in% c("<", "<=")) "left" else "right"
			return(list(side = side, cut = num, fixable = is.finite(num), ruleRepr = ruleRepr))
		}
		m2 = regexec(reRight, cleanRule, perl = TRUE)
		rm2 = regmatches(cleanRule, m2)[[1]]
		if (length(rm2) >= 3L) {
			num = as.numeric(rm2[2])
			op = rm2[3]
			# "0 >= x" means x <= 0 (left); "0 <= x" means x >= 0 (right)
			side = if (op %in% c(">", ">=")) "left" else "right"
			return(list(side = side, cut = num, fixable = is.finite(num), ruleRepr = ruleRepr))
		}
		return(list(side = "unknown", cut = NA_real_, fixable = FALSE, ruleRepr = ruleRepr))
	}
	return(list(side = "unknown", cut = NA_real_, fixable = FALSE, ruleRepr = ruleRepr))
}

#' Resolve which double-entry pairs get fixed censor thresholds
#'
#' Combines [umxACE_DE()] arguments with the \code{umxDoubleEntry} data-frame attribute.
#'
#' @param mzData MZ data frame (may carry \code{umxDoubleEntry}).
#' @param dzData DZ data frame.
#' @param doubleEntryPairs list of character length-2 vectors \code{c(contBase, censBase)}.
#' @param fixCensorThresholds \code{"no"}, \code{"yes"}, or \code{"auto"}.
#' @param censorCuts named numeric cuts or NULL.
#' @param doubleEntrySuffix c("_cont","_cens").
#' @return list with \code{fixedCuts} (named by cens base), \code{contByCens}, \code{sideByCens}.
#' @family xmu internal not for end user
xmu_ace_de_parse_censor_meta <- function(mzData, dzData, doubleEntryPairs, fixCensorThresholds, censorCuts = NULL, doubleEntrySuffix = c("_cont", "_cens")) {
	fixedCuts = numeric(0)
	contByCens = character(0)
	sideByCens = character(0)

	if (!is.null(censorCuts) && identical(fixCensorThresholds, "no")) {
		stop("Polite note: censorCuts is set but fixCensorThresholds = \"no\". Set fixCensorThresholds to \"yes\" or \"auto\", or omit censorCuts.")
	}
	if (identical(fixCensorThresholds, "no") && is.null(censorCuts)) {
		return(list(fixedCuts = fixedCuts, contByCens = contByCens, sideByCens = sideByCens))
	}

	# Map each DE pair
	pairInfo = list()
	for (pair in doubleEntryPairs) {
		contBase = pair[1]
		censBase = pair[2]
		traitBase = sub(paste0(doubleEntrySuffix[1], "$"), "", contBase)
		pairInfo[[censBase]] = list(cont = contBase, cens = censBase, trait = traitBase)
		contByCens[censBase] = contBase
	}

	attrList = attr(mzData, "umxDoubleEntry")
	if (is.null(attrList)) {
		attrList = attr(dzData, "umxDoubleEntry")
	}
	attrByCens = list()
	if (!is.null(attrList$pairs)) {
		for (p in attrList$pairs) {
			attrByCens[[p$cens]] = p
			# also allow match by trait base
			attrByCens[[p$base]] = p
			attrByCens[[p$cont]] = p
		}
	}

	resolveCutName = function(nm) {
		# return censBase for a name in censorCuts or attr keys
		for (censBase in names(pairInfo)) {
			info = pairInfo[[censBase]]
			if (nm %in% c(censBase, info$cont, info$trait)) {
				return(censBase)
			}
		}
		return(NA_character_)
	}

	if (!is.null(censorCuts)) {
		if (is.null(names(censorCuts)) || any(names(censorCuts) == "")) {
			stop("Polite note: censorCuts must be a named numeric vector (e.g. c(wt = 0)).")
		}
		for (nm in names(censorCuts)) {
			cutVal = as.numeric(censorCuts[[nm]])
			if (!is.finite(cutVal)) {
				stop("Polite note: censorCuts[\"", nm, "\"] must be a finite number.")
			}
			censBase = resolveCutName(nm)
			if (is.na(censBase)) {
				warning("umx note: censorCuts name \"", nm, "\" matches no double-entry pair in selDVs; ignored.", call. = FALSE)
				next
			}
			fixedCuts[censBase] = cutVal
			p = attrByCens[[censBase]]
			sideByCens[censBase] = if (!is.null(p$side)) p$side else "left"
		}
		# Partial censorCuts: only named pairs fixed; do not require remaining pairs
		return(list(fixedCuts = fixedCuts, contByCens = contByCens[names(fixedCuts)], sideByCens = sideByCens))
	}

	# No censorCuts: use attr for "yes" or "auto"
	missingFixable = character(0)
	for (censBase in names(pairInfo)) {
		p = attrByCens[[censBase]]
		if (is.null(p)) {
			if (identical(fixCensorThresholds, "yes")) {
				missingFixable = c(missingFixable, censBase)
			}
			next
		}
		if (isTRUE(p$fixable) && is.finite(p$cut)) {
			fixedCuts[censBase] = as.numeric(p$cut)
			sideByCens[censBase] = p$side
		} else if (identical(fixCensorThresholds, "yes")) {
			missingFixable = c(missingFixable, censBase)
		}
	}
	if (length(missingFixable) > 0) {
		stop("Polite note: fixCensorThresholds = \"yes\" but no finite cut for: ", paste(missingFixable, collapse = ", "), ". Provide censorCuts or prep with umx_make_double_entry_data() using a known numeric bound.")
	}
	list(fixedCuts = fixedCuts, contByCens = contByCens[names(fixedCuts)], sideByCens = sideByCens)
}

#' Apply double-entry mean/variance/threshold identification (post-supermodel)
#'
#' For every DE pair in \code{contByCens}: release binary \code{Vtot==1} (continuous
#' variance must not be forced to 1), and equate free means cont=cens.
#' For names in \code{fixedCuts}, fix the twin-shared threshold at the known cut;
#' other DE pairs keep a free threshold (cut estimated in data units).
#'
#' @param model Twin ACE model with \code{top$deviations_for_thresh} and \code{top$expMean}.
#' @param fixedCuts named numeric cuts (may be empty \code{numeric(0)}).
#' @param contByCens named character, cens base -> cont base (all DE pairs).
#' @param selDVs base names per individual (Cholesky / \code{Vtot} row order).
#' @param sep twin separator.
#' @param nSib number of sibs.
#' @param equateMeansWithCont free and label-equate means for pairs in \code{fixedCuts}.
#' @param freeVariance drop DE cens traits from \code{constrain_Bin_var_to_1}.
#' @param equateMeansForAllPairs if TRUE, mean-equate every pair in \code{contByCens} (not only fixed cuts).
#' @param freeTauStarts optional named numeric starts for free thresholds (data units); default continuous mean.
#' @return modified model.
#' @family xmu internal not for end user
xmu_ace_de_apply_censor_thresholds <- function(model, fixedCuts, contByCens, selDVs, sep, nSib = 2, equateMeansWithCont = TRUE, freeVariance = TRUE, equateMeansForAllPairs = TRUE, freeTauStarts = NULL) {
	dev = model$top$deviations_for_thresh
	if (is.null(dev)) {
		stop("Polite note: no deviations_for_thresh; cannot set double-entry thresholds.")
	}
	if ((equateMeansWithCont || equateMeansForAllPairs) && is.null(model$top$expMean)) {
		stop("Polite note: top$expMean missing; cannot equate cont/cens means (covariates not supported with DE ID fixes in this version).")
	}
	if (is.null(fixedCuts)) {
		fixedCuts = numeric(0)
	}

	vtotIdxToRelease = integer(0)
	pairsToMeanEquate = if (isTRUE(equateMeansForAllPairs)) names(contByCens) else names(fixedCuts)

	# 1) Thresholds: fix at known cut, or set free-tau start in data units (not 0.1)
	#    Free tau at 0.1 with mean ~170 is infeasible and kills multi-DE optim.
	for (censBase in names(contByCens)) {
		devCols = colnames(dev$labels)
		matchCols = devCols[devCols == censBase | startsWith(devCols, paste0(censBase, sep)) | grepl(paste0("^", censBase, "[0-9]+$"), devCols)]
		if (length(matchCols) < 1) {
			stop("Polite note: could not find threshold columns for \"", censBase, "\" in deviations_for_thresh.")
		}
		labs = unique(na.omit(as.character(dev$labels[1, matchCols])))
		if (length(labs) != 1L) {
			stop("Polite note: expected one twin-equated threshold label for ", censBase, "; got: ", paste(labs, collapse = ", "))
		}
		if (censBase %in% names(fixedCuts)) {
			model = omxSetParameters(model, labels = labs, free = FALSE, values = as.numeric(fixedCuts[[censBase]]))
		} else {
			# Free threshold in data units: start at prep cut if known, else continuous mean (never 0.1)
			startTau = NA_real_
			if (!is.null(freeTauStarts) && censBase %in% names(freeTauStarts) && is.finite(freeTauStarts[[censBase]])) {
				startTau = as.numeric(freeTauStarts[[censBase]])
			}
			if (!is.finite(startTau)) {
				contBase = contByCens[[censBase]]
				em = model$top$expMean
				meanCols = colnames(em$values)
				for (s in 1:nSib) {
					contCol = paste0(contBase, sep, s)
					if (!(contCol %in% meanCols)) contCol = paste0(contBase, s)
					if (contCol %in% meanCols) {
						startTau = as.numeric(em$values[1, contCol])
						break
					}
				}
			}
			if (!is.finite(startTau)) startTau = 0
			model = omxSetParameters(model, labels = labs, free = TRUE, values = startTau)
		}
	}

	# 2) Release V=1 and mean-equate for every DE pair (variance of weight/height is not 1)
	for (censBase in names(contByCens)) {
		idx = match(censBase, selDVs)
		if (is.na(idx)) {
			stop("Polite note: censored base \"", censBase, "\" not found in selDVs.")
		}
		vtotIdxToRelease = c(vtotIdxToRelease, as.integer(idx))

		if (censBase %in% pairsToMeanEquate) {
			contBase = contByCens[[censBase]]
			if (is.null(contBase) || !nzchar(contBase)) {
				stop("Polite note: missing continuous partner base for ", censBase)
			}
			em = model$top$expMean
			meanCols = colnames(em$labels)
			for (s in 1:nSib) {
				censCol = paste0(censBase, sep, s)
				contCol = paste0(contBase, sep, s)
				if (!(censCol %in% meanCols)) {
					censCol = paste0(censBase, s)
					contCol = paste0(contBase, s)
				}
				if (!(censCol %in% meanCols) || !(contCol %in% meanCols)) {
					stop("Polite note: could not find expMean columns for ", censBase, " / ", contBase, " twin ", s)
				}
				contLab = em$labels[1, contCol]
				contVal = em$values[1, contCol]
				em$free[1, censCol] = TRUE
				em$labels[1, censCol] = contLab
				em$values[1, censCol] = contVal
			}
			model$top$expMean = em
		}
	}

	if (isTRUE(freeVariance) && length(vtotIdxToRelease) > 0) {
		model = xmu_ace_de_release_binary_v1(model, vtotIndices = unique(vtotIdxToRelease))
	}
	return(model)
}

#' Drop selected traits from binary Vtot==1 constraint (DE known-cut ID)
#'
#' Post-hoc edit of supermodel \code{binLabels} / \code{constrain_Bin_var_to_1} only.
#' Does not change \code{xmuTwinSuper_SomeBinary}.
#'
#' @param model Twin model with \code{top$binLabels} (optional if already absent).
#' @param vtotIndices Integer diagonal indices into \code{Vtot} to stop constraining at 1.
#' @return modified model.
#' @family xmu internal not for end user
xmu_ace_de_release_binary_v1 <- function(model, vtotIndices) {
	if (length(vtotIndices) < 1) {
		return(model)
	}
	bl = model$top$binLabels
	if (is.null(bl)) {
		return(model)
	}
	oldLabs = as.character(bl$labels[, 1])
	releaseSet = paste0("Vtot[", vtotIndices, ",", vtotIndices, "]")
	keep = !(oldLabs %in% releaseSet)
	nKeep = sum(keep)

	# Remove old constraint and unit/bin label matrices, then rebuild if needed
	top = model$top
	top = mxModel(top, remove = TRUE, "constrain_Bin_var_to_1")
	top = mxModel(top, remove = TRUE, "binLabels")
	top = mxModel(top, remove = TRUE, "Unit_nBinx1")

	if (nKeep > 0) {
		newLabs = oldLabs[keep]
		top = mxModel(top,
			umxMatrix("binLabels", "Full", nrow = nKeep, ncol = 1, labels = newLabs),
			umxMatrix("Unit_nBinx1", "Unit", nrow = nKeep, ncol = 1),
			mxConstraint(name = "constrain_Bin_var_to_1", binLabels == Unit_nBinx1)
		)
	}
	model = mxModel(model, top)
	return(model)
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

	# Rows: drop _cont (plot cens + pure continuous). Cols: factor columns = pure + _cont (not cens phantoms).
	contIdx = grep("_cont$", allDVs)
	censIdx = grep("_cens$", allDVs)
	pureIdx = setdiff(1:nVar, c(contIdx, censIdx))
	keepIdx = sort(c(pureIdx, censIdx))
	keepColIdx = sort(c(pureIdx, contIdx))
	if (length(keepIdx) < 1) {
		keepIdx = 1:nVar
		keepColIdx = 1:nVar
	}
	if (length(keepColIdx) != length(keepIdx)) {
		# Fallback: square on keepIdx if counts mismatch
		keepColIdx = keepIdx
	}
	keepDVs = allDVs[keepIdx]
	nKeep = length(keepIdx)

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
		runOk = umx_has_been_run(model, stop = FALSE)
		m2ll = tryCatch(model$output$Minus2LogLikelihood, error = function(e) NA_real_)
		if (!runOk || is.null(m2ll) || length(m2ll) < 1 || !is.finite(m2ll[1])) {
			stop("Polite note: model has not been run successfully (no usable fit). Cannot summarize. Check optimizer errors / starting values.")
		}
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = xmu_twin_get_var_names(model, trim= TRUE, twinOneOnly= TRUE)
		nVar   = length(selDVs)

		# Rows: drop _cont. Cols: pure continuous + _cont factor columns (not cens phantom cols).
		contIdx = grep("_cont$", selDVs)
		censIdx = grep("_cens$", selDVs)
		pureIdx = setdiff(1:nVar, c(contIdx, censIdx))
		keepIdx = sort(c(pureIdx, censIdx))
		keepColIdx = sort(c(pureIdx, contIdx))
		if (length(keepIdx) < 1) {
			keepIdx = 1:nVar
			keepColIdx = 1:nVar
		}
		if (length(keepColIdx) != length(keepIdx)) {
			keepColIdx = keepIdx
		}
		nKeep   = length(keepIdx)
		keepDVs = selDVs[keepIdx]

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
		deMeta = attr(model, "umxDE")
		if (isTRUE(deMeta$fixedCensorThresholds) && length(deMeta$fixedCuts) > 0) {
			parts = character(0)
			for (nm in names(deMeta$fixedCuts)) {
				contNm = deMeta$contByCens[[nm]]
				if (is.null(contNm)) contNm = sub("_cens$", "_cont", nm)
				parts = c(parts, paste0(nm, " @ ", deMeta$fixedCuts[[nm]], " (means equated to ", contNm, "; V free)"))
			}
			message("Double-entry thresholds fixed: ", paste(parts, collapse = "; "), ".")
		}
		# Means: omit _cens (equated to _cont); show pure continuous + _cont only
		meanVals = NULL
		meanCaption = NULL
		if (!is.null(model$top$intercept)) {
			meanVals = model$top$intercept$values
			meanCaption = "Means (intercept; _cens columns omitted -- equated to _cont)"
		} else if (!is.null(model$top$expMean)) {
			meanVals = model$top$expMean$values
			meanCaption = "Means (from model$top$expMean; _cens omitted -- equated to _cont)"
		}
		if (!is.null(meanVals)) {
			cn = colnames(meanVals)
			keepMean = !grepl("_cens", cn)
			if (any(keepMean)) {
				meanVals = meanVals[, keepMean, drop = FALSE]
				row.names(meanVals) = "intercept"
				umx_print(meanVals, digits = digits, caption = meanCaption, report = report, append = TRUE, sortableDF = TRUE)
			}
		}

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
			if (!is.null(meanVals)) {
				umx_print(meanVals, digits = digits, caption = meanCaption, report = report, append = TRUE, sortableDF = TRUE)
			}
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



#' Pretty-print method for double-entry censored datasets
#'
#' @param x A data frame prepared with [umx_make_double_entry_data()].
#' @param n Number of data frame rows to display below summary table (default = 6).
#' @param ... Additional arguments passed to print.
#' @export
umxPrint_double_entry_data <- function(x, n = 6, ...) {
    meta = attr(x, "umxDoubleEntry")
    
    if (is.null(meta) || is.null(meta$pairs)) {
        # Fallback to standard data frame print if metadata missing
        NextMethod("print")
        return(invisible(x))
    }
    
    cat("## Double-Entry Data Summary (umxACE_DE)\n\n")
    cat(sprintf("* Dataset: %d rows x %d columns\n", nrow(x), ncol(x)))
    cat(sprintf("* Twin Structure: nSib = %d, separator = \"%s\"\n", meta$nSib, meta$sep))
    cat(sprintf("* Variable Suffixes: continuous = \"%s\", censored = \"%s\"\n\n", 
                meta$doubleEntrySuffix[1], meta$doubleEntrySuffix[2]))
    
    rows = list()
    for (p in meta$pairs) {
        trait = p$base
        
        # Build candidate column lists: non-twin ("wt_cont") then twin ("wt_cont_T1", "wt_cont_T2")
        contCandidates = c(p$cont, paste0(p$cont, meta$sep, 1:meta$nSib))
        censCandidates = c(p$cens, paste0(p$cens, meta$sep, 1:meta$nSib))
        
        # Find which candidates actually exist in colnames(x)
        foundIndices = which(contCandidates %in% colnames(x) | censCandidates %in% colnames(x))
        
        contCols = character(0)
        censCols = character(0)
        nCensVec = integer(0)
        pctCensVec = character(0)
        
        for (idx in foundIndices) {
            cnCont = contCandidates[idx]
            cnCens = censCandidates[idx]
            
            if (cnCont %in% colnames(x) || cnCens %in% colnames(x)) {
                contCols = c(contCols, cnCont)
                censCols = c(censCols, cnCens)
                
                vCont = if (cnCont %in% colnames(x)) x[[cnCont]] else rep(NA, nrow(x))
                vCens = if (cnCens %in% colnames(x)) x[[cnCens]] else rep(NA, nrow(x))
                
                # Valid cases: non-missing in either continuous or censored column
                validMask = !is.na(vCont) | !is.na(vCens)
                nValid    = sum(validMask)
                
                # Censored cases: marked 'censored' in ordinal column
                nCens = sum(vCens == "censored", na.rm = TRUE)
                pct   = if (nValid > 0) sprintf("%.1f%%", 100 * nCens / nValid) else "0.0%"
                
                nCensVec   = c(nCensVec, nCens)
                pctCensVec = c(pctCensVec, pct)
            }
        }
        
        rows[[length(rows) + 1]] = data.frame(
            Trait            = trait,
            `Continuous Col` = paste(contCols, collapse = ", "),
            `Censored Col`   = paste(censCols, collapse = ", "),
            `Censor Rule`    = p$ruleRepr,
            Side             = p$side,
            `Cut Value`      = ifelse(is.na(p$cut), "NA", as.character(p$cut)),
            `Fixed Cut?`     = ifelse(p$fixable, "Yes", "No"),
            `N Censored`     = paste(nCensVec, collapse = " / "),
            `% Censored`     = paste(pctCensVec, collapse = " / "),
            check.names      = FALSE
        )
    }
    
    summaryTable = do.call(rbind, rows)
    print(knitr::kable(summaryTable, format = "markdown"))
    cat("\n---\n")
    
    # Display head of data frame
    print(head(as.data.frame(x), n = n))
    invisible(x)
}
#' @method print umx_double_entry_data
#' @export
print.umx_double_entry_data <- umxPrint_double_entry_data
