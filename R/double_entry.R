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
#' @param name The name of the model (defaults to "ACE").
#' @param selDVs The variables to include from the data: preferably base names (e.g. "ht") 
#' which will be expanded using doubleEntrySuffix.
#' @param selCovs (optional) covariates to include from the data (do not include sep in names)
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param doubleEntrySuffix Suffixes for the continuous and censored variables (default = c("_cont", "_cens")).
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param data If provided, dzData and mzData are treated as levels of zyg to select() MZ and DZ data sets (default = NULL)
#' @param zyg If data provided, this column is used to select rows by zygosity (Default = "zygosity")
#' @param allContinuousMethod "cumulants" or "marginals". Used in all-continuous WLS data to determine if a means model needed.
#' @param residualizeContinuousVars Not yet implemented.
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
#' @seealso - [umxACE()], [umxSummary()], [plot()]
umxACE_DE <- function(name = "ACE", selDVs, selCovs = NULL, dzData = NULL, mzData = NULL, sep = NULL, data = NULL, zyg = "zygosity", type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, allContinuousMethod = c("cumulants", "marginals"), autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL, residualizeContinuousVars = FALSE, nSib = 2, dzAr = .5, dzCr = 1, weightVar = NULL, equateMeans = TRUE, addStd = TRUE, addCI = TRUE, doubleEntrySuffix = c("_cont", "_cens")) {
	tryHard = match.arg(tryHard)
	type    = match.arg(type)
	allContinuousMethod = match.arg(allContinuousMethod)
	if(residualizeContinuousVars){
		stop("residualizing (as opposed to modelling) continuous variables not implemented yet: just set to FALSE for now")
	}

	if(dzCr == .25 & (name == "ACE")){ name = "ADE" }

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

	# Auto-detect double entry variables and expand them
	if (is.null(sep)) {
		sep = "_T"
	}
	
	newSelDVs = c()
	doubleEntryPairs = list()
	
	# Detect if the user passed base variable names that need expansion
	for (v in selDVs) {
		contName = paste0(v, doubleEntrySuffix[1])
		censName = paste0(v, doubleEntrySuffix[2])
		contExists = any(grepl(paste0("^", contName, "(", sep, "[1-9])?$"), dataCols))
		censExists = any(grepl(paste0("^", censName, "(", sep, "[1-9])?$"), dataCols))
		
		if (contExists && censExists) {
			newSelDVs = c(newSelDVs, contName, censName)
		} else {
			newSelDVs = c(newSelDVs, v)
		}
	}
	selDVs = newSelDVs

	# Also scan the resulting list to find already-expanded pairs
	# (e.g. if they passed c("X_cont", "X_cens") directly)
	doubleEntryPairs = list()
	i = 1
	while (i < length(selDVs)) {
		v1 = selDVs[i]
		v2 = selDVs[i + 1]
		s1 = doubleEntrySuffix[1]
		s2 = doubleEntrySuffix[2]
		if (endsWith(v1, s1) && endsWith(v2, s2)) {
			prefix1 = substr(v1, 1, nchar(v1) - nchar(s1))
			prefix2 = substr(v2, 1, nchar(v2) - nchar(s2))
			if (prefix1 == prefix2) {
				doubleEntryPairs[[length(doubleEntryPairs) + 1]] = c(v1, v2)
				i = i + 2
				next
			}
		}
		i = i + 1
	}

	xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
	# New-style build-block: Expand var names if necessary and make the basic components of a twin model
	selVars = xmu_twin_upgrade_selDvs2SelVars(selDVs = selDVs, sep = sep, nSib= nSib)

	model = xmu_make_TwinSuperModel(name=name, mzData = mzData, dzData = dzData, selDVs = selDVs, selCovs= selCovs, sep = sep, type = type, allContinuousMethod = allContinuousMethod, numObsMZ = numObsMZ, numObsDZ = numObsDZ, nSib= nSib, equateMeans = equateMeans, weightVar = weightVar, bVector = FALSE, verbose= FALSE)
	tmp   = xmu_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
	nVar  = length(selVars)/nSib; # Number of dependent variables per **INDIVIDUAL**

	if(nSib==2){
		expCovMZ = mxAlgebra(rbind (cbind(ACE,  AC), cbind( AC, ACE)), dimnames = list(selVars, selVars), name = "expCovMZ")
		expCovDZ = mxAlgebra(rbind (cbind(ACE, hAC), cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
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
	model = as(model, "MxModelACE") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE, intervals = intervals)
	return(model)
}
