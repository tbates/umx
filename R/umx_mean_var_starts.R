umx_mean_var_starts <- function(mzData, dzData, nVar, varFormat=c("Choleksy")) {
	# Make mxData, dropping any unused columns
	dataType = umx_is_cov(dzData, boolean = FALSE)
	if(dataType == "raw") {
		allData = rbind(mzData, dzData)
		T1 = allData[, 1:nVar]
		T2 = allData[, (nVar+1):(nVar*2)]; names(T2)= names(T1)
		longData = rbind(T1, T2)

		# =====================================
		# = Add means and var matrices to top =
		# =====================================
		# Figure out start values while we are here
		# varStarts will be used to fill a, c, and e

		# Covariance matrix, 1/3 allocated to each of A=C=E.
		varStarts = umx_var(longData, format= "diag", ordVar = 1, use = "pairwise.complete.obs")
		varStarts = varStarts/3	

		# sqrt to switch from var to path coefficient scale
		if(nVar == 1){
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)

		# Mean starts (used across all raw solutions
		obsMeans = umx_means(allData, ordVar = 0, na.rm = TRUE)


		# Starting values for the means
		meanStarts = umx_apply(mean, of = longData, by = "columns", na.rm = TRUE)
		# Make wide again
		meanStarts = c(meanStarts, meanStarts)
		if(equateMeans){
			meanLabels = paste0("mean", selDVs[1:nVar]) # Names recycled for twin 2
			meanLabels = c(meanLabels, meanLabels)
		} else {
			meanLabels = paste0("mean", selDVs)
		}
	} else if(dataType %in% c("cov", "cor")){
		het_mz = umx_reorder(mzData, selVars)		
		het_dz = umx_reorder(dzData, selVars)
		varStarts = diag(het_mz)[1:nVar]

		if(nVar == 1){
			varStarts = sqrt(varStarts)/3
		} else {
			varStarts = t(chol(diag(varStarts/3))) # Divide variance up equally, and set to Cholesky form.
		}
		varStarts = matrix(varStarts, nVar, nVar)
		meanStarts = NA # Not used for summary data
	}

	return(varStarts=varStarts, meanStarts= meanStarts)
}

