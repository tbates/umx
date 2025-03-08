#' Update covariates in twin data sets to 99999 if missing and corresponding twin phenotype to NA
#'
#' Takes a dataframe with twin data and updates the covariates to 99999 if missing and the corresponding
#' twin phenotype to NA. This avoids removing rows with missing data in the covariates and does not
#' affect the estimation.
#' @param data A [data.frame()] to convert
#' @param covar The covariates.
#' @param pheno The phenotypes affected by covariates.
#' @param sep The separator used in the column names (default = "_T")
#' @return - dataframe with updated covariates and phenotypes
#' @export
#' @family xmu internal not for end user
#' @examples
#' # data(docData) 
#' # df = docData
#' # Add some missing data 
#' # df$varA1_T1[1:5] <- NA
#' # df <- xmu_update_covar(df, covar = "varA1", pheno = "varB1")
#' # head(df)
xmu_update_covar <- function(data, covar, pheno, sep = "_T") {
  if (!is.null(covar)) {
	for (cov in covar) {
	  ind1 <- paste0(cov, sep, "1")
	  ind2 <- paste0(cov, sep, "2")

	  # Skip rows without data in both covariates
	  if (all(is.na(data[[ind1]])) && all(is.na(data[[ind2]]))) next

	  for (p in pheno) {
		p_var1 <- paste0(p, sep, "1")
		p_var2 <- paste0(p, sep, "2")

		# Skip rows without data in both phenotypes
		if (all(is.na(data[[p_var1]])) && all(is.na(data[[p_var2]]))) next

		# Update covariates to 99999 if NA and the other covariate is present
		data[[ind1]][is.na(data[[ind1]]) & !is.na(data[[ind2]])] <- 99999
		data[[ind2]][is.na(data[[ind2]]) & !is.na(data[[ind1]])] <- 99999

		# Set measurements to NA if the respective covariate is 99999
		data[[p_var1]][data[[ind1]] == 99999] <- NA
		data[[p_var2]][data[[ind2]] == 99999] <- NA
	  }
	}

	# Convert remaining NAs in covariates to 99999 and set corresponding phenotype to NA
	for (cov in covar) {
	  ind1 <- paste0(cov, sep, "1")
	  ind2 <- paste0(cov, sep, "2")

	  # Replace remaining NAs in covariates with 99999
	  na_indices1 <- is.na(data[[ind1]])
	  na_indices2 <- is.na(data[[ind2]])

	  data[[ind1]][na_indices1] <- 99999
	  data[[ind2]][na_indices2] <- 99999

	  # Set corresponding phenotype variables to NA
	  for (p in pheno) {
		p_var1 <- paste0(p, sep, "1")
		p_var2 <- paste0(p, sep, "2")

		data[[p_var1]][na_indices1] <- NA
		data[[p_var2]][na_indices2] <- NA
	  }
	}
  } else {
	warning("No covariates provided. Skipping update.")
  }

  return(data)
}
