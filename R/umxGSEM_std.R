#   Copyright 2007-2026 Timothy C. Bates
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

#' Standardize GSEM S and V matrices to the correlation metric via Delta Method
#'
#' @description
#' Standardizes the genetic covariance matrix \eqn{S} and propagates the uncertainty
#' to the asymptotic sampling covariance matrix \eqn{V} using the Delta method.
#' Standardizes the LDSC intercepts matrix \eqn{I} by dividing by the heritabilities.
#'
#' @details
#' Standardizing \eqn{S} to a correlation matrix \eqn{R} and scaling \eqn{V} accordingly
#' is highly recommended for model fitting to prevent numerical underflow and improve
#' optimizer convergence rates when genetic variances are very small.
#'
#' @param covstruc A list containing the genetic covariance matrix \code{S}, its asymptotic covariance \code{V}, and optional intercepts \code{I}.
#' @param S Optional genetic covariance matrix.
#' @param V Optional asymptotic covariance matrix.
#' @return A list containing the standardized covariance structure:
#'   \item{S}{Standardized genetic correlation matrix.}
#'   \item{V}{Standardized asymptotic covariance matrix of the elements of \code{S}.}
#'   \item{I}{Standardized LDSC intercepts matrix (if present).}
#' @export
#' @family GSEM
#' @examples
#' # Example usage:
#' # std_cov = umxGSEM_std(covstruc)
umxGSEM_std <- function(covstruc = NULL, S = NULL, V = NULL) {
	if (is.null(covstruc)) {
		if (is.null(S) || is.null(V)) {
			stop("You must provide both S and V (either directly or via covstruc).")
		}
		covstruc = list(S = S, V = V)
	}
	S = covstruc$S
	V = covstruc$V
	
	kVal = ncol(S)
	mVal = kVal * (kVal + 1) / 2
	
	sdS = sqrt(diag(S))
	rMat = umxCov2cor(S)
	
	# Ensure V has correct column and row names
	if (is.null(colnames(V)) || is.null(rownames(V))) {
		namesV = xmu_gsem_vech_names(colnames(S))
		colnames(V) = namesV
		rownames(V) = namesV
	}
	
	# Construct Jacobian J
	jMat = matrix(0, nrow = mVal, ncol = mVal)
	rownames(jMat) = colnames(V)
	colnames(jMat) = colnames(V)
	
	for (u in 1:mVal) {
		nameU = colnames(V)[u]
		if (grepl("^var_", nameU)) {
			# Diagonal (variance): R_ii = 1 is constant, so derivatives are 0
			next
		} else if (grepl("^poly_", nameU)) {
			parts = strsplit(sub("^poly_", "", nameU), "_")[[1]]
			traitI = parts[1]
			traitJ = parts[2]
			
			nameII = paste0("var_", traitI)
			nameJJ = paste0("var_", traitJ)
			
			idxIJ = u
			idxII = which(colnames(V) == nameII)
			idxJJ = which(colnames(V) == nameJJ)
			
			i = which(colnames(S) == traitI)
			j = which(colnames(S) == traitJ)
			
			jMat[u, idxIJ] = 1 / (sdS[i] * sdS[j])
			jMat[u, idxII] = -0.5 * rMat[i, j] / S[i, i]
			jMat[u, idxJJ] = -0.5 * rMat[i, j] / S[j, j]
		}
	}
	
	vR = jMat %*% V %*% t(jMat)
	
	out = covstruc
	out$S = rMat
	out$V = vR
	if (!is.null(covstruc$I)) {
		out$I = covstruc$I / (sdS %*% t(sdS))
	}
	
	return(out)
}
