#
#   Copyright 2007-2018 Timothy C. Bates
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


#' umxCP: Build and run a Common pathway twin model
#'
#' Make a 2-group Common Pathway twin model (Common-factor common-pathway multivariate model).
#' 
#' The common-pathway model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences.
#' 
#' umxCP supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Common-pathway path diagram:
#' 
#' \figure{CP.png}
#' 
#' As can be seen, each phenotype also by default has A, C, and E influences specific to that phenotype.
#' 
#' @details
#' Like the \code{\link{umxACE}} model, the CP model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' Unlike the Cholesky, these factors do not act directly on the phenotype. Instead latent A, 
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure).
#' 
#' 
#' \strong{Data Input}
#' Currently, the umxCP function accepts only raw data. This may change in future versions.
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' \strong{Additional features}
#' The umxCP function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \strong{Matrices and Labels in CP model}
#' A good way to see which matrices are used in umxCP is to run an example model and plot it.
#'
#' The diagonals of matrices as, cs, and es contain the path loadings specific to each variable. So labels relevant to modifying these are of the form "as_r1c1", "as_r2c2" etc.
#' All the shared matrices are in the model "top". So to see the `as` values, you can say:
#' 
#' `m1$top#as$values`
#' 
#' The common-pathway loadings on the factors are in matrices a_cp, c_cp, e_cp.
#'
#' The common factors themselves are in the matrix cp_loadings (an nVar * 1 matrix)
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' `m1$top$expMean$labels[1,4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' @param name The name of the model (defaults to "CP").
#' @param selDVs The variables to include.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The suffix for twin 1 and twin 2, often "_T". If set, selDVs is just the base variable names.
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param nFac How many common factors (default = 1)
#' @param freeLowerA Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE Whether to leave the lower triangle of E free (default = FALSE).
#' @param correlatedA ?? (default = FALSE).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param boundDiag = Numeric lbound for diagonal of the a_cp, c_cp, & e_cp matrices. Set = NULL to ignore.
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = not yet implemented: Ordinal Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ = not yet implemented: Ordinal Number of MZ twins: Set this if you input covariance data.
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @param suffix DEPRECATED: Use sep instead (see above).
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxACE}()} for more examples of twin modeling, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData <- subset(GFF, zyg_2grp == "MZ")
#' dzData <- subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxCPold(selDVs = selDVs, sep = "_T", nFac = 3, dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' umxParameters(m1, patt = "^c")
#' m2 = umxModify(m1, regex = "(cs_.*$)|(c_cp_)", name = "dropC")
#' umxSummaryCP(m2, comparison = m1, file = NA)
#' umxCompare(m1, m2)
#' }
#' @md
umxCPold <- function(name = "CPold", selDVs, dzData, mzData, sep = NULL, nFac = 1, freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, correlatedA = FALSE, equateMeans= TRUE, dzAr= .5, dzCr= 1, boundDiag = 0, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), optimizer = NULL, suffix = "deprecated") {
	if(suffix != "deprecated"){
		message("Just a message: but please use 'sep' instead of suffix - suffix is deprecated, and will stop working in 2019")
		sep = suffix
	}
	nSib = 2
	xmu_twin_check(selDVs=selDVs, dzData = dzData, mzData = mzData, optimizer = optimizer, sep = sep, nSib = nSib)
	
	# expand var names
	selDVs   = umx_paste_names(selDVs, sep = sep, suffixes = 1:nSib)
	nVar     = length(selDVs)/nSib; # Number of dependent variables per **INDIVIDUAL** (so x2 per family)
	dataType = umx_is_cov(dzData)
	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from MZ and DZ Data
		mzData = mzData[, selDVs]
		dzData = dzData[, selDVs]
		# bind the MZ nd DZ data into one frame for precision
		allData = rbind(mzData, dzData)
		
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
		obsMeans = colMeans(allData, na.rm = TRUE);
		top = mxModel("top", 
			# Means (not yet equated across twins)
			umxMatrix("expMean", type = "Full" , nrow = 1, ncol = (nVar * nSib), free = TRUE, values = obsMeans, dimnames = list("means", selDVs) )
		) 
		MZ = mxModel("MZ", 
			mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"),
			mxFitFunctionML()
		)
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selDVs)		
		het_dz = umx_reorder(dzData, selDVs)
		top = mxModel("top") # no means
		# TODO umxCP: Add alternative fit types? (WLS?)
		MZ = mxModel("MZ", 
			mxData(het_mz, type = "cov", numObs = numObsMZ),
			mxExpectationNormal("top.expCovMZ"),
			mxFitFunctionML()
		)
		DZ = mxModel("DZ", 
			mxData(het_dz, type = "cov", numObs = numObsDZ),
			mxExpectationNormal("top.expCovDZ"),
			mxFitFunctionML()
		)
	} else {
		stop("Datatype \"", dataType, "\" not understood")
	}

	# TODO umxCP: Improve start values (Mike?) 
	if(correlatedA){
		a_cp_matrix = umxMatrix("a_cp", "Lower", nFac, nFac, free = TRUE, values = .7, jiggle = .05) # Latent common factor
	} else {
		a_cp_matrix = umxMatrix("a_cp", "Diag", nFac, nFac, free = TRUE, values = .7, jiggle = .05)
	}

	model = mxModel(name,
		mxModel(top,
			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
			# Latent common factor genetic paths
			a_cp_matrix,
			umxMatrix("c_cp", "Diag", nFac, nFac, free = TRUE, values =  0, jiggle = .05), # latent common factor Common environmental path coefficients
			umxMatrix("e_cp", "Diag", nFac, nFac, free = TRUE, values = .7, jiggle = .05), # latent common factor Unique environmental path coefficients
			# Constrain variance of latent phenotype factor to 1.0
			# Multiply by each path coefficient by its inverse to get variance component
			mxAlgebra(name = "A_cp", a_cp %*% t(a_cp)), # A_cp variance
			mxAlgebra(name = "C_cp", c_cp %*% t(c_cp)), # C_cp variance
			mxAlgebra(name = "E_cp", e_cp %*% t(e_cp)), # E_cp variance
			mxAlgebra(name = "L"   , A_cp + C_cp + E_cp), # total common factor covariance (a+c+e)
			mxMatrix("Unit", nrow=nFac, ncol=1, name = "nFac_Unit"),
			mxAlgebra(diag2vec(L)             , name = "diagL"),
			mxConstraint(diagL == nFac_Unit   , name = "fix_CP_variances_to_1"),

			umxMatrix("as", "Lower", nVar, nVar, free = TRUE, values = .5, jiggle = .05), # Additive gen path 
			umxMatrix("cs", "Lower", nVar, nVar, free = TRUE, values = .1, jiggle = .05), # Common env path 
			umxMatrix("es", "Lower", nVar, nVar, free = TRUE, values = .6, jiggle = .05), # Unique env path
			umxMatrix("cp_loadings", "Full", nVar, nFac, free = TRUE, values = .6, jiggle = .05), # loadings on latent phenotype
			# Quadratic multiplication to add cp_loading effects
			mxAlgebra(cp_loadings %&% A_cp + as %*% t(as), name = "A"), # Additive genetic variance
			mxAlgebra(cp_loadings %&% C_cp + cs %*% t(cs), name = "C"), # Common environmental variance
			mxAlgebra(cp_loadings %&% E_cp + es %*% t(es), name = "E"), # Unique environmental variance
			mxAlgebra(name = "ACE", A + C + E),
			mxAlgebra(name = "AC" , A + C),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selDVs, selDVs), name= "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selDVs, selDVs), name= "expCovDZ")
		),
		MZ, DZ,
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Equate means for twin1 and twin 2 (match labels in first & second halves of means labels matrix)
	if(equateMeans & dataType == "raw"){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar + 1):(nVar * 2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)                 # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
		)
	}
	if(!freeLowerA){
		toset  = model$top$matrices$as$labels[lower.tri(model$top$matrices$as$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerC){
		toset  = model$top$matrices$cs$labels[lower.tri(model$top$matrices$cs$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(!freeLowerE){
		toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}
	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix(name = "I", "Iden", nVar, nVar),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(name = "SD", solve(sqrt(I * ACE))),
			# Standard specific path coefficients
			mxAlgebra(name = "as_std", SD %*% as), # standardized a
			mxAlgebra(name = "cs_std", SD %*% cs), # standardized c
			mxAlgebra(name = "es_std", SD %*% es), # standardized e
			# Standardize loadings on Common factors
			mxAlgebra(SD %*% cp_loadings, name = "cp_loadings_std") # Standardized path coefficients (general factor(s))
		)
		model = mxModel(model, newTop)
		if(addCI){
			# TODO umxCP: add these by listing free parameters in model to get single labels?
			model = mxModel(model, mxCI(c('top.a_cp', 'top.c_cp', 'top.e_cp', 'top.as_std', 'top.cs_std', 'top.es_std', 'top.cp_loadings_std')))
		}
	}
	if(!is.null(boundDiag)){
		if(!is.numeric(boundDiag)){
			stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
		} else {				
			newLbound = model$top$matrices$a_cp@lbound
			if(length(boundDiag) > 1 ){
				if(length(boundDiag) != length(diag(newLbound)) ){
					stop("Typically boundDiag is 1 digit: if more, must be size of diag(a_cp)")
				}
			}
			diag(newLbound) = boundDiag; 
			model$top$a_cp$lbound = newLbound
			model$top$c_cp$lbound = newLbound
			model$top$e_cp$lbound = newLbound
		}
	}
	# Set values with the same label to the same start value... means for instance.
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelCP")
	
	if(autoRun){
		tryCatch({
			model = mxRun(model)
			umxSummary(model)
		}, warning = function(w) {
			message("Warning incurred trying to run model")
			message(w)
		}, error = function(e) {
			message("Error incurred trying to run model")
			message(e)
		})
	}
	return(model)
} # end umxCP

# ==============
# = Deprecated =
# ==============


#' Deprecated. May already stop() code and ask to be updated. May be dropped entirely in future.
#'
#' @param ... the old function's parameters (now stripped out to avoid telling people how to do it the wrong way :-)
#' @description 
#' 
#' umxSaturated should be replaced with \code{\link{mxRefModels}}
#' 
#' umx_grep_labels should be replaced with \code{\link{umx_grep}}
#' 
#' grepSPSS_labels should be replaced with \code{\link{umx_grep}}
#' 
#' umxStart should be replaced with \code{\link{umxValues}}
#' 
#' umxTryHard is deprecated: use \code{\link{umxRun}} instead
#'
#' genEpi_Jiggle is deprecated: use \code{\link{umxJiggle}} instead
#' 
#' umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#' 
#' umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#' 
#' umxPath is deprecated: Use \code{\link{mxPath}} and \code{\link{umxLabel}} instead
#' 
#' umxReportFit is deprecated: use \code{\link{umxSummary}} instead
#' 
#' umxGetLabels is deprecated: use \code{\link{umxGetParameters}} instead
#'
#' stringToMxAlgebra is deprecated: please use \code{\link{umx_string_to_algebra}} instead
#'
#' genEpi_EvalQuote is deprecated: please use \code{\link{umxEval}} instead
#'
#' umxReportCIs is deprecated: please use \code{\link{umxCI}} instead
#'
#' hasSquareBrackets is deprecated: please use \code{\link{umx_has_square_brackets}} instead
#' 
#' xmuHasSquareBrackets is deprecated: please use \code{\link{umx_has_square_brackets}} instead
#' 
#' replace umxReportFit with \code{\link{umxSummary}}
#' 
#' Replace umxGraph_RAM with \code{\link{plot}}
#'
#' Replace tryHard with \code{\link{mxTryHard}}
#'
#' Replace genEpi_ReRun with \code{\link{umxModify}}
#'
#' Replace mxStart with \code{\link{umxValues}}
#'
#' Replace umxLabeler with \code{\link{umxLabel}}
#'
#' Replace standardizeRAM with \code{\link{umx_standardize_RAM}}
#'
#' Replace genEpi_equate with \code{\link{umxEquate}}
#'
#' Replace genEpi_Path with \code{\link{umxPath}}
#'
#' Replace genEpiCompare with \code{\link{umxCompare}}
#'
#' Replace mxLatent with \code{\link{umxLatent}}
#' 
#' Change col.as.numeric to \code{\link{umx_as_numeric}}
#' 
#' Change cor.prob to \code{\link{umx_cor}}
#' 
#' Change umx_u_APA_pval to \code{\link{umx_APA_pval}}
#' 
#'
#' @name umx-deprecated
#' @family umx deprecated
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}, \url{http://openmx.ssri.psu.edu}
NULL