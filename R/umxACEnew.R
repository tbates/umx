#' Build and run a 2-group Cholesky twin model (uni-variate or multi-variate)
#'
#' @description
#' Implementing a core task in twin modeling, umxACE models the genetic and environmental
#' structure of one or more phenotypes (measured variables) using the Cholesky ACE model
#' (Neale and Cardon, 1996).
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxACE` implements a 2-group model to capture these data and represent the phenotypic variance as a sum of Additive genetic,
#' unique environmental (E) and, optionally, either common or shared-environment (C) or 
#' non-additive genetic effects (D).
#' 
#' The following figure shows how the ACE model appears as a path diagram (for one variable):
#' 
#' \figure{ACE_full_univariate.png}
#'
#' `umxACE` allows multivariate analyses, and this brings us to the Cholesky part of the model.
#' A Cholesky decomposition breaks
#' 
#' The Cholesky or lower-triangle decomposition allows a model which is both sure to be 
#' solvable, and also to account for all the variance (with some restrictions) in the data. The variance-covariance
#' matrix of the raw data is recovered as the product of the lower Cholesky and its transform.
#' 
#' This model creates as many latent A C and E variables as there are phenotypes, and, moving 
#' from left to right, decomposes the variance in each component into successively restricted 
#' factors. The following figure shows how the ACE model appears as a path diagram: See the details section below
#' for additional information on using umxACE.
#' 
#' 
#' \figure{ACE.png}
#' 
#' This figure also contains the key to understanding how to modify models that `umxACE` produces.
#' read the "Matrices and Labels in ACE model" section in details below...
#' 
#' **NOTE**: Scroll down to details for how to use the function, a figure
#' and multiple examples.
#' 
#' @details
#' \strong{Data Input}
#' The function flexibly accepts raw data, and also summary covariance data 
#' (in which case the user must also supple numbers of observations for the two input data sets).
#' 
#' TODO: Document type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' 
#' \strong{Ordinal Data}
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination. An experimental feature is under development to allow Tobit modeling. 
#' 
#' The function also supports weighting of individual data rows. In this case,
#' the model is estimated for each row individually, then each row likelihood
#' is multiplied by its weight, and these weighted likelihoods summed to form
#' the model-likelihood, which is to be minimized.
#' This feature is used in the non-linear GxE model functions.
#' 
#' \strong{Additional features}
#' The umxACE function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \strong{Matrices and Labels in ACE model}
#' 
#' Matrices 'a', 'c', and 'e' contain the path loadings of the Cholesky ACE factor model.
#' 
#' So, labels relevant to modifying the model are of the form \code{"a_r1c1", "c_r1c1"} etc.
#'
#' Variables are in rows, and factors are in columns. So to drop the influence of factor 2 on variable 3, you would say
#'
#'     \code{m2 = umxModify(m1, update = "c_r3c2")}
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' \code{m1$top$expMean$labels[1, 4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")}
#'
#' \emph{note}: Only one of C or D may be estimated simultaneously. This restriction reflects the lack
#' of degrees of freedom to simultaneously model C and D with only MZ and DZ twin pairs (Eaves et al. 1978 p267).
#' @param name The name of the model (defaults to"ACE").
#' @param selDVs The variables to include from the data: preferably, just "dep" not c("dep_T1", "dep_T2").
#' @param selCovs (optional) covariates to include from the data (do not include suffix in names)
#' @param covMethod How to treat covariates: "fixed" (default) or "random".
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param type Analysis method one of c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS")
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add intervals to compute CIs (defaults to TRUE).
#' @param numObsDZ Number of DZ twins: Set this if you input covariance data.
#' @param numObsMZ Number of MZ twins: Set this if you input covariance data.
#' @param boundDiag Numeric lbound for diagonal of the a, c, and e matrices. Defaults to 0 since umx version 1.8
#' @param weightVar If provided, a vector objective will be used to weight the data. (default = NULL).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param bVector Whether to compute row-wise likelihoods (defaults to FALSE).
#' @param thresholds How to implement ordinal thresholds c("deviationBased", "WLS").
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param suffix Deprecated: use "sep".
#' @return - \code{\link{mxModel}} of subclass mxModel.ACE
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{plot.MxModelACE}}, \code{\link{plot.MxModelACE}}, \code{\link{umxSummaryACE}}, \code{\link{umxModify}}
#' @references - Eaves, L. J., Last, K. A., Young, P. A., & Martin, N. G. (1978). Model-fitting approaches 
#' to the analysis of human behaviour. *Heredity*, **41**, 249-320. \url{https://www.nature.com/articles/hdy1978101.pdf}
#' @md
#' @examples
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#' # Pick the variables
#' selDVs = c("ht")
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEnew(selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData)
#' umxSummary(m1, std = FALSE) # unstandardized
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' plot(m1)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEnew("ADE", selDVs = selDVs, sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) 
#' # nb: Although summary is smart enough to print d, the underlying 
#' #     matrices are still called a, c & e.
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' 
#' # 1. This variable has a large variance, but umx picks good starts.
#' 
#' # 2. umxACEnew can figure out variable names: provide sep= "_T" and selVar = "wt" -> "wt_T1" "wt_T2"
#' 
#' # 3. umxACEnew picks the variables it needs from the data.
#' # 4. note: the default boundDiag = 0 lower-bounds a, c, and e at 0 (prevents mirror-solutions).
#'         # can remove this by setting boundDiag = NULL
#' m1 = umxACEnew(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = NULL)
#'
#' # MODEL MODIFICATION
#' # We can modify this model, say testing shared environment, and see a comparison:
#' 
#' m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
#' # nb: You can see names of free parameters with parameters(m2)
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData = mzData[1:80,] # quicker run to keep CRAN happy
#' dzData = dzData[1:80,]
#' selDVs = c("ht", "wt") # umx will add sep (in this case "") + "1" or '2'
#' m1 = umxACEnew(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#'
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#'
#'
#' # ===================
#' # = Ordinal example =
#' # ===================
#' require(umx)
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints <- quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 <- cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 <- cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into umxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] <- mxFactor(twinData[, ordDVs], levels = obesityLevels)
#' mzData <- twinData[twinData$zygosity %in% "MZFF", ]
#' dzData <- twinData[twinData$zygosity %in% "DZFF", ]
#' mzData <- mzData[1:80, ] # Just top 80 pairs to run fast
#' dzData <- dzData[1:80, ]
#' str(mzData) # make sure mz, dz, and t1 and t2 have the same levels!
#' 
#' # Data-prep done - here's where the model starts:
#' selDVs = c("obese")
#' m1 = umxACEnew(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#'
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obesityLevels   = c('normal', 'overweight', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' mzData <- mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData <- dzData[1:80,]
#' m1 = umxACEnew(selDVs = c("wt", "obese"), dzData = dzData, mzData = mzData, sep = '')
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' obesityLevels   = c('normal', 'obese')
#' cutPoints       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' \dontrun{
#' m1 = umxACEnew(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#' }
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' m1 = umxACEnew(selDVs = selDVs, dzData = dz, mzData = mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)
umxACEnew <- function(name = "ACE", selDVs, selCovs = NULL, covMethod = c("fixed", "random"), dzData, mzData, sep = NULL, type = c("Auto", "FIML", "cov", "cor", "WLS", "DWLS", "ULS"), dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, boundDiag = 0, 
	weightVar = NULL, equateMeans = TRUE, bVector = FALSE, thresholds = c("deviationBased"), autoRun = getOption("umx_auto_run"), optimizer = NULL, intervals = FALSE, suffix = "deprecated") {

		nSib = 2 # Number of siblings in a twin pair.
		covMethod  = match.arg(covMethod)
		thresholds = match.arg(thresholds)
		type = match.arg(type)

		# Allow suffix as a synonym for sep
		sep = xmu_set_sep_from_suffix(sep= sep, suffix= suffix)
		# TODO check covs
		xmu_twin_check(selDVs= selDVs, sep = sep, dzData = dzData, mzData = mzData, enforceSep = FALSE, nSib = nSib, optimizer = optimizer)
		
		if(dzCr == .25 & (name == "ACE")){
			name = "ADE"
		}

		# If given covariates, call umxACEcov
		if(!is.null(selCovs)){
			if(covMethod == "fixed"){
				stop("Fixed covariates are on the roadmap for umx in 2019. Until then, use umx_residualize on the data first.")
				# umxACEdefcov(name = name, selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep = sep, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
			} else if(covMethod == "random"){
				umxACEcov(name = name, selDVs= selDVs, selCovs= selCovs, dzData= dzData, mzData= mzData, sep = sep, dzAr = dzAr, dzCr = dzCr, addStd = addStd, addCI = addCI, boundDiag = boundDiag, equateMeans = equateMeans, bVector = bVector, thresholds = thresholds, autoRun = autoRun)
			}
		}else{
			# nSib = 2, equateMeans = TRUE, threshType = c("deviationBased"), verbose = verbose
			bits = xmu_make_top_twin_models(mzData = mzData, dzData = dzData, selDVs= selDVs, sep = sep, equateMeans = equateMeans,
							type = type, numObsMZ = numObsMZ, numObsDZ = numObsDZ, weightVar = weightVar, bVector = bVector)
			top     = bits$top
			MZ      = bits$MZ
			DZ      = bits$DZ
			bVector = bits$bVector

			if(bVector){
				mzWeightMatrix = bits$mzWeightMatrix
				dzWeightMatrix = bits$dzWeightMatrix
			}else{
				mzWeightMatrix = dzWeightMatrix = NULL
			}

			# Define varStarts ...
			tmp = xmu_mean_var_starts(mzData, dzData, selVars = selDVs, sep = sep, nSib = nSib, varForm = "Cholesky", equateMeans= equateMeans, SD= TRUE, divideBy = 3)
			varStarts = tmp$varStarts

			if(!is.null(sep)){
				selVars = tvars(selDVs, sep = sep, suffixes= 1:nSib)
			}
			nVar = length(selVars)/nSib; # Number of dependent variables ** per INDIVIDUAL ( so times-2 for a family) **

			# Finish building top
			top = mxModel(top,
				# "top" defines the algebra of the twin model, which MZ and DZ slave off of
				# NB: top already has the means model and thresholds matrix added if necessary  - see above
				# Additive, Common, and Unique environmental paths
				
				umxMatrix("a", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
				umxMatrix("c", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE),
				umxMatrix("e", type = "Lower", nrow = nVar, ncol = nVar, free = TRUE, values = varStarts, byrow = TRUE), 
		
				umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
				umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),
				# Multiply by each path coefficient by its inverse to get variance component
				# Quadratic multiplication to add common_loadings
				mxAlgebra(name = "A", a %*% t(a)), # additive genetic variance
				mxAlgebra(name = "C", c %*% t(c)), # common environmental variance
				mxAlgebra(name = "E", e %*% t(e)), # unique environmental variance
				mxAlgebra(name = "ACE", A+C+E),
				mxAlgebra(name = "AC" , A+C  ),
				mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
				mxAlgebra(rbind (cbind(ACE, AC),
				                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name = "expCovMZ"),
				mxAlgebra(rbind (cbind(ACE, hAC),
				                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ")
			)

		# =====================================
		# =  Assemble models into supermodel  =
		# =====================================
		model = xmu_assemble_twin_supermodel(name, MZ, DZ, top, bVector, mzWeightMatrix, dzWeightMatrix)

		if(!is.null(boundDiag)){
			if(!is.numeric(boundDiag)){
				stop("boundDiag must be a digit or vector of numbers. You gave me a ", class(boundDiag))
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
			}
		}
		if(addStd){
			newTop = mxModel(model$top,
				umxMatrix("I", "Iden", nVar, nVar), # nVar Identity matrix
				mxAlgebra(name = "Vtot", A + C+ E), # Total variance
				# TODO test that these are identical in all cases.
				# mxAlgebra(vec2diag(1/sqrt(diag2vec(Vtot))), name = "SD"), # SD
				mxAlgebra(name = "SD", solve(sqrt(I * Vtot))), # Total variance
				mxAlgebra(name = "a_std", SD %*% a), # standardized a
				mxAlgebra(name = "c_std", SD %*% c), # standardized c
				mxAlgebra(name = "e_std", SD %*% e)  # standardized e
			)
			model = mxModel(model, newTop)
			if(addCI){
				if(addStd){
					model = mxModel(model, mxCI(c('top.a_std', 'top.c_std', 'top.e_std')))
				}else{
					model = mxModel(model, mxCI(c('top.a', 'top.c', 'top.e')))
				}
			}
		}
		# Trundle through and make sure values with the same label have the same start value... means for instance.
		model = omxAssignFirstParameters(model)
		model = as(model, "MxModelACE") # set class so that S3 plot() dispatches.
		
		model = xmu_safe_run_summary(model, autoRun = autoRun)
		
		return(model)
	}
} # end umxACE
