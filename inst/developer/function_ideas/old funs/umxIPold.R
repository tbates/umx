#' umxIPold: Build and run an Independent pathway twin model
#'
#' Make a 2-group Independent Pathway twin model (Common-factor independent-pathway multivariate model)
#' The following figure shows the IP model diagrammatically:
#' 
#' \if{html}{\figure{IP.png}{options: width="50\%" alt="Figure: IP.png"}}
#' \if{latex}{\figure{IP.pdf}{options: width=7cm}}
#'
#' @param name The name of the model (defaults to "IP").
#' @param selDVs The variables to include.
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The suffix for twin 1 and twin 2, often "_T". If set, you can
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2").
#' @param nFac How many common factors for a, c, and e. If 1 number number is given, applies to all three.
#' @param freeLowerA Whether to leave the lower triangle of A free (default = FALSE).
#' @param freeLowerC Whether to leave the lower triangle of C free (default = FALSE).
#' @param freeLowerE Whether to leave the lower triangle of E free (default = FALSE).
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model).
#' @param correlatedA Whether factors are allowed to correlate (not implemented yet: FALSE).
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE).
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE).
#' @param numObsDZ = TODO: implement ordinal Number of DZ twins: Set this if you input covariance data,
#' @param numObsMZ = TODO: implement ordinal Number of MZ twins: Set this if you input covariance data.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "mxTryHardOrdinal", "mxTryHardWideSearch"
#' @param optimizer optionally set the optimizer (default NULL does nothing).
#' @return - [mxModel()]
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxIP()], [plot()] and [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @references - <https://www.github.com/tbates/umx>
#' @md
#' @examples
#' \dontrun{
#' require(umx)
#' data(GFF)
#' mzData <- subset(GFF, zyg_2grp == "MZ")
#' dzData <- subset(GFF, zyg_2grp == "DZ")
#' selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
#' m1 = umxIPold(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' nFac = c(a=3, c = 1, e = 1)
#' m1 = umxIPold(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, nFac= nFac)
#' umxSummary(m1)
#' plot(m1)
#' }
umxIPold <- function(name = "IP", selDVs, dzData, mzData, sep = NULL, nFac = c(a=1, c=1, e=1), freeLowerA = FALSE, freeLowerC = FALSE, freeLowerE = FALSE, equateMeans = TRUE, dzAr = .5, dzCr = 1, correlatedA = FALSE, addStd = TRUE, addCI = TRUE, numObsDZ = NULL, numObsMZ = NULL, autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL) {
	tryHard = match.arg(tryHard)
	if(tryHard == "yes"){
		tryHard = "mxTryHard"
	}
	# TODO implement correlatedA
	if(length(nFac) == 1){
		nFac = c(a = nFac, c = nFac, e = nFac)
	} else if (length(nFac) != 3){
		stop("nFac must be either 1 number or 3. You gave me ", length(nFac))
	}
	if(name == "IP"){
		# Add nFac to base name if no user-set name provided.
		if (length(nFac)==1){
			name = paste0(name, nFac, "fac")
		}else{
			name = paste0(name, paste0(c("a", "c", "e"), nFac, collapse = ""))
		}
	}

	# =================
	# = Set optimizer =
	# =================
	if(!is.null(optimizer)){
		umx_set_optimizer(optimizer)
	}

	if(correlatedA){
		message("I have not implemented correlatedA yet...")
	}
	
	nSib = 2;
	# expand var names
	if(!is.null(sep)){
		if(length(sep) != 1){
			stop("sep should be just one word, like '_T'. I will add 1 and 2 afterwards... \n",
			"i.e., set selDVs to 'obese', sep to '_T' and I look for 'obese_T1' and 'obese_T2' in the data...\n",
			"nb: variables MUST be sequentially numbered, i.e  'example_T1' and 'example_T2'")
		}
		selVars = umx_paste_names(selDVs, sep, 1:nSib)
	}else{
		selVars = selDVs
	}
	umx_check_names(selVars, mzData)
	umx_check_names(selVars, dzData)
	# message("selVars: ", omxQuotes(selVars))
	nVar = length(selVars)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**

	dataType = umx_is_cov(dzData)

	if(dataType == "raw") {
		if(!all(is.null(c(numObsMZ, numObsDZ)))){
			stop("You should not be setting numObsMZ or numObsDZ with ", omxQuotes(dataType), " data...")
		}
		# Drop any unused columns from mz and dzData
		mzData = mzData[, selVars, drop = FALSE]
		dzData = dzData[, selVars, drop = FALSE]
		if(any(umx_is_ordered(mzData))){
			stop("some selected variables are factors or ordinal... I can only handle continuous variables so far... sorry")
		}
	} else if(dataType %in% c("cov", "cor")){
		if(is.null(numObsMZ)){ stop(paste0("You must set numObsMZ with ", dataType, " data"))}
		if(is.null(numObsDZ)){ stop(paste0("You must set numObsDZ with ", dataType, " data"))}
		het_mz = umx_reorder(mzData, selVars)
		het_dz = umx_reorder(dzData, selVars)
		stop("COV not fully implemented yet for IP... Not sure if there's any demand, so email maintainer('umx') if you see this")
	} else {
		stop("Datatype ", omxQuotes(dataType), " not understood")
	}

	obsMZmeans = colMeans(mzData, na.rm = TRUE);
	model = mxModel(name,
		mxModel("top",
			umxLabel(mxMatrix("Full", 1, nVar*nSib, free=T, values=obsMZmeans, dimnames=list("means", selVars), name="expMean")), # Means 
			# (not yet equated for the two twins)
			# Matrices ac, cc, and ec to store a, c, and e path coefficients for independent general factors
			umxMatrix("ai", "Full", nVar, nFac['a'], free=TRUE, values=.6, jiggle=.05), # latent common factor Additive genetic path 
			umxMatrix("ci", "Full", nVar, nFac['c'], free=TRUE, values=.0, jiggle=.05), # latent common factor Common #environmental path coefficient
			umxMatrix("ei", "Full", nVar, nFac['e'], free=TRUE, values=.6, jiggle=.05), # latent common factor Unique environmental path #coefficient
			# Matrices as, cs, and es to store a, c, and e path coefficients for specific factors
			umxMatrix("as", "Lower", nVar, nVar, free=TRUE, values=.6, jiggle=.05), # Additive genetic path 
			umxMatrix("cs", "Lower", nVar, nVar, free=TRUE, values=.0, jiggle=.05), # Common environmental path 
			umxMatrix("es", "Lower", nVar, nVar, free=TRUE, values=.6, jiggle=.05), # Unique environmental path.

			umxMatrix("dzAr", "Full", 1, 1, free = FALSE, values = dzAr),
			umxMatrix("dzCr", "Full", 1, 1, free = FALSE, values = dzCr),

			# Multiply by each path coefficient by its inverse to get variance component
			# Sum the squared independent and specific paths to get total variance in each component
			mxAlgebra(name = "A", ai%*%t(ai) + as%*%t(as) ), # Additive genetic variance
			mxAlgebra(name = "C", ci%*%t(ci) + cs%*%t(cs) ), # Common environmental variance
			mxAlgebra(name = "E", ei%*%t(ei) + es%*%t(es) ), # Unique environmental variance

			mxAlgebra(name = "ACE", A+C+E),
			mxAlgebra(name = "AC" , A+C  ),
			mxAlgebra(name = "hAC", (dzAr %x% A) + (dzCr %x% C)),
			mxAlgebra(rbind (cbind(ACE, AC), 
			                 cbind(AC , ACE)), dimnames = list(selVars, selVars), name = "expCovMZ"),
			mxAlgebra(rbind (cbind(ACE, hAC),
			                 cbind(hAC, ACE)), dimnames = list(selVars, selVars), name = "expCovDZ"),

			# Algebra to compute total variances and standard deviations (diagonal only)
			mxMatrix("Iden", nrow = nVar, name = "I"),
			mxAlgebra(solve(sqrt(I * ACE)), name = "iSD")
		),
		mxModel("MZ", mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxModel("DZ", mxData(dzData, type = "raw"), 
			mxExpectationNormal("top.expCovDZ", "top.expMean"), 
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Equate means for twin1 and twin 2
	if(equateMeans){
		model = omxSetParameters(model,
		  labels    = paste0("expMean_r1c", (nVar+1):(nVar*2)), # c("expMeanr1c4", "expMeanr1c5", "expMeanr1c6"),
		  newlabels = paste0("expMean_r1c", 1:nVar)             # c("expMeanr1c1", "expMeanr1c2", "expMeanr1c3")
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
	} else {
		# set the first column off, bar r1
		model = omxSetParameters(model, labels = "es_r[^1]0-9?c1", free = FALSE, values = 0)

		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
		# toset  = model$top$matrices$es$labels[lower.tri(model$top$matrices$es$labels)]
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)

		# Used to drop the ei paths, as we have a full Cholesky for E, now just set the bottom row TRUE
		# toset = umxGetParameters(model, "^ei_r.c.", free= TRUE)
		# model = omxSetParameters(model, labels = toset, free = FALSE, values = 0)
	}

	if(addStd){
		newTop = mxModel(model$top,
			# nVar Identity matrix
			mxMatrix("Iden", nrow = nVar, name = "I"),
			# inverse of standard deviation diagonal  (same as "(\sqrt(I.Vtot))~"
			mxAlgebra(solve(sqrt(I * ACE)), name = "SD"),
			# Standard general path coefficients
			mxAlgebra(SD %*% ai, name = "ai_std"), # standardized ai
			mxAlgebra(SD %*% ci, name = "ci_std"), # standardized ci
			mxAlgebra(SD %*% ei, name = "ei_std"), # standardized ei
			# Standardize specific path coefficients
			mxAlgebra(SD %*% as, name = "as_std"), # standardized as
			mxAlgebra(SD %*% cs, name = "cs_std"), # standardized cs
			mxAlgebra(SD %*% es, name = "es_std")  # standardized es
		)
		model = mxModel(model, newTop)
		if(addCI){
			model = mxModel(model, mxCI(c('top.ai_std','top.ci_std','top.ei_std', 'top.as_std','top.cs_std','top.es_std')))
		}
	}
	model  = omxAssignFirstParameters(model) # ensure parameters with the same label have the same start value... means, for instance.
	model = as(model, "MxModelIP")
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard)
	return(model)
} # end umxIPold

