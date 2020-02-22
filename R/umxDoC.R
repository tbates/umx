# Direction of Causation Modeling 
# http://ibg.colorado.edu/cdrom2016/verhulst/Causation/DOC%20student.R

# TODO: plot.MxModelDoC, and umxSummary.MxModel_DoC methods
# TODO: support D for one or both traits

#' Build and run a 2-group Direction of Causation twin models.
#'
#' @description
#' Testing causal claims is often difficult due to an inability to conduct experimental randomization of traits and situations to people.  
#' When twins are available, even when measured on a single occasion, the pattern of cross-twin cross-trait correlations 
#' can (given distinguishable modes of inheritance for the two traits) falsify causal hypotheses.
#' 
#' `umxDoC` implements a 2-group model to form latent variables for each of two traits, and allows testing whether 
#' trait 1 causes trait 2, vice-versa, or even reciprocal causation.
#' 
#' The following figure shows how the DoC model appears as a path diagram (for two latent variables X and Y,
#' each with three indicators). Note: For pedagogical reasons, only the model for 1 twin is shown, and only one DoC pathway drawn.
#' 
#' \if{html}{\figure{DoC.png}{options: width="50\%" alt="Figure: Direction of Causation"}}
#' \if{latex}{\figure{DoC.pdf}{options: width=7cm}}
#'
#' @details
#' To be added. 
#' @param name The name of the model (defaults to "DOC").
#' @param var1Indicators variables defining latent trait 1
#' @param var2Indicators variables defining latent trait 2
#' @param causal whether to add the causal paths (default TRUE)
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param sep The separator in twin variable names, deafult = "_T", e.g. "dep_T1".
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @return - [mxModel()] of subclass MxModelDoC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [plot.MxModelDoC()], [umxSummary.MxModelDoC()], [umxModify()]
#' @references - N.A. Gillespie and N.G. Marting (2005). Direction of Causation Models. 
#' In *Encyclopedia of Statistics in Behavioral Science*, **1**. 496–499. Eds. Brian S. Everitt & David C. Howell.
#' @md
#' @examples
#' # ========================
#' # = Does Rain cause Mud? =
#' # ========================
#'
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' mzData = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' 
#' # =======================================
#' # = 2. Define manifests for var 1 and 2 =
#' # =======================================
#' var1 = paste0("a", 1:3)
#' var2 = paste0("b", 1:3)
#'
#' # =======================================================
#' # = 2. Make the non-causal (Cholesky) and causal models =
#' # =======================================================
#' Chol= umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= FALSE)
#' DoC = umxDoC(var1= var1, var2= var1, mzData= mzData, dzData= dzData, causal= TRUE)
#'
#' # ================================================
#' # = Make the directional models by modifying DoC =
#' # ================================================
#' A2B   = umxModify(DoC, "a2b", free = TRUE, name = "A2B"); summary(A2B)
#' B2A   = umxModify(DoC, "b2a", free = TRUE, name = "B2A"); summary(B2A)
#' Recip = umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip"); summary(Recip)
#'
#' \dontrun{
#' var1 = paste0("SOS", 1:8)
#' var2 = paste0("Vocab", 1:10)
#' Chol = umxDoC(var1= var1, var2= var2,mzData= mzData, dzData= dzData, causal= FALSE)
#' DoC  = umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= TRUE)
#' A2B  = umxModify(DoC, "a2b", free = TRUE, name = "A2B")
#' B2A  = umxModify(DoC, "b2a", free = TRUE, name = "B2A")
#' Recip= umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip")
#' umxCompare(Chol, c(A2B, B2A, Recip))
#'
#' }
#' 
umxDoC <- function(name = "DOC", var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = "_T", causal= TRUE, autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
	tryHard = match.arg(tryHard)
	nSib    = 2 # Number of siblings in a twin pair.
	nLat    = 2 # 2 latent variables

	nLat1   = length(var1Indicators) # measures for factor 1
	nLat2   = length(var2Indicators)
	nVar    = nLat1 + nLat2

	selVars = tvars(c(var1Indicators, var2Indicators), sep=sep)
	xmu_twin_check(selDVs= c(var1Indicators,var2Indicators), sep = sep, dzData = dzData, mzData = mzData, enforceSep = TRUE, nSib = nSib, optimizer = optimizer)
	mzData = xmu_make_mxData(mzData, manifests = selVars)
	dzData = xmu_make_mxData(dzData, manifests = selVars)

	# ========================
	# = Make Factor Loadings =
	# ========================
	# 1. Make matrix, initialised to fixed @ 0
	FacLoad = umxMatrix(name="FacLoad", "Full", nrow=nVar, ncol=nLat, free= FALSE, values = 0)
	# 2. Set FacLoad manifest loadings to pattern of 0 and 1
	FacLoad$free[1:nLat1                  ,1] = TRUE
	FacLoad$values[1:nLat1                ,1] = 1
	FacLoad$free[(nLat1+1):(nLat1+nLat2)  ,2] = TRUE
	FacLoad$values[(nLat1+1):(nLat1+nLat2),2] = 1


	top = mxModel("top", # (was "ACE")
		umxMatrix("dzAr" , "Full", nrow=nLat, ncol=nLat, free=FALSE, values= c(1,.5,.5,1) ), # Heredity Matrix for DZ
		umxMatrix("Ones" , "Full", nrow=nLat, ncol=nLat, free=FALSE, values= 1 ),            # Unit Matrix - For Com Env and MZ
		umxMatrix("Diag1", "Iden", nrow=nSib, ncol=nSib),                                   # Identity matrix (2by2: 1s on diag, 0 off diag)

		# Matrices for Cholesky (swapped out after if causal)
		umxMatrix("a", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Genetic effects on Latent Variables 
		umxMatrix("c", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Common env effects on Latent Variables
		umxMatrix("e", type="Lower", nrow=nLat, ncol=nLat, free= c(FALSE,TRUE,FALSE), values= 1), # Non-shared env effects on Latent Variables 

		# 4x4 Matrices for A, C, and E
		mxAlgebra(name="A"  , Ones  %x% (a %*% t(a))),
		mxAlgebra(name="Adz", dzAr  %x% (a %*% t(a))),
		mxAlgebra(name="C"  , Ones  %x% (c %*% t(c))),
		mxAlgebra(name="E"  , Diag1  %x% (e %*% t(e))),
		mxAlgebra(name="Vmz", A   + C + E),
		mxAlgebra(name="Vdz", Adz + C + E),

		### Generate the Asymmetric Matrix
		# Non-shared env effects on Latent Variables 
		umxMatrix("beta", "Full", nrow=nLat, ncol=nLat, free=FALSE, labels = c("a2a", "a2b", "b2a", "b2b"), values= 0),
		mxAlgebra(name= "cause", Diag1 %x% solve(Diag1 - beta)),

		### Generate the Factor Loading Matrix
		FacLoad,
		mxAlgebra(name="FacLoadtw", Diag1 %x% FacLoad),

		## Covariance between the items due to the latent factors
		mxAlgebra(name= "FacCovMZ", FacLoadtw %&% (cause %&% Vmz)),
		mxAlgebra(name= "FacCovDZ", FacLoadtw %&% (cause %&% Vdz)),
		# Matrices to store  a, c, and e "specific" path coefficients (residuals of each manifest)
		# TODO smart var starts here
		umxMatrix(name= "as", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "cs", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "es", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		mxAlgebra(name= "Asmz", Ones  %x% as),
		mxAlgebra(name= "Asdz", dzAr  %x% as),
		mxAlgebra(name= "Cstw", Ones  %x% cs),
		mxAlgebra(name= "Estw", Diag1 %x% es),
		mxAlgebra(name= "specCovMZ", Asmz + Cstw + Estw),
		mxAlgebra(name= "specCovDZ", Asdz + Cstw + Estw),
		# Expected Covariance Matrices for MZ and DZ
		mxAlgebra(name= "expCovMZ", FacCovMZ + specCovMZ, dimnames = list(selVars, selVars)),
		mxAlgebra(name= "expCovDZ", FacCovDZ + specCovDZ, dimnames = list(selVars, selVars)),
		
		# Means for the Manifest Variables # TODO Better starts for means... (easy)
		umxMatrix(name= "Means", "Full", nrow= 1, ncol= nVar, free= TRUE, values= .1),
		mxAlgebra(name= "expMean", cbind(top.Means, top.Means))
		# TODO Why not just make ncol = nCol*2 and allow label repeats the equate means? Alg might be more efficient?
	)

	MZ = mxModel("MZ", mzData, mxExpectationNormal("top.expCovMZ", means= "top.expMean", dimnames= selVars), mxFitFunctionML() )
	DZ = mxModel("DZ", dzData, mxExpectationNormal("top.expCovDZ", means= "top.expMean", dimnames= selVars), mxFitFunctionML() )

	if(!causal){
		# ========================
		# = Cholesky-based model =
		# ========================
		model = mxModel("Chol", top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ"))	)
	}else{
		# ===================
		# = DOC-based model =
		# ===================
		# Replace lower ace Matrices with diag.
		# Because covariance between the traits is "caused", theses matrices are diagonal instead of lower
		top = mxModel(top,
			umxMatrix("a", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Genetic effects on Latent Variables 
			umxMatrix("c", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Common env effects on Latent Variables
			umxMatrix("e", "Diag", nrow=nLat, ncol=nLat, free=FALSE,values=1)    # Non-shared env effects on Latent Variables 
		)
		model = mxModel("DOC", top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")) )		
	}
	# Factor loading matrix of Intercept and Slope on observed phenotypes
	# SDt = mxAlgebra(name= "SDt", solve(sqrt(Diag1 *Rt))) # Standardized deviations (inverse)
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelDoC") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE)
	return(model)
}



#' Plot a Direction of Causation Model.
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the *Rg* (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be pasted into, e.g. Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases plot.MxModelDoC
#' @param x a [umxDoC()] model to summarize.
# #' @param digits round to how many digits (default = 2).
# #' @param comparison you can run mxCompare on a comparison model (NULL).
# #' @param file The name of the dot file to write: "name" = use the name of the model.
# #' Defaults to NA = do not create plot output.
# #' @param std Whether to standardize the output (default = TRUE).
# #' @param showRg = whether to show the genetic correlations (FALSE).
# #' @param CIs Whether to show Confidence intervals if they exist (TRUE).
# #' @param returnStd Whether to return the standardized form of the model (default = FALSE).
# #' @param report If "html", then open an html table of the results.
# #' @param extended how much to report (FALSE).
# #' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @return - optional [mxModel()]
#' @export
#' @family Twin Reporting Functions
#' @seealso - [umxDoC()], [umxSummary.MxModelDoC()], [umxModify()]
#' @md
#' @examples
#' #
umxPlotDoC <- function(x, ...) {
	message("I'm sorry Dave, no plot for doc yet ;-(")
	# 1. draw latents
	# 2. draw manifests,
	# 3. draw ace to latents
	# 4. draw specifics to manifests (? or omit?)
	# 5. connect latents to manifests using free elements of columns of FacLoad
	# 6. add causal paths between latents

	# Chol$top$FacLoad
	# $free
	#       [,1]  [,2]
	# [1,]  TRUE FALSE
	# [2,]  TRUE FALSE
	# [3,]  TRUE FALSE
	# [4,] FALSE  TRUE
	# [5,] FALSE  TRUE
	# [6,] FALSE  TRUE

	# betas are in 
	# model$top$beta
	# $labels
	#      [,1]  [,2]
	# [1,] "a2a" "b2a"
	# [2,] "a2b" "b2b"		
}

#' @export
plot.MxModelDoC <- umxPlotDoC


#' Shows a compact, publication-style, summary of a umx Direction of Causation model
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelDoC
#' @param model a fitted [umxDoC()] model to summarize.
#' @param digits round to how many digits (default = 2).
#' @param comparison Run mxCompare on a comparison model (default NULL)
#' @param std Whether to standardize the output (default = TRUE).
#' @param showRg = whether to show the genetic correlations (FALSE).
#' @param CIs Whether to show Confidence intervals if they exist (TRUE).
#' @param report Print tables to the console (as 'markdown'), or open in browser ('html')
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output.
#' @param returnStd Whether to return the standardized form of the model (default = FALSE).
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @return - optional [mxModel()]
#' @export
#' @family Twin Reporting Functions
#' @seealso - [umxDoC()], [plot.MxModelDoC()], [umxModify()], [umxCP()], [plot()], [umxSummary()] work for IP, CP, GxE, SAT, and ACE models.
#' @md
#' @examples
#' require(umx)
#' data(twinData)
#' umx_set_auto_plot(FALSE) # turn off autoplotting for CRAN
#' mzData = subset(docData, zygosity %in% c("MZFF", "MZMM"))
#' dzData = subset(docData, zygosity %in% c("DZFF", "DZMM"))
#' DoC = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3),
#'			mzData= mzData, dzData= dzData, causal= TRUE)
#' A2B = umxModify(DoC, "a2b", free = TRUE, name = "A2B")
#' umxSummary(A2B)
umxSummaryDoC <- function(model, digits = 2, comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE , report = c("markdown", "html"), file = getOption("umx_auto_plot"), returnStd = FALSE, zero.print = ".", ...) {
	message("Summary support for DoC models not complete yet")

	# TODO: Detect value of DZ covariance, and if .25 set "C" to "D" in tables
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message(paste("Output for Model: ", thisFit$name))
			umxSummaryDoC(thisFit, digits = digits, file = file, returnStd = returnStd, showRg = showRg, comparison = comparison, std = std, CIs = CIs)
		}
	} else {
		umx_check_model(model, "MxModelDoC", beenRun = TRUE, callingFn = "umxSummaryDoC")
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar   = length(selDVs)/2
		nFac   = 2; # TODO look this up in the model

		if(CIs){
			message("CIs not supported for DoC models yet")
			# oldModel = model # Cache this in case we need it (CI stash model has string where values should be).
			# model = xmu_CI_stash(model, digits = digits, dropZeros = TRUE, stdAlg2mat = TRUE)
		} else if(any(c(std, returnStd))) {
			message("Std not support for DoC models yet")
			# model = xmu_standardize_Doc(model) # Make a standardized copy of model
		}

		# Chol= umxDoC(var1= var1, var2= var2, mzData= mzData, dzData= dzData, causal= FALSE, auto=F); Chol = mxRun(Chol)

		message("## Means")
		means = model$top$Means$values
		colnames(means) = selDVs[1:nVar]
		umx_print(means)
		
		ptable = summary(model)$parameters
		umx_print(ptable[, c("name", "Estimate", "Std.Error")])

		betaNames  = as.vector(model$top$beta$labels)
		betaValues = as.vector(model$top$beta$values)
		return()
		# model$top$beta$labels[model$top$beta$free]
		# umx_print(ptable[, c("name", "Estimate", "Std.Error")])

		message("## Common Factor paths")
		a_cp = model$top$a_cp$values # nFac * nFac matrix of path coefficients flowing into cp_loadings
		c_cp = model$top$c_cp$values
		e_cp = model$top$e_cp$values

		# Common factor ACE inputs are std to 1
		# Bind diags of a_cp, c and e columns into nFac-row matrix
		commonACE = cbind(diag(a_cp), diag(c_cp), diag(e_cp)) 
		commonACE = data.frame(commonACE, row.names = paste("Common.factor", 1:nFac, sep = "."), stringsAsFactors = FALSE);
		names(commonACE) = c ("A", "C", "E")
		if(report == "html"){
			umx_print(commonACE, digits = digits, zero.print = ".", file = "std_spec.html")
		} else {
			umx_print(commonACE, digits = digits, zero.print = ".")
		}
		
		if(class(model$top$matrices$a_cp)[1] == "LowerMatrix"){
			message("You used correlated genetic inputs to the common factor. This is the a_cp matrix")
			print(a_cp)
		}
		
		message("## Loading of each trait on the Common Factors")
		# Get standardized loadings on Common factors
		rowNames = sub("(_T)?1$", "", selDVs[1:nVar]) # Clean up names
		cp_loadings = model$top$cp_loadings$values # nVar * nFac matrix
		cp_loadings = data.frame(cp_loadings, row.names = rowNames, stringsAsFactors = FALSE);
		names(cp_loadings) = paste0("CP", 1:length(names(cp_loadings)))
		if(report == "html"){
			umx_print(cp_loadings, digits = digits, zero.print = ".", file = "std_common.html");
		} else {
			umx_print(cp_loadings, digits = digits, zero.print = ".")
		}

		message("## Specific-factor loadings")
		# Specific path coefficients ready to be stacked together
		as = model$top$as$values # Specific factor path coefficients
		cs = model$top$cs$values
		es = model$top$es$values

		specifics = data.frame(row.names = paste0('Specific ', c('a', 'c', 'e')), stringsAsFactors = FALSE,
			rbind(diag(as), 
				  diag(cs),
				  diag(es))
		)
		names(specifics) = rowNames;

		if(report == "html"){
			umx_print(specifics, digits = digits, zero.print = ".", file = "std_spec.html")
		} else {
			umx_print(specifics, digits = digits, zero.print = ".")
		}
		
		if(showRg) {
			message("Genetic Correlations")
			# Pre & post multiply covariance matrix by inverse of standard deviations
			A  = model$top$A$values # Variances
			C  = model$top$C$values
			E  = model$top$E$values
			Vtot = A + C + E; # Total variance
			nVarIden = diag(nVar)
			NAmatrix <- matrix(NA, nVar, nVar);

			rA = tryCatch(solve(sqrt(nVarIden * A)) %*% A %*% solve(sqrt(nVarIden * A)), error = function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(nVarIden * C)) %*% C %*% solve(sqrt(nVarIden * C)), error = function(err) return(NAmatrix)); # C correlations
			rE = tryCatch(solve(sqrt(nVarIden * E)) %*% E %*% solve(sqrt(nVarIden * E)), error = function(err) return(NAmatrix)); # E correlations
			genetic_correlations = data.frame(cbind(rA, rC, rE), row.names = rowNames);
			# Make a table
			names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
			if(report == "html"){
				umx_print(genetic_correlations, digits = digits, zero.print = ".", file = "geneticCorrs.html")
			} else {
				umx_print(genetic_correlations, digits = digits, zero.print = ".")
			}
			
		}
		if(!is.na(file)){
			# umxPlotCP(model, file = file, digits = digits, std = FALSE, means = FALSE)
		}
		if(returnStd) {
			invisible(model)
		}
	}
}

#' @export
umxSummary.MxModelDoC <- umxSummaryDoC

