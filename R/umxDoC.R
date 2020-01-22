# Direction of Causation Modeling 
# http://ibg.colorado.edu/cdrom2016/verhulst/Causation/DOC%20student.R

# TODO: plot.MxModelDoC, and umxSummary.MxModel_DoC methods
# TODO: support D for one or both traits

#' Build and run a 2-group Direction of Causation twin models.
#'
#' @description
#' Scientific theory makes claims about causation, but in social science, testing causal claims is often difficult due to an inability to 
#' conduct experimental randomization of traits and situations to people. When twins are available, even when measured on a single occasion, 
#' the pattern of cross-twin cross-trait correlations can (given distinct modes of inheritance) falsify causal hypotheses
#' 
#' Classical twin modeling uses the genetic and environmental differences 
#' among pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together.
#' 
#' `umxDoC` implements a 2-group model to form latent variables for each of two traits, and allows testing whether 
#' trait 1 causes trait 2, vice-versa, or even reciprocal causation.
#' 
#' The following figure shows how the DoC model appears as a path diagram (for two latent variables X and Y,
#' each with three indicators), with model shown only for 1 twin, and only one DoC pathway drawn.
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
#' @param dzData The DZ dataframe.
#' @param mzData The MZ dataframe.
#' @param sep The separator in twin variable names, often "_T", e.g. "dep_T1". Simplifies selDVs.
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param intervals Whether to run mxCI confidence intervals (default = FALSE)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @return - [mxModel()] of subclass mxModel.DOC
#' @export
#' @family Twin Modeling Functions
#' @seealso - [umxPlotDOC()], [umxSummaryDOC()], [umxModify()]
#' @references - N.A. Gillespie and N.G. Marting (2005). Direction of Causation Models. 
#' In *Encyclopedia of Statistics in Behavioral Science*, Volume 1 pp. 496–499. Eds. Brian S. Everitt & David C. Howell
#' @md
#' @examples
#' \dontrun{
#' # ========================
#' # = Does Rain cause Mud? =
#' # ========================
#'
#' # ================
#' # = Prepare Data =
#' # ================
#' mzData = read.csv("~/Desktop/DOC/MZdata.csv")
#' dzData = read.csv("~/Desktop/DOC/DZdata.csv")
#' varNames = paste0(rep(c("a", "b"), each= 3), c(1:3))
#'
#' names(mzData) = c("X", paste0(varNames, "_T1"), paste0(varNames, "_T2"), "sex_T1", "sex_T2")
#' names(dzData) = c("X", paste0(varNames, "_T1"), paste0(varNames, "_T2"), "sex_T1", "sex_T2")
#'
#' Chol  = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3), 
#' 			mzData= mzData, dzData= dzData, sep = "_T", causal= FALSE)
#' DoC   = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3),
#'			mzData= mzData, dzData= dzData, sep = "_T", causal= TRUE)
#' A2B   = umxModify(DoC, "a2b", free = TRUE, name = "A2B"); summary(A2B)
#' B2A   = umxModify(DoC, "b2a", free = TRUE, name = "B2A"); summary(B2A)
#' Recip = umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip"); summary(Recip)
#'
#' Chol = umxDoC(var1= paste0("SOS", 1:8), var2= paste0("Vocab", 1:10),mzData= mzData, dzData= dzData, 
#' 			sep = "_T", causal= FALSE, auto=FALSE); Chol = mxRun(Chol)
#' DoC = umxDoC(var1= paste0("SOS", 1:8), var2= paste0("Vocab", 1:10), mzData= mzData, dzData= dzData,
#' 			sep = "_T", causal= TRUE, auto=FALSE); DoC = mxRun(DoC)
#' A2B   = umxModify(DoC, "a2b", free = TRUE, name = "A2B", auto=F); A2B = mxRun(A2B)
#' B2A   = umxModify(DoC, "b2a", free = TRUE, name = "B2A", auto=F); B2A = mxRun(B2A)
#' Recip = umxModify(DoC, c("a2b", "b2a"), free = TRUE, name = "Recip", auto=F); Recip = mxRun(Recip)
#'
#' umxCompare(Chol, c(A2B, B2A, Recip))
#' }
#' 
umxDoC <- function(name = "DOC", var1Indicators, var2Indicators, mzData= NULL, dzData= NULL, sep = NULL, causal= TRUE, autoRun = getOption("umx_auto_run"), intervals = FALSE, tryHard = c("no", "yes", "ordinal", "search"), optimizer = NULL) {
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

	# ================
	# = Make FacLoad =
	# ================
	# 1. make matrix, initialised to fixed @ 0
	FacLoad = umxMatrix(name="FacLoad", "Full", nrow=nVar, ncol=nLat, free= FALSE, values = 0)
	# 2. set FacLoad manifest loadings to pattern of 0 and 1
	FacLoad$free[1:nLat1                  ,1] = TRUE
	FacLoad$values[1:nLat1                ,1] = 1
	FacLoad$free[(nLat1+1):(nLat1+nLat2)  ,2] = TRUE
	FacLoad$values[(nLat1+1):(nLat1+nLat2),2] = 1


	top = mxModel("top", # (was "ACE")
		umxMatrix("dzAr", "Full", nrow=nLat, ncol=nLat, free=FALSE, values= c(1,.5,.5,1) ), # Heredity Matrix for DZ
		umxMatrix("Unit", "Full", nrow=nLat, ncol=nLat, free=FALSE, values= 1 ),            # Unit Matrix - For Com Env and MZ
		umxMatrix("Iden", "Iden", nrow=nSib, ncol=nSib),                                   # Identity matrix (2by2: 1s on diag, 0 off diag)

		# Matrices for Cholesky (swapped out after if causal)
		umxMatrix("a", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Genetic effects on Latent Variables 
		umxMatrix("c", type="Lower", nrow=nLat, ncol=nLat, free=TRUE, values= .2),                # Common env effects on Latent Variables
		umxMatrix("e", type="Lower", nrow=nLat, ncol=nLat, free= c(FALSE,TRUE,FALSE), values= 1), # Non-shared env effects on Latent Variables 

		# 4x4 Matrices for A, C, and E
		mxAlgebra(name="A"  , Unit  %x% (a %*% t(a))),
		mxAlgebra(name="Adz", dzAr  %x% (a %*% t(a))),
		mxAlgebra(name="C"  , Unit  %x% (c %*% t(c))),
		mxAlgebra(name="E"  , Iden  %x% (e %*% t(e))),
		mxAlgebra(name="Vmz", A   + C + E),
		mxAlgebra(name="Vdz", Adz + C + E),

		### Generate the Asymmetric Matrix
		# Non-shared env effects on Latent Variables 
		umxMatrix("beta", "Full", nrow=nLat, ncol=nLat, free=FALSE, labels = c("a2a", "a2b", "b2a", "b2b"), values= 0),
		mxAlgebra(name= "cause", Iden %x% solve(Iden - beta)),

		### Generate the Factor Loading Matrix
		FacLoad,
		mxAlgebra(name="FacLoadtw", Iden %x% FacLoad),

		## Covariance between the items due to the latent factors
		mxAlgebra(name= "FacCovMZ", FacLoadtw %&% (cause %&% Vmz)),
		mxAlgebra(name= "FacCovDZ", FacLoadtw %&% (cause %&% Vdz)),
		# Matrices to store  a, c, and e "specific" path coefficients (residuals of manifest phenotypes)
		# TODO smart var starts here
		umxMatrix(name= "as", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "cs", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		umxMatrix(name= "es", "Diag", nrow=nVar, ncol=nVar, free=TRUE, values=0.3),
		mxAlgebra(name= "Asmz", Unit %x% as),
		mxAlgebra(name= "Asdz", dzAr %x% as),
		mxAlgebra(name= "Cstw", Unit %x% cs),
		mxAlgebra(name= "Estw", Iden %x% es),
		mxAlgebra(name= "specCovMZ", Asmz + Cstw + Estw),
		mxAlgebra(name= "specCovDZ", Asdz + Cstw + Estw),
		# Expected Covariance Matrices for MZ and DZ
		mxAlgebra(name= "expCovMZ", FacCovMZ + specCovMZ),
		mxAlgebra(name= "expCovDZ", FacCovDZ + specCovDZ),

		# Means for the Manifest Variables # TODO Better starts for means... (easy)
		umxMatrix(name="Means", "Full", nrow= 1, ncol= nVar, free= TRUE, values= .1),
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

		# Replace lower ace Matrices with diag for DOC script.
		# Because covariance between the traits is "caused", theses matrices are diagonal instead of lower
		top = mxModel(top,
			umxMatrix("a", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Genetic effects on Latent Variables 
			umxMatrix("c", "Diag", nrow=nLat, ncol=nLat, free=TRUE, values=0.2), # Common env effects on Latent Variables
			umxMatrix("e", "Diag", nrow=nLat, ncol=nLat, free=FALSE,values=1)    # Non-shared env effects on Latent Variables 
		)
		model = mxModel("DOC", top, MZ, DZ, mxFitFunctionMultigroup(c("MZ", "DZ")) )		
	}
	# Factor loading matrix of Intercept and Slope on observed phenotypes
	# SDt = mxAlgebra(name= "SDt", solve(sqrt(Iden *Rt))) # Standardized deviations (inverse)
	model = omxAssignFirstParameters(model)
	model = as(model, "MxModelDoC") # set class so that S3 plot() dispatches
	model = xmu_safe_run_summary(model, autoRun = autoRun, tryHard = tryHard, std = TRUE)
	return(model)
}


#' Shows a compact, publication-style, summary of a umx Direction of Causation model
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases umxSummary.MxModelDoC
#' @param model an [mxModel()] to summarize.
#' @param digits round to how many digits (default = 2).
#' @param comparison you can run mxCompare on a comparison model (NULL).
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = do not create plot output.
#' @param std Whether to standardize the output (default = TRUE).
#' @param showRg = whether to show the genetic correlations (FALSE).
#' @param CIs Whether to show Confidence intervals if they exist (TRUE).
#' @param returnStd Whether to return the standardized form of the model (default = FALSE).
#' @param report If "html", then open an html table of the results.
#' @param extended how much to report (FALSE).
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary.
#' @return - optional [mxModel()]
#' @export
#' @family Twin Reporting Functions
#' @seealso - [umxDoC()], [plot.MxModelDoC()], [umxModify()]
#' @references - <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @md
#' @examples
# #' require(umx)
# #' data(twinData)
# #' selDVs = c("bmi1", "bmi2")
# #' mzData <- subset(twinData, zygosity == "MZFF")
# #' dzData <- subset(twinData, zygosity == "DZFF")
# #' DoC   = umxDoC(var1= paste0("a", 1:3), var2 = paste0("b", 1:3),
# #'			mzData= mzData, dzData= dzData, sep = "_T", causal= TRUE)
# #' A2B   = umxModify(DoC, "a2b", free = TRUE, name = "A2B"); summary(A2B)
# #' umxSummary(m1)
umxSummaryDoC <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	report = match.arg(report)
	commaSep = paste0(umx_set_separator(silent=TRUE), " ")
	# depends on R2HTML::HTML
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummaryDoC(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		xmu_show_fit_or_comparison(model, comparison = comparison, digits = digits)
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar <- length(selDVs)/2;
		# TODO umxSummaryACE these already exist if a_std exists..
		# TODO replace all this with xmu_standardizeACE
		# Calculate standardized variance components
		a  <- mxEval(top.a, model); # Path coefficients
		c  <- mxEval(top.c, model);
		e  <- mxEval(top.e, model);
		A  <- mxEval(top.A, model); # Variances
		C  <- mxEval(top.C, model);
		E  <- mxEval(top.E, model);

		if(std){
			message("Standardized solution")
			Vtot = A + C + E;         # Total variance
			I  <- diag(nVar);         # nVar Identity matrix
			SD <- solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations
			# (same as "(\sqrt(I.Vtot))~"

			# Standardized _path_ coefficients ready to be stacked together
			a_std <- SD %*% a; # Standardized path coefficients
			c_std <- SD %*% c;
			e_std <- SD %*% e;
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			message("Raw solution")
			aClean = a
			cClean = c
			eClean = e
		}

		aClean[upper.tri(aClean)] = NA
		cClean[upper.tri(cClean)] = NA
		eClean[upper.tri(eClean)] = NA
		rowNames = sub("(_T)?1$", "", selDVs[1:nVar])
		Estimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames, stringsAsFactors = FALSE);

		if(model$top$dzCr$values == .25){
			colNames = c("a", "d", "e")
		} else {
			colNames = c("a", "c", "e")
		}
		names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
		if(report == "html"){
			# depends on R2HTML::HTML
			R2HTML::HTML(Estimates, file = "tmp.html", Border = 0, append = F, sortableDF = T); 
			umx_open("tmp.html")
		}
	
		if(extended == TRUE) {
			message("Unstandardized path coefficients")
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)] = NA
			cClean[upper.tri(cClean)] = NA
			eClean[upper.tri(eClean)] = NA
			unStandardizedEstimates = data.frame(cbind(aClean, cClean, eClean), row.names = rowNames);
			names(unStandardizedEstimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print)
		}

	hasCIs = umx_has_CIs(model)
	if(hasCIs & CIs) {
		# TODO umxACE CI code: Need to refactor into some function calls...
		# TODO and then add to umxSummaryIP and CP
		message("Creating CI-based report!")
		# CIs exist, get lower and upper CIs as a dataframe
		CIlist = data.frame(model$output$confidenceIntervals)
		# Drop rows fixed to zero
		CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0),]
		# Discard rows named NA
		CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
		# TODO fix for singleton CIs
		# THIS IS NOT NEEDED: confidenceIntervals come with estimate in the middle now...
		# These can be names ("top.a_std[1,1]") or labels ("a_r1c1")
		# imxEvalByName finds them both
		# outList = c();
		# for(aName in row.names(CIlist)) {
		# 	outList <- append(outList, imxEvalByName(aName, model))
		# }
		# # Add estimates into the CIlist
		# CIlist$estimate = outList
		# reorder to match summary
		# CIlist <- CIlist[, c("lbound", "estimate", "ubound")]
		CIlist$fullName = row.names(CIlist)
		# Initialise empty matrices for the CI results
		rows = dim(model$top$matrices$a$labels)[1]
		cols = dim(model$top$matrices$a$labels)[2]
		a_CI = c_CI = e_CI = matrix(NA, rows, cols)

		# iterate over each CI
		labelList = imxGenerateLabels(model)	
		rowCount = dim(CIlist)[1]
		# return(CIlist)
		for(n in 1:rowCount) { # n = 1
			thisName = row.names(CIlist)[n] # thisName = "a11"
				# convert labels to [bracket] style
				if(!umx_has_square_brackets(thisName)) {
				nameParts = labelList[which(row.names(labelList) == thisName),]
				CIlist$fullName[n] = paste(nameParts$model, ".", nameParts$matrix, "[", nameParts$row, ",", nameParts$col, "]", sep = "")
			}
			fullName = CIlist$fullName[n]

			thisMatrixName = sub(".*\\.([^\\.]*)\\[.*", replacement = "\\1", x = fullName) # .matrix[
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
			} else{
				stop(paste("Illegal matrix name: must begin with a, c, or e. You sent: ", thisMatrixName))
			}
		}
		# TODO Check the merge of a_, c_ and e_CI INTO the output table works with more than one variable
		# TODO umxSummaryDoC: Add option to use mxSE
		# print(a_CI)
		# print(c_CI)
		# print(e_CI)
		Estimates = data.frame(cbind(a_CI, c_CI, e_CI), row.names = rowNames, stringsAsFactors = FALSE)
		names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
		if(report == "html"){
			# depends on R2HTML::HTML
			R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T); 
			umx_open("tmpCI.html")
		}
		CI_Fit = model
		CI_Fit$top$a$values = a_CI
		CI_Fit$top$c$values = c_CI
		CI_Fit$top$e$values = e_CI
	} # end Use CIs
	} # end list catcher?
	
	
	if(!is.na(file)) {
		# message("making dot file")
		if(hasCIs & CIs){
			umxPlotDoC(CI_Fit, file = file, std = FALSE)
		} else {
			umxPlotDoC(model, file = file, std = std)
		}
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		# xmu_standardize_DoC(model)
	}
}

#' @export
umxSummary.MxModelDoC <- umxSummaryDoC

#' Plot a Direction of Causation Model.
#'
#' Summarize a fitted model returned by [umxDoC()]. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for other umx models here: [umxSummary()].
#' 
#' @aliases plot.MxModelDoC
#' @param model a [umxDOC()] to summarize.
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
umxPlotDoC <- function(model, ...) {
	message("I'm sorry Dave, no plot for doc yet ;-(")
}

#' @export
plot.MxModelDoC <- umxPlotDoC
