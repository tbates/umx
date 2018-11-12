#' Build and run a simplex twin model (not ready for use!)
#' 
#' The simplex model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences. `umxSimplex` makes a 2-group simplex twin model. 
#' 
#' **This code is beta** quality: **not** for publication use. It will be completed by Boulder 2020.
#' 
#' @details
#' 
#' The simplex model decomposes phenotypic variance
#' into Additive genetic, unique environmental (E) and, optionally, either
#' common or shared-environment (C) or non-additive genetic effects (D).
#' 
#' In the simplex model, these influences are modeled as a combination of:
#' * Innovations at a given time (`ai` `ci` and `ei` matrices).
#' * Influences transmitted from previous time (`at`, `ct`, and `et` matrices).
#' * Influences specific to a single time (`as`, `cs`, `es`).
#' 
#' These combine to explain the causes of variance in the phenotype (see Figure).
#' 
#' **Simplex path diagram**:
#' 
#' \if{html}{\figure{simplex.png}{options: width="50\%" alt="Figure: simplex.png"}}
#' \if{latex}{\figure{simplex.pdf}{options: width=7cm}}
#'
#' **Data Input**
#' Currently, the umxSimplex function accepts only raw data.
#' 
#' **Ordinal Data**
#' In an important capability, the model transparently handles ordinal (binary or multi-level
#' ordered factor data) inputs, and can handle mixtures of continuous, binary, and ordinal
#' data in any combination.
#' 
#' **Additional features**
#' The `umxSimplex` function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' **Matrices and Labels in the simplex model**
#' A good way to see which matrices are used in umxSummary is to run an example model and plot it.
#'
#' The loadings specific to each time point are contained on the diagonals of matrices 
#' `as`, `cs`, and `es`. So labels relevant to modifying these are of the form "as_r1c1", "as_r2c2" etc.
#' 
#' All the shared matrices are in the model "top". So to see the 'as' values, you can simply execute:
#' 
#' `m1$top$as$values`
#' 
#' The transmitted loadings are in matrices `at`, `ct`, `et`.
#'
#' The innovations are in the matrix `ai`, `ci`, and `ei`.
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`.
#' This has 1 row, and the columns are laid out for each variable for
#' twin 1, followed by each variable for twin 2.
#' 
#' Thus, in a model where the means for twin 1 and twin 2 had been equated 
#' (set = to T1), you could make them independent again with this script:
#'
#' `m1$top$expMean$labels[1,4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")`
#'
#' @param name The name of the model (defaults to "simplex")
#' @param selDVs The BASENAMES of the variables i.e., c(`obese`), not c(`obese_T1`, `obese_T2`)
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param sep The string preceding the final numeric twin identifier (often "_T")
#' Combined with selDVs to form the full var names, i.e., just "dep" --> c("dep_T1", "dep_T2")
#' @param equateMeans Whether to equate the means across twins (defaults to TRUE).
#' @param dzAr The DZ genetic correlation (default = .5. Vary to examine assortative mating).
#' @param dzCr The DZ "C" correlation (defaults = 1. To make an ADE model, set = .25).
#' @param addStd Whether to add the algebras to compute a std model (default = TRUE).
#' @param addCI Whether to add the interval requests for CIs (default = TRUE).
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned).
#' @param optimizer Optionally set the optimizer (default NULL does nothing).
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxACE}()} for more examples of twin modeling, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{https://www.github.com/tbates/umx}
#' @examples
#' data(iqdat)
#' mzData <- subset(iqdat, zygosity == "MZ")
#' dzData <- subset(iqdat, zygosity == "DZ")
#' nTimePoints = 4 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' # IQ_age1 -> IQ_age1_T1, IQ_age1_T2,  etc.
#' m1 = umxSimplex(selDVs = baseVarNames, sep = "_T", dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' parameters(m1, patt = "^s")
#' m2 = umxModify(m1, regex = "as_r1c1", name = "no_as", comp = TRUE)
#' umxCompare(m1, m2)
#' 
#' # test a 3 time-point model
#' nTimePoints = 3 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' # IQ_age1 -> IQ_age1_T1, IQ_age1_T2,  etc.
#' m1 = umxSimplex(selDVs = baseVarNames, sep = "_T", dzData = dzData, mzData = mzData)
#' @md
umxSimplex <- function(name = "simplex", selDVs, dzData, mzData, sep = NULL, equateMeans = TRUE, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, autoRun = getOption("umx_auto_run"), optimizer = NULL) {
	message("This is beta code - will be ready for Boulder 2020")
	nSib   = 2
	xmu_twin_check(selDVs=selDVs, dzData = dzData, mzData = mzData, optimizer = optimizer, sep = sep, nSib = nSib)
	# Expand var names
	selDVs = umx_paste_names(selDVs, sep = sep, suffixes = 1:2)
	nVar   = length(selDVs)/nSib

	dataType = umx_is_cov(dzData)
	if(dataType != "raw") {
		stop("Simplex only works with raw data at present. You offered up ", omxQuotes(dataType), " data...")
	}else{
		# Drop any unused columns from mzData and dzData
		umx_check_names(selDVs, mzData)
		umx_check_names(selDVs, dzData)
		mzData = mzData[, selDVs]
		dzData = dzData[, selDVs]
	}

	# ==================================
	# = Create start values and labels =
	# ==================================
	# mzData <- subset(iqdat, zygosity == "MZ")[,-1]
	# dzData <- subset(iqdat, zygosity == "DZ")[,-1]
	# nVar = 4
	tmp = umx_mean_var_starts(mzData= mzData, dzData= dzData, selVars= selVars, nSib= nSib, varFormat= c("Cholesky"), divideBy = 3)
	varStarts  = tmp$varStarts
	meanStarts = tmp$meanStarts
	meanLabels = tmp$meanLabels
	
	model = mxModel(name,
		# 1. replace hard-coded start values in "[ace][tsi]"
		# 	t -> 0 (except first 1) DONE
		# 	s -> 0 for A and C, es = var*1/3
		#   i -> 0 var*1/3 in each of A,C. ei@0
		mxModel("top",
			# Start transmitted components at zero (except first 1)(strong positive definite solution @mikeneale)
			umxMatrix('at', 'Diag', nrow = nVar, ncol = nVar, free = TRUE , values = c(varStarts[1], rep(0, nVar-1))),
			umxMatrix('ct', 'Diag', nrow = nVar, ncol = nVar, free = TRUE , values = c(varStarts[1], rep(0, nVar-1))),
			umxMatrix('et', 'Diag', nrow = nVar, ncol = nVar, free = FALSE, values = 0.0),

			# Innovations (diag, but start 1-row down)
			xmu_simplex_corner(umxMatrix('ai', 'Full', nrow = nVar, ncol = nVar), start = varStarts[-1]),
			xmu_simplex_corner(umxMatrix('ci', 'Full', nrow = nVar, ncol = nVar), start = varStarts[-1]),
			umxMatrix('ei', 'Full', nrow = nVar, ncol = nVar, free = FALSE, values = 0.0),
			# TODO check ei fixed@zero?

			# Residuals: all values equated by label, except E
			umxMatrix('as', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, labels = "as_r1c1", values = 0),
			umxMatrix('cs', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, labels = "cs_r1c1", values = 0),
			umxMatrix('es', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, values = varStarts),

			umxMatrix('I', 'Iden', nrow = nVar, ncol = nVar),
			mxAlgebra(name= 'Iai', solve(I - ai)),
			mxAlgebra(name= 'Ici', solve(I - ci)),
			mxAlgebra(name= 'Iei', solve(I - ei)),
			mxAlgebra(name= 'A'  , Iai %*% at %*% t(Iai) + as),
			mxAlgebra(name= 'C'  , Ici %*% ct %*% t(Ici) + cs),
			mxAlgebra(name= 'E'  , Iei %*% et %*% t(Iei) + es),
			# MZ covariance matrix and mean matrix
			mxAlgebra(name = 'ACE', A + C + E),
			mxAlgebra(name = 'AC' , A + C),
			mxAlgebra(name = 'hAC', .5 %x% A + C),
			mxAlgebra(name = 'expCovMZ', cbind(rbind(ACE,  AC), rbind( AC, ACE))),
			mxAlgebra(name = 'expCovDZ', cbind(rbind(ACE, hAC), rbind(hAC, ACE))),
			umxMatrix('means', 'Full', nrow = 1, ncol = (nVar*2), free = TRUE, labels = meanLabels, values = meanStarts)
		),
		mxModel("MZ",
			mxData(mzData, type = "raw"),
			mxExpectationNormal("top.expCovMZ", means = "top.means", dimnames = selDVs),  
			mxFitFunctionML()
		),
		mxModel("DZ",
			mxData(dzData, type = "raw"),
			mxExpectationNormal("top.expCovDZ", means = "top.means", dimnames =selDVs),
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Run the model
	# Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = omxAssignFirstParameters(model) 
	model = as(model, "MxModelSimplex")

	if(autoRun){
		model = mxRun(model)
		umxSummary(model)
	}
	return(model)
} # end umxSimplex

#' Shows a compact, publication-style, summary of a Simplex model.
#'
#' Summarize a fitted Simplex model returned by \code{\link{umxSimplex}}. Can control digits, report comparison model fits,
#' optionally show the Rg (genetic and environmental correlations), and show confidence intervals. the report parameter allows
#' drawing the tables to a web browser where they may readily be copied into non-markdown programs like Word.
#'
#' See documentation for RAM models summary here: \code{\link{umxSummary.MxModel}}.
#' 
#' View documentation on the ACE model subclass here: \code{\link{umxSummary.MxModelACE}}.
#' 
#' View documentation on the IP model subclass here: \code{\link{umxSummary.MxModelIP}}.
#' 
#' View documentation on the CP model subclass here: \code{\link{umxSummary.MxModelCP}}.
#' 
#' View documentation on the GxE model subclass here: \code{\link{umxSummary.MxModelGxE}}.
#' 
#' @aliases umxSummary.MxModelSimplex
#' @param model an \code{\link{mxModel}} to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = no plot.
#' @param comparison you can run mxCompare on a comparison model (default = NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg (T/F) Whether to show the genetic correlations (default = FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (default = TRUE)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If "html", then open an html table of the results (default = 'markdown')
#' @param extended how much to report (default = FALSE)
#' @param zero.print How to show zeros (default = ".")
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - \code{\link{umxSimplex}}
#' @references - \url{https://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' data(iqdat)
#' nTimePoints = 4 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' # IQ_age + 1:4 + "_T" 1:2
#' 
#' # Select Data
#' mzData <- subset(iqdat, zygosity == "MZ")
#' dzData <- subset(iqdat, zygosity == "DZ")
#' m1 = umxSimplex(selDVs = baseVarNames, sep = "_T", dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' \dontrun{
#' umxSummary(m1, file = NA);
#' umxSummary(m1, file = "name", std = TRUE)
#' stdFit = umxSummary(m1, returnStd = TRUE)
#' }
umxSummarySimplex <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	# Depends on R2HTML::HTML
	report = match.arg(report)
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummarySimplex(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
	umx_has_been_run(model, stop = TRUE)
	umx_show_fit_or_comparison(model, comparison = comparison, digits = digits)
	# Starting Values
	selDVs = model$MZ$expectation$dims
	nVar   = length(selDVs)/2;

	if(std){
		# Calculate standardized variance components
		message("Standardized solution (note: std is alpha quality)")
		model = umx_standardize_Simplex(model)
	} else {
		message("Raw solution")
	}
	# shit-sticks (tm)
	at = diag(model$top$at$values)
	ct = diag(model$top$ct$values)
	et = diag(model$top$et$values)
	as = diag(model$top$as$values) # Do these ever not all equal each other?
	cs = diag(model$top$cs$values)
	es = diag(model$top$es$values)
	ai = diag(model$top$ai$values[-1, ])
	ci = diag(model$top$ci$values[-1, ])
	ei = diag(model$top$ei$values[-1, ])

	All_t = data.frame(rbind(at, ct, et), row.names = c("at", "ct", "et")); names(All_t) <- selDVs[1:nVar]
	All_s = data.frame(rbind(as, cs, es), row.names = c("as", "cs", "es")); names(All_s) <- selDVs[1:nVar]
	All_i = data.frame(rbind(ai, ci, ei), row.names = c("ai", "ci", "ei")); names(All_i) <- selDVs[2:nVar]
	if(report == "html"){
		message("## Transmitted Influences")
		umx_print(All_t, digits = digits, zero.print = ".", file = "trans.html")
		message("## Innovations")
		umx_print(All_i, digits = digits, zero.print = ".", file = "innov.html")
		message("## Specific Effects")
		umx_print(All_s, digits = digits, zero.print = ".", file = "spec.html")
	} else {
		message("\n##-------------------##\n## Transmitted Influences")
		umx_print(All_t, digits = digits, zero.print = ".")
		message("\n##-------------------##\n## Innovations")
		umx_print(All_i, digits = digits, zero.print = ".")
		message("\n##-------------------##\n## Specific Effects")
		umx_print(All_s, digits = digits, zero.print = ".")
	}
	return()

	# ============================
	# = Just Junk down here now? =
	# ============================

	AClean[upper.tri(AClean)] = NA
	CClean[upper.tri(CClean)] = NA
	EClean[upper.tri(EClean)] = NA
	rowNames  = sub("(_T)?1$", "", selDVs[1:nVar])
	Estimates = data.frame(cbind(AClean, CClean, EClean), row.names = rowNames, stringsAsFactors = FALSE);

	colNames = c("A", "C", "E")
	if(model$top$dzCr$values == .25){
		colNames = c("A", "D", "E")
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
		AClean = A
		CClean = C
		EClean = E
		AClean[upper.tri(AClean)] = NA
		CClean[upper.tri(CClean)] = NA
		EClean[upper.tri(EClean)] = NA
		unStandardizedEstimates = data.frame(cbind(AClean, CClean, EClean), row.names = rowNames);
		names(unStandardizedEstimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
		umx_print(unStandardizedEstimates, digits = digits, zero.print = zero.print)
	}

	# Pre & post multiply covariance matrix by inverse of standard deviations
	if(showRg) {
		message("Genetic correlations")
		NAmatrix <- matrix(NA, nVar, nVar);
		# genetic correlations, C and E correlations
		rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); 
		rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); 
		rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); 
		rAClean = rA
		rCClean = rC
		rEClean = rE
		rAClean[upper.tri(rAClean)] = NA
		rCClean[upper.tri(rCClean)] = NA
		rEClean[upper.tri(rEClean)] = NA
		genetic_correlations = data.frame(cbind(rAClean, rCClean, rEClean), row.names = rowNames);
		names(genetic_correlations) <- rowNames
	 	# Make a nice table.
		names(genetic_correlations) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
		umx_print(genetic_correlations, digits = digits, zero.print = zero.print)
	}
	hasCIs = umx_has_CIs(model)
		if(hasCIs & CIs) {
			# TODO umxACE CI code: Need to refactor into some function calls...
			# TODO and then add to umxSummaryIP and CP
			message("Creating CI-based report!")
			# CIs exist, get lower and upper CIs as a dataframe
			CIlist = data.frame(model$output$confidenceIntervals)
			# Drop rows fixed to zero
			CIlist = CIlist[(CIlist$lbound != 0 & CIlist$ubound != 0), ]
			# Discard rows named NA
			CIlist = CIlist[!grepl("^NA", row.names(CIlist)), ]
			# TODO fix for singleton CIs
			CIlist <- CIlist[, c("lbound", "estimate", "ubound")] 
			CIlist$fullName = row.names(CIlist)
			# Initialise empty matrices for the CI results
			rows = dim(model$top$matrices$a$labels)[1]
			cols = dim(model$top$matrices$a$labels)[2]
			A_CI = C_CI = E_CI = matrix(NA, rows, cols)

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
				CIparts        = round(CIlist[n, c("estimate", "lbound", "ubound")], digits)
				thisString     = paste0(CIparts[1], " [",CIparts[2], ", ",CIparts[3], "]")

				if(grepl("^A", thisMatrixName)) {
					a_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^C", thisMatrixName)){
					c_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else if(grepl("^E", thisMatrixName)){
					e_CI[thisMatrixRow, thisMatrixCol] = thisString
				} else{
					stop(paste("Illegal matrix name: must begin with A, C, or E. You sent: ", thisMatrixName))
				}
			}
			Estimates = data.frame(cbind(A_CI, C_CI, E_CI), row.names = rowNames, stringsAsFactors = FALSE)
			names(Estimates) = paste0(rep(colNames, each = nVar), rep(1:nVar));
			Estimates = umx_print(Estimates, digits = digits, zero.print = zero.print)
			if(report == "html"){
				# Depends on R2HTML::HTML
				R2HTML::HTML(Estimates, file = "tmpCI.html", Border = 0, append = F, sortableDF = T); 
				umx_open("tmpCI.html")
			}
			CI_Fit = model
			CI_Fit$top$A$values = A_CI
			CI_Fit$top$C$values = C_CI
			CI_Fit$top$E$values = E_CI
		} # end Use CIs
	} # end list catcher?
	
	if(!is.na(file)) {
		if(hasCIs & CIs){
			umxPlotSimplex(CI_Fit, file = file, std = FALSE)
		} else {
			umxPlotSimplex(model, file = file, std = std)
		}
	}
	if(returnStd) {
		if(CIs){
			message("If you asked for CIs, returned model is not runnable (contains CIs not parameter values)")
		}
		umx_standardize(model)
	}
}

#' @export
umxSummary.MxModelSimplex <- umxSummarySimplex


#' Draw and display a graphical figure of a simplex model
#'
#' Options include digits (rounding), showing means or not, and which output format is desired.
#'
#' @aliases plot.MxModelSimplex
#' @param x The \code{\link{umxSimplex}} model to display graphically
#' @param file The name of the dot file to write: NA = none; "name" = use the name of the model
#' @param digits How many decimals to include in path loadings (defaults to 2)
#' @param means Whether to show means paths (defaults to FALSE)
#' @param std Whether to standardize the model (defaults to TRUE)
#' @param format = c("current", "graphviz", "DiagrammeR") 
#' @param ... Optional additional parameters
#' @return - Optionally return the dot code
#' @export
#' @seealso - \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, simplex, ACEv, or ACE model.
#' @seealso - \code{\link{umxSimplex}}
#' @family Plotting functions
#' @examples
#' \dontrun{
#' # TODO Add code (from umxSimplex) to build simplex model help
#' data(iqdat)
#' mzData <- subset(iqdat, zygosity == "MZ")
#' dzData <- subset(iqdat, zygosity == "DZ")
#' nTimePoints = 4 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' selDVs = tvars(baseVarNames, sep = "_T", suffixes= 1:2)
#' m1 = umxSimplex(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' plot(m1)
#' }
umxPlotSimplex <- function(x = NA, file = "name", digits = 2, means = FALSE, std = TRUE,  format = c("current", "graphviz", "DiagrammeR"), ...) {
	if(!class(x) == "MxModelSimplex"){
		stop("The first parameter of umxPlotCP must be a CP model, you gave me a ", class(x))
	}
	format = match.arg(format)
	model = x # Just to emphasise that x has to be a model 
	if(std){
		message("std is beta for simplex plot")
		model = umx_standardize_Simplex(model)
	}
	parameterKeyList = omxGetParameters(model)

	nVar   = dim(model$top$as$values)[[1]]
	selDVs = dimnames(model$MZ$data$observed)[[2]]
	selDVs = selDVs[1:(nVar)]
	selDVs = sub("(_T)?[0-9]$", "", selDVs)
	asLatents = paste0("as", 1:nVar)
	csLatents = paste0("cs", 1:nVar)
	esLatents = paste0("es", 1:nVar)

	atLatents = paste0("at", 1:nVar)
	ctLatents = paste0("ct", 1:nVar)
	etLatents = paste0("et", 1:nVar)

	aiLatents = paste0("ai", 2:(nVar))
	ciLatents = paste0("ci", 2:(nVar))
	eiLatents = paste0("ei", 2:(nVar))

	manifests = selDVs
	latents   = c(atLatents, aiLatents, asLatents);

	pre = "# Latents\n"
	for(var in latents) {
	   pre = paste0(pre, "\t", var, " [shape = circle];\n")
	}
	pre = paste0(pre, "\n# Manifests\n")
	for(n in c(1:nVar)) {
	   pre = paste0(pre, "\n\t", selDVs[n], " [shape = square];")
	}

	# TODO add a loop for a, c, e
	val = round(model$top$at$values[1, 1], digits)
	out = paste0("\n", "at1 -> at1 [dir=both, label=\"", val, "\"]")
	out = paste0(out, ";\n", "at1 -> ", selDVs[1], " [label=\"1\"]")
	val = round(model$top$as$values[1, 1], digits)
	out = paste0(out, ";\n", "as1 -> as1 [dir=both, label=\"", val, "\"]")
	out = paste0(out, ";\n", "as1 -> ", selDVs[1], " [label=\"1\"]")
	for (i in 2:nVar) {
		# 1's
		out = paste0(out, ";\n", "ai", i, " -> at", i, " [label=\"1\"]")
		out = paste0(out, ";\n", "at", i, " -> ", selDVs[i], " [label=\"1\"]")
		out = paste0(out, ";\n", "as", i, " -> ", selDVs[i], " [label=\"1\"]")

		# self2self var
		val = round(model$top$ai$values[i, i], digits)
		out = paste0(out, ";\n", "ai", i, " -> ai", i, " [dir=both, label=\"", val, "\"]")
		val = round(model$top$as$values[i, i], digits)
		out = paste0(out, ";\n", "as", i, " -> as", i, " [dir=both, label=\"", val, "\"]")

		val = round(model$top$at$values[i, i], digits)
		out = paste0(out, ";\n", "at", (i-1), " -> at", i, " [label=\"", val, "\"]")

	}
	ranks = paste0("\n{rank=min; ", paste0("ai", 2:nVar, collapse = " "),  "};")
	ranks = paste0(ranks, "\n{rank=same; ", paste0(selDVs, collapse = " "),  "};")
	ranks = paste0(ranks, "\n{rank=same; ", paste0("at", 1:nVar, collapse = " "),  "};")
	ranks = paste0(ranks, "\n{rank=max; ", paste0("as", 1:nVar, collapse = " "),  "};")
	# ranks = paste0("{rank=sink; ", paste(cSpecifics, collapse = "; "), "}");

	# CIstr = umx_APA_model_CI(model, cellLabel = thisParam, prefix = "top.", suffix = "_std", digits = digits)

	digraph = paste0("digraph G {\nsplines=\"FALSE\";\n", pre, ranks, out, "\n}");
	if(format != "current"){
		umx_set_plot_format(format)
	}
	xmu_dot_maker(model, file, digraph)
}

#' @export
plot.MxModelSimplex <- umxPlotSimplex

#' Standardize a Simplex twin model
#'
#' umx_standardize_Simplex
#'
#' @param model an \code{\link{umxSimplex}} model to standardize
#' @param ... Other options
#' @return - Standardized Simplex \code{\link{umxSimplex}} model
#' @export
#' @family zAdvanced Helpers
#' @references - \url{https://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' data(iqdat)
#' mzData = subset(iqdat, zygosity == "MZ")
#' dzData = subset(iqdat, zygosity == "DZ")
#' m1  = umxSimplex(selDVs = paste0("IQ_age", 1:4), sep = "_T", dzData = dzData, mzData = mzData)
#' std = umx_standardize_Simplex(m1)
umx_standardize_Simplex <- function(model, ...) {
	if(typeof(model) == "list"){ # Call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umx_standardize(thisFit)
		}
	} else {
		if(!umx_has_been_run(model)){
			stop("I can only standardize Simplex models that have been run. Just do\n",
			"yourModel = mxRun(yourModel)")
		}
		selDVs = model$MZ$expectation$dims
		nVar   = length(selDVs)/2;

		# Get raw values
		At = model$top$at$values
		Ct = model$top$ct$values
		Et = model$top$et$values

		As = model$top$as$values # Do these ever not all equal each other?
		Cs = model$top$cs$values
		Es = model$top$es$values

		Ai = model$top$ai$values
		Ci = model$top$ci$values
		Ei = model$top$ei$values

		# Calculate standardized variance components
		I = model$top$I$values   # nVar Identity matrix
		ACE = model$top$ACE$result  # A +C + E = Total variance
		InvVAR = solve(I * ACE) # Inverse of diagonal matrix of SDs

		model$top$at$values = InvVAR %*% At # standardized at
		model$top$ct$values = InvVAR %*% Ct # standardized ct
		model$top$et$values = InvVAR %*% Et # standardized et

		model$top$as$values = InvVAR %*% As # standardized as
		model$top$cs$values = InvVAR %*% Cs # standardized cs
		model$top$es$values = InvVAR %*% Es # standardized es

		model$top$ai$values = InvVAR %*% Ai # standardized ai
		model$top$ci$values = InvVAR %*% Ci # standardized ci
		model$top$ei$values = InvVAR %*% Ei # standardized ei
	
		return(model)
	}
}
#' @export
umx_standardize.MxModelSimplex <- umx_standardize_Simplex

# umx_standardize_Simplex <- function(model, ...) {
# 	if(typeof(model) == "list"){ # Call self recursively
# 		for(thisFit in model) {
# 			message("Output for Model: ", thisFit$name)
# 			umx_standardize(thisFit)
# 		}
# 	} else {
# 		if(!umx_has_been_run(model)){
# 			stop("I can only standardize Simplex models that have been run. Just do\n",
# 			"yourModel = mxRun(yourModel)")
# 		}
# 		selDVs = model$MZ$expectation$dims
# 		nVar   = length(selDVs)/2;
#
# 		# Get raw values
# 		at = model$top$at$values
# 		ct = model$top$ct$values
# 		et = model$top$et$values
#
# 		as = model$top$as$values # Do these ever not all equal each other?
# 		cs = model$top$cs$values
# 		es = model$top$es$values
#
# 		ai = model$top$ai$values
# 		ci = model$top$ci$values
# 		ei = model$top$ei$values
#
# 		# Calculate standardized variance components
# 		I = model$top$I$values   # nVar Identity matrix
# 		ACE = model$top$ACE$result  # A +C + E = Total variance
# 		InvSD = solve(sqrt(I * ACE)) # Inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.ACE))~"
#
# 		# Put Standardized _path_ coefficients back into model
# 		# model$top$A@result = InvSD %&% A # Standardized variance-covariance components
# 		# model$top$C@result = InvSD %&% C
# 		# model$top$E@result = InvSD %&% E
#
# 		model$top$at$values = InvSD %*% at # standardized at
# 		model$top$ct$values = InvSD %*% ct # standardized ct
# 		model$top$et$values = InvSD %*% et # standardized et
#
# 		model$top$as$values = InvSD %*% as # standardized as
# 		model$top$cs$values = InvSD %*% cs # standardized cs
# 		model$top$es$values = InvSD %*% es # standardized es
#
# 		model$top$ai$values = InvSD %*% ai # standardized ai
# 		model$top$ci$values = InvSD %*% ci # standardized ci
# 		model$top$ei$values = InvSD %*% ei # standardized ei
#
# 		return(model)
# 	}
# }
# #' @export
# umx_standardize.MxModelSimplex <- umx_standardize_Simplex
