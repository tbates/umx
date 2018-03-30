# TODO 
# 3. Rename BeA-> ai; BeC -> ci; BeE -> ei
# 1. Rename PsA->  a; PsC ->  c; PsE ->  e
# 2. Rename TeA-> as; TeC -> cs; TeE -> es

# ✓ Automate'BeA', 'BeC', 'BeE' matrix (currently hand-built)
# https://ibg.colorado.edu/dokuwiki/doku.php?id=workshop:2018:cdrom

#' Build and run a simplex twin model
#'
#' THIS IS BOILER PLATE AWAITING UPDATING FOR THIS BRAND_NEW FUNCTION IN BETA
#' 
#' Make a 2-group simplex twin model
#' The common-pathway model provides a powerful tool for theory-based decomposition of genetic
#' and environmental differences.
#' 
#' umxCP supports this with pairs of mono-zygotic (MZ) and di-zygotic (DZ) twins reared together
#' to model the genetic and environmental structure of multiple phenotypes
#' (measured behaviors).
#' 
#' Simplex path diagram:
#' 
#' \figure{simplex.png}
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
#' C, and E influences impact on one or more latent factors which in turn account for variance in the phenotypes (see Figure below).
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
#' The umxSummary function supports varying the DZ genetic association (defaulting to .5)
#' to allow exploring assortative mating effects, as well as varying the DZ \dQuote{C} factor
#' from 1 (the default for modeling family-level effects shared 100% by twins in a pair),
#' to .25 to model dominance effects.
#'
#' \strong{Matrices and Labels in the simplex model}
#' A good way to see which matrices are used in umxSummary is to run an example model and plot it.
#'
#' The loadings specific to each time point are contained on the diagonals of matrices 
#' `as`, `cs`, and `es`. So labels relevant to modifying these are of the form "as_r1c1", "as_r2c2" etc.
#' 
#' All the shared matrices are in the model "top". So to see the 'as' values, you can simply execute:
#' 
#' m1$top$as$values
#' 
#' The transmitted loadings are in matrices at, ct, et.
#'
#' The innovations are in the matrix `ai`, `ci`, and `ei`.
#'	
#' Less commonly-modified matrices are the mean matrix `expMean`. This has 1 row, and the columns are laid out for each variable for twin 1, followed by each variable for twin 2.
#' 
#' So, in a model where the means for twin 1 and twin 2 had been equated (set = to T1), you could make them independent again with this script:
#'
#' m1$top$expMean$labels[1,4:6] =  c("expMean_r1c4", "expMean_r1c5", "expMean_r1c6")
#'
#' @param name The name of the model (defaults to "CP")
#' @param selDVs The variables to include
#' @param dzData The DZ dataframe
#' @param mzData The MZ dataframe
#' @param sep The suffix for twin 1 and twin 2, often "_T". If set, selDVs is just the base variable names.
#' omit suffixes in selDVs, i.e., just "dep" not c("dep_T1", "dep_T2")
#' @param equateMeans Whether to equate the means across twins (defaults to T)
#' @param dzAr The DZ genetic correlation (defaults to .5, vary to examine assortative mating)
#' @param dzCr The DZ "C" correlation (defaults to 1: set to .25 to make an ADE model)
#' @param addStd Whether to add the algebras to compute a std model (defaults to TRUE)
#' @param addCI Whether to add the interval requests for CIs (defaults to TRUE)
#' @param autoRun Whether to mxRun the model (default TRUE: the estimated model will be returned)
#' @param optimizer Optionally set the optimizer (default NULL does nothing)
#' @param suffix Allowed as a synonym for sep (will be deprecated).
#' @return - \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @seealso - \code{\link{umxACE}()} for more examples of twin modeling, \code{\link{plot}()}, \code{\link{umxSummary}()} work for IP, CP, GxE, SAT, and ACE models.
#' @references - \url{http://www.github.com/tbates/umx}
#' @examples
#' data(iqdat)
#' mzData <- subset(iqdat, zygosity == "MZ")
#' dzData <- subset(iqdat, zygosity == "DZ")
#' nTimePoints = 4 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' selDVs = tvars(baseVarNames, sep = "_T", suffixes= 1:2)
#' # IQ_age1_T1, IQ_age2_T1, IQ_age3_T1, IQ_age4_T1, IQ_age1_T2, IQ_age2_T2, IQ_age3_T2, IQ_age4_T2,
#' m1 = umxSimplex(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#' parameters(m1, patt = "^c")
#' m2 = umxModify(m1, regex = "TeA_r1c1", name = "dropA", comp = TRUE)
#' umxCompare(m1, m2)
umxSimplex <- function(name = "simplex", selDVs, dzData, mzData, sep = NULL, equateMeans = TRUE, dzAr = .5, dzCr = 1, addStd = TRUE, addCI = TRUE, autoRun = getOption("umx_auto_run"), optimizer = NULL, suffix = NULL) {
	nSib = 2
	model = mxModel(name,
		mxModel("top",
			# Components for "SA", "SC" and "SE"
			umxMatrix('PsA', 'Diag', nrow = nVar, ncol = nVar, free = TRUE , values = c(100, 5, 5, 5)),
			umxMatrix('PsC', 'Diag', nrow = nVar, ncol = nVar, free = TRUE , values = c(70, 10, 10, 10)),
			umxMatrix('PsE', 'Diag', nrow = nVar, ncol = nVar, free = FALSE, values = 0),
			# Te (residuals) all values equated by label, except E
			umxMatrix('TeA', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, labels = "TeA_r1c1", values = 2),
			umxMatrix('TeC', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, labels = "TeC_r1c1", values = 5),
			umxMatrix('TeE', 'Diag', nrow = nVar, ncol = nVar, free = TRUE, labels = c("TeE_r1c1", "TeE_r2c2", "TeE_r3c3", "TeE_r3c4"), values = 50),
			# Innovations (diag, but start 1-row down)
			xmu_simplex_corner(umxMatrix('BeA', 'Full', nrow = nVar, ncol = nVar), start = .9),
			xmu_simplex_corner(umxMatrix('BeC', 'Full', nrow = nVar, ncol = nVar), start = .8),
			umxMatrix('BeE', 'Full', nrow = nVar, ncol = nVar, free = FALSE, values = 0),
			# xmu_simplex_corner(umxMatrix('BeE', 'Full', nrow = nVar, ncol = nVar, free = FALSE, values = 0), start = 0),
			umxMatrix('I', 'Iden', nrow = nVar, ncol = nVar),
			mxAlgebra(name= 'iBeA', solve(I - BeA)),
			mxAlgebra(name= 'iBeC', solve(I - BeC)),
			mxAlgebra(name= 'iBeE', solve(I - BeE)),
			mxAlgebra(name= 'SigmaA', iBeA %*% PsA %*% t(iBeA) + TeA),
			mxAlgebra(name= 'SigmaC', iBeC %*% PsC %*% t(iBeC) + TeC),
			mxAlgebra(name= 'SigmaE', iBeE %*% PsE %*% t(iBeE) + TeE),
			# MZ covariance matrix and mean matrix "sumstatMZ"
			umxMatrix('means', 'Full', nrow = 1, ncol = nVar2, free = TRUE, labels = meanLabs, values = stmean),
			mxAlgebra(name = 'SigmaPh11'  , SigmaA + SigmaC + SigmaE),
			mxAlgebra(name = 'SigmaPh21mz', SigmaA + SigmaC),
			mxAlgebra(name = 'SigmaMZ'  , cbind(rbind(SigmaPh11, SigmaPh21mz), rbind(SigmaPh21mz, SigmaPh11))),
			# DZ covariance matrix and mean matrix "sumstatDZ"
			mxAlgebra(name= 'SigmaPh21dz', .5 %x% SigmaA + SigmaC),
			mxAlgebra(name= 'SigmaDZ', cbind(rbind(SigmaPh11, SigmaPh21dz), rbind(SigmaPh21dz, SigmaPh11)))
		),
		mxModel("MZ",
			mxData(mzData, type = "raw"), 
			mxExpectationNormal("top.SigmaMZ", means = "top.means", dimnames = selVars),  
			mxFitFunctionML()
		),
		mxModel("DZ",
			mxData(dzData, type = "raw"),
			mxExpectationNormal("top.SigmaDZ", means = "top.means", dimnames =selVars),
			mxFitFunctionML()
		),
		mxFitFunctionMultigroup(c("MZ", "DZ"))
	)
	# Run the model
	model = omxAssignFirstParameters(model) # Just trundle through and make sure values with the same label have the same start value... means for instance.
	model = as(model, "MxModel.Simplex")
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
#' View documentation on the ACE model subclass here: \code{\link{umxSummary.MxModel.ACE}}.
#' 
#' View documentation on the IP model subclass here: \code{\link{umxSummary.MxModel.IP}}.
#' 
#' View documentation on the CP model subclass here: \code{\link{umxSummary.MxModel.CP}}.
#' 
#' View documentation on the GxE model subclass here: \code{\link{umxSummary.MxModel.GxE}}.
#' 
#' @aliases umxSummary.MxModel.Simplex
#' @param model an \code{\link{mxModel}} to summarize
#' @param digits round to how many digits (default = 2)
#' @param file The name of the dot file to write: "name" = use the name of the model.
#' Defaults to NA = no plot.
#' @param comparison you can run mxCompare on a comparison model (NULL)
#' @param std Whether to standardize the output (default = TRUE)
#' @param showRg = whether to show the genetic correlations (FALSE)
#' @param CIs Whether to show Confidence intervals if they exist (T)
#' @param returnStd Whether to return the standardized form of the model (default = FALSE)
#' @param report If "html", then open an html table of the results
#' @param extended how much to report (FALSE)
#' @param zero.print How to show zeros (".")
#' @param ... Other parameters to control model summary
#' @return - optional \code{\link{mxModel}}
#' @export
#' @family Twin Modeling Functions
#' @family Reporting functions
#' @seealso - \code{\link{umxACE}} 
#' @references - \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
#' @examples
#' data(iqdat)
#' nTimePoints = 4 # Number of time points
#' baseVarNames = paste0("IQ_age", 1:nTimePoints)
#' selDVs = tvars(baseVarNames, sep = "_T", suffixes= 1:2)
#' # IQ_age1_T1, IQ_age2_T1, IQ_age3_T1, IQ_age4_T1, IQ_age1_T2, IQ_age2_T2, IQ_age3_T2, IQ_age4_T2,
#' 
#' # Select Data
#' mzData <- subset(iqdat, zygosity == "MZFF")
#' dzData <- subset(iqdat, zygosity == "DZFF")
#' m1 = umxSimplex(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' \dontrun{
#' umxSummary(m1)
#' umxSummary(m1, file = NA);
#' umxSummary(m1, file = "name", std = TRUE)
#' stdFit = umxSummary(m1, returnStd = TRUE)
#' }
umxSummarySimplex <- function(model, digits = 2, file = getOption("umx_auto_plot"), comparison = NULL, std = TRUE, showRg = FALSE, CIs = TRUE, report = c("markdown", "html"), returnStd = FALSE, extended = FALSE, zero.print = ".", ...) {
	# depends on R2HTML::HTML
	report = match.arg(report)
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umxSummarySimplex(thisFit, digits = digits, file = file, showRg = showRg, std = std, comparison = comparison, CIs = CIs, returnStd = returnStd, extended = extended, zero.print = zero.print, report = report)
		}
	} else {
		umx_has_been_run(model, stop = TRUE)
		if(is.null(comparison)){
			# \u00d7 = times sign
		 	message(paste0(model$name, " -2 \u00d7 log(Likelihood) = ", 
				round(-2 * logLik(model), digits = digits))
			)
	} else {
		message("Comparison of model with parent model:")
		umxCompare(comparison, model, digits = 3)
	}
	# Starting Values 
	# MZcov  = cov(iqdatMZ, use = "pairwise.complete.obs") # MZ covariance matrix
	# # Average T1 and T2, divide by 3 (A=C=E)
	# stACE = (MZcov[1:nVar,1:nVar]/3 + MZcov[(nVar+1):(2*nVar),(nVar+1):(2*nVar)]/3)/2
	# # stSA   = SMZ1, stSC = SMZ1, stSE = SMZ1 # starting values for SE
	# meanMZ = apply(iqdatMZ, 2, mean, na.rm = TRUE) # means
	# meanDZ = apply(iqdatDZ, 2, mean, na.rm = TRUE) # means
	# stmean = (meanMZ[1:nTimePoints] + meanDZ[1:nTimePoints])/2 # starting values for the means
	
	selDVs = dimnames(model$top.expCovMZ)[[1]]
	nVar   = length(selDVs)/2;
	# TODO umxSummarySimplex these already exist if a_std exists..
	# TODO Replace all this with umxSummarySimplex
	# Calculate standardized variance components

	# shit-sticks (tm) ACE
	PsA = diag(model$top$PsA$values)
	PsC = diag(model$top$PsC$values)
	PsE = diag(model$top$PsE$values)
	stickyACE = cbind(PsA, PsA, PsA) # Bind columns of a, c and e
	stickyACE = data.frame(stickyACE, row.names = paste("time", 1:nFac, sep = "."));
	names(stickyACE) = c ("A", "C", "E")
	message("## Common Factor paths")
	if(report == "html"){
		umx_print(stickyACE, digits = digits, zero.print = ".", file = "std_spec.html")
	} else {
		umx_print(stickyACE, digits = digits, zero.print = ".")
	}

	# 3. Rename BeA-> ai; BeC -> ci; BeE -> ei
	# 1. Rename PsA->  a; PsC ->  c; PsE ->  e
	# 2. Rename TeA-> as; TeC -> cs; TeE -> es


	TeA = diag(model$top$TeA$values) # Do these ever not all equal each other?
	TeC = diag(model$top$TeC$values)
	TeE = diag(model$top$TeE$values)

	BeA = diag(model$top$BeA$values[-1, ])
	BeC = diag(model$top$BeC$values[-1, ])
	BeE = diag(model$top$BeE$values[-1, ])

	Ps = data.frame(rbind(PsA, PsC, PsE), row.names = c("A", "C", "E"));
	Te = data.frame(rbind(TeA, TeC, TeE), row.names = c("A", "C", "E"));
	Be = data.frame(rbind(BeA, BeC, BeE), row.names = c("A", "C", "E"));

 	# Make a nice table.
	names(Ps) = paste0(rep(c("rA", "rC", "rE"), each = nVar), rep(1:nVar));
	umx_print(Ps, digits = digits, zero.print = zero.print)


	if(std){
		message("Standardized solution")
		Vtot  = A + C + E; # Total variance
		I     = diag(nVar); # nVar Identity matrix
		InvSD = sqrt(solve(I * Vtot))

		# Standardized _variance_ coefficients ready to be stacked together
		A_std = InvSD %&% A 	# Standardized variance coefficients
		C_std = InvSD %&% C
		E_std = InvSD %&% E
		
		AClean = A_std
		CClean = C_std
		EClean = E_std
	} else {
		message("Raw solution")
		AClean = A
		CClean = C
		EClean = E
	}

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
		rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error = function(err) return(NAmatrix)); # genetic correlations
		rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error = function(err) return(NAmatrix)); # C correlations
		rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error = function(err) return(NAmatrix)); # E correlations
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
umxSummary.MxModel.Simplex <- umxSummarySimplex