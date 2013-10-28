# http://adv-r.had.co.nz/Philosophy.html
# https://github.com/hadley/devtools
# setwd("~/bin/umx"); devtools::document(); devtools::install(); 
# setwd("~/bin/umx"); devtools::check()
# devtools::load_all()
# devtools::dev_help("umxX")
# show_news()

# ==============
# = Deprecated =
# ==============

# umxTryHard
#
# umxTryHard Is deprecated: use \code{\link{umxRun}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/

umxTryHard <- function(model, n=3, calc_SE=F){ stop("Use umxRun() in place of umxTryHard") }

# umxLabels
#
# umxLabels Is deprecated: use \code{\link{umxLabel}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/

umxLabels <- function(from=NA, to=NA, connect="single", prefix="", suffix="") {stop("please use umxPath in place of umxLabels. To label models or matrices, use umxLabel")}

# genEpi_Jiggle
# genEpi_Jiggle is deprecated: use \code{\link{umxJiggle}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/

genEpi_Jiggle <- function(matrixIn, mean = 0, sd = .1, dontTouch = 0) {stop("please use umxJiggle in place of genEpi_Jiggle")}

# umxPath
# umxPath is deprecated: Use \code{\link{mxPath}} and \code{\link{umxLabel}} instead
#
# @export
# @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
# @references - http://openmx.psyc.virginia.edu/

umxPath <- function(from = NA, to = NA, connect = "single", arrows = 1, free = TRUE, values = NA, labels = NA, lbound = NA, ubound = NA, prefix = "", suffix = "",...) {
	stop("replace umxPath with mxPath, and run umxLabel(model) on the model when you are done to add default labels, plus umxStart(model) to add default start values")
	# {$|single,all.pairs,all.bivariate,unique.pairs,unique.bivariate|}
	# Purpose: make mxPaths with informative labels, comments to tim.bates@ed.ac.uk
	# use case
	# umxPath(from = "OriginSES", to = "PaternalSESn")
	# Use case: umxPath("F1", paste("m",1:4,sep="")) # "F1_to_m1" "F1_to_m2" "F1_to_m3" "F1_to_m4"
	# TODO: make this generate the paths as well... i.e., "umxPath()"
	# TODO: handle connection style
	# nb: bivariate length = n-1 recursive 1=0, 2=1, 3=3, 4=7 i.e., 
	if(any(is.na(to)) & (suffix=="var"| suffix=="unique")){
		# handle from only, variance and residuals
		part1  = paste(prefix, from, sep="")
		myLabels = (paste(part1, suffix, sep="_"))
	} else if(any(from == "one")){
		# handle means (from == "one")
		if(!all(from == "one")){
			stop(cat("Error in umxLabels: from was a mix of one and not-one",from))
		} else {
			myLabels = paste(to, "mean", sep="_")
		}
	} else if(connect == "unique.bivariate") {
		if(!all(is.na(to))){
			if(!(from == to)){
				stop("with connect = 'unique.bivariate', to must be blank, or the same as from")
			}
		}
		labels = as.character(combn(from, m = 2, FUN = paste, collapse = "_"))
		return(labels)
	} else {
		fromPart = paste(prefix, from, sep = "")
		toPart   = paste(to  , suffix, sep = "_")
		myLabels = paste(fromPart, toPart, sep = "_to_")
	}
	mxPath(from = from, to = to, connect = connect, arrows = arrows, free = free, values = values, labels = myLabels, lbound = lbound, ubound = ubound)
}

#' umxReportFit
#'
#' Deprecated: Please use \code{\link{umxSummary}} instead. Both functions report fit in a compact form suitable for a journal. 
#'
#' @param model The \code{\link{mxModel}} whose fit will be reported
#' @param saturatedModels Saturated models if needed for fit indices (see note below: 
#' Only needed for raw data, and then not if you've run umxRun
#' @param report The format for the output (currently only a 1-liner is supported)
#' @param showEstimates Whether to show the raw or standadized estimates.
#' Options are "none|raw|std|both" (The default is standardized parameters. Choose none just to get the fit statistics)
#' @seealso - \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}
#' @references - \url{http://openmx.psyc.virginia.edu}
#'  - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in covariance structure analysis: Coventional criteria versus new alternatives. Structural Equation Modeling, 6, 1-55. 
#'  - Yu, C.Y. (2002). Evaluating cutoff criteria of model fit indices for latent variable models with binary and continuous outcomes. University of California, Los Angeles, Los Angeles. Retrieved from \url{http://www.statmodel.com/download/Yudissertation.pdf}
#' @export
#' @import OpenMx
#' @examples
#' \dontrun{
#' umxSummary(m1)
#' }

umxReportFit <- function(model, saturatedModels = NULL, report = "line", showEstimates = "std", precision = 2, displayColumns = c("row", "col", "Std.Estimate")){
	warning("umxReportFit is deprecated now: use umxSummary() in its place")
	# report = "line|table"
	# showEstimates = "std|none"
	# TODO make table take lists of models...
	# TODO could have a toggle for computing hte saturated models...

	output <- model@output
	# stop if there is no objective function
	if ( is.null(output) ) stop("Provided model has no objective function, and thus no output. mxRun(model) first")
	# stop if there is no output
	if ( length(output) < 1 ) stop("Provided model has no output. I can only standardize models that have been mxRun() first!")
	
	if(is.null(saturatedModels)){
		# saturatedModels not passed in from outside, so get them from the model
		modelSummary = OpenMx::summary(model)
		
		if(is.na(modelSummary$SaturatedLikelihood)){
			message("There is no saturated likelihood: computing that now...")
			saturatedModels = umxSaturated(model)
			modelSummary = summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
		}
	} else {
		modelSummary = summary(model, SaturatedLikelihood = saturatedModels$Sat, IndependenceLikelihood = saturatedModels$Ind)
	}

	if(showEstimates != "none"){
		if("Std.Estimate" %in%  names(modelSummary$parameters)){
			if(showEstimates == "both") {
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error", "Std.Estimate", "Std.SE")
			} else if(showEstimates == "std"){
				namesToShow = c("name", "matrix", "row", "col", "Std.Estimate", "Std.SE")
			}else{
				namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")					
			}
		} else {
			namesToShow = c("name", "matrix", "row", "col", "Estimate", "Std.Error")
		}
		print(modelSummary$parameters[,namesToShow], digits= 3, na.print = "", zero.print = "0", justify = "none")
	}
	
	with(modelSummary, {
		if(!is.finite(TLI)){			
			TLI_OK = "OK"
		} else {
			if(TLI > .95) {
				TLI_OK = "OK"
				} else {
					TLI_OK = "bad"
				}
			}
			if(!is.finite(RMSEA)) {
				RMSEA_OK = "OK"
			} else {
			if(RMSEA < .06){
				RMSEA_OK = "OK"
				} else {
					RMSEA_OK = "bad"
				}
			}
			if(report == "table"){
				x = data.frame(cbind(model@name, round(Chi,2), formatC(p, format="g"), round(CFI,3), round(TLI,3), round(RMSEA, 3)))
				names(x) = c("model","\u03A7","p","CFI", "TLI","RMSEA") # \u03A7 is unicode for chi
				print(x)
			} else {
				x = paste0(
					"\u03A72\u00B2(", degreesOfFreedom, ") = ", round(Chi,2),
					", p = "    , formatC(p, format="g"),
					"; CFI = "  , round(CFI,3),
					"; TLI = "  , round(TLI,3),
					"; RMSEA = ", round(RMSEA, 3), 
					", TLI = "  , TLI_OK,
					", RMSEA = ", RMSEA_OK)
					print(x)
			}
	})
}

umxGetLabels <- function(inputTarget, regex = NA, free = NA, verbose = F) {
	message("Please use umxGetParameters instead of umxGetLabels")
	if(class(inputTarget)[1] %in% c("MxRAMModel","MxModel")) {
		topLabels = names(omxGetParameters(inputTarget, indep=FALSE, free=free))
	} else if(is(inputTarget, "MxMatrix")) {
		if(is.na(free)) {
			topLabels = inputTarget@labels
		} else {
			topLabels = inputTarget@labels[inputTarget@free==free]
		}
		}else{
			stop("I am sorry Dave, umxGetLabels needs either a model or a matrix: you offered a ", class(inputTarget)[1])
		}
	theLabels = topLabels[which(!is.na(topLabels))] # exclude NAs
	if( !is.na(regex) ) {
		if(length(grep("[\\.\\*\\[\\(\\+\\|]+", regex) )<1){ # no grep found: add some anchors for safety
			regex = paste("^", regex, "[0-9]*$", sep=""); # anchor to the start of the string
			if(verbose==T){
				cat("note: anchored regex to beginning of string and allowed only numeric follow\n");
			}
		}
		
		theLabels = grep(regex, theLabels, perl = F, value=T) # return more detail
		if(length(theLabels)==0){
			stop("found no matching labels!");
		}
	}
	return(theLabels)
}

col.as.numeric <- function(df) {
	stop("col.as.numeric is deprecated. Please replace with umx.as.numeric()")
	# use case
	# col.as.numeric(df)
	for (i in names(df)) {
		df[,i] = as.numeric(df[,i])
	}
	return(df)
}

cor.prob <- function (X, df = nrow(X) - 2, use = "pairwise.complete.obs", digits = 3) {message("Use umx_cor")}


umx_u_APA_pval <- function(p, min = .001, rounding = 3, addComparison = T) {stop("umx_u_APA_pval is deprecated: Use umx_APA_pval")}



# ==========================
# = Deprecated from genepi =
# ==========================

# ==============
# = Deprecated =
# ==============

makeACE <-function(modelName="ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzAr=.5, dzCr=1, addStd=F) {
	stop("don't call makeACE(), it's so... 2009... Use makeACE_2Group() instead")
}

twoGroupCommonPath <- function(modelName="CP", selDVs, dzData, mzData, nSib=2) {
	stop("Change \"twoGroupCommonPath()\" to: makeCP_2Group()")
}

twoGroupIndependentPath <- function(modelName="IP", selDVs, dzData, mzData, nSib=2, freeLowerE=F) {
	stop("Change \"twoGroupIndependentPath\" to: makeIP_2Group")
}

two_group_G_by_E <- function(modelName = "GbyE", selDVs, selDefs, dzData, mzData, nSib=2) {
	stop("Change \"two_group_G_by_E\" to: makeGxE_2Group")
}

twoGroupCholesky <-function(modelName = "ACE", selDVs, dzData, mzData, nSib=2, equateMeans=T, dzr=.5, addStd=F) {
	stop("Change \"twoGroupCholesky\" to: makeACE_2Group")
}

twoGroupOrdinal <- function(modelName ="Ordinal", selDVs, factorLevels, dzData, mzData, nSib, lowerBoundZ= -5, aceStart=c(.6,.3,.7)) {
	stop("Change \"twoGroupOrdinal\" to: makeOrdinal_2Group")
}

tryHard <- function(model, n = 3, calc_SE = F){stop("Use umxRun() in place of umxTryHard() and tryHard()")}

genEpi_ReRun <- function(lastFit, dropList=NA, regex=NA, free=F, value=0, freeToStart=NA, newName=NA, verbose=F, intervals=F) {message("please call umxReRun()")}

mxStart        <- function(x=1, sd=NA, n=1){ traceback(); stop("Deprecated: Please use umxStart() instead of mxStart")}
umxLabeler     <- function(mx_matrix = NA, baseName = NA, setfree = F, drop = 0, jiggle = NA, boundDiag = NA) {stop("replace 'umxLabeler' with 'umxLabel'")}

standardizeRAM <- function(model, return="parameters", Amatrix=NA, Smatrix=NA, Mmatrix=NA) {stop("Deprecated: Please use umxStandardizeModel() instead of standardizeRAM")}
genEpi_equate  <- function(myModel, master, slave, free = T, verbose = T, name = NA) {stop("replace genEpi_equate() with umxEquate()") }
genEpi_Path    <- function(from, to,arrows=1, connect="single", free = T, values = NA, labels = NA, lbound = NA, ubound = NA){ message("genEpi_Path is deprecated: replace with umxPath")}
genEpiCompare  <- function(base = NA, comparison = NA, all = T, excel = T) { stop("deprecated, use umxCompare")}
mxLatent       <- function(latent=NA, formedBy=NA, forms=NA, data, endogenous=FALSE, model.name=NA, help=FALSE, labelSuffix="") {message("please call umxLatent()")}

hcor                     <- function(data, ML = F, use="pairwise.complete.obs"){message("Please call umxHetCor() instead of hcor")}
lower2full               <- function(lower.data, diag = F, byrow = T) {stop("please replace lower2full with umxLower2Full()")}
mxIsOrdinalVar           <- function(df, names = F) {message("replace mxIsOrdinalVar with umxIsOrdinalVar")}
genEpi_GetLabels         <- function(inputTarget, regex = NA, free = NA, verbose = F) { stop("please replace genEpi_GetLabels with umxGetLabels()")}
mxMakeThresholdsMatrices <- function(df, deviationBased = T, droplevels = T, verbose = F) {	message("please change to 'umxMakeThresholdMatrix()'")}
mxAutoThreshRAMObjective <- function(df,  deviationBased = T, droplevels = T, verbose = F) { message("please call umxThresholdRAMObjective()")}

graphViz_Cholesky <- function(model = NA, dotFilename = "name", accuracy = 2, showMeans=F) {
	stop("Deprecated: PLease replace graphViz_Cholesky with umxPlotCholesky")
}


summaryACEFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, showStd = showStd, comparison = comparison, CIs = CIs) {
	stop("Deprecated: Please replace summaryACEFit with umxSummaryACE")
}

summaryCommonFit <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace summaryCommonFit with umxSummaryCP")
}

umxPlotCholesky <- function(thisFit, accuracy = accuracy, dotFilename = dotFilename, returnStd = returnStd, extended = extended, showRg = showRg, parentModel = parentModel, CIs=CIs) {
	stop("Deprecated: Please replace umxPlotCholesky with umxPlotACE")
}


stringToMxAlgebra <- function(algString, name = NA, dimnames = NA) {
	stop("Deprecated: Please replace stringToMxAlgebra with umx_string_to_Algebra")
}

genEpi_EvalQuote <- function(expstring, model, compute, show){
	stop("Deprecated: Please replace genEpi_EvalQuote with umxEval")
}