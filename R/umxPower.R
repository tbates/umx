#' Test the power of an ACE model to detect paths of interest.
#'
#' @description Simulate a univariate ACE model and return the power to detect dropping or changing one or more paths.
#' @param nMZpairs Number of MZ pairs (Default 500)
#' @param nDZpairs Number of DZ pairs (Default nMZpairs)
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE  Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param drop Path(s) to drop (default "a_r1c1" drop a)
#' @param value Value to set the dropped path(s) (Default 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param sig.level Default alpha = 0.05
#' @param power Default (1- TypeII) = .8 (80% power)
#' @param type Type of model c("univariate", "bivariate", "GxE") (EXPERIMENTAL MAY GO AWAY OR CHANGE)
#' @param method How to estimate power: Default =  use non-centrality parameter ("ncp"). Alternative is "empirical"
#' @param tryHard Whether to tryHard to find a solution (default = "no", alternatives are "yes"...)
#' @param optimizer If set, will switch the optimizer.
#' @return mxPower object
#' @export
#' @family Twin Modeling Functions
#' @seealso - [OpenMx::mxPower()]
#' @references - Visscher, P.M., Gordon, S., Neale, M.C. (2008). Power of the classical twin design
#' revisited: II detection of common environmental variance. *Twin Res Hum Genet*, **11**: 48-54.
#' @md
power.ACE.test <- function(nMZpairs= 500, nDZpairs = nMZpairs, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0, EE= NULL, sig.level = 0.05, power = .8, type = c("univariate", "bivariate", "GxE"), method = c("ncp", "empirical"), search = FALSE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL){
	message("This is beta code!")
	
	if(!all.equal(type, c("univariate", "bivariate", "GxE"))){
		stop("type = ", omxQuotes(type), " not used yet")
	}
	# type   = match.arg(type)
	method = match.arg(method)
	tryHard = match.arg(tryHard)

	# Turn off plotting
	umx_set_silent(TRUE)
	oldPlot = umx_set_auto_plot(silent=TRUE); umx_set_auto_plot(FALSE)

	# 1. Generate data and run model 1
	# tmp = umx_make_TwinData(nMZpairs= 500, nDZpairs = 500, AA= .5, CC= 0, EE= NULL, varNames= "var", mean= 0, empirical= TRUE)
	tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC= CC, EE= EE, varNames= "var", mean= 0, empirical= TRUE)
	mzData = subset(tmp, zygosity == "MZ")
	dzData = subset(tmp, zygosity == "DZ")
	
	# ==============================================
	# = build the "true" and "false" (null) models =
	# ==============================================
	ace = umxACE(selDVs = "var", sep= "_T", mzData = mzData, dzData= dzData, tryHard = tryHard, optimizer = optimizer)
	nullModel = umxModify(ace, update = drop, value = value, name= paste0("drop_", drop[1]), tryHard= tryHard)

	# return plot to old value
	umx_set_auto_plot(oldPlot)
	umx_set_silent(FALSE)
	
	if(search){
		tmp = mxPowerSearch(ace, nullModel, sig.level = sig.level, power = power, method = method)
		plot(power ~ N, data = tmp)
		abline(h= power)
	} else {
		tmp          = mxPower(ace, nullModel, sig.level = sig.level, power = power, method = method)
		nFound       = attributes(tmp)$detail$n
		totalFamiies = (nMZpairs + nDZpairs)
		MZDZprop     = nMZpairs/totalFamiies
		message(paste0("For ", power*100, "% power, you need ", round(nFound * MZDZprop, 1), " MZ pairs and ", round(nFound * (1 - MZDZprop), 1), " DZ pairs"))
	}
	return(tmp)
}
