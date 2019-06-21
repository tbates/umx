#' Test the power of an ACE model to detect paths of interest.
#'
#' @description `power.ACE.test` simulates a univariate ACE model and returns the power to
#' detect dropping one or more paths. The interface and functionality of this service are experimental and subject to change.
#' @details This is under construction.
#' 
#' @param nMZpairs Number of MZ pairs (Default 500)
#' @param nDZpairs Number of DZ pairs (Default nMZpairs)
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE  Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param drop Path(s) to drop (Default "a_r1c1", i.e., drop a)
#' @param value Value to set drop path(s) to (Default 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param sig.level Default alpha = 0.05
#' @param power Default (1- TypeII) = .8 (80 percent power)
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
#' @examples
#'
#' # =====================
#' # = Power to detect A =
#' # =====================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "a_r1c1", AA= .5, CC= 0) 
#' # Suggests n = 30
#'
#' # Salutary note: You need well fitting models for power to be valid.
#' # TryHard suggests 64.7 MZ pairs and 129.3 DZ pairs
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "a_r1c1", AA= .5, CC= 0,
#' 	tryHard= "yes")
#' 
#' # Compare to empirical mode
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "a_r1c1", AA= .5, CC= 0, 
#'		method= "empirical", tryHard= "yes")
#' # For 80% power, you need 58.2 MZ pairs and 116.4 DZ pairs
#'
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "a_r1c1", 
#' 	AA= .5, CC= .3)
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "^[ac]_r1c1", 
#'		AA= .5, CC= .3, tryHard= "yes")
#'
#' # ===================
#' # = Show range of N =
#' # ===================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop= "a_r1c1", 
#'		AA= .5, CC= 0, search = TRUE)
#'
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop= "a_r1c1", 
#'		AA= .5, CC= 0, method = "empirical")
#'
#' # ================================
#' # = Power with more DZs than MZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 2000, drop = "a_r1c1", AA= .5, CC= 0)
#'
#' # ================================
#' # = Power with more MZs than DZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs= 1000, drop= "a_r1c1", AA= .5, CC= 0)
#'
#' # ===========================
#' # = Pick a value (not zero) =
#' # ===========================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs= 1000, drop= "a_r1c1", value= .2,
#'		AA= .5, CC= 0)
#'
#' # ====================
#' # = Show off options =
#' # ====================
#' # 1. tryHard
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = "a_r1c1", 
#' 	AA= .5, CC= 0, tryHard= "yes")
#'
#' # 2. toggle optimizer
#' power.ACE.test(nMZpairs= 500, nDZpairs= 1000, drop= "a_r1c1", 
#' 	AA= .5, CC= 0, optimizer= "SLSQP")
#'
#' # ===================================
#' # = Test dropping a series of paths =
#' # ===================================
#' # droplist = c("a_r1c1", "c_r1c1")
#' # for (dropWhat in dropList) {
#' # 	power.ACE.test(nMZpairs= 2000, nDZpairs= 1000, drop = dropWhat, AA= .5, CC= 0)
#' # }
#'
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
