#' Test the power of an ACE model to detect A, C, or E
#'
#' @description
#' `power.ACE.test` simulates a univariate ACE model and returns the power to detect dropping one or more label.
#' The interface and functionality of this service are experimental and subject to change.
#' @details
#' This is under construction.
#' @param nMZpairs= 500
#' @param nDZpairs = nMZpairs
#' @param AA = .5
#' @param CC= 0
#' @param EE= NULL
#' @param drop = c("a_r1c1")
#' @param value = 0
#' @param search = FALSE
#' @param sig.level = 0.05
#' @param power = .8
#' @param type = c("univariate", "bivariate", "GxE")
#' @param method = c("ncp", "empirical")
#' @param tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch")
#' @param optimizer = NULL
#' @return - nothing
#' @export
#' @family Twin Modeling Functions
#' @seealso - [OpenMx::mxPower()]
#' @references - [https://github.com/tbates/umx], [https://tbates.github.io]
#' @md
#' @examples
#'
#' # =====================
#' # = Power to detect A =
#' # =====================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0)
#'
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), value = 0, AA= .5, CC= .3)
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' # power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("^[ac]_r1c1"), value = 0, AA= .5, CC= .3, tryHard= "yes")
#'
#' # ===================
#' # = Show range of N =
#' # ===================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), AA= .5, CC= 0, search = TRUE)
#'
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), AA= .5, CC= 0, method = "empirical")
#'
#' # ================================
#' # = Power with more DZs than MZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 2000, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0)
#'
#' # ================================
#' # = Power with more MZs than DZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs = 1000, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0)
#'
#' # ===========================
#' # = Pick a value (not zero) =
#' # ===========================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs = 1000, drop = c("a_r1c1"), value = .2, AA= .5, CC= 0)
#'
#' # ===================================
#' # = Test dropping a series of paths =
#' # ===================================
#' # or just get people to call it repeatedly?
#' for (dropWhat in drop) {
#'
#' }
#'
power.ACE.test <- function(nMZpairs= 500, nDZpairs = nMZpairs, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0, EE= NULL, sig.level = 0.05, power = .8, type = c("univariate", "bivariate", "GxE"), method = c("ncp", "empirical"), search = FALSE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL){
	type   = match.arg(type)
	method = match.arg(method)
	# strict = FALSE
	# tol = .Machine$double.eps^0.25
	oldPlot = umx_set_auto_plot(silent=TRUE); umx_set_auto_plot(FALSE)

	# 1. make data and run model 1
	tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC= CC, EE= NULL, varNames= "var", mean= 0, empirical= TRUE)
	mzData = subset(twinData, zygosity == "MZ")
	dzData = subset(twinData, zygosity == "DZ")
	ace = umxACE(selDVs = "var", sep= "_T", mzData = mzData, dzData= dzData)
	nullModel = umxModify(ace, update = drop, value = value, name= paste0("drop_", drop[1]))
	
	if(search){
		tmp = mxPowerSearch(ace, nullModel, method = method)
		plot(power ~ N, data = tmp)
		abline(h= power)
	} else {
		tmp = mxPower(ace, nullModel, method = method)
	}
	
	# umxSummary(ae, ee)

	# pars = round(cbind(aceFit$output$estimate, aceFit$output$standardErrors), 3)
	# colnames(pars) = c("Estimates", "Std. Err.")
	umx_set_auto_plot(oldPlot)
}
