#' Test the power of an ACE model to detect A, C, or E
#'
#' @description
#' `power.ACE.test` simulates a univariate ACE model and returns the power to detect dropping one or more label.
#' The interface and functionality of this service are experimental and subject to change.
#' @details
#' This is under construction.
#' @param nMZpairs Number of MZ pairs (default 500)
#' @param nDZpairs Number of DZ pairs (Defaul = nMZpairs)
#' @param AA Additive genetic variance (Default = .5)
#' @param CC Shared environment variance (Default = 0)
#' @param EE  Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param drop Path(s) to drop (default "a_r1c1" drops a)
#' @param value Value to set the dropped path(s) (Default = 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param sig.level Default alpha = 0.05
#' @param power Default (1- TypeII) = .8 (80% power)
#' @param type Type of model c("univariate", "bivariate", "GxE") (EXPERIMENTAL MAY GO AWAY OR CHANGE)
#' @param method How to estimate power: Deaful =  using non-centrality parameter ("ncp"). Alternative is "empirical"
#' @param tryHard Whether to tryHard to find a solution (default = "no", alternatives are "yes"...)
#' @param optimizer If set, will switch the optimizer.
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
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), AA= .5, CC= 0)
#'
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), AA= .5, CC= .3)
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("^[ac]_r1c1"), 
#'		AA= .5, CC= .3, tryHard= "yes")
#'
#' # ===================
#' # = Show range of N =
#' # ===================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), 
#'		AA= .5, CC= 0, search = TRUE)
#'
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' power.ACE.test(nMZpairs= 500, nDZpairs = 1000, drop = c("a_r1c1"), 
#'		AA= .5, CC= 0, method = "empirical")
#'
#' # ================================
#' # = Power with more DZs than MZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 500, nDZpairs = 2000, drop = c("a_r1c1"), AA= .5, CC= 0)
#'
#' # ================================
#' # = Power with more MZs than DZs =
#' # ================================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs = 1000, drop = c("a_r1c1"), AA= .5, CC= 0)
#'
#' # ===========================
#' # = Pick a value (not zero) =
#' # ===========================
#' 
#' power.ACE.test(nMZpairs= 2000, nDZpairs = 1000, drop = c("a_r1c1"), value = .2,
#'		AA= .5, CC= 0)
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
	type   = match.arg(type)
	method = match.arg(method)
	# strict = FALSE
	# tol = .Machine$double.eps^0.25
	oldPlot = umx_set_auto_plot(silent=TRUE); umx_set_auto_plot(FALSE)

	# 1. make data and run model 1
	# tmp = umx_make_TwinData(nMZpairs= 500, nDZpairs = 500, AA= .5, CC= 0, EE= NULL, varNames= "var", mean= 0, empirical= TRUE)
	tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC= CC, EE= EE, varNames= "var", mean= 0, empirical= TRUE)
	mzData = subset(tmp, zygosity == "MZ")
	dzData = subset(tmp, zygosity == "DZ")
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
