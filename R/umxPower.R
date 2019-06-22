#' Test the power of an ACE model to detect paths of interest.
#'
#' @description `power.ACE.test` simulates a univariate ACE model (with nMZpairs= 2000 and MZ_DZ_ratio*nMZpairs DZ twins. It 
#' computes power to detect dropping one or more paths specified in `drop=`. 
#' The interface and functionality of this service are experimental and subject to change.
#' @details
#' Statistical power is the proportion of studies that, over the long run, one should expect to yield a statistically
#' significant result given certain study characteristics such as sample size (N), the expected effect size (\eqn{\beta}),
#' and the criterion for statistical significance (\eqn{\alpha}).
#' 
#' A typical target for power is 80%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
#' of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth want to
#' discourage running of studies with low power: A study with 20 percent power will fail to detect real effects 80% of the time.
#' At the same time, the Type-I error rate is a nominal 5%, and with any researcher degrees of freedom, perhaps much more than that.
#' Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
#' This sounds silly, but empirical rates are often as low as 20% (cite Button).
#'
#' If you have few subjects and desired adequate power, you should raise the p-value. Be aware that consumers will be skeptical.
#'
#'
#' @param MZ_DZ_ratio MZ pairs per DZ pair (Default 1 = equal numbers.)
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE  Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param drop Path(s) to drop (Default "a_r1c1", i.e., drop a)
#' @param value Value to set drop path(s) to (Default 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param n  If provided, solve for power /sig.level at the given n (Default NULL)
#' @param sig.level alpha (p-value) Default = 0.05
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
#' # ==========================================
#' # = Power to detect A with equal MZ and DZ =
#' # ==========================================
#' power.ACE.test(drop = "a_r1c1", AA= .5, CC= 0) 
#' # Suggests n = 30
#'
#' # ================================
#' # = Show power across range of N =
#' # ================================
#' power.ACE.test(drop= "a_r1c1", AA= .5, CC= 0, search = TRUE)
#'
#' # Salutary note: You need well fitting models for power to be valid. TryHard
#' # helps with this, as does the default 2000-pair data simulation
#' power.ACE.test(drop = "a_r1c1", AA= .5, CC= 0, tryHard= "yes")
#' 
#'
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' # 80% power = 101.5 MZ and 101.5 DZ pairs
#' power.ACE.test(drop = "c_r1c1", AA= .5, CC= .3, tryHard="yes")
#'
#' # ========================================
#' # = Set a to a fixed, but non-zero value =
#' # ========================================
#' 
#' power.ACE.test(drop= "a_r1c1", value= .2, AA= .5, CC= 0)
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' # rather improbable hypothesis that twins show no familial similarity
#' power.ACE.test(drop = "^[ac]_r1c1", AA= .5, CC= .3)
#'
#'
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' # Compare to empirical mode: suggests 83.6 MZ and 83.6 DZ pairs
#'
#' power.ACE.test(drop= "a_r1c1", AA= .5, CC= 0, method= "empirical")
#' # method= "empirical": For 80% power, you need 76 MZ and 76 DZ pairs
#' power.ACE.test(drop= "a_r1c1", AA= .5, CC= 0, method = "ncp")
#' # method = "ncp": For 80% power, you need 83.5 MZ and 83.5 DZ pairs
#'
#' # ===============================================
#' # = Power with more DZs than MZs and vice versa =
#' # ===============================================
#'
#' # Power about the same: total pairs with 2 MZs per DZ = 692, vs. 707
#' power.ACE.test(MZ_DZ_ratio = 2/1, drop = "a_r1c1", AA= .3, CC= 0)
#' power.ACE.test(MZ_DZ_ratio = 1/2, drop = "a_r1c1", AA= .3, CC= 0)
#'
#'
#' # ====================
#' # = Show off options =
#' # ====================
#' # 1. tryHard
#' power.ACE.test(drop = "a_r1c1", AA= .5, CC= 0, tryHard= "yes")
#'
#' # 2. toggle optimizer
#' power.ACE.test(drop= "a_r1c1", AA= .5, CC= 0, optimizer= "SLSQP")
#'
#' # ===================================
#' # = Test dropping a series of paths =
#' # ===================================
#' # droplist = c("a_r1c1", "c_r1c1")
#' # for (dropWhat in dropList) {
#' # 	power.ACE.test(nMZpairs= 2000, nDZpairs= 1000, drop = dropWhat, AA= .5, CC= 0)
#' # }
#'
power.ACE.test <- function(MZ_DZ_ratio= 1, drop = c("a_r1c1"), value = 0, AA= .5, CC= 0, EE= NULL, n = NULL, sig.level = 0.05, power = .8, type = c("univariate", "bivariate", "GxE"), method = c("ncp", "empirical"), search = FALSE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL){
	message("This is beta code!")	
	if(!all.equal(type, c("univariate", "bivariate", "GxE"))){
		stop("type = ", omxQuotes(type), " not used yet")
	}
	# type   = match.arg(type)
	nMZpairs = 2000;
	nDZpairs = nMZpairs / MZ_DZ_ratio

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
	# = Build the "true" and "false" (null) models =
	# ==============================================
	ace = umxACE(selDVs = "var", sep= "_T", mzData = mzData, dzData= dzData, tryHard = tryHard, optimizer = optimizer)
	nullModel = umxModify(ace, regex = drop, value = value, name= as.character(mxMakeNames(paste0("drop_", drop[1]))), tryHard= tryHard)

	# return plot to old value
	umx_set_auto_plot(oldPlot)
	umx_set_silent(FALSE)
	
	if(search){
		# power is not an input to mxPowerSearch
		tmp = mxPowerSearch(trueModel=ace, falseModel= nullModel, n = n, sig.level = sig.level, method = method)
		plot(power ~ N, data = tmp)
		abline(h= power)
	} else {
		tmp = mxPower(trueModel=ace, falseModel= nullModel, n= n, sig.level = sig.level, power = power, method = method)
		nFound       = attributes(tmp)$detail$n
		totalFamiies = (nMZpairs + nDZpairs)
		MZDZprop     = nMZpairs/totalFamiies
		message(paste0("For ", power*100, "% power, you need ", round(nFound * MZDZprop, 1), " MZ and ", round(nFound * (1 - MZDZprop), 1), " DZ pairs"))
	}
	return(tmp)
}
