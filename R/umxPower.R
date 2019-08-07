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
#' This sounds silly, but empirical rates are often as low as 20% (# TODO cite Button).
#'
#' If you have few subjects and desired adequate power, you should raise the p-value. Be aware that consumers will be skeptical.
#'
#'
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE  Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param drop Path(s) to drop (Default "a_r1c1", i.e., drop a)
#' @param value Value to set drop path(s) to (Default 0)
#' @param n  If provided, solve for power /sig.level at the given n (Default NULL)
#' @param MZ_DZ_ratio MZ pairs per DZ pair (Default 1 = equal numbers.)
#' @param sig.level alpha (p-value) Default = 0.05
#' @param power Default = .8 (80 percent power, equal to 1 - Type II rate)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param type Type of model c("univariate", "bivariate", "GxE") (EXPERIMENTAL MAY GO AWAY OR CHANGE)
#' @param method How to estimate power: Default =  use non-centrality parameter ("ncp"). Alternative is "empirical"
#' @param tryHard Whether to tryHard to find a solution (default = "no", alternatives are "yes"...)
#' @param optimizer If set, will switch the optimizer.
#' @param nSim Total number of pairs to simulate in the models (default = 4000)
#' @return mxPower object
#' @export
#' @family Twin Modeling Functions
#' @seealso - [OpenMx::mxPower()]
#' @references - Visscher, P.M., Gordon, S., Neale, M.C. (2008). Power of the classical twin design
#' revisited: II detection of common environmental variance. *Twin Res Hum Genet*, **11**: 48-54.
#' [doi](https://doi.org/10.1375/twin.11.1.48)
#' @md
#' @examples
#'
#' # TODO why not equivalent to this?
#' # https://genepi.qimr.edu.au//general/TwinPowerCalculator/twinpower.cgi
#'
#' # ===============================================
#' # = Power to detect a^2=.5 with equal MZ and DZ =
#' # ===============================================
#' power.ACE.test(AA= .5, CC= 0, update = "a_r1c1") 
#' # Suggests n = 84 MZ and 94 DZ pairs.
#'
#' # ================================
#' # = Show power across range of N =
#' # ================================
#' power.ACE.test(AA= .5, CC= 0, update = "a_r1c1", search = TRUE)
#'
#' # Salutary note: You need well fitting models with correct betas in the data
#' # for power to be valid.
#' # tryHard helps ensure this, as does the default nSim= 4000 pair data.
#' # Power is important to get right, so I recommend using tryHard = "yes"
#' power.ACE.test(AA= .5, CC= 0, update = "a_r1c1", tryHard= "yes")
#' 
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' # 102 of each of MZ and DZ pairs for 80% power.
#' power.ACE.test(AA= .5, CC= .3, update = "c_r1c1", tryHard= "yes")
#'
#' # ========================================
#' # = Set 'a' to a fixed, but non-zero value =
#' # ========================================
#' 
#' power.ACE.test(update= "a_r1c1", value= sqrt(.2), AA= .5, CC= 0)
#' # TODO get power.ACE.test to print the value of A in the null model.
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' # rather improbable hypothesis that twins show no familial similarity
#' power.ACE.test(update = "^[ac]_r1c1", AA= .5, CC= .3)
#'
#' # ===================================================
#' # = More power to detect A > 0 when more C present  =
#' # ===================================================
#' 
#' power.ACE.test(update = "a_r1c1", AA= .5, CC= .0)
#' power.ACE.test(update = "a_r1c1", AA= .5, CC= .3)
#'
#' # ====================================================
#' # = More power to detect C > 0 when more A present?  =
#' # ====================================================
#' 
#' power.ACE.test(update = "c_r1c1", AA= .0, CC= .5)
#' power.ACE.test(update = "c_r1c1", AA= .3, CC= .5)
#'
#'
#' # ===============================================
#' # = Power with more DZs than MZs and vice versa =
#' # ===============================================
#'
#' # Power about the same: total pairs with 2 MZs per DZ = 692, vs. 707
#' power.ACE.test(MZ_DZ_ratio= 2/1, update= "a_r1c1", AA= .3, CC= 0, method="ncp", tryHard="yes")
#' power.ACE.test(MZ_DZ_ratio= 1/2, update= "a_r1c1", AA= .3, CC= 0, method="ncp", tryHard="yes")
#'
#' \dontrun{
#' 
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' # Compare to empirical mode: suggests 83.6 MZ and 83.6 DZ pairs
#'
#' power.ACE.test(update= "a_r1c1", AA= .5, CC= 0, method= "empirical")
#' # method= "empirical": For 80% power, you need 76 MZ and 76 DZ pairs
#' power.ACE.test(update= "a_r1c1", AA= .5, CC= 0, method = "ncp")
#' # method = "ncp": For 80% power, you need 83.5 MZ and 83.5 DZ pairs
#'
#' # ====================
#' # = Show off options =
#' # ====================
#' # 1. tryHardsource("../../OpenMx/inst/models/nightly/Power2.R", chdir = TRUE)

#' power.ACE.test(update = "a_r1c1", AA= .5, CC= 0, tryHard= "yes")
#'
#' # 2. toggle optimizer
#' power.ACE.test(update= "a_r1c1", AA= .5, CC= 0, optimizer= "SLSQP")
#'
#' # 3. How many twin pairs in the base simulated data?
#' power.ACE.test(update = "a_r1c1", AA= .5, CC= 0)
#' power.ACE.test(update = "a_r1c1", AA= .5, CC= 0, nSim= 20)
#'
#' }
#'
#' # ===================================
#' # = Test dropping a series of paths =
#' # ===================================
#' # droplist = c("a_r1c1", "c_r1c1")
#' # for (dropWhat in dropList) {
#' # 	power.ACE.test(nMZpairs= 2000, nDZpairs= 1000, drop = dropWhat, AA= .5, CC= 0)
#' # }
#'
power.ACE.test <- function(AA= .5, CC= 0, EE= NULL, update = c("a_r1c1"), value = 0,  n = NULL, MZ_DZ_ratio= 1, sig.level = 0.05, power = .8, type = c("univariate", "bivariate", "GxE"), method = c("ncp", "empirical"), search = FALSE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL, nSim=4000){
	# decimalplaces <- function(x) {
	#     if (abs(x - round(x)) > .Machine$double.eps^0.5) {
	#         nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
	#     } else {
	#         return(0)
	#     }
	# }
	message("This is beta code!")	
	method = match.arg(method)
	tryHard = match.arg(tryHard)
	if(!all.equal(type, c("univariate", "bivariate", "GxE"))){
		stop("type = ", omxQuotes(type), " not used yet. Likely never will be as these become separate functions")
	}else{
		type = match.arg(type)
	}

	# nSim = 4000
	# MZ_DZ_ratio is an odds & pMZ = odds/(1+odds)
	pMZ = MZ_DZ_ratio/(1 + MZ_DZ_ratio)
	nMZpairs = round(nSim * pMZ)
	nDZpairs = round(nSim * (1 - pMZ))
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
	nullModel = umxModify(ace, regex = update, value = value, name= as.character(mxMakeNames(paste0("drop_", update[1]))), tryHard= tryHard)

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
		nFound = attributes(tmp)$detail$n
		message(paste0("For ", power*100, "% power, you need ", 
			round(nFound * pMZ), " MZ and ",
		 	round(nFound * (1 - pMZ)), " DZ pairs"))
	}
	return(tmp)
}


#' Test power to detect path in a model.
#'
#' @description
#' `umxPower` takes an input model (the model of the true data), and tests power (or determines n)
#' to detect dropping (or changing the value) a path in this true model.
#'
#' @param trueModel The model with the parameters at values you expect in the population.
#' @param update The parameter(s) to drop
#' @param n How many subjects? (Default = NULL)
#' @param power Default = NULL (conventional level = .8)
#' @param sig.level Default = .05
#' @param value Value of dropped parameter (default = 0)
#' @param method "ncp" (default) or "empirical"
#' @param tabulatePower Whether to show a spread of n and corresponding power (Default = FALSE)
#' @return power table
#' @export
#' @family Teaching and Testing functions
#' @seealso - [umxRAM()]
#' @references - [tutorials](https://tbates.github.io)
#' @md
#' @examples
#' # ===================================================
#' # = Power to detect correlation of .3 in 200 people =
#' # ===================================================
#'
#' tmp = umx_make_raw_from_cov(qm(1, .3| .3, 1), n=200, varNames= c("X", "Y"), empirical= TRUE)
#' 
#' # 1. Make model with true correlation of X & Y = .3
#' m1 = umxRAM("corXY", data = tmp,
#'    umxPath("X", with = "Y"),
#'    umxPath(var = c("X", "Y"))
#' )
#' # 2. Test power to detect .3 versus 0, with n= 90 subjects
#' umxPower(m1, "X_with_Y", n= 90, method="ncp")
#' 
#' # ####################
#' # # Estimating power #
#' # ####################
#' # 
#' #    method = ncp
#' #         n = 90
#' #     power = 0.829
#' # sig.level = 0.05
#' # statistic = LRT
#'
#' \dontrun{
#' # Use method = empirical 
#' umxPower(m1, "X_with_Y", n = 90)
#' }
#' 
#' \dontrun{
#' # Test power for a cor.test doing the same thing..
#' pwr::pwr.r.test(r = .3, n = 90)
#' #           n = 90
#' #           r = 0.3
#' #   sig.level = 0.05
#' #       power = 0.827
#' # alternative = two.sided
#' }
#'
umxPower <- function(trueModel, update= NULL, n= NULL, power = NULL, sig.level= .05, value = 0, method= c("ncp", "empirical"), tabulatePower= FALSE){
	method   = match.arg(method)
	n_null   = is.null(n)
	pwr_null = is.null(power)
	sig_null = is.null(sig.level)
	setList = omxQuotes(c("n", "power", "sig.level")[which(c(n_null, pwr_null, sig_null))])
	nulls     = sum(n_null, pwr_null, sig_null)
	if(tabulatePower){
		if(!is.null(power)){
			stop("Ignoring power: when searching, I search for suitable n to get power")
		}
		mxPowerSearch(trueModel, falseModel = m2, n = n, sig.level = sig.level, method = method)
	} else {
		if(nulls == 0){
			stop("You filled in all three of ", setList, ": I've got nothing to estimate...\nSet one of these three to null. Probably n")
		} else if (nulls == 1){
			# great!
		} else if (nulls == 2){
			stop("You only set ", setList, ". I need two of n, power, and sig.level set by you to allow me to estimate the remaining one...")
		} else if (nulls == 3){
			stop("You didn't set any of ", setList, ": You need to fix two of these for me to be able to estimate the remaining one...")
		}
		m2 = umxModify(m1, update, value = value, name= paste0("drop_", update))
		message("\n####################\n# Estimating ", 	c("n", "power", "sig.level")[which(c(n_null, pwr_null, sig_null))], " #
####################\n")
		mxPower(trueModel, m2, n= n, power=power, sig.level = sig.level, method= method)	
		# rockchalk::lazyCor(.3,2)
	}
}

