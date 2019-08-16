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
#' A typical target for power is 80\%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
#' of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth
#' discourage running studies with low power: A study with 20 percent power will fail to detect real effects 80\% of the time.
#' But even with zero power, the Type-I error rate remains a nominal 5\\% (and with any researcher degrees of freedom, perhaps much more than that).
#' Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
#' This sounds silly, but empirical rates are often as low as 20\% (Button, et al., 2013).
#'
#' Illustration of  \eqn{\alpha}, \eqn{\beta}, and power (1-\eqn{\beta}):
#' 
#' \if{html}{\figure{power.png}{options: width="50\%" alt="Figure: power.png"}}
#' \if{latex}{\figure{power.pdf}{options: width=7cm}}
#'
#'
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE Unique environment variance. Leave NULL to compute an amount summing to 1
#' @param update Component to drop (Default "a", i.e., drop a)
#' @param n If provided, solve for power /sig.level at the given n (Default NULL)
#' @param MZ_DZ_ratio MZ pairs per DZ pair (Default 1 = equal numbers.)
#' @param sig.level alpha (p-value) Default = 0.05
#' @param power Default = .8 (80 percent power, equal to 1 - Type II rate)
#' @param value Value to set dropped path to (Default 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param method How to estimate power: Default =  use non-centrality parameter ("ncp"). Alternative is "empirical"
#' @param tryHard Whether to tryHard to find a solution (default = "no", alternatives are "yes"...)
#' @param optimizer If set, will switch the optimizer.
#' @param nSim Total number of pairs to simulate in the models (default = 4000)
#' @return [OpenMx::mxPower()] or [OpenMx::mxPowerSearch()] object
#' @family Twin Modeling Functions
#' @seealso - [OpenMx::mxPower()]
#' @references -
#' * Visscher, P.M., Gordon, S., Neale, M.C. (2008). Power of the classical twin design
#' revisited: II detection of common environmental variance. *Twin Res Hum Genet*, **11**: 48-54.
#' doi: [10.1375/twin.11.1.48](https://doi.org/10.1375/twin.11.1.48)
#' * Button, K. S., Ioannidis, J. P., Mokrysz, C., Nosek, B. A., Flint, J., Robinson, E. S., and Munafo, M. R. (2013).
#' Power failure: why small sample size undermines the reliability of neuroscience. 
#' *Nature Reviews Neuroscience*, **14**, 365-376. doi: [10.1038/nrn3475](https://doi.org/10.1038/nrn3475)
#' @export
#' @md
#' @examples
#'
#' # ===============================================
#' # = Power to detect a^2=.5 with equal MZ and DZ =
#' # ===============================================
#' power.ACE.test(AA = .5, CC = 0, update = "a") 
#' # Suggests n = 84 MZ and 94 DZ pairs.
#'
#' # ================================
#' # = Show power across range of N =
#' # ================================
#' power.ACE.test(AA= .5, CC= 0, update = "a", search = TRUE)
#'
#' # Salutary note: You need well fitting models with correct betas in the data
#' # for power to be valid.
#' # tryHard helps ensure this, as does the default nSim= 4000 pair data.
#' # Power is important to get right, so I recommend using tryHard = "yes"
#' power.ACE.test(AA= .5, CC= 0, update = "a", tryHard= "yes")
#' 
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' # 102 of each of MZ and DZ pairs for 80% power.
#' power.ACE.test(AA= .5, CC= .3, update = "c", tryHard= "yes")
#'
#' # ========================================
#' # = Set 'a' to a fixed, but non-zero value =
#' # ========================================
#' 
#' power.ACE.test(update= "a", value= sqrt(.2), AA= .5, CC= 0)
#' # TODO get power.ACE.test to print the value of A in the null model.
#'
#' # ========================================
#' # = Drop More than one parameter (A & C) =
#' # ========================================
#' # E vs AE: the hypothesis that twins show no familial similarity.
#' power.ACE.test(update = "a_after_dropping_c", AA= .5, CC= .3)
#'
#' # ===================================================
#' # = More power to detect A > 0 when more C present  =
#' # ===================================================
#' 
#' power.ACE.test(update = "a", AA= .5, CC= .0)
#' power.ACE.test(update = "a", AA= .5, CC= .3)
#'
#' # ====================================================
#' # = More power to detect C > 0 when more A present?  =
#' # ====================================================
#' 
#' power.ACE.test(update = "c", AA= .0, CC= .5)
#' power.ACE.test(update = "c", AA= .3, CC= .5)
#'
#'
#' # ===============================================
#' # = Power with more DZs than MZs and vice versa =
#' # ===============================================
#'
#' # Power about the same: total pairs with 2 MZs per DZ = 692, vs. 707
#' power.ACE.test(MZ_DZ_ratio= 2/1, update= "a", AA= .3, CC= 0, method="ncp", tryHard="yes")
#' power.ACE.test(MZ_DZ_ratio= 1/2, update= "a", AA= .3, CC= 0, method="ncp", tryHard="yes")
#'
#' \dontrun{
#' 
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#' # Compare to empirical mode: suggests 83.6 MZ and 83.6 DZ pairs
#'
#' power.ACE.test(update= "a", AA= .5, CC= 0, method= "empirical")
#' # method= "empirical": For 80% power, you need 76 MZ and 76 DZ pairs
#' power.ACE.test(update= "a", AA= .5, CC= 0, method = "ncp")
#' # method = "ncp": For 80% power, you need 83.5 MZ and 83.5 DZ pairs
#'
#' # ====================
#' # = Show off options =
#' # ====================
#' # 1. tryHard
#' 
#' power.ACE.test(update = "a", AA= .5, CC= 0, tryHard= "yes")
#'
#' # 2. toggle optimizer
#' power.ACE.test(update= "a", AA= .5, CC= 0, optimizer= "SLSQP")
#'
#' # 3. How many twin pairs in the base simulated data?
#' power.ACE.test(update = "a", AA= .5, CC= 0)
#' power.ACE.test(update = "a", AA= .5, CC= 0, nSim= 20)
#'
#' }
#'
power.ACE.test <- function(AA= .5, CC= 0, EE= NULL, update = c("a", "c", "a_after_dropping_c"), value = 0, n = NULL, MZ_DZ_ratio = 1, sig.level = 0.05, power = .8, method = c("ncp", "empirical"), search = FALSE, tryHard = c("no", "yes", "mxTryHard", "mxTryHardOrdinal", "mxTryHardWideSearch"), optimizer = NULL, nSim=4000){
	# # TODO why not equivalent to this?
	# # https://genepi.qimr.edu.au//general/TwinPowerCalculator/twinpower.cgi
	#
	# type = c("univariate", "bivariate", "GxE")
	# decimalplaces <- function(x) {
	#     if (abs(x - round(x)) > .Machine$double.eps^0.5) {
	#         nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = TRUE)[[1]][[2]])
	#     } else {
	#         return(0)
	#     }
	# }
	message("This is beta code: I likely will alter the interface!")	
	method  = match.arg(method)
	tryHard = match.arg(tryHard)
	update  = match.arg(update)
	falseModelName = paste0("drop_", update)

	if(method=="ncp" & !is.null(n) ){
		stop("method = 'ncp' does not work for fixed n. Use method = 'empirical' instead, or specify power to estimate in place of n")
	}

	# nSim = 4000
	# MZ_DZ_ratio is an odds & pMZ = odds/(1+odds)
	pMZ = MZ_DZ_ratio/(1 + MZ_DZ_ratio)
	nMZpairs = round(nSim * pMZ)
	nDZpairs = round(nSim * (1 - pMZ))
	# Turn off plotting
	oldSilent = umx_set_silent(TRUE)
	oldPlot = umx_set_auto_plot(FALSE, silent=TRUE);

	# 1. Generate data and run model 1
	# tmp = umx_make_TwinData(nMZpairs= 500, nDZpairs = 500, AA= .5, CC= 0, EE= NULL, varNames= "var", mean= 0, empirical= TRUE)
	tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC= CC, EE= EE, varNames= "var", mean= 0, empirical= TRUE)
	mzData = subset(tmp, zygosity == "MZ")
	dzData = subset(tmp, zygosity == "DZ")
	
	# ==============================================
	# = Build the "true" and "false" (null) models =
	# ==============================================
	# Make, don't run yet
	trueModel = umxACE(selDVs = "var", sep = "_T", mzData = mzData, dzData= dzData, autoRun = FALSE, optimizer = optimizer)

	# update = c("a", "c", "a_after_dropping_c")
	if(update == "a"){
		update = "a_r1c1"
	} else if(update == "c"){
		update = "c_r1c1"
	} else if(update == "a_after_dropping_c"){
		trueModel = umxModify(trueModel, update="c_r1c1", value = value, autoRun = FALSE)
		update = "a_r1c1"
	}
	# run the true Model
	trueModel = xmu_safe_run_summary(trueModel, autoRun = TRUE, summary = FALSE, std = TRUE, tryHard = tryHard, comparison= FALSE)
	# make and run the falseModel
	nullModel = umxModify(trueModel, update = update, value = value, name = falseModelName, tryHard = tryHard)

	# return plot to old value
	umx_set_auto_plot(oldPlot)
	umx_set_silent(oldSilent)    # reinstate
	
	if(search){
		# power is not an input to mxPowerSearch
		tmp = mxPowerSearch(trueModel= trueModel, falseModel= nullModel, n = n, sig.level = sig.level, method = method)
		plot(power ~ N, data = tmp)
		abline(h = power)
	} else {
		tmp = mxPower(trueModel=trueModel, falseModel= nullModel, n= n, sig.level = sig.level, power = power, method = method)
		nFound = attributes(tmp)$detail$n
		pairsUsed = paste0(round(nFound * pMZ), " MZ and ",round(nFound * (1 - pMZ)), " DZ pairs")
		if(!is.null(n)){
			paramSize = attributes(tmp)$detail$parameterDiff
			message(paste0("With ", pairsUsed, ", you have ", power * 100, "% power to detect a parameter of ", round(paramSize, 3)))
		} else {
			message(paste0("For ", power * 100, "% power, you need ", pairsUsed))
		}
	}
	return(tmp)
}


#' Test power to detect specified path values in a model.
#'
#' @description
#' `umxPower` takes an input model (the model of the true data), and tests power (or determines n)
#' to detect dropping (or changing the value) a path in this true model.
#' 
#' A typical target for power is 80\%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
#' of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth
#' discourage running studies with low power: A study with 20 percent power will fail to detect real effects 80\% of the time.
#' But even with zero power, the Type-I error rate remains a nominal 5\% (and with any researcher degrees of freedom, perhaps much more than that).
#' Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
#' This sounds silly, but empirical rates are often as low as 20\% (Button, et al., 2013).
#'
#' Illustration of  \eqn{\alpha}, \eqn{\beta}, and power (1-\eqn{\beta}):
#' 
#' \if{html}{\figure{power.png}{options: width="50\%" alt="Figure: power.png"}}
#' \if{latex}{\figure{power.pdf}{options: width=7cm}}
#'
#' @param trueModel The model with the parameters at values you expect in the population.
#' @param update The parameter(s) to drop
#' @param n How many subjects? (Default = NULL)
#' @param power Default = NULL (conventional level = .8)
#' @param sig.level Default = .05
#' @param value Value of dropped parameter (default = 0)
#' @param method "ncp" (default) or "empirical"
#' @param explore Whether to tabulate the range of n or effect size (if n specified). Default = FALSE.
#' @param silent Supress model runs printouts to console (TRUE)
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
#' # 1 Make some data
#' tmp = umx_make_raw_from_cov(qm(1, .3| .3, 1), n=2000, varNames= c("X", "Y"), empirical= TRUE)
#' 
#' # 2. Make model of true XY correlation of .3
#' m1 = umxRAM("corXY", data = tmp,
#'    umxPath("X", with = "Y"),
#'    umxPath(var = c("X", "Y"))
#' )
#' # 3. Test power to detect .3 versus 0, with n= 90 subjects
#' umxPower(m1, "X_with_Y", n= 90)
#' 
#' # ####################
#' # # Estimating power #
#' # ####################
#' # 
#' #    method = ncp
#' #         n = 90
#' #     power = 0.83
#' # sig.level = 0.05
#' # statistic = LRT
#'
#' # =================================================
#' # = Tabulate Power across a range of values of  n =
#' # =================================================
#' umxPower(m1, "X_with_Y", explore = TRUE)
#'
#' \dontrun{
#' 
#' # =====================================
#' # = Examples with method = empirical  =
#' # =====================================
#' 
#' # Power to detect r = .3 given n=90
#' umxPower(m1, "X_with_Y", n = 90, method = "empirical")
#' # power is .823
#' # Test using cor.test doing the same thing.
#' pwr::pwr.r.test(r = .3, n = 90)
#' #           n = 90
#' #           r = 0.3
#' #   sig.level = 0.05
#' #       power = 0.827
#' # alternative = two.sided
#' 
#' # Power search for detectable effect size, given n = 90
#' umxPower(m1, "X_with_Y", n= 90, method = "empirical", explore = TRUE)
#'
#' # Search X_with_Y:power relationship for n=90
#' # |    | X_with_Y | power | lower | upper |
#' # |:---|:---------|:------|:------|:------|
#' # | 1  | 0.03     | 0.27  | 0.15  | 0.44  |
#' # | 2  | 0.03     | 0.32  | 0.20  | 0.48  |
#' # | 3  | 0.04     | 0.38  | 0.26  | 0.53  |
#' # | 4  | 0.04     | 0.45  | 0.33  | 0.57  |
#' # | 5  | 0.04     | 0.51  | 0.41  | 0.61  |
#' # | 6  | 0.05     | 0.58  | 0.49  | 0.66  |
#' # | 7  | 0.05     | 0.64  | 0.57  | 0.71  |
#' # | 8  | 0.06     | 0.70  | 0.64  | 0.75  |
#' # | 9  | 0.06     | 0.75  | 0.69  | 0.80  |
#' # | 10 | 0.06     | 0.80  | 0.74  | 0.85  |
#' # | 11 | 0.07     | 0.84  | 0.77  | 0.88  |
#' # | 12 | 0.07     | 0.87  | 0.80  | 0.92  |
#' # | 13 | 0.08     | 0.90  | 0.83  | 0.94  |
#' # | 14 | 0.08     | 0.92  | 0.85  | 0.96  |
#' # | 15 | 0.08     | 0.94  | 0.87  | 0.97  |
#' # | 16 | 0.09     | 0.95  | 0.89  | 0.98  |
#' # | 17 | 0.09     | 0.96  | 0.91  | 0.98  |
#' # | 18 | 0.10     | 0.97  | 0.92  | 0.99  |
#' # | 19 | 0.10     | 0.98  | 0.93  | 0.99  |
#' # | 20 | 0.10     | 0.98  | 0.94  | 0.99  |
#'
#' }
#'
umxPower <- function(trueModel, update= NULL, n= NULL, power = NULL, sig.level= .05, value = 0, method= c("ncp", "empirical"), explore = FALSE, digits = 2, silent = TRUE){
	# rockchalk::lazyCor(.3,2)
	method   = match.arg(method)
	oldSilent = umx_set_silent(silent, silent = TRUE) # set silent and store existing value
	
	if(explore & method=="ncp" & !is.null(n) ){
		stop("method = 'ncp' does not work for searching with fixed n. Use method = 'empirical' instead, or specify power to estimate in place of n")
	}

	n_null         = is.null(n)
	pwr_null       = is.null(power)
	sig_null       = is.null(sig.level)
	nulls          = sum(n_null, pwr_null, sig_null)
	setList        = omxQuotes(c("n", "power", "sig.level")[which(!c(n_null, pwr_null, sig_null))])
	beingEstimated = omxQuotes(c("n", "power", "sig.level")[which(c(n_null, pwr_null, sig_null))])

	nullModel = umxModify(trueModel, update, value = value, name= paste0("drop_", update))
	if(explore){
		if(!is.null(power)){
			stop("Can't set power when exploring: I can only explore FOR power or effect size across a range of ns or effect sizes")
		}
		tmp = mxPowerSearch(trueModel, falseModel = nullModel, n = n, sig.level = sig.level, method = method)
		tmp = (umx_round(tmp, digits = digits))
		if(method=="ncp"){
			# delete the unused lower-upper columns
			tmp = tmp[,	1:2]
		}
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
		nullModel = umxModify(trueModel, update, value = value, name= paste0("drop_", update))
		message("\n#####################\n# Estimating ", beingEstimated, " #\n#####################\n")
		tmp = mxPower(trueModel, nullModel, n= n, power=power, sig.level = sig.level, method= method)	
		attributes(tmp)$detail$power = round(attributes(tmp)$detail$power, digits)
	}
	umx_set_silent(oldSilent) # reinstate
	return(tmp)
}

