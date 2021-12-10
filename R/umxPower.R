#' Test the power of an ACE model to detect paths of interest.
#'
#' @description `power.ACE.test` simulates a univariate ACE model. It 
#' computes power to detect dropping one or more paths (a, c, or a after dropping c), specified in `drop=`. 
#' 
#' The interface and functionality of this service are experimental and subject to change.
#' 
#' @details
#' Statistical power is the proportion of studies that, over the long run, one should expect to yield a statistically
#' significant result given certain study characteristics such as sample size (N), the expected effect size (\eqn{\beta}),
#' and the criterion for statistical significance (\eqn{\alpha}).
#'
#' (with nMZpairs= 2000 and MZ_DZ_ratio*nMZpairs DZ twins.
#'
#' A typical target for power is 80%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
#' of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth
#' discourage running studies with low power: A study with 20 percent power will fail to detect real effects 80% of the time.
#' But even with zero power, the Type-I error rate remains a nominal 5% (and with any researcher degrees of freedom, perhaps much more than that).
#' Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
#' This sounds silly, but empirical rates are often as low as 20% (Button, et al., 2013).
#'
#' Illustration of  \eqn{\alpha}, \eqn{\beta}, and power (1-\eqn{\beta}):
#' 
#' \if{html}{\figure{power.png}{options: width=50% alt="Figure: power.png"}}
#' \if{latex}{\figure{power.pdf}{options: width=7cm}}
#'
#'
#' @param AA Additive genetic variance (Default .5)
#' @param CC Shared environment variance (Default 0)
#' @param EE Unique environment variance. Leave NULL (default) to compute an amount summing to 1.
#' @param DD Dominance Is set (default= NULL) compute an ADE rather than ACE model (DZr=.25)
#' @param update Component to drop (Default "a", i.e., drop a)
#' @param n If provided, solve at the given number of MZ+DZ pairs (Default NULL)
#' @param MZ_DZ_ratio MZ pairs per DZ pair (Default 1 = equal numbers.)
#' @param sig.level alpha (p-value) Default = 0.05
#' @param power Default = .8 (80 percent power, equal to 1 - Type II rate)
#' @param value Value to set dropped path to (Default 0)
#' @param search Whether to return a search across power or just a point estimate (Default FALSE = point)
#' @param method How to estimate power: Default =  use non-centrality parameter ("ncp"). Alternative is "empirical"
#' @param tryHard Whether to tryHard to find a solution (default = "yes", alternatives are "no"...)
#' @param digits Rounding for reporting parameters (default 2)
#' @param optimizer If set, will switch the optimizer.
#' @param nSim Total number of pairs to simulate in the models (default = 4000)
#' @return [OpenMx::mxPower()] object
#' @family Twin Modeling Functions
#' @seealso - [umxPower()], [OpenMx::mxPower()], [umxACE()] 
#' @references * Visscher, P.M., Gordon, S., Neale, M.C. (2008). Power of the classical twin design
#' revisited: II detection of common environmental variance. *Twin Res Hum Genet*, **11**: 48-54.
#' \doi{10.1375/twin.11.1.48}.
#' * Button, K. S., Ioannidis, J. P., Mokrysz, C., Nosek, B. A., Flint, J., Robinson, E. S., and Munafo, M. R. (2013).
#' Power failure: why small sample size undermines the reliability of neuroscience. 
#' *Nature Reviews Neuroscience*, **14**, 365-376. \doi{10.1038/nrn3475}
#' @export
#' @md
#' @examples
#'
#' # =====================================================
#' # = N for .8 power to detect a^2 = .5 equal MZ and DZ =
#' # =====================================================
#' power.ACE.test(AA = .5, CC = 0, update = "a")
#' # Suggests n = 84 MZ and 94 DZ pairs.
#'
#' \dontrun{
#' # ================================
#' # = Show power across range of N =
#' # ================================
#' power.ACE.test(AA= .5, CC= 0, update = "a", search = TRUE)
#'
#' # Salutary note: You need well fitting models with correct betas in the data
#' # for power to be valid.
#' # tryHard helps ensure this, as does the default nSim= 4000 pair data.
#' # Power is important to get right, so I recommend using tryHard = "yes" (the default)
#' 
#' # =====================
#' # = Power to detect C =
#' # =====================
#' 
#' # 102 of each of MZ and DZ pairs for 80% power (default).
#' power.ACE.test(AA= .5, CC= .3, update = "c")
#'
#' # ==========================================
#' # = Set 'a' to a fixed, but non-zero value =
#' # ==========================================
#' 
#' power.ACE.test(update= "a", value= sqrt(.2), AA= .5, CC= 0)
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
#' # ===================================
#' # = Power with more DZs or more MZs =
#' # ===================================
#'
#' # Power about the same: total pairs with 2 MZs per DZ
#' power.ACE.test(MZ_DZ_ratio= 2/1, update= "a", AA= .3, CC= 0, method="ncp", tryHard="yes")
#' power.ACE.test(MZ_DZ_ratio= 1/2, update= "a", AA= .3, CC= 0, method="ncp", tryHard="yes")
#' power.ACE.test(update= "a", AA= .3, CC= 0, method="ncp", tryHard="yes")
#'
#' 
#' # =====================================
#' # = Compare ncp and empirical methods =
#' # =====================================
#'
#' power.ACE.test(update= "a", AA= .5, CC= 0, method = "ncp")
#' # method = "ncp": For 80% power, you need 166 MZ and 166 DZ pairs
#' power.ACE.test(update= "a", AA= .5, CC= 0, method= "empirical")
#' # method= "empirical": For 80% power, you need 154 MZ and 154 DZ pairs
#'
#' # ====================
#' # = Show off options =
#' # ====================
#' # 1. tryHard
#' 
#' power.ACE.test(update = "a", AA= .5, CC= 0, tryHard= "no")
#'
#' # 2. toggle optimizer
#'
#' power.ACE.test(update= "a", AA= .5, CC= 0, optimizer= "SLSQP")
#'
#' # 3. You can raise or lower the number of pairs used in the true model
#' #    by varying nSim (twin pairs in the simulated data).
#'
#' power.ACE.test(update = "a", AA= .5, CC= 0, nSim= 20)
#'
#' }
#'
power.ACE.test <- function(AA= .5, CC= 0, EE= NULL, DD = NULL, update = c("a", "c", "a_after_dropping_c", "d"), value = 0, n = NULL, MZ_DZ_ratio = 1, sig.level = 0.05, power = .8, method = c("ncp", "empirical"), search = FALSE, tryHard = c("yes", "no", "ordinal", "search"), digits = 2, optimizer = NULL, nSim=4000){
	# # TODO why not equivalent to this?
	# # https://genepi.qimr.edu.au//general/TwinPowerCalculator/twinpower.cgi
	#
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

	# =================================================
	# = Build the "true" model & "false" (null) model =
	# =================================================
	# 1. Generate data and run model 1
	if(!is.null(DD)){
		tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC=0, DD= DD, EE= EE, varNames= "var", mean= 0, empirical= TRUE)
		mzData = subset(tmp, zygosity == "MZ")
		dzData = subset(tmp, zygosity == "DZ")
		trueModel = umxACE(selDVs = "var", sep = "_T", mzData = mzData, dzData= dzData, dzCr = .25, autoRun = FALSE, optimizer = optimizer)
	} else {
		tmp = umx_make_TwinData(nMZpairs= nMZpairs, nDZpairs = nDZpairs, AA= AA, CC= CC, EE= EE, varNames= "var", mean= 0, empirical= TRUE)
		mzData = subset(tmp, zygosity == "MZ")
		dzData = subset(tmp, zygosity == "DZ")
		trueModel = umxACE(selDVs = "var", sep = "_T", mzData = mzData, dzData= dzData, autoRun = FALSE, optimizer = optimizer)
	}

	# update = c("a", "c", "a_after_dropping_c")
	if(update == "a"){
		update = "a_r1c1"
		paramSize = AA
	} else if(update == "c"){
		update = "c_r1c1"
		paramSize = CC
	} else if(update == "d"){
		update = "c_r1c1"
		paramSize = DD
	} else if(update == "a_after_dropping_c"){
		trueModel = umxModify(trueModel, update="c_r1c1", value = 0, autoRun = FALSE)
		update = "a_r1c1"
		paramSize = AA
	}
	# Run the true Model, then modify to create the falseModel
	trueModel = xmu_safe_run_summary(trueModel, summary = FALSE, std = TRUE, tryHard = tryHard, comparison= FALSE)
	nullModel = umxModify(trueModel, update = update, value = value, name = falseModelName, tryHard = tryHard)

	# return plot and silent to old values
	umx_set_auto_plot(oldPlot)
	umx_set_silent(oldSilent)
	
	if(search){
		# power is not an input to mxPowerSearch
		tmp = mxPowerSearch(trueModel= trueModel, falseModel= nullModel, n = n, sig.level = sig.level, method = method)
		plot(power ~ N, data = tmp,  xlab= "N (total MZ+DZ pairs)")
		abline(h = power)
	} else {
		tmp = mxPower(trueModel= trueModel, falseModel= nullModel, n= n, sig.level = sig.level, power = power, method = method)
		nFound = attributes(tmp)$detail$n
		pairsUsed = paste0(round(nFound * pMZ), " MZ and ",round(nFound * (1 - pMZ)), " DZ pairs")
		if(!is.null(n)){
			if(!is.null(power)){
				cat(paste0("Given", pairsUsed, ", and your chosen ", round(power * 100, digits),
					" power, you can detect a ", update, " parameter of ", round(paramSize, 3), ".\n"))
			}else{
				empiricalPower = attributes(tmp)$detail$power
				cat(paste0("Given ", pairsUsed, " and ", round(paramSize, 3), " ", update, 
					" you have ", round(empiricalPower * 100, digits), "% power.\n"))	
			}
		} else {
			cat(paste0("For ", round(power * 100, digits), "% power to detect ", omxQuotes(update), 
				" of size ", paramSize, ", you need ", pairsUsed, ".\n"))
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
#' A typical target for power is 80%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
#' of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth
#' discourage running studies with low power: A study with 20 percent power will fail to detect real effects 80% of the time.
#' But even with zero power, the Type-I error rate remains a nominal 5% (and with any researcher degrees of freedom, perhaps much more than that).
#' Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
#' This sounds silly, but empirical rates are often as low as 20% (Button, et al., 2013).
#'
#' Illustration of  \eqn{\alpha}, \eqn{\beta}, and power (1-\eqn{\beta}):
#' 
#' \if{html}{\figure{power.png}{options: width=50% alt="Figure: power.png"}}
#' \if{latex}{\figure{power.pdf}{options: width=7cm}}
#'
#' @param trueModel The model with the parameters at values you expect in the population.
#' @param update The parameter(s) to drop
#' @param n How many subjects? (Default = NULL)
#' @param power Default = NULL (conventional level = .8)
#' @param sig.level Default = .05
#' @param value Value of dropped parameter (default = 0)
#' @param method "ncp" (default) or "empirical"
#' @param plot whether to plot the power.
#' @param explore Whether to tabulate the range of n or effect size (if n specified). Default = FALSE.
#' @param digits Rounding precision for reporting result.
#' @param silent Suppress model runs printouts to console (TRUE)
#' @return power table
#' @export
#' @family Teaching and Testing functions
#' @seealso - [power.ACE.test()], [umxRAM()]
#' @references - Miles, J. (2003). A framework for power analysis using a structural equation modelling procedure. *BMC Medical Research Methodology*, **3**, 27. \doi{10.1186/1471-2288-3-27}
#' * [Superpower package](https://CRAN.R-project.org/package=Superpower)
#' @md
#' @examples
#' \dontrun{
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
#' umxPower(m1, "X_with_Y", explore = TRUE)
#' umxPower(m1, "X_with_Y", n= 90, explore = TRUE)
#' umxPower(m1, "X_with_Y", n= 90, method = "empirical", explore = TRUE)
#'
#'
#' data(twinData) # ?twinData from Australian twins.
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACE(selDVs = "ht", selCovs = "age", sep = "", dzData = dzData, mzData = mzData)
#'
#' # drop more than 1 path
#' umxPower(m1, update = c("c_r1c1", "age_b_Var1"), method = 'ncp', n=90, explore = TRUE)
#'
#' # Specify only 1 parameter (not 'age_b_Var1' and 'c_r1c1' ) to search a parameter:power relationship
#' # note: Can't use method = "ncp" with search)
#' umxPower(m1, update = c("c_r1c1", "age_b_Var1"), method = 'empirical', n=90, explore = TRUE)
#' umxPower(m1, update = c("c_r1c1"), method = 'empirical', n=90, explore = TRUE)
#' 
#' }
umxPower <- function(trueModel, update= NULL, n= NULL, power = NULL, sig.level= .05, value = 0, method= c("ncp", "empirical"), explore = FALSE, digits = 2, plot=TRUE, silent = TRUE){
	# rockchalk::lazyCor(.3,2)
	method   = match.arg(method)
	oldSilent = umx_set_silent(silent, silent = TRUE) # set silent and store existing value
	
	if(explore & method=="ncp" & !is.null(n) ){
		stop("method = 'ncp' does not work for both explore AND fixed n. Try method = 'empirical'")
	}
	
	if(explore & !is.null(n) & length(update)>1 ){
		stop("Exploration with fixed n only works for updates of 1 parameter. Either remove n=, or try one parameter at a time")
	}

	n_null         = is.null(n)
	pwr_null       = is.null(power)
	sig_null       = is.null(sig.level)
	nulls          = sum(n_null, pwr_null, sig_null)
	setList        = omxQuotes(c("n", "power", "sig.level")[which(!c(n_null, pwr_null, sig_null))])
	beingEstimated = omxQuotes(c("n", "power", "sig.level")[which(c(n_null, pwr_null, sig_null))])

	nullModel = umxModify(trueModel, update, value = value, name= paste0("drop_", update, collapse = "_"))
	if(explore){
		# if n is set, the models must differ by only 1 parameter. search then searches the power 
		# for detecting detecting different parameter value changes at the given n.
		tmp = mxPowerSearch(trueModel, falseModel = nullModel, n = n, sig.level = sig.level, method = method)
		tmp = umx_round(tmp, digits = digits)
		if(method == "ncp"){
			# delete the unused lower-upper columns
			tmp = tmp[,	1:2]
		}
		if(plot){
			if(is.null(n)){
				# establish values at fixed power (or .8)
				if(is.null(power)){ power = .80}
				est = mxPower(trueModel, falseModel = nullModel, power = power, sig.level = sig.level, method = method)
				estimatedN = round(attributes(est)$detail$n, digits)
				# color the power line, and plot dots at estimated points.
				p = ggplot(data = tmp, aes(x= n, y= power)) + geom_line(color = "red", size = .5, alpha = 0.9)
				p = p + geom_point()
				p = p + labs(x= "Sample Size (N)", y= "Power = 1 - \U03B2",
				   title = paste0("Statistical power to detect true model"),
			       subtitle = paste0("Alpha = ", sig.level),
			       caption = paste0("Lists of changed paths: ", omxQuotes(update))
				) 
				p = p + ggplot2::theme_bw() + cowplot::draw_label(paste0("N for ", power*100, "% power = ", estimatedN), x = estimatedN, y = .8, hjust = 0)
				p = p + ggplot2::geom_vline(xintercept = estimatedN, linetype=2, colour="grey") # dashed
				print(p)
			} else {
				if(is.null(power)){ power = .80}
				# est = mxPower(trueModel, falseModel = nullModel, n = n, power = power, sig.level = sig.level, method = method)
				# estimatedEffect = round(attributes(est)$detail$n, digits)

				# color the power line, and plot dots at estimated points.
				tmp$x = tmp[,1] # create a uniform name for the x axis variable (effect size of whatever labelled parameter was dropped)
				p = ggplot(data = tmp, aes(x= x, y = power)) + geom_line(color = "red", size = .5, alpha = 0.9)
				p = p + geom_point()
				p = p + labs(
				   x        = paste0("Effect Size (", omxQuotes(update), ")"), 
				   y        = "Power = 1 - \U03B2",
				   title    = paste0("Statistical power with N = ", n),
			       subtitle = paste0("Alpha = ", sig.level)
				) 
				# p = p + ggplot2::theme_bw() + cowplot::draw_label(paste0("Effect for ", power*100, "% power = ", estimatedEffect), x = estimatedEffect, y = .8, hjust = 0)
				# p = p + ggplot2::geom_vline(xintercept = estimatedEffect, linetype=2, colour="grey") # dashed
				print(p)
			}
		}
	} else {
		# explore = FALSE
		if(nulls == 0){
			stop("You filled in all three of ", setList, ": I've got nothing to estimate...\nSet one of these to null. Probably n")
		} else if (nulls == 1){
			# great!
		} else if (nulls == 2){
			stop("You only set ", setList, ". I need two of n, power, and sig.level set by you to allow me to estimate the remaining one...")
		} else if (nulls == 3){
			stop("You didn't set any of ", setList, ": You need to fix two of these for me to be able to estimate the remaining one...")
		}
		nullModel = umxModify(trueModel, update, value = value, name= paste0("drop_", update, collapse = "_"))
		message("\n#####################\n# Estimating ", beingEstimated, " #\n#####################\n")
		tmp = mxPower(trueModel, nullModel, n = n, power = power, sig.level = sig.level, method = method)
		attributes(tmp)$detail$power = round(attributes(tmp)$detail$power, digits)
		if(plot){
			# message("Nothing to plot: try `explore = TRUE` next time?")
		}
		
	}
	umx_set_silent(oldSilent) # reinstate silent status
	return(tmp)
}

