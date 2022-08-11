#
#   Copyright 2007-2022 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# ==================
# = Model Builders =
# ==================

#' FIML-based Exploratory Factor Analysis (EFA)
#'
#' Perform full-information maximum-likelihood factor analysis on a data matrix.
#' 
#' As in [factanal()], you need only specify the number of factors and offer up
#' some manifest data, e.g:
#'                                                              
#' \code{umxEFA(factors = 2, data = mtcars)}
#' 
#' Equivalently, you can also give a list of factor names:
#' 
#' \code{umxEFA(factors = c("g", "v"), data = mtcars)}
#' 
#' The factor model is implemented as a structural equation model, e.g.
#' 
#'
#' \if{html}{\figure{umxEFA.png}{options: width=50% alt="Figure: umxEFA.png"}}
#' \if{latex}{\figure{umxEFA.pdf}{options: width=7cm}}
#' 
#' You can request \code{scores} from the model. Unlike factanal, these can cope with missing data.
#' 
#' You can also rotate the factors using any rotation function.
#' 
#' @details
#' In an EFA, all items may load on all factors.
#' 
#' Should work with rotations provided in `library("GPArotation")` and `library("psych")`, e.g
#' 
#' **Orthogonal**: "varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT" and "bifactor"
#' **Oblique**: "Promax", "promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin" and "cluster"
#' 
#' 
# #' For identification we need \ifelse{html}{{m<sup>2</sup>}{\eqn{m^2}} degrees of freedom. 
#' For identification we need \ifelse{html}{\out{m<sup>2</sup>}}{\eqn{m^2}} degrees of freedom. 
#' We get m(m+1)/2 from fixing factor variances to 1 and covariances to 0.
#' We get another m(m-1)/2 degrees of freedom by fixing the upper-right hand corner of
#' the factor loadings component of the A matrix at 0.
#' 
#' To aid optimization, manifest residual variances are `lbounded` at 0.
#' 
#' EFA reports standardized loadings: to do this, we scale the data.
#' 
#' *note*: Bear in mind that factor scores are indeterminate (can be rotated to an infinity of equivalent solutions).
#' 
#' Thanks to @ConorDolan for code implementing the rotation matrix and other suggestions!
#' 
#' 
#' @aliases umxFactanal umxEFA
#' @param x Either 1: data, 2: Right-hand-side ~ formula , 3: Vector of variable names, or 4: Name for the model.
#' @param factors Either number of factors to request or a vector of factor names.
#' @param data A dataframe you are modeling.
#' @param rotation A rotation to perform on the loadings (default  = "varimax" (orthogonal))
#' @param scores Type of scores to produce, if any. The default is none, "Regression" gives Thompson's scores. Other options are 'ML', 'WeightedML', Partial matching allows these names to be abbreviated.
#' @param minManifests The least number of variables required to return a score for a participant (Default = NA).
#' @param return by default, the resulting MxModel is returned. Say "loadings" to get a fact.anal object.
#' @param report Report as markdown to the console, or open a table in browser ("html")
#' @param summary  run [umxSummary()] on the underlying umxRAM model? (Default = FALSE)
#' @param name A name for your model (default = efa)
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param digits rounding (default = 2)
#' @param n.obs Number of observations in if covmat provided (default = NA)
#' @param covmat Covariance matrix of data you are modeling (not implemented)
#' @return - EFA [mxModel()]
#' @family Super-easy helpers
#' @export
#' @md
#' @seealso - [factanal()], [mxFactorScores()]
#' @references - <https://github.com/tbates/umx>,
#' 
#' Hendrickson, A. E. and White, P. O. (1964). Promax: a quick method for rotation to orthogonal oblique structure. *British Journal of Statistical Psychology*, **17**, 65–70. \doi{10.1111/j.2044-8317.1964.tb00244.x}.
#' 
#' Kaiser, H. F. (1958). The varimax criterion for analytic rotation in factor analysis. *Psychometrika*, **23**, 187–200. \doi{10.1007/BF02289233}.
#' 
#'
#' @examples
#' \dontrun{
#' myVars = c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(mtcars[, myVars], factors =   2, rotation = "promax")
#' # By default, returns the model
#' umx_is_MxModel(m1) # TRUE
#' # The loadings are stashed in the model:
#' loadings(m1)
#' 
#' # Formula interface in umxEFA
#' m2 = umxEFA(~ mpg + disp + hp + wt + qsec, factors = 2, rotation = "promax", data = mtcars)
#' loadings(m2)
#' 
#' # base-R factanal Formula interface for comparison
#' m2 = factanal(~ mpg + disp + hp + wt + qsec, factors = 2, rotation = "promax", data = mtcars)
#' loadings(m2)
#'
#' # Return the loadings object
#' x = umxEFA(mtcars[, myVars], factors = 2, return = "loadings")
#' names(x) # "loadings" "rotmat"
#' 
#' # scores requested, so these will be returned
#' x = umxEFA(name = "score", factors = "g", data = mtcars[, myVars], scores= "Regression")
#' head(x)
#' #       g
#' # 1  -0.48059346
#' # 2  -0.42354000
#' # 3  -0.87078110
#'
#' m1 = umxEFA(myVars, factors = 2, data = mtcars, rotation = "promax")
#' m1 = umxEFA(name = "named", factors = "g", data = mtcars[, myVars])
#' m1 = umxEFA(name = "by_number", factors = 2, rotation = "promax", data = mtcars[, myVars])
#' 
#' }
umxEFA <- function(x = NULL, factors = NULL, data = NULL, scores = c("none", 'ML', 'WeightedML', 'Regression'), minManifests = NA,
	rotation = c("varimax", "promax", "none"), return = c("model", "loadings"), report = c("markdown", "html"), summary = FALSE, name = "efa", digits = 2, tryHard = c("no", "yes", "ordinal", "search"), n.obs = NULL, covmat = NULL){
	# TODO: umxEFA: Detect ordinal items and switch to DWLS?
	rotation = xmu_match.arg(rotation, c("varimax", "promax", "none"), check = FALSE)
	tryHard    = match.arg(tryHard)
	scores   = match.arg(scores)
	return   = match.arg(return)

	# "Bartlett" given Bartlett's weighted least-squares scores. 
	# name     = "efa"
	# factors  = 1
	# data     = mtcars[,c("mpg", "disp", "hp", "wt", "qsec")]
	# rotation = "varimax"
	if (!is.null(data)){
		# x must be formula, or column list && covmat and n.obs must be NULL
		if(!is.null(covmat) || !is.null(n.obs)){
			stop("umxEFA: covmat and n.obs must be empty when using 'data =' ...")
		}
		if(!is.null(x)){
			if (inherits(x,"formula")){
				if(is.null(data)){
					stop(paste("If you provide a formula in x to select variable, data must contain a dataframe"))
				} else {
					x = all.vars(x)
					data = data[, x]
					name = "EFA"
				}
			} else if(length(x) > 1) {
				umx_check_names(x, data)
				data = data[,x]
				name = "EFA"
			}else{
				name = x
			}
		}else{
			name = "EFA"
		}
	} else if(!is.null(covmat) || !is.null(n.obs)){
		# data must be NULL
		stop("With cov data, you may as well be using factanal()...")
		if(!is.null(data)){
			stop("You can't offer up both a data.frame and a covmat.")
		}
	} else {
		# data is empty, so x must be data
		if(!is.null(x)){
			if(is.data.frame(x)){
				data = x # get data from x
			}else if (is.matrix(x)){
				data = as.data.frame(x)
			}
		} else if(is.null(data)){
			stop("You need to provide a data.frame to analyze: This can be in x, or data, or covmat")
		}
		name = "EFA"
	}

	# TODO: umxEFA scale data - What about for scores? Do we want std loadings in that case?...
	data = umx_scale(data)
	if(is.null(factors)){
		stop("You need to request at least 1 latent factor, e.g.: factors = 4")
	} else if( length(factors) == 1 && inherits(factors, "numeric")){
		factors = paste0("F", c(1:factors))
	}else{
		# factors is a list of factor names (we hope)
	}
	# TODO umxEFA: Adapt to input datatype, i.e., add cov handler
	manifests = names(data)
	m1 = umxRAM(model = name, data = data, autoRun = FALSE,
		umxPath(factors, to = manifests, connect = "unique.bivariate"),
		umxPath(v.m. = manifests),
		umxPath(v1m0 = factors)
	)
	# Fix upper-right triangle of A-matrix factor columns at zero
	nFac       = length(factors)
	nManifests = length(manifests)
	if(nFac > 1){
		for(i in 2:nFac){
			m1$A$free[1:(i-1)  , factors[i]] = FALSE
			m1$A$values[1:(i-1), factors[i]] = 0
		}
	}
	# TODO: umxEFA lbound the lambda top-right corner of A @0 to avoid mirror indeterminacy
	# Bound residual variance at 0
	for(i in seq_along(manifests)) {
	   thisManifest = manifests[i]
	   m1$S$lbound[thisManifest, thisManifest] = 0
	}

	m1 = umxRun(m1, tryHard= tryHard)

	# ============================
	# = Do rotation if requested =
	# ============================
	if(rotation != "none" && nFac > 1){
		oldLoadings = loadings.MxModel(m1)
		newLoadings = eval(parse(text = paste0(rotation, "(oldLoadings)")))
		if(!umx_set_silent(silent=TRUE)){
			print("Rotation results")
			print(newLoadings) # print out the nice rotation result
			rm = newLoadings$rotmat
			print("Factor Correlation Matrix")
			print(solve(t(rm) %*% rm))
		}
		# stash the rotated result in the model A matrix
		m1$A$values[manifests, factors] = newLoadings$loadings[1:nManifests, 1:nFac] 
	} else if(!umx_set_silent(silent=TRUE)){
		print("Results")
		print(loadings(m1))
		newLoadings = loadings.MxModel(m1)
	}

	if(summary){
		umxSummary(m1, digits = digits, report = report);
	}
	
	if(scores != "none"){
		factorScores = umxFactorScores(m1, type = scores, minManifests = minManifests)
		return(factorScores)
	}
	if(return == "loadings"){
		invisible(newLoadings)
	}else if(return == "model"){
		invisible(m1)
	}else{
		stop("polite error: Not sure what ", omxQuotes(return), " is to return it" )
	}
}

#' @export
umxFactanal <- umxEFA

#' Return factor scores from a model as an easily consumable dataframe.
#' @description
#' umxFactorScores takes a model, and computes factors scores using the selected method (one 
#' of 'ML', 'WeightedML', or 'Regression')
#' It is a simple wrapper around mxFactorScores. For missing data, you must specify the least number of 
#' variables allowed for a score (subjects with fewer than minManifests will return a score of NA.
#' @param model The model from which to generate scores.
#' @param type  Method of computing the score ('ML', 'WeightedML', or 'Regression').
#' @param minManifests The minimum number of variables not NA to return a score for a participant (Default = ask).
#' @param return What to return (defaults to "Scores", which is what most users want, but can return "StandardErrors" on each score.
#' @return - dataframe of scores.
#' @export
#' @family Reporting Functions
#' @seealso - [mxFactorScores()]
#' @references - <https://github.com/tbates/umx>, <https://tbates.github.io>
#' @md
#' @examples
#' \dontrun{
#' m1 = umxEFA(mtcars, factors = 2)
#' x = umxFactorScores(m1, type = 'Regression', minManifests = 3)
#' 
#' # =========================================================================
#' # = histogram of F1 and plot of F1 against F2 showing they are orthogonal =
#' # =========================================================================
#' hist(x$F1)
#' plot(F1 ~ F2, data = x)
#' 
#' m1 = umxEFA(mtcars, factors = 1)
#' x = umxFactorScores(m1, type = 'Regression', minManifests = 3)
#' x
#' }
umxFactorScores <- function(model, type = c('ML', 'WeightedML', 'Regression'), minManifests = NA, return = c("Scores", "StandardErrors")) {
	type = match.arg(type)
	return = match.arg(return)
	suppressMessages({
		scores = mxFactorScores(model, type = type, minManifests = minManifests)
	})
	# Only need score from [nrow, nfac, c("Scores", "StandardErrors")]
	out = scores[ , , return, drop = FALSE]
	out = data.frame(out)
	# make names always "F1", not "F1.Scores" "F1.StandardErrors"
	names(out) <- dimnames(scores)[[2]]
	return(out)

	# if(dim(scores)[2] == 1){
	# 	# simulate drop = FALSE if only 1 factor
	# 	out = scores[ , 1, return]
	# 	out = data.frame(out)
	# 	names(out) <- dimnames(scores)[[2]]
	# 	return(out)
	# } else {
	# 	return(scores[ , , return])
	# }
}


#' Build a SEM implementing the instrumental variable design
#'
#' `umxMR` (`umxTwoStage`) implements a Mendelian randomization or instrumental variable Structural Equation Model.
#' For ease of learning, the parameters follow the `tsls()` function in the sem package.
#' 
#' The example is a [Mendelian Randomization](https://en.wikipedia.org/wiki/Mendelian_randomization)
#' analysis showing the utility of SEM over two-stage regression.
#' 
#' The following figure shows how the MR model appears as a path diagram:
#' 
#' \if{html}{\figure{TSLS.png}{options: width=50% alt="Figure: Mendelian Randomisation analysis.png"}}
#' \if{latex}{\figure{TSLS.pdf}{options: width=7cm}}

#' @aliases umxMR
#' @param formula The structural equation to be estimated (default = Y ~ X). A constant is implied if not explicitly deleted.
#' @param instruments A one-sided formula specifying instrumental variables (default = qtl).
#' @param data Frame containing the variables in the model.
#' @param subset (optional) vector specifying a subset of observations to be used in fitting the model.
#' @param contrasts	an optional list (not supported)
#' @param name for the model (default = "tsls")
#' @param ...	arguments to be passed along. (not supported)
#' @return - [mxModel()]
#' @export
#' @family Super-easy helpers
#' @seealso - [umx_make_MR_data()], [umxRAM()]
#' @references - Fox, J. (1979) Simultaneous equation models and two-stage least-squares. In Schuessler, K. F. (ed.) *Sociological Methodology*, Jossey-Bass.
#' * Greene, W. H. (1993) *Econometric Analysis*, Second Edition, Macmillan.
#' * Sekula, P., Del Greco, M. F., Pattaro, C., & Kottgen, A. (2016). Mendelian Randomization as an Approach to 
#' Assess Causality Using Observational Data. *Journal of the American Society of Nephrology*, **27**), 3253-3265. <doi:10.1681/ASN.2016010098>
#' @md
#' @examples
#' \dontrun{
#' library(umx)
#' 
#' 
#' # ====================================
#' # = Mendelian Randomization analysis =
#' # ====================================
#' 
#' df = umx_make_MR_data(10e4)
#' m1 = umxMR(Y ~ X, instruments = ~ qtl, data = df)
#' parameters(m1)
#' plot(m1, means = FALSE, min="") # help DiagrammaR layout the plot.
#' m2 = umxModify(m1, "qtl_to_X", comparison=TRUE, tryHard="yes", name="QTL_affects_X") # yip
#' m3 = umxModify(m1, "X_to_Y"  , comparison=TRUE, tryHard="yes", name="X_affects_Y") # nope
#' plot(m3, means = FALSE)
#' 
#' # Errant analysis using ordinary least squares regression (WARNING this result is CONFOUNDED!!)
#' m1 = lm(Y ~ X    , data = df); coef(m1) # incorrect .35 effect of X on Y
#' m1 = lm(Y ~ X + U, data = df); coef(m1) # Controlling U reveals the true 0.1 beta weight
#'
#'
#' df = umx_make_MR_data(10e4) 
#' m1 = umxMR(Y ~ X, instruments = ~ qtl, data = df)
#' coef(m1)
#' 
#' # ======================
#' # = Now with sem::tsls =
#' # ======================
#' # library(sem) # may require you to install X11
#' m2 = sem::tsls(formula = Y ~ X, instruments = ~ qtl, data = df)
#' coef(m2)
#'
#' # Try with missing value for one subject: A benefit of the FIML approach in OpenMx.
#' m3 = tsls(formula = Y ~ X, instruments = ~ qtl, data = (df[1, "qtl"] = NA))
#' }
umxTwoStage <- function(formula= Y ~ X, instruments = ~qtl, data, subset, contrasts= NULL, name = "IV_model", ...) {
	umx_check(is.null(contrasts), "stop", "Contrasts not supported yet in umxMR: e-mail maintainer('umx') to prioritize")	
	if(!inherits(formula, "formula")){
		stop("formula must be a formula")
	}
	allForm = all.vars(terms(formula))
	if(length(allForm) != 2){
		stop("I'm currently limited to 1 DV, 1 IV, and 1 instrument: 'formula' had ", length(allForm), " items")
	}
	DV   = allForm[1] # left hand item
	Xvars  = all.vars(delete.response(terms(formula)))
	inst = all.vars(terms(instruments))
	if(length(inst) != 1){
		stop("I'm currently limited to 1 DV, 1 IV, and 1 instrument: 'instruments' had ", length(allForm), " items")
	}
	manifests <- c(allForm, inst)     # manifests <- c("qtl", "X", "Y")
	latentErr <- paste0("e", allForm) # latentErr <- c("eX", "eY")
	umx_check_names(manifests, data = data, die = TRUE)

	IVModel = umxRAM(name, data = data,
		# Causal and confounding paths
		umxPath(inst , to = Xvars), # beta of SNP effect          :  X ~ b1 x inst
		umxPath(Xvars, to = DV),    # Causal effect of Xvars on DV: DV ~ b2 x X
		# Latent error stuff + setting up variance and means for variables
		umxPath(v.m. = inst),     # Model variance and mean of instrument
		umxPath(var = latentErr), # Variance of residual errors
		umxPath(latentErr, to = allForm, fixedAt = 1), # X and Y residuals@1
		umxPath(unique.bivariate = latentErr, values = 0.2, labels = paste0("phi", length(latentErr)) ), # Correlation among residuals
		umxPath(means = c(Xvars, DV))
	)
	return(IVModel)
}

#' @export
umxMR <- umxTwoStage
