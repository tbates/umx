# devtools::document("~/bin/umx"); devtools::install("~/bin/umx");
# ==================
# = Model Builders =
# ==================
#' umxEFA
#'
#' Perform full-information maximum-likelihood factor analysis on a data matrix.
#' You need only offer up manifest data, specify the number of factors.
#' 
#' You can create an EFA either by specifying some factor names:
#' 
#' umxEFA(factors = c("g", "v"), data = mtcars)
#' 
#' or 
#' 
#' umxEFA(factors = 2, data = mtcars)
#'
#' \figure{umxEFA.png}
#' 
#' \emph{notes}: In an EFA, all items may load on all factors.
#' For identification we need m^2 degrees of freedom. We get m * (m+1)/2 from fixing factor variances to 1 and covariances to 0.
#' We get another m(m-1)/2 degrees of freemdom by fixing the upper-right hand corner of the factor loadings
#' component of the A matrix. The manifest variances are also lbounded at 0.
#' 
#' EFA reports standardized loadings: to do this, we scale the data.
#' 
#' Bear in mind that factor scores are indeterminate and can be rotated.
#' 
#' This is very much early days. I will add "scores" if there is demand. Currently, you can
#' get scores from with \code{\link{mxFactorScores}}.
#' 
#' todo: detect ordinal items and switch to UWLS
#' 
#' @param x Either 1: data, 2: A formula (not implemented yet), 3: A collection of variable names, or 4: A name for the model.
#' @param factors Either number of factors to request or a vector of factor names.
#' @param data A dataframe of manifest columns you are modeling
#' @param covmat Covariance matrix of data you are modeling (not implemented)
#' @param n.obs Number of observations in covmat (if provided, default = NA)
#' @param rotation A rotation to perform on the loadings (default  = "varimax")
#' @param name A name for your model.
#' @param digits rounding (default = 2)
#' @param report What to report
#' @return - EFA \code{\link{mxModel}}
#' @family Super-easy helpers
#' @export
#' @seealso - \code{\link{factanal}}
#' @references - \url{http://github.com/tbates/umx}
#' @examples
#' myVars <- c("mpg", "disp", "hp", "wt", "qsec")
#' m1 = umxEFA(mtcars[, myVars], factors =   2, rotation = "promax")
#' loadings(m1)
#' m2 = factanal(~ mpg + disp + hp + wt + qsec, factors = 2, rotation = "promax", data = mtcars)
#' loadings(m2)
#' \dontrun{
#' plot(m2)
#' m3 = umxEFA(myVars, factors = 2, data = mtcars, rotation = "promax")
#' m4 = umxEFA(name = "named", factors = "g", data = mtcars[, myVars])
#' m5 = umxEFA(name = "by_number", factors = 2, rotation = "promax", data = mtcars[, myVars])
#' }
umxEFA <- function(x= NULL, factors = NULL, data = NULL, covmat = NULL, n.obs = NULL, rotation = c("varimax", "promax", "none"), name = "efa", digits = 2, report = c("1", "table", "html")){
	message("umxEFA is beta-only, and NOT ready for prime time")
	# name     = "efa"
	# factors  = 1
	# data     = mtcars[,c("mpg", "disp", "hp", "wt", "qsec")]
	# rotation = "varimax"
	if (!is.null(data)){
		# x must be formula, or column list && covmat and n.obs must be NULL
		if(!is.null(covmat) || !is.null(n.obs)){
			stop("Covmat and n.obs must be empty when using 'data =' ...")
		}
		if(!is.null(x)){
			if(length(x) > 1) {
				umx_check_names(x, data)
				data = data[,x]
			} else if (inherits(x,"formula")){
				stop("Nice formula! Sadly I can't handle formulae yet: email tim and abuse him about this failing")
				# todo: handle is.formula()
			}
		}
	} else if(!is.null(covmat) || !is.null(n.obs)){
		# data must be NULL
		stop("Covmat support not yet implemented - but you may as well be using factanal...")
		if(!is.null(data)){
			stop("You can't offer up both a data.frame and a covmat.")
		}
	} else {
		# x must be data
		if(!is.null(x)){
			if(is.data.frame(x) || is.matrix(x)){
				data = x # get data from x
			}
		} else if(is.null(data)){
			stop("You need to provide a data.frame to analyse: this can be in x, or data, or covmat")
		}
	}

	# what about for scores? then we don't want std loadings...
	data = umx_scale(data)
	rotation = umx_default_option(rotation, c("varimax", "promax", "none"), check = FALSE)
	if(is.null(factors)){
		stop("You need to request at least 1 latent factor.")
	} else if( length(factors)==1 && class(factors)=="numeric"){
		factors = paste0("F", c(1:factors))
	}
	# TODO adapt to input datatype, i.e., add cov handler
	manifests <- names(data)
	m1 <- umxRAM(model = name, data = data, autoRun = FALSE,
		umxPath(factors, to = manifests, connect = "unique.bivariate"),
		umxPath(v.m. = manifests),
		umxPath(v1m0 = factors, fixedAt = 1)
	)
	# Fix upper-right triangle of A-matrix factor columns at zero
	nFac       = length(factors)
	nManifests = length(manifests)
	if(nFac > 1){
		for(i in 2:nFac){
			m1$A$free[1:(i-1), factors[i]] = FALSE
			m1$A$values[1:(i-1), factors[i]] = 0
		}
	}
	# lbound the manifest diagonal to avoid mirror indeterminacy
	for(i in seq_along(manifests)) {
	   thisManifest = manifests[i]
	   m1$A$lbound[thisManifest, thisManifest] = 0
	}
	m1 = mxRun(m1)
	if(rotation != "none" && nFac > 1){
		x = loadings(m1, verbose = F)
		x = eval(parse(text = paste0(rotation, "(x)")))
		m1$A$values[manifests, factors] = x$loadings[1:nManifests, 1:nFac]
		print(x)
	} else {
		print(loadings(m1))
	}
	umxSummary(m1, digits = digits, report = report);
	invisible(m1)
}