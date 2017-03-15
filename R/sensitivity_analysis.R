#' sensitivity_analysis
#'
#' @description
#' Reports a comprehensive sensitivity analysis.
#'
#' @details Original written by Sven Kepes, Frank Bosco, Jamie Field, Mike McDaniel. Version 1.5 (July 26, 2016)
#' @param dat the dataframe of meta-analytic data (must contain columns for r and N).
#' @param Output.Title = "No Output.Title Provided".
#' @param prw.r.alternative what option chosen: defaults to two.
#' @return - \code{\link{mxModel}}
#' @export
#' @family Stats
#' @references - \url{https://github.com/tbates/umx}
#' @examples
#' \dontrun{
#' sensitivity_analysis(dat = data, Output.Title = "Brain volume and IQ")
#' }
sensitivity_analysis <- function(dat = NA, Output.Title = "No Output.Title Provided", prw.r.alternative = c("two.sided", "less", "greater")) {
	# require(foreign)
	# import	pwr.r.test
	if(is.na(dat)){
		die("you need to provide a dataframe of meta-analytic statistics")
	}
	colNames = names(df)
	msg = "r is the correlations, N the number of subjects"
	umx_check_names(c("r", "N"), data = dat, die = T, message = msg)
	if(!"r" %in% colNames){
		"you need a column called 'r' containing the correlations"	
	}
	# ======================================
	# = Calculations to set-up the dataset =
	# ======================================
	### Add z (yi) and the variance of z (vi) to the dataset (needed for calculations; e.g., for the selection models)
	# "ZCOR" is the Fisher's r-to-z transformed correlation coefficient (Fisher, 1921).
	dat = metafor::escalc(measure="ZCOR", ri = r, ni = N, data = dat)

	### Add sei (sqrt of variance of z (vi)
	dat$sei = sqrt(dat$vi)

	### Sort dataset by N (descending)
	dat <- dat[order(-dat$N),] 
	### Calculating cumulative N values
	dat$cumsumN <- cumsum(dat$N)
	dat$cumsumN <- paste0(dat$N," (Ncum = ", dat$cumsumN,")")


	# =================
	# = Meta-analysis =
	# =================
	# = 1. Run naive meta on z scores. Back-transform the results to r.
	# = 2. One-sample removed analysis (osr; aka leave-one-out analysis or loo)
	# = 3. Trim and fill analysis
	# = 4. Contour-enhanced funnel plot (with trim and fill imputations)
	# = 5. A priori selection models (with moderate and severe assumptions of publication bias
	# = 6. PET-PEESE analysis
	# = 7. Cumulative meta-analysis by precision, using SE of Pearson's r (i.e., sample size!)

	# ===================================================================
	# = 1. Run naive meta on z scores. Back-transform the results to r. =
	# ===================================================================
	res       <- rma(measure = "ZCOR", ri = r, ni = N, data = dat, level = 95, method = "DL", slab = dat$cumsumN)
	res_raw_r <- rma(measure = "COR" , ri = r, ni = N, data = dat, level = 95, method = "DL")
	# print(res)

	OUTPUT.naive.RE.k          <- as.numeric(nrow(dat))
	OUTPUT.naive.RE.N          <- sum(dat$N)
	res.ztor.for.ci            <- predict(res, level = 95, transf = transf.ztor)
	OUTPUT.naive.RE.mean.r     <- res.ztor.for.ci$pred
	OUTPUT.naive.RE.95CI.lower <- as.numeric(res.ztor.for.ci$ci.lb)
	OUTPUT.naive.RE.95CI.upper <- as.numeric(res.ztor.for.ci$ci.ub)
	res.ztor.for.pi            <- predict(res, level = 90, transf = transf.ztor)
	OUTPUT.naive.RE.90PI.lower <- as.numeric(res.ztor.for.pi$cr.lb)
	OUTPUT.naive.RE.90PI.upper <- as.numeric(res.ztor.for.pi$cr.ub)
	OUTPUT.naive.RE.QE         <- res$QE
	OUTPUT.naive.RE.I2         <- res$I2
	OUTPUT.naive.RE.tau        <- sqrt(res$tau2)
	# End of the naive meta-analysis syntax
	
	# ===========================================================================
	# = 2. One-sample removed analysis (osr; aka leave-one-out analysis or loo) =
	# ===========================================================================

	re.osr            <- leave1out(res)
	OUTPUT.osr.min    <- transf.ztor(min(re.osr$estimate))
	OUTPUT.osr.max    <- transf.ztor(max(re.osr$estimate))
	OUTPUT.osr.median <- transf.ztor(median(re.osr$estimate))
	# End of one-sample removed syntax

	# =============================
	# = 3. Trim and fill analysis =
	# =============================

	### Fixed-effects trim and fill
	res.tf.fe              <- rma(measure = "ZCOR", ri = r, ni = N, data = dat, method = "FE") ### fit FE model
	res.tf.fe              <- trimfill(res.tf.fe, estimator = "L0") ### trim and fill based on FE model
	fill                   <- res.tf.fe$fill
	OUTPUT.TF.FE.side      <- res.tf.fe$side
	OUTPUT.TF.FE.k.removed <- as.numeric(res.tf.fe$k0)
	res.tf.fe              <- rma(res.tf.fe$yi, res.tf.fe$vi, method = "DL") ### fit RE model based on imputed data
	res.tf.fe$fill         <- fill
	class(res.tf.fe)       <- c("rma.uni.trimfill", "rma")
	# print(res.tf.fe) # BIG!
	res.tf.fe.ztor          <- predict(res.tf.fe, level = 95, transf = transf.ztor)
	OUTPUT.TF.FE.mean.r     <- as.numeric(res.tf.fe.ztor$pred)
	OUTPUT.TF.FE.95CI.lower <- as.numeric(res.tf.fe.ztor$ci.lb)
	OUTPUT.TF.FE.95CI.upper <- as.numeric(res.tf.fe.ztor$ci.ub)


	### Random-effects trim and fill
	res.tf.re              <- rma(measure="ZCOR", ri=r, ni=N, data=dat, level=95, method = "DL")
	res.tf.re              <- trimfill(res.tf.re, estimator = "L0")
	OUTPUT.TF.RE.side      <- res.tf.re$side
	OUTPUT.TF.RE.k.removed <- as.numeric(res.tf.re$k0)	
	res.tf.re              <- rma(res.tf.re$yi, res.tf.re$vi, method = "DL")#fit RE model based on imputed data
	fill                   <- res.tf.re$fill
	res.tf.re$fill         <- fill
	class(res.tf.re) <- c("rma.uni.trimfill", "rma")

	# print(res.tf.re) (BIG)
	res.tf.re.ztor          <- predict(res.tf.re, level=95, transf=transf.ztor)
	OUTPUT.TF.RE.mean.r     <- as.numeric(res.tf.re.ztor$pred)
	OUTPUT.TF.RE.95CI.lower <- as.numeric(res.tf.re.ztor$ci.lb)
	OUTPUT.TF.RE.95CI.upper <- as.numeric(res.tf.re.ztor$ci.ub)
	# End of the trim and fill syntax

	# ====================================================================
	# = 4. Contour-enhanced funnel plot (with trim and fill imputations) =
	# ====================================================================

	# Contour-enhanced funnel plot with fixed-effects trim and fill imputation
	funnel(res.tf.fe, yaxis="seinv", level=c(90, 95), shade=c("white","darkgray"), refline = 0)

	### Contour-enhanced funnel plot with random-effects trim and fill imputation
	# funnel(res.tf.fe, yaxis="seinv", level=c(90, 95), shade=c("white","darkgray"), refline = 0)

	### End of the contour-enhanced funnel plot syntax

	# ==========================================================================================
	# = 5. A priori selection models (with moderate and severe assumptions of publication bias =
	# = Following Vevea and Woods (2005) a priory selection models                             =
	# ==========================================================================================
	# Reference: Vevea, J. L., & Woods, C. M. (2005). Publication bias in research synthesis: Sensitivity analysis using a priori weight functions. Psychological Methods, 10, 428-443. doi: 10.1037/1082-989X.10.4.428
	# Code is adapted from Andy Field's code, available at http://www.statisticshell.com/meta_analysis/pub_bias_r.R

	# ===================
	# = checked to here =
	# ===================

	options(warn = -1)
	myT   <- dat$yi
	n     <- length(myT)
	v     <- dat$vi
	sezr  <- dat$sei
	npred <- 0
	X     <- matrix(c(rep(1, times = n)), n, npred + 1, byrow = TRUE)

	if(length(v) != n){
		stop("Number of conditional variances must equal number of effects.")
	}
	for(i in 1:length(v)){
		if(v[i] <0) stop("Variances must be non-negative.")
	}
	s <- sqrt(v)

	if(dim(X)[1] != n){
		stop("Number of rows in predictor matrix must match number of effects.")
	}

	# Set p = number of predictors
	p <- dim(X)[2] - 1
	# ====================================================================================================================
	# = In this example, cut-points and weights are for the condition denoted "severe two-tailed selection" in the paper. =
	# ====================================================================================================================

	# Enter cut-points for p-value intervals
	a <- c(.005, .010, .050, .100, .250, .350, .500, .650, .750, .900, .950, .990, .995, 1.00)
	# Enter fixed weight function
	w1 <- matrix(c(1.0, .99, .95, .90, .80, .75, .65, .60, .55, .50, .50, .50, .50, .50), ncol = 1)
	w2 <- matrix(c(1.0, .99, .90, .75, .60, .50, .40, .35, .30, .25, .10, .10, .10, .10), ncol = 1)
	w3 <- matrix(c(1.0, .99, .95, .90, .80, .75, .60, .60, .75, .80, .90, .95, .99, 1.0), ncol = 1)
	w4 <- matrix(c(1.0, .99, .90, .75, .60, .50, .25, .25, .50, .60, .75, .90, .99, 1.0), ncol = 1)

	# ===================================
	# = Do not modify below this point! =
	# ===================================
	# Set k = number of intervals
	k <- length(a)

	if(length(w1) != k) stop("Number of weights must match number of intervals")
	if(length(w2) != k) stop("Number of weights must match number of intervals")
	if(length(w3) != k) stop("Number of weights must match number of intervals")
	if(length(w4) != k) stop("Number of weights must match number of intervals")

	# ==============================
	# = Do weighted least squares. =
	# ==============================
	# Don't do this at home; for general regression problems, the algorithm is unstable,
	# but it's easy and unlikely to cause problems in this context.
	varmat <- diag(v)
	tmat   <- matrix(myT,nrow=n)
	beta   <- solve( t(X) %*% solve(varmat) %*% X) %*% t(X) %*% solve(varmat) %*% tmat

	# Set starting value for parameter vector
	params <- beta

	# ==============================
	# = Here we set up Bij and bij =
	# ==============================

	# First, we need predicted values
	xb  <- X %*% beta
	sig <- sqrt(v)

	# =============================
	# = Now bij = - si probit(aj) =
	# =============================
	b <- matrix(rep(0, n * (k - 1)), nrow = n, ncol = k - 1)
	for(i in 1:n){
		for(j in 1:k-1){ 
	    	b[i,j] <- -s[i] * qnorm(a[j])
		}
	}

	# ==========================
	# = BIG BUNCH OF FUNCTIONS =
	# ==========================
	# ============================
	# = And now Bij (Equation 5) =
	# ============================
	Bij <- function(params) {
		B <- matrix(rep(0,n*k),nrow=n,ncol=k)
		beta <- matrix(params[1:(p+1)],ncol=1)
		xb <- X%*%beta
		for(i in 1:n){
			for(j in 1:k) {
				if(j==1){
					B[i,j] <- 1.0 - pnorm( (b[i,1]-xb[i]) / sig[i])
				}else if(j<k){
					B[i,j] <- pnorm( (b[i,j-1]-xb[i]) / sig[i]) - pnorm( (b[i,j]-xb[i]) / sig[i])
				}else{
					B[i,j] <- pnorm( (b[i,k-1] - xb[i]) / sig[i])
	   		 	}
			}
		}
		return(B)
	}

	# ==================================
	# = Unadjusted likelihood function =
	# ==================================
	like1 <- function(params) {
	  beta <- matrix(params[1:(p+1)],ncol = 1)
	  xb <- X %*% beta - 2 * (- 1/2 * (sum( (myT - xb)^2/v ) + sum(log(v))) )
	}

	# ===========================================================================
	# = Adjusted likelihood function Moderate One-Tailed Selection (Equation 6) =
	# ===========================================================================
	like2 <- function(params) {
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb   <- X%*%beta
	  ll   <- -1/2 * (sum( (myT-xb)^2/v ) + sum(log(v))) 
	  B    <- Bij(params)
	  Bw   <- B%*%w1
	  ll   <- ll - sum(log(Bw))
	  return(-2 * ll)
	}

	# =========================================================================
	# = Adjusted likelihood function Severe One-Tailed Selection (Equation 6) =
	# =========================================================================
	like3 <- function(params) {
	  beta <- matrix(params[1:(p+1)], ncol = 1)
	  xb <- X %*% beta
	  ll <- -1/2 * (sum( (myT-xb)^2/v ) + sum(log(v)))   
	  B <- Bij(params)
	  Bw <- B %*% w2
	  ll <- ll - sum(log(Bw))
	  return(ll)
	}

	# ===========================================================================
	# = Adjusted likelihood function Moderate Two-Tailed Selection (Equation 6) =
	# ===========================================================================
	like4 <- function(params) {
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb <- X%*%beta
	  ll <- -1/2 * (sum( (myT-xb)^2/v ) + sum(log(v)))   
	  B <- Bij(params)
	  Bw <- B%*%w3
	  ll <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	# =========================================================================
	# = Adjusted likelihood function Severe Two-Tailed Selection (Equation 6) =
	# =========================================================================
	like5 <- function(params) {
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb <- X%*%beta
	  ll <- -1/2 * (sum( (myT-xb)^2/v ) + sum(log(v)))   
	  B <- Bij(params)
	  Bw <- B%*%w4
	  ll <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	# =========================================
	# = Second derivatives, unadjusted model  =
	# =========================================
	fese <- function(params) {
		beta <- matrix(params[1:(p+1)],ncol=1)
		xb <- X%*%beta
		hessian <- matrix(rep(0,(p+1)*(p+1)),p+1,p+1)
		for(i in 1:n) {
		  for(j in 1:(p+1)){
			for(m in 1:(p+1)){
	        	hessian[j,m] <- hessian[j,m] - X[i,j]*X[i,m]/v[i]
			}
	  	}
	  }
	  temp   <- solve(hessian)
	  retval <- rep(0,(p+1))
	  for(i in 1:(p+1)){
	  	retval[i] <- sqrt(-temp[i,i])
	  }
	  return(retval)
	}

	# Fit unweighted model by maximum likelihood
	fe1 <- nlminb(objective = like1, start = params)
	fe1$par[2] <- (exp(2 * fe1$par[1])-1)/(exp(2 * fe1$par[1]) + 1)

	# ====================================================
	# = Fit adjusted model Moderate one-Tailed Selection =
	# ====================================================
	fe2 <- nlminb(objective = like2, start = fe1$par)
	fe2$par[2] <- (exp(2 * fe2$par[1]) - 1)/(exp(2 * fe2$par[1]) + 1)

	# ==================================================
	# = Fit adjusted model Severe one-Tailed Selection =
	# ==================================================
	fe3 <- nlminb(objective = like3, start = fe1$par)
	fe3$par[2] <- (exp(2 * fe3$par[1]) - 1)/(exp(2 * fe3$par[1]) + 1)

	# ====================================================
	# = Fit adjusted model Moderate Two-Tailed Selection =
	# ====================================================
	fe4 <- nlminb(objective=like4, start=fe1$par)
	fe4$par[2] <- (exp(2*fe4$par[1])-1)/(exp(2*fe4$par[1])+1)

	# ==================================================
	# = Fit adjusted model Severe Two-Tailed Selection =
	# ==================================================
	fe5 <- nlminb(objective=like5, start=fe1$par)
	fe5$par[2] <- (exp(2*fe5$par[1])-1)/(exp(2*fe5$par[1])+1)

	# ====================================================================
	# = Also, set variance component to balanced method of moments start =
	# ====================================================================
	rss <- sum((myT-X%*%beta)^2)
	vc <- rss/(n-p-1) - mean(v)
	if (vc < 0.0) vc <- 0.0

	# ===========================================
	# = Set starting value for parameter vector =
	# ===========================================
	params <- c(beta,vc)

	# ====================================================
	# = Here we set up Bij and bij for the second time...=
	# ====================================================

	# Bunch of duplicated function step on the ones created above...
	# First, we need predicted values
	xb  <- X%*%beta
	sig <- sqrt(v + vc)

	# Now bij = -si probit(aj)
	b <- matrix(rep(0, n * (k - 1)), nrow = n, ncol = k - 1)
	for(i in 1:n){
		for(j in 1:k - 1){
			b[i, j] <- -s[i] * qnorm(a[j])
		}
	}

	# ============================
	# = And now Bij (Equation 5) =
	# ============================
	Bij <- function(params) {
		B    <- matrix(rep(0, n * k), nrow = n, ncol = k)
		beta <- matrix(params[1:(p+1)], ncol = 1)
		xb   <- X %*% beta
		for(i in 1:n){
			for(j in 1:k) {
				if(j == 1){
	      	  		B[i,j] <- 1.0 - pnorm((b[i, 1] - xb[i]) / sig[i])
				} else if(j < k){
					B[i,j] <- pnorm( (b[i, j - 1] - xb[i]) / sig[i]) - pnorm( (b[i, j] - xb[i]) / sig[i])
				} else {
					B[i,j] <- pnorm( (b[i,k-1] - xb[i]) / sig[i])
				}
			}
		}
		return(B)
	}

	# ==================================
	# = Unadjusted likelihood function =
	# ==================================
	like1 <- function(params) {
	  vc   <- params[p + 2]
	  beta <- matrix(params[1:(p + 1)], ncol = 1)
	  xb   <- X %*% beta
	  vall <- v + vc
	  return(-2 * (-1/2 * (sum((myT - xb)^2 / vall ) + sum(log(vall))) ))
	}

	# ===========================================================================
	# = Adjusted likelihood function Moderate One-Tailed Selection (Equation 6) =
	# ===========================================================================
	like2 <- function(params) {
	  vc   <- params[p+2]
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb   <- X%*%beta
	  vall <- v+vc
	  ll   <- -1/2 * (sum( (myT-xb)^2/vall ) + sum(log(vall))) 
	  B    <- Bij(params)
	  Bw   <- B%*%w1
	  ll   <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	#
	# Adjusted likelihood function Severe One-Tailed Selection (Equation 6)
	like3 <- function(params) {
	  vc   <- params[p+2]
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb   <- X%*%beta
	  vall <- v+vc
	  ll   <- -1/2 * (sum( (myT-xb)^2/vall ) + sum(log(vall))) 
	  B    <- Bij(params)
	  Bw   <- B%*%w2
	  ll   <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	#
	# Adjusted likelihood function Moderate two-Tailed Selection (Equation 6)
	like4 <- function(params) {
	  vc   <- params[p+2]
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb   <- X%*%beta
	  vall <- v+vc
	  ll   <- -1/2 * (sum( (myT-xb)^2/vall ) + sum(log(vall))) 
	  B    <- Bij(params)
	  Bw   <- B%*%w3
	  ll   <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	#
	# Adjusted likelihood function Severe Two-Tailed Selection (Equation 6)
	like5 <- function(params) {
	  vc   <- params[p+2]
	  beta <- matrix(params[1:(p+1)],ncol=1)
	  xb   <- X%*%beta
	  vall <- v+vc
	  ll   <- -1/2 * (sum( (myT-xb)^2/vall ) + sum(log(vall))) 
	  B    <- Bij(params)
	  Bw   <- B%*%w4
	  ll   <- ll - sum(log(Bw))
	  return(-2*ll)
	}

	# =========================================
	# = Second derivatives, unadjusted model  =
	# =========================================
	rese <- function(params) {
	  vc      <- params[p+2]
	  beta    <- matrix(params[1:(p+1)],ncol=1)
	  xb      <- X%*%beta
	  vall    <- v+vc
	  hessian <- matrix(rep(0,(p+2)*(p+2)),p+2,p+2)
	  for(i in 1:n) {
	    for(j in 1:(p + 1)){
	      for(m in 1:(p + 1)){ 
	        hessian[j, m] <- hessian[j, m] - X[i, j] * X[i, m]/vall[i]
			}
		}
	    for(j in 1:(p+1)) {
	      hessian[j, p + 2] <- hessian[j,p+2] - (myT[i]-xb[i])/vall[i]/vall[i]
	      hessian[p + 2, j] <- hessian[p+2,j] - (myT[i]-xb[i])/vall[i]/vall[i]
	    }
	    hessian[p+2,p+2] <- hessian[p+2,p+2] + 1/vall[i]/vall[i]/2 - (myT[i]-xb[i])*(myT[i]-xb[i])/vall[i]/vall[i]/vall[i]
	  }
	  temp   <- solve(hessian)
	  retval <- rep(0,(p+2))
	  for(i in 1:(p+2)){
	  	 retval[i] <- sqrt(-temp[i,i])
	  }
	  return(retval)
	}

	# ==============================================
	# = Fit unadjusted model by maximum likelihood =
	# ==============================================
	re1 <- nlminb(objective=like1, start=params,lower=c(rep(-Inf,p+1),0.0))
	#se1 <- rese(re1$par)
	re1$par[3] <- (exp(2*re1$par[1])-1)/(exp(2*re1$par[1])+1)

	# Fit adjusted model Moderate one-Tailed Selection
	re2 <- nlminb(objective=like2, start=re1$par,lower=c(rep(-Inf,p+1),0.0))
	re2$par[3] <- (exp(2*re2$par[1])-1)/(exp(2*re2$par[1])+1)

	# Fit adjusted model Severe one-Tailed Selection
	re3 <- nlminb(objective=like3, start=re1$par,lower=c(rep(-Inf,p+1),0.0))
	re3$par[3] <- (exp(2*re3$par[1])-1)/(exp(2*re3$par[1])+1)

	# Fit adjusted model Moderate Two-Tailed Selection
	re4 <- nlminb(objective=like4, start=re1$par,lower=c(rep(-Inf,p+1),0.0))
	re4$par[3] <- (exp(2*re4$par[1])-1)/(exp(2*re4$par[1])+1)

	# Fit adjusted model Severe Two-Tailed Selection
	re5 <- nlminb(objective=like5, start=re1$par,lower=c(rep(-Inf,p+1),0.0))
	re5$par[3] <- (exp(2*re5$par[1])-1)/(exp(2*re5$par[1])+1)

	unadjfe <- rbind(unadjfe1 <- fe1$par, unadjfe2 <- fese(fe1$par))
	unadjfe[2,2] = NA
	rownames(unadjfe) <- c("Parameter Estimates", "Standard errors                  ")
	colnames(unadjfe) <- c("zr", "r")

	unadjre <- rbind(unadjre1 <- re1$par, unadjre2 <- c(rese(re1$par),NA))
	rownames(unadjre) <- c("Parameter Estimates", "Standard errors                  ")
	colnames(unadjre) <- c("zr", "v", "r")

	adjfe <- rbind(adj1 <- fe2$par, adj2 <- fe3$par, adj3 <- fe4$par, adj4 <- fe5$par)
	rownames(adjfe) <- c("Moderate One-Tailed Selection    ", "Severe One-Tailed Selection", "Moderate Two-Tailed Selection", "Severe Two-Tailed Selection")
	colnames(adjfe) <- c("zr", "r")

	adjre <- rbind(adj1 <- re2$par, adj2 <- re3$par, adj3 <- re4$par, adj4 <- re5$par)
	rownames(adjre) <- c("Moderate One-Tailed Selection    ", "Severe One-Tailed Selection", "Moderate Two-Tailed Selection", "Severe Two-Tailed Selection")
	colnames(adjre) <- c("zr", "v", "r")

	#funnel(myT, sezr, xlim=NULL, ylim=NULL, xlab="Effect Size (Zr)", ylab="Standard Error",comb.f=FALSE, axes=TRUE, pch=1, text=NULL, cex=1.5, col=1,log="", yaxis="se", sm=NULL,level=.95)

	out1 <- paste("**********  SENSITIVITY OF EFFECT-SIZE ESTIMATES TO PUBLICATION BIAS  **********")
	out1 <- paste(out1, "EFFECT-SIZE PARAMETER:   Correlation")
	out1 <- paste(out1, "EFFECT-SIZE ESTIMATOR:   r ")     
	out1 <- paste(out1, "FIXED-EFFECTS Publication Bias Model: Vevea & Woods (2005), Psychological Methods") 
	out1 <- paste(out1, "Unadjusted Parameter Estimate")
	out2 <- paste("Adjusted Parameter Estimate")
	out3 <- paste("RANDOM-EFFECTS Publication Bias Model: Vevea & Woods (2005), Psychological Methods")
	out3 <- paste(out3, "In this model v estimates population effect-size variance")
	out3 <- paste(out3, "Unadjusted Parameter Estimates")
	out4 <- paste("Adjusted Parameter Estimates")

	cat(out1); print(unadjfe); 
	cat(out2); print(adjfe); 
	cat(out3); print(unadjre); 
	cat(out4); print(adjre)

	OUTPUT.SelMod.1tmod.est.zr  <- adjre[1, 1]
	OUTPUT.SelMod.1tmod.est.var <- adjre[1, 2]
	OUTPUT.SelMod.1tmod.est.r   <- adjre[1, 3]
	OUTPUT.SelMod.1tsev.est.zr  <- adjre[2, 1]
	OUTPUT.SelMod.1tsev.est.var <- adjre[2, 2]
	OUTPUT.SelMod.1tsev.est.r   <- adjre[2, 3]

	### End of the selection model syntax
	
	# =========================
	# = 6. PET-PEESE analysis =
	# =========================
	# Based on Stanley and Doucouliagos (2014)
	# Reference: Stanley, T. D., & Doucouliagos, H. (2014). Meta-regression approximations to reduce publication selection bias. Research Synthesis Methods, 5, 60-78. doi: 10.1002/jrsm.1095

	# ==========================================================================
	# = Calculate se of correlation, variance, and precision, and precision_sq =
	# ==========================================================================
	dat$FisherZ   <- 0.5 * log((1 + dat$r) / (1 - dat$r))
	dat$FisherZSE <- 1 / (sqrt(dat$N - 3))
	dat$r_se      <- (1 - (dat$r^ 2)) * dat$FisherZSE

	dat$r_variance     <- dat$r_se^2
	dat$r_precision    <- 1/dat$r_se
	dat$r_precision_sq <- dat$r_precision^2
	reg1               <- lm(r ~ r_se, weights = r_precision_sq, data = dat)
	summary(reg1)
	OUTPUT_PET <- reg1$coefficients[1]

	### Intercept
	OUTPUT_PET_pval <- (coef(summary(reg1))[1, 4]) / 2
	reg2            <- lm(r ~ r_variance, weights = r_precision_sq, data = dat)
	OUTPUT_PEESE    <- reg2$coefficients[1]
	PETPEESEFINAL   <- ifelse(OUTPUT_PET_pval < .05, OUTPUT_PEESE, OUTPUT_PET)
	# End of the PET-PEESE syntax

	# ==========================================================================================
	# = 7. Cumulative meta-analysis by precision, using SE of Pearson's r (i.e., sample size!) =
	# ==========================================================================================

	res.cma <- cumul(res, order=order(dat$vi))
	# print(res.cma)
	forestOUTPUT <- forest(res.cma, transf=transf.ztor)

	### Estimate of the five most precise samples (in the r-metric)
	res.cma <- cumul(res, order = order(dat$vi), transf = transf.ztor)
	OUTPUT.CMA.5thEstimateTemp <- res.cma[5, 0]
	OUTPUT.CMA.5thEstimate     <- OUTPUT.CMA.5thEstimateTemp$estimate
	class(OUTPUT.CMA.5thEstimate)
	# End of the cumulative meta-analysis syntax

	# =============================================================================
	# = THE FOLLOWING IS CODE FOR Excess Significance of Correlation effect sizes =
	# =============================================================================
	rho   <- OUTPUT.naive.RE.mean.r
	print(paste0("rho: ", rho))

	# ==========================================================================================================
	# = Call statistical significance function to add the statistical significance for each r to the dataframe =
	# ==========================================================================================================
	dat$stat.sig <- Statistical.Sig.r(r = dat$r, n = dat$N)

	# ====================================================================================================
	# = Create a dichotomous variable in the dataframe that equals 1 if the significance value <- to .05 =
	# ====================================================================================================
	dat$significant <- 0  # initialize
	dat$significant[dat$stat.sig <= .05] <- 1
	# ==============================================================
	# = Count the number of statistically significant correlations =
	# ==============================================================
	observed.significant = sum(dat$significant)

	# =========================================================================================================
	# = Calculate the post-hoc power of each study. rho is the point estimate from the meta-analysis. p = .05 =
	# =========================================================================================================
	power <- pwr.r.test(n = dat$N, r = rho, sig.level = .05, power = NULL, alternative = prw.r.alternative)
	# add the power of each study to the dataframe
	dat$power <- power$power

	# Calculate the expected number of significant studies
	expected.significant = sum(dat$power)

	#  Define k as the number of rows in the dataframe
	k = nrow(dat)

	# Calculate chi-square p 246 Ioannidis & Trikalinos (2007)
	chi.square <- (((observed.significant - expected.significant)^2) / expected.significant) + 
	  (((observed.significant - expected.significant)^2) / (k - expected.significant))
	  ## Chi-square statistical significance
	p.sig.chi.square <-  1 - pchisq(chi.square, 1) 

	print(paste0("chi-square (Ioannidis & Trikalinos, 2007, p 246:", chi.square, ", p = ", p.sig.chi.square))
	# # TODO (U+03A7

	# ===================================
	# = binomial test for 10 or greater =
	# ===================================
	mean.power = mean(dat$power)
	print(paste0("mean.power: ", mean.power))

	# =======================================================================================================================
	# = probability vector contains the probabilities of obtaining a significant value for the observed through the total k =
	# =======================================================================================================================
	prob.vector <- dbinom(x = observed.significant:k, size = k, prob = mean.power)
	# A loop to sum the probability vector
	binomial.probability <- 0  #initialize the counter
	# get length of vector
	length.of.prob.vector <- length (prob.vector)
	for (i in 1:length.of.prob.vector){
	  binomial.probability <- binomial.probability + prob.vector[i]
	}
	fisher.exact.test.probability <- TestSetProbabilty(observed.significant, dat$power)

		# "---------- Summary of Excess Significance Results----------",
# cat("Estimate of Rho", rho, "\n")
tableOut[[ "** Summary of Excess Significance Analysis **" ]]  <- "-------------"
tableOut[[ "** Number of Correlations (k) **" ]]               <- k
tableOut[[ "Statistically Significant Results Observed" ]]     <- sprintf("%4.2f", observed.significant)
tableOut[[ "Statistically Significant Results Expected" ]]     <- sprintf("%4.2f", expected.significant)
tableOut[[ "Chi-square test probability of these results" ]]   <- sprintf("%4.2f", p.sig.chi.square)
tableOut[[ "Binomial test probability of these results" ]]     <- sprintf("%4.2f", binomial.probability)
tableOut[[ "Fisher exact test probability of these results" ]] <- sprintf("%4.2f", fisher.exact.test.probability)

	##########  End of Excess Significance Analysis ##########################

	# ========================
	# = Create results table =
	# ========================
tableOut <- list()
tableOut[[ "Title" ]]                                    <- Output.Title
tableOut[[ "k (number of effect sizes)" ]]               <- OUTPUT.naive.RE.k
tableOut[[ "N (Cumulative samples size)" ]]              <- OUTPUT.naive.RE.N
tableOut[[ "Random effects mean effect size estimate" ]] <- sprintf("%4.2f", OUTPUT.naive.RE.mean.r)
tableOut[[ "95% CI lower value" ]]                       <- sprintf("%4.2f", OUTPUT.naive.RE.95CI.lower)
tableOut[[ "95% CI upper value " ]]                      <- sprintf("%4.2f", OUTPUT.naive.RE.95CI.upper)
tableOut[[ "90% CI lower value" ]]                       <- sprintf("%4.2f", OUTPUT.naive.RE.90PI.lower)
tableOut[[ "90% CI upper value" ]]                       <- sprintf("%4.2f", OUTPUT.naive.RE.90PI.upper)
tableOut[[ "Q statistic" ]]                              <- sprintf("%4.2f", OUTPUT.naive.RE.QE)
tableOut[[ "I squared" ]]                                <- sprintf("%4.2f", OUTPUT.naive.RE.I2)
tableOut[[ "Tau" ]]                                      <- sprintf("%4.2f", OUTPUT.naive.RE.tau)

		# "---------- Leave one out Results----------",
tableOut[[ "**Leave One Out (LOO) results **" ]] <- "-------------"
tableOut[[ "Minimum random effects LOO mean" ]]  <- sprintf("%4.2f", OUTPUT.osr.min)
tableOut[[ "Maximum random effects LOO mean" ]]  <- sprintf("%4.2f", OUTPUT.osr.max)
tableOut[[ "Median random effects LOO mean" ]]   <- sprintf("%4.2f", OUTPUT.osr.median)

		# "---------- Trim & Fill Results----------"
tableOut[[ "**Trim & Fill Results**" ]]                                                        <- "-------------"
tableOut[[ "FE Model: Side of distribution in which data were imputed" ]]                      <- OUTPUT.TF.FE.side
tableOut[[ "FE Model: Number of effects imputed" ]]                                            <- OUTPUT.TF.FE.k.removed
tableOut[[ "FE Model: Trim and fill adjusted mean r" ]]                                        <- sprintf("%4.2f", OUTPUT.TF.FE.mean.r)
tableOut[[ "FE Model: 95% CI lower value of T&F adjusted distribution" ]]                      <- sprintf("%4.2f", OUTPUT.TF.FE.95CI.lower)
tableOut[[ "FE Model: 95% CI upper value of T&F adjusted distribution" ]]                      <- sprintf("%4.2f", OUTPUT.TF.FE.95CI.upper)
tableOut[[ "RE Model: Side of distribution in which data were imputed (with FE imputation)" ]] <- OUTPUT.TF.RE.side
tableOut[[ "RE Model: Number of effects imputed (with FE imputation)" ]]                       <- OUTPUT.TF.RE.k.removed
tableOut[[ "RE Model: Trim and fill adjusted mean r (with FE imputation)" ]]                   <- sprintf("%4.2f", OUTPUT.TF.RE.mean.r)
tableOut[[ "RE Model: 95% CI lower value of T&F adjusted distribution" ]]                      <- sprintf("%4.2f", OUTPUT.TF.RE.95CI.lower)
tableOut[[ "RE Model: 95% CI upper value of T&F adjusted distribution" ]]                      <- sprintf("%4.2f", OUTPUT.TF.RE.95CI.upper)

# "---------- Cumulative Meta-Analysis by Descending N Results ----------",
tableOut[[ "** Cumulative Meta-Analysis by Descending N Results **" ]] <- "-------------"
tableOut[[ "RE Model estimated mean of five largest samples" ]]        <- sprintf("%4.2f", OUTPUT.CMA.5thEstimate)

# "---------- Selection Model Results----------",
tableOut[[ "** Selection Model Results **" ]]                      <- "-------------"
tableOut[[ "Estimated mean r with moderate bias" ]]                <- sprintf("%4.2f", OUTPUT.SelMod.1tmod.est.r)
tableOut[[ "Estimated mean Fisher z with moderate bias" ]]         <- sprintf("%4.2f", OUTPUT.SelMod.1tmod.est.zr)
tableOut[[ "Estimated variance of Fisher z with moderate bias)" ]] <- sprintf("%4.2f", OUTPUT.SelMod.1tmod.est.var)
tableOut[[ "Estimated mean r with severe bias" ]]                  <- sprintf("%4.2f", OUTPUT.SelMod.1tsev.est.r)
tableOut[[ "Estimated mean Fisher z with severe bias" ]]           <- sprintf("%4.2f", OUTPUT.SelMod.1tsev.est.zr)
tableOut[[ "Estimated variance of Fisher z with severe bias" ]]    <- sprintf("%4.2f", OUTPUT.SelMod.1tsev.est.var)

		# "---------- PET-PEESE Results----------",
tableOut[[ "** PET-PEESE Results **" ]]                            <- "-------------"
tableOut[[ "PET value" ]]                                          <- sprintf("%4.2f", OUTPUT_PET)
tableOut[[ "PET p value" ]]                                        <- sprintf("%4.2f", OUTPUT_PET_pval)
tableOut[[ "PEESE value" ]]                                        <- sprintf("%4.2f", OUTPUT_PEESE)
tableOut[[ "PET-PEESE (use this value)" ]]                         <- sprintf("%4.2f", PETPEESEFINAL)

		# "---------- Probability of Excess Significance Results----------",
tableOut[[ "** Probability of Excess Significance Results **" ]]     <- "-------------"
tableOut[[ "Number of Observed Statistically Significant Results" ]] <- sprintf("%4.2f", OUTPUT.PTES.Number.of.Observed.Significant)
tableOut[[ "Number of Expected Statistically Significant Results" ]] <- sprintf("%4.2f", OUTPUT.PTES.Number.of.Expected.Significant)
tableOut[[ "Chi-square test probability of these results" ]]         <- sprintf("%4.2f", OUTPUT.PTES.Chi.square.test.probability)
tableOut[[ "Binomial test probability of these results" ]]           <- sprintf("%4.2f", OUTPUT.PTES.Binomial.test.probability)
tableOut[[ "Fisher exact test probability of these results" ]]       <- sprintf("%4.2f", OUTPUT.PTES.Fisher.exact.test.probability)


	output.dataframe <- data.frame(names(tableOut), unlist(tableOut, use.names = FALSE))
	# =====================================================================
	# = Write table to clipboard than to Excel then can move it into Word =
	# =====================================================================
	umx_write_to_clipboard(output.dataframe)
	cat(paste0(
		    "**************************************************************************\n",
			"Results are copied to clipboard. Open Spreadsheet (e.g., Excel) and paste.\n",
			"**************************************************************************\n"
		)
	)
	# End of results table syntax
}

# This function runs Fisher's exact test to compute the probability of getting the observed or more rejections
# of the null hypothesis from a set of experiments with the indicated power values.
# str(power)
Statistical.Sig.r <- function(r = dat$r, n = dat$N){     
  # Calculating statistical significance with both a t and a z test. reporting the z test
  # Get t and and its significance value
  t = abs(r)/ sqrt((1 - (r * r))/ (n - 2))
  pval.t <- ((1 - (pt(t, 20)))) * 2
  # Convert R to Fisher z, get the z test statistic, get the significance value
  FisherZ = 0.5 * log((1 + r) / (1 - r))
  # print(FisherZ)
  FisherZSE = 1 / (sqrt(n - 3))
  z.test.statistic <- FisherZ/FisherZSE
  # print(z.test.statistic)
  pval.z <- 2 * (1- pnorm(abs(z.test.statistic)))
  # print(pval.z)
  return(pval.z)
}

# Fetch Greg Francis Code for Fisher z test
# function is called TestSetProbability()
# install.packages("combinat")
# library(combinat)
# source("C:/Users/Mike McDaniel/Dropbox/Active McDaniel Research/Excess significance/TestSetProbabilty.R")
# This function runs Fisher's exact test to compute the probability of getting the observed or more rejections
# of the null hypothesis from a set of experiments with the indicated power values.
# Greg Francis (gfrancis@purdue.edu)
# 17 November 2012

TestSetProbabilty <- function(Observed, Power){
  numExperiments = sum(!is.na(Power))
  sumProb = 0
  # Use binomial distribution if power values are identical for all experiments in the set
  if(var(Power) == 0){
    test <- binom.test(Observed, numExperiments, p = Power[1], alternative = "greater")
    sumProb = test$p.value
  } else {
	  # Have to do a Fisher's exact test or a Chi Square test for varying power values
	  # Fisher's exact test for small enough number of experiments
    if(Observed > 0 && Observed < numExperiments && numExperiments <= 15){
      # For Observed and up  
      if(Observed >= (numExperiments/2)){
        BetaValues = (1 - Power)
        prodBeta = prod(BetaValues)
        sumProb = 0
        for(chkObsv in Observed:numExperiments){  		
          # get all possible combinations of rejections
          patterns    = combn(numExperiments, chkObsv)
          numPatterns = length(patterns)/chkObsv
          # go through each pattern and compute probability of that pattern
          sumchkObsvProb = 0
          if(chkObsv > 0){
            for(i in 1:numPatterns){
              probRejectsY = 1
              probRejectsN = 1
              probNReject  = 1
              # probability
              for(j in 1:chkObsv) {
                # product of powers for those that reject
                probRejectsY = probRejectsY * Power[patterns[(i-1)* chkObsv +j]]
                # cat(patterns[(i-1)* chkObsv +j], "\t", Power[patterns[(i-1)* chkObsv +j]], "\n")
                # product of Beta for those that do not reject
                probNReject = probNReject * BetaValues[patterns[(i-1)* chkObsv +j]]
              }
              #	cat("------\n")
              # probability for entire set pattern
              if(probNReject==0){probNReject=1} # in case all experiments reject
              sumchkObsvProb = sumchkObsvProb + prodBeta*probRejectsY/probNReject		
            }
          }
          sumProb = sumProb + sumchkObsvProb
        }
      } else {  # compute observed down and then subtract from one
        BetaValues = 1 - Power
        prodBeta   = prod(BetaValues)
        sumProb    = 0
        for(chkObsv in 0:(Observed - 1)){	
          if(chkObsv > 0){  # 0 is a special case, handled below		
            # get all possible combinations of rejections
            patterns       = combn(numExperiments, chkObsv)
            numPatterns    = length(patterns)/chkObsv
            # go through each pattern and compute probability of that pattern
            sumchkObsvProb = 0
            
            for(i in 1:numPatterns){
              probRejectsY = 1
              probRejectsN = 1
              probNReject  = 1
              # probability
              for(j in 1:chkObsv) {
                # product of powers for those that reject
                probRejectsY = probRejectsY * Power[patterns[(i-1)* chkObsv +j]]
                #		cat(patterns[(i-1)* chkObsv +j], "\t", Power[patterns[(i-1)* chkObsv +j]], "\n")
                # product of Beta for those that do not reject
                probNReject = probNReject * BetaValues[patterns[(i-1)* chkObsv +j]]
              }
              #	cat("------\n")
              # probability for entire set pattern
              if(probNReject==0){probNReject=1} # in case all experiments reject
              sumchkObsvProb = sumchkObsvProb + prodBeta*probRejectsY/probNReject		
            }
          }
          else {  # special case when none reject            
            sumchkObsvProb = prod(BetaValues)
          }
          sumProb = sumProb + sumchkObsvProb
        }		
        sumProb = 1- sumProb		
      }
    }else if (Observed == numExperiments){
      sumProb = prod(Power)
    } else if (Observed == 0){
      sumProb = 1
    } else {
	  # Chi Square test for more than 15 experiments
      Expected = sum(Power)
      A = ((Observed - Expected)^2)/(Expected) + ( (Observed-Expected)^2)/(numExperiments-Expected)
      sumProb <- 1 - pchisq(A, df = 1)
      cat("O=", Observed, " E=", Expected, " A=", A, " p=", sumProb, "\n")
      # Only looking for over-publication, not under-publication
      if(Observed < Expected){
        sumProb = 1
      }
    }
  }
  return(sumProb)
}
# End of function of TestSetProbabilty 


# x = sensitivity_analysis(dat = df, Output.Title = "No Output.Title Provided")
# metafor::influence(model, digits, ...)
