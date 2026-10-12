
# =============================
# = --Standardization Functions =#
# =============================


#' Return a standardized version of a Structural Model
#'
#' umx_standardize takes umx models, including RAM and twin models, and returns a standardized version.
#'
#'
#' @description
#' Return the standardized version of a model (such as ACE, CP etc.)
#'
#' Versions exist for RAM, ACE, ACEv, ACEcov, IP, CP and GxE models.
#'
#' @param model The [OpenMx::mxModel()] whose fit will be reported.
#' @param ... Other parameters.
#' @family xmu internal not for end user
#' @export
umx_standardize <- function(model, ...){
	UseMethod("umx_standardize", model)
}

#' @export
umx_standardize.default <- function(model, ...){
	stop("umx_standardize is not defined for objects of class:", class(model))
}

#' Standardize a Structural Model (not for end users)
#'
#' You probably want [umx_standardize()], not this.
#' 
#' `xmu_standardize_RAM` takes a RAM-style model, and returns standardized version.
#'
#' @param model The [OpenMx::mxModel()] you wish to standardize
#' @param ... Other options
#' @family xmu internal not for end user
#' @references <https://github.com/tbates/umx>
#' @export
#' @examples
#' \dontrun{
#' require(umx)
#' data(demoOneFactor)
#' manifests = names(demoOneFactor)
#'
#' m1 = umxRAM("std_ex", data = demoOneFactor, type = "cov",
#' 	umxPath("G", to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = "G", fixedAt = 1.0)
#' )
#'
#' m1 = xmu_standardize_RAM(m1)
#' m1 = umx_standardize(m1)
#' umxSummary(m1)
#' 
#' }
xmu_standardize_RAM <- function(model, ...) {
	if (!is.null(model$expectation) && class(model$expectation)[[1]] == "MxExpectationLISREL") {
		return(xmu_standardize_LISREL(model, ...))
	}
	umx_check(umx_is_RAM(model), action="stop", message="xmu_standardize_RAM needs a RAM model")
	umx_has_been_run(model)

	# mxStandardizeRAMpaths(model=, SE=, cov=)
	# Get the names of the A, S and M matrices
	nameA = model$expectation$A
	nameS = model$expectation$S
	nameM = model$expectation$M

	# Get the A and S matrices, and make an identity matrix
	A = model[[nameA]]
	S = model[[nameS]]
	I = diag(nrow(S$values))
	
	# this can fail (non-invertible etc. so we wrap it in try-catch)
	tryCatch({	
		# Calculate the expected covariance matrix
		IA = solve(I - A$values)
		expCov = IA %*% S$values %*% t(IA)
		# Return 1/SD to a diagonal matrix
		InvSD = 1/sqrt(diag(expCov))
		# Give the inverse SDs names, because mxSummary treats column names as characters
		names(InvSD) = as.character(1:length(InvSD))
		if (!is.null(dimnames(A$values))){names(InvSD) = as.vector(dimnames(S$values)[[2]])}
		# Put the inverse SDs into a diagonal matrix (might as well recycle I matrix from above)
		diag(I) = InvSD
		# Standardize the A, S and M matrices
		#  A paths are value*sd(from)/sd(to) = I %*% A %*% solve(I)
		#  S paths are value/(sd(from*sd(to))) = I %*% S %*% I
		stdA = I %*% A$values %*% solve(I)
		stdS = I %*% S$values %*% I
		# Populate the model
		model[[nameA]]$values[,] = stdA
		model[[nameS]]$values[,] = stdS
		if (!is.na(nameM)){model[[nameM]]$values[,] = rep(0, length(InvSD))}
	}, warning = function(cond) {
		# warning-handler-code
		message(cond)
	}, error = function(cond) {
		cat("The model could not be standardized")
		message(cond)
	}, finally = {
		# cleanup-code
	})
	# Return the model
	invisible(model)
}
#' @export
umx_standardize.MxModel <- xmu_standardize_RAM

#' xmu_standardize_LISREL
#'
#' Standardize a LISREL model
#'
#' @param model The [OpenMx::mxModel()] you wish to standardize
#' @param ... Other options
#' @family xmu internal not for end user
#' @export

xmu_standardize_LISREL <- function(model, ...) {
	if (class(model$expectation)[[1]] != "MxExpectationLISREL") {
		stop("xmu_standardize_LISREL needs a LISREL model")
	}
	umx_has_been_run(model)

	tryCatch({
		expect <- model$expectation
		
		getMatrixVal <- function(matName) {
			if (is.null(matName) || is.na(matName)) return(NULL)
			mat <- model[[matName]]
			if (is.null(mat)) return(NULL)
			val <- list(mat$values, mat$result)
			val[[which.max(c(length(val[[1]]), length(val[[2]])))]]
		}
		
		LX <- getMatrixVal(expect@LX)
		LY <- getMatrixVal(expect@LY)
		BE <- getMatrixVal(expect@BE)
		GA <- getMatrixVal(expect@GA)
		PH <- getMatrixVal(expect@PH)
		PS <- getMatrixVal(expect@PS)
		TD <- getMatrixVal(expect@TD)
		TE <- getMatrixVal(expect@TE)
		TH <- getMatrixVal(expect@TH)
		TX <- getMatrixVal(expect@TX)
		TY <- getMatrixVal(expect@TY)
		KA <- getMatrixVal(expect@KA)
		AL <- getMatrixVal(expect@AL)
		
		hasX <- !is.null(LX)
		hasY <- !is.null(LY)
		
		if (hasX) {
			SD_xi <- sqrt(diag(PH))
			SD_xi <- ifelse(SD_xi <= 0, 0, SD_xi)
			V_xi <- diag(SD_xi, nrow=length(SD_xi))
			InvV_xi <- diag(ifelse(SD_xi == 0, 0, 1/SD_xi), nrow=length(SD_xi))
		} else {
			V_xi <- matrix(0, 0, 0)
			InvV_xi <- matrix(0, 0, 0)
			SD_xi <- numeric(0)
		}
		
		if (hasY) {
			I_BE <- diag(1, nrow=nrow(BE))
			InvI_BE <- solve(I_BE - BE)
			
			if (hasX) {
				cov_eta <- InvI_BE %*% (GA %*% PH %*% t(GA) + PS) %*% t(InvI_BE)
			} else {
				cov_eta <- InvI_BE %*% PS %*% t(InvI_BE)
			}
			
			SD_eta <- sqrt(diag(cov_eta))
			SD_eta <- ifelse(SD_eta <= 0, 0, SD_eta)
			V_eta <- diag(SD_eta, nrow=length(SD_eta))
			InvV_eta <- diag(ifelse(SD_eta == 0, 0, 1/SD_eta), nrow=length(SD_eta))
		} else {
			V_eta <- matrix(0, 0, 0)
			InvV_eta <- matrix(0, 0, 0)
			SD_eta <- numeric(0)
		}
		
		if (hasY) {
			cov_Y <- LY %*% cov_eta %*% t(LY) + TE
			SD_Y <- sqrt(diag(cov_Y))
			SD_Y <- ifelse(SD_Y <= 0, 0, SD_Y)
			V_Y <- diag(SD_Y, nrow=length(SD_Y))
			InvV_Y <- diag(ifelse(SD_Y == 0, 0, 1/SD_Y), nrow=length(SD_Y))
		} else {
			V_Y <- matrix(0, 0, 0)
			InvV_Y <- matrix(0, 0, 0)
			SD_Y <- numeric(0)
		}
		
		if (hasX) {
			cov_X <- LX %*% PH %*% t(LX) + TD
			SD_X <- sqrt(diag(cov_X))
			SD_X <- ifelse(SD_X <= 0, 0, SD_X)
			V_X <- diag(SD_X, nrow=length(SD_X))
			InvV_X <- diag(ifelse(SD_X == 0, 0, 1/SD_X), nrow=length(SD_X))
		} else {
			V_X <- matrix(0, 0, 0)
			InvV_X <- matrix(0, 0, 0)
			SD_X <- numeric(0)
		}
		
		if (hasY) {
			if (!is.null(model[[expect@LY]])) model[[expect@LY]]$values[,] <- InvV_Y %*% LY %*% V_eta
			if (!is.null(model[[expect@BE]])) model[[expect@BE]]$values[,] <- InvV_eta %*% BE %*% V_eta
			if (!is.null(model[[expect@PS]])) model[[expect@PS]]$values[,] <- InvV_eta %*% PS %*% InvV_eta
			if (!is.null(model[[expect@TE]])) model[[expect@TE]]$values[,] <- InvV_Y %*% TE %*% InvV_Y
			
			if (!is.null(TY)) {
				model[[expect@TY]]$values[,] <- InvV_Y %*% TY
			}
			if (!is.null(AL)) {
				model[[expect@AL]]$values[,] <- InvV_eta %*% AL
			}
		}
		
		if (hasX) {
			if (!is.null(model[[expect@LX]])) model[[expect@LX]]$values[,] <- InvV_X %*% LX %*% V_xi
			if (!is.null(model[[expect@PH]])) model[[expect@PH]]$values[,] <- InvV_xi %*% PH %*% InvV_xi
			if (!is.null(model[[expect@TD]])) model[[expect@TD]]$values[,] <- InvV_X %*% TD %*% InvV_X
			
			if (!is.null(TX)) {
				model[[expect@TX]]$values[,] <- InvV_X %*% TX
			}
			if (!is.null(KA)) {
				model[[expect@KA]]$values[,] <- InvV_xi %*% KA
			}
		}
		
		if (hasX && hasY) {
			if (!is.null(model[[expect@GA]])) model[[expect@GA]]$values[,] <- InvV_eta %*% GA %*% V_xi
			if (!is.null(model[[expect@TH]])) model[[expect@TH]]$values[,] <- InvV_X %*% TH %*% InvV_Y
		}
	}, warning = function(cond) {
		message(cond)
	}, error = function(cond) {
		cat("The model could not be standardized")
		message(cond)
	}, finally = {
	})
	invisible(model)
}

#' @export
umx_standardize.MxLISRELModel <- xmu_standardize_LISREL

#' xmu_standardize_ACE
#'
#' Standardize an ACE model *BUT* you probably want [umx_standardize()].
#'
#' @param model an [umxACE()] model to standardize
#' @param ... Other options
#' @return - Standardized ACE [umxACE()] model
#' @export
#' @family xmu internal not for end user
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' 
#' \dontrun{
#' require(umx)
#' data(twinData)
#' selDVs = c("bmi1", "bmi2")
#' mzData = twinData[twinData$zygosity %in% "MZFF", selDVs]
#' dzData = twinData[twinData$zygosity %in% "DZFF", selDVs]
#' m1     = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' std    = xmu_standardize_ACE(m1)
#' }
xmu_standardize_ACE <- function(model, ...) {
	if(typeof(model) == "list"){ # Call self recursively
		for(thisFit in model) {
			message("Output for Model: ", thisFit$name)
			umx_standardize(thisFit)
		}
	} else {
		if(!umx_has_been_run(model)){
			stop("I can only standardize ACE models that have been run. Just do\n",
			"yourModel = mxRun(yourModel)")
		}
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar <- length(selDVs)/2;
		# Calculate standardized variance components
		a  <- mxEval(top.a, model); # Path coefficients
		c  <- mxEval(top.c, model);
		e  <- mxEval(top.e, model);

		A  <- mxEval(top.A, model); # Variances
		C  <- mxEval(top.C, model);
		E  <- mxEval(top.E, model);
		Vtot = A + C + E;           # Total variance
		I  <- diag(nVar);           # nVar Identity matrix
		SD <- solve(sqrt(I * Vtot)) # Inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	
		# Standardized _path_ coefficients ready to be stacked together
		model$top$a$values = SD %*% a; # Standardized path coefficients
		model$top$c$values = SD %*% c;
		model$top$e$values = SD %*% e;
		return(model)
	}
}
#' @export
umx_standardize.MxModelACE <- xmu_standardize_ACE

#' xmu_standardize_ACEcov
#'
#' Standardize an ACE model with covariates
#'
#' @param model an [umxACEcov()] model to standardize
#' @param ... Other options
#' @return - Standardized [umxACEcov()] model
#' @export
#' @family xmu internal not for end user
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' require(umx)
#' data(twinData)
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs  = c("bmi")
#' selCovs = c("ht") # silly example
#' selVars = umx_paste_names(c(selDVs, selCovs), sep = "", suffixes= 1:2)
#' mzData = subset(twinData, zyg == 1, selVars)[1:80, ]
#' dzData = subset(twinData, zyg == 3, selVars)[1:80, ]
#' m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, 
#' 	 sep = "", autoRun = TRUE)
#' fit = xmu_standardize_ACEcov(m1)
#' }
xmu_standardize_ACEcov <- function(model, ...) {
	if(typeof(model) == "list"){ # call self recursively
		for(thisFit in model) {
			message("Output for Model: ",thisFit$name)
			umx_standardize(thisFit)
		}
	} else {
		if(!umx_has_been_run(model)){
			stop("I can only standardize models that have been run. Just do\n",
			"yourModel = mxRun(yourModel)")
		}
		if(!is.null(model$top$a_std)){
			# Standardized general path components
			model$top$a$values = model$top$a_std$result # standardized a
			model$top$c$values = model$top$c_std$result # standardized c
			model$top$e$values = model$top$e_std$result # standardized e
		} else {
			stop("Please run umxACEcov(..., std = TRUE). All I do is copy a_std values into a..., so model has to have been run!")
		}
		return(model)
	}
}

#' @export
umx_standardize.MxModelACEcov <- xmu_standardize_ACEcov

#' Standardize a SexLim model
#'
#' `xmu_standardize_SexLim` would move standardized Sexlim values into raw cells, but can't as these are algebras.
#'
#' @param model an [umxSexLim()] model to standardize
#' @param ... Other options
#' @return - standardized [umxSexLim()] model
#' @export
#' @family xmu internal not for end user
#' @examples
#' \dontrun{
#' model = xmu_standardize_SexLim(model)
#' }
xmu_standardize_SexLim <- function(model, ...){
	stop("xmu_standardize_SexLim doesn't work as Am etc. are algebras")
	# 'AmStd', 'CmStd', 'EmStd',
	# 'AfStd', 'CfStd', 'EfStd',
	# 'Am', 'Cm', 'Em',
	# 'Af', 'Cf', 'Ef',
	# 'Amf', 'Cmf',
	# 'minCor',
	# 'Vm', 'Vf',
	# 'iSDm', 'iSDf',

	if(umx_has_been_run(model)){
		# Standardized ACEm
		model$top$Am$result = model$top$AmStd$result # standardized Am
		model$top$Cm$result = model$top$CmStd$result # standardized Cm
		model$top$Em$result = model$top$EmStd$result # standardized Em

		# Standardized ACEm
		model$top$Af$result = model$top$AfStd$result # standardized Af
		model$top$Cf$result = model$top$CfStd$result # standardized Cf
		model$top$Ef$result = model$top$EfStd$result # standardized Ef

	} else {
		stop("Please run umxSexLim(..., std = TRUE). All I do is copy the values from the standardized matrices already in the model..., so they have to be run!")
	}
	return(model)
}
#' @export
umx_standardize.MxModelSexLim <- xmu_standardize_SexLim


#' non-user: Standardize an IP model
#'
#' You probably want [umx_standardize()]. This function simply copies the standardized IP components into the ai ci ei and as cs es matrices
#'
#' @param model an [umxIP()] model to standardize
#' @param ... Other options
#' @return - standardized IP [umxIP()] model
#' @export
#' @family xmu internal not for end user
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' model = xmu_standardize_IP(model)
#' }
xmu_standardize_IP <- function(model, ...){
	if(!is.null(model$top$ai_std)){
		# Standardized general path components
		model$top$ai$values = model$top$ai_std$result # standardized ai
		model$top$ci$values = model$top$ci_std$result # standardized ci
		model$top$ei$values = model$top$ei_std$result # standardized ei
		# Standardized specific coefficients
		model$top$as$values = model$top$as_std$result # standardized as
		model$top$cs$values = model$top$cs_std$result # standardized cs
		model$top$es$values = model$top$es_std$result # standardized es
	} else {
		stop("Please run umxIP(..., std = TRUE). All I do is copy ai_std values into ai..., so they have to be run!")
	}
	return(model)
}

#' @export
umx_standardize.MxModelIP <- xmu_standardize_IP

#' Function to standardize a common pathway model
#'
#' You probably want [umx_standardize()]. This function simply inserts the standardized CP components into the ai ci ei and as cs es matrices
#'
#' @param model an [umxCP()] model to standardize
#' @param ... Other options
#' @return - standardized [umxCP()] model
#' @export
#' @family xmu internal not for end user
#' @references <https://tbates.github.io>,  <https://github.com/tbates/umx>
#' @examples
#' \dontrun{
#' selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
#' m1 = umxCP(selDVs = selDVs, nFac = 3, data=GFF, zyg="zyg_2grp")
#' m2 = xmu_standardize_CP(m1)
#' }
xmu_standardize_CP <- function(model, ...){
	if(!is.null(model$top$as_std)){
		# Standardized general path components
		# Standardized cp loadings
		model@submodels$top$cp_loadings@values = model$top$algebras$cp_loadings_std$result 
		# Standardized specific path coefficients
		model@submodels$top$as@values = model$top$as_std$result # standardized as
		model@submodels$top$cs@values = model$top$cs_std$result # standardized cs
		model@submodels$top$es@values = model$top$es_std$result # standardized es
		return(model)
	} else {
		selDVs = dimnames(model$top.expCovMZ)[[1]]
		nVar   = length(selDVs)/2;
		nFac   = dim(model$top$matrices$a_cp)[[1]]	
		# Calculate standardized variance components
		a_cp = mxEval(top.a_cp , model); # nFac * nFac path matrix flowing into cp_loadings array
		c_cp = mxEval(top.c_cp , model);
		e_cp = mxEval(top.e_cp , model);
		as = mxEval(top.as, model); # Specific factor path coefficients
		cs = mxEval(top.cs, model);
		es = mxEval(top.es, model);
		cp_loadings = mxEval(top.cp_loadings, model); # nVar * nFac matrix
		A  = mxEval(top.A, model);  # Variances
		C  = mxEval(top.C, model);
		E  = mxEval(top.E, model);
		Vtot = A + C + E; # total variance
		nVarIden = diag(nVar)
		SD       = solve(sqrt(nVarIden * Vtot)); # inverse of diagonal matrix of standard deviations  (in classic MX -> "(\sqrt(I.Vtot))~"
		# Standardize loadings on Common factors
		std_commonLoadings = SD %*% cp_loadings; # Standardized path coefficients (general factor(s))
		as_std = SD %*% as; # Standardized path coefficients (nVar specific factors matrices)
		cs_std = SD %*% cs;
		es_std = SD %*% es;
		# Standardized common and specific path coefficients
		model$top$cp_loadings$values = std_commonLoadings # standardized cp loadings
		model$top$as$values = as_std # standardized as
		model$top$cs$values = cs_std # standardized cs
		model$top$es$values = es_std # standardized es
		return(model)
	}
}

#' @export
umx_standardize.MxModelCP <- xmu_standardize_CP

