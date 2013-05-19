

umxUnexplainedCausalNexus = function(IV_from, delta, DV_to, model) {
	# umxUnexplainedCausalNexus(IV_from, delta, DV_to, model)
	manifests = model@manifestVars
	partialDataRow <- matrix(0, 1, length(manifests))  # add dimnames to support string varnames 
	dimnames(partialDataRow) = list("val", manifests)
	partialDataRow[1, IV_from] <- delta                # delta is in raw IV_from units
	partialDataRow[1, DV_to]   <- NA
	completedRow <- umxConditionalsFromModel(model, partialDataRow, meanOffsets=TRUE)
	# by default, meanOffsets = FALSE, and the results take expected means into account
	return(completedRow[1, DV_to])
}

umxComputeConditionals <- function(sigma, mu, current, onlyMean = F) {
	# http://openmx.psyc.virginia.edu/thread/2076
	# Usage:
	# Result <- conditionalsFromModel(model, newData)

	# Result is a replica of the newData data frame with missing values and (if a RAM model) latent variables populated.
	require(OpenMx)

	if(dim(mu)[1] > dim(mu)[2] ) {
		mu <- t(mu)
	}

	nVar <- length(mu)
	vars <- colnames(sigma)

	if(!is.matrix(current)) {
		current <- matrix(current, 1, length(current), dimnames=list(NULL, names(current)))
	}
	
	# Check inputs
	if(dim(sigma)[1] != nVar || dim(sigma)[2] != nVar) {
		stop("Non-conformable sigma and mu matrices in conditional computation.")
	}
	
	if(is.null(vars)) {
		vars <- rownames(sigma)
		if(is.null(vars)) {
			vars <- colnames(mu)
			if(is.null(vars)) {
				vars <- names(current)
				if(is.null(vars)) {
					vars <- paste("X", 1:dim(sigma)[1], sep="")
					names(current) <- vars
				}
				names(mu) <- vars
			}
			dimnames(sigma) <- list(vars, vars)
		}
		rownames(sigma) <- vars
	}
	
	if(is.null(colnames(sigma))) {
		colnames(sigma) <- vars
	}
	
	if(is.null(rownames(sigma))) {
		rownames(sigma) <- colnames(sigma)
	}

	if(!setequal(rownames(sigma), colnames(sigma))) {
		stop("Rows and columns of sigma do not match in conditional computation.")
	}
	
	if(!setequal(rownames(sigma), vars) || !setequal(colnames(sigma), vars)) {
		stop("Names of covariance and means in conditional computation fails.")
	}
	
	if(length(current) == 0) {
		if(onlyMean) {
			return(mu)
		}
		return(list(sigma=covMat, mu=current))
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			print(paste("Got data vector of length ", ncol(current), " and names of length ", length(vars)))
			stop("Length and names of current values mismatched in conditional computation.")
		}
		names(current) <- vars[1:ncol(current)]
	}
	
	if(is.null(names(current))) {
		if(length(vars) == 0 || ncol(current) != length(vars)) {
			if(length(vars) == 0 || ncol(current) != length(vars)) {
				print(paste("Got mean vector of length ", ncol(current), " and names of length ", length(vars)))
				stop("Length and names of mean values mismatched in conditional computation.")
			}
		}
		names(mu) <- vars
	}
	
	# Get Missing and Non-missing sets
	if(!setequal(names(current), vars)) {
		newSet <- setdiff(vars, names(current))
		current[newSet] <- NA
		current <- current[vars]
	}
	
	# Compute Schur Complement
	# Calculate parts:
	missing <- names(current[is.na(current)])
	nonmissing <- setdiff(vars, missing)
	ordering <- c(missing, nonmissing)
	
	totalCondCov <- NULL

	# Handle all-missing and none-missing cases
	if(length(missing) == 0) {
		totalMean = current
		names(totalMean) <- names(current)
		totalCondCov = sigma
	} 

	if(length(nonmissing) == 0) {
		totalMean = mu
		names(totalMean) <- names(mu)
		totalCondCov = sigma
	}

	# Compute Conditional expectations
	if(is.null(totalCondCov)) {
		
		covMat <- sigma[ordering, ordering]
		missMean <- mu[, missing]
		haveMean <- mu[, nonmissing]

		haves <- current[nonmissing]
		haveNots <- current[missing]

		missCov <- sigma[missing, missing]
		haveCov <- sigma[nonmissing, nonmissing]
		relCov <- sigma[missing, nonmissing]
		relCov <- matrix(relCov, length(missing), length(nonmissing))

		invHaveCov <- solve(haveCov)
		condMean <- missMean + relCov %*% invHaveCov %*% (haves - haveMean)

		totalMean <- current * 0.0
		names(totalMean) <- vars
		totalMean[missing] <- condMean
		totalMean[nonmissing] <- current[nonmissing]
	}

	if(onlyMean) {
		return(totalMean)
	}
	
	if(is.null(totalCondCov)) {
		condCov <- missCov - relCov %*% invHaveCov %*% t(relCov)
	
		totalCondCov <- sigma * 0.0
		totalCondCov[nonmissing, nonmissing] <- haveCov
		totalCondCov[missing, missing] <- condCov
	}
	
	return(list(sigma=totalCondCov, mu=totalMean))
	
}

umxConditionalsFromModel <- function(model, newData=NULL, returnCovs=FALSE, meanOffsets = FALSE) {
	# TODO:  Special case for latent variables
	# FIXME: Update for fitfunction/expectation
	expectation <- model$objective
	A <- NULL
	S <- NULL
	M <- NULL
	
	# Handle missing data
	if(is.null(newData)) {
		data <- model$data
		if(data@type != "raw") {
			stop("Conditionals requires either new data or a model with raw data.")
		}
		newData <- data@observed
	}
	
	if(is.list(expectation)) {  # New fit-function style
		eCov <- model$fitfunction@info$expCov
		eMean <- model$fitfunction@info$expMean
		expectation <- model$expectation
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X@values, list(X=expectation@A)))
			S <- eval(substitute(model$X@values, list(X=expectation@S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation@M)) {
				M <- eval(substitute(model$X@values, list(X=expectation@M)))
			}
		}
	} else { 						# Old objective-style
		eCov <- model$objective@info$expCov
		eMean <- model$objective@info$expMean
		if(!length(setdiff(c("A", "S", "F"), names(getSlots(class(expectation)))))) {
			A <- eval(substitute(model$X@values, list(X=expectation@A)))
			S <- eval(substitute(model$X@values, list(X=expectation@S)))
			if("M" %in% names(getSlots(class(expectation))) && !is.na(expectation@M)) {
				M <- eval(substitute(model$X@values, list(X=expectation@M)))
			}
		}
	}

	if(!is.null(A)) {
		# RAM model: calculate total expectation
		I <- diag(nrow(A))
		Z <- solve(I-A)
		eCov <- Z %*% S %*% t(Z)
		if(!is.null(M)) {
			eMean <- Z %*% t(M)
		}
		latents <- model@latentVars
		newData <- data.frame(newData, matrix(NA, ncol=length(latents), dimnames=list(NULL, latents)))
	}
	
	# No means
	if(meanOffsets || !dim(eMean)[1]) {
		eMean <- matrix(0.0, 1, ncol(eCov), dimnames=list(NULL, colnames(eCov)))
	}
	
	# TODO: Sort by pattern of missingness, lapply over patterns
	nRows = nrow(newData)
	outs <- omxApply(newData, 1, umxComputeConditionals, sigma=eCov, mu=eMean, onlyMean=!returnCovs)
	if(returnCovs) {
		means <- matrix(NA, nrow(newData), ncol(eCov))
		covs <- rep(list(matrix(NA, nrow(eCov), ncol(eCov))), nRows)
		for(i in 1:nRows) {
			means[i,] <- outs[[i]]$mu
			covs[[i]] <- outs[[i]]$sigma
		}
		return(list(mean = means, cov = covs))
	}
	return(t(outs))
}


require(sem)
require(OpenMx)
source("~/bin/omxhelper/genEpi.lib.R")
source("~/bin/umx/umx.lib.R")
 
# Often you will see data presented as a lower diagonal.
# the readMoments() function in the sem package is a nice helper to read this from the screen:
 
data = sem::readMoments(file = "", diag = TRUE)
1
.304 1
.305 .344   1
.100 .156 .158   1
.284 .192 .324 .360   1
.176 .136 .226 .210 .265  1
 
# terminates with an empty line: see ?readMoments for more help
 
# now letsfill in the upper triangle with a flipped version of the lower
data[upper.tri(data, diag=F)] = t(data)[upper.tri(data, diag=F)]
 
# Set up manifest variables
manifests = c("income", "occup", "educ", "church", "member", "friends")
 
# Use these to create names for our dataframe
dimnames(data) = list(manifests, manifests)
 
# And latents
latents   = "social" # 1 latent, with three formative inputs, and three reflective outputs (each with residuals)
 
# Just to be helpful to myself, I've made lists of the formative sources, and the reflective receiver variables in this MIMIC model
receivers = manifests[4:6]
sources   = manifests[1:3]
 
MIMIC <- mxModel("MIMIC", type="RAM",
    manifestVars = manifests,
    latentVars   = latents, 
    # Factor loadings
    mxPath(from = sources , to = "social" ),
    mxPath(from = "social", to = receivers),
 
    # Correlated formative sources for F1, each with variance = 1
    mxPath(from = sources, connect = "unique.bivariate", arrows = 2),
    mxPath(from = sources, arrows = 2, values = 1, free=F ),
 
    # Residual variance on receivers
    mxPath(from = receivers, arrows = 2),
    mxData(data, type = "cor", numObs = 530)
)

MIMIC <- mxRun(MIMIC); summary(MIMIC)

MIMIC = umxLabel(MIMIC)
umxGetLabels(MIMIC ,regex= ".")
umxGraph_RAM(MIMIC, std = T, precision = 2, dotFilename = "name",  pathLabels = "none", showFixed = F)
umxUnexplainedCausalNexus(IV_from="educ", delta=1, DV_to= "church", model = MIMIC)
