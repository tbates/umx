# Author: Michael D. Hunter
# Date: 2013.06.21
# Purpose: Notes from discussion during OpenMx meeting about the user interface
#  for the WLS Fit Function, the new mxData type, and the WLS helper function.

#------------------------------------------------------------------------------
# Example OpenMx Models

# This is fine
amod <- mxModel(name="FIML Naive User 1",
	mxPaths etc,
	mxData(observed=blah, type="raw"),
	mxFitFunctionML()
)


# This throws an error at mxRun runtime that fit function WLS requires
#  data of type acov but model has data of type raw.  Run the 
#  omxWLSHelper function to generate a list of matrices to populate
#  a new mxData object.  For example,
#    wlsData <- omxWLSHelper(yourData)
#    mxData(observed=wlsData$observed, type="acov", acov=wlsData$acov, thresholds=wlsData$thresholds) 
bmod <- mxModel(model=amod, name="WLS Naive User 1",
	mxFitFunctionWLS() # default is type="WLS"?
)


# This throws an error?  mxModel objects can't have data frames and matrix objects (as opposed to MxMatrix objects in them?
cmod <- mxModel(model=amod, name="WLS Naive User 2",
	wlsPrep(blah),
	mxFitFunctionWLS()
)


w <- omxWLSHelper(blah)
dmod <- mxModel(model=amod, name="WLS Good User 1",
	mxData(w$observed, type="acov", acov=w$acov, thresholds=w$thresholds),
	# how about instead giving some brains to mxData?
	mxData(w$observed, type = "acov"), # w contains observed, acov and thresholds. type= is only needed for consistency: mxData should be able to tell what it has received by an attribute.
	mxFitFunctionWLS()
)


emod <- mxModel(model=amod, name="WLS Careless User 1",
	mxData(w$observed, type="acov", acov=diag(w$acov), thresholds=w$thresholds),
	mxFitFunctionWLS(type="DWLS")
) #throws an error because acov argument is a vector not a matrix


fmod <- mxModel(model=amod, name="WLS Good User 2",
	mxData(w$observed, type="acov", acov=matrix(diag(w$acov)), thresholds=w$thresholds),
	mxFitFunctionWLS(type="DWLS")
)


gmod <- mxModel(model=amod, name="WLS Good User 3",
	mxData(w$observed, type="acov", acov=diag(diag(w$acov)), thresholds=w$thresholds),
	mxFitFunctionWLS(type="DWLS")
)

#------------------------------------------------------------------------------
# Behind the scences things that are happening and being checked

omxWLSHelper <- function(data, type = c("WLS", "ULS", "DWLS")) {
}

mxWLSData <- function(data, type = c("WLS", "ULS", "DWLS")) {
	observed = cov(data)
	# set thresholds
	if(omx_any_is_ordinal(data)){
		# some ordinal: retrun thresholds
		thresholds = TODO
	} else {
		thresholds = NA # if no ordinal, thresholds returns NA
	}
	
	# set acov
	if(type == "DWLS"){
		# acov returns an Nx1 matrix of the diagonals
		# TODO speed up processing of the DWLS to not compute off-diagonal elements
	}else if (type == "ULS"){
		# acov returns NA or the right size identity matrix?
		# Probably NA because the reason for using ULS might be more variables than the user wants to store: 200*201/2 = 20,100.
	}else if (type == "WLS") {
		# acov returns the full asymptotic covariance matrix
		# ordinal vs continuous vs joint happens based on the pressence/absence of ordinal-only vs continuous-only vs both variable type.
	}
	return(list(observed=, acov=, thresholds=))	
} 

mxData <- function(data, type = c("WLS", "ULS", "DWLS")) {
	acov argument accepts an NxN matrix which is checked for being a covariance matrix
	acov argument also accepts an Nx1 matrix, not check for being a covariance matrix
	dimension of acov should be checked against observed
		N = nrow(acov) == (n*(n+1))/2 for n=nrow(observed)=ncol(observed)
	if observed is an acov object ignore everything else (perhaps check type="acov" & is.null( other things))

	if data type="acov" and some variables are ordinal, then must have thresholds
	if data type="acov" and no variables are ordinal, then cannot have thresholds
	Number of thresholds matrix columns equals number of ordinal variables
	thresholds must have !is.null(dimnames(thresholds)[[2]])
	dimnames(thresholds)[[2]] must correspond with names in observed covariance matrix.
	the omxWLSPrep function should have the above true.


mxFitFunctionWLS()<- function(data, type = c("WLS", "ULS", "DWLS")) {
	# checks type of model.data %in% c("WLS", "ULS", "DWLS") and adopts that
	if (model.data.type %in% c("WLS", "ULS", "DWLS")){
		# carry on...
	}else{
		if (data.type =="raw"){
			message("error suggests running helper function to populate mxData")
		} else if (data.type == "cov" & is.na(data.acov) & WLS fit function type="ULS"{
		error suggests changing data type to "acov"	
		}else{
		  message("error just says WLS fit function requires mxData of type "acov" but has type cor/cov/raw.	")
		}	
	}
	
	if fit function type="DWLS", and the acov is neither diagonal nor Nx1, error says fit function is DWLS but acov in data is neither diagonal nor Nx1
	if fit function type="ULS", then data acov argument must be either an identity matrix or NA.  Suggests "You may want to change the acov argument to your mxData to be NA"
}