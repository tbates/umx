#------------------------------------------------------------------------------
# Author: Michael D. Hunter
# Date: 2014.06.03
# Purpose: Demonstrate the proposed user interface for WLS fit functions.
#------------------------------------------------------------------------------
require(OpenMx)
# ===============================================
# = proposed method to create acov data for WLS =
# ===============================================

data(demoOneFactor)
df <- demoOneFactor
df <- mxFactor(df) # do the appropriate mxFactoring of ordinal variables
dataPrepd <- mxWeightData(df) # Returns an mxData(, type ="acov")
# message("Creating WLS data")

# Make and run simple WLS model
m1 <- mxModel(model = "Simple WLS Model", 
	amat, bmat, cmat, zmatm, zmatl, # matrices
	mxExpectationLISREL(LX = 'A', PH = 'B', TD = 'C', TX = 'ZM', AL = 'ZL', threholds = ...),
	mxFitFunctionWLS(),
	dataPrepd # data
)
m1 <- mxRun(m1) # Runs fast with WLS

# ====================
# = Convert to ML... =
# ====================
m2 <- mxModel(model = m1, name = "ML Version",
	mxData(data, type='raw'), # replace data
	mxFitFunctionML()         # add ML fitfunction
)

m2 <- mxRun(m2)
# Slow but faster than other ML because of good starting values from WLS run !

# Can do the data conversion at model build time, rather than before if desired
dmod <- mxModel(model = 'make WLS on run', amat, bmat, cmat, zmatm, zmatl,
	mxExpectationLISREL(LX = 'A', PH = 'B', TD = 'C', TX = 'ZM', AL = 'ZL', threholds = ...),
	mxWeightData(data),
	mxFitFunctionWLS()
)

dmod <- mxRun(dmod)

# ==============================
# = What to do for RAM models? =
# ==============================

# To avoid user having to know about mxFitFunctionWLS(), use fit = "WLS" to select this for the RAM case

pathic <- mxModel(model="Pathic", type = "RAM", fit = "WLS",
	mxPath(from = "a", to = "b","c"),
	mxWeightData(data)
)
# Will select mxFitFunctionWLS() (and check data type for match)
# Assuming that legal alternatives for fit include 'ULS', 'DWLS', 'WLS'

# ==========================
# = Alternative data model =
# ==========================

# Keeping mxData so nothing new to learn in the model

mxData(mxACOV(df), type = "acov")

# note this is analogous to
mxData(cov(df), type = "cov")