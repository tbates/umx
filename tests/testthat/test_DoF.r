# m1sat = omxSaturatedModel(m1)
# summary(m1, SaturatedLikelihood = m1sat)
# TODO: fix this error to make it helpful... easy to say
# You likely wanted to say:
# summary(m1, refModels = mxRefModels(m1, run=TRUE))

# Load libraries
require(OpenMx)
data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)[1:6]
latents = c("G")
m1 <- mxModel("onlyusedVars", type="RAM",
	manifestVars = manifests,
	latentVars   = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxPath(from = "one", to = manifests, arrows = 1), # manifest means
	mxData(myFADataRaw[, manifests], type = "raw")
)
m2 <- mxModel(m1, mxData(myFADataRaw, type = "raw"), name="tooManyInData")
m1 = mxRun(m1);
m2 = mxRun(m2);
m1s = summary(m1, refModels = mxRefModels(m1, run=TRUE))
m2s = summary(m2, refModels = mxRefModels(m2, run=TRUE))
# expect this == 9
omxCheckEquals(9, m1s$degreesOfFreedom - m1s$saturatedDoF)
omxCheckEquals(9, m2s$degreesOfFreedom - m2s$saturatedDoF)

# given SaturatedLikelihood=SomeMxModel, OpenMx should grab the degrees of freedom from that model

n <- 5 # number of variables in data 
m <- 4 # number of used variables in data
n*(n-1)/2 + 2*m
m*(m-1)/2 + 2*m

# Given your observed statistics (2221) and discovery of this bug, I was able to calculate
DoF    <- 2221 - 3 # obs stats minus estimated params 
n      <- 39 # number of variables that must be in data
m      <- 7  # number of variables used in model
satDoF <- 2221 - (n*(n-1)/2 + 2*m)
DoF-satDoF # Reported incorrectly as 752 by OpenMx. Should be 32
