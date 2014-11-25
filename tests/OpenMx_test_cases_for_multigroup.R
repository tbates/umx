require(OpenMx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)

m1 <- mxModel("model1", type = "RAM", 
	manifestVars = manifests, latentVars = latents, 
	mxPath(from = latents,   to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents,   arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)

m2 <- mxRename(m1, "model2")

m3 = mxModel("bob", m1, m2,
	mxFitFunctionMultigroup(c("model1", "model2"))
)

m3 = mxRun(m3); summary(m3)
# All have been run, of course
m3@.wasRun #[1] TRUE
m3$model1@.wasRun #[1] TRUE
m3$model2@.wasRun #[1] TRUE

mRef = omxSaturatedModel(m3)
# The model 'model1' has not been run. So reference models of all the variables in the data will be made.  For reference models of only the variables used in the model, provide the model after it has been run.
# The model 'model2' has not been run. So reference models of all the variables in the data will be made.  For reference models of only the variables used in the model, provide the model after it has been run.

# summary(m3, refModels = mRef)
# we should return an error here... the user should be providing a list of two models to refModels, with the saturated model coming first

refModelList = mxRefModels(m3, run = TRUE)
# The model 'model1' has not been run. So reference models of all the variables in the data will be made.  For reference models of only the variables used in the model, provide the model after it has been run.
# The model 'model2' has not been run. So reference models of all the variables in the data will be made.  For reference models of only the variables used in the model, provide the model after it has been run.

# Should have the full list of stats now, but:

summary(m3, refModels = refModelList)
# TLI: NaN
# RMSEA:  0 *(Non-centrality parameter is negative)
# Some of your fit indices are missing.
#   To get them, fit saturated and independence models, and include them with
  # summary(yourModel, SaturatedLikelihood=..., IndependenceLikelihood=...).

