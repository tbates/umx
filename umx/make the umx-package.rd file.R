promptPackage("umx", final = T)

Index:

Build, Run, and Report

\code{\link{umxLabel}}                Add labels to all parameters of a model

\code{\link{umxLabel}}                Add labels to all parameters of a model

\code{\link{umxStart}}                Set sensible start values

\code{\link{umxRun}}                  umxRun

\code{\link{umxSummary}}              Compact summary

\code{\link{umxCompare}}              Compare one or more models

\code{\link{umxReportCIs}}            Report FIML CIs on a model

\code{\link{umxGraph_RAM}}            Create a Path Figure of your SEM

Updating & MOdifying

\code{\link{umxReRun}}                Re-run a model with changes

\code{\link{umxEquate}}               Equate parameters in a model  by label

\code{\link{umxMI_top}}               Modification indices for RAM models

\code{\link{umxReportTime}}           Show how long a model took to run

\code{\link{umxStandardizeModel}}     Standardize a RAM Model

\code{\link{umxUpdateOpenMx}}         umxUpdateOpenMx

High-level reporting

\code{\link{umxUnexplainedCausalNexus}}

Utilities

\code{\link{umxHasCIs}}               Utility to ask a model if it has CIs

\code{\link{umxHetCor}}               Calculate HeteroChoric correlations

\code{\link{umxJiggle}}               Jiggle values in a list

Non-SEM helpers

\code{\link{print.dataframe}}         print dataframe in readable form by supressing NA and rounding 0

\code{\link{umxAnovaReport}}          A handy ANOVA report formatter

\code{\link{Stouffer.test}}           Run a Stouffer.test

}


\references{
  - \url{http://openmx.psyc.virginia.edu/}
}

\examples{
\dontrun{
require("OpenMx")
require("umx")
data(demoOneFactor)
latents = c("G")
manifests = names(demoOneFactor)
fit1 <- mxModel("One Factor", type="RAM",
	manifestVars = manifests,
	latentVars  = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents  , arrows = 2, free = F, values = 1),
	mxData(cov(demoOneFactor), type = "cov", numObs = nrow(demoOneFactor))
)

omxGetParameters(fit1) # nb: By default, paths have no labels, and starts of 0

# umxLabel easily add informative and predictable labels to each free path (works with matrix style as well!)
# and with umxStart, we can easily add sensible guesses for start values...
fit1 = umxLabel(fit1)  
fit1 = umxStart(fit1)  

# Re-run omxGetParameters...
omxGetParameters(fit1) # Wow! Now your model has informative labels, & better starts

# umxRun the model (calculates saturated models for raw data, & repeats if the model is not code green)
fit1 = umxRun(fit1)    

# Let's get some journal-ready fit information

umxReportFit(fit1) 

umxReportFit(fit1, ) 

# Model updating example
# Can we equate the loading of X5 on g to zero?
fit2 = omxSetParameters(fit1, labels = "G_to_x1", values = 0, free = F, name = "no_effect_of_g_on_X5")
fit2 = mxRun(fit2)
# Model comparison example
umxCompare(fit1, fit2)

# Same thing with umxReRun
fit2 = umxReRun(fit1, "x5_with_x5", name = "no_residual_onX5")

umxCompare(fit1, fit2)

# And make a Figure it dot format!
# If you have installed GraphViz, the first command will open it for you to see!

umxGraph_RAM(fit1, std = T)

# Run this if you don't have GraphViz
umxGraph_RAM(fit1, std = T, dotFilename = NA)


}
}
