\name{umx-package}
\alias{umx-package}
\alias{umx}
\docType{package}
\title{
umx
}
\description{
A library of Helper functions for OpenMx. If you are just
    starting, try typing ?umxReport()
}
\details{
\tabular{ll}{
Package: \tab umx\cr
Version: \tab 0.5\cr
License: \tab GPL-3\cr
URL: \tab http://github.com/tbates/umx\cr
Depends: \tab R (>= 2.11.0), OpenMx (>= 1.3.0),\cr
Suggests: \tab testthat, roxygen2\cr
Collate: \tab 'build_run_modify.r' 'deprecated.r' 'fit_and_reporting.r'
'umx.lib.R' 'xmu.r' 'model_builders.r' 'misc_and_utility.r'\cr
Packaged: \tab 2013-08-20 19:34:12 UTC; tim\cr
Built: \tab R 2.15.2; ; 2013-08-20 19:34:13 UTC; unix\cr
}

Index:
\preformatted{
Stouffer.test           Stouffer.test
print.dataframe         print.dataframe
umxAnovaReport          umxAnovaReport
umxCompare              umxCompare
umxEquate               umxEquate
umxHasCIs               umxHasCIs
umxHetCor               umxHetCor
umxJiggle               umxJiggle
umxLabel                umxLabel
umxMI                   umxMI
umxMI_top               umxMI_top
umxReRun                umxReRun
umxReportCIs            umxReportCIs
umxReportFit            umxReportFit
umxReportTime           umxReportTime
umxRun                  umxRun
umxStandardizeModel     umxStandardizeModel
umxStart                umxStart
umxSummary              umxSummary
umxUnexplainedCausalNexus
umxUpdateOpenMx         umxUpdateOpenMx
xmuLabel_MATRIX_Model   xmuLabel_MATRIX_Model (not a user function)
xmuMakeThresholdsMatrices
                        xmuMakeThresholdsMatrices (not a user function)
xmuPropagateLabels      xmuPropagateLabels (not a user function)
}
}
\author{
Tim Bates

Maintainer: Who to complain to <timothy.c.bates@gmail.com>
}
\keyword{ package }

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
