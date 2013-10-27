\name{umx-package}
\alias{umx-package}
\alias{umx}
\docType{package}
\title{
umx
}
\description{
A library of Helper functions for OpenMx. If you are just
    starting, see the examples below:
}
\details{
\tabular{ll}{
Package: \tab umx\cr
Version: \tab 0.6\cr
License: \tab GPL-3\cr
URL: \tab \url{http://github.com/tbates/umx}\cr
Depends: \tab R (>= 2.11.0), OpenMx (>= 1.3.0),\cr
Suggests: \tab \cr
Collate: \tab 'build_run_modify.r' 'deprecated.r' 'fit_and_reporting.r'
'umx.lib.R' 'xmu.r' 'model_builders.r' 'misc_and_utility.r'\cr
Packaged: \tab 2013-08-22 22:49:21 UTC; tim\cr
Built: \tab R 2.15.2; ; 2013-08-22 22:49:22 UTC; unix\cr
}

install.packages("devtools")
library("devtools")
install_github("umx", username = "tbates")
library("umx")

\bold{Index:}

\bold{Build, Run, and Report}

\code{\link{umxLabel}}                Add labels to all parameters of a model

\code{\link{umxStart}}                Set sensible start values

\code{\link{umxRun}}                  Run a model, adding saturated models if necessary, optionally adds labels and starts

\code{\link{umxCompare}}              Compare one or more models

\code{\link{umxSummary}}              Compact report of model fit

\code{\link{umxCI}}                  Report FIML CIs on a model, creating them if necessary

\code{\link{umxPlot}}                Create a Path Diagram from your model!


\bold{Updating & Modifying}

\code{\link{umxReRun}}                Re-run a model with changes

\code{\link{umxEquate}}               Equate parameters in a model by label

\code{\link{umxDrop1}}                Compute a table of fits based on dropping single paths

\code{\link{umxAdd1}}                 Compute a table of fits based on adding paths to the model

\code{\link{umxMI}}                   Compute modification indices for RAM models

\bold{High-level reporting}

\code{\link{umxUnexplainedCausalNexus}}

\bold{Utilities}

\code{\link{umxStandardizeModel}}     Standardize a RAM Model

\code{\link{umxReportTime}}           Show how long a model took to run

\code{\link{umxHetCor}}               Calculate HeteroChoric correlations

\code{\link{umxJiggle}}               Jiggle values in a list

\code{\link{umxUpdateOpenMx}}         umxUpdateOpenMx

\code{\link{umxHasCIs}}               Utility function to ask a model if it has CIs

\code{\link{umxIsMxModel}}            Utility function to an object "Are you an OpenMx model?"

\code{\link{umxIsRAMmodel}}            Utility function to an object "Are you an OpenMx RAM model?"

\bold{Non-SEM helpers}

\code{\link{print.dataframe}}         print dataframe in readable form by supressing NA and rounding 0

\code{\link{umxAnovaReport}}          Format an anova model into an APA-style report

\code{\link{Stouffer.test}}           Run a Stouffer.test (meta-analytic p-value)

\code{\link{umx_u_APA_pval}}          Utility function to format a p-value for APA (three decimals, "<.001" if under that)

\code{\link{umxDescriptives}}         Utility function to generate participant section for an APA-style article.

\code{\link{umx_pp33.Rd}}             Reproducibility function: Use non-central distribution: likelihood of a result given the effect was true

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

umxSummary(fit1) 

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
# If you have installed GraphViz, the next command will open it for you to see!

umxPlot(fit1, std = T)

# Run this instead if you don't have GraphViz
umxPlot(fit1, std = T, dotFilename = NA)

}
}
