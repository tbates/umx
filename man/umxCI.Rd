\name{umxCI}
\alias{umxCI}
\title{umxCI}
\usage{
  umxCI(model = NULL, addCIs = T, runCIs = "if necessary",
    showErrorcodes = T)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to report
  \code{\link{mxCI}}s on}

  \item{addCIs}{Whether or not to add mxCIs if none are
  found (defaults to TRUE)}

  \item{runCIs}{Whether or not to run the CIs: if F, this
  function can simply add CIs and return the model. Valid
  values = "no", "yes", "if necessary" all the CIs and
  return that model for \code{\link{mxRun}}ning later}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxCI adds mxCI() calls for all free parameters in a
  model, runs the CIs, and reports a neat summary.
}
\details{
  This function also reports any problems computing a CI.
  The codes are standard OpenMx errors and warnings
  \itemize{ \item 1: The final iterate satisfies the
  optimality conditions to the accuracy requested, but the
  sequence of iterates has not yet converged. NPSOL was
  terminated because no further improvement could be made
  in the merit function (Mx status GREEN) \item 2: The
  linear constraints and bounds could not be satisfied. The
  problem has no feasible solution. \item 3: The nonlinear
  constraints and bounds could not be satisfied. The
  problem may have no feasible solution. \item 4: The major
  iteration limit was reached (Mx status BLUE). \item 6:
  The model does not satisfy the first-order optimality
  conditions to the required accuracy, and no improved
  point for the merit function could be found during the
  final linesearch (Mx status RED) \item 7: The function
  derivates returned by funcon or funobj appear to be
  incorrect. \item 9: An input parameter was invalid }
}
\examples{
require(OpenMx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxCI(model)
\dontrun{
umxCI(model, addCIs = T) # add Cis for all free parameters if not present
umxCI(model, runCIs = "yes") # force update of CIs
umxCI(model, runCIs = "if necessary") # don't force update of CIs, but if they were just added, then calculate them
umxCI(model, runCIs = "no") # just add the mxCI code to the model, don't run them
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxCI}}, \code{\link{umxLabel}},
  \code{\link{umxRun}}
}

