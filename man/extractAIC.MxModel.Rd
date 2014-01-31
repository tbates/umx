\name{extractAIC.MxModel}
\alias{extractAIC.MxModel}
\title{extractAIC}
\usage{
  extractAIC.MxModel(model)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the AIC
  from}
}
\value{
  - AIC value
}
\description{
  returns the AIC for an OpenMx model helper function for
  \code{\link{logLik.MxModel}} (which enables AIC(model);
  logLik(model); BIC(model) Original Author: brandmaier
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
m1 = umxRun(m1, setLabels = T, setStarts = T)
extractAIC(m1)
}
\references{
  -
  \url{http://openmx.psyc.virginia.edu/thread/931#comment-4858}
}
\seealso{
  - \code{\link{AIC}}, \code{\link{umxCompare}},
  \code{\link{logLik.MxModel}}
}

