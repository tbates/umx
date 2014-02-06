\name{umxFitIndices}
\alias{umxFitIndices}
\title{umxFitIndices}
\usage{
  umxFitIndices(model, indepfit)
}
\arguments{
  \item{modelfit}{an \code{\link{mxModel}}}

  \item{indepfit}{an \code{\link{mxModel}}}
}
\value{
  -
}
\description{
  A list of fit indices
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
umxFitIndices(m1)
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxSummary}}, \code{\link{umxCompare}},
  \code{\link{summary}}, \code{\link{umxLabel}},
  \code{\link{umxRun}}, \code{\link{umxStart}}
}

