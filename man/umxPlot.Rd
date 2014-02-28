\name{umxPlot}
\alias{umxPlot}
\title{umxPlot}
\usage{
umxPlot(model = NA, std = T, digits = 2, dotFilename = "name",
  pathLabels = "none", showFixed = F, showError = T, precision = NULL)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to make a path
  diagram from}

  \item{std}{Whether to standardize the model.}

  \item{digits}{The number of decimal places to add to the
  path coefficients}

  \item{dotFilename}{A file to write the path model to. if
  you leave it at the default "name", then the model's
  internal name will be used}

  \item{pathLabels}{whether to show labels on the paths.
  both will show both the parameter and the label. ("both",
  "none" or "labels")}

  \item{showFixed}{whether to show fixed paths (defaults to
  FALSE)}

  \item{showError}{whether to show errors}

  \item{precision}{deprecated for digits}
}
\description{
A function to create graphical path diagrams from your
OpenMx models!
}
\examples{
\dontrun{
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
umxPlot(m1)
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

