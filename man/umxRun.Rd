\name{umxRun}
\alias{umxRun}
\title{umxRun}
\usage{
  umxRun(model, n = 1, calc_SE = T, calc_sat = T,
    setValues = F, setLabels = F, comparison = NULL,
    setStarts = NULL)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to run.}

  \item{n}{The maximum number of times you want to run the
  model trying to get a code green run (defaults to 1)}

  \item{calc_SE}{Whether to calculate standard errors (not
  used when n = 1) for the summary (they are not very
  accurate, so if you use \code{\link{mxCI}} or
  \code{\link{umxCI}}, you can turn this off)}

  \item{calc_sat}{Whether to calculate the saturated and
  independence models (for raw \code{\link{mxData}}
  \code{\link{mxModel}}s) (defaults to TRUE - why would you
  want anything else?)}

  \item{setValues}{Whether to set the starting values of
  free parameters (defaults to F)}

  \item{setLabels}{Whether to set the labels (defaults to
  F)}

  \item{comparison}{Whether to run umxCompare() after
  umxRun}

  \item{setStarts}{Deprecated way to setValues}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxRun is a version of \code{\link{mxRun}} which can run
  also set start values, labels, and run multiple times It
  can also calculate the saturated and independence
  likelihoods necessary for most fit indices.
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
umxSummary(m1, show = "std")
\dontrun{
model = umxRun(model) # just run: will create saturated model if needed
model = umxRun(model, setValues = T, setLabels = T) # set start values and label all parameters
model = umxRun(model, n=10) # run, but also re-run if not green the first run
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxRun}}, \code{\link{umxLabel}},
  \code{\link{umxStart}}, \code{\link{mxModel}},
  \code{\link{umxCIboot}}
}

