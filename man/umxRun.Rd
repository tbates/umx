\name{umxRun}
\alias{umxRun}
\title{umxRun}
\usage{
  umxRun(model, n = 1, calc_SE = T, calc_sat = T,
    setStarts = F, setLabels = F)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to run.}

  \item{n}{The maximum number of times you want to run the
  model trying to get a code green run (defaults to 1)}

  \item{calc_SE}{Whether to calculate standard errors (not
  used when n = 1) for the summary (they are not very
  accurate, so if you use \code{\link{mxCI}} you can turn
  this off)}

  \item{calc_sat}{Whether to calculate the saturated and
  independence models (for raw \code{\link{mxData}}
  \code{\link{mxModel}}s) (defaults to TRUE - why would you
  want anything else?)}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxRun is a version of \code{\link{mxRun}} which can run
  multiple times by default The main value for umxRun over
  mxRun is with raw data. It's slightly faster, but can
  also calculate the saturated and independence likelihoods
  necessary for most fit indices.
}
\examples{
\dontrun{
model = umxRun(model) # just run: will create sturated model if needed
model = umxRun(model, setStarts = T, setLabels = T) # set start values and label all parameters
model = umxRun(model, n=10) # run, but also re-run if not clean the first time
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxRun}}, \code{\link{umxLabel}},
  \code{\link{umxStart}}
}

