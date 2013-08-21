\name{umxRun}
\alias{umxRun}
\title{umxRun}
\usage{
  umxRun(model, n = 3, calc_SE = T, calc_sat = T)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to run.}

  \item{n}{The maximum number of times you want to run the
  model trying to get a code green run (defaults to 3)}

  \item{calc_SE}{Whether to calculate standard errors for
  the summary (they are not very accurate, so if you use
  \code{\link{mxCI}} you can turn this off)}

  \item{calc_sat}{Whether to calculate the saturated and
  independence models (for raw \code{\link{mxData}}
  \code{\link{mxModel}}s)}
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
model = umxRun(model)
model = umxRun(model, n=10)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxRun}}, \code{\link{umxLabel}},
  \code{\link{umxStart}}
}

