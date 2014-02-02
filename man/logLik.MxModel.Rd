\name{logLik.MxModel}
\alias{logLik.MxModel}
\title{logLik.MxModel}
\usage{
  logLik.MxModel(model)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the log
  likelihood from}
}
\value{
  - the log likelihood
}
\description{
  returns the log likelihood for an OpenMx model helper
  function which enables ACI(model); BIC(model); and
  logLik(model) functions
}
\examples{
\dontrun{
AIC(model)
}
}
\references{
  -
  \url{http://openmx.psyc.virginia.edu/thread/931#comment-4858}
}
\seealso{
  - \code{\link{AIC}}, \code{\link{umxCompare}}
}

