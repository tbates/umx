\name{umxHasCIs}
\alias{umxHasCIs}
\title{umxHasCIs}
\usage{
  umxHasCIs(model)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} to check for
  presence of CIs}
}
\value{
  - TRUE or FALSE
}
\description{
  A utility function to return a binary answer to the
  question "does this \code{\link{mxModel}} have confidence
  intervals?"
}
\examples{
\dontrun{
umxHasCIs(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxCI}}, \code{\link{umxRun}},
  \code{\link{umxReportCIs}}
}

