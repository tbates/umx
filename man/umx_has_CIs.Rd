\name{umx_has_CIs}
\alias{umx_has_CIs}
\title{umx_has_CIs}
\usage{
  umx_has_CIs(model)
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
umx_has_CIs(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxCI}}, \code{\link{umxRun}},
  \code{\link{umxCIs}}
}

