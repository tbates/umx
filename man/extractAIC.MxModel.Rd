\name{extractAIC.MxModel}
\alias{extractAIC.MxModel}
\title{extractAIC}
\usage{
  extractAIC.MxModel(model)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the AIC for}
}
\value{
  - AIC value
}
\description{
  returns the AIC for an OpenMx model
}
\examples{
\dontrun{
x = extractAIC(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxRun}}, \code{\link{umxCompare}}
}

