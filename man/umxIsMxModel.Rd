\name{umxIsMxModel}
\alias{umxIsMxModel}
\title{umxIsMxModel}
\usage{
  umxIsMxModel(obj)
}
\arguments{
  \item{obj}{an object to be tested to see if it is an
  OpenMx \code{\link{mxModel}}}
}
\value{
  - Boolean
}
\description{
  Utility function returning a binary answer to the
  question "Is this an OpenMx model?"
}
\examples{
\dontrun{
if(umxIsMxModel(fit1)){
	message("nice OpenMx model!")
}
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxModel}}
}

