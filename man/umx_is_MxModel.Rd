\name{umx_is_MxModel}
\alias{umx_is_MxModel}
\title{umx_is_MxModel}
\usage{
  umx_is_MxModel(obj)
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
m1 = mxModel("test")
if(umx_is_MxModel(m1)){
	message("nice OpenMx model!")
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxModel}}
}

