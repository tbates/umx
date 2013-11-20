\name{umx_is_RAM}
\alias{umx_is_RAM}
\title{umx_is_RAM}
\usage{
  umx_is_RAM(obj)
}
\arguments{
  \item{obj}{an object to be tested to see if it is an
  OpenMx RAM \code{\link{mxModel}}}
}
\value{
  - Boolean
}
\description{
  Utility function returning a binary answer to the
  question "Is this a RAM model?"
}
\examples{
\dontrun{
if(umx_is_RAM(fit1)){
	message("nice RAM model!")
}
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{mxModel}}
}

