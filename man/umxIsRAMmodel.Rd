\name{umxIsRAMmodel}
\alias{umxIsRAMmodel}
\title{umxIsRAMmodel}
\usage{
  umxIsRAMmodel(obj)
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
if(umxIsRAMmodel(fit1)){
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

