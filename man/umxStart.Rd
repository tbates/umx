\name{umxStart}
\alias{umxStart}
\title{umxStart}
\usage{
  umxStart(obj = NA, sd = NA, n = 1, onlyTouchZeros = F)
}
\arguments{
  \item{obj}{The RAM or matrix \code{\link{mxModel}}, or
  \code{\link{mxMatrix}} that you want to set start values
  for.}

  \item{sd}{Optional Standard Deviation for start values}

  \item{n}{Optional Mean for start values}

  \item{onlyTouchZeros}{Don't start things that appear to
  have already been started (useful for speeding
  \code{\link{umxReRun}})}
}
\value{
  - \code{\link{mxModel}} with updated start values
}
\description{
  umxStart will set start values for the free parameters in
  RAM and Matrix \code{\link{mxModel}}s, or even
  mxMatrices. It will try and be smart in guessing these
  from the values in your data, and the model type.
}
\examples{
\dontrun{
model = umxStart(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}}
}

