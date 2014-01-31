\name{umxGetExpectedCov}
\alias{umxGetExpectedCov}
\title{umxGetExpectedCov}
\usage{
  umxGetExpectedCov(model, latent = T, manifest = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the
  covariance matrix from}

  \item{latent}{Whether to select the latent variables
  (defaults to TRUE)}

  \item{manifest}{Whether to select the manifest variables
  (defaults to TRUE)}
}
\value{
  - expected covariance matrix
}
\description{
  extract the expected covariance matrix from an
  \code{\link{mxModel}}
}
\examples{
\dontrun{
model = umxGetExpectedCov(model, manifest = T) # just the manifests
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu/thread/2598}
  Original written by
  \url{http://openmx.psyc.virginia.edu/users/bwiernik}
}
\seealso{
  - \code{\link{umxRun}}, \code{\link{umxCIpboot}}
}
\keyword{manip}

