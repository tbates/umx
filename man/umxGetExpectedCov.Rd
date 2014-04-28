\name{umxGetExpectedCov}
\alias{umxGetExpectedCov}
\title{umxGetExpectedCov}
\usage{
umxGetExpectedCov(model, latent = T, manifest = T, digits = NULL)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the
  covariance matrix from}

  \item{latent}{Whether to select the latent variables
  (defaults to TRUE)}

  \item{manifest}{Whether to select the manifest variables
  (defaults to TRUE)}

  \item{digits}{precision of reporting. Leave NULL to do no
  rounding.}
}
\value{
- expected covariance matrix
}
\description{
extract the expected covariance matrix from an \code{\link{mxModel}}
}
\examples{
require(OpenMx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxGetExpectedCov(model = m1)
umxGetExpectedCov(m1, digits = 3)
}
\references{
- \url{http://www.github.com/tbates/umx/thread/2598}
Original written by \url{http://www.github.com/tbates/umx/users/bwiernik}
}
\seealso{
- \code{\link{umxRun}}, \code{\link{umxCI_boot}}
}

