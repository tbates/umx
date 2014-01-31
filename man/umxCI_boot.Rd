\name{umxCI_boot}
\alias{umxCI_boot}
\title{umxCI_boot}
\usage{
  umxCI_boot(model, rawData = NULL,
    type = c("par.expected", "par.observed", "empirical"),
    std = TRUE, rep = 1000, conf = 95, dat = FALSE)
}
\arguments{
  \item{model}{is an optimized mxModel}

  \item{rawData}{is the raw data matrix used to estimate
  model}

  \item{type}{is the kind of bootstrap you want to run.
  "par.expected" and "par.observed" use parametric Monte
  Carlo bootstrapping based on your expected and observed
  covariance matrices, respectively. "empirical" uses
  empirical bootstrapping based on rawData.}

  \item{std}{specifies whether you want CIs for
  unstandardized or standardized parameters (default: std =
  T)}

  \item{rep}{is the number of bootstrap samples to compute
  (default = 1000).}

  \item{conf}{is the confidence value (default = 95)}

  \item{dat}{specifies whether you want to store the
  bootstrapped data in the output (useful if you want to do
  multiple different analyses, such as mediation analyses)}
}
\value{
  - expected covariance matrix
}
\description{
  Extract the expected covariance matrix from an
  \code{\link{mxModel}} The function creates a sampling
  distribution for parameters by repeatedly drawing samples
  with replacement from your data and then computing the
  statistic for each redrawn sample.
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
	m1 = umxRun(m1, setLabels = T, setStarts = T)
	umxCI_boot(m1, type = "par.expected")
}
\references{
  - \url{http://openmx.psyc.virginia.edu/thread/2598}
  Original written by
  \url{http://openmx.psyc.virginia.edu/users/bwiernik}
}
\seealso{
  - \code{\link{umxRun}}, \code{\link{umxGetExpectedCov}}
}

