\name{umxFixLatentVariances}
\alias{umxFixLatentVariances}
\title{umxFixLatentVariances}
\usage{
umxFixLatentVariances(model, fixedValue = 1, latents = NULL)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to set}

  \item{fixedValue}{(Default <- 1)}

  \item{latents}{(If NULL then all)}
}
\value{
- \code{\link{mxModel}}
}
\description{
Fix the variance of all, or selected, latents @ selected values.
}
\examples{
require(OpenMx)
data(demoOneFactor)
m1 <- umxRAM("One Factor",
	mxPath(from = "g", to = names(demoOneFactor)),
	mxPath(from = names(demoOneFactor), arrows = 2),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = umxFixLatentVariances(m1)
}
\references{
- \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
}

