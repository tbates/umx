\name{omxRMSEA}
\alias{omxRMSEA}
\title{omxRMSEA}
\usage{
  omxRMSEA(model, ci.lower = 0.05, ci.upper = 0.95)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to WITH}

  \item{ci.lower}{the lower Ci to compute}

  \item{ci.upper}{the upper Ci to compute}
}
\value{
  - object containing the RMSEA and lower and upper bounds
}
\description{
  Compute the confidence interval on RMSEA
}
\examples{
\dontrun{
omxRMSEA(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

