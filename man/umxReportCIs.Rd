\name{umxReportCIs}
\alias{umxReportCIs}
\title{umxReportCIs}
\usage{
  umxReportCIs(model = NA, addCIs = T,
    runCIs = "if necessary")
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to report
  \code{\link{mxCI}}s on}

  \item{addCIs}{Whether or not to add mxCIs if none are
  found (defaults to TRUE)}

  \item{runCIs}{Whether or not to run the CIs: if F, this
  function can simply add CIs and return the model. Valid
  values = "no", "yes", "if necessary" all the CIs and
  return that model for \code{\link{mxRun}}ning later}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxReportCIs umxReportCIs adds mxCI() calls for all free
  parameters in a model, runs the CIs, and reports a neat
  summary.
}
\examples{
\dontrun{
umxReportCIs(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxCI}}, \code{\link{umxLabel}},
  \code{\link{umxRun}}
}

