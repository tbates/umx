\name{umxReportFit}
\alias{umxReportFit}
\title{umxReportFit}
\usage{
  umxReportFit(model, saturatedModels = NULL,
    report = "line", showEstimates = "std")
}
\arguments{
  \item{model}{The \code{\link{mxModel}} whose fit will be
  reported}

  \item{saturatedModels}{Saturated models if needed for fit
  indices (see note below: Only needed for raw data, and
  then not if you've run umxRun}

  \item{report}{The format for the output (currently only a
  1-liner is supported)}

  \item{showEstimates}{Whether to show the raw or
  standadized estimates (default is std)}
}
\description{
  umxReportFit Report fit of a model in a compact form
  suitable for a journal
}
\examples{
\dontrun{
model = umxReportFit(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

