\name{umxReportFit}
\alias{umxReportFit}
\title{umxReportFit}
\usage{
  umxReportFit(model, saturatedModels = NULL,
    report = "line", showEstimates = "std")
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to WITH}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxReportFit Report fit of a model in a compact form
  suitable for a journal
}
\examples{
model = umxReportFit(model)
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}
\keyword{manip}

