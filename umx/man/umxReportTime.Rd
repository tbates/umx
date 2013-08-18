\name{umxReportTime}
\alias{umxReportTime}
\title{umxReportTime}
\usage{
  umxReportTime(model, formatStr = "H \%H M \%M S \%OS3",
    tz = "GMT")
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get the time
  from}
}
\description{
  umxReportTime Reports how long the model took to execute
  compactly, without having to run a summary
}
\examples{
umxReportTime(model)
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{summary}}, \code{\link{umxRun}}
}

