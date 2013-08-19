\name{umxReportTime}
\alias{umxReportTime}
\title{umxReportTime}
\usage{
  umxReportTime(model, formatStr = "H \%H M \%M S \%OS3",
    tz = "GMT")
}
\arguments{
  \item{model}{an \code{\link{mxModel}} from which to get
  the elapsed time}

  \item{formatStr}{A format string, defining how to show
  the time}

  \item{tz}{The time zone in which the model was executed}
}
\description{
  umxReportTime Reports how long the model took to execute
  compactly, without having to run a summary
}
\examples{
\dontrun{
umxReportTime(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{summary}}, \code{\link{umxRun}}
}

