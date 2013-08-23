\name{umxReportTime}
\alias{umxReportTime}
\title{umxReportTime}
\usage{
  umxReportTime(model, formatStr = "H \%H M \%M S \%OS3",
    tz = "GMT")
}
\arguments{
  \item{model}{An \code{\link{mxModel}} from which to get
  the elapsed time}

  \item{formatStr}{A format string, defining how to show
  the time}

  \item{tz}{The time zone in which the model was executed}
}
\description{
  A functoin to compactly report how long a model took to
  execute
}
\examples{
\dontrun{
umxReportTime(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{summary}}, \code{\link{umxRun}}
}

