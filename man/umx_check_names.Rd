\name{umx_check_names}
\alias{umx_check_names}
\title{umx_check_names}
\usage{
  umx_check_names(namesNeeded, data, die = TRUE)
}
\arguments{
  \item{namesNeeded}{list of variable names to find}

  \item{data}{data.frame to search in for names}
}
\value{
  - boolean
}
\description{
  check if a list of names are in the names() of a
  dataframe
}
\examples{
\dontrun{
umx_check_names(c("E", data))
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

