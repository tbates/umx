\name{umxFindObject}
\alias{umxFindObject}
\title{umxFindObject}
\usage{
  umxFindObject(pattern = ".*", requiredClass = "MxModel")
}
\arguments{
  \item{pattern}{the pattern that matching objects must
  contain}

  \item{requiredClass}{the class of object that will be
  matched}
}
\value{
  - a list of objects matching the class and name
}
\description{
  find objects a certain class, whose name matches a search
  string. The string (pattern) is grep-enabled, so you can
  match wild-cards
}
\examples{
\dontrun{
umxFindObject("^m[0-9]") # mxModels beginning "m1" etc.
}
}
\references{
  -
}
\seealso{
  - \code{\link{grep}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

