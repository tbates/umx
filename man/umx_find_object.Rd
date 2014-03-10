\name{umx_find_object}
\alias{umx_find_object}
\title{umx_find_object}
\usage{
umx_find_object(pattern = ".*", requiredClass = "MxModel")
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
Find objects a certain class, whose name matches a search
string. The string (pattern) is grep-enabled, so you can
match wild-cards
}
\examples{
\dontrun{
umx_find_object("^m[0-9]") # mxModels beginning "m1" etc.
}
}
\references{
-
}
\seealso{
- \code{\link{grep}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

