\name{umxLessThan}
\alias{\%\%<\%\%}
\alias{umxLessThan}
\title{umxLessThan}
\usage{
  umxLessThan(table, x)
}
\description{
  A version of less-than which returns FALSE for NAs
  (rather than NA)
}
\examples{
c(1:3, NA, 5) = \%<\% 2
}
\seealso{
  - \code{\link{umxGreaterThan}},
}

