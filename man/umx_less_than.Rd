\name{umx_less_than}
\alias{\%<\%}
\alias{umx_less_than}
\title{umx_less_than}
\usage{
  umx_less_than(table, x)

  %<%(table, x)
}
\description{
  A version of less-than which returns FALSE for NAs
  (rather than NA)
}
\details{
  #
}
\examples{
c(1:3, NA, 5) \%<\% 2
}
\seealso{
  - \code{\link{umx_greater_than}},
}

