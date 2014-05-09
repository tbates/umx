\name{umx_less_than}
\alias{\%<\%}
\alias{umx_less_than}
\title{umx_less_than}
\usage{
umx_less_than(table, x)

table \%<\% x
}
\arguments{
  \item{table}{thing to compare 1}

  \item{x}{thing to compare 2}
}
\description{
A version of less-than which returns FALSE for NAs (rather than NA)
}
\examples{
c(1:3, NA, 5) \%<\% 2
}
\seealso{
- \code{\link{umx_greater_than}},
}

