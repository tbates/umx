\name{reliability}
\alias{reliability}
\title{reliability}
\usage{
  reliability(S)
}
\arguments{
  \item{S}{A square, symmetric, numeric covariance matrix}
}
\value{
  -
}
\description{
  Compute and report Coefficient alpha
}
\examples{
treat car aspects as items of a test
reliability(cov(mtcars))
}
\references{
  - \url{http://Rcmdr}
}
\seealso{
  - \code{\link{cov}}
}
\keyword{manip}

