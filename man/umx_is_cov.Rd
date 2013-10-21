\name{umx_is_cov}
\alias{umx_is_cov}
\title{umx_is_cov}
\usage{
  umx_is_cov(data, verbose = F)
}
\arguments{
  \item{data}{dataframe to test}
}
\value{
  - 0 if raw, 1 if cor, 2 if cov
}
\description{
  test if a data frame or matrix is cov or cor data, or is
  likely to be raw...
}
\examples{
\dontrun{
umx_is_cov(df)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

