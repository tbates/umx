\name{umx_is_cov}
\alias{umx_is_cov}
\title{umx_is_cov}
\usage{
umx_is_cov(data = NULL, boolean = F, verbose = F)
}
\arguments{
  \item{data}{dataframe to test}

  \item{boolean}{whether to return the type ("cov") or a
  boolean (default = string)}
}
\value{
- "raw", "cor", or "cov", or, if boolean= T, then T | F
}
\description{
test if a data frame or matrix is cov or cor data, or is
likely to be raw...
}
\examples{
df = cov(mtcars)
umx_is_cov(df)
df = cor(mtcars)
umx_is_cov(df)
umx_is_cov(df, boolean = T)
umx_is_cov(mtcars, boolean = T)
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

