\name{umx_round}
\alias{umx_round}
\title{umx_round}
\usage{
  umx_round(x, digits, coerce = T)
}
\arguments{
  \item{x}{an dataframe to round in}

  \item{digits}{how many digits to round to}

  \item{coerce}{whether to make the column numeric if it is
  not}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  A version of round() which works on dataframes that
  contain non-numeric data (or data that cannot be coerced
  to numeric) Helpful for dealing with table output that
  mixes numeric and string types.
}
\examples{
\dontrun{
umx_round(df, coerce=T)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

