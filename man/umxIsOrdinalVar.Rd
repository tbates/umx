\name{umxIsOrdinalVar}
\alias{umxIsOrdinalVar}
\title{umxIsOrdinalVar}
\usage{
  umxIsOrdinalVar(df, names = F)
}
\arguments{
  \item{df}{an \code{\link{data.frame}} to look in for
  ordinal variables}
}
\value{
  - list of variable names
}
\description{
  return the names of any ordinal variables in a dataframe
}
\examples{
\dontrun{
umxIsOrdinalVar(df)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

