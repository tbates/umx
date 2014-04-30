\name{umxMakeThresholdMatrix}
\alias{umxMakeThresholdMatrix}
\title{umxMakeThresholdMatrix}
\usage{
umxMakeThresholdMatrix(df, deviationBased = T, droplevels = F,
  verbose = F)
}
\arguments{
  \item{df}{Dataframe for which to make a threshold
  matrix.}

  \item{deviationBased}{whether to use the deviation system
  to ensure order thresholds (default = TRUE)}

  \item{droplevels}{whether to also drop unused levels
  (default = TRUE)}

  \item{verbose}{whether to say what the function is doing
  (default = FALSE)}
}
\value{
- \code{\link{mxModel}}
}
\description{
The purpose of this function is to generate an mxRAMObjective. It is used by \code{\link{umxThresholdRAMObjective}}.
You likely want that, not this.
}
\examples{
umxMakeThresholdMatrix(mtcars, verbose = T)
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other advanced umx helpers: \code{\link{umxThresholdRAMObjective}}
}

