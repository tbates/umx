\name{xmuMakeThresholdsMatrices}
\alias{xmuMakeThresholdsMatrices}
\title{xmuMakeThresholdsMatrices (not a user function)}
\usage{
xmuMakeThresholdsMatrices(df, droplevels = F, verbose = F)
}
\arguments{
  \item{df}{a \code{\link{data.frame}} containing the data
  for your \code{\link{mxData}} statement}

  \item{droplevels}{a binary asking if empty levels should
  be dropped (defaults to FALSE)}

  \item{verbose}{how much feedback to give (defaults to
  FALSE)}
}
\value{
- a list containing an \code{\link{mxMatrix}} called "thresh",
an \code{\link{mxRAMObjective}} object, and an \code{\link{mxData}} object
}
\description{
You should not be calling this directly.
This is not as reliable a strategy and likely to be superceeded...
}
\examples{
\dontrun{
junk = xmuMakeThresholdsMatrices(df, droplevels=F, verbose=T)
}
}
\references{
- http://openmx.psyc.virginia.edu/
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
}

