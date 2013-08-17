\name{umxReRun}
\alias{umxReRun}
\title{umxRun}
\usage{
  umxReRun(lastFit, dropList = NA, regex = NA, free = F,
    value = 0, freeToStart = NA, name = NA, verbose = F,
    intervals = F, newName = "deprecated")
}
\arguments{
  \item{lastFit}{The \code{\link{mxModel}} you wish to
  update and run.}

  \item{dropList}{A list of strings. If not NA, then the
  labels listed here will be dropped (or set to the value
  and free state you specify)}

  \item{regex}{= A regular expression. If not NA, then all
  labels matching this expression will be dropped (or set
  to the value and free state you specify)}

  \item{free}{Whether to set the parameters whose labels
  you specify to free or fixed (defaults to FALSE, i.e.,
  fixed)}

  \item{freeToStart}{Whether to only update parameters
  which are free to start (defaults to NA - i.e, not
  checked)}

  \item{name}{The name for the new model)}

  \item{verbose}{The name for the new model)}

  \item{intervals}{whether to run confidence intervals (see
  \code{\link{mxRun}})}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxRun is a version of \code{\link{mxRun}} which can run
  multiple times by default The main value for umxRun over
  mxRun is with raw data. It's slightly faster, but can
  also calculate the saturated and independence likelihoods
  necessary for most fit indices.
}
\examples{
fit2 = umxReRun(fit1, regex="Cs", name="AEmodel")
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxRun}}, \code{\link{umxLabel}},
  \code{\link{umxStart}}
}

