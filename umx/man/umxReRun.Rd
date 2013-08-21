\name{umxReRun}
\alias{umxReRun}
\title{umxReRun}
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

  \item{regex}{A regular expression. If not NA, then all
  labels matching this expression will be dropped (or set
  to the value and free state you specify)}

  \item{free}{Whether to set the parameters whose labels
  you specify to free or fixed (defaults to FALSE, i.e.,
  fixed)}

  \item{value}{The value to set the parameters whose labels
  you specify too (defaults to 0)}

  \item{freeToStart}{Whether to only update parameters
  which are free to start (defaults to NA - i.e, not
  checked)}

  \item{name}{The name for the new model}

  \item{verbose}{How much feedback to give}

  \item{intervals}{Whether to run confidence intervals (see
  \code{\link{mxRun}})}

  \item{newName}{DEPRECATED! equivalent of name. Use name =
  to change name of the new model}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxReRun Is a convenience function to re-run an
  \code{\link{mxModel}}, optionally dropping parameters The
  main value for umxReRun is compactness. So this one-liner
  drops a path labelled "Cs", and returns the updated
  model: fit2 = umxReRun(fit1, dropList = "Cs", name =
  "newModelName")
}
\details{
  If you're a beginner, stick to fit2 =
  omxSetParameters(fit1, labels = "Cs", values = 0, free =
  F, name = "newModelName") fit2 = mxRun(fit2)
}
\examples{
\dontrun{
fit2 = umxReRun(fit1, regex = "Cs", name = "drop_cs")
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{mxRun}}, \code{\link{umxLabel}},
  \code{\link{omxGetParameters}}
}

