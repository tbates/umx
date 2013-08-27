\name{umxGetParameters}
\alias{umxGetParameters}
\title{umxGetParameters}
\usage{
  umxGetParameters(inputTarget, regex = NA, free = NA,
    verbose = F)
}
\arguments{
  \item{inputTarget}{An object to get parameters from}

  \item{regex}{A regular expression to filter the labels
  defaults to NA - no filtering)}

  \item{free}{A Boolean determining whether to return free
  parameters or not.}

  \item{verbose}{How much feedback to give}
}
\description{
  Get the parameter labels from a model. enabled with
  regular expressions for more power and ease!
}
\examples{
\dontrun{
umxGetParameters(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{omxGetParameters}}
}

