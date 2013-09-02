\name{umxGetParameters}
\alias{umxGetParameters}
\title{umxGetParameters}
\usage{
  umxGetParameters(inputTarget, regex = NA, free = NA,
    verbose = F)
}
\arguments{
  \item{inputTarget}{An object to get parameters from:
  could be a RAM \code{\link{mxModel}}}

  \item{regex}{A regular expression to filter the labels
  defaults to NA - just returns all labels)}

  \item{free}{A Boolean determining whether to return only
  free parameters.}

  \item{verbose}{How much feedback to give}
}
\description{
  Get the parameter labels from a model. Like
  \code{\link{omxGetParameters}}, but supercharged with
  regular expressions for more power and ease!
}
\examples{
\dontrun{
umxGetParameters(model)
umxGetParameters(mxEval("as", model1)) # all labels of matrix "as" in model "model1"
umxGetParameters(model, regex = "as_r_2c_[0-9]", free = T) # get all columns in row 2 of matrix "as"
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{omxGetParameters}}, \code{\link{umxLabel}},
  \code{\link{umxRun}}
}

