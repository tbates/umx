\name{umxMI}
\alias{umxMI}
\title{umxMI}
\usage{
  umxMI(model = NA, numInd = 10, typeToShow = "both",
    decreasing = T, cache = T)
}
\arguments{
  \item{model}{An \code{\link{mxModel}} for which to report
  modification indices}

  \item{numInd}{How many modifications to report}

  \item{typeToShow}{Whether to shown additions or deletions
  (default = "both")}

  \item{decreasing}{How to sort (default = T, decreasing)}

  \item{cache}{= Future function to cahce these
  time-consuming results}
}
\description{
  umxMI A function to report the top modifications which
  would improve fit.
}
\examples{
\dontrun{
umxMI_top(model)
umxMI_top(model, numInd=5, typeToShow="add") # valid options are "both|add|delete"
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxAdd1}}, \code{\link{umxDrop1}},
  \code{\link{umxRun}}, \code{\link{umxSummary}}
}

