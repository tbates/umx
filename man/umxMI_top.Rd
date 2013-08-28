\name{umxMI_top}
\alias{umxMI_top}
\title{umxMI_top}
\usage{
  umxMI_top(model = NA, numInd = 10, typeToShow = "both",
    decreasing = T, cache = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to report
  modification indices for}

  \item{numInd}{= How many modifications to report}

  \item{typeToShow}{= Whether to additions or deletions
  (default = "both")}

  \item{decreasing}{= How to sort (default = T,
  decreasing)}

  \item{cache}{= Future function to cahce these
  time-consuming results}
}
\description{
  umxMI_top A function to report the top modifications
  which would improve fit.
}
\examples{
\dontrun{
umxMI_top(model)
umxMI_top(model, numInd=5, typeToShow="add") # valid options are "both|add|delete"
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{umxMI_top}}, \code{\link{umxRun}},
  \code{\link{umxReportFit}}
}

