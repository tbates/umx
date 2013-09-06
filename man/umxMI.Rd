\name{umxMI}
\alias{umxMI}
\title{umxMI}
\usage{
  umxMI(model = NA, numInd = 10, typeToShow = "both",
    decreasing = T, cache = T)

  umxMI(model = NA, numInd = 10, typeToShow = "both",
    decreasing = T, cache = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to derive
  modification indices for}

  \item{vector}{= Whether to report the results as a vector
  default = TRUE}

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
  umxMI A function to compute and report modifications
  which would improve fit. You will probably use
  \code{\link{umxMI_top}} instead

  umxMI A function to report the top modifications which
  would improve fit.
}
\examples{
\dontrun{
umxMI(model)
}
\dontrun{
umxMI_top(model)
umxMI_top(model, numInd=5, typeToShow="add") # valid options are "both|add|delete"
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}

  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxMI_top}}, \code{\link{umxAdd1}},
  \code{\link{umxDrop1}}, \code{\link{umxRun}},
  \code{\link{umxSummary}}

  - \code{\link{umxAdd1}}, \code{\link{umxDrop1}},
  \code{\link{umxRun}}, \code{\link{umxSummary}}
}

