\name{xmuMI}
\alias{xmuMI}
\title{xmuMI}
\usage{
xmuMI(model, vector = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to derive
  modification indices for}

  \item{vector}{= Whether to report the results as a vector
  default = TRUE}
}
\description{
A function to compute and report modifications which would improve fit.
You will probably use \code{\link{umxMI}} instead
}
\examples{
\dontrun{
xmuMI(model)
}
}
\references{
-
}
\seealso{
- \code{\link{umxMI}}, \code{\link{umxAdd1}}, \code{\link{umxDrop1}}, \code{\link{umxRun}}, \code{\link{umxSummary}}
}

