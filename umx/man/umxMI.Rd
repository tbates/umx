\name{umxMI}
\alias{umxMI}
\title{umxMI}
\usage{
  umxMI(model, vector = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to derive
  modification indices for}

  \item{vector}{= Whether to report the results as a vector
  default = TRUE}
}
\description{
  umxMI A function to compute and report modifications
  which would improve fit. You will probably use
  \code{\link{umxMI_top}} instead
}
\examples{
\dontrun{
umxMI(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{umxMI_top}}, \code{\link{umxRun}},
  \code{\link{umxReportFit}}
}

