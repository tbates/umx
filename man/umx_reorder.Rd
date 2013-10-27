\name{umx_reorder}
\alias{umx_reorder}
\title{umx_reorder}
\usage{
  umx_reorder(old, newOrder)
}
\arguments{
  \item{old}{a square matrix of correlation or covariances
  to reorder}

  \item{newOrder}{The order you'd like the variables to be
  in}
}
\value{
  - the re-ordered (and/or resized) matrix
}
\description{
  Reorder the variables in a correlation matrix. Can also
  remove one or more variables from a matrix using this
  function
}
\examples{
\dontrun{
reorderedMatrix = umx_reorder(oldMatrix, newOrder)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

