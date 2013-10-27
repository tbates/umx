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
  - the re-ordered matrix
}
\description{
  Reorder the variables in a correlation matrix
}
\examples{
\dontrun{
het_dz = umx_reorder(het_dz, newNameOrder)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

