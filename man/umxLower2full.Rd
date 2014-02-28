\name{umxLower2full}
\alias{umxLower2full}
\title{umxLower2full}
\usage{
umxLower2full(lower.data, diag = F, byrow = T)
}
\arguments{
  \item{lower.data}{An \code{\link{mxMatrix}}}

  \item{diag}{A boolean noting whether the lower matrix
  includes the diagonal}

  \item{byrow}{Whether the matrix is to be filled by row or
  by column}
}
\value{
- \code{\link{mxMatrix}}
}
\description{
Take a lower triangle of data (either from a "lower"
\code{\link{mxMatrix}}, or entered from as you might see in
a journal article) and turn it into a full matrix
}
\examples{
\dontrun{
matrix = umxLower2full(matrix)
lower2full(lower.tri, diag = F)
lower2full(lower.data, diag = T, byrow = F)
lower2full(lower.no.diag, diag = F, byrow = F)
lower2full(lower.bycol, diag = T, byrow = F)
lower2full(lower.byrow, diag = T, byrow = T)
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

