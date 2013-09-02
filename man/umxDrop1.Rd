\name{umxDrop1}
\alias{umxDrop1}
\title{umxDrop1}
\usage{
  umxDrop1(model, regex)
}
\arguments{
  \item{model}{An \code{\link{mxModel}} to drop parameters
  from}

  \item{regex}{A string to select which parameters. e.g.
  "^a_" (begins with "a_") or "ab|ba" either "ab" or "ba"}
}
\description{
  Drops each free parameter (selected via regex), returning
  an \code{\link{mxCompare}} table comparing the effects. A
  great way to quickly determine which of several
  parameters can be dropped without excessive cost
}
\examples{
\dontrun{
drop "a_r1c1" and "a_r1c2" (separately) and see which matters more.
umxDrop1(model, regex="a_r1c1|a_r1c2")
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

