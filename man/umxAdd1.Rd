\name{umxAdd1}
\alias{umxAdd1}
\title{umxAdd1}
\usage{
  umxAdd1(model, pathList1, pathList2 = NULL, arrows = 2)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to alter}

  \item{pathList}{a list of variables that (currently) will
  be expanded in a set of bivariate links}
}
\value{
  table of fits
}
\description{
  Add each of a set of paths you provide to the model,
  returning a table of theire effect on fit
}
\examples{
\dontrun{
model = umxAdd1(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxDrop1}}, \code{\link{umxModel}}
}

