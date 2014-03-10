\name{xmuLabel_MATRIX_Model}
\alias{xmuLabel_MATRIX_Model}
\title{xmuLabel_MATRIX_Model (not a user function)}
\usage{
xmuLabel_MATRIX_Model(model, suffix = "", verbose = T)
}
\arguments{
  \item{model}{a model to label}

  \item{suffix}{a string to append to each label}

  \item{verbose}{how much feedback to give}
}
\value{
- \code{\link{mxModel}}
}
\description{
This function label all the free parameters in a (non-RAM)
OpenMx \code{\link{mxModel}} nb: We don't assume what each
matrix is for, and just stick a_r1c1 into each cell
}
\examples{
\dontrun{
model = xmuLabel_MATRIX_Model(model)
model = xmuLabel_MATRIX_Model(model, suffix = "male")
}
}
\references{
- http://openmx.psyc.virginia.edu/
}
\seealso{
- \code{\link{umxLabel}}
}

