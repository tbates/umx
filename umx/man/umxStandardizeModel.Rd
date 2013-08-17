\name{umxStandardizeModel}
\alias{umxStandardizeModel}
\title{umxStandardizeModel}
\usage{
  umxStandardizeModel(model, return = "parameters",
    Amatrix = NA, Smatrix = NA, Mmatrix = NA)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you wish to
  standardise}

  \item{return}{Whether you want a standardize model
  \code{\link{mxModel}}, just the parameters (default) or
  matrices: valid options: "parameters", "matrices", or
  "model"}

  \item{Amatrix}{Optionally tell the function what the name
  of the asymmetric matrix is (defaults to RAM standard A)}

  \item{Smatrix}{Optionally tell the function what the name
  of the symmetric matrix is (defaults to RAM standard S)}

  \item{Mmatrix}{Optionally tell the function what the name
  of the means matrix is (defaults to RAM standard M)}
}
\value{
  - a \code{\link{mxModel}} or else parameters or matrices
  if you request those
}
\description{
  umxStandardizeModel takes a RAM-style model, and returns
  standardized version.
}
\examples{
model = umxStandardizeModel(model, return = "model")
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{omxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

