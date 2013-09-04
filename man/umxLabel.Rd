\name{umxLabel}
\alias{umxLabel}
\title{umxLabel}
\usage{
  umxLabel(obj, suffix = "", baseName = NA, setfree = F,
    drop = 0, labelFixedCells = T, jiggle = NA,
    boundDiag = NA, verbose = F, overRideExisting = F)
}
\arguments{
  \item{obj}{An \code{\link{mxModel}} (RAM or matrix
  based), \code{\link{mxPath}}, or \code{\link{mxMatrix}}}

  \item{suffix}{String to append to each label (might be
  used to distinguish, say male and female submodels in a
  model)}

  \item{baseName}{String to prepend to labels. Defaults to
  empty}

  \item{setfree}{Whether to also}

  \item{drop}{The value to fix "drop" paths to (defaults to
  0)}

  \item{jiggle}{How much to jiggle values in a matrix or
  list of path values}

  \item{boundDiag}{Whether to bound the diagonal of a
  matrix}

  \item{verbose}{How much feedback to give the user
  (default = F)}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxLabel adds labels to things, be it an:
  \code{\link{mxModel}} (RAM or matrix based), an
  \code{\link{mxPath}}, or an \code{\link{mxMatrix}} This
  is a core function in umx: Adding labels to paths opens
  the door to \code{\link{umxEquate}}, as well as
  \code{\link{omxSetParameters}}
}
\examples{
\dontrun{
 model = umxLabel(model)
 umxLabel(mxMatrix("Full", 3,3, values = 1:9, name = "a"))
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxGetParameters}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

