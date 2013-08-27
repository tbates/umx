\name{umxGraph_RAM}
\alias{umxGraph_RAM}
\title{umxGraph_RAM}
\usage{
  umxGraph_RAM(model = NA, std = T, precision = 2,
    dotFilename = "name", pathLabels = "none",
    showFixed = F, showError = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to make a path
  diagram from}

  \item{std}{Whether to standardize the model.}

  \item{precision}{The number of decimal places to add to
  the path coefficients}

  \item{dotFilename}{A file to write the path model to. if
  you leave it at the default "name", then the model's
  internal name will be used}

  \item{pathLabels}{whether to show labels on the paths.
  both will show both the parameter and the label. ("both",
  "none" or "labels")}

  \item{showFixed}{whether to show fixed paths (defaults to
  FALSE)}

  \item{showError}{whether to show errors}
}
\description{
  A function to create graphical path diagrams from your
  OpenMx models!
}
\examples{
\dontrun{
umxGraph_RAM(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

