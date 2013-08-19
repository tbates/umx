\name{umxUpdateOpenMx}
\alias{umxUpdateOpenMx}
\title{umxUpdateOpenMx}
\usage{
  umxUpdateOpenMx(bleedingEdge = F, loadNew = T, anyOK = F)
}
\arguments{
  \item{bleedingEdge}{binary factor determining whether to
  request the beta or relase version (F)}

  \item{loadNew}{binary parameter determining whether to
  load the library after (optionally) updating}

  \item{anyOK}{minimum version to accept without updating}
}
\description{
  This function automates the process of updating OpenMx
  while it is not a cran package
}
\examples{
\dontrun{
umxUpdateOpenMx()
}
}

