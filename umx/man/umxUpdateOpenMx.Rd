\name{umxUpdateOpenMx}
\alias{umxUpdateOpenMx}
\title{umxUpdateOpenMx}
\usage{
  umxUpdateOpenMx(bleedingEdge = F, loadNew = T, anyOK = F)
}
\arguments{
  \item{bleedingEdge}{A Boolean determining whether to
  request the beta (TRUE) or relase version (defaults to
  FALSE)}

  \item{loadNew}{A Boolean parameter determining whether to
  load the library after (optionally) updating}

  \item{anyOK}{The minimum version to accept without
  updating}
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

