\name{umx_rename_file}
\alias{umx_rename_file}
\title{umx_rename_file}
\usage{
  umx_rename_file(baseFolder = "Finder", findStr = NA,
    replaceStr = NA, listPattern = NA, test = T,
    overwrite = F)
}
\arguments{
  \item{baseFolder}{The folder to search in. If set to
  "Finder" (and you are on OS X) it will use the current
  frontmost Finder window. If it is blank, a choose folder
  dialog will be thrown.}

  \item{findStr}{= The string to find}

  \item{replaceStr}{= The replacement string}

  \item{listPattern}{= A pre-filter for files}

  \item{test}{Boolean determining whether to chagne the
  names, or just report on what would have happened}

  \item{overwrite}{Boolean determining}
}
\value{
  -
}
\description{
  rename files. On OS X, the function can access the
  current frontmost Finder window. The file renaming is
  fast and, because you can use regular expressions,
  powerful
}
\examples{
\dontrun{
umx_rename_file(baseFolder = "~/Downloads/", findStr = "", replaceStr = "", test = T)
umx_rename_file(baseFolder = "Finder", findStr = "[Ss]eason +([0-9]+)", replaceStr="S\\1", test = T)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{grep}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

