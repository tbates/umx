\name{umx_show}
\alias{umx_show}
\title{umx_show}
\usage{
umx_show(model, what = c("values", "free", "labels"), matrices = c("S",
  "A"))
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to show data from}

  \item{what}{legal options are "values" (default), "free",
  or "labels")}

  \item{matrices}{to show (default is c("S", "A"))}
}
\value{
- \code{\link{mxModel}}
}
\description{
Show matrix contents. The user can select  values, free, and/or labels, and which matrices to display
}
\examples{
\dontrun{
model = umx_show(model)
}
}
\references{
- \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxStart}}

Other umx core functions: \code{\link{umxLabel}};
  \code{\link{umxReRun}}; \code{\link{umxRun}};
  \code{\link{umxValues}}; \code{\link{umx}},
  \code{\link{umx-package}}, \code{\link{umx-package}}
}

