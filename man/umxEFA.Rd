% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/model_builders.r
\name{umxEFA}
\alias{umxEFA}
\title{umxEFA}
\usage{
umxEFA(name = "", latents, data, report = c("table", "line", "long"))
}
\arguments{
\item{name}{the name for your new EFA model}

\item{latents}{List of factors in your CFA}

\item{data}{The dataframe of manifest columns you are modeling}

\item{report}{What to report}
}
\value{
- \code{\link{mxModel}}
}
\description{
A helper for EFA that only requires you to enter your latents and manifests
}
\examples{
\dontrun{
umxEFA("test", latents = "g", data = mtcars[, c("mpg", "disp", "hp", "wt")])
}
}
\references{
- \url{http://github.com/tbates/umx}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other Super-easy helpers: \code{\link{umx}},
  \code{\link{umx-package}}
}
