\name{umxCompare}
\alias{umxCompare}
\title{umxCompare}
\usage{
  umxCompare(base = NULL, comparison = NULL, all = TRUE,
    output = "return")
}
\arguments{
  \item{base}{The base \code{\link{mxModel}} for
  comparison}

  \item{comparison}{The model (or list of models) which
  will be compared for fit with the base model (can be
  empty)}

  \item{all}{Whether to make all possible comparisons if
  there is more than one base model (defaults to T)}

  \item{output}{Optionally output to an html table which
  will open your default browser: handy for getting tables
  into word}
}
\description{
  umxCompare compares two or more \code{\link{mxModel}}s.
  If you leave comparison blank, it will just give fit info
  for the base model
}
\examples{
\dontrun{
umxCompare(model1, model2)
umxCompare(model1, c(model2, model3))
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu/}
}
\seealso{
  - \code{\link{mxCompare}}, \code{\link{umxSummary}},
  \code{\link{umxRun}},
}

