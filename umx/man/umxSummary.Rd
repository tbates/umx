\name{umxSummary}
\alias{umxSummary}
\title{umxSummary}
\usage{
  umxSummary(model, saturatedModels = NULL, precision = 2,
    displayColumns = c("row", "col", "Std.Estimate"),
    report = NA)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to summarise}

  \item{saturatedModels}{Option saturated models as a basis
  for comparison for model}

  \item{precision}{: the number of decimal places you want
  for p-values etc. (defaults to 2)}

  \item{displayColumns}{the subset of fit columns to show
  in the summary}

  \item{report}{Currently not used - but will allow users
  to request different forms of report}
}
\description{
  umxSummary is a modified summary function for
  \code{\link{OpenMx}}, designed to give you the
  information you need for writing paper. Given a model, it
  returns a simplified version of the path loadings, along
  with chi^2, AIC and other important fit parameters.
}
\examples{
\dontrun{
umxSummary(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{umxRun}}, \code{\link{umxReportFit}}
}

