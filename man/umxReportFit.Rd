\name{umxReportFit}
\alias{umxReportFit}
\title{umxReportFit}
\usage{
  umxReportFit(model, saturatedModels = NULL,
    report = "line", showEstimates = "std", precision = 2,
    displayColumns = c("row", "col", "Std.Estimate"))
}
\arguments{
  \item{model}{The \code{\link{mxModel}} whose fit will be
  reported}

  \item{saturatedModels}{Saturated models if needed for fit
  indices (see note below: Only needed for raw data, and
  then not if you've run umxRun}

  \item{report}{The format for the output (currently only a
  1-liner is supported)}

  \item{showEstimates}{Whether to show the raw or
  standadized estimates. Options are "none|raw|std|both"
  (The default is standardized parameters. Choose none just
  to get the fit statistics)}
}
\description{
  Deprecated: Please use \code{\link{umxSummary}} instead.
  Both functions report fit in a compact form suitable for
  a journal.
}
\examples{
\dontrun{
umxSummary(m1)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu} - Hu, L., &
  Bentler, P. M. (1999). Cutoff criteria for fit indexes in
  covariance structure analysis: Coventional criteria
  versus new alternatives. Structural Equation Modeling, 6,
  1-55.  - Yu, C.Y. (2002). Evaluating cutoff criteria of
  model fit indices for latent variable models with binary
  and continuous outcomes. University of California, Los
  Angeles, Los Angeles. Retrieved from
  \url{http://www.statmodel.com/download/Yudissertation.pdf}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

