\name{umxSummary}
\alias{umxSummary}
\title{umxSummary}
\usage{
  umxSummary(model, saturatedModels = NULL,
    report = "line", showEstimates = NULL, precision = 2)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} whose fit will be
  reported}

  \item{saturatedModels}{Saturated models if needed for fit
  indices (see note below: Only needed for raw data, and
  then not if you've run umxRun}

  \item{report}{The format for the output line or table
  (default is "line")}

  \item{showEstimates}{What estimates to show: raw or
  standardized, a custom list or (default) none (just shows
  the fit indices) Options are "none|raw|std|both" (The
  default is standardized parameters. Choose none just to
  get the fit statistics)}
}
\description{
  Report the fit of a model in a compact form suitable for
  a journal. Emits a "warning" not when model fit is worse
  than accepted criterion (TLI > .95 and RMSEA < .06; (Hu &
  Bentler, 1999; Yu, 2002).
}
\examples{
\dontrun{
umxSummary(m1)
umxSummary(m1, report = "table")
umxSummary(m1, saturatedModels = umxSaturated(m1))
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}

  - Hu, L., & Bentler, P. M. (1999). Cutoff criteria for
  fit indexes in covariance structure analysis: Coventional
  criteria versus new alternatives. Structural Equation
  Modeling, 6, 1-55.

  - Yu, C.Y. (2002). Evaluating cutoff criteria of model
  fit indices for latent variable models with binary and
  continuous outcomes. University of California, Los
  Angeles, Los Angeles. Retrieved from
  \url{http://www.statmodel.com/download/Yudissertation.pdf}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

