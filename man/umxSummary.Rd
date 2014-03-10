\name{umxSummary}
\alias{umxSummary}
\title{umxSummary}
\usage{
umxSummary(model, saturatedModels = NULL, report = "line",
  showEstimates = NULL, digits = 2, RMSEA_CI = F, precision = NULL)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} whose fit will be
  reported}

  \item{saturatedModels}{Saturated models if needed for fit
  indices (see example below: Only needed for raw data, and
  then not if you've run umxRun}

  \item{report}{The format for the output line or table
  (default is "line")}

  \item{showEstimates}{What estimates to show. Options are
  "raw | std | list | NULL" for raw, standardized, a custom
  list or (default) none (just shows the fit indices)}
}
\description{
Report the fit of a model in a compact form suitable for a
journal. Emits a "warning" not when model fit is worse than
accepted criterion (TLI > .95 and RMSEA < .06; (Hu &
Bentler, 1999; Yu, 2002).
}
\details{
notes on CIs and Identification Note, the conventional
standard errors reported by OpenMx are used to produce the
CIs you see in umxSummary These are used to derive
confidence intervals based on the formula 95%CI = estimate
+/- 1.96*SE)

Sometimes they appear NA. This often indicates an model
which is not \url{http://identified}. This can include
empirical under-identification - for instance two factors
that are essentially identical in structure.

A signature of this would be paths estimated at or close to
zero. Fixing one or two of these to zero may fix the
standard error calculation, and alleviate the need to
estimate likelihood-based or bootstrap CIs

If factor loadings can flip sign and provide identical fit,
this creates another form of under-identification and can
break confidence interval estimation, but I think Fixing a
factor loading to 1 and estimating factor variances can
help here
}
\examples{
require(OpenMx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1, show = "std")
umxSummary(m1, show = "std", digits = 1)
\dontrun{
umxSummary(m1, report = "table")
umxSummary(m1, saturatedModels = umxSaturated(m1))
}
}
\references{
- Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit
indexes in covariance structure analysis: Coventional
criteria versus new alternatives. Structural Equation
Modeling, 6, 1-55.

- Yu, C.Y. (2002). Evaluating cutoff criteria of model fit
indices for latent variable models with binary and
continuous outcomes. University of California, Los Angeles,
Los Angeles.  Retrieved from
\url{http://www.statmodel.com/download/Yudissertation.pdf}
\url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{mxCI}},
\code{\link{umxCI}},\code{\link{umxCI_boot}},
\code{\link{umxLabel}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

