\name{umxValues}
\alias{umxValues}
\title{umxValues}
\usage{
umxValues(obj = NA, sd = NA, n = 1, onlyTouchZeros = F)
}
\arguments{
  \item{obj}{The RAM or matrix \code{\link{mxModel}}, or
  \code{\link{mxMatrix}} that you want to set start values
  for.}

  \item{sd}{Optional Standard Deviation for start values}

  \item{n}{Optional Mean for start values}

  \item{onlyTouchZeros}{Don't start things that appear to
  have already been started (useful for speeding
  \code{\link{umxReRun}})}
}
\value{
- \code{\link{mxModel}} with updated start values
}
\description{
umxValues will set start values for the free parameters in RAM and Matrix \code{\link{mxModel}}s, or even mxMatrices.
It will try and be smart in guessing these from the values in your data, and the model type.
If you give it a numeric input, it will use obj as the mean, return a list of length n, with sd = sd
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
mxEval(S, m1) # default variances are 0
m1 = umxValues(m1)
mxEval(S, m1) # plausible variances
umxValues(14, sd = 1, n = 10) # return vector of length 10, with mean 14 and sd 1
# # TODO get this working
# umx_print(mxEval(S,m1), 2, zero.print= ".") # plausible variances
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- Core functions:

Other umx core functions: \code{\link{col.as.numeric}},
  \code{\link{cor.prob}}, \code{\link{genEpiCompare}},
  \code{\link{genEpi_EvalQuote}},
  \code{\link{genEpi_GetLabels}},
  \code{\link{genEpi_Jiggle}}, \code{\link{genEpi_Path}},
  \code{\link{genEpi_ReRun}}, \code{\link{genEpi_equate}},
  \code{\link{graphViz_Cholesky}},
  \code{\link{grepSPSS_labels}},
  \code{\link{hasSquareBrackets}}, \code{\link{makeACE}},
  \code{\link{mxAutoThreshRAMObjective}},
  \code{\link{mxLatent}},
  \code{\link{mxMakeThresholdsMatrices}},
  \code{\link{mxStart}}, \code{\link{standardizeRAM}},
  \code{\link{stringToMxAlgebra}},
  \code{\link{summaryACEFit}},
  \code{\link{summaryCommonFit}}, \code{\link{tryHard}},
  \code{\link{twoGroupCholesky}},
  \code{\link{twoGroupCommonPath}},
  \code{\link{twoGroupIndependentPath}},
  \code{\link{twoGroupOrdinal}},
  \code{\link{two_group_G_by_E}},
  \code{\link{umx-deprecated}}, \code{\link{umxGetLabels}},
  \code{\link{umxGraph_RAM}}, \code{\link{umxLabeler}},
  \code{\link{umxLabels}}, \code{\link{umxPath}},
  \code{\link{umxPlotCholesky}},
  \code{\link{umxReportCIs}}, \code{\link{umxReportFit}},
  \code{\link{umxReportFit}}, \code{\link{umxStart}},
  \code{\link{umxTryHard}}, \code{\link{umx_grep_labels}},
  \code{\link{umx_u_APA_pval}}; \code{\link{umxLabel}};
  \code{\link{umxReRun}}; \code{\link{umxRun}};
  \code{\link{umx}}, \code{\link{umx-package}},
  \code{\link{umx-package}}
}

