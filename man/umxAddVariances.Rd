\name{umxAddVariances}
\alias{umxAddVariances}
\title{umxAddVariances}
\usage{
umxAddVariances(model, add.to = c("all", "manifests", "latents"))
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to add variances
  to}

  \item{add.to}{= c("all", "manifests", "latents")}
}
\value{
- \code{\link{mxModel}}
}
\description{
Convenience function to save the user specifying mxpaths adding variance to each variable
}
\examples{
\dontrun{
model = umxAddVariances(model)
}
}
\references{
- \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
}
\seealso{
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
  \code{\link{umxValues}}; \code{\link{umx_show}};
  \code{\link{umx}}, \code{\link{umx-package}},
  \code{\link{umx-package}}
}

