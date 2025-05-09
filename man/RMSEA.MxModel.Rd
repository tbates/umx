% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_and_reporting.R
\name{RMSEA.MxModel}
\alias{RMSEA.MxModel}
\title{RMSEA function for MxModels}
\usage{
\method{RMSEA}{MxModel}(x, ci.lower = 0.025, ci.upper = 0.975, digits = 3)
}
\arguments{
\item{x}{an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}} from which to get RMSEA}

\item{ci.lower}{the lower CI to compute (only 95\%, i.e., .025 supported)}

\item{ci.upper}{the upper CI to compute (only 95\%, i.e., .975 supported)}

\item{digits}{digits to show (default = 3)}
}
\value{
\itemize{
\item object containing the RMSEA, lower and upper bounds, and p-close
}
}
\description{
Return RMSEA and its confidence interval on a model.
RMSEA(tmp, silent=TRUE)$RMSEA
}
\examples{
\dontrun{
require(umx)
data(demoOneFactor)
manifests = names(demoOneFactor)

m1 = umxRAM("One Factor", data = demoOneFactor, type = "cov",
	umxPath("G", to = manifests),
	umxPath(var = manifests),
	umxPath(var = "G", fixedAt = 1)
)
RMSEA(m1)

x = RMSEA(m1)
x$RMSEA # 0.0309761

# Raw: needs to be run by umx to get RMSEA
m2 = umxRAM("One Factor", data = demoOneFactor,
	umxPath("G", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "G")
)
RMSEA(m2)
}
}
\references{
\itemize{
\item \url{https://github.com/tbates/umx}
}
}
\seealso{
Other Reporting functions: 
\code{\link{RMSEA}()},
\code{\link{RMSEA.summary.mxmodel}()},
\code{\link{extractAIC.MxModel}()},
\code{\link{loadings}()},
\code{\link{loadings.MxModel}()},
\code{\link{residuals.MxModel}()},
\code{\link{tmx_show}()},
\code{\link{tmx_show.MxMatrix}()},
\code{\link{umxCI}()},
\code{\link{umxCI_boot}()},
\code{\link{umxConfint}()},
\code{\link{umxExpCov}()},
\code{\link{umxExpMeans}()},
\code{\link{umxFitIndices}()},
\code{\link{umxRotate}()}
}
\concept{Reporting functions}
