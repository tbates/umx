% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_and_reporting.R
\name{umxPlotCP}
\alias{umxPlotCP}
\alias{plot.MxModelCP}
\title{Draw and display a graphical figure of Common Pathway model}
\usage{
umxPlotCP(
  x = NA,
  means = FALSE,
  std = TRUE,
  digits = 2,
  showFixed = TRUE,
  file = "name",
  format = c("current", "graphviz", "DiagrammeR"),
  SEstyle = FALSE,
  strip_zero = TRUE,
  ...
)
}
\arguments{
\item{x}{The Common Pathway \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}} to display graphically}

\item{means}{Whether to show means paths (defaults to FALSE)}

\item{std}{Whether to standardize the model (defaults to TRUE)}

\item{digits}{How many decimals to include in path loadings (defaults to 2)}

\item{showFixed}{Whether to graph paths that are fixed but != 0 (default = TRUE)}

\item{file}{The name of the dot file to write: NA = none; "name" = use the name of the model}

\item{format}{= c("current", "graphviz", "DiagrammeR")}

\item{SEstyle}{report "b (se)" instead of "b [lower, upper]" when CIs are found (Default FALSE)}

\item{strip_zero}{Whether to strip the leading "0" and decimal point from parameter estimates (default = TRUE)}

\item{...}{Optional additional parameters}
}
\value{
\itemize{
\item Optionally return the dot code
}
}
\description{
Options include digits (rounding), showing means or not, and which output format is desired.
}
\examples{
\dontrun{
require(umx)
umx_set_optimizer("SLSQP")
data(GFF)
mzData = subset(GFF, zyg_2grp == "MZ")
dzData = subset(GFF, zyg_2grp == "DZ")
selDVs = c("gff", "fc", "qol", "hap", "sat", "AD") 
m1 = umxCP("new", selDVs = selDVs, sep = "_T", 
	dzData = dzData, mzData = mzData, nFac = 3
)
# m1 = mxTryHardOrdinal(m1)
umxPlotCP(m1)
plot(m1) # No need to remember a special name: plot works fine!
}
}
\references{
\itemize{
\item \url{https://tbates.github.io}
}
}
\seealso{
\itemize{
\item \code{\link[=plot]{plot()}}, \code{\link[=umxSummary]{umxSummary()}} work for IP, CP, GxE, SAT, and ACE models.
}

\itemize{
\item \code{\link[=umxCP]{umxCP()}}
}

Other Plotting functions: 
\code{\link{ggAddR}()},
\code{\link{plot.MxLISRELModel}()},
\code{\link{plot.MxModel}()},
\code{\link{plot.MxModelTwinMaker}()},
\code{\link{umx}},
\code{\link{umxPlot}()},
\code{\link{umxPlotACE}()},
\code{\link{umxPlotACEcov}()},
\code{\link{umxPlotACEv}()},
\code{\link{umxPlotDoC}()},
\code{\link{umxPlotFun}()},
\code{\link{umxPlotGxE}()},
\code{\link{umxPlotGxEbiv}()},
\code{\link{umxPlotIP}()},
\code{\link{umxPlotPredict}()},
\code{\link{umxPlotSexLim}()},
\code{\link{umxPlotSimplex}()}
}
\concept{Plotting functions}
