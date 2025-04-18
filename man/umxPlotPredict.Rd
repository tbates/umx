% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_and_reporting.R
\name{umxPlotPredict}
\alias{umxPlotPredict}
\title{\code{umxPlotPredict} Take a model and plot the y against predicted(y)}
\usage{
umxPlotPredict(
  model,
  xlab = "Predicted Y",
  ylab = "Observed Y",
  r2x = 1.5,
  r2y = 4.5,
  font_size = 13,
  rsq = FALSE,
  font = "Times"
)
}
\arguments{
\item{model}{lm or other model that understands predict()}

\item{xlab}{X-axis label (default x).}

\item{ylab}{Y-axis label (default y).}

\item{r2x}{x location for the fit summary.}

\item{r2y}{y location for the fit summary.}

\item{font_size}{Default 13}

\item{rsq}{R^2 or r (defaults to FALSE = r)}

\item{font}{Default "Times"}
}
\value{
\itemize{
\item plot you can edit.
}
}
\description{
\code{umxPlotPredict} is a function which
}
\examples{
data(mtcars)
tmp = lm(mpg ~ wt, data = mtcars)
umxPlotPredict(tmp, r2x = 2, r2y = 10)
}
\seealso{
\itemize{
\item \code{\link[ggplot2:qplot]{ggplot2::qplot()}}
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
\code{\link{umxPlotCP}()},
\code{\link{umxPlotDoC}()},
\code{\link{umxPlotFun}()},
\code{\link{umxPlotGxE}()},
\code{\link{umxPlotGxEbiv}()},
\code{\link{umxPlotIP}()},
\code{\link{umxPlotSexLim}()},
\code{\link{umxPlotSimplex}()}
}
\concept{Plotting functions}
