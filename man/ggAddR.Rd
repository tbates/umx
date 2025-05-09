% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{ggAddR}
\alias{ggAddR}
\title{Add a fit statistic to a ggplot}
\usage{
ggAddR(model, effect = NA, xloc = 8, yloc = 10)
}
\arguments{
\item{model}{a statistical model which contains a fit measure.}

\item{effect}{optional hard coded fit/effect.}

\item{xloc}{x location of R.}

\item{yloc}{y location of R.}
}
\value{
\itemize{
\item plot
}
}
\description{
Add a fit statistic to a ggplot
}
\examples{
\dontrun{
m1 = lm(mpg ~ wt, data = mtcars)
p = ggplot2::ggplot(data = mtcars, aes(x = wt, y = mpg))+ geom_point() +geom_smooth()+
ggAddR(m1, effect = NA, xloc=2, yloc= 10); p
}
}
\seealso{
\itemize{
\item \code{\link[=umxPlot]{umxPlot()}}, \code{\link[=umxPlotFun]{umxPlotFun()}}
}

Other Plotting functions: 
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
\code{\link{umxPlotPredict}()},
\code{\link{umxPlotSexLim}()},
\code{\link{umxPlotSimplex}()}
}
\concept{Plotting functions}
