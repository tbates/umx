% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financial functions.R
\name{plot.percent}
\alias{plot.percent}
\title{Plot a percent change graph}
\usage{
\method{plot}{percent}(x, ...)
}
\arguments{
\item{x}{percent object.}

\item{...}{further arguments passed to or from other methods.}
}
\value{
\itemize{
\item invisible
}
}
\description{
Plot method for "percent" objects: e.g. \code{\link[=fin_percent]{fin_percent()}}.
}
\examples{
# Percent needed to return to original value after 10\% off
fin_percent(-10)
# Percent needed to return to original value after 10\% on
tmp = fin_percent(10)
plot(tmp)

# Percent needed to return to original value after 50\% off 34.50
fin_percent(-50, value = 34.5, logY = FALSE)

}
\seealso{
\itemize{
\item \code{\link[=fin_percent]{fin_percent()}}
}
}
