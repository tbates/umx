% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.r
\name{umx_set_table_format}
\alias{umx_set_table_format}
\title{umx_set_table_format}
\usage{
umx_set_table_format(knitr.table.format = NULL)
}
\arguments{
\item{knitr.table.format}{format for tables (if empty, returns the current value of knitr.table.format)}
}
\value{
- Current knitr.table.format setting
}
\description{
Set knitr.table.format default (output style for tables). Legal values are 
"latex", "html", "markdown", "pandoc", and "rst".
}
\examples{
library(umx)
old = umx_set_table_format() # get existing value
umx_set_table_format("latex")
umx_set_table_format("html")
umx_set_table_format("markdown")
umx_set_table_format(old)    # reinstate
}
\references{
- \url{http://tbates.github.io}, \url{https://github.com/tbates/umx}
}
\seealso{
Other Get and set: \code{\link{umx_get_checkpoint}},
  \code{\link{umx_get_cores}},
  \code{\link{umx_get_optimizer}},
  \code{\link{umx_set_auto_plot}},
  \code{\link{umx_set_auto_run}},
  \code{\link{umx_set_checkpoint}},
  \code{\link{umx_set_condensed_slots}},
  \code{\link{umx_set_cores}},
  \code{\link{umx_set_optimizer}},
  \code{\link{umx_set_plot_format}}
}
