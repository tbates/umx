% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_set_plot_use_hrbrthemes}
\alias{umx_set_plot_use_hrbrthemes}
\title{Set theme system to use for plots.}
\usage{
umx_set_plot_use_hrbrthemes(umx.plot.use_hrbrthemes = NULL, silent = FALSE)
}
\arguments{
\item{umx.plot.use_hrbrthemes}{whether to them plots with hrbrthemes (if empty returns the current value)}

\item{silent}{If TRUE, no message will be printed.}
}
\value{
\itemize{
\item Current setting
}
}
\description{
Set output file suffix (default = "gv", alternative is "dot"). If you call this with no
value, it will return the current setting. If you call it with TRUE, it toggles the setting.
}
\examples{
umx_set_plot_use_hrbrthemes() # print current state
old = umx_set_plot_use_hrbrthemes(silent = TRUE) # store current value
umx_set_plot_use_hrbrthemes(TRUE)
umx_set_plot_use_hrbrthemes(old) # reinstate
}
\seealso{
Other Get and set: 
\code{\link{umx}},
\code{\link{umx_get_alphas}()},
\code{\link{umx_get_checkpoint}()},
\code{\link{umx_get_options}()},
\code{\link{umx_set_auto_plot}()},
\code{\link{umx_set_auto_run}()},
\code{\link{umx_set_checkpoint}()},
\code{\link{umx_set_condensed_slots}()},
\code{\link{umx_set_cores}()},
\code{\link{umx_set_data_variance_check}()},
\code{\link{umx_set_dollar_symbol}()},
\code{\link{umx_set_optimization_options}()},
\code{\link{umx_set_optimizer}()},
\code{\link{umx_set_plot_file_suffix}()},
\code{\link{umx_set_plot_format}()},
\code{\link{umx_set_separator}()},
\code{\link{umx_set_silent}()},
\code{\link{umx_set_table_format}()}
}
\concept{Get and set}
