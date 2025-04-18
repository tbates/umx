% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_set_cores}
\alias{umx_set_cores}
\title{umx_set_cores}
\usage{
umx_set_cores(cores = NA, model = NULL, silent = FALSE)
}
\arguments{
\item{cores}{number of cores to use. NA (the default) returns current value. "-1" will set to \code{omxDetectCores()}.}

\item{model}{an (optional) model to set. If left NULL, the global option is updated.}

\item{silent}{If TRUE, no message will be printed.}
}
\value{
\itemize{
\item number of cores
}
}
\description{
set the number of cores (threads) used by OpenMx
}
\examples{
library(umx)
manifests = c("mpg", "disp", "gear")
m1 = mxModel("ind", type = "RAM",
	manifestVars = manifests,
	mxPath(from = manifests, arrows = 2),
	mxPath(from = "one", to = manifests),
	mxData(mtcars[, manifests], type = "raw")
)
umx_set_cores() # print current value
oldCores = umx_set_cores(silent = TRUE)  # store existing value
umx_set_cores(omxDetectCores()) # set to max
umx_set_cores(-1); umx_set_cores() # set to max
m1 = umx_set_cores(1, m1)  # set m1 usage to 1 core
umx_set_cores(model = m1)  # show new value for m1
umx_set_cores(oldCores)    # reinstate old global value
}
\references{
\itemize{
\item \url{https://tbates.github.io},  \url{https://github.com/tbates/umx}
}
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
\code{\link{umx_set_data_variance_check}()},
\code{\link{umx_set_dollar_symbol}()},
\code{\link{umx_set_optimization_options}()},
\code{\link{umx_set_optimizer}()},
\code{\link{umx_set_plot_file_suffix}()},
\code{\link{umx_set_plot_format}()},
\code{\link{umx_set_plot_use_hrbrthemes}()},
\code{\link{umx_set_separator}()},
\code{\link{umx_set_silent}()},
\code{\link{umx_set_table_format}()}
}
\concept{Get and set}
