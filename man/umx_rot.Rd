% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_rot}
\alias{umx_rot}
\title{Rotate a vector}
\usage{
umx_rot(vec, na.last = FALSE)
}
\arguments{
\item{vec}{vector to rotate}

\item{na.last}{Whether to set the last value to NA (default = FALSE)}
}
\value{
\itemize{
\item \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}}
}
}
\description{
\code{umx_rot} rotates the items of a vector (1 place, by default). So: c(1,2,3) -> c(2,3,1)
}
\examples{
umx_rot(1:10)
umx_rot(c(3,4,5,6,7))
# [1] 4 5 6 7 3
}
\references{
\itemize{
\item \url{https://tbates.github.io}
}
}
\seealso{
Other String Functions: 
\code{\link{umx}},
\code{\link{umx_explode}()},
\code{\link{umx_explode_twin_names}()},
\code{\link{umx_grep}()},
\code{\link{umx_names}()},
\code{\link{umx_paste_names}()},
\code{\link{umx_str_chars}()},
\code{\link{umx_str_from_object}()},
\code{\link{umx_trim}()}
}
\concept{String Functions}
