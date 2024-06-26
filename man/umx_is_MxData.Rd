% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_is_MxData}
\alias{umx_is_MxData}
\title{Check if an object is an mxData object}
\usage{
umx_is_MxData(x)
}
\arguments{
\item{x}{An object to test for being an MxData object}
}
\value{
- Boolean
}
\description{
Is the input an MxData?
}
\examples{
umx_is_MxData(mtcars)
umx_is_MxData(mxData(mtcars, type= "raw"))
umx_is_MxData(mxData(cov(mtcars), type= "cov", numObs = 73))
umx_is_MxData(mxDataWLS(na.omit(twinData[, c("wt1", "wt2")]), type= "WLS"))
}
\references{
- <https://github.com/tbates/umx>
}
\seealso{
Other Test: 
\code{\link{umx_check}()},
\code{\link{umx_check_OS}()},
\code{\link{umx_check_model}()},
\code{\link{umx_check_names}()},
\code{\link{umx_check_parallel}()},
\code{\link{umx_has_CIs}()},
\code{\link{umx_has_been_run}()},
\code{\link{umx_has_means}()},
\code{\link{umx_has_square_brackets}()},
\code{\link{umx_is_MxMatrix}()},
\code{\link{umx_is_MxModel}()},
\code{\link{umx_is_RAM}()},
\code{\link{umx_is_cov}()}
}
\concept{Test}
