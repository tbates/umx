% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_is_endogenous}
\alias{umx_is_endogenous}
\title{List endogenous variables in a model}
\usage{
umx_is_endogenous(model, manifests_only = TRUE)
}
\arguments{
\item{model}{an \code{\link[OpenMx:mxModel]{OpenMx::mxModel()}} from which to get endogenous variables}

\item{manifests_only}{Whether to check only manifests (default = TRUE)}
}
\value{
\itemize{
\item list of endogenous variables
}
}
\description{
Return a list of all the endogenous variables (variables with at least one incoming single-arrow path) in a model.
}
\examples{
\dontrun{
require(umx)
data(demoOneFactor)
m1 = umxRAM("umx_is_endogenous", data = demoOneFactor, type = "cov",
	umxPath("g", to = names(demoOneFactor)),
	umxPath(var = "g", fixedAt = 1),
	umxPath(var = names(demoOneFactor))
)
umx_is_endogenous(m1, manifests_only = TRUE)
umx_is_endogenous(m1, manifests_only = FALSE)

}
}
\references{
\itemize{
\item \url{https://tbates.github.io}, \url{https://github.com/tbates/umx}
}
}
\seealso{
Other Check or test: 
\code{\link{umx}},
\code{\link{umx_check_names}()},
\code{\link{umx_is_class}()},
\code{\link{umx_is_exogenous}()},
\code{\link{umx_is_numeric}()},
\code{\link{umx_is_ordered}()}
}
\concept{Check or test}
