\name{umx_is_MxModel}
\alias{umx_is_MxModel}
\title{umx_is_MxModel}
\usage{
umx_is_MxModel(obj)
}
\arguments{
  \item{obj}{an object to be tested to see if it is an
  OpenMx \code{\link{mxModel}}}
}
\value{
- Boolean
}
\description{
Utility function returning a binary answer to the question "Is this an OpenMx model?"
}
\examples{
m1 = mxModel("test")
if(umx_is_MxModel(m1)){
	message("nice OpenMx model!")
}
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{mxModel}}

Other umx misc functions: \code{\link{umxEval}};
  \code{\link{umx_add_variances}};
  \code{\link{umx_check_model}};
  \code{\link{umx_check_names}}; \code{\link{umx_has_CIs}};
  \code{\link{umx_has_been_run}};
  \code{\link{umx_has_square_brackets}};
  \code{\link{umx_is_RAM}}; \code{\link{umx_is_cov}};
  \code{\link{umx_is_endogenous}};
  \code{\link{umx_is_exogenous}};
  \code{\link{umx_is_ordinal}}; \code{\link{umx_reorder}};
  \code{\link{umx_string_to_algebra}}
}

