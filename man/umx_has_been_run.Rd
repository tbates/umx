\name{umx_has_been_run}
\alias{umx_has_been_run}
\title{umx_has_been_run}
\usage{
umx_has_been_run(model, stop = FALSE)
}
\arguments{
  \item{model}{The \code{\link{mxModel}} you want to check
  has been run}

  \item{stop}{Whether to stop if the model has not been run
  (defaults to FALSE)}
}
\value{
- boolean
}
\description{
check if an mxModel has been run or not
}
\examples{
\dontrun{
umx_has_been_run(model)
}
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other umx misc functions: \code{\link{umxEval}};
  \code{\link{umx_add_variances}};
  \code{\link{umx_check_model}};
  \code{\link{umx_check_names}}; \code{\link{umx_has_CIs}};
  \code{\link{umx_has_square_brackets}};
  \code{\link{umx_is_MxModel}}; \code{\link{umx_is_RAM}};
  \code{\link{umx_is_cov}};
  \code{\link{umx_is_endogenous}};
  \code{\link{umx_is_exogenous}};
  \code{\link{umx_is_ordinal}}; \code{\link{umx_reorder}};
  \code{\link{umx_string_to_algebra}}
}

