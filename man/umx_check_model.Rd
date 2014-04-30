\name{umx_check_model}
\alias{umx_check_model}
\title{umx_check_model}
\usage{
umx_check_model(obj, type = NULL, hasData = NULL, checkSubmodels = F)
}
\arguments{
  \item{obj}{an object to check}

  \item{type}{= what type the model must be (defaults to
  not checking NULL)}

  \item{hasData}{whether the model should have data or not
  (defaults to not checking NULL)}

  \item{checkSubmodels}{whether to check submodels (not
  implemented yet) (default = FALSE)}
}
\value{
- boolean
}
\description{
Check an OpenMx model
}
\examples{
require(OpenMx)
data(demoOneFactor)
latents  = c("G")
manifests = names(demoOneFactor)
m1 <- mxModel("One Factor", type = "RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = latents, arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
umx_check_model(model)
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{umx}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other umx misc functions: \code{\link{umxEval}};
  \code{\link{umx_check_names}}; \code{\link{umx_has_CIs}};
  \code{\link{umx_has_been_run}};
  \code{\link{umx_has_square_brackets}};
  \code{\link{umx_is_MxModel}}; \code{\link{umx_is_RAM}};
  \code{\link{umx_is_cov}}; \code{\link{umx_is_ordinal}};
  \code{\link{umx_reorder}};
  \code{\link{umx_string_to_algebra}}
}

