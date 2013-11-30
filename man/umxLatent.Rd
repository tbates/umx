\name{umxLatent}
\alias{umxLatent}
\title{umxLatent}
\usage{
  umxLatent(latent = NULL, formedBy = NULL, forms = NULL,
    data = NULL, endogenous = FALSE, model.name = NULL,
    labelSuffix = "", verbose = T)
}
\arguments{
  \item{latent}{the name of the latent variable (string)}

  \item{formedBy}{the list of variables forming this
  latent}

  \item{forms}{the list of variables which this latent
  forms (leave blank for formedBy)}

  \item{data}{the dataframe being used in this model}

  \item{endogenous}{= FALSE}

  \item{model.name}{= NULL}

  \item{labelSuffix}{a string to add to the end of each
  label}

  \item{verbose}{= T}
}
\value{
  - path list
}
\description{
  Helper to ease the creation of latent variables,
  including formative variables (where the manifests define
  the latent, and need wiring up behind, and variances
  setting) The following figures show how a reflective:
  \figure{reflective.png} and a formative variable are
  created: formative\figure{formative.png}
}
\examples{
\dontrun{
umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)

library(OpenMx)
library(umx)
data(demoOneFactor)
latents = c("G")
manifests = names(demoOneFactor) x1-5
theData = cov(demoOneFactor)
m1 = mxModel("reflective", type = "RAM",
    manifestVars = manifests,
    latentVars   = latents,
    # Factor loadings
    umxLatent("G", forms = manifests, data = theData),
	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
)
m1 = umxRun(m1, setStarts = T, setLabels = T); summary(m1)
umxPlot(m1)

m2 = mxModel("formative", type = "RAM",
    manifestVars = manifests,
    latentVars   = latents,
    # Factor loadings
    umxLatent("G", formedBy = manifests, data = theData),
	mxData(theData, type = "cov", numObs = nrow(demoOneFactor))
)
m2 = umxRun(m2, setStarts = T, setLabels = T); summary(m2)
umxPlot(m2)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

