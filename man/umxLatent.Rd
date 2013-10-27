\name{umxLatent}
\alias{umxLatent}
\title{umxLatent}
\usage{
  umxLatent(latent = NA, formedBy = NA, forms = NA, data,
    endogenous = FALSE, model.name = NA, help = FALSE,
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

  \item{model.name}{= NA}

  \item{help}{= FALSE}

  \item{labelSuffix}{a string to add to the end of each
  label}

  \item{verbose}{= T}
}
\value{
  - path list
}
\description{
  Helper to ease the creation of latent variables,
  including reflective variables (where the manifests
  define the latent, and need wireing up behind)
}
\examples{
\dontrun{
umxLatent(latent = NA, formedBy = manifestsOrigin, data = df)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

