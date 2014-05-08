\name{umxRAM}
\alias{umxRAM}
\title{umxRAM}
\usage{
umxRAM(name, ..., exog.variances = FALSE, endog.variances = TRUE,
  fix = c("none", "latents", "firstLoadings"), latentVars = NULL,
  data = NULL, independent = NA, remove_unused_manifests = TRUE)
}
\arguments{
  \item{name}{friendly name for the model}

  \item{...}{A list of paths and a data source}

  \item{exog.variances}{If TRUE (the default is FALSE),
  free variance parameters are added for exogenous
  variables that lack them.}

  \item{endog.variances}{If TRUE (the default), free
  error-variance parameters are added for any endogenous
  variables that lack them.}

  \item{fix}{Whether to fix latent or first paths to 1.
  Options are: c("none", "latents", "firstLoadings")}

  \item{independent}{Whether the model is independent
  (default = NA)}
}
\value{
- \code{\link{mxModel}}
}
\description{
Making it as simple as possible to create a RAM model. It borrows its philosophy and some parameter
labels from John Fox's sem::specifyModel.
}
\details{
Like mxModel, you list the theoretical causal paths. Unlike mxModel:
\enumerate{
\item{You can request creation of error variances using \code{endog.variances = TRUE} }
\item{You can request creation of variances for exogenous variables (using \code{exog.variances = TRUE})}
\item{For identification, you can request either \code{fix = "latents"} or \code{fix = "firstLoadings"} to fix either the variance of latents or their first factor loading at 1.}
}
Additional conveniences:
\enumerate{
\item{type defaults to "RAM"}
\item{You don't need to list manifestVars (they are assumed to map onto names in the \code{mxData})}
\item{Any variables you mention that are not found in mxData are assumed to be latents}
}
}
\examples{
thedata = mtcars[,c("mpg", "cyl", "disp")]
a = mxPath(from = "l_power", to = "mpg")
b = mxPath(from = c("cyl","disp"), to = "l_power")
d = mxData(thedata, type = "raw")
e = mxData(cov(thedata), type = "cov", numObs = nrow(thedata))

m1 = umxRAM("tim", a, b, d)
m1 = umxRAM("tim", a, b, e)
m1 = mxRun(m1)
\dontrun{
plot(m1)
m1 = umxRAM("tim", a, b) # error: no data
# TODO implement handling a dataframe
umxRAM("tim", a, b, thedata)
}
}
\references{
- \url{https://github.com/tbates/umx}, \url{tbates.github.io}, \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
}

