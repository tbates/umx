\name{umxRAM}
\alias{umxRAM}
\title{umxRAM}
\usage{
umxRAM(name, ..., fixed = NA, independent = NA)
}
\arguments{
  \item{name}{friendly name for the model}

  \item{...}{A list of paths and a data source}

  \item{independent}{Whether the model is independent
  (default = NA)}

  \item{fixed}{A list of exogenous manifests that should be
  allowed to covary}
}
\value{
- \code{\link{mxModel}}
}
\description{
Making it as simple as possible to create a RAM model
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
m1 = umxRAM("tim", a, b)
# TODO implement handling a dataframe
umxRAM("tim", a, b, thedata)
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}
}

