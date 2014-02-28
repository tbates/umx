\name{umxSaturated}
\alias{umxSaturated}
\title{umxSaturated}
\usage{
umxSaturated(model, evaluate = T, verbose = T)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} to get independence
  and saturated fits to}
}
\value{
- A list of the saturated and independence models, from
which fits can be extracted
}
\description{
Computes the saturated and independence forms of a RAM
model (needed to return most fit statistics when using raw
data). umxRun calls this automagically.
}
\examples{
\dontrun{
model_sat = umxSaturated(model)
summary(model, SaturatedLikelihood = model_sat$Sat, IndependenceLikelihood = model_sat$Ind)
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxSummary}}, \code{\link{umxRun}}
}

