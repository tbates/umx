\name{umxAnovaReport}
\alias{umxAnovaReport}
\title{umxAnovaReport}
\usage{
  umxAnovaReport(model1, model2 = NULL, raw = T,
    printDIC = F)
}
\arguments{
  \item{model1}{an \code{\link{lm}} model to make a table
  from}

  \item{model2}{an (optional) second \code{\link{lm}} model
  to compare to model 1}

  \item{raw}{should the raw table also be output? (allows
  checking that nothing crazy is going on)}

  \item{printDIC}{a boolean toggle whether tou want
  AIC-type fit change table printed}
}
\description{
  umxAnovaReport is a convenience function to format
  results for journals. There are others. Bt I made this
  one. If you give it the output of an lm, it runs anova()
  and lm.beta(), and puts that together in a regression
  table... Alternatively if you fill in the optional second
  model, it compares them (just like
  \code{\link{umxCompare}})
}
\examples{
model = lm(mpg ~ cyl + disp, data = mtcars)
umxAnovaReport(model)
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{umxReportFit}}, \code{\link{umxCompare}},
  \code{\link{anova}}, \code{\link{lm.beta}}
}

