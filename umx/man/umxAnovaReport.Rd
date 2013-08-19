\name{umxAnovaReport}
\alias{umxAnovaReport}
\title{umxAnovaReport}
\usage{
  umxAnovaReport(model, printDIC = F)
}
\arguments{
  \item{model}{an \code{\link{anova}} model to make a table
  from}

  \item{printDIC}{a boolean toggle whether tou want
  AIC-type fit change table printed}
}
\description{
  umxAnovaReport just runs anova(); lm.beta(), and puts
  that together in a regression table...
}
\examples{
model = lm(mpg ~ cyl+disp, data = mtcars)
\dontrun{
umxAnovaReport(model)
}
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{Anova}}, \code{\link{anova}},
  \code{\link{lm.beta}}
}

