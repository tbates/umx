\name{APA_ANOVA}
\alias{APA_ANOVA}
\title{APA_ANOVA}
\usage{
  APA_ANOVA(model, printDIC = F)
}
\arguments{
  \item{model}{an \code{\link{anova}} model to make a table
  from}

  \item{printDIC}{a boolean toggle whether tou want
  AIC-type fit change table printed}
}
\description{
  APA_ANOVA just runs anova(); lm.beta(), and puts that
  together in a regression table...
}
\examples{
APA_ANOVA(model)
}
\references{
  - http://openmx.psyc.virginia.edu/
}
\seealso{
  - \code{\link{Anova}}, \code{\link{anova}},
  \code{\link{lm.beta}}
}

