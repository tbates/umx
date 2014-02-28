\name{anova.report.F}
\alias{anova.report.F}
\title{anova.report.F}
\usage{
\method{anova}{report.F}(model, digits = 2)
}
\arguments{
  \item{model}{an \code{\link{lm}} model to report F for.}

  \item{digits}{how many numbers after the decimal for F
  (p-value is APA-style)}
}
\value{
- F in text format
}
\description{
Generate a text-format report of the F values in an ANOVA.
Like anova.report.F(model) # "F(495,1) = 0.002"
}
\examples{
m1 = lm(mpg~ cyl + wt, data = mtcars)
anova.report.F(m1)
m2 = lm(mpg~ cyl, data = mtcars)
anova.report.F(m2)
anova.report.F(anova(m1, m2))
}
\seealso{
- \code{\link{lm}}, \code{\link{anova}},
\code{\link{summary}}
}

