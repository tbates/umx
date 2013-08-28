\name{umx_u_APA_pval}
\alias{umx_u_APA_pval}
\title{umx_u_APA_pval}
\usage{
  umx_u_APA_pval(p, min = 0.001, rounding = 3,
    addComparison = T)
}
\arguments{
  \item{p}{A p-value to round}

  \item{min}{Threshold to say < min}

  \item{rounding}{Number of decimal to round to}

  \item{addComparison}{Whether to return the bare number,
  or to add the appropriate comparison symbol (= <)}
}
\value{
  - a value
}
\description{
  round a p value so you get < .001 instead of .000000002
  or .134E-16
}
\examples{
\dontrun{
umx_u_APA_pval(1.23E3)
}
}
\seealso{
  - \code{\link{round}}
}

