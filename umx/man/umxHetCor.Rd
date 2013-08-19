\name{umxHetCor}
\alias{umxHetCor}
\title{umxHetCor}
\usage{
  umxHetCor(data, ML = F, use = "pairwise.complete.obs")
}
\arguments{
  \item{data}{a \code{\link{data.frame}} of columns for
  which to compute heterochoric correlations}

  \item{ML}{Whether to use Maximum likelihood computation
  of correlations (default = F)}

  \item{use}{How to delete missing data}
}
\value{
  - a matrix of correlations
}
\description{
  umxHetCor Helper to return just the correlations from
  John Fox's polycor::hetcor function
}
\examples{
\dontrun{
cor_mat_ = umxHetCor(df)
umxHetCor(data, use="pairwise.complete.obs")
}
}
\references{
  -
}
\seealso{
  - \code{\link{hetcor}}
}

