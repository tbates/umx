\name{umxHetCor}
\alias{umxHetCor}
\title{umxHetCor}
\usage{
  umxHetCor(data, ML = F, use = "pairwise.complete.obs",
    treatAllAsFactor = F, verbose = F)
}
\arguments{
  \item{data}{A \code{\link{data.frame}} of columns for
  which to compute heterochoric correlations}

  \item{ML}{Whether to use Maximum likelihood computation
  of correlations (default = F)}

  \item{use}{How to delete missing data: "complete.obs",
  "pairwise.complete.obs" Default is pairwise.complete.obs}

  \item{treatAllAsFactor}{Whether to treat all columns as
  factors, whether they are currently or not.}

  \item{verbose}{How much to tell the user about what was
  done.}
}
\value{
  - A matrix of correlations
}
\description{
  umxHetCor Helper to return just the correlations from
  John Fox's polycor::hetcor function
}
\examples{
\dontrun{
cor_df = umxHetCor(df)
cor_df = umxHetCor(df, ML = F, use="pairwise.complete.obs")
}
}
\references{
  -
}
\seealso{
  - \code{\link{hetcor}}
}

