\name{umx_round}
\alias{umx_round}
\title{umx_round}
\usage{
umx_round(df, digits = getOption("digits"), coerce = FALSE)
}
\arguments{
  \item{df}{a dataframe to round in}

  \item{digits}{how many digits to round to (defaults to
  getOption("digits"))}

  \item{coerce}{whether to make the column numeric if it is
  not (default = FALSE)}
}
\value{
- \code{\link{mxModel}}
}
\description{
A version of round() which works on dataframes that contain non-numeric data (or data that cannot be coerced to numeric)
Helpful for dealing with table output that mixes numeric and string types.
}
\examples{
head(umx_round(mtcars, coerce = F))
head(umx_round(mtcars, coerce = T))
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
Other umx utility functions: \code{\link{\%>\%}},
  \code{\link{\%>\%}}, \code{\link{umx_greater_than}};
  \code{\link{qm}}; \code{\link{umx.as.numeric}};
  \code{\link{umx_find_object}}; \code{\link{umx_grep}};
  \code{\link{umx_move_file}};
  \code{\link{umx_rename_file}}; \code{\link{umx_rename}};
  \code{\link{umx_swap_a_block}}
}

