\name{umx_is_ordinal}
\alias{umx_is_ordinal}
\title{umx_is_ordinal}
\usage{
umx_is_ordinal(df, names = F, strict = T)
}
\arguments{
  \item{df}{an \code{\link{data.frame}} to look in for
  ordinal variables}

  \item{names}{whether to return the names of ordinal
  variables, or a binary list (default = F)}

  \item{strict}{whether to stop when unordered factors are
  found (default = T)}
}
\value{
- list of variable names
}
\description{
return the names of any ordinal variables in a dataframe
}
\examples{
tmp = mtcars
tmp$cyl = ordered(mtcars$cyl)
umx_is_ordinal(tmp)
isContinuous = !umx_is_ordinal(tmp)
# nb: Factors are not necessarily ordered! By default unordered factors cause an error...
\dontrun{
tmp$cyl = factor(mtcars$cyl)
umx_is_ordinal(tmp)
}
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other umx misc functions: \code{\link{umxEval}};
  \code{\link{umx_check_model}};
  \code{\link{umx_check_names}}; \code{\link{umx_has_CIs}};
  \code{\link{umx_has_been_run}};
  \code{\link{umx_has_square_brackets}};
  \code{\link{umx_is_MxModel}}; \code{\link{umx_is_RAM}};
  \code{\link{umx_is_cov}}; \code{\link{umx_reorder}};
  \code{\link{umx_string_to_algebra}}
}

