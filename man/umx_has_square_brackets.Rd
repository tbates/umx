\name{umx_has_square_brackets}
\alias{umx_has_square_brackets}
\title{umx_has_square_brackets}
\usage{
umx_has_square_brackets(input)
}
\arguments{
  \item{input}{The label to check for square brackets
  (string input)}
}
\value{
- boolean
}
\description{
Helper function, checking if a label has sqaure brackets
}
\examples{
umx_has_square_brackets("[hello]")
umx_has_square_brackets("goodbye")
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
  \code{\link{umx_is_MxModel}}; \code{\link{umx_is_RAM}};
  \code{\link{umx_is_cov}}; \code{\link{umx_is_ordinal}};
  \code{\link{umx_reorder}};
  \code{\link{umx_string_to_algebra}}
}

