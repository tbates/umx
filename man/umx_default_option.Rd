\name{umx_default_option}
\alias{umx_default_option}
\title{umx_default_option}
\usage{
umx_default_option(x, option_list, check = T)
}
\arguments{
  \item{x}{the value chosen (may be a selection, or the
  default list of options)}
}
\value{
- the option
}
\description{
handle parameter options given as a default list in a
function
}
\examples{
option_list = c("default", "par.observed", "empirical")
umx_default_option("par.observed", option_list)
umx_default_option("bad", option_list)
umx_default_option("allow me", option_list, check = F)
umx_default_option(option_list, option_list)
option_list = c(NULL, "par.observed", "empirical")
umx_default_option(option_list, option_list) # fails with NULL!!!!!
option_list = c(NA, "par.observed", "empirical")
umx_default_option(option_list, option_list) # use NA instead
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

