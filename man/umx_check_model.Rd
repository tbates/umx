\name{umx_check_model}
\alias{umx_check_model}
\title{umx_check_model}
\usage{
umx_check_model(obj, type = NULL, hasData = NULL)
}
\arguments{
  \item{obj}{an object to check}

  \item{type}{= what type the model must be (defaults to
  not checking NULL)}

  \item{hasData}{whether the model should have data or not
  (defaults to not checking NULL)}
}
\value{
- boolean
}
\description{
Check an OpenMx model
}
\examples{
\dontrun{
umx_check_model(model)
}
}
\references{
- \url{http://openmx.psyc.virginia.edu}
}
\seealso{
- \code{\link{umx}}, \code{\link{umxRun}},
\code{\link{umxStart}}
}

