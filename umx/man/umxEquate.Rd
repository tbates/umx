\name{umxEquate}
\alias{umxEquate}
\title{umxEquate}
\usage{
  umxEquate(model, master, slave, free = T, verbose = T,
    name = NULL)
}
\arguments{
  \item{model}{an \code{\link{mxModel}} within which to
  equate chosen parameters}

  \item{master}{A list of labels with which slave labels
  will be equated}

  \item{slave}{A list of labels which will be updated to
  match master labels, thus equating the parameters}

  \item{free}{Boolean determining what the initial state is
  of master and slave parameters. Allows stepping over
  parameters if they are in a particular state}

  \item{verbose}{How much feedback will be given}

  \item{name}{Optional new name for the returned model}
}
\value{
  - \code{\link{mxModel}}
}
\description{
  umxEquate equates slave labels to a set of master labels
}
\examples{
\dontrun{
 model = umxEquate(model)
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

