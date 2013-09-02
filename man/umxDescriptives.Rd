\name{umxDescriptives}
\alias{umxDescriptives}
\title{umxDescriptives}
\usage{
  umxDescriptives(data = NULL, measurevar,
    groupvars = NULL, na.rm = FALSE, conf.interval = 0.95,
    .drop = TRUE)
}
\arguments{
  \item{data}{data.frame to compute descriptive statistics
  for}

  \item{measurevar}{The data column to summarise}

  \item{groupvars}{A list of columns to group the data by}

  \item{na.rm}{whether to remove NA from the data}

  \item{conf.interval}{The size of the CI you request - 95
  by default}

  \item{.drop}{Whether to drop TODO}
}
\description{
  Summarize data for an APA style subjects table
}
\examples{
\dontrun{
umxDescriptives(data)
}
}
\references{
  -
  \url{http://www.cookbook-r.com/Manipulating_data/Summarizing_data}
}
\seealso{
  - \code{\link{plyr}}
}

