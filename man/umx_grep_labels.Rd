\name{umx_grep_labels}
\alias{umx_grep_labels}
\title{umx_grep_labels}
\usage{
  umx_grep_labels(df, grepString, output = "both",
    ignore.case = T, useNames = F)
}
\arguments{
  \item{df}{an \code{\link{data.frame}} to search the
  labels of}

  \item{grepString}{the search string}

  \item{output}{the column name, the label, or both
  (default)}

  \item{ignore.case}{whether to be case sensitive or not
  (default TRUE)}

  \item{useNames}{whether to search the names as well as
  the labels}
}
\value{
  - list of matched column name and labels
}
\description{
  search the labels of an SPSS file
}
\examples{
\dontrun{
umx_grep_labels(df)
umx_grep_labels(relig, "Race", output="both", ignore.case=T)
umx_grep_labels(relig, "race", output="label")
umx_grep_labels(relig, "race", output="name")
}
}
\references{
  - \url{http://openmx.psyc.virginia.edu}
}
\seealso{
  - \code{\link{umxLabel}}, \code{\link{umxRun}},
  \code{\link{umxStart}}
}

