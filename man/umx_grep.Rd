\name{umx_grep}
\alias{umx_grep}
\title{umx_grep}
\usage{
umx_grep(df, grepString, output = "both", ignore.case = T, useNames = F)
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
umx_grep(mtcars, "hp", output="both", ignore.case=T)
umx_grep(mtcars, "^h.*", output="both", ignore.case=T)
\dontrun{
umx_grep(spss_df, "labeltext", output = "label")
umx_grep(spss_df, "labeltext", output = "name")
}
}
\references{
- \url{http://www.github.com/tbates/umx}
}
\seealso{
- \code{\link{umxLabel}}, \code{\link{umxRun}}, \code{\link{umxValues}}

Other umx utility functions: \code{\link{\%>\%}},
  \code{\link{\%>\%}}, \code{\link{umx_greater_than}};
  \code{\link{qm}}; \code{\link{umx.as.numeric}};
  \code{\link{umx_find_object}};
  \code{\link{umx_move_file}};
  \code{\link{umx_rename_file}}; \code{\link{umx_rename}};
  \code{\link{umx_round}}; \code{\link{umx_swap_a_block}}
}

