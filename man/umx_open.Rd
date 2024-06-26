% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_open}
\alias{umx_open}
\title{Open a file or folder}
\usage{
umx_open(filepath = getwd())
}
\arguments{
\item{filepath}{The file to open}
}
\value{
None
}
\description{
Open a file or folder. Works on OS X, mostly on windows, and hopefully on unix.
}
\details{
NOTE: Your filepath is \code{\link[=shQuote]{shQuote()}}'d by this function.
}
\examples{
\dontrun{
umx_open() # Default is to open working directory getwd()
umx_open("~/bin/umx/R/misc_and_utility copy.r")
}
}
\references{
\itemize{
\item \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
}
}
\seealso{
Other File Functions: 
\code{\link{dl_from_dropbox}()},
\code{\link{umx}},
\code{\link{umx_file_load_pseudo}()},
\code{\link{umx_make_sql_from_excel}()},
\code{\link{umx_move_file}()},
\code{\link{umx_rename_file}()},
\code{\link{umx_write_to_clipboard}()}
}
\concept{File Functions}
