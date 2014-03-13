\name{umx_print}
\alias{umx_print}
\title{umx_print}
\usage{
umx_print(x, digits = getOption("digits"), quote = FALSE, na.print = "",
  zero.print = "0", justify = "none", file = c(NA, "tmp.html"), ...)
}
\arguments{
  \item{x}{A data.frame to print}

  \item{digits}{The number of decimal places to print
  (defaults to getOption("digits")}

  \item{quote}{Parameter passed to print}

  \item{na.print}{String to replace NA with (default to
  blank "")}

  \item{zero.print}{String to replace 0.000 with (defaults
  to "0")}

  \item{justify}{Parameter passed to print}

  \item{output}{file to write to and open in browser}

  \item{...}{Optional parameters for print}
}
\description{
A helper to aid the interpretability of printed tables from
OpenMx (and elsewhere). Its most useful characteristic is
allowing you to change how NA and zero appear. By default,
Zeros have the decimals suppressed, and NAs are suppressed
altogether.
}
\examples{
umx_print(mtcars[1:10,], digits = 2, zero.print = ".", justify = "left")
\dontrun{
umx_print(model)
umx_print(mtcars[1:10,], file = "Rout.html")
}
}
\seealso{
- \code{\link{print}}
}

