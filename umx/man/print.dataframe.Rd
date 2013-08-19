\name{print.dataframe}
\alias{print.dataframe}
\title{print.dataframe}
\usage{
  print.dataframe(x, digits = getOption("digits"),
    quote = FALSE, na.print = "", zero.print = "0",
    justify = "none", ...)
}
\arguments{
  \item{x}{a data.frame to print}

  \item{digits}{the number of decimal places to print
  (defaults to getOption("digits")}

  \item{quote}{parameter passed to print}

  \item{na.print}{String to replace NA with (default to
  blank "")}

  \item{zero.print}{String to replace 0.000 with (defaults
  to "0")}

  \item{justify}{parameter passed to print}

  \item{...}{option parameters for print}
}
\description{
  print.dataframe is a helper to aid the interpretability
  of printed tables from OpenMx (and elsewhere). Its most
  useful characteristic is allowing you to change how NA
  and zero appear. By default, Zeros have the decimals
  suppressed, and NAs are suppressed altogether.
}
\examples{
\dontrun{
print.dataframe(model)
}
}
\seealso{
  - \code{\link{print}}
}

