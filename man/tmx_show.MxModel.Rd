% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmx.R
\name{tmx_show.MxModel}
\alias{tmx_show.MxModel}
\title{Show matrices of RAM models in a easy-to-learn-from format.}
\usage{
\method{tmx_show}{MxModel}(
  x,
  what = c("values", "free", "labels", "nonzero_or_free"),
  show = c("free", "fixed", "all"),
  matrices = c("S", "A", "M"),
  digits = 2,
  report = c("html", "markdown"),
  na.print = "",
  zero.print = ".",
  html_font = NULL,
  style = c("paper", "material_dark", "classic", "classic_2", "minimal", "material"),
  bootstrap_options = c("hover", "bordered", "condensed", "responsive"),
  lightable_options = "striped",
  freeColor = c("black", "#AAAAAA")
)
}
\arguments{
\item{x}{an object e.g. \code{\link[=umxRAM]{umxRAM()}} \code{\link[=umxMatrix]{umxMatrix()}} from which to show parameters.}

\item{what}{legal options are "values" (default), "free", or "labels").}

\item{show}{filter on what to show c("all", "free", "fixed").}

\item{matrices}{to show  (default is c("S", "A")). "thresholds" in beta.}

\item{digits}{precision to report. Default = round to 2 decimal places.}

\item{report}{How to report the results. "html" = open in browser.}

\item{na.print}{How to display NAs (default = "")}

\item{zero.print}{How to display 0 values (default = ".")}

\item{html_font}{Default is null. Set (e.g. "Optima") to override the style's default font.}

\item{style}{The style for the table (Defaults to "paper". Other options are "material_dark", "classic", "classic_2", "minimal", "material")}

\item{bootstrap_options}{border etc. Defaults to c("hover", "bordered", "condensed", "responsive")}

\item{lightable_options}{Default is "striped"}

\item{freeColor}{Default is "black" for free,  fixed is gray "#AAAAAA"}
}
\value{
None
}
\description{
A great way to learn about models is to look at the matrix contents. \code{tmx_show} is designed to
do this in a way that makes it easy to process for users: The matrix contents are formatted as
tables, and can even be displayed as tables in a web browser.
}
\details{
The user can select which matrices to view, whether to show values, free, and/or labels, and the precision of rounding.
}
\examples{
\dontrun{
require(umx)
data(demoOneFactor)
manifests = names(demoOneFactor)
m1 = umxRAM("tmx_sh", data = demoOneFactor, type = "cov",
	umxPath("G", to = manifests),
	umxPath(var = manifests),
	umxPath(var = "G", fixedAt = 1)
)

# =============================================
# = Show smart table on the web (the default) =
# =============================================
tmx_show(m1, report = "html")
tmx_show(m1, what = "free", matrices = "thresholds")
tmx_show(m1, zero.print = "-")

tmx_show(m1, report = "markdown")
tmx_show(m1, digits = 3, report = "markdown")
tmx_show(m1, matrices = "S", report = "markdown")
tmx_show(m1, what = "free"  , report = "markdown")
tmx_show(m1, what = "labels", report = "markdown")
tmx_show(m1, what = "free", matrices = "A", report= "markdown")
}

}
\references{
\itemize{
\item \url{https://tbates.github.io}
}
}
\seealso{
Other Teaching and Testing functions: 
\code{\link{umxDiagnose}()},
\code{\link{umxPower}()}
}
\concept{Teaching and Testing functions}
