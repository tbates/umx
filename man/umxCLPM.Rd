% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umxCLPM.R
\name{umxCLPM}
\alias{umxCLPM}
\title{Runs cross-lagged panel models}
\usage{
umxCLPM(
  waves,
  name = NULL,
  model = c("Hamaker2015", "Heise1969", "STARTS1995"),
  data = NULL,
  summary = !umx_set_silent(silent = TRUE),
  autoRun = getOption("umx_auto_run"),
  tryHard = c("no", "yes", "ordinal", "search")
)
}
\arguments{
\item{waves}{Number of waves of data.}

\item{name}{The name of the model (defaults to either "Heise1969", "Hamaker2015", or "STARTS1995").}

\item{model}{Model type ("Hamaker2015", "Heise1969", or "STARTS1995")}

\item{data}{Data frame for the analysis}

\item{summary}{Optionally show a summary.}

\item{autoRun}{Whether to run the model (default), or just to create it and return without running.}

\item{tryHard}{Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"}
}
\value{
\itemize{
\item \code{\link[=mxModel]{mxModel()}}
}
}
\description{
One way of assessing causal relationships is by introducing time into the analyses.
\code{umxCLPM} implements three cross-lagged panel models (CLPM) from the literature.
The first is the classic CLPM from Heise (1969), the second is the CLPM from Hamaker et al. (2015),
and the third is the CLPM from STARTS (1995).
You simply pass the number of waves and the data set and the model you want to run.

Sketch mode is available, just do no pass data and you will be returned a model object to be manipulated later.
}
\examples{
\dontrun{

# ================
# = 1. Load Data =
# ================
data(docData)
dt <- docData[2:9]

# ============================
# = 2. Make a CLPM model     =
# ============================
hamaker <- umxCLPM(waves = 4, name = "mymodel", model = "Hamaker2015", data = dt)
}
}
\references{
\itemize{
\item Kenny, D.A., & Zautra, A. (1995). The trait-state-error model for multiwave data. \emph{Journal of Consulting and Clinical Psychology}, \strong{63}, 52–59. \doi{10.1037/0022-006X.63.1.52}
}
\itemize{
\item Hamaker E.L., Kuiper R.M., & Grasman R. (2015). A critique of the cross-lagged panel model. \emph{Psychological Methods}, \strong{20}, 102–116. \doi{10.1037/a0038889}
\item Heise D. R. (1970). Causal inference from panel data. \emph{Sociological Methodology}, 2, 3–27. \doi{10.2307/270780}
}
}
\concept{CLPM Functions}
