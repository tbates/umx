% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umxDoC.R
\name{umxDiffMZ}
\alias{umxDiffMZ}
\title{MZ differences method for testing evidence for causality.}
\usage{
umxDiffMZ(
  x,
  y,
  data,
  sep = "_T",
  zygosity = "zygosity",
  zygList = c("MZFF", "MZMM"),
  labxy = c(-1.2, 1.8),
  xylim = c(NA, NA),
  digits = 2
)
}
\arguments{
\item{x}{Presumed causal variable, e.g. "effort"}

\item{y}{Presumed caused outcome, e.g. "testScore"}

\item{data}{Dataframe containing the twin data.}

\item{sep}{The separator "_T" used to make twin var names from x and y.}

\item{zygosity}{The column containnig "zygosity" data}

\item{zygList}{The MZ zygosity codes c("MZFF", "MZMM")}

\item{labxy}{Where to locate the R2 label (defaul = c(x=-2,y=3))}

\item{xylim}{= clip x any axes to range, e.g c(-3,-3)}

\item{digits}{Rounding for beta (def2)}
}
\value{
\itemize{
\item Graph for decorating
}
}
\description{
\code{umxDiffMZ} implements the methods, e.g. De Moor (2008), in which MZ differences on a variable
\code{x} asserted to be causal of an outcome variable \code{y} are tested for association with differences on y.
The logic of the design is shown below:

**Figure showing from MZ differences logic for the p(mud| do(rain)) Pearl **:
\if{html}{\figure{DiffMZ_rain_mud.png}{options: width=50\% alt="Figure: MZ differences model"}}
\if{latex}{\figure{DiffMZ_rain_mud.pdf}{options: width=7cm}}

Example output is shown below, with the fitted line and fit shown.

\strong{Example output from \code{umxDiffMZ}}:
\if{html}{\figure{DiffMZ_example.png}{options: width=50\% alt="Figure: MZ differences model"}}
\if{latex}{\figure{DiffMZ_example.pdf}{options: width=7cm}}
}
\examples{
data(twinData)
umxDiffMZ(x="ht", y="wt", labxy = c(-.5, 3), data = twinData, sep = "")
umxDiffMZ(x="ht", y="wt", xylim = c(-2, 2), data = twinData, sep = "")
}
\references{
\itemize{
\item De Moor, M. H., Boomsma, D. I., Stubbe, J. H., Willemsen, G., & de Geus, E. J. (2008). Testing causality in the association between regular exercise and symptoms of anxiety and depression. Archives of General Psychiatry, 65(8), 897-905. \doi{10.1001/archpsyc.65.8.897}.
}
}
\seealso{
\itemize{
\item \code{\link[=umxDoC]{umxDoC()}}, \code{\link[=umxDiscTwin]{umxDiscTwin()}}
}

Other Twin Modeling Functions: 
\code{\link{power.ACE.test}()},
\code{\link{umxACEcov}()},
\code{\link{umxACEv}()},
\code{\link{umxACE}()},
\code{\link{umxCP}()},
\code{\link{umxDiscTwin}()},
\code{\link{umxDoCp}()},
\code{\link{umxDoC}()},
\code{\link{umxGxE_window}()},
\code{\link{umxGxEbiv}()},
\code{\link{umxGxE}()},
\code{\link{umxIP}()},
\code{\link{umxReduceACE}()},
\code{\link{umxReduceGxE}()},
\code{\link{umxReduce}()},
\code{\link{umxRotate.MxModelCP}()},
\code{\link{umxSexLim}()},
\code{\link{umxSimplex}()},
\code{\link{umxSummarizeTwinData}()},
\code{\link{umxSummaryACEv}()},
\code{\link{umxSummaryACE}()},
\code{\link{umxSummaryDoC}()},
\code{\link{umxSummaryGxEbiv}()},
\code{\link{umxSummarySexLim}()},
\code{\link{umxSummarySimplex}()},
\code{\link{umxTwinMaker}()},
\code{\link{umx}}
}
\concept{Twin Modeling Functions}