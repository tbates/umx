\name{umxACE}
\alias{umxACE}
\title{umxACE}
\usage{
umxACE(name = "ACE", selDVs, dzData, mzData, numObsDZ = NULL,
  numObsMZ = NULL, weightVar = NULL)
}
\arguments{
  \item{name}{The name of the model (defaults to"ACE")}

  \item{selDVs}{The variables to include from the data}

  \item{dzData}{The DZ dataframe}

  \item{mzData}{The MZ dataframe}

  \item{numObsDZ}{= Number of DZ twins: Set this if you
  input covariance data}

  \item{numObsMZ}{= Number of MZ twins: Set this if you
  input covariance data}

  \item{weightVar}{= If provided, a vector objective used
  to weight the data. (default = NULL)}
}
\value{
- \code{\link{mxModel}}
}
\description{
Make a 2-group ACE model
}
\examples{
require(OpenMx)
require(umx)
data(twinData)
twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
selDVs = c("bmi1","bmi2")
mzData <- as.matrix(subset(twinData, ZYG == "MZFF", selDVs))
dzData <- as.matrix(subset(twinData, ZYG == "DZFF", selDVs))
m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
m1 = umxRun(m1)
umxSummaryACE(m1)
}
\references{
- \url{http://github.com/tbates/umx}
}
\seealso{
Other umx twin modeling: \code{\link{umxGxE_window}}
}

