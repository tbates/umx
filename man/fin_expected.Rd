% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financial functions.R
\name{fin_expected}
\alias{fin_expected}
\title{Compute the future value and gain of an investment}
\usage{
fin_expected(
  current = 89,
  fair = 140,
  ticker = "NVDA",
  capital = 0.15,
  verb = FALSE
)
}
\arguments{
\item{current}{The current market value of the instrument}

\item{fair}{The user's estimated fair value.}

\item{ticker}{A lable for printing}

\item{capital}{The cost of capital (defaults to .15)}

\item{verb}{Verbose or concise (FALSE)}
}
\value{
\itemize{
\item expected gain
}
}
\description{
fin_expected takes a current and fair value, as well as a cost of capital, and returns the expected gain.
}
\examples{

fin_expected(current= 114,fair=140,ticker="NVDA", capital=.15, verb=T)
NVDA  return =  41 \%
delta (fair-current)= $ 26 
growth = $ 21 
expected gain = $ 47 
future value (final) = $ 161 

fin_expected(24, 130, ticker="SMMT")
SMMT  return =  523 \%

fin_expected(24, 75, ticker="SMMT", verb=T)
SMMT  return =  259 \%
delta (fair-current)= $ 51 
growth = $ 11.25 
expected gain = $ 62.25 
future value (final) = $ 86.25 

fin_expected(750, 1000, ticker="LLY", verb=T)
LLY  return =  53 \%
delta (fair-current)= $ 250
growth = $ 150 
expected gain = $ 400 
future value (final) = $ 1150 

}
\seealso{
\itemize{
\item \code{\link[=fin_interest]{fin_interest()}}
}

Other Miscellaneous Functions: 
\code{\link{deg2rad}()},
\code{\link{fin_JustifiedPE}()},
\code{\link{fin_NI}()},
\code{\link{fin_interest}()},
\code{\link{fin_net_present_value}()},
\code{\link{fin_percent}()},
\code{\link{fin_ticker}()},
\code{\link{fin_valuation}()},
\code{\link{rad2deg}()},
\code{\link{umxBrownie}()}
}
\concept{Miscellaneous Functions}
