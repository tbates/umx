% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financial functions.R
\name{fin_NI}
\alias{fin_NI}
\title{Compute NI given annual Earnings.}
\usage{
fin_NI(annualEarnings, symbol = "£")
}
\arguments{
\item{annualEarnings}{Employee annual earnings.}

\item{symbol}{Currency symbol to embed in the result.}
}
\value{
\itemize{
\item NI
}
}
\description{
Employees pay contributions at 12\%\% on annual earnings between £9,568 and £50,270. Above that you pay at 2\%\%.
Employers pay at 13.8\%\% on all annual earnings of more than £8,840, although there are different thresholds
for those under the age of 21 and for apprentices under the age of 25.
}
\examples{
fin_NI(42e3)
fin_NI(142000)

}
\references{
\itemize{
\item \url{https://www.telegraph.co.uk/tax/tax-hacks/politicians-running-scared-long-overdue-national-insurance-overhaul/}
}
}
\seealso{
\itemize{
\item \code{\link[=fin_interest]{fin_interest()}}, \code{\link[=fin_percent]{fin_percent()}}, \code{\link[=fin_valuation]{fin_valuation()}}
}

Other Miscellaneous Functions: 
\code{\link{deg2rad}()},
\code{\link{fin_JustifiedPE}()},
\code{\link{fin_expected}()},
\code{\link{fin_interest}()},
\code{\link{fin_net_present_value}()},
\code{\link{fin_percent}()},
\code{\link{fin_ticker}()},
\code{\link{fin_valuation}()},
\code{\link{rad2deg}()},
\code{\link{umxBrownie}()}
}
\concept{Miscellaneous Functions}
