% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/financial functions.R
\name{fin_interest}
\alias{fin_interest}
\title{Compute the value of a principal & annual deposits at a compound interest over a number of years}
\usage{
fin_interest(
  principal = 100,
  deposits = 0,
  inflate = 0,
  interest = 0.05,
  yrs = 10,
  final = NULL,
  n = 12,
  when = "beginning",
  symbol = NULL,
  largest_with_cents = 0,
  baseYear = as.numeric(format(Sys.time(), "\%Y")),
  table = TRUE,
  report = c("markdown", "html")
)
}
\arguments{
\item{principal}{The initial investment at time 0 (default 100)}

\item{deposits}{Optional periodic additional investment each \emph{year}.}

\item{inflate}{How much to inflate deposits over time (default 0)}

\item{interest}{Annual interest rate (default .05)}

\item{yrs}{Duration of the investment (default 10).}

\item{final}{if set (default = NULL), returns the rate required to turn principal into final after yrs (principal defaults to 1)}

\item{n}{Compounding intervals per year (default 12 (monthly), use 365 for daily)}

\item{when}{Deposits made at the "beginning" (of each year) or "end"}

\item{symbol}{Currency symbol to embed in the result.}

\item{largest_with_cents}{Default = 0}

\item{baseYear}{Default = current year (for table row labels)}

\item{table}{Whether to print a table of annual returns (default TRUE)}

\item{report}{"markdown" or "html",}
}
\value{
\itemize{
\item Value of balance after yrs of investment.
}
}
\description{
Allows you to determine the final value of an initial \code{principal} (with optional
periodic \code{deposits}), over a number of years (\code{yrs}) at a given rate of \code{interest}.
Principal and deposits are optional. You control compounding periods each year (n) and whether deposits occur at the beginning or end of the year.
The function outputs a nice table of annual returns, formats the total using a user-settable currency \code{symbol}. Can also \code{report} using a web table.

\emph{notes}: Graham valuation: fair P/E = 9 + (1.5 * growth\%). e.g.  $INTEL fair P/E = 9+.5\emph{3 = 10.5 up to  9+2}10 = 29
Can move the weighting between a conservative .5 and an optimistic 2 (in terms of how long the growth will last and how low the hurdle rate is)
}
\examples{
\dontrun{
# 1. Value of a principal after yrs years at 5\% return, compounding monthly.
# Report in browser as a nice table of annual returns and formatted totals.
fin_interest(principal = 5000, interest = 0.05, rep= "html")
}

# Report as a nice markdown table
fin_interest(principal = 5000, interest = 0.05, yrs = 10)

umx_set_dollar_symbol("£")
# 2 What rate is needed to increase principal to final value in yrs time?
fin_interest(1, final = 1.4, yrs=5)
fin_interest(principal = 50, final=200, yrs = 5)

# 3. What's the value of deposits of $100/yr after 10 years at 7\% return?
fin_interest(0, deposits = 100, interest = 0.07, yrs = 10, n = 12)

# 4. What's the value of $20k + $100/yr over 10 years at 7\% return?
fin_interest(principal= 20e3, deposits= 100, interest= .07, yrs= 10, symbol="$")

# 5. What is $10,000 invested at the end of each year for 5 years at 6\%?
fin_interest(deposits = 10e3, interest = 0.06, yrs = 5, n=1, when= "end")

# 6. What will $20k be worth after 10 years at 15\% annually (n=1)?
fin_interest(deposits=20e3, interest = 0.15, yrs = 10, n=1, baseYear=1)
# $466,986

# manual equivalent
sum(20e3*(1.15^(10:1))) # 466985.5

# 7. Annual (rather than monthly) compounding (n=1)
fin_interest(deposits = 100, interest = 0.07, yrs = 10, n=1)

# 8 Interest needed to increase principal to final value in yrs time.
fin_interest(principal = 100, final=200, yrs = 5)

}
\references{
\itemize{
\item \url{https://en.wikipedia.org/wiki/Compound_interest}
}
}
\seealso{
\itemize{
\item \code{\link[=umx_set_dollar_symbol]{umx_set_dollar_symbol()}}, \code{\link[=fin_percent]{fin_percent()}}, \code{\link[=fin_NI]{fin_NI()}}, \code{\link[=fin_valuation]{fin_valuation()}}
}

Other Miscellaneous Functions: 
\code{\link{deg2rad}()},
\code{\link{fin_JustifiedPE}()},
\code{\link{fin_NI}()},
\code{\link{fin_expected}()},
\code{\link{fin_net_present_value}()},
\code{\link{fin_percent}()},
\code{\link{fin_ticker}()},
\code{\link{fin_valuation}()},
\code{\link{rad2deg}()},
\code{\link{umxBrownie}()}
}
\concept{Miscellaneous Functions}
