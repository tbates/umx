% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/umxPower.R
\name{umxPower}
\alias{umxPower}
\title{Test power to detect specified path values in a model.}
\usage{
umxPower(
  trueModel,
  update = NULL,
  n = NULL,
  power = NULL,
  sig.level = 0.05,
  value = 0,
  method = c("ncp", "empirical"),
  explore = FALSE,
  digits = 2,
  plot = TRUE,
  silent = TRUE
)
}
\arguments{
\item{trueModel}{The model with the parameters at values you expect in the population.}

\item{update}{The parameter(s) to drop}

\item{n}{How many subjects? (Default = NULL)}

\item{power}{Default = NULL (conventional level = .8)}

\item{sig.level}{Default = .05}

\item{value}{Value of dropped parameter (default = 0)}

\item{method}{"ncp" (default) or "empirical"}

\item{explore}{Whether to tabulate the range of n or effect size (if n specified). Default = FALSE.}

\item{digits}{Rounding precision for reporting result.}

\item{plot}{whether to plot the power.}

\item{silent}{Suppress model runs printouts to console (TRUE)}
}
\value{
power table
}
\description{
\code{umxPower} takes an input model (the model of the true data), and tests power (or determines n)
to detect dropping (or changing the value) a path in this true model.

A typical target for power is 80\%. Much as the accepted critical p-value is .05, this has emerged as a trade off, in this case
of resources required for more powerful studies against the cost of missing a true effect.  People interested in truth
discourage running studies with low power: A study with 20 percent power will fail to detect real effects 80\% of the time.
But even with zero power, the Type-I error rate remains a nominal 5\% (and with any researcher degrees of freedom, perhaps much more than that).
Low powered research, then, fails to detect true effects, and generates support for random false theories about as often.
This sounds silly, but empirical rates are often as low as 20\% (Button, et al., 2013).

Illustration of  \eqn{\alpha}, \eqn{\beta}, and power (1-\eqn{\beta}):

\if{html}{\figure{power.png}{options: width=50\% alt="Figure: power.png"}}
\if{latex}{\figure{power.pdf}{options: width=7cm}}
}
\examples{
\dontrun{
# ===================================================
# = Power to detect correlation of .3 in 200 people =
# ===================================================

# 1 Make some data
tmp = umx_make_raw_from_cov(qm(1, .3| .3, 1), n=2000, varNames= c("X", "Y"), empirical= TRUE)

# 2. Make model of true XY correlation of .3
m1 = umxRAM("corXY", data = tmp,
   umxPath("X", with = "Y"),
   umxPath(var = c("X", "Y"))
)
# 3. Test power to detect .3 versus 0, with n= 90 subjects
umxPower(m1, "X_with_Y", n= 90)

# ####################
# # Estimating power #
# ####################
# 
#    method = ncp
#         n = 90
#     power = 0.83
# sig.level = 0.05
# statistic = LRT

# =================================================
# = Tabulate Power across a range of values of  n =
# =================================================
umxPower(m1, "X_with_Y", explore = TRUE)

# =====================================
# = Examples with method = empirical  =
# =====================================

# Power to detect r = .3 given n=90
umxPower(m1, "X_with_Y", n = 90, method = "empirical")
# power is .823
# Test using pwr library r.test doing the same thing.
pwr::pwr.r.test(r = .3, n = 90)
#           n = 90
#           r = 0.3
#   sig.level = 0.05
#       power = 0.827
# alternative = two.sided

# Power search for detectable effect size, given n = 90
umxPower(m1, "X_with_Y", explore = TRUE)
umxPower(m1, "X_with_Y", n= 90, explore = TRUE)
umxPower(m1, "X_with_Y", n= 90, method = "empirical", explore = TRUE)


data(twinData) # ?twinData from Australian twins.
twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
mzData = twinData[twinData$zygosity \%in\% "MZFF", ]
dzData = twinData[twinData$zygosity \%in\% "DZFF", ]
m1 = umxACE(selDVs = "ht", selCovs = "age", sep = "", dzData = dzData, mzData = mzData)

# drop more than 1 path
umxPower(m1, update = c("c_r1c1", "age_b_Var1"), method = 'ncp', n=90, explore = TRUE)

# Specify only 1 parameter (not 'age_b_Var1' and 'c_r1c1' ) to search a parameter:power relationship
# note: Can't use method = "ncp" with search)
umxPower(m1, update = c("c_r1c1", "age_b_Var1"), method = 'empirical', n=90, explore = TRUE)
umxPower(m1, update = c("c_r1c1"), method = 'empirical', n=90, explore = TRUE)

}
}
\references{
\itemize{
\item Miles, J. (2003). A framework for power analysis using a structural equation modelling procedure. \emph{BMC Medical Research Methodology}, \strong{3}, 27. \doi{10.1186/1471-2288-3-27}
}
\itemize{
\item \href{https://CRAN.R-project.org/package=Superpower}{Superpower package}
}
}
\seealso{
\itemize{
\item \code{\link[=power.ACE.test]{power.ACE.test()}}, \code{\link[=umxRAM]{umxRAM()}}
}

Other Teaching and Testing functions: 
\code{\link{tmx_show.MxModel}()},
\code{\link{umxDiagnose}()}
}
\concept{Teaching and Testing functions}
