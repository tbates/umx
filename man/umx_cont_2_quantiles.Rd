% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/misc_and_utility.R
\name{umx_cont_2_quantiles}
\alias{umx_cont_2_quantiles}
\alias{umx2ord}
\title{umx_cont_2_quantiles}
\usage{
umx_cont_2_quantiles(
  x,
  nlevels = NULL,
  type = c("mxFactor", "ordered", "unordered"),
  verbose = FALSE,
  returnCutpoints = FALSE
)
}
\arguments{
\item{x}{a variable to recode as ordinal (email maintainer("umx") if you'd like this upgraded to handle df input)}

\item{nlevels}{How many bins or levels (at most) to use (i.e., 10 = deciles)}

\item{type}{what to return (Default is "mxFactor") options: "ordered" and "unordered")}

\item{verbose}{report the min, max, and decile cuts used (default = FALSE)}

\item{returnCutpoints}{just return the cutpoints, for use directly}
}
\value{
\itemize{
\item recoded variable as an \code{\link[OpenMx:mxFactor]{OpenMx::mxFactor()}}
}
}
\description{
Recode a continuous variable into n-quantiles (default = deciles (10 levels)).
It returns an \code{\link[OpenMx:mxFactor]{OpenMx::mxFactor()}}, with the levels labeled with the max value
in each quantile (i.e., open on the left-side). quantiles are labeled "quantile1"
"quantile2" etc.
}
\details{
\strong{Note}: Redundant quantiles are merged. i.e., if the same score identifies
all deciles up to the fourth, then these will be merged into one bin, labeled "quantile4".
}
\examples{
x = umx_cont_2_quantiles(rnorm(1000), nlevels = 10, verbose = TRUE)
x = data.frame(x)
str(x); levels(x)
table(x)
\dontrun{
ggplot2::qplot(x$x)
y = mxDataWLS(x, type = "WLS")
}

# ===========================
# = Use with twin variables =
# ===========================

data(twinData)
x = twinData
cuts  = umx_cont_2_quantiles(rbind(x$wt1, x$wt2) , nlevels = 10, returnCutpoints = TRUE)
x$wt1 = umx_cont_2_quantiles(x$wt1, nlevels = cuts) # use same for both...
x$wt2 = umx_cont_2_quantiles(x$wt2, nlevels = cuts) # use same for both...
str(x[, c("wt1", "wt2")])

# More examples

x = umx_cont_2_quantiles(mtcars[, "mpg"], nlevels = 5) # quintiles
x = umx2ord(mtcars[, "mpg"], nlevels = 5) # using shorter alias
x = umx_cont_2_quantiles(mtcars[, "cyl"], nlevels = 10) # more levels than integers exist
x = umx_cont_2_quantiles(rbinom(10000, 1, .5), nlevels = 2)
}
\references{
\itemize{
\item \url{https://github.com/tbates/umx}, \url{https://tbates.github.io}
}
}
\seealso{
Other Data Functions: 
\code{\link{noNAs}()},
\code{\link{prolific_anonymize}()},
\code{\link{prolific_check_ID}()},
\code{\link{prolific_read_demog}()},
\code{\link{umx}},
\code{\link{umxFactor}()},
\code{\link{umxHetCor}()},
\code{\link{umx_as_numeric}()},
\code{\link{umx_lower2full}()},
\code{\link{umx_make_MR_data}()},
\code{\link{umx_make_TwinData}()},
\code{\link{umx_make_fake_data}()},
\code{\link{umx_make_raw_from_cov}()},
\code{\link{umx_merge_randomized_columns}()},
\code{\link{umx_polychoric}()},
\code{\link{umx_polypairwise}()},
\code{\link{umx_polytriowise}()},
\code{\link{umx_read_lower}()},
\code{\link{umx_rename}()},
\code{\link{umx_reorder}()},
\code{\link{umx_score_scale}()},
\code{\link{umx_select_valid}()},
\code{\link{umx_stack}()},
\code{\link{umx_strings2numeric}()}
}
\concept{Data Functions}
