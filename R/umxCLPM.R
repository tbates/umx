#' Runs cross-lagged panel models 
#'
#' @description
#' One way of assessing causal relationships is by introducing time into the analyses. 
#' `umxCLPM` implements three cross-lagged panel models (CLPM) from the literature. 
#' The first is the classic CLPM from Heise (1969), the second is the CLPM from Hamaker et al. (2015),
#' and the third is the CLPM from STARTS (1995).
#' You simply pass the number of waves and the data set and the model you want to run.
#' 
#' Sketch mode is available, just do no pass data and you will be returned a model object to be manipulated later. 
#' 
#' @param waves Number of waves of data.
#' @param name The name of the model (defaults to either "Heise1969", "Hamaker2015", or "STARTS1995").
#' @param model Model type ("Hamaker2015", "Heise1969", or "STARTS1995")
#' @param data Data frame for the analysis
#' @param autoRun Whether to run the model (default), or just to create it and return without running.
#' @param tryHard Default ('no') uses normal mxRun. "yes" uses mxTryHard. Other options: "ordinal", "search"
#' @param summary Optionally show a summary.
#' @return - [mxModel()]
#' @export
#' @family CLPM Functions
#' @references - Kenny, D.A., & Zautra, A. (1995). The trait-state-error model for multiwave data. *Journal of Consulting and Clinical Psychology*, **63**, 52–59. \doi{10.1037/0022-006X.63.1.52}
#' * Hamaker E.L., Kuiper R.M., & Grasman R. (2015). A critique of the cross-lagged panel model. *Psychological Methods*, **20**, 102–116. \doi{10.1037/a0038889}
#' * Heise D. R. (1970). Causal inference from panel data. *Sociological Methodology*, 2, 3–27. \doi{10.2307/270780}
#' @md
#' @examples
#' \dontrun{
#' 
#' # ================
#' # = 1. Load Data =
#' # ================
#' data(docData)
#' dt <- docData[2:9]
#'
#' # ============================
#' # = 2. Make a CLPM model     =
#' # ============================
#' hamaker <- umxCLPM(waves = 4, name = "mymodel", model = "Hamaker2015", data = dt)
#'}
umxCLPM <- function(waves, name = NULL, model = c("Hamaker2015", "Heise1969", "STARTS1995"), data = NULL, summary = !umx_set_silent(silent = TRUE), autoRun = getOption("umx_auto_run"), tryHard = c("no", "yes", "ordinal", "search")) {

  message("This function is still experimental and needs review of the specs from Heise and STARTS")
  sketch = FALSE
  xs = sprintf("X%03d", 1:waves)
  ys = sprintf("Y%03d", 1:waves)
  ps = sprintf("p%03d", 1:waves)
  qs = sprintf("q%03d", 1:waves)
  us = sprintf("u%03d", 1:waves)
  vs = sprintf("v%03d", 1:waves)

  if (model == "Heise1969") {
    if (missing(name)) name = model 
    if(missing(data)){
      data = c(xs, ys)
      mean_names = data 
      sketch = TRUE
    } else {
      colnames(data) = c(xs, ys)
      mean_names = colnames(data)
    }

    m1 = umxRAM(name,
      data = data,
      autoRun = FALSE,
      # WITHIN MODEL
      umxPath(ps, xs, fixedAt = 1),
      umxPath(var = c(ps[2:waves]), labels = "us" ),
      umxPath(var = c(qs[2:waves]), labels = "vs" ), 
      umxPath(qs, ys, fixedAt = 1),
      # CAUSAL
      umxPath(ps[1:waves - 1], qs[2:waves], values = 0.05),
      umxPath(qs[1:waves-1], ps[2:waves], values = 0.05),
      # AR
      umxPath(ps[1:waves - 1], ps[2:waves], values = 1),
      umxPath(qs[1:waves - 1], qs[2:waves], values = 1),
      # CORRELATIONS
      umxPath(ps[2:waves], with = qs[2:waves], values = 0.02),
      umxPath(ps[1], with = qs[1], values = 0.02),
      # MEANS
      umxPath(means = mean_names), 
      # UNCORRELATED VARIANCES
      umxPath(var = c(ps[1], qs[1]), values = 1)
    )
  }  else if (model == "Hamaker2015") {

    if (waves < 3) stop("Hamaker2015 requires at least 3 waves")

    if(missing(data)){
      data = c(xs, ys)
      mean_names = data 
      sketch = TRUE
    } else {
      colnames(data) = c(xs, ys)
      mean_names = c(colnames(data))
    }
    m1 = umxRAM(name,
      data = data,
      autoRun = FALSE,
      # RANDOM INTERCEPTS
      umxPath("xir", xs, fixedAt = 1),
      umxPath("yir", ys, fixedAt = 1),
      # WITHIN MODEL
      umxPath(ps, xs, fixedAt = 1),
      umxPath(var = ps[2:waves], labels = "us"),
      umxPath(var = qs[2:waves], labels = "vs"), 
      umxPath(qs, ys, fixedAt = 1),
      # CAUSAL
      umxPath(ps[1:waves - 1], qs[2:waves], values = 0.2),
      umxPath(qs[1:waves-1], ps[2:waves], values = 0.05),
      # AR
      umxPath(ps[1:waves - 1], ps[2:waves], values = 1),
      umxPath(qs[1:waves - 1], qs[2:waves], values = 1),
      # CORRELATIONS
      umxPath(ps[2:waves], with = qs[2:waves], values = 0.02, labels = "uv"),
      umxPath(ps[1], with = qs[1], values = 0.08),
      umxPath("xir", with = "yir", values = 0.08),
      # UNCORRELATED VARIANCES
      umxPath(var = c("xir", "yir"), values = 1),
      umxPath(var = c(ps[1], qs[1]), values = 1),
      # MEANS
      umxPath(means = mean_names)
    )

  } else if (model == "STARTS1995") {

    if (waves < 4) stop("STARTS1995 requires at least 4 waves")

    if(missing(data)){
      data = c(xs, ys)
      mean_names = data 
      sketch = TRUE
    } else {
      colnames(data) = c(xs, ys)
      message(colnames(data))
      mean_names = colnames(data)
    }

    m1 = umxRAM(name,
      data = data,
      autoRun = FALSE,
      # RANDOM INTERCEPTS
      umxPath("xir", xs, fixedAt = 1),
      umxPath("yir", ys, fixedAt = 1),
      # WITHIN MODEL
      umxPath(ps, xs, fixedAt = 1),
      umxPath(var = ps[2:waves], labels = "us"),
      umxPath(var = qs[2:waves], labels = "vs"), 
      umxPath(qs, ys, fixedAt = 1),
      # CAUSAL
      umxPath(ps[1:waves - 1], qs[2:waves], values = 0.05),
      umxPath(qs[1:waves-1], ps[2:waves], values = 0.05),
      # AR
      umxPath(ps[1:waves - 1], ps[2:waves], values = .1),
      umxPath(qs[1:waves - 1], qs[2:waves], values = .1),
      # CORRELATIONS
      umxPath(ps[2:waves], with = qs[2:waves], values = 0.02, labels = "uv"),
      umxPath(ps[1], with = qs[1], values = 0.02),
      umxPath("xir", with = "yir", values = 0.02),
      # MEANS
      umxPath(means = mean_names),
      # UNCORRELATED VARIANCES
      umxPath(var = xs, values = 0.5),
      umxPath(var = ys, values = 0.5),
      umxPath(var = c("xir", "yir"), values = 1),
      umxPath(var = c(ps[1], qs[1]), values = 1)
    )

  } 

  m1 = as(m1, "MxModel") 

  if (!sketch) m1  = xmu_safe_run_summary(m1, autoRun = autoRun,  summary = summary, tryHard =  tryHard)

  return(m1)
}

