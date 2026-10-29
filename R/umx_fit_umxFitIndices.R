#' Get fit indices for models with umxFitIndices
#'
#' Computes fit indices for one model or a list of models, returned as a
#' data.frame with one row per model. `set = "core"` (default) gives the
#' handful of indices worth reporting; `set = "all"` gives everything,
#' including residual-based indices (SRMR, CRMR, SMAR, CMAR, etc.) for when
#' Reviewer 2 wants something other than CFA/TLI/RMSEA.
#'
#' Multigroup and definition-variable models are supported. For multigroup
#' models, residual-based and variable-count indices are `NA`.
#'
#'
#' Fit information reported includes:
#'
#' **Model characteristics**:
#'   numObs, estimated parameters,
#'   observed statistics, observed summary statistics,
#'   -2*log(Likelihood), degrees of freedom,
#'   N_eff (likelihood-contributing rows; an independent check on `numObs`)
#'
#' **Chi-squared test**:
#'   Chi, ChiDoF, p (of Chi), ChiPerDoF,
#'
#' **Noncentrality-based indices**:
#'   RMSEA, RMSEA_lo, RMSEA_hi, RMSEANull, RMSEAClose (p value), independenceRMSEA,
#'   NCP, NCP_lo, NCP_hi, F0, F0_lo, F0_hi, Mc (aka NCI, MFI)
#'
#' **Comparative fit indices**:
#'   TLI (aka NNFI), CFI, IFI, PRATIO, PCFI
#'
#' **Residual-based indices**:
#'   RMR, SRMR, SRMR_mplus, CRMR,
#'   MAR, SMAR, SMAR_mplus, CMAR
#'
#' **Information-theory criteria** (chi-square and -2LL versions; df, parameters, and sample-size penalties)
#'   AIC, BIC (matching `summary()`), plus `IC_<Chi|deviance>_<AIC|BIC|CAIC|BCC>_<df|par|sample>`
#'   ECVI, ECVI_lo, ECVI_hi, MECVI, MECVI_lo, MECVI_hi
#'
#' **LISREL and other early fit indices** (we recommend not reporting these)
#'   GFI, AGFI, PGFI, GH, NFI, PNFI, RFI
#'
#'
#' Want more? *Open an Issue* at [GitHub](https://github.com/tbates/umx/issues).
#'
#' @param fits One [OpenMx::mxModel()] or a list of them.
#' @param set `"core"` (default) for the reportable handful, `"all"` for everything, or a character vector of index names.
#' @param ... Additional parameters passed to [OpenMx::summary.MxModel()]. `refModels` supplied here are used as-is.
#' @return A data.frame with a `model` column and one column per index, one row per model.
#' @export
#' @family Reporting functions
#' @author Brenton M. Wiernik, Athanassios Protopapas, Paolo Ghisletta, Markus Brauer
#' @examples
#' \dontrun{
#' library(umx)
#' data(demoOneFactor)
#' latents = c("G")
#' manifests = names(demoOneFactor)
#' m1 = umxRAM("One Factor",
#' 	data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' umxFitIndices(m1)
#'
#' # And with raw data
#' m2 = umxRAM("m1", data = demoOneFactor,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = latents)
#' )
#' umxFitIndices(m1, refModels = mxRefModels(m2, run = TRUE))
#'
#' # A comparison table for several models, core indices only
#' umxFitIndices(list(m1, m2))
#' }
#' 
umxFitIndices = function(fits, set = c("core", "all"), ...) {
  coreCols = c("Chi", "ChiDoF", "p", "RMSEA", "RMSEA_lo", "RMSEA_hi", "CFI", "TLI",
    "Minus2LogLikelihood", "degreesOfFreedom", "numObs", "N_eff", "AIC", "BIC")
  # Columns needing observed/expected matrices (not computed unless selected)
  matCols = c("GFI", "AGFI", "PGFI", "RMR", "SRMR", "SRMR_mplus", "CRMR",
    "MAR", "SMAR", "SMAR_mplus", "CMAR")
  # Columns that are NA for multigroup models (CAIC needs no per-variable counts, so it stays)
  multiNACols = c(matCols, "GH", "MECVI", "MECVI_lo", "MECVI_hi",
    "observedSummaryStatistics",
    "IC_Chi_BCC_df", "IC_Chi_BCC_par", "IC_Chi_BCC_sample",
    "IC_deviance_BCC_df", "IC_deviance_BCC_par", "IC_deviance_BCC_sample")
  if (umx_is_MxModel(fits)) {
    fits = list(fits)
  }
  if (!is.list(fits) || !length(fits)) {
    stop("Give me a model or a list of models.", call. = FALSE)
  }
  for (m in fits) {
    if (!umx_is_MxModel(m)) {
      stop("Every fit must be an MxModel.", call. = FALSE)
    }
  }
  wantAll = FALSE
  if (missing(set) || identical(set, c("core", "all"))) {
    picked = coreCols
  } else if (length(set) == 1L && set %in% c("core", "all")) {
    wantAll = (set == "all")
    picked = if (wantAll) NULL else coreCols
  } else {
    picked = set
  }
  if (!is.null(picked) && !length(picked)) {
    stop("`set` must name at least one index.", call. = FALSE)
  }
  needMats = wantAll || (!is.null(picked) && any(picked %in% matCols))
  summDots = list(...)
  userRefs = summDots$refModels
  summDots$refModels = NULL

  oneModel = function(model, needMats) {
    if (!umx_is_RAM(model)) {
      message("Not fully tested against non-RAM models...")
    }
    isMulti = inherits(model$fitfunction, "MxFitFunctionMultigroup")
    if (!is.null(userRefs)) {
      refs = userRefs
    } else if (imxHasDefinitionVariable(model)) {
      refs = xmu_ref_models_defvars(model)
    } else {
      refs = NULL
      probe = do.call(summary, c(list(model), summDots))
      if (is.na(probe$SaturatedLikelihood)) {
        refs = xmu_mxRefModels(model, run = TRUE, beginMessage = FALSE)
      }
    }
    if (!is.null(refs)) {
      for (rn in c("Saturated", "Independence")) {
        st = tryCatch(refs[[rn]]$output$status$code, error = function(e) NULL)
        if (!is.null(st) && length(st) == 1L && !is.na(st) && st != 0) {
          warning("Reference model ", rn, " finished with status ", st,
            ": Chi, CFI, TLI, and RMSEA are unreliable.", call. = FALSE)
        }
      }
    }
    modSumm = if (is.null(refs)) do.call(summary, c(list(model), summDots)) else summary(model, refModels = refs)
  numObs       = modSumm$numObs
  nParam       = modSumm$estimatedParameters
  deviance     = modSumm$Minus2LogLikelihood
  df           = modSumm$degreesOfFreedom
  devInd       = modSumm$IndependenceLikelihood
  devSat       = modSumm$SaturatedLikelihood
  dfInd        = modSumm$independenceDoF
  dfSat        = modSumm$saturatedDoF
  Chi          = modSumm$Chi
  dfChi        = modSumm$ChiDoF
  pChi         = modSumm$p
  ChiPerDf     = Chi / dfChi
  ChiInd       = devInd - devSat
  dfChiInd     = dfInd - dfSat

  if (isMulti) {
    manifests = NA_character_
    nManifest = NA_integer_
  } else if (model$data$type == "raw") {
    if (!is.null(model$manifestVars)) {
      manifests = model$manifestVars
      nManifest = length(manifests)
    } else if (!(length(model$expectation$dims) == 1 && is.na(model$expectation$dims))) {
      manifests = model$expectation$dims[model$expectation$dims %in% colnames(model$data$observed)]
      nManifest = length(manifests)
    } else {
      # TODO: Extract dimnames from expectation slot objects according to the MxExpectation class
      warning(paste("Models with raw data and expectation type:", class(model$expectation),
                    "only supported if dimnames are specified in the expectation.",
                    "\nFit indices computed using the number of observed variables will be `NA`."))
      manifests = NA_character_
      nManifest = NA_integer_
    }
  } else {
    nManifest = NCOL(model$data$observed)
  }
  q = (nManifest * (nManifest + 1)) / 2
  if (isMulti) {
    q_m = NA_integer_
  } else if (model$data$type == "raw" || ! is.na(model$data$means)) {
    q_m = q + nManifest
  } else {
    q_m = NA_integer_
  }

  RMSEASquared = modSumm$RMSEASquared
  ciRMSEA      = modSumm$RMSEACI
  NCP          = RMSEASquared * (numObs * dfChi) # Chi - dfChi
  ciNCP        = ciRMSEA^2 * (modSumm$numObs * modSumm$ChiDoF)
  F0           = NCP / numObs
  ciF0         = ciNCP / numObs
  NFI          = (devInd - deviance) / (devInd - devSat)
  RFI          = 1 - (Chi / dfChi) / (ChiInd / dfChiInd)
  IFI          = (ChiInd - Chi) / (ChiInd - dfChi)
  CFI          = modSumm$CFI
  PRATIO       = dfChi / dfChiInd
  PNFI         = PRATIO * NFI
  PCFI         = PRATIO * CFI
  # https://doi.org/10.1080/10705519409539970 gives a different formula for PCFI:
  #   PCFI = CFI * df / ((nManifest * (nManifest - 1)) / 2)
  # But most users of PCFI use the form given here
  GH           =  nManifest / (nManifest + 2 * F0)
  Mc           = exp(-0.5 * NCP / numObs)
  RMSEASquaredInd = (ChiInd/(dfChiInd) - 1)/numObs
  RMSEAInd = ifelse(RMSEASquaredInd < 0, 0, sqrt(RMSEASquaredInd))

  # Information-based indices
  infoCrit = modSumm$informationCriteria
  Fvalue = infoCrit["AIC:", "par"] - 2 * nParam

  infoCrit = rbind(modSumm$informationCriteria,
                   `CAIC:`  = c(Fvalue - (log(numObs) + 1) * df,
                                Fvalue + (log(numObs) + 1) * nParam,
                                NA),
                   `BCC:`   = c(Fvalue - (2 * numObs / (numObs - nManifest - 1)) * df,
                                Fvalue + (2 * numObs / (numObs - nManifest - 1)) * nParam,
                                NA)
  )
  if (Fvalue == deviance) {
    infoCritDev = infoCrit
    infoCritChi = infoCrit - deviance + Chi
  } else {
    infoCritChi = infoCrit
    infoCritDev = infoCrit - Chi + deviance
  }
  informationCriteria = list(Chi = infoCritChi,
                             deviance = infoCritDev)
  ECVI    = (Chi + 2 * nParam) / numObs
  MECVI   = (Chi + 2 * nParam * numObs / (numObs - nManifest - 1)) / numObs
  ciECVI  = (ciNCP + dfChi + 2 * nParam) / numObs
  ciMECVI = (ciNCP + dfChi + 2 * nParam * numObs / (numObs - nManifest - 1)) / numObs


  if (needMats && !isMulti) {
    modelMatrices = xmuModelMatrices(model, manifests)
    IdManifest = diag(nManifest)

    # Residual-based indices
    if (!inherits(model$fitfunction, "MxFitFunctionML")) {
      warning("Bias correction and confidence intervals for residual-based\nfit indices currently only designed to work with maximum likelihood estimation.")
    }
    residualIndices = with(modelMatrices,
                           xmuResidualIndices(obsCov, obsCor, obsMeans,
                                              estCov, estCor, estMeans))

    # LISREL indices
    if (model$data$type == "cor") {
      invSigS = solve(modelMatrices$estCor) %*% modelMatrices$obsCor
    } else {
      invSigS = solve(modelMatrices$estCov) %*% modelMatrices$obsCov
    }
    GFI = 1 - tr(invSigS - IdManifest)^2 / tr(invSigS)^2
    AGFI =  1 - (q / dfChi) * (1 - GFI)
    PGFI =  GFI * dfChi / q
  } else {
    residualIndices = list(estimate = c(RMR = NA_real_, SRMR = NA_real_,
      SRMR_mplus = NA_real_, CRMR = NA_real_, MAR = NA_real_, SMAR = NA_real_,
      SMAR_mplus = NA_real_, CMAR = NA_real_))
    GFI = AGFI = PGFI = NA_real_
  }

  ciPick = function(v, i) {
    if (is.null(v) || length(v) < i) NA_real_ else unname(v[i])
  }
  resEst = residualIndices$estimate
  flat = list(
    numObs = numObs,
    estimatedParameters = nParam,
    observedStatistics = modSumm$observedStatistics,
    observedSummaryStatistics = if (isMulti) NA_real_ else ifelse(model$data$type == "raw" || !(length(model$data$means) == 1 && is.na(model$data$means)), q_m, q),

    Minus2LogLikelihood = deviance,
    degreesOfFreedom = df,

    Chi = Chi,
    ChiDoF = dfChi,
    p = pChi,
    ChiPerDoF = ChiPerDf,

    RMSEA = modSumm$RMSEA,
    RMSEA_lo = ciPick(ciRMSEA, 1), RMSEA_hi = ciPick(ciRMSEA, 2),
    RMSEANull = modSumm$RMSEANull,
    RMSEAClose = modSumm$RMSEAClose,
    independenceRMSEA = RMSEAInd,
    NCP = NCP, NCP_lo = ciPick(ciNCP, 1), NCP_hi = ciPick(ciNCP, 2),
    F0 = F0, F0_lo = ciPick(ciF0, 1), F0_hi = ciPick(ciF0, 2),
    Mc = Mc,

    RMR = resEst[["RMR"]], SRMR = resEst[["SRMR"]],
    SRMR_mplus = resEst[["SRMR_mplus"]], CRMR = resEst[["CRMR"]],
    MAR = resEst[["MAR"]], SMAR = resEst[["SMAR"]],
    SMAR_mplus = resEst[["SMAR_mplus"]], CMAR = resEst[["CMAR"]],

    GFI = GFI, AGFI = AGFI, PGFI = PGFI, GH = GH,
    NFI = NFI, RFI = RFI, IFI = IFI,
    TLI = modSumm$TLI, CFI = CFI,
    PRATIO = PRATIO, PNFI = PNFI, PCFI = PCFI,

    ECVI = ECVI, ECVI_lo = ciPick(ciECVI, 1), ECVI_hi = ciPick(ciECVI, 2),
    MECVI = MECVI, MECVI_lo = ciPick(ciMECVI, 1), MECVI_hi = ciPick(ciMECVI, 2),

    AIC = modSumm$AIC.Mx, BIC = modSumm$BIC.Mx,
    N_eff = xmu_eff_nobs(model)
  )
  for (src in c("Chi", "deviance")) {
    mat = informationCriteria[[src]]
    for (rn in rownames(mat)) {
      for (cn in colnames(mat)) {
        flat[[paste0("IC_", src, "_", sub(":$", "", rn), "_", cn)]] = unname(mat[rn, cn])
      }
    }
  }
  flat = lapply(flat, unname)
  flat
  }

  allRows = vector("list", length(fits))
  multiFlags = logical(length(fits))
  for (i in seq_along(fits)) {
    allRows[[i]] = oneModel(fits[[i]], needMats)
    multiFlags[i] = inherits(fits[[i]]$fitfunction, "MxFitFunctionMultigroup")
  }
  vocab = names(allRows[[1]])
  if (is.null(picked)) {
    selected = vocab
  } else {
    unknown = setdiff(picked, vocab)
    if (length(unknown)) {
      stop("Unknown index names: ", paste(unknown, collapse = ", "),
        ". Valid names are: ", paste(vocab, collapse = ", "), call. = FALSE)
    }
    selected = picked
  }
  if (any(multiFlags) && any(selected %in% multiNACols)) {
    message("Multigroup models report NA for residual-based and variable-count indices.")
  }
  modelNames = vapply(fits, function(m) m$name, character(1))
  vals = as.data.frame(do.call(rbind, lapply(allRows, unlist)), stringsAsFactors = FALSE)
  out = cbind(data.frame(model = modelNames, stringsAsFactors = FALSE), vals[, selected, drop = FALSE])
  row.names(out) = NULL
  class(out) = c("umxFitIndices", "data.frame")
  attr(out, "name") = paste(modelNames, collapse = ", ")
  attr(out, "call") = deparse(sys.call())
  return(out)
}

#' Reference models that keep the definition variables
#'
#' [OpenMx::mxRefModels()] drops definition variables, so a model whose means
#' depend on them can beat its own "saturated" model (chi-square < 0, CFI > 1).
#' Here, per group, the saturated model has a free covariance and the independence
#' model a diagonal one; in both, every manifest mean is regressed on all definition
#' variables used in that group.
#'
#' Definition variables that act on the covariance (e.g. continuous-time intervals)
#' are only carried in the means, so the fitted model is not strictly nested in the
#' saturated reference; chi-square/CFI are then descriptive.
#'
#' Needs raw data in every group. Ordinal data is refused: threshold reference
#' models are not built yet.
#'
#' @param x An [OpenMx::mxModel()] with raw data and definition variables.
#' @return A list with run `Saturated` and `Independence` models, suitable for `summary(x, refModels = )`.
#' @export
#' @seealso - [umxFitIndices()], [OpenMx::mxRefModels()]
xmu_ref_models_defvars <- function(x) {
  groups = xmu_model_groups(x)
  for (g in groups) {
    m = if (identical(g, x@name)) x else x[[g]]
    if (is.null(m$data) || m$data$type != "raw") {
      stop("Defvar-aware reference models need raw data in every group.", call. = FALSE)
    }
    mans = intersect(xmu_model_manifests(m), names(m$data$observed))
    isOrd = vapply(m$data$observed[, mans, drop = FALSE],
      function(col) is.ordered(col) || is.factor(col), logical(1))
    if (any(isOrd)) {
      stop("Defvar-aware reference models cannot handle ordinal data yet.", call. = FALSE)
    }
  }
  one_group = function(g, type) {
    m    = if (identical(g, x@name)) x else x[[g]]
    dat  = m$data$observed
    mans = xmu_model_manifests(m)
    labs = unlist(lapply(c(m@matrices, if (identical(g, x@name)) list() else x@matrices),
      function(M) as.vector(M$labels)))
    defs = unique(sub("^data\\.", "", grep("^data\\.", labs, value = TRUE)))
    defs = intersect(defs, names(dat))
    p = length(mans)
    Mint = mxMatrix("Full", 1, p, free = TRUE, values = colMeans(dat[, mans, drop = FALSE], na.rm = TRUE),
      labels = paste0(g, "_int_", mans), name = "Mint")
    if (type == "Saturated") {
      S = mxMatrix("Symm", p, p, free = TRUE, values = diag(p) * 0.5, name = "S", dimnames = list(mans, mans))
    } else {
      S = mxMatrix("Diag", p, p, free = TRUE, values = 0.5, name = "S", dimnames = list(mans, mans))
    }
    expN = mxExpectationNormal(covariance = "S", means = "Mu", dimnames = mans)
    if (length(defs)) {
      B  = mxMatrix("Full", length(defs), p, free = TRUE, values = 0,
        labels = outer(defs, mans, function(d, v) paste0(g, "_b_", d, "_", v)), name = "B")
      D  = mxMatrix("Full", 1, length(defs), free = FALSE, labels = paste0("data.", defs), name = "D")
      Mu = mxAlgebra(Mint + D %*% B, name = "Mu", dimnames = list(NULL, mans))
      mxModel(g, mxData(dat, type = "raw"), Mint, B, D, Mu, S, expN, mxFitFunctionML())
    } else {
      Mu = mxAlgebra(Mint, name = "Mu", dimnames = list(NULL, mans))
      mxModel(g, mxData(dat, type = "raw"), Mint, Mu, S, expN, mxFitFunctionML())
    }
  }
  build_ref = function(type) {
    if (length(groups) == 1 && identical(groups, x@name)) {
      return(mxRun(mxModel(one_group(groups, type), name = paste0(type, "Cond")), silent = TRUE))
    }
    mxRun(mxModel(paste0(type, "Cond"), lapply(groups, one_group, type = type),
      mxFitFunctionMultigroup(groups)), silent = TRUE)
  }
  list(Saturated = build_ref("Saturated"), Independence = build_ref("Independence"))
}

#' Effective N of a model: rows that enter the likelihood
#'
#' Counts rows with at least one manifest observed and no missing definition
#' variable, summed over groups. OpenMx already excludes such rows from
#' `numObs`, so this usually equals `numObs`; it is reported as an independent
#' check. `NA` for non-raw data.
#'
#' @param x An [OpenMx::mxModel()] with raw data.
#' @return Integer count of rows entering the likelihood (`NA` for non-raw data).
#' @export
#' @seealso - [umxFitIndices()]
xmu_eff_nobs <- function(x) {
  groups = xmu_model_groups(x)
  sum(vapply(groups, function(g) {
    m   = if (identical(g, x@name)) x else x[[g]]
    if (is.null(m$data) || m$data$type != "raw") return(NA_integer_)
    dat  = m$data$observed
    mans = xmu_model_manifests(m)
    labs = unlist(lapply(c(m@matrices, x@matrices), function(M) as.vector(M$labels)))
    defs = intersect(unique(sub("^data\\.", "", grep("^data\\.", labs, value = TRUE))), names(dat))
    keep = rowSums(!is.na(dat[, mans, drop = FALSE])) > 0
    if (length(defs)) keep = keep & stats::complete.cases(dat[, defs, drop = FALSE])
    sum(keep)
  }, integer(1)))
}

#' Names of the groups contributing to the fit
#'
#' @param x An [OpenMx::mxModel()].
#' @return Character vector of group names: the model itself unless multigroup.
#' @keywords internal
xmu_model_groups <- function(x) {
  if (inherits(x$fitfunction, "MxFitFunctionMultigroup")) {
    sub("\\.fitfunction$", "", x$fitfunction$groups)
  } else {
    x@name
  }
}

#' Manifest names of one group
#'
#' @param m An [OpenMx::mxModel()] (one group).
#' @return Character vector of manifest names: expectation dims, or manifestVars for RAM models.
#' @keywords internal
xmu_model_manifests <- function(m) {
  mans = m$expectation$dims
  if (is.null(mans) || all(is.na(mans))) mans = m@manifestVars
  mans
}

xmuModelMatrices = function(model, manifests) {
  if (model$data$type == "raw"){
    if (any(is.na(model$data$observed))) {
      obsFiml = psych::corFiml(model$data$observed[,manifests], covar = TRUE)
      obsCov = obsFiml$cov
      obsCor = obsFiml$cor
      obsMeans = obsFiml$mean
    } else {
      obsCov = cov(model$data$observed[,manifests])
      obsCor = cov2cor(obsCov)
      obsMeans = colMeans(model$data$observed)
    }
  } else if (model$data$type == "cor") {
    obsCov = NA_real_
    obsCor = model$data$observed
    obsMeans = model$data$means
  } else {
    obsCov = model$data$observed
    obsCor = cov2cor(obsCov)
    obsMeans = model$data$means
  }
  estCov = mxGetExpected(model, "covariance")
  estCor = cov2cor(estCov)
  if(!(length(obsMeans) == 1 && is.na(obsMeans))) {
    estMeans = mxGetExpected(model, "means")
  } else {
    estMeans = NA_real_
  }
  return(list(
    obsCov = obsCov,
    obsCor = obsCor,
    obsMeans = obsMeans,
    estCov = estCov,
    estCor = estCor,
    estMeans = estMeans
  ))
}

xmuResiduals = function(obsCov, obsCor, obsMeans,
                        estCov, estCor, estMeans,
                        which = c("raw", "cor.bentler", "cor.bollen", "cor.mplus")) {
  which = match.arg(which, c("raw", "normalized", "standardized",
                             "standardized.mplus", "cor.bentler",
                             "cor.bollen", "cor.mplus"), several.ok = TRUE)
  # Raw
  raw = list(
    Cov = obsCov - estCov,
    Means = obsMeans - estMeans
  )

  # TODO: Compute normalized and standardized residuals
  # # Normalized
  # normalized = c(
  #   Cov = obsCov - estCov
  #   Means = obsMeans - estMeans
  # )
  #
  # # Standardized
  # standardized = c(
  #   Cov = obsCov - estCov
  #   Means = obsMeans - estMeans
  # )
  #
  # # Standardized - MPLus-style
  # standardized.mplus = c(
  #   Cov = obsCov - estCov
  #   Means = obsMeans - estMeans
  # )

  # Correlation - EQS/Benter-style
  if (length(obsCov) == 1 && is.na(obsCov)) {
    cor.bentler = list(
      Cov = matrix(NA_real_, nrow = nrow(obsCor), ncol = ncol(obsCov)),
      Means = rep(NA_real_, length(obsMeans))
    )
  } else {
    cor.bentler = list(
      Cov = (obsCov - estCov) / sqrt(diag(obsCov) %*% t(diag(obsCov))),
      Means = ifelse(length(obsMeans) == 1 && is.na(obsMeans), NA_real_, (obsMeans - estMeans) / sqrt(diag(obsCov)))
    )
  }

  # Correlation - Bollen-style
  cor.bollen = list(
    Cov = obsCor - estCor,
    Means = ifelse(length(obsMeans) == 1 && is.na(obsMeans), NA_real_, obsMeans / sqrt(diag(obsCov)) - estMeans / sqrt(diag(estCov)))
  )

  # Correlation - MPlus-style (for Information = Observed; http://www.statmodel.com/download/SRMR.pdf)
  if (length(obsCov) == 1 && is.na(obsCov)) {
    cor.mplus = cor.bentler
  } else {
    cor.mplus = cor.bollen
    diag(cor.mplus$Cov) = diag(cor.bentler$Cov)
  }

  return(list(
    raw = raw,
    # normalized = normalized,
    # standardized = standardized,
    # standardized.mplus = standardized.mplus,
    cor.bentler = cor.bentler,
    cor.bollen = cor.bollen,
    cor.mplus = cor.mplus
  )[which])

}

xmuResidualIndices = function(obsCov, obsCor, obsMeans,
                              estCov, estCor, estMeans) {
  # TODO: Add unbiased stdandardized estimators
  # TODO: Add confidence intervals
  resMat = xmuResiduals(obsCov, obsCor, obsMeans,
                        estCov, estCor, estMeans,
                        which = c("raw", "cor.bentler", "cor.bollen", "cor.mplus"))

  # SRMR/SMAR = residuals standardized using the EQS/Bentler approach
  # SRMR_mplus/SMAR_mplus = residuals standardized using the MPlus approach (Information=Observed; http://www.statmodel.com/download/SRMR.pdf)
  # CRMR/CMAR = residuals standardized using the Bollen approach (residual correlations)

  if(length(obsMeans) == 1 && is.na(obsMeans)) {
    
    if (!(length(obsCov) == 1 && is.na(obsCov))) {
      RMR        = sqrt(mean(    c(vech(resMat$raw$Cov))^2))
      SRMR       = sqrt(mean(    c(vech(resMat$cor.bentler$Cov))^2))
      SRMR_mplus = sqrt(mean(    c(vech(resMat$cor.mplus$Cov))^2))
      MAR        =      mean(abs(c(vech(resMat$raw$Cov))))
      SMAR       =      mean(abs(c(vech(resMat$cor.bentler$Cov))))
      SMAR_mplus =      mean(abs(c(vech(resMat$cor.mplus$Cov))))
    } else {
      RMR = SRMR = SRMR_mplus = MAR = SMAR = SMAR_mplus = NA
    }
    CRMR = sqrt(mean(    c(vech(resMat$cor.bollen$Cov))^2))
    CMAR =      mean(abs(c(vech(resMat$cor.bollen$Cov))))

  } else {
    if (!(length(obsCov) == 1 && is.na(obsCov))) {
      RMR        = sqrt(mean(    c(vech(resMat$raw$Cov),         resMat$raw$Means)^2))
      SRMR       = sqrt(mean(    c(vech(resMat$cor.bentler$Cov), resMat$cor.bentler$Means)^2))
      SRMR_mplus = sqrt(mean(    c(vech(resMat$cor.mplus$Cov),   resMat$cor.mplus$Means)^2))
      MAR        =      mean(abs(c(vech(resMat$raw$Cov),         resMat$raw$Means)))
      SMAR       =      mean(abs(c(vech(resMat$cor.bentler$Cov), resMat$cor.bentler$Means)))
      SMAR_mplus =      mean(abs(c(vech(resMat$cor.mplus$Cov),   resMat$cor.mplus$Means)))
    } else {
      RMR = SRMR = SRMR_mplus = MAR = SMAR = SMAR_mplus = NA
    }
    CRMR = sqrt(mean(    c(vech(resMat$cor.bollen$Cov), resMat$cor.bollen$Means)^2))
    CMAR =      mean(abs(c(vech(resMat$cor.bollen$Cov), resMat$cor.bollen$Means)))
  }

  # Return valus structure in preparation for confidence intervals and SEs
  return(
    list(
      estimate = c(RMR = RMR, SRMR = SRMR, SRMR_mplus = SRMR_mplus, CRMR = CRMR,
                   MAR = MAR, SMAR = SMAR, SMAR_mplus = SMAR_mplus, CMAR = CMAR)
    )
  )
}

#' Print fit indices
#'
#' @param x A [umxFitIndices()] data.frame.
#' @param digits Decimals to show.
#' @param ... Ignored.
#' @method print umxFitIndices
#' @export
print.umxFitIndices = function(x, digits = max(1L, getOption("digits") - 3L), ...) {
  if (!inherits(x, "umxFitIndices")) {
    stop(gettextf("'x' must inherit from class %s", dQuote("umxFitIndices")),
         domain = NA)
  }
  models = x$model
  vals = x[, setdiff(names(x), "model"), drop = FALSE]
  if (nrow(x) == 1L) {
    cat("\nFit indices for", models, "\n\n")
    tab = data.frame(index = names(vals), value = round(as.numeric(vals[1, ]), digits),
      stringsAsFactors = FALSE)
    print(tab, row.names = FALSE, right = FALSE)
  } else {
    cat("\nFit indices\n\n")
    rounded = vals
    for (nm in names(rounded)) {
      rounded[[nm]] = round(rounded[[nm]], digits)
    }
    print(data.frame(model = models, rounded, stringsAsFactors = FALSE),
      row.names = FALSE, right = FALSE)
  }
  invisible(x)
}

