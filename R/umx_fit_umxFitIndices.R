#' Get additional fit-indices for a model with umxFitIndices
#'
#' Computes a variety of fit indices. Originated in this thread: http://openmx.ssri.psu.edu/thread/765
#'
#' Note: This function is currently not robust across multi-group designs or
#' definition variables. It is designed to provide residual-based fit indices
#' (SRMR, CRMR, SMAR, CMAR, etc.) and less-often reported fit indices where
#' Reviewer 2 wants something other than CFA/TLI/RMSEA.
#'
#'
#' Fit information reported includes:
#'
#' Model characterstics:
#'   numObs, estimatedParameters,
#'   observedStatistics, observedSummaryStatistics,
#'   Minus2LogLikelihood, degreesOfFreedom
#'
#' Chi-squared test:
#'   Chi, ChiDoF, p (of Chi), ChiPerDoF,
#'
#' Noncentrality-based indices:
#'   RMSEA, RMSEACI, RMSEANull, RMSEAClose (p value), independenceRMSEA,
#'   NCP, NCPCI, F0, F0CI, Mc (aka NCI, MFI)
#'
#' Comparative fit indices:
#'   TLI (aka NNFI), CFI, IFI, PRATIO, PCFI
#'
#' Residual-based indices (with and without mean  structures):
#'   RMR, SRMR, SRMR_mplus, CRMR,
#'   MAR, SMAR, SMAR_mplus, CMAR
#'
#' Information-theory criteria (computed using chi-square or -2LL; df or paramters penalties)
#'   AIC, AICc, BIC, SABIC, CAIC, BCC
#'   ECVI, ECVICI, MECVI, MECVICI
#'
#' LISREL and other early fit indices (we recommend not reporting these)
#'   GFI, AGFI, PGFI, GH, NFI, PNFI, RFI
#'
#'
#' Want more? Open an Issue at GitHub.
#'
#' @param model The \code{\link{mxModel}} for which you want fit indices.
#' @param ... Additional parameters passed to \code{\link{summary.MxModel}}.
#' @return List of fit statistics
#' @export
#' @family Reporting functions
#' @author Brenton M. Wiernik, Athanassios Protopapas, Paolo Ghisletta, Markus Brauer
#' @references -
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
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
umxFitIndices = function(model, ...) {
  if(!umx_is_RAM(model)){
    message("Not fully tested against non-RAM models...")
  }
  modSumm      = summary(model, ...)
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

  if (model$data$type == "raw") {
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
  if (model$data$type == "raw" || ! is.na(model$data$means)) {
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
  if (Fvalue == Chi) {
    infoCritChi = infoCrit
    infoCritDev = infoCrit - Chi + deviance
  } else {
    infoCritDev = infoCrit
    infoCritChi = infoCrit - deviance + Chi
  }
  informationCriteria = list(Chi = infoCritChi,
                             deviance = infoCritDev)
  ECVI    = (Chi + 2 * nParam) / numObs
  MECVI   = (Chi + 2 * nParam * numObs / (numObs - nManifest - 1)) / numObs
  ciECVI  = (ciNCP + dfChi + 2 * nParam) / numObs
  ciMECVI = (ciNCP + dfChi + 2 * nParam * numObs / (numObs - nManifest - 1)) / numObs


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

  indices =
    list(
      numObs = numObs,
      estimatedParameters = nParam,
      observedStatistics  = modSumm$observedStatistics,
      observedSummaryStatistics = ifelse(model$data$type == "raw" || !(length(model$data$means) == 1 && is.na(model$data$means)), q_m, q),

      Minus2LogLikelihood = deviance,
      degreesOfFreedom = df,

      Chi = Chi,
      ChiDoF = dfChi,
      p = pChi,
      ChiPerDoF = ChiPerDf,

      RMSEA = modSumm$RMSEA,
      RMSEACI = ciRMSEA,
      RMSEANull = modSumm$RMSEANull,
      RMSEAClose = modSumm$RMSEAClose,
      independenceRMSEA = RMSEAInd,
      NCP = NCP, NCPCI = ciNCP,
      F0 = F0, F0CI = ciF0,
      Mc = Mc,

      residualIndices = residualIndices,

      GFI = GFI, AGFI = AGFI, PGFI = PGFI, GH = GH,
      NFI = NFI, RFI = RFI, IFI = IFI,
      TLI = modSumm$TLI, CFI = CFI,
      PRATIO = PRATIO, PNFI = PNFI, PCFI = PCFI,

      informationCriteria = informationCriteria,
      ECVI = ECVI, ECVICI = ciECVI, MECVI = MECVI, MECVICI = ciMECVI
    )
  class(indices) = c("umxFitIndices", class(indices))
  return(indices)
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
  if(!(length(obsMeans == 1) && is.na(obsMeans))) {
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

  if(!(length(obsCov) == 1 && is.na(obsCov))) {
    RMR        = sqrt(mean(    c(vech(resMat$raw$Cov),         resMat$raw$Means)^2))
    SRMR       = sqrt(mean(    c(vech(resMat$cor.bentler$Cov), resMat$cor.bentler$Means)^2))
    SRMR_mplus = sqrt(mean(    c(vech(resMat$cor.mplus$Cov),   resMat$cor.mplus$Means)^2))
    MAR        =      mean(abs(c(vech(resMat$raw$Cov),         resMat$raw$Means)))
    SMAR       =      mean(abs(c(vech(resMat$cor.bentler$Cov), resMat$cor.bentler$Means)))
    SMAR_mplus =      mean(abs(c(vech(resMat$cor.mplus$Cov),   resMat$cor.mplus$Means)))

    RMR_nomean        = sqrt(mean(    c(vech(resMat$raw$Cov))^2))
    SRMR_nomean       = sqrt(mean(    c(vech(resMat$cor.bentler$Cov))^2))
    SRMR_mplus_nomean = sqrt(mean(    c(vech(resMat$cor.mplus$Cov))^2))
    MAR_nomean        =      mean(abs(c(vech(resMat$raw$Cov))))
    SMAR_nomean       =      mean(abs(c(vech(resMat$cor.bentler$Cov))))
    SMAR_mplus_nomean =      mean(abs(c(vech(resMat$cor.mplus$Cov))))
  } else {
    RMR = SRMR = SRMR_mplus = MAR = SMAR = SMAR_mplus =
      RMR_nomean = SRMR_nomean = SRMR_mplus_nomean = MAR_nomean = SMAR_nomean =
      NA
  }
  CRMR = sqrt(mean(    c(vech(resMat$cor.bollen$Cov), resMat$cor.bollen$Means)^2))
  CMAR =      mean(abs(c(vech(resMat$cor.bollen$Cov), resMat$cor.bollen$Means)))

  CRMR_nomean = sqrt(mean(    c(vech(resMat$cor.bollen$Cov))^2))
  CMAR_nomean =      mean(abs(c(vech(resMat$cor.bollen$Cov))))

  if(length(obsMeans) == 1 && is.na(obsMeans)) {
    RMR = RMR_nomean
    SRMR = SRMR_nomean
    SRMR_mplus = SRMR_mplus_nomean
    MAR = MAR_nomean
    SMAR = SMAR_nomean
    SMAR_mplus = SMAR_mplus_nomean
    CRMR = CRMR_nomean
    CMAR = CMAR_nomean
  }

  return(
    list(
      RMR = RMR, SRMR = SRMR, SRMR_mplus = SRMR_mplus, CRMR = CRMR,
      MAR = MAR, SMAR = SMAR, SMAR_mplus = SMAR_mplus, CMAR = CMAR,
      RMR_nomean = RMR_nomean, SRMR_nomean = SRMR_nomean,
      SRMR_mplus_nomean = SRMR_mplus_nomean, CRMR_nomean = CRMR_nomean,
      MAR_nomean = MAR_nomean, SMAR_nomean = SMAR_nomean,
      SMAR_mplus_nomean = SMAR_mplus_nomean, CMAR_nomean = CMAR_nomean
    )
  )
}

print.umxFitIndices = function(x, digits = max(1L, getOption("digits") - 3L), ...) {
  if (!inherits(x, "umxFitIndices")) {
    stop(gettextf("'x' must inherit from class %s", dQuote("umxFitIndices")),
         domain = NA)
  }
  cat('\n')
  cat('Model characteristics\n')
  cat('=====================\n')
  cat('Number of observations (sample size):  ', round(x$numObs, digits), '\n')
  cat('Number of estimated parameters:        ', round(x$estimatedParameters, digits), '\n')
  cat('Number of observed statistics:         ', round(x$observedStatistics, digits), '\n')
  cat('Number of observed summary statistics: ', round(x$observedSummaryStatistics, digits), '\n')
  cat('   (means, variances, covariances)\n')
  cat('Deviance (-2 * LogLikelihood):         ', round(x$Minus2LogLikelihood, digits), '\n')
  cat('\n\n')

  cat('Chi squared test\n')
  cat('================\n')
  if (x$Chi <= 0) {
    cat("chi-square:  ", "\u03C7\u00B2 ( df=", x$ChiDoF, " ) = ",
        round(x$Chi, digits),
        ",  p = ", format.pval(x$p, digits, eps = 0), "\n", sep = "")
  } else {
    cat("chi-square:  ", "\u03C7\u00B2 ( df=", x$ChiDoF, " ) = ",
        format(round(x$Chi, max(0, digits - log10(x$Chi)))),
        ",  p = ", format.pval(x$p, digits, eps = 0), "\n", sep = "")
  }
  cat('chi-square per df (\u03C7\u00B2 / df):', round(x$ChiPerDoF, digits), '\n')
  cat('\n\n')

  cat('Noncentrality-based indices\n')
  cat('===========================\n')
  if (length(x$RMSEASquared) == 1 && !is.na(x$RMSEASquared) && x$RMSEASquared < 0) {
    cat('** (Non-centrality parameter is negative)')
  }
  cat('RMSEA (root mean squared error of approximation): ', round(x$RMSEA, digits),
      '  [95% CI (', round(x$RMSEACI[1], digits), ', ',
      round(x$RMSEACI[2], digits), ')]', '\n', sep = "")
  cat('  Prob(RMSEA <= ', round(x$RMSEANull, digits), "): ",
      format.pval(x$RMSEAClose, digits, eps = 0), "\n", sep = "")
  cat('RMSEA of independence (null or baseline) model:  ', round(x$independenceRMSEA, digits), '\n')
  cat('Noncentrality parameter (NCP or d): ', round(x$NCP, digits),
      '  [95% CI (', round(x$NCPCI[1], digits), ', ',
      round(x$NCPCI[2], digits), ')]', '\n', sep = "")
  cat('Rescaled NCP (F0 or t):             ', round(x$F0,  digits),
      '  [95% CI (', round(x$F0CI[1], digits), ', ',
      round(x$F0CI[2], digits), ')]', '\n', sep = "")
  cat('Mc (McDonald centrality index): ', round(x$Mc, digits), '\n')
  cat('   [Also called MFI (McDonald fit index) or NCI (Noncentrality index)]\n')
  cat('\n\n')

  cat('Incremental fit indices\n')
  cat('=======================\n')
  cat('CFI (Comparative fit index):', round(x$CFI, digits), '\n')
  cat('   [Also called RNI (Relative noncentrality index)]\n')
  cat('TLI (Tucker-Lewis index):   ', round(x$TLI, digits), '\n')
  cat('   [Also called NNFI (Non-normed fit index)]\n')
  cat('\n')
  cat('Parsimony ratio (df / df_indepedence):', round(x$PRATIO, digits), '\n')
  cat('PCFI (Parsimonious CFI):           ', round(x$PCFI,      digits), '\n')
  cat('IFI (Incremental fit index):       ', round(x$IFI,       digits), '\n')
  cat('   [Also called BL89 (Bollen, 1989, fit index)]\n')

  if(!is.na(x$independenceRMSEA) && x$independenceRMSEA <= .158) {
    message(paste0("\nNote: Independence (Null) model has RMSEA = ",
                   round(x$independenceRMSEA, digits),
                   ".\nIf the model shows good fit (RMSEA <= .05), TLI has a maximum value <= .90.",
                   "\nInterpret incremental fit indices (TLI, CFI, etc.) with caution.\n"))
  }
  cat('\n\n')

  cat('Residuals-based indices\n')
  cat('=======================\n')

  if (is.na(x$residualIndices$SRMR)) {
    cat("Observed values are correlations",
        "  Use CRMR instead of RMR or SRMR.",
        "  Use CMAR instead of MAR or SMAR.",
        "\n")
    cat('CRMR (Correlation root mean squared residual):  ',
        round(x$residualIndices$CRMR, digits),
        if(!is.na(x$residualIndices$CRMR_nomean)) {c('(with mean structure)\n',
                                                     ' [Bollen method SRMR]', rep(' ', 13), round(x$residualIndices$CRMR_nomean, digits),
                                                     '(without mean structure)\n\n')} else {'\n   [Bollen method SRMR]\n'} )
    cat('CMAR (Correlation mean absolute residual): ',
        round(x$residualIndices$CMAR, digits),
        if(!is.na(x$residualIndices$CMAR_nomean)) {c('(with mean structure)\n ',
                                                     ' [Bollen method SMAR]', rep(' ', 2), round(x$residualIndices$CMAR_nomean, digits),
                                                     '(without mean structure)\n\n')} else {'\n   [Bollen method SMAR]\n'} )
  } else {
    cat('RMR (Root mean squared residual):               ',
        round(x$residualIndices$RMR, digits),
        if(!is.na(x$residualIndices$RMR_nomean)) {c('(with mean structure)\n',
                                                    rep(' ', 24), round(x$residualIndices$RMR_nomean, digits),
                                                    '(without mean structure)')}, '\n')
    cat('SRMR (Standardized root mean squared residual):\n')
    cat('  Bentler method (lavaan, sem, EQS, AMOS):      ',
        round(x$residualIndices$SRMR, digits),
        if(!is.na(x$residualIndices$SRMR_nomean)) {c('(with mean structure)\n',
                                                     rep(' ', 24), round(x$residualIndices$SRMR_nomean, digits),
                                                     '(without mean structure)')}, '\n')
    cat('  MPlus method:                                 ',
        round(x$residualIndices$SRMR_mplus, digits),
        if(!is.na(x$residualIndices$SRMR_mplus_nomean)) {c('(with mean structure)\n',
                                                           rep(' ', 24), round(x$residualIndices$SRMR_mplus_nomean, digits),
                                                           '(without mean structure)')}, '\n')
    cat('CRMR (Correlation root mean squared residual):  ',
        round(x$residualIndices$CRMR, digits),
        if(!is.na(x$residualIndices$CRMR_nomean)) {c('(with mean structure)\n',
                                                     ' [Bollen method SRMR]', rep(' ', 13), round(x$residualIndices$CRMR_nomean, digits),
                                                     '(without mean structure)\n\n')} else {'\n   [Bollen method SRMR]\n'} )

    cat('MAR (Mean absolute residual):                   ',
        round(x$residualIndices$MAR,  digits),
        if(!is.na(x$residualIndices$MAR_nomean)) {c('(with mean structure)\n',
                                                    rep(' ', 24), round(x$residualIndices$MAR_nomean, digits),
                                                    '(without mean structure)')}, '\n')
    cat('SMAR (Standardized mean absolute residual):\n')
    cat('  Bentler method (lavaan, sem, EQS, AMOS):      ',
        round(x$residualIndices$SMAR, digits),
        if(!is.na(x$residualIndices$SMAR_nomean)) {c('(with mean structure)\n',
                                                     rep(' ', 24), round(x$residualIndices$SMAR_nomean, digits),
                                                     '(without mean structure)')}, '\n')
    cat('  MPlus method:                                 ',
        round(x$residualIndices$SMAR_mplus, digits),
        if(!is.na(x$residualIndices$SMAR_mplus_nomean)) {c('(with mean structure)\n',
                                                           rep(' ', 24), round(x$residualIndices$SMAR_mplus_nomean, digits),
                                                           '(without mean structure)')}, '\n')
    cat('CMAR (Correlation mean absolute residual):      ',
        round(x$residualIndices$CMAR, digits),
        if(!is.na(x$residualIndices$CMAR_nomean)) {c('(with mean structure)\n',
                                                     ' [Bollen method SMAR]', rep(' ', 13), round(x$residualIndices$CMAR_nomean, digits),
                                                     '(without mean structure)\n\n')} else {'\n   [Bollen method SMAR]\n'} )
  }
  cat('\n')

  colnames(x$informationCriteria$Chi) =
    colnames(x$informationCriteria$deviance) =
    c(" |  df Penalty", " |  Parameters Penalty", " |  Sample-Size Adjusted")

  rownames(x$informationCriteria$Chi) =
    rownames(x$informationCriteria$deviance) =
    c('AIC (Akaike information criterion):',
      'BIC (Bayesian information criterion):',
      'CAIC (Consistent AIC):',
      'BCC (Browne-Cudeck criterion):'
    )

  cat('Information-based fit indices (computed using \u03C7\u00B2)\n')
  cat('=================================================\n')
  print(round(x$informationCriteria$Chi, digits))
  cat("\n")
  cat('ECVI (Expected cross-validation index): ', round(x$ECVI, digits),
      '  [95% CI (', round(x$ECVICI[1], digits), ', ',
      round(x$ECVICI[2], digits), ')]', '\n', sep = "")
  cat('MECVI (Modified ECVI): ', round(x$ECVI, digits),
      '  [95% CI (', round(x$MECVICI[1], digits), ', ',
      round(x$MECVICI[2], digits), ')]', '\n', sep = "")
  cat('\n\n')

  cat('Information-based fit indices (computed using -2lnL)\n')
  cat('====================================================\n')
  print(round(x$informationCriteria$deviance, digits))
  cat("\n\n")

  cat('LISREL and other early fit indices\n')
  cat('==================================\n')
  cat('** These indices are strongly discouraged due to numerous problems.',
      '\n   It is recommended that you do not report them.\n')
  cat('GFI (Goodness of fit index): ', round(x$GFI,  digits), '\n')
  cat('AGFI (Adjusted GFI):         ', round(x$AGFI, digits), '\n')
  cat('PGFI (Parsimonious GFI):     ', round(x$PGFI, digits), '\n')
  cat('NFI (Normed fit index):      ', round(x$NFI,  digits), '\n')
  cat('PNFI (Parsimonious NFI):     ', round(x$PNFI, digits), '\n')
  cat('   [Also called PFI (Parsimonious fit index)]\n')
  cat('RFI (Relative fit index):    ', round(x$RFI,  digits), '\n')
  cat('GH (Gamma hat):              ', round(x$GH,   digits), '\n')
  cat('   [Estimated population GFI]')

  invisible(x)

}
