#' Get additional fit-indices for a model with umxFitIndices
#'
#' Computes a variety of fit indices. Originated in this thread: http://openmx.ssri.psu.edu/thread/765
#'
#' Note: This function is currently not robust across multi-group designs or
#' definition variables. It is designed to provide residual-based fit indices
#' (SRMR, CRMR, SMAR, CMAR, etc.) and less-often reported fit indices where
#' Reviewer 2 wants something other than CFA/TLI/RMSEA.
#'
#' Fit information reported includes:
#' N, deviance, N.parms, N.manifest,
#' Chisq, df, p.Chisq, Chisq/df,
#' NCP, F0, Mc, NCI, MFI,
#' RMSEA, indep.RMSEA,
#' RMR, SRMR, SRMR_mplus, CRMR,
#' MAR, SMAR, CMAR,
#' GFI, AGFI, PGFI, GH,
#' NFI, RFI, IFI, BL89, RNI,
#' NNFI, TLI, CFI,
#' PRATIO, PNFI, PCFI,
#' AICchi, AICdev,
#' CAICchi, CAICdev,
#' BICchi, BICdev,
#' BCC, ECVI, MECVI
#'
#' Want more? File a report at GitHub
#'
#' @param model The \code{\link{mxModel}} for which you want fit indices.
#' @param ... Additional parameters passed \code{\link{summary.MxModel}}.
#' @return Table of fit statistics
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
    } else if (!is.na(model$expectation$dims)) {
      manifests = model$expectation$dims[model$expectation$dims %in% colnames(model$data$observed)]
    } else {
      # TODO: Extract dimnames from expectation slot objects according to the MxExpectation class
      manifests = NULL
    }
  } else {
    manifests = colnames(model$data$observed)
  }

  if (!is.null(manifests)) {
    nManifest = length(manifests)
    q         = (nManifest * (nManifest + 1)) / 2
  } else {
    warning(paste("Models with raw data and expectation type:", class(model$expectation),
                  "only supported if dimnames are specified in the expectation.",
                  "\nFit indices computed using the number of observed variables will be `NA`."))
    nManifest = NA
    q         = NA
  }

  RMSEASquared = modSumm$RMSEASquared
  ciRMSEA      = modSumm$RMSEACI
  NCP          = RMSEASquared * (numObs * dfChi) # Chi - dfChi
  ciNCP        = ciRMSEA^2 * (modSumm$numObs * modSumm$ChiDoF)
  F0           = NCP / numObs
  ciF0         = ciNCP / numObs
  NFI          = (devInd - deviance) / (devInd - devSat)
  RFI          = 1 - (Chi / dfChi) / ((ChiInd / dfChiInd)
  IFI          = (ChiInd - Chi) / (ChiInd - dfChi)
  CFI          = modSumm$CFI
  PRATIO       = dfChi / dfChiInd
  PNFI         = PRATIO * NFI
  PCFI         = PRATIO * CFI
  # https://doi.org/10.1080/10705519409539970 gives a different formula for PCFI:
  #   PCFI = CFI * df / ((nManifest * (nManifest - 1)) / 2)
  # But most users of PCFI use the form given here
  GH           =  nManifest / (nManifest + 2 * F0)
  Mc = NCI = MFI = exp(-0.5 * NCP / numObs)

  # LISREL Indices
  GFI =
    1 - (
      sum(
        diag(
          ((solve(estimate.cor) %*% observed.cor) - Id.manifest) %*%
            ((solve(estimate.cor) %*% observed.cor) - Id.manifest)
        )
      ) /
        sum(
          diag(
            (solve(estimate.cor) %*% observed.cor) %*%
              (solve(estimate.cor) %*% observed.cor)
          )
        )
    )
  AGFI =  1 - (q / df) * (1 - GFI)
  PGFI =  GFI * df / q

  # Information-based indices
  infoCrit = rbind(modSumm$informationCriteria,
                   `CAIC:` = c(Chi - (log(numObs) + 1) * df,
                               Chi + (log(numObs) + 1) * nParam)
                   )
  infoCritDev = infoCrit - Chi + deviance
  ECVI     =  infoCrit["AIC:", "par"] / numbObs
  BCC      =  infoCrit["AIC:", "par"] / (numObs - nManifest - 2)
  MECVI    =  1 / BCC
  informationCriteria = list(Chi = infoCrit,
                            deviance = infoCritDev)

  # Residual based indices
  residualIndices = xmuResidualIndices(model, nManifest, q)

  RMSEASquaredInd = (ChiInd/(dfChiInd) - 1)/numObs
  RMSEAInd = ifelse(RMSEASquaredInd < 0, 0, sqrt(RMSEASquaredInd))

  indices =
    list(
      numObs = numObs,
      estimatedParameters = nParam,
      observedStatistics  = modSumm$observedStatistics,
      observedSummaryStatistics = ifelse(is.null(estimate.means), q, q_m)

      Minus2LogLikelihood = deviance,
      degreesOfFreedom = df,

      Chi = Chi,
      ChiDoF = dfChi,
      p = pChi,
      ChiPerDoF = ChiPerDf,

      RMSEA = modSumm$RMSEA
      RMSEACI = ciRMSEA,
      RMSEAClose = modSumm$RMSEAClose,
      independenceRMSEA = RMSEAInd,
      NCP = NCP, NCPCI = ciNCP,
      F0 = F0, F0CI = ciF0,
      Mc = Mc, NCI = NCI, MFI = MFI,

      residualIndices = residualIndices,

      GFI = GFI, AGFI = AGFI, PGFI = PGFI, GH = GH,
      NFI = NFI, RFI = RFI, IFI = IFI,
      TLI = modSumm$TLI, CFI = CFI,
      PRATIO = PRATIO, PNFI = PNFI, PCFI = PCFI,

      informationCriteria = informationCriteria,
      BCC = BCC, ECVI = ECVI, MECVI = MECVI
    )
  class(indices) = c("umxFitIndices", class(indices))
  return(indices)
}

xmuResidualIndices = function(model, nManifest,
                              q = (nManifest * (nManifest + 1)) / 2) {
  if (!inherits(model$fitfunction, "MxFitFunctionML")) {
    warning("Bias correction and confidence intervals for residual-based\nfit indices currently only designed to work with maximum likelihood estimation.")
  }
  if (!is.null(manifests)) {
    if (is.null(q)) {
      q = (nManifest * (nManifest + 1)) / 2
    }
    if (model$data$type == "raw"){
      if (any(is.na(model$data$observed))) {
        obsFiml = psych::corFiml(model$data$observed[,manifests], covar = TRUE)
        observed.cov = obsFiml$cov
        observed.cor = obsFiml$cor
        observed.means = obsFiml$mean
      } else {
        observedCov = cov(model$data$observed[,manifests])
        observed.cor = cov2cor(observed.cov)
        observed.means = colMeans(model$data$observed)
      }
    } else if (model$data$type == "cor") {
      observed.cov = NULL
      observed.cor = model$data$observed
      observed.means = model$data$means
    } else {
      observed.cov = model$data$observed
      observed.cor = cov2cor(observed.cov)
      observed.means = model$data$means
    }
    estimate.cov = mxGetExpected(model, "covariance")
    estimate.cor = cov2cor(estimate.cov)
    if(!is.na(observed.means)) {
      estimate.means = mxGetExpected(model, "means")
    } else {
      estimate.means = NULL
    }
    Id.manifest = diag(nManifest)
  } else {
    return(NULL)
  }

  # TODO: Add unbiased stdandardized estimators
  # TODO: Add confidence intervals
  residual.cov =  observed.cov - estimate.cov
  residual.cor =  observed.cor - estimate.cor
  if(!is.null(observed.cov)) {
    invS = diag( 1 / sqrt(diag(observed.cov)))
  } else {
    invS = NA
  }
  if(!is.null(estimate.means)) {
    q_m = q + N.manifest
    residual.means = observed.means - estimate.means
    residual.meansZ = observed.means / sqrt(diag(observed.cov)) - estimate.means / sqrt(diag(estimate.cov))
    if(!is.null(observed.cov)) {
      RMR        =  sqrt( ( sum( vech(residual.cov)^2 ) + sum(residual.means^2) ) / q )
      SRMR       =  sqrt( ( sum( vech(invS %*% residual.cov %*% invS)^2 ) + sum( (residual.means * diag(invS))^2 ) ) / q )  # EQS/Bentler approach
      SRMR_mplus =  sqrt( ( sum( vechs(residual.cor)^2 ) + sum( ( diag(residual.cov) * diag(invS^2) )^2 ) + sum(residual.meansZ^2) ) / q )
      MAR        =        ( sum( abs( vech(residual.cov) ) ) + sum( abs( residual.means ) ) ) / q
      SMAR       =        ( sum( abs( vech(invS %*% residual.cov %*% invS) ) ) + sum( abs(residual.means * diag(invS)) ) ) / q    # Bentler approach

      RMR_nomean        =  sqrt(   sum( vech(residual.cov)^2 ) / q )
      SRMR_nomean       =  sqrt(   sum( vech(invS %*% residual.cov %*% invS)^2 ) / q )  # EQS/Bentler approach
      SRMR_mplus_nomean =  sqrt( ( sum( vechs(residual.cor)^2 ) + sum( ( diag(residual.cov) * diag(invS^2) )^2 ) ) / q )
      MAR_nomean        =  sum(    abs( vech(residual.cov) ) ) / q
      SMAR_nomean       =  sum(    abs( vech(invS %*% residual.cov %*% invS) ) ) / q    # Bentler approach

    } else {
      RMR = SRMR = SRMR_mplus = "Observed values are correlations, use CRMR instead."
      MAR = SMAR = "Observed values are correlations, use CMAR instead."
      RMR_nomean = SRMR_nomean = SRMR_mplus_nomean = "Observed values are correlations, use CRMR_nomean instead."
      MAR_nomean = SMAR_nomean = "Observed values are correlations, use CMAR_nomean instead."
    }
    CRMR         =  sqrt( ( sum( vechs(residual.cor^2) ) + sum(residual.meansZ^2) ) / q )    # Bollen approach
    CMAR         =        ( sum( abs( vechs(residual.cor) ) ) + sum( abs(residual.meansZ) ) ) / q  # Bollen approach

    CRMR_nomean  =  sqrt( sum( vechs(residual.cor^2) ) / q )    # Bollen approach
    CMAR_nomean  =        sum( abs( vechs(residual.cor) ) ) / q  # Bollen approach

  } else {
    RMR_nomean = SRMR_nomean = SRMR_mplus_nomean = MAR_nomean = SMAR_nomean = CRMR_nomean = CMAR_nomean = NA
    if(!is.null(observed.cov)) {
      RMR        =  sqrt(   sum( vech(residual.cov)^2 ) / q )
      SRMR       =  sqrt(   sum( vech(invS %*% residual.cov %*% invS)^2 ) / q )  # EQS/Bentler approach
      SRMR_mplus =  sqrt( ( sum( vechs(residual.cor)^2 ) + sum( ( diag(residual.cov) * diag(invS^2) )^2 ) ) / q )
      MAR        =  sum(    abs( vech(residual.cov) ) ) / q
      SMAR       =  sum(    abs( vech(invS %*% residual.cov %*% invS) ) ) / q    # Bentler approach
    } else {
      RMR = SRMR = SRMR_mplus = NULL
      MAR = SMAR = NULL
    }
    CRMR         =  sqrt( sum( vechs(residual.cor^2) ) / q )    # Bollen approach
    CMAR         =        sum( abs( vechs(residual.cor) ) ) / q # Bollen approach
  }

  return(
    list(
      RMR = RMR, SRMR = SRMR, SRMR_mplus = SRMR_mplus, CRMR = CRMR,
      MAR = MAR, SMAR = SMAR, CMAR = CMAR,
      RMR_nomean = RMR_nomean, SRMR_nomean = SRMR_nomean,
      SRMR_mplus_nomean = SRMR_mplus_nomean, CRMR_nomean = CRMR_nomean,
      MAR_nomean = MAR_nomean, SMAR_nomean = SMAR_nomean, CMAR_nomean = CMAR_nomean
    )
  )
}

print.umxFitIndices = function(x, digits = max(1L, getOption("digits") - 3L), ...) {
  if (!inherits(x, "umxFitIndices")) {
    stop(gettextf("'x' must inherit from class %s", dQuote("umxFitIndices")),
         domain = NA)
  }
  cat('Model characteristics\n')
  cat('=====================\n')
  cat('Number of observations (sample size):  ', round(x$numObs, digits), '\n')
  cat('Number of estimated parameters:        ', round(x$estimatedParameters, digits), '\n')
  cat('Number of observed statistics:         ', round(x$observedStatistics, digits), '\n')
  cat('Number of observed summary statistics: ', round(x$observedSummaryStatistics, digits), '\n')
  cat('   (Variable means, variances, covariances)\n')
  cat('Deviance (-2 * LogLikelihood):         ', round(x$Minus2LogLikelihood, digits), '\n')
  cat('\n')

  cat('Chi squared test\n')
  cat('================\n')
  cat("chi-square:  ", "\u03C7² ( df=", x$ChiDoF, " ) = ",
      format(round(x$Chi, max(0, digits - log10(x$Chi)))),
      ",  p = ", format.pval(x$p, digits, eps = 0), "\n", sep = ""), "\n", sep = "")
  cat('chi-square per df (\u03C7² / df):', round(x$ChiPerDoF, digits), '\n')
  cat('\n')

  cat('Noncentrality-based indices\n')
  cat('===========================\n')
  cat('RMSEA (root mean squared error of approximation):', round(x$RMSEA, digits), '\n')
  cat('RMSEA of independence (null or baseline) model:  ', round(x$indep.RMSEA, digits), '\n')
  cat('Noncentrality parameter (NCP or d):', round(x$NCP, digits), '\n')
  cat('Rescaled NCP (F0 or t):            ', round(x$F0,  digits), '\n')
  cat('Mc (McDonald centrality index): ', round(x$MFI, digits), '\n')
  cat('   [Also called MFI (McDonald fit index) or NCI (Noncentrality index)]\n')
  cat('\n')

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
  cat('\n')

  cat('Residuals-based indices\n')
  cat('=======================\n')

  if (is.null(x$SRMR)) {
    cat("Observed values are correlations",
        "  Use CRMR instead of RMR or SRMR.",
        "  Use CMAR instead of MAR or SMAR.",
        "\n")
  } else {
    cat('RMR (Root mean squared residual):', round(x$residualIndices$RMR, digits), if(!is.na(x$residualIndices$RMR_nomean)) {paste('(with mean structure)   ', round(x$residualIndices$RMR_nomean, digits), '(without mean structure)')}, '\n')
    cat('SRMR (Standardized root mean squared residual):\n')
    cat('  Bentler method (lavaan, sem, EQS, AMOS):    ', round(x$residualIndices$SRMR, digits),       if(!is.na(x$residualIndices$SRMR_nomean)) {paste('(with mean structure)   ',       round(x$residualIndices$SRMR_nomean, digits), '(without mean structure)')}, '\n')
    cat('  MPlus method:                               ', round(x$residualIndices$SRMR_mplus, digits), if(!is.na(x$residualIndices$SRMR_mplus_nomean)) {paste('(with mean structure)   ', round(x$residualIndices$SRMR_mplus_nomean, digits), '(without mean structure)')}, '\n')
    cat('CRMR (Correlation root mean squared residual):', round(x$residualIndices$CRMR, digits),       if(!is.na(x$residualIndices$CRMR_nomean)) {paste('(with mean structure)   ',       round(x$residualIndices$CRMR_nomean, digits), '(without mean structure)')}, '\n')
    cat('   [Bollen method SRMR]\n')
    cat('\n')

    cat('MAR (Mean absolute residual):              ', round(x$residualIndices$MAR,  digits), if(!is.na(x$residualIndices$MAR_nomean)) {paste('(with mean structure)   ',  round(x$residualIndices$MAR_nomean, digits), '(without mean structure)')}, '\n')
    cat('SMAR (Standardized mean absolute residual):', round(x$residualIndices$SMAR, digits), if(!is.na(x$residualIndices$SMAR_nomean)) {paste('(with mean structure)   ', round(x$residualIndices$SMAR_nomean, digits), '(without mean structure)')}, '\n')
    cat('CMAR (Correlation mean absolute residual): ', round(x$residualIndices$CMAR, digits), if(!is.na(x$residualIndices$CMAR_nomean)) {paste('(with mean structure)   ', round(x$residualIndices$CMAR_nomean, digits), '(without mean structure)')}, '\n')
    cat('\n')
  }

  ## TODO: Fix printing of information criteria
  max_nchar_chi  = max(sapply(c(round(x$informationCriteria$Chi, 0), round(c(x$ECVI, x$MECVI, x$BCC), 0)), nchar))
  max_nchar_dev  = max(sapply(c(round((x$informationCriteria$deviance, 0)), nchar))

  colnames(x$informationCriteria$Chi) <- c("|  df penalty  ", "|  Parameters penalty  ", "|  Small-sample adjusted  |")
  colnames(x$informationCriteria$deviance) <- c("|  df penalty  ", "|  Parameters penalty  ", "|  Small-sample adjusted  |")

  rownames(x$informationCriteria$Chi) <- c('AIC (Akaike information criterion):', 'BIC (Bayesian information criterion):', 'CAIC (Consistent AIC):')
  rownames(x$informationCriteria$deviance) <- c('AIC (Akaike information criterion):', 'BIC (Bayesian information criterion):', 'CAIC (Consistent AIC):')
  
  cat('Information-based fit indices (computed using \u03C7²)\n')
  cat('======================================================\n')
  print(round(x$informationCriteria$Chi, digits))
  cat("\n")
  cat('BCC (Browne-Cudeck criterion):         ', round(x$BCC,      digits), '\n')
  cat('ECVI (Expected cross-validation index):', round(x$ECVI,     digits), '\n')
  cat('MECVI (Modified ECVI):                 ', round(x$MECVI,    digits), '\n')
  cat('\n')

  cat('Information-based fit indices (computed deviance)\n')
  cat('=================================================\n')
  print(round(x$informationCriteria$deviance, digits))
  cat("\n")

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

  if(x$independenceRMSEA <= .158) {
    message(paste0("Note: Independence (Null) model has RMSEA = ", 
                  round(x$independenceRMSEA, digits), 
                  ".\nIf the model shows good fit (RMSEA <= .05), TLI has a maximum value <= .90.",
                  "\nInterpret incremental fit indices (TLI, CFI, etc.) with caution."))
  }
  invisible(x)

}
