#' Get additional fit-indices for a model with umxFitIndices
#'
#' A list of fit indices. Originated in this thread: https://openmx.ssri.psu.edu/thread/765
#' note: This is not a full-fat fit reporter. It is not robust across multi-group designs,
#' definition variables. It is primarily designed to add less-often reported fit indices for 
#' RAM models where reviewer 2 wants something other than CFA/TLI/RMSEA :-).
#' 
#' Fit information reported includes: N, deviance, N.parms, Chi, df, p.Chi, Chi.df, AICchi, AICdev, BCCchi, BCCdev, BICchi, BICdev, 
#' CAICchi, CAICdev, RMSEA, SRMR, RMR, SMAR, MAR, SMAR.nodiag, MAR.nodiag, GFI, AGFI, PGFI, 
#' NFI, RFI, IFI, NNFI.TLI, CFI, PRATIO, PNFI, PCFI, NCP, ECVIchi, ECVIdev, MECVIchi, MECVIdev, MFI, GH 
#'
#' Want more? File a report at github
#' 
#' @param model The \code{\link{mxModel}} for which you want fit indices.
#' @param refModels Independence and saturated models. default mxRefModels(model, run = TRUE)
#' @return Table of fit statistics
#' @export
#' @family Reporting functions
#' @references - 
#' @examples
#' require(umx)
#' data(demoOneFactor)
#' latents  = c("G")
#' manifests = names(demoOneFactor)
#' m1 <- umxRAM("One Factor",
#' 	data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
#' 	umxPath(latents, to = manifests),
#' 	umxPath(var = manifests),
#' 	umxPath(var = latents, fixedAt = 1)
#' )
#' umxFitIndices(m1)
#' # And with raw data
#' m1 <- umxRAM("m1", data = demoOneFactor,
#' 	umxPath(latents, to = manifests),
#' 	umxPath(v.m. = manifests),
#' 	umxPath(v1m0 = latents)
#' )
#' umxFitIndices(m1)
# Round to 3 places, and report as a markdown table
#' umxAPA(umxFitIndices(m1), digits = 3)
umxFitIndices <- function(model, refModels = mxRefModels(model, run = TRUE)) {
	if(!umx_is_RAM(model)){
		message("Not fully tested against non-RAM models...")
	}
	# refModels = mxRefModels(model, run = TRUE)
	modelSummary = summary(model, refModels = refModels)
	indepSummary = summary(refModels$Independence) #, refModels = refModels$Independence
	N            = modelSummary$numObs
	N.parms      = modelSummary$estimatedParameters
	N.manifest   = length(model@manifestVars) # assumes RAM
	deviance     = modelSummary$Minus2LogLikelihood
	Chi          = modelSummary$Chi
	df           = modelSummary$degreesOfFreedom
	p.Chi        = 1 - pchisq(Chi, df)
	Chi.df       = Chi/df
	indep.chi    = indepSummary$Chi
	indep.df     = indepSummary$degreesOfFreedom
	q            = (N.manifest * (N.manifest + 1)) / 2
	N.latent     = length(model@latentVars)
	observed.cov = model$data$observed
	
	if(model$data$type == "raw"){
		observed.cov = cov(observed.cov)
	}
	observed.cor = cov2cor(observed.cov)

	estimate.cov =	mxGetExpected(model, "covariance")
	estimate.cor =  cov2cor(estimate.cov)
	Id.manifest  =  diag(N.manifest)
	residual.cov =  observed.cov - estimate.cov
	residual.cor =  observed.cor - estimate.cor
	F0       =  max((Chi-df)/(N-1),0)
	NFI      =  (indep.chi - Chi)/indep.chi
	NNFI.TLI =  (indep.chi - indep.df/df * Chi)/(indep.chi - indep.df)
	PNFI     =  (df/indep.df) * NFI
	RFI      =  1 - (Chi/df) / (indep.chi/indep.df)
	IFI      =  (indep.chi - Chi)/(indep.chi - df)
	CFI      =  min(1.0-(Chi - df)/(indep.chi - indep.df),1)
	PRATIO   =  df/indep.df
	PCFI     =  PRATIO*CFI
	NCP      =  max((Chi - df), 0)
	RMSEA    =  sqrt(F0/df) # need confidence intervals
	MFI      =  exp(-0.5 * (Chi - df)/N)
	GH       =  N.manifest / (N.manifest + 2 * ((Chi - df)/(N - 1)))
	GFI      =  1 - (
		 sum(diag(((solve(estimate.cor) %*% observed.cor)-Id.manifest) %*% ((solve(estimate.cor) %*% observed.cor) - Id.manifest))) /
	    sum(diag((solve(estimate.cor) %*% observed.cor) %*% (solve(estimate.cor) %*% observed.cor)))
	)
	AGFI        =  1 - (q/df) * (1 - GFI)
	PGFI        =  GFI * df/q
	AICchi      =  Chi + 2 * N.parms
	AICdev      =  deviance + 2 * N.parms
	BCCchi      =  Chi + 2 * N.parms/(N - N.manifest - 2)
	BCCdev      =  deviance + 2 * N.parms/(N - N.manifest - 2)
	BICchi      =  Chi + N.parms * log(N)
	BICdev      =  deviance + N.parms * log(N)
	CAICchi     =  Chi + N.parms * (log(N) + 1)
	CAICdev     =  deviance + N.parms * (log(N) + 1)
	ECVIchi     =  1/N * AICchi
	ECVIdev     =  1/N * AICdev
	MECVIchi    =  1/BCCchi
	MECVIdev    =  1/BCCdev
	RMR         =  sqrt(mean((residual.cov^2)[lower.tri(residual.cov,diag=TRUE)]))
	SRMR        =  sqrt(mean((residual.cor^2)[lower.tri(residual.cor,diag=TRUE)]))
	MAR         =  mean(abs(residual.cov[lower.tri(residual.cor,diag=TRUE)]))
	SMAR        =  mean(abs(residual.cor[lower.tri(residual.cor,diag=TRUE)]))
	MAR.nodiag  =  mean(abs(residual.cov[lower.tri(residual.cov,diag=FALSE)]))
	SMAR.nodiag =  mean(abs(residual.cor[lower.tri(residual.cor,diag=FALSE)]))
	indices     =  rbind(N, deviance, N.parms, Chi, df, p.Chi, Chi.df,
		AICchi, AICdev,
		BCCchi, BCCdev,
		BICchi, BICdev,
		CAICchi, CAICdev,
		RMSEA, SRMR, RMR,
		SMAR, MAR,
		SMAR.nodiag, MAR.nodiag,
		GFI, AGFI, PGFI,
		NFI, RFI, IFI,
		NNFI.TLI, CFI,
		PRATIO, PNFI, PCFI, NCP,
		ECVIchi, ECVIdev, MECVIchi, MECVIdev, MFI, GH
	)
	return(indices)
}