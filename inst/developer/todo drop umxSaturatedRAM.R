# ggplot2 function which takes care of adding a trend line, R^2, nice fonts...
lm_eqn = function(m) {
  l <- list(a = format(coef(m)[1], digits = 2),
      b = format(abs(coef(m)[2]), digits = 2),
      r2 = format(summary(m)$r.squared, digits = 3));

  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }

  as.character(as.expression(eq));                 
}


# ==============================================
# = function for saturated model with cov data =
# ==============================================

umxSaturatedRAM <- function(model, evaluate = T, verbose=F) {
	theData = model@data@observed
	manifests           = model@manifestVars
	nVar                = length(manifests)
	dataMeans          = 0
	meansLabels         = paste("mean", 1:nVar, sep="")
	loadingsLabels      = paste("F", 1:nVar, "loading", sep="")
	factorLoadingStarts = t(chol(theData))
	independenceStarts  = diag(theData)
	# m2 <- mxModel("sat",
	# 	mxMatrix(name = "factorMeans"    , type="Zero" , nrow = 1   , ncol = nVar), # Bunch of Zeros
	# 	mxMatrix(name = "factorLoadings" , type="Lower", nrow = nVar, ncol = nVar, free=T, values = factorLoadingStarts), # labels = loadingsLabels),
	# 	mxAlgebra(name = "expCov"        , expression = factorLoadings %*% t(factorLoadings)),
	# 	mxMatrix(name = "expMean", type="Full", nrow = 1, ncol = nVar, values = dataMeans, free = T, labels = meansLabels),
	# 	mxFIMLObjective(covariance="expCov", means="expMean", dimnames = manifests),
	# 	mxData(theData, type="cov", numObs= model@data@numObs)
	# )
	# m2 <- mxOption(m2, "Calculate Hessian", "No")
	# m2 <- mxOption(m2, "Standard Errors", "No")

	m3 <- mxModel("independence",
		# TODO: slightly inefficient, as this has an analytic solution
		mxMatrix(name = "A" , type="Zero", nrow = nVar, ncol = nVar),
		mxMatrix(name = "variableLoadings" , type="Diag", nrow = nVar, ncol = nVar, free=T, values = independenceStarts), # labels = loadingsLabels),
		mxMatrix(name = "F" , type="Iden", nrow = nVar, dimnames = list(manifests,manifests)),
		mxAlgebra(variableLoadings %*% t(variableLoadings), name = "expCov"),
		mxRAMObjective(A= "A", S = "variableLoadings", F = "F"),
		mxData(theData, type="cov", numObs= model@data@numObs)
	)

	m3 <- mxOption(m3, "Calculate Hessian", "No")
	m3 <- mxOption(m3, "Standard Errors", "No")
	if(evaluate){
		if(verbose){
			message("I am going to run the saturated and independence models: this may take some time")
		}
		# m2 = mxRun(m2)
		m3 = mxRun(m3)
	}
	if(verbose){
		m = deparse(substitute(model))
		message("you can use this result in the summary function like this:
		summary(", m, ", SaturatedLikelihood = ", m, "_sat$SaturatedLikelihood, IndependenceLikelihood = ", m, "_sat$IndependenceLikelihood)
		or use 
		umxReportFit(", m, ", saturatedModels = ", m, "_sat)")
	}
	# return(list(SaturatedLikelihood = m2, IndependenceLikelihood = m3))
	return(m3)
}

# ================
# = SET THIS!!!! =
# ================
# fit = # set this to the girl1 model
#
# a = umxSaturatedRAM(fit)
# summary(a)
