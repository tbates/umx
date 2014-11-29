fit.index <- function(indepfit,modelfit) {
	indep			<- summary(indepfit)
	indep.chi		<- indep$Chi
	indep.df		<- indep$degreesOfFreedom
	model			<- summary(modelfit)
	model.chi		<- model$Chi
	model.dev		<- model$SaturatedLikelihood
	model.df		<- model$degreesOfFreedom
	model.ml		<- as.numeric(modelfit$objective@result)
	N				<- model$numObs
	N.est.param		<- model$estimatedParameters
	N.manifest		<- length(modelfit@manifestVars)
	N.latent		<- length(modelfit@latentVars)
	observed.cov	<- modelfit@data@observed
	A				<- modelfit@matrices$A@values
	S				<- modelfit@matrices$S@values
	F				<- modelfit@matrices$F@values
	I				<- diag(N.manifest+N.latent)
	
	estimate.cov	<- F %*% (qr.solve(I-A)) %*% S %*% (t(qr.solve(I-A))) %*% t(F)
	observed.cor	<- cov2cor(observed.cov)
	estimate.cor	<- cov2cor(estimate.cov)
	Id.manifest		<- diag(N.manifest)
	residual.cov	<- observed.cov-estimate.cov
	residual.cor	<- observed.cor-estimate.cor
	F0				<- (model.chi-model.df)/(N-1)
	if (F0<0) {F0	<- 0}

	NFI				<- (indep.chi-model.chi)/indep.chi
	NNFI.TLI		<- (indep.chi-indep.df/model.df*model.chi)/(indep.chi-indep.df)
	PNFI			<- (model.df/indep.df)*NFI
	IFI				<- (indep.chi-model.chi)/(indep.chi-model.df)
	CFI				<- 1.0-(model.chi-model.df)/(indep.chi-indep.df)
	if (CFI>1) {CFI	 = 1}

	RMSEA			<- sqrt(F0/model.df) # need	confidence intervals
	MFI				<- exp(-0.5*(model.chi-model.df)/N)
	GH				<- N.manifest /	(N.manifest+2*((model.chi-model.df)/(N-1)))
	GFI				<- 1 - (sum(diag(((solve(estimate.cor)%*%observed.cor)-Id.manifest)%*%((solve(estimate.cor)%*%observed.cor)-Id.manifest))) /
    (sum(diag((solve(estimate.cor)%*%observed.cor)%*%(solve(estimate.cor)%*%observed.cor)))))
	AGFI			<- 1.0 - ((N.manifest*(N.manifest+1))/(2*model.df) * (1-GFI))
	PGFI			<- (2*model.df)/(N.manifest*(N.manifest+1))*GFI
	#AIC			<- model.chi-2*model.df
	AIC				<- model.chi+2*N.est.param
	#CAIC			<- model.chi-(log(N)+1.0)*model.df
	CAIC			<- model.chi+(log(N)+1.0)*N.est.param
	#BIC			<- model.chi-log(N)*model.df
	BIC				<- model.chi+log(N)*N.est.param
	RMR				<- sqrt((2*sum(residual.cov^2))/(N.manifest*(N.manifest+1)))
	SRMR			<- sqrt((2*sum(residual.cor^2))/(N.manifest*(N.manifest+1)))
	indices			<- rbind(N,model.chi,model.df,NFI,NNFI.TLI,PNFI,IFI,CFI,RMSEA,MFI,GH,GFI,AGFI,PGFI,AIC,CAIC,BIC,RMR,SRMR)
    return(indices)
}
