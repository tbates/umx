summaryACEFit<-function(fit, selDVs, accuracy=3, dotFilename=NA, returnStd=F, extended=F, showRg=T, showStd=T) {
	# e.g.
	# stdFit = summaryACEFit(fit, selDVs=selDVs, accuracy=3, dotFilename="test.dot", returnStd=F, extended=F);
	nVar <- length(selDVs)/2;
	if(type.of(fit)=="list"){
		fitlist = fit
	} else {
		fitlist = c(fit)
	}

	if(showStd){
		cat("Standardized solution\n")
	} else {
		cat("Raw solution\n")
	}

	for(fit in fitlist) {
		# Calculate standardised variance components
		a  <- mxEval(top.a, fit); # Path coefficients
		c  <- mxEval(top.c, fit);
		e  <- mxEval(top.e, fit);
		A  <- mxEval(top.A, fit); # Variances
		C  <- mxEval(top.C, fit);
		E  <- mxEval(top.E, fit);
		Vtot = A+C+E;             # Total variance
		I  <- diag(nVar); # nVar Identity matrix
		SD <- solve(sqrt(I*Vtot)) # Inverse of diagonal matrix of standard deviations  (same as "(\sqrt(I.Vtot))~"
	
		# Standardized _path_ coefficients ready to be stacked together
		a_std <- SD %*% a; # Standardized path coefficients
		c_std <- SD %*% c;
		e_std <- SD %*% e;
		if(showStd){
			aClean = a_std
			cClean = c_std
			eClean = e_std
		} else {
			aClean = a
			cClean = c
			eClean = e
		}
		aClean[upper.tri(aClean)]=NA
		cClean[upper.tri(cClean)]=NA
		eClean[upper.tri(eClean)]=NA
		Estimates = data.frame(cbind(aClean,cClean,eClean), row.names=selDVs[1:nVar]);
		names(Estimates) = paste(rep(c("a", "c", "e"), each=nVar), rep(1:nVar), sep="");
		print(round(Estimates,accuracy)); 

		if(extended==TRUE) {
			cat("\nUnstandardized path coefficients\n")
			aClean = a
			cClean = c
			eClean = e
			aClean[upper.tri(aClean)]=NA
			cClean[upper.tri(cClean)]=NA
			eClean[upper.tri(eClean)]=NA
			unStandardizedEstimates = data.frame(cbind(aClean,cClean,eClean), row.names=selDVs[1:nVar]);
			names(unStandardizedEstimates) = paste(rep(c("a", "c", "e"), each=nVar), rep(1:nVar), sep="");
			print(round(unStandardizedEstimates,accuracy));
		}

		if(showRg) {
			# Pre & post multiply covariance matrix by inverse of standard deviations
			cat("\nGenetic correlations\n")
			NAmatrix <- matrix(NA, nVar, nVar);
			rA = tryCatch(solve(sqrt(I*A)) %*% A %*% solve(sqrt(I*A)), error=function(err) return(NAmatrix)); # genetic correlations
			rC = tryCatch(solve(sqrt(I*C)) %*% C %*% solve(sqrt(I*C)), error=function(err) return(NAmatrix)); # shared environmental correlations
			rE = tryCatch(solve(sqrt(I*E)) %*% E %*% solve(sqrt(I*E)), error=function(err) return(NAmatrix)); # Unique environmental correlations
			rAClean = rA
			rCClean = rC
			rEClean = rE
			rAClean[upper.tri(rAClean)]=NA
			rCClean[upper.tri(rCClean)]=NA
			rEClean[upper.tri(rEClean)]=NA
			genetic_correlations  = data.frame(cbind(rAClean, rCClean, rEClean), row.names=selDVs[1:nVar] );
			names(genetic_correlations)<-selDVs[1:nVar]
		 	# Make a nice-ish table
			names(genetic_correlations)= paste(rep(c("rA", "rC", "rE"), each=nVar), rep(1:nVar), sep="");
			print( round(genetic_correlations,accuracy) );
		}
		stdFit = fit
		stdFit@submodels$top@matrices$a@values = a_std
		stdFit@submodels$top@matrices$c@values = c_std
		stdFit@submodels$top@matrices$e@values = e_std
		if(!is.na(dotFilename)) {
			if(showStd){
				graphViz_Cholesky(stdFit, selDVs, dotFilename)
			}else{
				graphViz_Cholesky(fit, selDVs, dotFilename)
			}
		}
	}
	# MZc = mxEval(MZ.expCov,  fit);
	# DZc = mxEval(DZ.expCov,  fit);
	# M   = mxEval(MZ.expMean, fit);

	if(type.of(fit)=="list"){
		#nothing 
	} else {
		genEpi_TableFitStatistics(fit, extended=extended)
		if(returnStd) {
			return(stdFit)
		}
	}

}