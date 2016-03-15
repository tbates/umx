# # ------------------------------------------------------------------------------
# # Program: twinHetMulAceCon.R
# #  Author: Hermine Maes
# #    Date: 10 24 2015
# #
# # Multivariate Twin Analysis with Sex Limitation Model to estimate causes of (co)variation
# # Matrix style model - Raw data - Continuous data
# #
# # MODEL:MV Quantitative & Qualitative Sex-Limitation script (ACE Correlated Factors model & ACE Cholesky model)
# # Correlation Approach to ensure that order of variables does NOT affect ability of model to account for DZOS data
# # Ref: Neale et al., Multivariate genetic analysis of sex-lim and GxE interaction, Twin Research & Human Genetics, 2006
# # Restrictions: Assumes means and variances can be equated across birth order within zygosity groups
# #
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
#
# # Load Library
# require(umx)
# require(psych)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Load Data
# allVars   <- c('fan','zyg',
#  'ht1','wt1','bmi1','bml1','bic1','caf1','ssc1','sil1','tri1',
#  'ht2','wt2','bmi2','bml2','bic2','caf2','ssc2','sil2','tri2')
# us_skinfold_data <- read.table("us_skinfold_data.rec", header = FALSE, na.strings = ".", col.names = allVars)
# describe(us_skinfold_data, skew=F)
# us_skinfold_data[,c('bic1','bic2')] <- us_skinfold_data[,c('bic1','bic2')]/3.4
# us_skinfold_data[,c('tri1','tri2')] <- us_skinfold_data[,c('tri1','tri2')]/3
# us_skinfold_data[,c('caf1','caf2')] <- us_skinfold_data[,c('caf1','caf2')]/3
# us_skinfold_data[,c('ssc1','ssc2')] <- us_skinfold_data[,c('ssc1','ssc2')]/5
# us_skinfold_data[,c('sil1','sil2')] <- us_skinfold_data[,c('sil1','sil2')]/5
# describe(us_skinfold_data, skew=F)
#
# # Select Variables for Analysis
# Vars      <- c('ssc','sil','caf','tri','bic')
# nv        <- 5       # number of variables
# ntv       <- nv*2    # number of total variables
# selVars   <- paste(Vars,c(rep(1,nv),rep(2,nv)),sep="")
# nvm1      <- nv-1    # number of variables minus one
#
# # Select Data for Analysis
# mzmData   <- subset(us_skinfold_data, zyg==1, selVars)
# dzmData   <- subset(us_skinfold_data, zyg==3, selVars)
# mzfData   <- subset(us_skinfold_data, zyg==2, selVars)
# dzfData   <- subset(us_skinfold_data, zyg==4, selVars)
# dzoData   <- subset(us_skinfold_data, zyg==5, selVars)
#
# # ====================
# # = checked and done =
# # ====================
#
# # Create Functions to Assign Labels
# laLower   <- function(la,nv) { paste(la,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="_") }
# laSdiag   <- function(la,nv) { paste(la,rev(nv+1-sequence(1:(nv-1))),rep(1:(nv-1),(nv-1):1),sep="_") }
# laFull    <- function(la,nv) { paste(la,1:nv,rep(1:nv,each=nv),sep="_") }
# laDiag    <- function(la,nv) { paste(la,1:nv,1:nv,sep="_") }
# laSymm    <- function(la,nv) { paste(la,rev(nv+1-sequence(1:nv)),rep(1:nv,nv:1),sep="_") }
#
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Cholesky (Ch) Approach
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 1 Nonscalar Sex Limitation
# # Quantitative Sex Differences & Qualitative Sex Differences for A
# # Male and female Cholesky paths, and male- OR female-specific A paths to be estimated
# # ---------------------------------------------------------------------------------------------------------------------|
#
# # Set Starting Values
# svMe      <- c(5,8,4,4,8)              # start value for means
# laMe      <- paste(selVars,"Mean",sep="_")
# svPa      <- .2                        # start value for path coefficient
# svPaD     <- vech(diag(svPa,nv,nv))    # start values for diagonal of covariance matrix
# svPe      <- .8                        # start value for path coefficient for e
# svPeD     <- vech(diag(svPe,nv,nv))    # start values for diagonal of covariance matrix
# lbPa      <- .0001                     # start value for lower bounds
# lbPaD     <- diag(lbPa,nv,nv)          # lower bounds for diagonal of covariance matrix
# lbPaD[lower.tri(lbPaD)] <- -10         # lower bounds for below diagonal elements
# lbPaD[upper.tri(lbPaD)] <- NA          # lower bounds for above diagonal elements
#
# # Matrices for Path Coefficients
# pathAm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("am",nv), lbound=lbPaD, name="am" )
# pathCm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("cm",nv), lbound=lbPaD, name="cm" )
# pathEm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPeD, labels=laLower("em",nv), lbound=lbPaD, name="em" )
# pathAf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("af",nv), lbound=lbPaD, name="af" )
# pathCf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("cf",nv), lbound=lbPaD, name="cf" )
# pathEf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPeD, labels=laLower("ef",nv), lbound=lbPaD, name="ef" )
#
# # Matrices for Sex-specific Path Coefficients - only of these four can be estimated at once with DZO twins
# pathAsm   <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=0, labels=laLower("asm",nv), lbound=lbPaD, name="asm" )
# pathAsf   <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=FALSE, values=0, labels=laLower("asf",nv), lbound=lbPaD, name="asf" )
# pathCsm   <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=FALSE, values=0, labels=laLower("csm",nv), lbound=lbPaD, name="csm" )
# pathCsf   <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=FALSE, values=0, labels=laLower("csf",nv), lbound=lbPaD, name="csf" )
#
# # Algebra for Variance Components
# covAm     <- mxAlgebra( expression=am %*% t(am), name="Am" )
# covCm     <- mxAlgebra( expression=cm %*% t(cm), name="Cm" )
# covEm     <- mxAlgebra( expression=em %*% t(em), name="Em" )
# covAf     <- mxAlgebra( expression=af %*% t(af), name="Af" )
# covCf     <- mxAlgebra( expression=cf %*% t(cf), name="Cf" )
# covEf     <- mxAlgebra( expression=ef %*% t(ef), name="Ef" )
# covAmf    <- mxAlgebra( expression=am %*% t(af), name="Amf" )
# covCmf    <- mxAlgebra( expression=cm %*% t(cf), name="Cmf" )
#
# # Algebra for Sex-specific Variance Components - to test Qualitative Sex Effects
# covAsm    <- mxAlgebra( expression=asm %*% t(asm), name="Asm" )
# covAsf    <- mxAlgebra( expression=asf %*% t(asf), name="Asf" )
# covCsm    <- mxAlgebra( expression=csm %*% t(csm), name="Csm" )
# covCsf    <- mxAlgebra( expression=csf %*% t(csf), name="Csf" )
#
# # Algebra for Total Variances and Standard Deviations (diagonal only)
# covPm     <- mxAlgebra( expression=Am+Cm+Em+Asm+Csm, name="Vm" )
# covPf     <- mxAlgebra( expression=Af+Cf+Ef+Asf+Csf, name="Vf" )
# matI      <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I" )
# invSDm    <- mxAlgebra( expression=solve(sqrt(I*Vm)), name="iSDm" )
# invSDf    <- mxAlgebra( expression=solve(sqrt(I*Vf)), name="iSDf" )
#
# # Matrices and Algebra for Genetic and Environmental Correlations
# matI      <- mxMatrix( type="Iden", nrow=nv, ncol=nv, name="I")
# matZ      <- mxMatrix( type="Zero", nrow=nvm1, ncol=1, name="Z")
# matIm1    <- mxMatrix( type="Iden", nrow=nvm1, ncol=nvm1, name="Im1")
# matZI     <- mxAlgebra( expression=cbind(Z,Im1), name="ZI")
# matIZ     <- mxAlgebra( expression=cbind(Im1,Z), name="IZ")
# corAm     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Am)) %&% Am) %*% t(IZ), name="rAm" )
# corCm     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Cm)) %&% Cm) %*% t(IZ), name="rCm" )
# corEm     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Em)) %&% Em) %*% t(IZ), name="rEm" )
# corAf     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Af)) %&% Af) %*% t(IZ), name="rAf" )
# corCf     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Cf)) %&% Cf) %*% t(IZ), name="rCf" )
# corEf     <- mxAlgebra( expression=ZI %*% (solve(sqrt(I*Ef)) %&% Ef) %*% t(IZ), name="rEf" )
#
# # Constraints on Correlations across Sex
# constRa   <- mxConstraint( expression=vech(rAf)==vech(rAm), name="constRa" )
# constRc   <- mxConstraint( expression=vech(rCf)==vech(rCm), name="constRc" )
# constRe   <- mxConstraint( expression=vech(rEf)==vech(rEm), name="constRe" )
#
# # Matrices for Expected Mean Matrices in MZ & DZ twins
# meanGm    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=frMe, values=svMe, label=paste(Vars,"Mm",sep=""), name="expMeanGm" )
# meanGf    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=frMe, values=svMe, label=paste(Vars,"Mf",sep=""), name="expMeanGf" )
# meanGo    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=frMe, values=svMe, label=paste(Vars,rep(c("Mm","Mf"),each=nv),sep=""), name="expMeanGo" )
#
# # Algebra for Expected Variance/Covariance Matrices in MZ & DZ twins
# covMZm    <- mxAlgebra( expression= rbind( cbind(Vm, Am+Cm+Asm+Csm), cbind(Am+Cm+Asm+Csm, Vm)), name="expCovMZm" )
# covDZm    <- mxAlgebra( expression= rbind( cbind(Vm, 0.5%x%(Am+Asm)+Cm+Csm), cbind(0.5%x%(Am+Asm)+Cm+Csm, Vm)), name="expCovDZm" )
# covMZf    <- mxAlgebra( expression= rbind( cbind(Vf, Af+Cf+Asf+Csf), cbind(Af+Cf+Asf+Csf, Vf)), name="expCovMZf" )
# covDZf    <- mxAlgebra( expression= rbind( cbind(Vf, 0.5%x%(Af+Asf)+Cf+Csf), cbind(0.5%x%(Af+Asf)+Cf+Csf, Vf)), name="expCovDZf" )
# covDZo    <- mxAlgebra( expression= rbind( cbind(Vm, 0.5%x%Amf+Cmf), cbind(0.5%x%t(Amf)+t(Cmf), Vf)), name="expCovDZo" )
#
# # Data Objects for Multiple Groups
# dataMZm   <- mxData( observed=mzmData, type="raw" )
# dataDZm   <- mxData( observed=dzmData, type="raw" )
# dataMZf   <- mxData( observed=mzfData, type="raw" )
# dataDZf   <- mxData( observed=dzfData, type="raw" )
# dataDZo   <- mxData( observed=dzoData, type="raw" )
#
# # Objective Objects for Multiple Groups
# expMZm    <- mxExpectationNormal( covariance="expCovMZm", means="expMeanGm", dimnames=selVars )
# expDZm    <- mxExpectationNormal( covariance="expCovDZm", means="expMeanGm", dimnames=selVars )
# expMZf    <- mxExpectationNormal( covariance="expCovMZf", means="expMeanGf", dimnames=selVars )
# expDZf    <- mxExpectationNormal( covariance="expCovDZf", means="expMeanGf", dimnames=selVars )
# expDZo    <- mxExpectationNormal( covariance="expCovDZo", means="expMeanGo", dimnames=selVars )
# funML     <- mxFitFunctionML()
#
# # Algebras for Parameter Estimates and Derived Variance Components
# colZm     <- paste(Vars,rep(c('Am','Cm','Em'),each=nv),sep="")
# colZf     <- paste(Vars,rep(c('Af','Cf','Ef'),each=nv),sep="")
# colSZm    <- paste(Vars,rep(c('Asm','Csm'),each=nv),sep="")
# colSZf    <- paste(Vars,rep(c('Asf','Csf'),each=nv),sep="")
# estVarsZm <- mxAlgebra( cbind(Am/Vm,Cm/Vm,Em/Vm), name="VarsZm", dimnames=list(NULL,colZm))
# estVarsZf <- mxAlgebra( cbind(Af/Vf,Cf/Vf,Ef/Vf), name="VarsZf", dimnames=list(NULL,colZf))
# estVarSZm <- mxAlgebra( cbind(Asm/Vm,Csm/Vm), name="VarSZm", dimnames=list(NULL,colSZm))
# estVarSZf <- mxAlgebra( cbind(Asf/Vf,Csf/Vf), name="VarSZf", dimnames=list(NULL,colSZf))
# estCorsZm <- mxAlgebra( cbind(solve(sqrt(I*(Am+Asm))) %&% (Am+Asm),solve(sqrt(I*(Cm+Csm))) %&% (Cm+Csm),solve(sqrt(I*Em)) %&% Em), name="CorsZm", dimnames=list(NULL,colZm))
# estCorsZf <- mxAlgebra( cbind(solve(sqrt(I*(Af+Asf))) %&% (Af+Asf),solve(sqrt(I*(Cf+Csf))) %&% (Cf+Csf),solve(sqrt(I*Ef)) %&% Ef), name="CorsZf", dimnames=list(NULL,colZf))
#
# # Combine Groups
# makeChModel  <- function(name) {
# parsZm    <- list( pathAm, pathCm, pathEm, pathAsm, pathCsm, covAm, covCm, covEm, covAsm, covCsm, covPm, estVarsZm, estVarSZm, estCorsZm, matI )
# parsZf    <- list( pathAf, pathCf, pathEf, pathAsf, pathCsf, covAf, covCf, covEf, covAsf, covCsf, covPf, estVarsZf, estVarSZf, estCorsZf, matI )
# parsZmf   <- list( covAmf, covCmf)
# scalar    <- list(matI, matZ, matIm1, matZI, matIZ, corAf, corCf, corEf, corAm, corCm, corEm, constRa, constRc, constRe)
# modelMZm  <- mxModel( parsZm, meanGm, covMZm, dataMZm, expMZm, funML, name="MZm" )
# modelDZm  <- mxModel( parsZm, meanGm, covDZm, dataDZm, expDZm, funML, name="DZm" )
# modelMZf  <- mxModel( parsZf, meanGf, covMZf, dataMZf, expMZf, funML, name="MZf" )
# modelDZf  <- mxModel( parsZf, meanGf, covDZf, dataDZf, expDZf, funML, name="DZf" )
# modelDZo  <- mxModel( parsZf, parsZm, parsZmf, meanGo, covDZo, dataDZo, expDZo, funML, name="DZo" )
# multi     <- mxFitFunctionMultigroup( c("MZf","DZf","MZm","DZm","DZo") )
# name      <- mxModel( name, parsZm, parsZf, parsZmf, scalar, modelMZm, modelDZm, modelMZf, modelDZf, modelDZo, multi )
# }
# HetChAceAsmModel <- makeChModel("HetChAceAsm")
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Run Qualitative Sex Differences ACE model
# HetChAceAsmFit   <- mxRun(HetChAceAsmModel)
# HetChAceAsmSum   <- summary(HetChAceAsmFit)
# HetChAceAsmSum$Mi
# # for (i in 1:4) { HetChAceAsmFit <- mxRun(HetChAceAsmFit); print(HetChAceAsmFit $output$mi) }
# round(HetChAceAsmFit$VarsZm$result,4); round(HetChAceAsmFit$VarSZm$result,4); round(HetChAceAsmFit$CorsZm$result,4)
# round(HetChAceAsmFit$VarsZf$result,4); round(HetChAceAsmFit$VarSZf$result,4); round(HetChAceAsmFit$CorsZf$result,4)
#
# HetChAceAsfModel <- mxModel(HetChAceAsmModel, name="HetChAceAsf")
# HetChAceAsfModel <- omxSetParameters( HetChAceAsfModel, labels=laLower("asm",nv), free=FALSE, values=0 )
# HetChAceAsfModel <- omxSetParameters( HetChAceAsfModel, labels=laLower("asf",nv), free=TRUE, values=0 )
# HetChAceAsfFit   <- mxRun(HetChAceAsfModel)
# HetChAceAsfSum   <- summary(HetChAceAsfFit)
# HetChAceAsfSum$Mi
# #for (i in 1:4) { HetChAceAsfFit <- mxRun(HetChAceAsfFit); print(HetChAceAsfFit $output$mi) }
# round(HetChAceAsfFit$VarsZm$result,4); round(HetChAceAsfFit$VarSZm$result,4); round(HetChAceAsfFit$CorsZm$result,4)
# round(HetChAceAsfFit$VarsZf$result,4); round(HetChAceAsfFit$VarSZf$result,4); round(HetChAceAsfFit$CorsZf$result,4)
# mxCompare(HetChAceAsfFit, HetChAceAsmFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 2 Nonscalar Sex Limitation
# # Quantitative Sex Differences & Qualitative Sex Differences for C
# # Male and female Cholesky paths, and male- OR female-specific C paths to be estimated
# #----------------------------------------------------------------------------------------------------------------------|
#
# HetChAceCsmModel <- mxModel (HetChAceAsmModel, name="HetChAceCsm")
# HetChAceCsmModel <- omxSetParameters( HetChAceCsmModel, labels=laLower("asm",nv), free=FALSE, values=0 )
# HetChAceCsmModel <- omxSetParameters( HetChAceCsmModel, labels=laLower("csm",nv), free=TRUE, values=0 )
# HetChAceCsmFit   <- mxRun(HetChAceCsmModel)
# HetChAceCsmSum   <- summary(HetChAceCsmFit)
# HetChAceCsmSum$Mi
# #for (i in 1:4) { HetChAceCsmFit <- mxRun(HetChAceCsmFit); print(HetChAceCsmFit$output$mi) }
# round(HetChAceCsmFit$VarsZm$result,4); round(HetChAceCsmFit$VarSZm$result,4); round(HetChAceCsmFit$CorsZm$result,4)
# round(HetChAceCsmFit$VarsZf$result,4); round(HetChAceCsmFit$VarSZf$result,4); round(HetChAceCsmFit$CorsZf$result,4)
# mxCompare(HetChAceAsfFit, HetChAceCsmFit)
#
# HetChAceCsfModel <- mxModel (HetChAceAsmModel, name="HetChAceCsf")
# HetChAceCsfModel <- omxSetParameters( HetChAceCsfModel, labels=laLower("asm",nv), free=FALSE, values=0 )
# HetChAceCsfModel <- omxSetParameters( HetChAceCsfModel, labels=laLower("csf",nv), free=TRUE, values=0 )
# HetChAceCsfFit   <- mxRun(HetChAceCsfModel)
# HetChAceCsfSum   <- summary(HetChAceCsfFit)
# HetChAceCsfSum$Mi
# #for (i in 1:4) { HetChAceCsfFit <- mxRun(HetChAceCsfFit); print(HetChAceCsfFit$output$mi) }
# round(HetChAceCsfFit$VarsZm$result,4); round(HetChAceCsfFit$VarSZm$result,4); round(HetChAceCsfFit$CorsZm$result,4)
# round(HetChAceCsfFit$VarsZf$result,4); round(HetChAceCsfFit$VarSZf$result,4); round(HetChAceCsfFit$CorsZf$result,4)
# mxCompare(HetChAceAsfFit, HetChAceCsfFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 3 Nonscalar Sex Limitation
# # Quantitative Sex Differences but NO Qualitative Sex Differences
# # Male and female paths, without male- or female-specific paths
# # ---------------------------------------------------------------------------------------------------------------------|
#
# HetChAceModel <- mxModel (HetChAceAsmModel, name="HetChAce")
# HetChAceModel <- omxSetParameters( HetChAceModel, labels=laLower("asm",nv), free=FALSE, values=0 )
# HetChAceFit   <- mxRun(HetChAceModel)
# HetChAceSum   <- summary(HetChAceFit)
# HetChAceSum$Mi
# #for (i in 1:4) { HetChAceFit <- mxRun(HetChAceFit); print(HetChAceFit$output$mi) }
# round(HetChAceFit$VarsZm$result,4); round(HetChAceFit$CorsZm$result,4)
# round(HetChAceFit$VarsZf$result,4); round(HetChAceFit$CorsZf$result,4)
# mxCompare(HetChAceAsfFit, HetChAceFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 4 Homogeneity
# # NO Quantitative Sex Differences AND NO Qualitative Sex Differences
# # Same paths for males and females
# # ---------------------------------------------------------------------------------------------------------------------|
#
# pathAm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("a",nv), lbound=lbPaD, name="am" )
# pathCm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("c",nv), lbound=lbPaD, name="cm" )
# pathEm    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPeD, labels=laLower("e",nv), lbound=lbPaD, name="em" )
# pathAf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("a",nv), lbound=lbPaD, name="af" )
# pathCf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPaD, labels=laLower("c",nv), lbound=lbPaD, name="cf" )
# pathEf    <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=TRUE, values=svPeD, labels=laLower("e",nv), lbound=lbPaD, name="ef" )
# pathAsm   <- mxMatrix( type="Lower", nrow=nv, ncol=nv, free=FALSE, values=0, labels=laLower("asm",nv), lbound=lbPaD, name="asm" )
#
# #HomChAceModel <- makeChModel("HomChAce")
# parsZf    <- list( pathAf, pathCf, pathEf, pathAsf, pathCsf, covAf, covCf, covEf, covAsf, covCsf, covPf, estVarsZf, estCorsZf, matI )
# parsZm    <- list( pathAm, pathCm, pathEm, pathAsm, pathCsm, covAm, covCm, covEm, covAsm, covCsm, covPm, estVarsZm, estCorsZm, matI )
# parsZfm   <- list( covAmf, covCmf)
# modelMZf  <- mxModel( parsZf, meanGf, covMZf, dataMZf, expMZf, funML, name="MZf" )
# modelDZf  <- mxModel( parsZf, meanGf, covDZf, dataDZf, expDZf, funML, name="DZf" )
# modelMZm  <- mxModel( parsZm, meanGm, covMZm, dataMZm, expMZm, funML, name="MZm" )
# modelDZm  <- mxModel( parsZm, meanGm, covDZm, dataDZm, expDZm, funML, name="DZm" )
# modelDZo  <- mxModel( parsZf, parsZm, parsZfm, meanGo, covDZo, dataDZo, expDZo, funML, name="DZo" )
# multi     <- mxFitFunctionMultigroup( c("MZf","DZf","MZm","DZm","DZo") )
# HomChAceModel <- mxModel( name="HomChAce", parsZf, parsZm, parsZfm, modelMZf, modelDZf, modelMZm, modelDZm, modelDZo, multi )
#
# HomChAceFit   <- mxRun(HomChAceModel)
# HomChAceSum   <- summary(HomChAceFit)
# HomChAceSum$Mi
# #for (i in 1:4) { HomChAceFit <- mxRun(HomChAceFit); print(HomChAceFit$output$mi) }
# round(HomChAceFit$VarsZm$result,4); round(HomChAceFit$CorsZm$result,4)
# round(HomChAceFit$VarsZf$result,4); round(HomChAceFit$CorsZf$result,4)
# mxCompare(HetChAceFit, HomChAceFit)
#
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Generate Output Table of all Nested Models
# # ---------------------------------------------------------------------------------------------------------------------|
# compare <- c( HetChAceAsmFit, HetChAceCsfFit, HetChAceCsmFit, HetChAceFit, HomChAceFit)
# mxCompare(HetChAceAsfFit, compare)
#
# HetCh.fit <- rbind(
#  mxCompare(HetChAceAsfFit, HetChAceAsmFit),
#  mxCompare(HetChAceAsfFit, HetChAceCsfFit)[2,],
#  mxCompare(HetChAceAsfFit, HetChAceCsmFit)[2,],
#  mxCompare(HetChAceAsfFit, HetChAceFit)[2,],
#  mxCompare(HetChAceFit, HomChAceFit)[2,])
# HetCh.fit
#
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Correlated Factors (Cf) Approach
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 1 Nonscalar Sex Limitation
# # Quantitative Sex Differences & Qualitative Sex Differences for A
# # Male and female paths, plus male and female Ra, Rc and Re between variables
# # Male-Female correlations in DZO group between A factors Rao FREE, 
# # Rc constrained across male/female and oppsex
# # ---------------------------------------------------------------------------------------------------------------------|
#
# # Matrices for Path Coefficients
# pathAm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("am",nv), lbound=.0001, name="am" )
# pathCm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("cm",nv), lbound=.0001, name="cm" )
# pathEm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("em",nv), lbound=.0001, name="em" )
# pathAf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("af",nv), lbound=.0001, name="af" )
# pathCf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("cf",nv), lbound=.0001, name="cf" )
# pathEf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("ef",nv), lbound=.0001, name="ef" )
#
# # Matrices for Correlation Coefficients within/across Individuals
# pathRam   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ram",nv), lbound=-1, ubound=1, name="Ram" )
# pathRcm   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rc",nv), lbound=-1, ubound=1, name="Rcm" )
# pathRem   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rem",nv), lbound=-1, ubound=1, name="Rem" )
# pathRaf   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("raf",nv), lbound=-1, ubound=1, name="Raf" )
# pathRcf   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rc",nv), lbound=-1, ubound=1, name="Rcf" )
# pathRef   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ref",nv), lbound=-1, ubound=1, name="Ref" )
#
# # Matrices for Correlation Coefficients across Opposite Sex Pairs
# frODiag   <- c(rep(c(F,rep(T,nv)),nv-1),F)
# svODiag   <- c(rep(c(1,rep(.4,nv)),nv-1),1)
# corRao    <- mxMatrix(type = "Full", nrow = nv, ncol = nv, free = TRUE, values=1, label=laFull("rao",nv), lbound=-1, ubound = 1, name = "Rao")
# corRco    <- mxMatrix(type = "Symm", nrow = nv, ncol = nv, free = frODiag, values=svODiag, label=laSymm("rc",nv), lbound = -1, ubound = 1, name = "Rco")
#
# # Algebra to Constrain Correlation Matrices to be Positive Definite
# cnstrPos  <- mxMatrix( type="Full", nrow=1, ncol=6, free=FALSE, values=.0001, name="cnstrPos" )
# minCor    <- mxAlgebra(cbind(min(eigenval(Ram)), min(eigenval(Rcm)), min(eigenval(Rem)),
#                              min(eigenval(Raf)), min(eigenval(Rcf)), min(eigenval(Ref))), name="minCor" )
# constr    <- mxConstraint(minCor > cnstrPos, name="constr" )
#
# # Algebra for Variance Components
# covAm     <- mxAlgebra(name="Am",  am %*% (Ram) %*% t(am))
# covCm     <- mxAlgebra(name="Cm",  cm %*% (Rcm) %*% t(cm))
# covEm     <- mxAlgebra(name="Em",  em %*% (Rem) %*% t(em))
# covAf     <- mxAlgebra(name="Af",  af %*% (Raf) %*% t(af))
# covCf     <- mxAlgebra(name="Cf",  cf %*% (Rcf) %*% t(cf))
# covEf     <- mxAlgebra(name="Ef",  ef %*% (Ref) %*% t(ef))
# covAmf    <- mxAlgebra(name="Amf", am %*% (Rao) %*% t(af))
# covCmf    <- mxAlgebra(name="Cmf", cm %*% (Rco) %*% t(cf))
# # covAfm    <- mxAlgebra(af %*% t(Rao) %*% t(am), name="Afm" ) # = t(Amf)
# # covCfm    <- mxAlgebra(cf %*% t(Rco) %*% t(cm), name="Cfm" ) # = t(Cmf)
#
# # Algebra for Total Variances and Standard Deviations (diagonal only)
# covPm     <- mxAlgebra(Am+Cm+Em, name = "Vm")
# covPf     <- mxAlgebra(Af+Cf+Ef, name = "Vf")
# matI      <- mxMatrix(type = "Iden", nrow = nv, ncol = nv, name = "I")
# invSDm    <- mxAlgebra(solve(sqrt(I*Vm)), name = "iSDm")
# invSDf    <- mxAlgebra(solve(sqrt(I*Vf)), name = "iSDf")
#
# # Matrices for Expected Mean Matrices in MZ & DZ twins
# svMe      <- c(5,8,4,4,8)       # start value for means
# meanGm    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=svMe, label=paste(Vars,"Mm",sep=""), name="expMeanGm" )
# meanGf    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=svMe, label=paste(Vars,"Mf",sep=""), name="expMeanGf" )
# meanGo    <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=T, values=svMe, label=paste(Vars,rep(c("Mm","Mf"),each=nv),sep=""), name="expMeanGo" )
#
# # Algebra for Expected Variance/Covariance Matrices in MZ & DZ twins
# covMZm    <- mxAlgebra( expression= rbind( cbind(Vm, Am+Cm), cbind(Am+Cm, Vm)), name="expCovMZm" )
# covDZm    <- mxAlgebra( expression= rbind( cbind(Vm, 0.5%x%Am+Cm), cbind(0.5%x%Am+Cm, Vm)), name="expCovDZm" )
# covMZf    <- mxAlgebra( expression= rbind( cbind(Vf, Af+Cf), cbind(Af+Cf, Vf)), name="expCovMZf" )
# covDZf    <- mxAlgebra( expression= rbind( cbind(Vf, 0.5%x%Af+Cf), cbind(0.5%x%Af+Cf, Vf)), name="expCovDZf" )
# covDZo    <- mxAlgebra( expression= rbind( cbind(Vm, 0.5%x%Amf+Cmf), cbind(0.5%x%t(Amf)+t(Cmf), Vf)), name="expCovDZo" )
#
# # Data Objects for Multiple Groups
# dataMZm   <- mxData( observed=mzmData, type="raw" )
# dataDZm   <- mxData( observed=dzmData, type="raw" )
# dataMZf   <- mxData( observed=mzfData, type="raw" )
# dataDZf   <- mxData( observed=dzfData, type="raw" )
# dataDZo   <- mxData( observed=dzoData, type="raw" )
#
# # Objective Objects for Multiple Groups
# expMZm    <- mxExpectationNormal( covariance="expCovMZm", means="expMeanGm", dimnames=selVars )
# expDZm    <- mxExpectationNormal( covariance="expCovDZm", means="expMeanGm", dimnames=selVars )
# expMZf    <- mxExpectationNormal( covariance="expCovMZf", means="expMeanGf", dimnames=selVars )
# expDZf    <- mxExpectationNormal( covariance="expCovDZf", means="expMeanGf", dimnames=selVars )
# expDZo    <- mxExpectationNormal( covariance="expCovDZo", means="expMeanGo", dimnames=selVars )
# funML     <- mxFitFunctionML()
#
# # Algebras for Parameter Estimates and Derived Variance Components
# colZm     <- paste(Vars,rep(c('Am','Cm','Em'),each=nv),sep="")
# colZf     <- paste(Vars,rep(c('Af','Cf','Ef'),each=nv),sep="")
# estVarsZm <- mxAlgebra( cbind(Am/Vm,Cm/Vm,Em/Vm), name="VarsZm", dimnames=list(NULL,colZm))
# estVarsZf <- mxAlgebra( cbind(Af/Vf,Cf/Vf,Ef/Vf), name="VarsZf", dimnames=list(NULL,colZf))
# estCorsZm <- mxAlgebra( cbind(Ram,Rcm,Rem), name="CorsZm", dimnames=list(NULL,colZm))
# estCorsZf <- mxAlgebra( cbind(Raf,Rcf,Ref), name="CorsZf", dimnames=list(NULL,colZf))
#
# # Combine Groups
# makeModel  <- function(name) {
# parsZm    <- list( pathAm, pathCm, pathEm, pathRam, pathRcm, pathRem, covAm, covCm, covEm, covPm, estVarsZm, estCorsZm )
# parsZf    <- list( pathAf, pathCf, pathEf, pathRaf, pathRcf, pathRef, covAf, covCf, covEf, covPf, estVarsZf, estCorsZf )
# parsZmf   <- list( corRao, corRco, covAmf, covCmf)
# modelMZm  <- mxModel( parsZm, meanGm, covMZm, dataMZm, expMZm, funML, name="MZm" )
# modelDZm  <- mxModel( parsZm, meanGm, covDZm, dataDZm, expDZm, funML, name="DZm" )
# modelMZf  <- mxModel( parsZf, meanGf, covMZf, dataMZf, expMZf, funML, name="MZf" )
# modelDZf  <- mxModel( parsZf, meanGf, covDZf, dataDZf, expDZf, funML, name="DZf" )
# modelDZo  <- mxModel( parsZf, parsZm, parsZmf, meanGo, covDZo, dataDZo, expDZo, funML, name="DZo" )
# multi     <- mxFitFunctionMultigroup( c("MZf","DZf","MZm","DZm","DZo") )
# name      <- mxModel( name, parsZm, parsZf, parsZmf, modelMZm, modelDZm, modelMZf, modelDZf, modelDZo, multi, cnstrPos, minCor, constr )
# }
# HetCfAceRgModel <- makeModel("HetCfAceRg")
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Run Qualitative Sex Differences ACE model
# HetCfAceRgFit   <- mxRun(HetCfAceRgModel)
# HetCfAceRgSum   <- summary(HetCfAceRgFit)
# HetCfAceRgSum$Mi
# #for (i in 1:4) { HetCfAceRgFit <- mxRun(HetCfAceRgFit); print(HetCfAceRgFit$output$mi) }
# round(HetCfAceRgFit$VarsZm$result,4); round(HetCfAceRgFit$CorsZm$result,4)
# round(HetCfAceRgFit$VarsZf$result,4); round(HetCfAceRgFit$CorsZf$result,4)
# mxCompare(HetCfAceRgFit)
#
# source("GenEpiHelperFunctions.R")
# parameterSpecifications(HetCfAceRgFit)
#
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 2 Nonscalar Sex Limitation
# # Quantitative Sex Differences & Qualitative Sex Differences for C
# # Male and female paths, plus male and female Ra, Rc and Re between variables
# # Male-Female correlations in DZO group between
# # C factors Rco FREE, Ra constrained across male/female and oppsex
# # ---------------------------------------------------------------------------------------------------------------------|
#
# # Matrices for Correlation Coefficients within/across Individuals
# pathRam   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ra" , nv), lbound = -1, ubound = 1, name = "Ram" )
# pathRcm   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rcm", nv), lbound = -1, ubound = 1, name = "Rcm" )
# pathRem   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rem", nv), lbound = -1, ubound = 1, name = "Rem" )
# pathRaf   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ra" , nv), lbound = -1, ubound = 1, name = "Raf" )
# pathRcf   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rcf", nv), lbound = -1, ubound = 1, name = "Rcf" )
# pathRef   <- mxMatrix(type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ref", nv), lbound = -1, ubound = 1, name = "Ref" )
# corRao    <- mxMatrix(type="Symm", nrow=nv, ncol=nv, free=frODiag, values=svODiag, label=laSymm("ra",nv), lbound=-1, ubound=1, name="Rao" )
# corRco    <- mxMatrix(type="Full", nrow=nv, ncol=nv, free=TRUE, values=1, label=laFull("rco",nv), lbound=-1, ubound=1, name="Rco" )
#
# HetCfAceRcModel <- makeModel("HetCfAceRc")
# HetCfAceRcFit   <- mxRun(HetCfAceRcModel)
# HetCfAceRcSum   <- summary(HetCfAceRcFit)
# HetCfAceRcSum$Mi
# #for (i in 1:4) { HetCfAceRcFit <- mxRun(HetCfAceRcFit); print(HetCfAceRcFit$output$mi) }
# round(HetCfAceRcFit$VarsZm$result,4); round(HetCfAceRcFit$CorsZm$result,4)
# round(HetCfAceRcFit$VarsZf$result,4); round(HetCfAceRcFit$CorsZf$result,4)
# mxCompare(HetCfAceRcFit, HetCfAceRcFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 3 Scalar Sex Limitation
# # Quantitative Sex Differences but NO Qualitative Sex Differences
# # Male and female paths, but one set of Ra, Rc and Re between variables (same for males and females)
# # ---------------------------------------------------------------------------------------------------------------------|
#
# pathRam   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ra",nv), lbound=-1, ubound=1, name="Ram" )
# pathRcm   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rc",nv), lbound=-1, ubound=1, name="Rcm" )
# pathRem   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("re",nv), lbound=-1, ubound=1, name="Rem" )
# pathRaf   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("ra",nv), lbound=-1, ubound=1, name="Raf" )
# pathRcf   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("rc",nv), lbound=-1, ubound=1, name="Rcf" )
# pathRef   <- mxMatrix( type="Stand", nrow=nv, ncol=nv, free=TRUE, values=.4, label=laSdiag("re",nv), lbound=-1, ubound=1, name="Ref" )
# corRao    <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=frODiag, values=svODiag, label=laSymm("ra",nv), lbound=-1, ubound=1, name="Rao" )
# corRco    <- mxMatrix( type="Symm", nrow=nv, ncol=nv, free=frODiag, values=svODiag, label=laSymm("rc",nv), lbound=-1, ubound=1, name="Rco" )
#
# HetCfAceModel <- makeModel("HetCfAce")
# HetCfAceFit   <- mxRun(HetCfAceModel)
# HetCfAceSum   <- summary(HetCfAceFit)
# HetCfAceSum$Mi
# #for (i in 1:4) { HetCfAceFit <- mxRun(HetCfAceFit); print(HetCfAceFit$output$mi) }
# round(HetCfAceFit$VarsZm$result,4); round(HetCfAceFit$CorsZm$result,4)
# round(HetCfAceFit$VarsZf$result,4); round(HetCfAceFit$CorsZf$result,4)
# mxCompare(HetCfAceRgFit, HetCfAceFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # 4 Homogeneity
# # NO Quantitative Sex Differences AND NO Qualitative Sex Differences
# # Same paths for males and females
# # ---------------------------------------------------------------------------------------------------------------------|
#
# pathAm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("a",nv), name="am" )
# pathCm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("c",nv), name="cm" )
# pathEm    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("e",nv), name="em" )
# pathAf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("a",nv), name="af" )
# pathCf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("c",nv), name="cf" )
# pathEf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.5, label=laDiag("e",nv), name="ef" )
#
# HomCfAceModel <- makeModel("HomCfAce")
# HomCfAceFit   <- mxRun(HomCfAceModel)
# HomCfAceSum   <- summary(HomCfAceFit)
# HomCfAceSum$Mi
# #for (i in 1:4) { HomCfAceFit <- mxRun(HomCfAceFit); print(HomCfAceFit$output$mi) }
# round(HomCfAceFit$VarsZm$result,4); round(HomCfAceFit$CorsZm$result,4)
# round(HomCfAceFit$VarsZf$result,4); round(HomCfAceFit$CorsZf$result,4)
# mxCompare(HetCfAceFit, HomCfAceFit)
#
# # -------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|---------|
# # Generate Output Table of all Nested Models
# # ---------------------------------------------------------------------------------------------------------------------
# compare <- c( HetCfAceRcFit, HetCfAceFit, HomCfAceFit)
# mxCompare(HetCfAceRgFit, compare)
#
# HetCf.fit <- rbind(
#  mxCompare(HetCfAceRgFit, HetCfAceRcFit),
#  mxCompare(HetCfAceRcFit, HetCfAceFit)[2,],
#  mxCompare(HetCfAceFit, HomCfAceFit)[2,])
# HetCf.fit