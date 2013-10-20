data(myFADataRaw, package="OpenMx")
x = mxFactor(round(myFADataRaw[,1:3]), levels = c(0:6))

# Create Datasets by Zygosity
dataBinMZm <- subset(twinDataBin, zyg == 1, selVars)
dataBinMZf <- subset(twinDataBin, zyg == 2, selVars)
dataBinDZm <- subset(twinDataBin, zyg == 3, selVars)
dataBinDZf <- subset(twinDataBin, zyg == 4, selVars)
dataBinDZo <- subset(twinDataBin, zyg == 5, selVars)

# ------------------------------------------------------------------------------
# PREPARE GENETIC MODEL
# ------------------------------------------------------------------------------
# Heterogeneity CCC ACE Model 
# ------------------------------------------------------------------------------
# Specify arguments for Threshold Matrices
svThresh       <- matrix(rep(c(lth1, rep(ith1,  nth-1)), nv), nrow = nth, ncol = nv)
lbThresh       <- matrix(rep(c(-3,   rep(0.001, nth-1)), nv), nrow = nth, ncol = nv)
laThreshMale   <- paste("thm", 1:nth, sep = "")
laThreshFemale <- paste("thf", 1:nth, sep = "")

# Matrix & Algebra for expected means vector and expected thresholds
Mean <- mxMatrix(name = "Mean", type = "Zero" , nrow = 1 , ncol = nv)
Inc  <- mxMatrix(name = "Inc" , type = "Lower", nrow = nth, ncol = nth, free = FALSE, values = 1)
Threshrem <- mxMatrix(name = "Threshrem", type = "Full" , nrow = nth, ncol = nv, free = TRUE, values = svThresh, labels = laThreshMale, lbound = lbThresh)
Threshref <- mxMatrix(name = "Threshref", type = "Full" , nrow = nth, ncol = nv, free = TRUE, values = svThresh, labels = laThreshFemale, lbound = lbThresh )

expMean       <- mxAlgebra(cbind(Mean, Mean), name = "expMean" )
ThreshIncm    <- mxAlgebra(Inc %*% Threshrem, name = "ThreshIncm" )
ThreshIncf    <- mxAlgebra(Inc %*% Threshref, name = "ThreshIncf" )
expThreshreZm <- mxAlgebra(cbind(ThreshIncm, ThreshIncm), name = "expThreshreZm" )
expThreshreZf <- mxAlgebra(cbind(ThreshIncf, ThreshIncf), name = "expThreshreZf" )
expThreshreZo <- mxAlgebra(cbind(ThreshIncm, ThreshIncf), name = "expThreshreZo" )

# Matrices a, c, and e to store a, c, and e path coefficients
pathAm    <- mxMatrix( type="Diag", nrow = nv, ncol = nv, free = TRUE, values = .7, labels = "am1", lbound = .0001, ubound = 1, name = "am" )
pathCm    <- mxMatrix( type="Diag", nrow = nv, ncol = nv, free = TRUE, values = .0, labels = "cm1", lbound = .0001, ubound = 1, name = "cm" )
pathEm    <- mxMatrix( type="Diag", nrow = nv, ncol = nv, free = TRUE, values = .7, labels = "em1", lbound = .05  , ubound = 1, name = "em" )

pathAf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.7, labels="af1", lbound=.0001, ubound=1, name="af" )
pathCf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.0, labels="cf1", lbound=.0001, ubound=1, name="cf" )
pathEf    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=.7, labels="ef1", lbound=.05, ubound=1, name="ef" )

pathRg    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=FALSE, values=.8, labels="rg1", lbound=-1, ubound=1, name="rg" )
pathRc    <- mxMatrix( type="Diag", nrow=nv, ncol=nv, free=TRUE, values=1,  labels="rc1", lbound=-1, ubound=1, name="rc" )

# Matrices A, C, and E compute variance components
covAm     <- mxAlgebra( expression=am %*% t(am), name="Am" )
covCm     <- mxAlgebra( expression=cm %*% t(cm), name="Cm" )
covEm     <- mxAlgebra( expression=em %*% t(em), name="Em" )

covAf     <- mxAlgebra( expression=af %*% t(af), name="Af" )
covCf     <- mxAlgebra( expression=cf %*% t(cf), name="Cf" )
covEf     <- mxAlgebra( expression=ef %*% t(ef), name="Ef" )
                
# Algebra to compute total variances and standard deviations (diagonal only)
nvI       <- mxMatrix(name="nvI" , type="Iden", nrow=nv)
Unv1      <- mxMatrix(name="Unv1", type="Unit", nrow=nv, ncol=1)
Vm        <- mxAlgebra((Am+Cm+Em), name="Vm" )
Vf        <- mxAlgebra((Af+Cf+Ef), name="Vf" )
iSDm      <- mxAlgebra(solve(sqrt(nvI*Vm)), name="iSDm" )
iSDf      <- mxAlgebra(solve(sqrt(nvI*Vf)), name="iSDf" )

# Constraint on variance of Binary variables
Var1m     <- mxConstraint(diag2vec(Vm) == Unv1, name = "Var1m")
Var1f     <- mxConstraint(diag2vec(Vf) == Unv1, name = "Var1f")

# Algebras generated to hold Parameter Estimates and Derived Variance Components
rowVars   <- rep('vars',nv)
colVarsZf <- c('Af','Cf','Ef','SAf','SCf','SEf')
estVarsZf <- mxAlgebra(cbind(Af, Cf, Ef, Af/Vf, Cf/Vf, Ef/Vf), name = "VarsZf", dimnames = list(rowVars,colVarsZf))
colVarsZm <- c('Am','Cm','Em','SAm','SCm','SEm')
estVarsZm <- mxAlgebra( cbind(Am,Cm,Em,Am/Vm,Cm/Vm,Em/Vm), name="VarsZm", dimnames = list(rowVars,colVarsZm))
ciVarsZf  <- mxCI("VarsZf")
ciVarsZm  <- mxCI("VarsZm")
#ciVarsZmf <- mxCI("rc")
        
# Algebra for expected variance/covariance matrix 
expCovMZm <- mxAlgebra(rbind(cbind(Vm , Am+Cm), cbind(Am+Cm , Vm)), name="expCovMZm" )
expCovMZf <- mxAlgebra(rbind(cbind(Vf , Af+Cf), cbind(Af+Cf , Vf)), name="expCovMZf" )
expCovDZm <- mxAlgebra(rbind(cbind(Vm , 0.5%x%Am+Cm), cbind(0.5%x%Am+Cm , Vm)),  name="expCovDZm" )
expCovDZf <- mxAlgebra(rbind(cbind(Vf , 0.5%x%Af+Cf), cbind(0.5%x%Af+Cf , Vf)),  name="expCovDZf" )
expCovDZo <- mxAlgebra(name="expCovDZo", expression = 
	rbind(
		cbind(Vm , 0.5 %x% (rg * (am %*% t(af) )) + rc * (cm %*% t(cf) )), 
		cbind(0.5 %x% (rg * (af %*% t(am))) + rc * (cf %*% t(cm)), Vf)
	)
)

# Data objects for Multiple Groups
dataMZf <- mxData(dataBinMZf, type = "raw")
dataDZf <- mxData(dataBinDZf, type = "raw")
dataMZm <- mxData(dataBinMZm, type = "raw")
dataDZm <- mxData(dataBinDZm, type = "raw")
dataDZo <- mxData(dataBinDZo, type = "raw")

# Objective objects for Multiple Groups
objMZf <- mxFIMLObjective( covariance="expCovMZf", means="expMean", dimnames=selVars, thresholds="expThreshreZf" )
  
# Combine Groups
pars      <- list( nvI, Unv1, Mean, expMean, Inc)
parsZf    <- list( pathAf, pathCf, pathEf, covAf, covCf, covEf, Vf, iSDf, Threshref, ThreshIncf, estVarsZf )
parsZm    <- list( pathAm, pathCm, pathEm, covAm, covCm, covEm, Vm, iSDm, Threshrem, ThreshIncm, estVarsZm )

modelMZf  <- mxModel("MZf" pars, parsZf, expMean, expThreshreZf, expCovMZf, dataMZf, objMZf, name = )
modelDZf  <- mxModel("DZf", pars, parsZf, expMean, expThreshreZf, expCovDZf, dataDZf, objDZf)
modelMZm  <- mxModel("MZm", pars, parsZm, expMean, expThreshreZm, expCovMZm, dataMZm, objMZm)
modelDZm  <- mxModel("DZm", pars, parsZm, expMean, expThreshreZm, expCovDZm, dataDZm, objDZm)
modelDZo  <- mxModel("DZo", pars, parsZf, pathRg , pathRc, parsZm, expMean, expThreshreZo, expCovDZo, dataDZo, objDZo)

QualAceModel <- mxModel("QualAce", pars, parsZf, parsZm, Var1m, Var1f, ciVarsZf, ciVarsZm, modelMZf, modelDZf, modelMZm, modelDZm, modelDZo, 
	mxAlgebra(MZf.objective + DZf.objective + MZm.objective + DZm.objective + DZo.objective, name="m2LL"),
	mxAlgebraObjective( "m2LL" )
)

# RUN GENETIC MODEL
# Run Qualitative Sex Differences ACE model
QualAceFit <- mxRun(QualAceModel, intervals=T); summary(QualAceFit)
round(QualAceFit@output$estimate, 4)
round(cbind(QualAceFit$VarsZf@result,QualAceFit$VarsZm@result), 4)
print(QualAceFit@output$minimum)
for (i in 1:4) {
	QualAceFit <- mxRun(QualAceFit)
	print(QualAceFit@output$minimum) 
}

# FIT SUBMODELS
# Run non-scalar Sex Differences ACE model
QuanAceModel <- mxModel(QualAceFit, name="QuanACE")
QuanAceModel <- omxSetParameters(QuanAceModel, labels="rc1", free=FALSE, values=1 )
QuanAceFit   <- mxRun(QuanAceModel, intervals=T)
round(QuanAceFit@output$estimate,4)
round(cbind(QuanAceFit$VarsZf@result,QuanAceFit$VarsZm@result),4)

# Run Homogeneity ACE model
HomAceModel <- mxModel( QuanAceFit, name="HomACE" )
HomAceModel <- omxSetParameters( HomAceModel, labels=c("af1","am1"), free=TRUE, values=.6, newlabels='a1' )
HomAceModel <- omxSetParameters( HomAceModel, labels=c("cf1","cm1"), free=TRUE, values=.6, newlabels='c1' )
HomAceModel <- omxSetParameters( HomAceModel, labels=c("ef1","em1"), free=TRUE, values=.6, newlabels='e1' )
HomAceFit   <- mxRun(HomAceModel, intervals=T)