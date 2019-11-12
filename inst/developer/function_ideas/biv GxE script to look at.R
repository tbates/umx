mxOption(NULL, "Default optimizer", "NPSOL")

# -----------------------------------------------------------------------
# PREPARE DATA

# Read Data
twinData <- read.table("D:/FHI/Sosiale forhold og helse/Analyses/IBS/SFH_17_09_2018_subset_pss_discordance_strain_lowsupport_family_format.txt", header=T, sep='\t', dec=',')
str(twinData)
nobs = dim(twinData)[1]
twinData$Zage_s_1 = scale(twinData$q_age_s_1)
twinData$Zage_s_2 = scale(twinData$q_age_s_2)
twinData$lowsupport_sc_s_1 = scale(twinData$lowsupport_s_1)
twinData$lowsupport_sc_s_2 = scale(twinData$lowsupport_s_2)

# Select Ordinal Variables
nth       = 1                         # number of thresholds
varso     = c('ibs_gen_s')                   # list of ordinal variables names
nvo       = length(varso)                         # number of ordinal variables
ntvo      = nvo*2                     # number of total ordinal variables
ordVars   = paste(varso,c(rep(1,nvo),rep(2,nvo)),sep="_")


# Select Variables for Analysis
Vars 	  = c("lowsupport_sc_s","ibs_gen_s") 
nv		  = length(Vars)	 	# number of variables
nind    = 2
ntv     = nv*nind # number of total variables
selVars = paste(Vars,c(rep(1,nv),rep(2,nv)),sep="_")	
def     = c("sex_s", "Zage_s")
ndef    = length(def)
defVars = paste(def,c(rep(1,ndef),rep(2,ndef)),sep="_")

# Select Data for Analysis
mzData  <- subset(twinData, zyg3_s=='Mz', c(selVars,defVars))
dzData  <- subset(twinData, zyg3_s=='DzL', c(selVars,defVars))

mzData = mzData[complete.cases(mzData[,c(defVars, "lowsupport_sc_s_1", "lowsupport_sc_s_2")]),]
dzData = dzData[complete.cases(dzData[,c(defVars, "lowsupport_sc_s_1", "lowsupport_sc_s_2")]),]

mzDataF = mzData
dzDataF = dzData
mzDataF[,ordVars] <- mxFactor( x=mzDataF[,ordVars], levels=c(0,1) )
dzDataF[,ordVars] <- mxFactor( x=dzDataF[,ordVars], levels=c(0,1) )

# Raw data in OpenMx format
dataMZ 	  <- mxData(observed = mzDataF, type = "raw" )
dataDZ 	  <- mxData(observed = dzDataF, type = "raw" )



# ---------------------Cholesky part!------------------------------------

# Set up Cholesky ADE decomposition, with RawData and Matrices Input
# -----------------------------------------------------------------------

## Labeling 
aLabs 	<- c("aM","aC","aU")
dLabs	<- c("dM","dC","dU")
eLabs 	<- c("eM","eC","eU")
meanLabs 	<- c("meanM","meanP")

aModLabs   <- c("aMod11","aMod21","aMod22")
dModLabs   <- c("dMod11","dMod21","dMod22")
eModLabs   <- c("eMod11","eMod21","eMod22")

threshLabs  <- paste(varso,"thresh",sep="_")
betaLabs_age <- paste("beta","Age",Vars, sep="_")
betaLabs_sex <- paste("beta","Sex",Vars, sep="_")

# Set Starting Values
frMV      <- c(TRUE, FALSE)             # free status for variables
frCvD     <- diag(frMV,ntv,ntv)        # lower bounds for diagonal of covariance matrix
frCvD[lower.tri(frCvD)] <- TRUE        # lower bounds for below diagonal elements
frCvD[upper.tri(frCvD)] <- TRUE        # lower bounds for above diagonal elements
frCv      <- matrix(as.logical(frCvD),4)
svMe      <- c(0,0)                   # start value for means
svPa      <- .4                        # start value for path coefficient
svPaD     <- vech(diag(svPa,nv,nv))    # start values for diagonal of covariance matrix
svPe      <- .8                        # start value for path coefficient for e
svPeD     <- vech(diag(svPe,nv,nv))    # start values for diagonal of covariance matrix
lbPa      <- 0                    # start value for lower bounds
lbPaD     <- diag(lbPa,nv,nv)          # lower bounds for diagonal of covariance matrix
lbPaD[lower.tri(lbPaD)] <- -10         # lower bounds for below diagonal elements
lbPaD[upper.tri(lbPaD)] <- NA          # lower bounds for above diagonal elements
svTh      <- 1.5     # start value for thresholds


pathModVal = c(0,0.1,0.1)
B_AgeVal = 0.5
B_SexVal = 0.5

## Modeling

# Matrices a, c, and e to store a, c, and e Path Coefficients
pathA <- mxMatrix(name = "a", type = "Lower", nrow = nv, ncol = nv, free=T, labels = aLabs, values=svPaD, lbound=lbPaD)
pathD <- mxMatrix(name = "d", type = "Lower", nrow = nv, ncol = nv, free=T, labels = dLabs, values=svPaD, lbound=lbPaD)
pathE <- mxMatrix(name = "e", type = "Lower", nrow = nv, ncol = nv, free=T, labels = eLabs, values=svPeD, lbound=lbPaD)

modPathA = mxMatrix(name="aMod", "Lower", nrow=nv, ncol=nv, free=c(F,T,T), values=pathModVal, labels=aModLabs)
modPathD = mxMatrix(name="dMod", "Lower", nrow=nv, ncol=nv, free=c(F,T,T), values=pathModVal, labels=dModLabs)
modPathE = mxMatrix(name="eMod", "Lower", nrow=nv, ncol=nv, free=c(F,T,T), values=pathModVal, labels=eModLabs)

# Moderator
mod_tw1 = mxMatrix(name="Mod1", type="Full", nrow=1, ncol=1, free=FALSE, labels='data.lowsupport_sc_s_1')
mod_tw2 = mxMatrix(name="Mod2", type="Full", nrow=1, ncol=1, free=FALSE, labels='data.lowsupport_sc_s_2')

# Matrices generated to hold A, D, and E computed Variance Components
varA1 = mxAlgebra(name = "A1", expression = (a + Mod1%x%aMod) %*% t(a+ Mod1%x%aMod))
varD1 = mxAlgebra(name = "D1", expression = (d + Mod1%x%dMod) %*% t(d+ Mod1%x%dMod))
varE1 = mxAlgebra(name = "E1", expression = (e + Mod1%x%eMod) %*% t(e+ Mod1%x%eMod))

covA12 = mxAlgebra(name = "A12", expression = (a + Mod1%x%aMod) %*% t(a+ Mod2%x%aMod))
covD12 = mxAlgebra(name = "D12", expression = (d + Mod1%x%dMod) %*% t(d+ Mod2%x%dMod))
covE12 = mxAlgebra(name = "E12", expression = (e + Mod1%x%eMod) %*% t(e+ Mod2%x%eMod))

covA21 = mxAlgebra(name = "A21", expression = (a + Mod2%x%aMod) %*% t(a+ Mod1%x%aMod))
covD21 = mxAlgebra(name = "D21", expression = (d + Mod2%x%dMod) %*% t(d+ Mod1%x%dMod))
covE21 = mxAlgebra(name = "E21", expression = (e + Mod2%x%eMod) %*% t(e+ Mod1%x%eMod))

varA2 <- mxAlgebra(name = "A2", expression = (a + Mod2%x%aMod) %*% t(a+ Mod2%x%aMod))
varD2 <- mxAlgebra(name = "D2", expression = (d + Mod2%x%dMod) %*% t(d+ Mod2%x%dMod))
varE2 <- mxAlgebra(name = "E2", expression = (e + Mod2%x%eMod) %*% t(e+ Mod2%x%eMod))

myVarA <- mxAlgebra(name = "A0", expression = a %*% t(a))
myVarD <- mxAlgebra(name = "D0", expression = d %*% t(d))
myVarE <- mxAlgebra(name = "E0", expression = e %*% t(e))

# Algebra to compute total variances and standard deviations (diagonal only) per twin
var1     <- mxAlgebra( A1+D1+E1, name="V1" )
var2     <- mxAlgebra( A2+D2+E2, name="V2" )

myVar     <- mxAlgebra( A0+D0+E0, name="V0" )

# Constraint on variance of Binary variables
matUnv <- mxMatrix( type="Unit", nrow=nvo, ncol=1, name="Unv1" )
var_constraint <- mxConstraint(V0[2,2] == Unv1, name="Var1")


### Algebra for expected variance/covariance matrices
expCovMZ <- mxAlgebra(name = "expCovMZ", expression = rbind (cbind(A1+D1+E1, A12+D12),
                                                             cbind(A21+D21,    A2+D2+E2)))

expCovDZ <- mxAlgebra(name = "expCovDZ", expression = rbind (cbind(A1+D1+E1,     0.5%x%A12+0.25%x%D12),
                                                             cbind(0.5%x%A21+0.25%x%D21,        A2+D2+E2))) 


# Matrices for expected Means for females and males
#setting up the regression
intercept      <- mxMatrix( type="Full", nrow=1, ncol=ntv, free=frMV, values=svMe, labels=meanLabs, name="intercept" )
threshold      <-mxMatrix( type="Full", nrow=1, ncol=ntvo, free=T, values=svTh, labels=threshLabs, name="Threshold" )

# Regression effects
B_Age        <- mxMatrix( type="Full", nrow=1, ncol=nv, free=TRUE, values=.1, labels=betaLabs_age, name="bAge" )
defAge       <- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, labels=c("data.Zage_s_1","data.Zage_s_2"), name="Age")

B_Sex        <- mxMatrix( type="Full", nrow=1, ncol=nv, free=TRUE, values=.1, labels=betaLabs_sex, name="bSex" )
defSex       <- mxMatrix( type="Full", nrow=1, ncol=2, free=FALSE, labels=c("data.sex_s_1","data.sex_s_2"), name="Sex")

expMean  	<- mxAlgebra( intercept + (Age %x% bAge) + (Sex %x% bSex) , name="expMean")


inclusions   <- list (B_Age, B_Sex,  defAge, defSex, intercept, expMean, threshold)


# Objective objects for Multiple Groups
expMZ     <- mxExpectationNormal( covariance="expCovMZ", means="expMean", dimnames=selVars, thresholds="Threshold", threshnames=ordVars )
expDZ     <- mxExpectationNormal( covariance="expCovDZ", means="expMean", dimnames=selVars ,thresholds="Threshold", threshnames=ordVars )
funML     <- mxFitFunctionML()
	

# MZ and DZ models
modelMZ <- mxModel(pathA, pathD, pathE, modPathA, modPathD, modPathE, mod_tw1, mod_tw2,
                   varA1, varD1, varE1, covA12, covD12, covE12, covA21, covD21, covE21, varA2, varD2, varE2, var1, var2,
                   myVarA, myVarD, myVarE, myVar, matUnv, var_constraint,
                   B_Age, B_Sex,  defAge, defSex, intercept, expMean, threshold,
                   dataMZ, expCovMZ, expMZ, funML, name = "MZ")
modelDZ <- mxModel(pathA, pathD, pathE, modPathA, modPathD, modPathE, mod_tw1, mod_tw2,
                   varA1, varD1, varE1, covA12, covD12, covE12, covA21, covD21, covE21, varA2, varD2, varE2, var1, var2,
                   myVarA, myVarD, myVarE, myVar, matUnv, var_constraint,
                   B_Age, B_Sex,  defAge, defSex, intercept, expMean, threshold,
                   dataDZ, expCovDZ, expDZ, funML, name = "DZ")

multi     	<- mxFitFunctionMultigroup( c("MZ","DZ") )
ADEmodModel  <- mxModel( "ADEmod", modelMZ, modelDZ, funML, multi )

ADEmodFit  <-  mxRun(ADEmodModel)
summary(ADEmodFit)

MainEffectsModel = mxModel (ADEmodFit, name='MainEffects')
MainEffectsModel = omxSetParameters(MainEffectsModel, labels=c('aMod21','aMod22'), values=0, free=F)
MainEffectsModel = omxSetParameters(MainEffectsModel, labels=c('dMod21','dMod22'), values=0, free=F)
MainEffectsModel = omxSetParameters(MainEffectsModel, labels=c('eMod21','eMod22'), values=0, free=F)
MainEffectsFit = mxRun(MainEffectsModel)
mxCompare(ADEmodFit, MainEffectsFit)
summary(MainEffectsFit)


#=======================================================================#
#   PLOT MODERATION    FULL MODEL                                       #
#=======================================================================#

lowsupport_sc <- sort(unique(scale(rbind(mzData$lowsupport_sc_s_1, mzData$lowsupport_sc_s_2, dzData$lowsupport_sc_s_1, dzData$lowsupport_sc_s_2))))
lowsupport_length=length(lowsupport_sc)


# Purcell's representation (total variances)
Va=array(NA, dim=c(nv,nv,lowsupport_length))
Vd=array(NA, dim=c(nv,nv,lowsupport_length))
Ve=array(NA, dim=c(nv,nv,lowsupport_length))
Vt=array(NA, dim=c(nv,nv,lowsupport_length))
SVa=array(NA, dim=c(nv,nv,lowsupport_length))
SVd=array(NA, dim=c(nv,nv,lowsupport_length))
SVe=array(NA, dim=c(nv,nv,lowsupport_length))
corA = array(NA, dim=c(nv,nv,lowsupport_length))
corD = array(NA, dim=c(nv,nv,lowsupport_length))
corE = array(NA, dim=c(nv,nv,lowsupport_length))
corP = array(NA, dim=c(nv,nv,lowsupport_length))

for (i in 1:lowsupport_length) {
  Va[,,i] <- mxEval((a + aMod%x%lowsupport_sc[i]) %*% t(a + aMod%x%lowsupport_sc[i]), ADEmodFit$MZ)
  Vd[,,i] <- mxEval((d + dMod%x%lowsupport_sc[i]) %*% t(d + dMod%x%lowsupport_sc[i]), ADEmodFit$MZ)
  Ve[,,i] <- mxEval((e + eMod%x%lowsupport_sc[i]) %*% t(e + eMod%x%lowsupport_sc[i]), ADEmodFit$MZ)
  Vt[,,i] <- Va[,,i] + Vd[,,i] + Ve[,,i]
  SVa[,,i] <- Va[,,i]/Vt[,,i]
  SVd[,,i] <- Vd[,,i]/Vt[,,i]
  SVe[,,i] <- Ve[,,i]/Vt[,,i]
  corA[,,i] <- solve(sqrt(diag(2)*Va[,,i]))%*%Va[,,i]%*%solve(sqrt(diag(2)*Va[,,i]))
  # corD[,,i] <- solve(sqrt(diag(2)*Vd[,,i]))%*%Vd[,,i]%*%solve(sqrt(diag(2)*Vd[,,i]))
  corE[,,i] <- solve(sqrt(diag(2)*Ve[,,i]))%*%Ve[,,i]%*%solve(sqrt(diag(2)*Ve[,,i]))
  corP[,,i] <- solve(sqrt(diag(2)*Vt[,,i]))%*%Vt[,,i]%*%solve(sqrt(diag(2)*Vt[,,i]))
}

out <- as.matrix(cbind(Va[2,2,],Vd[2,2,],Ve[2,2,],Vt[2,2,],SVa[2,2,],SVd[2,2,],SVe[2,2,], corA[2,1,],corD[2,1,],corE[2,1,], corP[2,1,]))
names(out) = c('Va', 'Vd', 'Ve', 'Vt', 'SVa', 'SVd', 'SVe', 'corA', 'corD', 'corE', 'corP')
head(out)


par(mar=c(5.1,4.1,4.1,2.1))
par(mfrow=c(1,2))
matplot(lowsupport_sc, out[,1:4], type="l", lty=4:1, col=4:1, xlab="Low support", ylab="Variance Component", main="Total - Unstandardized\nvariance components")
legend(0,12, c("Va","Vd","Ve","Vt"), lty=4:1, col=4:1, cex = 1, y.intersp = .5)
abline(v = 0, lty = 2, lwd = 0.5)

matplot(lowsupport_sc, out[,5:7], type="l", lty=4:2, col=4:2, xlab="Low support", ylab="Variance Component", main="Standardized\nvariance components")
abline(v = 0, lty = 2, lwd = 0.5)

par(mfrow=c(1,1))
matplot(lowsupport_sc, out[,8:11], type='l', lty=4:1, col=4:1, xlab='Low support', ylab="Correlation", main="Correlation")
legend(4,0.4, c("rA","rD","rE","rP"), lty=4:1, col=4:1, cex = 1, y.intersp = .5)
abline(v = 0, lty = 2, lwd = 0.5)

par(mfrow=c(1,1))
stackpoly(lowsupport_sc, out[,1:3], stack = T, col = c('black', 'gray', 'gray40'), axis4 = F, xat = -1:6, ylim = c(0, 13), xlab='Low support')
abline(v = 0, lty = 2, lwd = 0.5)
legend(3,12, fill = c('black', 'gray', 'gray40'), legend = c("A","D","E"), border = 'black', bty = 'n')

