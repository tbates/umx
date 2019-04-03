require(umx); require(MASS)

curve = 'gompertz'

airStarts = c(108, 40, .4)  # .1,.01,.001
Amat = umxMatrix("amat", "Full", nrow=1, ncol=1, values=airStarts[1], free=T, lbound=-500, ubound=500, labels="aGrowth")
Imat = umxMatrix("imat", "Full", nrow=1, ncol=1, values=airStarts[2], free=T, lbound=-500, ubound=500, labels="iGrowth")
Rmat = umxMatrix("rmat", "Full", nrow=1, ncol=1, values=airStarts[3], free=T, lbound=-500, ubound=500, labels="rGrowth")

# Number of occasions
nocc = 4
noccx2 = 8
#mzData = mvrnorm(75, mu=rep(0,noccx2), Sigma=diag(noccx2))
#dzData = mvrnorm(75, mu=rep(0,noccx2), Sigma=diag(noccx2))
selVars = umx_paste_names(paste0("Time", 1:nocc), sep = "_T", suffixes= 1:2)
txt = "1.000 	
         0.389 1.000	         
         0.289 0.460 1.000	         
         0.298 0.484 0.580 1.000	         
         	         
         0.763 0.418 0.339 0.327  1.000	         
         0.498 0.685 0.514 0.406  0.506 1.000	         
         0.355 0.541 0.809 0.594  0.438 0.524 1.000	         
         0.327 0.406 0.594 0.806  0.354 0.423 0.629 1.000"  
cormz = data.matrix(read.table(text=txt, fill=TRUE, col.names=selVars)  )
cormz = umx_lower2full(cormz, diag = T, byrow = T, dimnames = NULL)
txt = "1.000
        0.630 1.000 
        0.450 0.529 1.000
        0.213 0.505 0.724 1.000
        
        0.736 0.506 0.367 0.250  1.000
        0.657 0.636 0.382 0.308  0.641 1.000
        0.530 0.525 0.646 0.588  0.598 0.641 1.000    
        0.158 0.331 0.601 0.721  0.321 0.422 0.715 1.000"  
cordz = data.matrix( read.table(text=txt, fill=TRUE, col.names=selVars)  )
cordz = umx_lower2full(cordz, diag = T, byrow = T, dimnames = NULL)

sdmz  = diag(c(4.247, 3.351, 4.014, 4.972, 4.011, 3.536, 4.294, 4.899))
sigmz =  sdmz %*% cormz %*% sdmz
meanmz = c(40.562, 57.450, 71.528, 82.560, 39.536, 56.706, 70.527, 81.135)
mzData = as.data.frame(mvrnorm(75, mu=meanmz, Sigma=sigmz, empirical=T))

sddz  = diag(c(5.350,3.841,4.579,5.326,4.991,4.295,4.742,5.407))
sigdz =  sddz %*% cordz %*% sddz
meandz = c(40.134,54.248,68.208,79.451,39.146,55.658,67.386,79.830)
dzData = as.data.frame(mvrnorm(75, mu=meandz, Sigma=sigdz, empirical=T))

names(mzData) = selVars
names(dzData) = selVars

### Data prep complete.  Now specify model
Tmat = umxMatrix("tmat", "Full", nrow = nocc, ncol = 1, values = 1:nocc, free = FALSE)
Umat = umxMatrix("umat", "Full", nrow = nocc, ncol = 1, values = 1,      free = FALSE)

if(curve == 'logistic'){
	# Logistic
	Jalg = mxAlgebra(imat %x% umat+(amat-imat) %x% (exp(-(tmat-umat) %x% rmat))                                            , name="jalg") # Logistic function itself 
	Kalg = mxAlgebra((imat %x% umat-(exp(-(tmat-umat) %x% rmat))*(((amat*imat) %x% umat)/jalg)) / jalg                     , name="kalg") # dj/da
	Lalg = mxAlgebra(( (amat %x% umat) - (umat-(exp(-(tmat-umat) %x% rmat))) * (((amat*imat) %x% umat)/jalg) ) / jalg      , name="lalg") # dj/di
	Malg = mxAlgebra( ( ((amat-imat) %x% (tmat-umat))*(exp(-(tmat-umat) %x% rmat))*(((amat*imat) %x% umat)/jalg) ) / jalg  , name="malg") # dj/dr
} else if(curve=='exponential'){
	# Exponential
	Jalg = mxAlgebra( amat %x% umat-(amat-imat) %x% exp(-(tmat-umat) %x% rmat)    , name="jalg")      # Exponential curve itself 
	Kalg = mxAlgebra( umat-(exp(-(tmat-umat) %x% rmat))                           , name="kalg")      # dj/da
	Lalg = mxAlgebra( exp(-(tmat-umat) %x% rmat)                                  , name="lalg")      # dj/di
	Malg = mxAlgebra( ((amat-imat) %x% (tmat-umat)) * (exp(-(tmat-umat) %x% rmat)), name="malg")      # dj/dr
} else if(curve=='gompertz'){
	#Gompertz
	Jalg = mxAlgebra( amat %x% umat %x% (exp (log(imat/amat) * exp(-(tmat-umat) %x% rmat)) )                                                  , name="jalg")
	Kalg = mxAlgebra((umat- exp(-(tmat-umat) %x% rmat))* ( exp ((log(imat/amat)) %x% exp(-(tmat-umat) %x% rmat)) )                            , name="kalg")
	Lalg = mxAlgebra((amat/imat) %x% ( exp((-(tmat-umat) %x% rmat)+(log(imat/amat) %x% exp(-(tmat-umat) %x% rmat))) )                         , name="lalg")
	Malg = mxAlgebra(((-amat*log(imat/amat)) %x% (tmat-umat))*( exp((-(tmat-umat) %x% rmat)+(log(imat/amat) %x% exp(-(tmat-umat) %x% rmat))) ), name="malg")
} else {
	stop(paste('Unknown curve type: ',curve))
}
# Construct factor loading matrix
Falg = mxAlgebra(cbind(kalg,lalg,malg), name="falg")

# Matrix for mean parameters
meanMat = mxMatrix("Full",nrow=3,ncol=1,free=c(T,T,T), values=c(airStarts[1:3]), labels=c("aGrowth","iGrowth","rGrowth"), lbound=-500, ubound=500, name="factorMeans")
                                                                                                
# Jalg, Kalg, Lalg, Malg, Falg, Meanmat, Meanalg, Covalg                                        

# Twin part

# Matrices for path coefficients
nFac = 3 # Number of latent factors in the growth curve model
lb = matrix(NA, nFac, nFac); diag(lb) = 0
lbres = matrix(NA, nocc, nocc); diag(lbres) = 0
aMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=TRUE, values=0,  labels=labFun("a",nFac,nFac,    lower=T), lbound=lb,    name="a") # Additive genetic latent growth factor
cMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=TRUE, values=0,  labels=labFun("c",nFac,nFac,    lower=T), lbound=lb,    name="c") # Common environmental latent growth factor
eMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=TRUE, values=.5, labels=labFun("e",nFac,nFac,    lower=T), lbound=lb,    name="e") # Unique environmental latent growth factor
aResMat = mxMatrix("Diag" , nrow=nocc, ncol=nocc, free=TRUE, values=.1, labels=labFun("aRes",nocc,nocc, lower=F), lbound=lbres, name="aRes") # Additive genetic residual)
cResMat = mxMatrix("Diag" , nrow=nocc, ncol=nocc, free=TRUE, values=.1, labels=labFun("cRes",nocc,nocc, lower=F), lbound=lbres, name="cRes") # Common environment residual)
eResMat = mxMatrix("Diag" , nrow=nocc, ncol=nocc, free=TRUE, values=1,  labels=labFun("eRes",nocc,nocc, lower=F), lbound=lbres, name="eRes") # Unique environment residual)

# Multiply by each path coefficient by its inverse to get variance component for factors
  aFacAlg    = mxAlgebra(a %*% t(a), name="A") # additive genetic     factor variance
  cFacAlg    = mxAlgebra(c %*% t(c), name="C") # common environmental factor variance
  eFacAlg    = mxAlgebra(e %*% t(e), name="E") # unique environmental factor variance

# Multiply by each path coefficient by its inverse to get variance component for Residuals
  aLatentAlg = mxAlgebra(aRes %*% t(aRes), name="ARes") # additive genetic     Residual variance
  cLatentAlg = mxAlgebra(cRes %*% t(cRes), name="CRes") # common environmental Residual variance
  eLatentAlg = mxAlgebra(eRes %*% t(eRes), name="ERes") # unique environmental Residual variance
# MZ & DZ ACE Factor algebra
  mzLatentAlg = mxAlgebra(rbind (cbind(A+C+E  , A+C),
                                  cbind(A+C    , A+C+E)), name="mzLatentCov")
  dzLatentAlg = mxAlgebra(rbind (cbind(A+C+E    , .5%x%A+C),
                                  cbind(.5%x%A+C , A+C+E)), name="dzLatentCov")

# MZ & DZ ACE Residual algebra
  mzResAlg = mxAlgebra(rbind(    cbind(ARes+CRes+ERes  , ARes+CRes),
                                  cbind(ARes+CRes , ARes+CRes+ERes)), dimnames = list(selVars, selVars), name="mzResCov")

  dzResAlg = mxAlgebra(rbind(    cbind(ARes+CRes+ERes  , .5%x%ARes+CRes),
                                  cbind(.5%x%ARes+CRes  , ARes+CRes+ERes)), dimnames = list(selVars, selVars), name="dzResCov")

# MZ & DZ ACE Covariance algebra
  idenMat = mxMatrix("Iden", nrow=2, ncol=2, name="iden2")
  unitMat = mxMatrix("Unit", nrow=1, ncol=2, name="U2")

  mzCovAlg = mxAlgebra(((iden2 %x% falg) %&% mzLatentCov) + mzResCov , dimnames = list(selVars, selVars), name="mzExpCov")
  dzCovAlg = mxAlgebra(((iden2 %x% falg) %&% dzLatentCov) + dzResCov , dimnames = list(selVars, selVars), name="dzExpCov")

# Algebra for the expected means
  meanAlg = mxAlgebra((U2%x%t(falg %*% factorMeans)), name="expMean")


# Make the mz group: define variance as square of path coefficients, define algebra of twin covariance, 
# import mz data, and set objective to model the covariance and means observed in these data

mzGroup = mxModel( "mz", aMatrix, cMatrix, eMatrix, aResMat, cResMat, eResMat, meanAlg, 
                    aLatentAlg, cLatentAlg, eLatentAlg, mzLatentAlg, mzCovAlg, mzResAlg, 
					idenMat, unitMat, aFacAlg, cFacAlg, eFacAlg, meanMat, meanAlg, Falg, 
					aResMat, cResMat, eResMat, aFacAlg, cFacAlg, eFacAlg, Amat, Imat, Rmat, Tmat, Umat,
					Jalg, Kalg, Lalg, Malg, 
					mxData(mzData, type="raw"), 
					mxExpectationNormal("mzExpCov", "expMean", dimnames=selVars), 
					mxFitFunctionML()
				  )
	
#mzGroupRun = mxRun(mzGroup)				

# make the dz group: much simpler - just has algebras for DZ expected covariances, and dz Data, but the other stuff is the same so copy from mzGroup
dzGroup = mxModel(mzGroup, name="dz",
					dzLatentAlg, dzCovAlg, dzResAlg, 
					mxData(dzData, type="raw"), 
					mxExpectationNormal("dzExpCov", "expMean", dimnames=selVars),
					mxFitFunctionML()
					)

# Combine the mz and dz groups in a supermodel which can have as its objective maximizing the likelihood of both groups simultaneously.
model = mxModel("ACE", mzGroup, dzGroup, mxFitFunctionMultigroup(c('mz', 'dz')))

#Run ACE LGC model
summary(fit = mxTryHard(model, scale=.05, extraTries=30))
fitsat = mxRefModels(fit, run=T)
summary(fit, refModels=fitsat)

#Run Cholesky
#require(umx)
cholFit = umxACE("Bayley", selDVs=selVars, dzData=dzData, mzData=mzData)

# Rest commented out
##XX## #Modify to make Cholesky manually...
##XX## aMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=FALSE, values=c(0), labels=labFun("a",nFac,nFac,    lower=T), lbound=lb, name="a") # Additive genetic latent growth factor
##XX## cMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=FALSE, values=c(0), labels=labFun("c",nFac,nFac,    lower=T), lbound=lb, name="c") # Common environmental latent growth factor
##XX## eMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=FALSE, values=0, labels=labFun("e",nFac,nFac,    lower=T), lbound=lb, name="e") # Unique environmental latent growth factor
##XX## aResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=.1, labels=labFun("aRes",nocc,nocc, lower=T), lbound=lbres, name="aRes") # Additive genetic residual)
##XX## cResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=.1, labels=labFun("cRes",nocc,nocc, lower=T), lbound=lbres, name="cRes") # Common environment residual)
##XX## eResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=.1,  labels=labFun("eRes",nocc,nocc, lower=T), lbound=lbres, name="eRes") # Unique environment residual)
##XX## mzGroup = mxModel( "mz", aMatrix, cMatrix, eMatrix, aResMat, cResMat, eResMat, meanAlg, 
##XX##                     aLatentAlg, cLatentAlg, eLatentAlg, mzLatentAlg, mzCovAlg, mzResAlg, 
##XX## 					idenMat, unitMat, aFacAlg, cFacAlg, eFacAlg, meanMat, meanAlg, Falg, 
##XX## 					aResMat, cResMat, eResMat, aFacAlg, cFacAlg, eFacAlg, Amat, Imat, Rmat, Tmat, Umat,
##XX## 					Jalg, Kalg, Lalg, Malg, 
##XX## 					mxData(mzData, type="raw"), 
##XX## 					mxExpectationNormal("mzExpCov", "expMean", dimnames=selVars), 
##XX## 					mxFitFunctionML()
##XX## 				  )
##XX## 	
##XX## Amat = mxMatrix("Full", nrow=1,    ncol=1, values=0,    free=F, lbound=-500, ubound=500, labels="aGrowth", name="amat")
##XX## Imat = mxMatrix("Full", nrow=1,    ncol=1, values=0,    free=F, lbound=-500, ubound=500, labels="iGrowth", name="imat")
##XX## Rmat = mxMatrix("Full", nrow=1,    ncol=1, values=0,    free=F, lbound=-500, ubound=500, labels="rGrowth", name="rmat")
##XX## 
##XX## aMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=F, values=0,  labels=labFun("a",nFac,nFac,    lower=T), lbound=lb, name="a") # Additive genetic latent growth factor
##XX## cMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=F, values=0,  labels=labFun("c",nFac,nFac,    lower=T), lbound=lb, name="c") # Common environmental latent growth factor
##XX## eMatrix = mxMatrix("Lower", nrow=nFac, ncol=nFac, free=F, values=0,  labels=labFun("e",nFac,nFac,    lower=T), lbound=lb, name="e") # Unique environmental latent growth factor
##XX## aResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=.1*diag(nocc), labels=labFun("aRes",nocc,nocc, lower=T), lbound=lbres, name="aRes") # Additive genetic residual)
##XX## cResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=.1*diag(nocc), labels=labFun("cRes",nocc,nocc, lower=T), lbound=lbres, name="cRes") # Common environment residual)
##XX## eResMat = mxMatrix("Lower" , nrow=nocc, ncol=nocc, free=TRUE, values=10*diag(nocc),    labels=labFun("eRes",nocc,nocc, lower=T), lbound=lbres, name="eRes") # Unique environment residual)
##XX## 
##XX## mzCovAlg = mxAlgebra(mzResCov , dimnames = list(selVars, selVars), name="mzExpCov")
##XX## dzCovAlg = mxAlgebra(dzResCov , dimnames = list(selVars, selVars), name="dzExpCov")
##XX## 
##XX## meanMat$values=0
##XX## meanMat$free = F
##XX## meanAlg = mxMatrix("Full",nrow=1,ncol=8,labels=paste0("MeanTime",1:4), free=T, name="expMean")
##XX## meanAlgDZ = mxMatrix("Full",nrow=1,ncol=8,labels=paste0("MeanTimeDZ",1:4), free=T, name="expMean")
##XX## 
##XX## mzGroup = mxModel( "mz", aMatrix, cMatrix, eMatrix, aResMat, cResMat, eResMat, meanAlg, 
##XX##                     aLatentAlg, cLatentAlg, eLatentAlg, mzLatentAlg, mzCovAlg, mzResAlg, 
##XX## 					idenMat, unitMat, aFacAlg, cFacAlg, eFacAlg, meanMat, meanAlg, Falg, 
##XX## 					aResMat, cResMat, eResMat, aFacAlg, cFacAlg, eFacAlg, Amat, Imat, Rmat, Tmat, Umat,
##XX## 					Jalg, Kalg, Lalg, Malg, 
##XX## 					mxData(mzData, type="raw"), 
##XX## 					mxExpectationNormal("mzExpCov", "expMean", dimnames=selVars), 
##XX## 					mxFitFunctionML()
##XX## 				  )
##XX## dzGroup = mxModel(mzGroup, name="dz",
##XX## 					dzLatentAlg, dzCovAlg, dzResAlg, meanAlgDZ,
##XX## 					mxData(dzData, type="raw"), 
##XX## 					mxExpectationNormal("dzExpCov", "expMean", dimnames=selVars),
##XX## 					mxFitFunctionML()
##XX## 					)
##XX## 
##XX## # Combine the mz and dz groups in a supermodel which can have as its objective maximizing the likelihood of both groups simultaneously.
##XX## cholModel = mxModel("ACE", mzGroup, dzGroup, mxFitFunctionMultigroup(c('mz', 'dz')))
##XX## summary(cholFit = mxTryHard(cholModel), refModels=fitsat)#, unsafe=T))
##XX## 