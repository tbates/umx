# http://ibg.colorado.edu/cdrom2016/maes/UnivariateAnalysis/one/oneACEb.R
# Load Data
data(twinData)
dim(twinData)

# Create Binary Variables
binData = twinData
binData$ob1 = ifelse(twinData[,'bmi1'] > 22, 1, 0)
binData$ob2 = ifelse(twinData[,'bmi2'] > 22, 1, 0)
binData$ob1 = mxFactor(binData$ob1, levels = 0:1)
binData$ob2 = mxFactor(binData$ob2, levels = 0:1)

mzData = binData[binData$zyg %in% 1, c("ob1", "ob2")]
dzData = binData[binData$zyg %in% 3, c("ob1", "ob2")]
mzDataWLS = mxDataWLS(mzData)
dzDataWLS = mxDataWLS(dzData)

m1 <- umxRAM("One Factor", data=mxData(observed = cov(demoOneFactor), type = "cov", numObs = 500),
   umxPath(latents, to = manifests),
   umxPath(var = manifests),
   umxPath(var = latents, fixedAt = 1.0)
)

require(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor); latents <- c("G")
m1 <- mxModel("OneFactor", type="RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(from=latents, to=manifests,values=0.8),
	mxPath(from=manifests, arrows=2,values=1),
	mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
	mxData(cov(demoOneFactor), type="cov",numObs=500)
)
difftime(format("2016-11-26"), Sys.Date(), units = "auto")
m1 <- mxRun(mxModel("raw", type="RAM",
	manifestVars = manifests, latentVars = latents,
	mxPath(latents, to=manifests,values=0.8),
	mxPath(manifests, arrows=2,values=1),
	mxPath(latents, arrows=2, free=FALSE, values=1.0),
	mxPath("one", manifests, values = -.04),
	mxPath("one", latents, free = F, values = 0),
	mxData(demoOneFactor, type="raw")
))
summary(m1) # code 6 despite pretty perfect starts...
m2 = mxBootstrap(m1)
summary(m2, boot.quantile=c(.025,.975), boot.SummaryType = "quantile")

umx_set_optimizer("NPSOL")
m1 <- mxRun(m1); summary(m1)
