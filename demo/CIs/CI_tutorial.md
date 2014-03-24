# CIs

## SEs in the summary

## mxCIs
### Adding mxCIs
#### Using Labels

```S    
library(OpenMx)
# generate data
covariance <- matrix(c(1.0, 0.5, 0.5, 1.0), nrow=2, dimnames=list(c("a", "b"), c("a", "b")))
# Model the matrix "covariance"
m1 <- mxModel("CI Example",
	mxMatrix(name="expectedCov", "Symm", 2, 2, free=T, values = c(1, .5, 1), labels = c("var1", "cov12", "var2")),
	mxMLObjective("expectedCov", dimnames=c("a", "b"), thresholds = NA),
	# TODO: Turn these two lines on to avoid errors in OpenMx 2
	# mxExpectationNormal("expectedCov", dimnames=c("a", "b")),
	# mxFitFunctionML(),
	mxData(covariance, "cov", numObs=10000)
)
# Run the model
m1 <- mxRun(m1, intervals=TRUE)
summary(m1)$parameters[,c(1,5,6)]
# add a 95 percent CI request... dies... :-(
m1 <- mxModel(m1, mxCI("var1"))
m1 <- mxRun(m1, intervals = T)
summary(m1)$CI
```
#### Using Matrix names
#### Using Matrix Cell addresses

```S
require(OpenMx)
data(demoOneFactor)
manifests <- names(demoOneFactor)
latents <- c("G")
m1 <- mxModel("OneFactor",type="RAM",
      manifestVars = manifests,
      latentVars = latents,
      mxPath(from=latents, to=manifests),
      mxPath(from=manifests, arrows=2),
      mxPath(from=latents, arrows=2, free=FALSE, values=1.0),
      mxData(cov(demoOneFactor), type="cov", numObs=500)
)
 
m1 <-umxLabel(m1)
m1 = mxRun(m1, intervals=TRUE)
summary(m1)
m1 <- mxModel(m1, mxCI(c('OneFactor.A')))
m1 = mxRun(m1, intervals=TRUE)
# view confidence intervals
summary(m1)$CI

```

### Bootstrapping
