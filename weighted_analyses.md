# Weighted analyses
Often people have datasets where different rows have different weights: more weight needs to be given to some subjects than others.

So, how do you do this in OpenMx?

Turns out it's pretty straight forward, but does require a little bit of extra work. 

The workflow is as follows:

1. Make your model as you would unweighted
2. set `vector = TRUE` in the mx

```S
require("OpenMx"); require(MASS); set.seed(200)

```

# Simulate two variables X and Y, which covary

```S
rs = .5; nSubs = 1000
selVars <- c('X','Y')
nVar = length(selVars)
xy <- mvrnorm (nSubs, c(0,0), matrix(c(1,rs,rs,1),2,2))
testData <- data.frame(xy) 
names(testData) <- selVars

```
# Step 1: Make the standard unweighted model
```S
m1 <- mxModel("vector_is_FALSE", 
	mxMatrix(name = "expCov", type = "Symm", nrow = nVar, ncol = nVar, free = T, labels = c("XX","XY","YY"), values = var(testData)),
	mxMatrix(name = "expMean", type = "Full", nrow = 1, ncol = nVar, free = T, labels = c("meanX","meanY"), values = 0, ubound=1),
	mxExpectationNormal(covariance = "expCov", means = "expMean", dimnames = selVars),
	mxFitFunctionML(vector = FALSE),
	mxData(observed = testData, type = "raw")
)
m1 <- mxRun(m1); round(omxGetParameters(m1),2)
# summary shows all parameters recovered fine.
round(cov(testData),2)
```

# Now: Switch to vector
```S
mc <- mxModel("vector_is_TRUE", 
	mxModel(m1, name = "vector",
		mxAlgebra(name = "minus2LL", -2 * sum(log(vector.fitfunction)) ),
		mxFitFunctionAlgebra("minus2LL"),
		mxFitFunctionML(vector = T)
	)
)
```S

All good! We've now done what OpenMx does for us by default: Assembled a likelihood to optimise against based on the likelihoods of each row given the current parameter estimates.

```S
mc <- mxRun(mc); round(omxGetParameters(mc),2)
```S

# Step 3: Make a windowed version

First, let's make up a weight variable which preferences data in the middle of the dataset.

```S
moderatorVariable = rnorm(nSubs)
modSD             = sd(moderatorVariable)
bw                = 2 * nSubs^(-.2) * modSD
zx                = (moderatorVariable - 0)/bw
k                 = (1 / sqrt(2 * pi)) * exp((-(zx)^2) / 2)
weightVector      = k/.399 # plot(moderatorVariable, w) # normal-curve yumminess
plot(moderatorVariable, weightVector) # normal-curve yumminess

```
Now let's apply these weights. What we're going to do is put our model, `m1` inside a container model (I'm calling it "windowed")

That container model contains a matrix ("*weights*") which is the weightVector we made above, AND an algebra that applies those weights to the logged fits from `m1`

```S
mw <- mxModel("windowed",
	mxModel(m1, name = "vector", mxFitFunctionML(vector = T) ),
	mxMatrix(name = "weights", type = "Full", nrow = 1000, ncol = 1, free = F, values = weightVector),
	mxAlgebra(name = "minus2LL", -2 * sum(weights * log(vector.fitfunction)) ),
	mxFitFunctionAlgebra("minus2LL")
)
mw <- mxRun(mw); round(omxGetParameters(mw),4)
out.mw = umx_round(summary(mw)$parameters[,c("Estimate", "Std.Error")],3, coerce = F)
names(out.mw) <- c("weighted_Est", "weighted_SE")
out.m1 = umx_round(summary(m1)$parameters[,c("name","Estimate", "Std.Error")],3, coerce = F)
out.all = cbind(out.m1, out.mw)
```
Now, as you can see below, the estimates are similar (but a bit off the population), and the SE has increased. Both these are side effects of not taking account (fully weighting) all the data.

Of course you will use weighting to appropriately weight cases to estimate the population values of your model parameters.

Cool runnings,
tim

```S
within(out.all, {
	SE_Percent = round(((weighted_SE - Std.Error)/(weighted_SE + Std.Error)),2)
	out.all
})
```
