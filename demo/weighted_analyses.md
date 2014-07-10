## Weighted analyses
Often people have datasets where different rows have different weights: more weight needs to be given to some subjects than others.

So, how do you do this in [OpenMx](http://openmx.psyc.virginia.edu)?

Turns out it's pretty straight forward, but does require a little bit of extra work. 

The workflow is as follows:

1. Make your model as you would unweighted and…
2. set `vector = TRUE` in `mxFitFunctionML()`
3. Add a container model which weights the fit vector and optimizes on that.

### Step 0: Setup

Here, we just load libraries and simulate two correlated variables "X" and "Y"

```S
require("OpenMx"); require(MASS); set.seed(200)
r = .5; nSubs = 1000
selVars <- c('X','Y')
nVar = length(selVars)
xy <- mvrnorm(nSubs, c(0,0), matrix(c(1, r, r, 1), 2 , 2))
testData <- data.frame(xy); names(testData) <- selVars
```

### Step 1: Make the standard (unweighted) model

```S
m1 <- mxModel("regularModel", 
	mxMatrix(name = "expCov", type = "Symm", nrow = nVar, ncol = nVar, free = T, labels = c("XX","XY","YY"), values = var(testData)),
	mxMatrix(name = "expMean", type = "Full", nrow = 1, ncol = nVar, free = T, labels = c("meanX","meanY"), values = 0, ubound=1),
	mxExpectationNormal(covariance = "expCov", means = "expMean", dimnames = selVars),
	mxFitFunctionML(vector = FALSE),
	mxData(observed = testData, type = "raw")
)
m1 <- mxRun(m1); round(omxGetParameters(m1),2)
```

| XX   | XY   | YY   | meanX | meanY |
|:-----|:-----|:-----|:------|:------|
| 0.99 | 0.48 | 1.01 | 0.00  | 0.03  |

Summary shows all parameters recovered fine.

```S
round(cov(testData), 2)
```

|   | X    | Y    |
|:--|:-----|:-----|
| X | 0.99 | 0.48 |
| Y | 0.48 | 1.01 |

### Step 2: Switch to vector

You'd do this in one go, of course, but for didactic purposes, it's nice to see that a vector FitFunction 
requires that you also compute a single optimisable number based on that fit vector. In this case
-2 × the sum of the logged likelihoods.

```S
mc <- mxModel("vector_is_TRUE", 
	mxModel(m1, name = "vector",
		mxFitFunctionML(vector = T)
	),
	mxAlgebra(name = "minus2LL", -2 * sum(log(vector.fitfunction)) ),
	mxFitFunctionAlgebra("minus2LL")
)

```

All good! We've now done what OpenMx does for us by default: Assembled a likelihood to optimize against based on the likelihoods of each row given the current parameter estimates.

```S
mc <- mxRun(mc); round(omxGetParameters(mc), 2)
```

Still working fine:

| XX   | XY   | YY   | meanX | meanY |
|:-----|:-----|:-----|:------|:------|
| 0.99 | 0.48 | 1.01 | 0.00  | 0.03  |

### Step 3: Make a weighted version

First, let's make up a simple ramp-style weight variable which assumes we've want to weight scores proportional to their position in the dataset for some reason.

```S
weightVector = seq(0,1, length = nSubs)*2
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

Now, let's pull the estimates and SEs from m1, and from mw (weighted m1) and compare...

```S
out.mw = umx_round(summary(mw)$parameters[,c("Estimate", "Std.Error")],3, coerce = F)
names(out.mw) <- c("weighted_Est", "weighted_SE")
out.m1 = umx_round(summary(m1)$parameters[,c("name","Estimate", "Std.Error")],3, coerce = F)
out.all = cbind(out.m1, out.mw)

within(out.all, {
	SE_Percent = round(((weighted_SE - Std.Error)/(weighted_SE + Std.Error)),2)
	out.all
})
```

|   | name  | Estimate | Std.Error | weighted_Est | weighted_SE | SE_Percent |
|:--|:------|:---------|:----------|:-------------|:------------|:-----------|
| 1 | XX    | 0.994    | 0.044     | 0.708        | 0.032       | -0.16      |
| 2 | XY    | 0.481    | 0.035     | 0.363        | 0.029       | -0.09      |
| 3 | YY    | 1.009    | 0.045     | 0.967        | 0.043       | -0.02      |
| 4 | meanX | -0.005   | 0.032     | 0.549        | 0.027       | -0.08      |
| 5 | meanY | 0.032    | 0.032     | 0.302        | 0.031       | -0.02      |

As you can see above, the estimates are quite a way off (but if our window was a better model of the population, they'd be closer to the truth…), and the SE has shrunk. SE can go up or down after weighting as a side effect of confidence about parameters withing subsets of the data that might be smaller but more homogeneous.

Of course you will use weighting to appropriately weight cases to estimate the population values of your model parameters.

Cool runnings,
tim
