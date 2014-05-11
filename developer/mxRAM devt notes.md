# Questions to answer for mxRAM

1. With reflective latents, (incoming from manifests)
* How to automagically intercorrelate them?
	1. list these manifests as fixed?
	2. detect them as inputs and correlate them with each other?

| Formula type               | OpenMx                       | SEM     | Lavaan |
|:---------------------------|:-----------------------------|:--------|:-------|
| latent variable definition | from = y, to = x             | y -> x  | x =~ y |
| regression                 | from = x, to = y             | x -> y  | y ~  x |
| (residual) (co)variance    | from = x, to = y, arrows = 2 | x <-> y | x ~~ y |
| intercept                  | from="one", to = x           | 1 -> x  | x ~  1 |

```splus
library(umx); library(lavaan)
data(PoliticalDemocracy)

m2 = umxRAM("Bollen1989",
	# Measurement model
    mxPath(from = "ind60", to = c("x1", "x2", "x3")),
    mxPath(from = "dem60", to = c("y1", "y2", "y3", "y4")),
    mxPath(from = "dem65", to = c("y5", "y6", "y7", "y8")),
  	# Regressions
    mxPath(from = "ind60", to = "dem60"),
    mxPath(from = c("ind60", "dem60"), to = "dem65"),
	# Residual correlations
    mxPath(from = "y1", to = "y5", arrows = 2),
    mxPath(from = "y2", to = c("y4", "y6"), arrows = 2),
    mxPath(from = "y3", to = "y7", arrows = 2),
    mxPath(from = "y4", to = "y8", arrows = 2),
    mxPath(from = "y6", to = "y8", arrows = 2),
	mxData(cov(PoliticalDemocracy), type = "cov", numObs = 500),
	
    # things I don't yet add but could
	# residuals for manifests
	mxPath(from = names(PoliticalDemocracy), arrows = 2),
	# variance for latents
	mxPath(from = c("ind60", "dem60", "dem65"), values = .5, arrows = 2)
)
# m2 = umxFixEndogenousLatentVars(m2)
m2 = umxAddResiduals(m2)
m2 = umxFixLatents(m2)
m3 = umxFixFirstLoadings(m2)
umx_show(m2, matrices = c("A","S"))
m2 = mxRun(m2)
plot(m2, showFixed = T)
umxSummary(m2, show = "std")
RMSEA(m2)
```
### Using Lavaan: 10 lines excluding comments

```splus    
model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'
fit <- sem(model, data = PoliticalDemocracy)
summary(fit)

```

### sem package

```splus
    
```

```splus
    
umx_print(data.frame(m1@matrices$A@values), zero.print = ".", digits=2)
#       y1 y2 y3 y4 y5 y6 y7 y8 x1 x2 x3 ind60 dem60 dem65
# y1     .  .  .  .  .  .  .  .  .  .  .     .   0.3     .
# y2     .  .  .  .  .  .  .  .  .  .  .     .   0.3     .
# y3     .  .  .  .  .  .  .  .  .  .  .     .   0.3     .
# y4     .  .  .  .  .  .  .  .  .  .  .     .   0.3     .
# y5     .  .  .  .  .  .  .  .  .  .  .     .     .   0.3
# y6     .  .  .  .  .  .  .  .  .  .  .     .     .   0.3
# y7     .  .  .  .  .  .  .  .  .  .  .     .     .   0.3
# y8     .  .  .  .  .  .  .  .  .  .  .     .     .   0.3
# x1     .  .  .  .  .  .  .  .  .  .  .   0.3     .     .
# x2     .  .  .  .  .  .  .  .  .  .  .   0.3     .     .
# x3     .  .  .  .  .  .  .  .  .  .  .   0.3     .     .
# ind60  .  .  .  .  .  .  .  .  .  .  .     .     .     .
# dem60  .  .  .  .  .  .  .  .  .  .  .   0.3     .     .
# dem65  .  .  .  .  .  .  .  .  .  .  .   0.3   0.3     .

m1@matrices$A@values[1:4,13]= .2
m1@matrices$A@values[m1@matrices$A@values > 0] = .18
m1 = mxRun(m1)

umx_print(data.frame(m1@matrices$S@values), zero.print = ".", digits=2)
diag(m1@matrices$S@values)= diag(m1@matrices$S@values)*.6 
m1 = mxRun(m1)

library(lavaan); library(OpenMx)
latents   <- c("ind60", "dem60", "dem65")
manifests <- c("y1", "y2", "y3", "y4", "y5", "y6", "y7", "y8", "x1", "x2", "x3")

m1 = mxModel("Bollen1989", type = "RAM",
	latentVars = latents,
	manifestVars = manifests,
	# Measurement model (loading 1 fixed to 1 for each latent)
    mxPath(from = "ind60", to = c("x1", "x2", "x3")      , free = c(F, T, T)   , values = c(1, .8, .8)),
    mxPath(from = "dem60", to = c("y1", "y2", "y3", "y4"), free = c(F, T, T, T), values = c(1, .8, .8, .8)),
    mxPath(from = "dem65", to = c("y5", "y6", "y7", "y8"), free = c(F, T, T, T), values = c(1, .8, .8, .8)),
  	# Regressions
    mxPath(from = "ind60", to = "dem60", values = 1.5),
    mxPath(from = c("ind60", "dem60"), to = "dem65", values = .7),
	# Residual correlations
    mxPath(from = "y1", to = "y5", arrows = 2, values = 1),
    mxPath(from = "y2", to = c("y4", "y6"), arrows = 2, values = 1),
    mxPath(from = "y3", to = "y7", arrows = 2, values = 1),
    mxPath(from = "y4", to = "y8", arrows = 2, values = 1),
    mxPath(from = "y6", to = "y8", arrows = 2, values = 1),
	mxData(cov(PoliticalDemocracy), type = "cov", numObs = 500),

    # basic stuff: residuals for manifests and latents
	mxPath(from = names(PoliticalDemocracy), arrows = 2, values = diag(var(PoliticalDemocracy))/3),
	mxPath(from = c("ind60", "dem60", "dem65"), values = .5, arrows = 2)
)
umx_show(m1)
m1 = umxSetValues(m1, onlyTouchZeros = T)
m1 = umxSetLabels(m1)
m1 = mxRun(m1)

m1 <- mxModel("m1", 
	mxPath(from = c("cyl", "disp"), to = "l_power"),
	mxPath(from = "l_power", to = "mpg"),
	mxData(thedata, type = "raw"),

    type="RAM",                
	latentVars = "l_power",
	manifestVars = c("mpg",  "cyl",  "disp"),
	mxPath(from = "one", to = c("mpg",  "cyl",  "disp"), arrows = 1), # manifest means
	mxPath(from = c("cyl", "disp"), connect = "unique.bivariate", arrows = 2, free = T)
)
m1 = umxRun(m1, setLabels = T, setValues = T)
plot(m1, showMeans = T)
umxSummary(m1)

```


# measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8


## Added by lavaan:
### Variances:
x1                0.082    0.019                      0.082    0.154
x2                0.120    0.070                      0.120    0.053
x3                0.467    0.090                      0.467    0.239
y1                1.891    0.444                      1.891    0.277
y2                7.373    1.374                      7.373    0.486
y3                5.067    0.952                      5.067    0.478
y4                3.148    0.739                      3.148    0.285
y5                2.351    0.480                      2.351    0.347
y6                4.954    0.914                      4.954    0.443
y7                3.431    0.713                      3.431    0.322
y8                3.254    0.695                      3.254    0.315
ind60             0.448    0.087                      1.000    1.000
dem60             3.956    0.921                      0.800    0.800
dem65             0.172    0.215                      0.039    0.039

```splus
library(umx)
names(mtcars)
m1 = mxRAM(latentVars = "eff",
	mxPath(from = c("mpg", "cyl", "disp", "hp") to = "eff")
	data = mtcars
)
```

# ==============
# = umxRAM fun =
# ==============

require(OpenMx); data(demoOneFactor)
latents  = c("G")
manifests = paste0("x", 1:3)
latents = "g"
m1 <- umxRAM("One Factor",
	manifestVars = manifests,
	latentVars = latents,
	mxPath(from = "g"  , arrows = 2, free = F, values = 1.0),
	mxPath(from = latents, to = manifests),
	mxPath(from = manifests, arrows = 2),
	data = mxData(cov(demoOneFactor[,manifests]), type = "cov", numObs = 500)
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1, show = "std")

m2 <- umxRAM("One Factor", fix = "latents",
	mxPath(from = "g", to = paste0("x", 1:3)),
	data = mxData(cov(demoOneFactor[,manifests]), type = "cov", numObs = 500)
)

m2 = umxRun(m2); umxSummary(m2, show = "std")
umxCheck(m2)
umx_show(m2)

mxCompare(m1,m2)

umxCheck <- function(model) {
	
}