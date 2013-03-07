m1 <- mxModel("Initial Model",
  mxMatrix(name = "alpha" , type = "Full", nrow = 1, ncol = 5, free = T, values = 0),
  mxMatrix(name = "mu"    , type = "Full", 1, 1, F, 0.0),
  mxMatrix(name = "lambda", type = "Full", 1, 5, T, 0.8),
  mxMatrix(name = "phi"   , type = "Symm", 1, 1, F, 1.0),
  mxMatrix(name = "theta" , type = "Diag", 5, 5, T, 0.6),
  mxAlgebra( t(lambda) %*% phi %*% lambda + theta, name = "cov"),
  mxAlgebra(alpha + mu %*% lambda, name = "mean"),
  mxFIMLObjective("cov", "mean", names(demoOneFactor)),
  mxData(demoOneFactor, "raw")
)

# run the model and put results into object with new name.
system.time(mxRun(m1))
m1 <- mxRun(m1)
umxReportFit(m1)

f1 <- mxModel(m1, name = "FactorScore")

# free factor mean value
f1$mu@free[,] <- T
# fix free parameters at estimated values
f1$alpha@free[,]  <- F
f1$lambda@free[,] <- F
f1$theta@free[,]  <- F
# change objective function
f1@objective <- mxFIMLObjective("theta", "mean", names(expData),
vector=TRUE)
# change data
for (i in 1:numrow(demoOneFactor)) {
	f1@data <- mxData(demoOneFactor[1,], "raw")
	fullModel <- mxModel("Weighted Factor Score Model", f1,
		mxAlgebra(name = "weight", expression = 1 / (sqrt(2*pi) * sqrt(det(FactorScore.phi))) * exp(-.5 * (FactorScore.mu %&% solve(FactorScore.phi)))),
		mxAlgebra(name = "alg"   , expression = -2 * log(weight %x% FactorScore.objective)),
		mxAlgebraObjective("alg")
	)
	fsResults <- mxRun(fullModel)
}

# Make the weighted factor score model

# Factor score estimation is a controversial topic. If you want to use a traditional 
# (i.e., Bartlett) estimator, you could use the OpenMx estimated loadings and use 
# the functions in the psych library.

# http://openmx.psyc.virginia.edu/svn/trunk/demo/OneFactorModel_PathRaw.R
require(OpenMx)
data(demoOneFactor)
manifests = names(demoOneFactor)
myCov = cov(demoOneFactor)
nVar =  length(manifests); nFac = 1
nObs = nrow(demoOneFactor)

m2 <- mxModel("One_Factor", type="RAM",
	latentVars = "G",
	manifestVars = manifests,
	mxPath(from = "G", to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = "G", arrows = 2, free = F, values = 1.0),
	mxData(cov(demoOneFactor), type = "cov", numObs = nObs)
)

m2 <- mxModel("One_Fac_raw", type="RAM",
	latentVars = "G",
	manifestVars = manifests,
	mxPath(from = "G", to = manifests),
	mxPath(from = manifests, arrows = 2),
	mxPath(from = "one", to = manifests, arrows = 1),
	mxPath(from = "G", arrows = 2, free = F, values = 1.0),
	mxData(demoOneFactor, type = "raw")
)
system.time(mxRun(m2))
m2 = mxRun(m2)
umxReportFit(m2)

m1 <- mxModel("One_Factor",
	mxMatrix(name  = "alpha" , type = "Full", nrow = nVar, ncol = nFac, values = 0.2, free = T),
	mxMatrix(name  = "lambda", type = "Symm", nrow = nFac, ncol = nFac, values = 1.0, free = F),
	mxMatrix(name  = "theta" , type = "Diag", nrow = nVar, ncol = nVar, values = 1.0, free = T),
	mxAlgebra(name = "cov", exp  = alpha %*% lambda %*% t(alpha) + theta),
	mxMLObjective(covariance = "cov", dimnames = manifests),
	mxData(myCov, type = "cov", numObs = nObs)
)
m1 = mxRun(m1)
umxReportFit(m1)

m3 <- mxModel("Initial Model",
  mxMatrix(name = "alpha" , type = "Full", nrows = nFac, ncols = nVar, free = T, values =  0),
  mxMatrix(name = "mu"    , type = "Full", nrows = nFac, ncols = nFac, free = F, values =  0),
  mxMatrix(name = "lambda", type = "Full", nrows = nFac, ncols = nVar, free = T, values = .8),
  mxMatrix(name = "phi"   , type = "Symm", nrows = nFac, ncols = nFac, free = F, values =  1),
  mxMatrix(name = "theta" , type = "Diag", nrows = nVar, ncols = nVar, free = T, values = .6),

  mxAlgebra(name = "cov"  , exp = t(lambda) %*% phi %*% lambda + theta),

  mxAlgebra(name = "mean" , exp = alpha + mu %*% lambda),
  mxData(demoOneFactor, "raw"),
  mxFIMLObjective("cov", "mean", names(demoOneFactor))
)

m3 = mxRun(m3)
umxReportFit(m3)
