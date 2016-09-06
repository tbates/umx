require(umx)
data(twinData) 
names(twinData)
# latentVars   = c("DefDummy")
# latents   = c("L1", "L2")
# manifestVars = selDVs

# umxPath(definition = "age"){
# 	theName = paste0(definition, "_def")
# 	umxPath(var   = theName, fixedAt = 0),
# 	umxPath(means = theName, fixedAt = 0, labels = paste0("data.", definition)),
#
# }
# umxPath(v0m0 = "Age_def", labels = "data.age"),

selDVs  = c("bmi1", "bmi2")
selDefs = c("age")
selVars = c(selDVs, selDefs)
mzData  = subset(twinData, zyg == 1 & !is.na(age), selVars); dim(mzData)
dzData  = subset(twinData, zyg == 3 & !is.na(age), selVars); dim(dzData)

m1 <- umxRAM("MZ", data = mxData(mzData, type = "raw"),remove_unused_manifests = FALSE,
	# variances and covariances + means for vars, and defVar
	umxPath(v.m.  = selDVs),
	umxPath(var   = "Age_def", fixedAt = 0),
	umxPath(means = "Age_def", fixedAt = 0, labels = "data.age"),

	# =========
	# = Model =
	# =========
	umxPath("bmi1", with = "bmi2"),
	# betaWeights
	mxPath("Age_def", to = selDVs, labels = c("beta_1", "beta_1"))
	# mxPath("Age_def", to = selDVs, labels = c("beta_1", "beta_2"))
)
plot(m1, digits = 3)

m1$matrices
m1$algebras

# Remember to knock off 1 and 2 
# from group 1's data
# so as to estimate variance of 
# combined sample without the mean 
# correction. First we compute some 
# summary statistics from the data
# -------------------------------------
ObsCovs        <- cov(rbind(group1 - rep(c(1,2), each=N), group2))
ObsMeansGroup1 <- c(mean(group1[,1]), mean(group1[,2]))
ObsMeansGroup2 <- c(mean(group2[,1]), mean(group2[,2]))

# Second we extract the parameter 
# estimates and matrix algebra results 
# from the model.
# -------------------------------------
Sigma <- mxEval(S[1:2,1:2], m1)
Mu    <- mxEval(M[1:2], m1)
beta  <- mxEval(A[1:2,3], m1)


m1 <- umxRAM("MZ", data = mzData,
	umxPath(v1m0 = c("L1", "L2")),
	umxPath(var   = selDVs, fixedAt = 0),
	# umxPath(means = selDVs, fixedAt = 0),
	umxPath(means = selDVs),

	umxPath("L1", to = selDVs[1]),
	umxPath("L2", to = selDVs[2]),
	umxPath("L1", with = "L2")
)
plot(m1)


selVars   = c(selDVs, selDefs)
obsMean   = mean(colMeans(mzData[,selDVs], na.rm = TRUE)); # Just one average mean for all twins
nVar      = length(selDVs)/nSib; # number of dependent variables ** per INDIVIDUAL ( so times-2 for a family)**
rawVar    = diag(var(mzData[,selDVs], na.rm = TRUE))[1]
startMain = sqrt(c(.8, .0 ,.6) * rawVar)	

# drop any unused variables
dzData = dzData[,selVars]
mzData = mzData[,selVars]

missingT1 = is.na(mzData[,selDefs[1]])
missingT2 = is.na(mzData[,selDefs[2]])
mzData = mzData[!(missingT1 | missingT2), ]

missDef = is.na(dzData[,selDefs[1]]) | is.na(dzData[,selDefs[2]])
dzData = dzData[!missDef, ]


model = mxModel(name,
	mxModel("top",
		# Matrices a, c, and e to store a, c, and e path coefficients
		umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = startMain[1], name = "a" ), jiggle = .0001),
		# Matrices to store moderated path coefficients                       
		umxLabel(mxMatrix("Lower", nrow = nVar, ncol = nVar, free = TRUE, values = 0, name = "am" )),

		# Matrices A, C, and E compute non-moderated variance components 
		mxAlgebra(name = "V", a %*% t(a) ),
		# Algebra to compute total variances and inverse of standard deviations (diagonal only)
		mxMatrix(name  = "I", "Iden", nrow = nVar, ncol = nVar),
		mxAlgebra(name = "iSD", solve(sqrt(I * V)) ),

		# Matrix & Algebra for expected means vector (non-moderated)
		mxMatrix(name = "Means", "Full", nrow = 1, ncol = nVar, free = TRUE, values = obsMean, labels = "mean"), # needs mods for multivariate!
		# Matrices for betas
		mxMatrix(name = "betaLin" , "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "lin11"), 
		mxMatrix(name = "betaQuad", "Full", nrow = nVar, ncol = 1, free = TRUE, values = .0, labels = "quad11")
	),
	mxModel("MZ",
		# matrices for covariates (just on the means)
		# Matrix for moderating/interacting variable
		mxMatrix(name = "Def", "Full", nrow=1, ncol=1, free=F, labels = paste0("data.", selDefs[1])), # twin1 c("data.age1") (twin2  has to be the same!!!!)
		# Algebra for expected mean vector
		# TODO simplyfy this algebra... one for both twins and all def vars... not 4* cov...
		mxAlgebra(top.betaLin  %*% Def  , name = "DefRlin"),
		mxAlgebra(top.betaQuad %*% Def^2, name = "DefRquad"),
		mxAlgebra( cbind(top.Means + DefRlin + DefRquad, top.Means + DefRlin + DefRquad), name = "expMeanMZ"),
		
		# Compute ACE variance components
		mxAlgebra((top.a + top.am %*% Def) %*% t(top.a+ top.am %*% Def), name = "A11"),
		# Algebra for expected variance/covariance matrix and expected mean vector in MZ
		mxAlgebra(rbind(cbind(A11        , 0.5 %x% A12),
		                cbind(0.5 %x% A21, A22 + C22 + E22) ), name="expCovMZ"),
		# Data & Objective
		mxData(mzData, type = "raw"),
		mxExpectationNormal("expCovMZ", means = "expMeanMZ", dimnames = selDVs),
		mxFitFunctionML()
	),
    mxModel("DZ",
		mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[1],sep=""), name="Def"), # twin1  c("data.divorce1")
		mxMatrix("Full", nrow=1, ncol=1, free=F, labels=paste("data.",selDefs[2],sep=""), name="Def"), # twin2  c("data.divorce2")
		# Compute ACE variance components
		mxAlgebra((top.a+ top.am%*% Def) %*% t(top.a+ top.am%*% Def), name="A11"),

		# Expected DZ variance/covariance matrix
		mxAlgebra(rbind(cbind(A11  , 0.5%x%A12),
		                cbind(0.5%x%A21+C21, A22+C22+E22) ), name="expCovDZ"),

		# Algebra for expected mean vector
		mxAlgebra(top.betaLin %*% Def  , name = "DefRlin"),
		mxAlgebra(top.betaQuad%*% Def^2, name = "DefRquad"),
		mxAlgebra(top.betaLin %*% Def  , name = "DefRlin"),
		mxAlgebra(top.betaQuad%*% Def^2, name = "DefRquad"),
		mxAlgebra(cbind(top.Means + DefRlin + DefRquad, top.Means + DefRlin + DefRquad), name = "expMeanDZ"),
		# Data & Objective
        	mxData(dzData, type = "raw"),
		mxExpectationNormal("expCovDZ", means = "expMeanDZ", dimnames = selDVs),
		mxFitFunctionML()
    ),
	mxFitFunctionMultigroup(c("MZ", "DZ"))
)