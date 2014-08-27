data(myFADataRaw)
manifests = c("z1", "z2", "z3")
oneFactorOrd    <- myFADataRaw[,manifests]
oneFactorOrd$z1 <- mxFactor(oneFactorOrd$z1, levels=c(0, 1))
oneFactorOrd$z2 <- mxFactor(oneFactorOrd$z2, levels=c(0, 1))
oneFactorOrd$z3 <- mxFactor(oneFactorOrd$z3, levels=c(0, 1, 2))
str(oneFactorOrd)

m9 <- mxModel("CF", type = "RAM", manifestVars = manifests, latentVars = "F1",
	# residual variance (fixed at 1)
    mxPath(from = manifests, arrows = 2, free = F, values = 1, labels = paste0("e",1:3)),
	# latent variance (free)
	mxPath(from = "F1", to = "F1", arrows = 2, free = T, values = 1, labels ="varF1"),
	# factor loadings
	mxPath(from = "F1", to = paste0("z", 1:3), arrows = 1, free = c(F, T, T), values = 1, labels = paste0("l", 1:3)),
	# means
	mxPath(from = "one", to = "F1", free = F, values = 0, labels = "meanF"),
	mxPath(from = "one", to = manifests, free = F, values = 0, labels = paste0("meanz", 1:3)),

	mxMatrix(name = "threshMat", "Full", nrow = 2, ncol = 3, 
		free = c(T, T, T, F, F, T), values = c(-1, 0, -.5, NA, NA, 1.2), 
		dimnames = list(c(), manifests), byrow = T
	),
	mxExpectationRAM(A = "A", S = "S", F = "F", M = "M", thresholds = "threshMat"),
	mxFitFunctionML(),
	mxData(oneFactorOrd, type = "raw")  # residual variances
) # close model
m9 <- mxRun(m9)
round(umxExpCov(m9), 2)
cov2cor(umxExpCov(m9))
umxHetCor(m9$data$observed)

# What to do if the var of a binary variable is fixed at 1?
# Or if the ordinal should be around 1?

# =================
# = Joint example =
# =================
require(OpenMx)
conts     = c("x1", "x2", "x3")
notbin    = c(conts, "z3")
ords      = c("z1", "z2", "z3")
manifests = c(conts, ords)
oneFactorJoint <- myFADataRaw[, manifests]

# oneFactorJoint$x1 = oneFactorJoint$x1 *10
# oneFactorJoint$x2 = oneFactorJoint$x2 *10
# oneFactorJoint$x3 = oneFactorJoint$x3 *10

oneFactorJoint$z1 <- mxFactor(oneFactorOrd$z1, levels=c(0, 1))
oneFactorJoint$z2 <- mxFactor(oneFactorOrd$z2, levels=c(0, 1))
oneFactorJoint$z3 <- mxFactor(oneFactorOrd$z3, levels=c(0, 1, 2))
str(oneFactorJoint)
m8 <- mxModel("joint", type = "RAM", manifestVars = manifests, latentVars = "F1",
	# can fix ordinals@1 for same result if we leave their thresholds free)
	# last alternative is to what...)

	# residual variances (start at 1, binaries fixed@1)
	umxPath(var = manifests, free = manifests %in% notbin, values = 1, labels = paste0("e", 1:6)),

	# latent variance
	umxPath(var = "F1", fixedAt = 1, labels = "varF1"),

	# factor loadings
	umxPath(from = "F1", to = manifests, free = T, values = 1, labels = paste0("l", 1:6)), 

	# means
	umxPath(means = manifests, free=manifests %in% notbin, values = 0, labels = paste0("mean", manifests)),
	umxPath(means = "F1", fixedAt = 0, labels = "meanF1"),

	umxThresholdMatrix(oneFactorJoint),
	# mxMatrix(name = "threshMat", "Full", nrow = 2, ncol = 3, free = c(T, T, T, F, F, T), values = c(-1, 0,-.5, NA, NA, 1.2), dimnames = list(c(), ords), byrow = T),
	# mxMatrix(name = "threshMat", "Full", nrow = 2, ncol = 3, free = F, values = c(-1, 0,-.5, NA, NA, 1.2), dimnames = list(c(), ords), byrow = T),

	mxExpectationRAM(A = "A", S = "S", F = "F", M = "M", thresholds = "threshMat"),
	mxFitFunctionML(),
	mxData(oneFactorJoint, type="raw")
)
m8 = umxValues(m8)
m8 = umxValues(m8)
# todo
# Error in colMeans(theData[, manifests], na.rm = TRUE) :
#   'x' must be numeric

m8 = umxLabel(m8)
m8 = umxLabel(m8)
m8 <- mxRun(m8)
umx_show(m8)
summary(m8)
mxCompare(m8, ma)
plot(m8, showFixed=T); summary(m8)
round(umxExpCov(m8), 2)
round(cov2cor(umxExpCov(m8)),2)
ggplot2::qplot(z3, z2, data = oneFactorJoint, geom = "jitter")

h = umxHetCor(m8$data$observed, verbose=T)
d = diag(var(oneFactorJoint, use="complete"))
d[umx_is_ordered(oneFactorJoint)] = 1

# Compute the covariance, leaving ordered variables at 1
covStarts = diag(d) %*% h %*% diag(d)
# knock down the covariances
covStarts[lower.tri(covStarts)] = covStarts[lower.tri(covStarts)]* .5

a = t(chol(covStarts * .45)) # these are probably OK as start values
c = t(chol(covStarts * .1))  # these are probably OK as start values
e = t(chol(covStarts * .45)) # these are probably OK as start values

m8$threshMat$values
http://www.bbc.co.uk/iplayer/episode/p018dvyg/horizon-19811982-9-the-pleasure-of-finding-things-out


