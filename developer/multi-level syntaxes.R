# what is this for?
# "/Users/tim/bin/OpenMx/trunk/models/nightly/MultilevelStateSpaceEx5.R",

 # Collaborate with Paras and Miek Neale?

# ============================================
# = Pedigree diagram analogy for multi-level =
# ============================================

# Stub function to convert dataframes to database format not necessarily optimal, but gets the base user going.
mxDF2DB <- function(dataframe) {
	return(database)
}

mxCascade(model, database){
	# Multi-level is a special name
	"data.teacher" # "teacher" must exist in the db;
}

for(k in unique(data$V1)){
	indivmodels[[k]] <- mxModel(name=paste('indiv', k, sep=''),
	a, b, c, d, q, x0, u,
	LW, PW, TW, CW, LB, PB, TB, CB,
	mxExpectationStateSpace(A="A", B="B", C="C", D="D", Q="Q", R="CovW", x0="x0", P0="CovB", u="u"),
	# note the R and P0 matrices
	mxFitFunctionML(),
	mxData(dataL[[k]], type='raw')) # note separate data
}

maxCores = 8
vCores   = 1:maxCores
time     = rep(NA, maxCores)
for(i in vCores) {
	umx_set_cores(i)
	source("~/bin/OpenMx/trunk/models/nightly/3LatentMultiRegWithContinuousModerator-c.R")
	time[i] = summary(threeLatentWithModeratorOut)$cpuTime
}
plot(time ~ vCores)
 
# MIND GAMES:
# Stoic resilience
# The Stoic philosopher Epictetus taught that “it’s not events, but our opinion about events, that causes us suffering” – an insight that inspired cognitive behavioural therapy and modern resilience training.
#
# Buddhist mindfulness
# The Buddha said: “We are what we think. All that we are is created by our thoughts.” We can change our relationship to our thoughts through mindfulness meditation. Many organisations now practise mindfulness, and there’s even a parliamentary committee devoted to it.
#
# Humanist happiness
# Epicurus taught that the meaning of life is to be happy. We can learn to be happy, by enjoying the present moment and not striving after false desires.
#
# Aristotelian flow
# Aristotle thought happiness comes when we fulfil the drives of our nature for learning, connectedness, freedom and meaningful work – an insight that inspired self-determination theory in psychology.
#
# Christian charity
# The idea that companies should look after the wellbeing of its employees was pioneered by Quaker companies such as Rowntree’s, which had the first-ever welfare department, as well as a library, an in-house magazine, an amateur dramatics company and a company orchestra.
#
# Height, weight, and BMI data from Australian twins. 
# The total sample has been subdivided into a young cohort, aged 18-30 years, and an older cohort aged 31 and above.
# Cohort 1 Zygosity is coded as follows 1 == MZ females 2 == MZ males 3 == DZ females 4 == DZ males 5 == DZ opposite sex pairs
# tip: ?twinData to learn more about this data set

require(OpenMx)
require(umx.twin)
data(twinData)
names(twinData)
# "fam", "age", "zyg", "part", "wt1", "wt2", "ht1", "ht2", "htwt1", "htwt2", "bmi1", "bmi2"

# Set the zygosity to a factor
twinData = factor(twinData, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
table(twinData, useNA = "always")
selDVs = c("bmi1", "bmi2")
mzData <- subset(twinData, ZYG == "MZFF", selDVs)
dzData <- subset(twinData, ZYG == "DZFF", selDVs)
m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
m1 = mxRun(m1)
umxSummaryACE(m1)

# Drop c and test loss of fit
m2 = umxReRun(m1, dropList = "c_r1c1", name = "dropC")
umxCompare(m1, m2)
umxSummaryACE(m2, comparison = m1)
# Example with covariance data only
m1 = umxACE(selDVs = selDVs, dzData = cov(dzData, use = "pairwise"), mzData = cov(mzData, use = "pairwise"), numObsDZ = nrow(dzData), numObsMZ = nrow(mzData))
m1 = umxRun(m1)
umxSummaryACE(m1)
m2 = umxReRun(m1, dropList = "c_r1c1", name = "dropC")
umxSummaryACE(m2, comparison = m1)

require(OpenMx)
data(demoOneFactor)
varNames = names(demoOneFactor)
m2 <- mxModel("One Factor",
	mxMatrix(name = "A", "Full", 5, 1, free = T, values = 0.2), 
	mxMatrix(name = "L", "Symm", 1, 1, free = F, values = 1  ), 
	mxMatrix(name = "U", "Diag", 5, 5, free = T, values = 1  ), 
	mxAlgebra(name = "R", A %*% L %*% t(A) + U, dimnames = varNames), 
	mxMLObjective("R"), 
	mxData(cov(demoOneFactor), type = "cov", numObs = 500)
)
m2 = mxRun(m2)
summary(m2)


Post for umx

Joint model: note expCov is merely the best the specified model can do: It might still fit terribly, in which case its expectations will be awful: that is what bad fit is: poor expectations (given the number of parameters used to generate the expectation).

BBS paper on Causality

For any sub-set of data from the world (including multiple time samples of the possible data), arbitrarily many false theories may be constructed which fit these data. Given this, how may we test theories? In particular, what can we test, given models in SEM?

Machines do not generate theories.

```splus

library(OpenMx)

# create a covariance matrix for two variables, "a" and "b"
varNames = c("a", "b")
covariance <- matrix(c(1.0, 0.5, 0.5, 1.0), nrow = 2, dimnames = list(varNames, varNames))
myData <- mxData(covariance, "cov", numObs = 100)

# create an expected covariance matrix
expCov <- mxMatrix(name = "expectedCov", "Symm", nrow = 2, ncol = 2,
	free = TRUE, values = c(1, .5, 1), labels = c("var1", "cov12", "var2")
)

# Specify the model
model <- mxModel(model = "CI Example", 
	myData, expCov,
    mxCI(c("var1", "cov12", "var2")),
	mxExpectationNormal("expectedCov", dimnames = varNames),
	mxFitFunctionML()
)

# Run it
model <- mxRun(model, intervals = TRUE)

# view confidence intervals
summary(model)$CI
# view all results
summary(model)
# note: the CIs are tighter than the SEs would suggest (based on CI = est ± 1.96 × SE

```