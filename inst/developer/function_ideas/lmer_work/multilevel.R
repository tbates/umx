library(lme4)
library(visreg)
str(sleepstudy)
df = umx_rename(sleepstudy, "RT","Reaction")
fm1 <- lmer(RT ~ Days + (Days | Subject), df)
visreg(fm1)

sub <− umxRAM("bySubj", mxData(data.frame(Subject=unique(sleepstudy$Subject)), type="raw", primaryKey = "Subject"),
	umxPath(var = c("intercept" , "slope")),
	umxPath("intercept" , with="slope", values=.25, labels="cov1")
)

manifestVars=" Reaction " , latentVars = "Days" ,
ss <− mxModel("sleep", sub,
	mxData(sleepstudy, type = "raw", sort=FALSE),
	mxPath(means = "Reaction"),
	mxPath(means = "Days", free=FALSE, labels="data.Days"),
	mxPath("Days", to = "Reaction"),
	mxPath(var = "Reaction"),
	mxPath(c('sub.intercept','sub.slope'), to = 'Reaction', free=FALSE, values=c(1, NA), labels=c(NA, "data.Days"), joinKey="Subject")
)

umxSubjectLevel <- function(data = theData, primaryKey = "Subject") {
	subjectData = data.frame(Subject=unique(theData[,primaryKey]))
	subjectLevelModel <− umxRAM("subjectLevel", data = mxData(subjectData, type="raw", primaryKey = primaryKey),
		umxPath(var = c("intercept" , "slope")),
		umxPath("intercept" , with = "slope", labels="cov1")
	)
}


umxSubjectLevel <- function(data = theData, primaryKey = "Subject") {
	subjectData = data.frame(Subject = unique(data[,primaryKey]))
	subjectLevelModel <− umxRAM("subjectLevel", # level1?
		data = mxData(subjectData, type = "raw", primaryKey = primaryKey),
		umxPath(var = c("intercept" , "slope")),
		umxPath("intercept", with = "slope", labels = "cov1")
	)
}

multi_level_sem <− mxModel("sleep", type="RAM", 
	umxSubjectLevel(data = theData, primaryKey = "Subject"),
	manifestVars = "Reaction" , latentVars = "Days",
	mxData(sleepstudy, type = "raw", sort = FALSE) ,
	mxPath("one", "Reaction", arrows = 1, free = TRUE),
	mxPath("one", "Days", arrows = 1, free = FALSE, labels = "data.Days"),
	mxPath("Days", "Reaction", arrows = 1, free = TRUE),
	mxPath("Reaction", arrows = 2, values = 1),
	mxPath(c('bySubj.intercept','bySubj.slope'),'Reaction', free=FALSE, values=c(1,NA), labels=c(NA, "data.Days"), joinKey="Subject")
)

# [mplus ex9.6](https://www.statmodel.com/usersguide/chap9/ex9.6.html)
# Two-level CFA with continuous factor indicators (y1-y4), a random intercept factor (clusterID?), and covariates (x1 and x2)
# VARIABLE:	NAMES ARE y1-y4 x1 x2 w clus;
 # 	WITHIN = x1 x2;
# 	BETWEEN = w;
# 	CLUSTER = clus;
# ANALYSIS:	TYPE = TWOLEVEL;
# MODEL:
# 	%WITHIN%
# 	fw BY y1-y4;
# 	fw ON x1 x2;
# 	%BETWEEN%
# 	fb BY y1-y4;
# 	y1-y4@0;
# 	fb ON w;

umxML <- function(within = NA, between = NA, cluster = "", levels = 2, data = NA) {
	type=RAM
}

library(umx)

# ===========================
# = how I made the Rda file =
# ===========================
# setwd("~/bin/umx/")
# ex9_6 <- read.table("~/bin/OpenMx/inst/models/nightly/data/ex9.6.dat")
# names(ex9_6) <- c(paste0("y", 1:4), "x1", "x2", "w", "clusterID")
# head(ex9_6)
# devtools::use_data(ex9_6)

data(ex9_6, verbose=TRUE)
str(ex9_6)

# ==========================================
# = Create the between and within datasets =
# ==========================================
# # TODO can automate this splitting of the data in 2 ... 

# one instance of each unique element of V8 in first appearance order
betweenData <- ex96[!duplicated(ex96$clusterID), c('w', 'clusterID')] # just the w and cluster columns
withinData  <- ex96[,!(names(ex96) %in% "w")]                         # df without the w column (why?)

betweenModel <- mxModel('between', type="RAM", mxData(type="raw", observed=betweenData, primaryKey="clusterID"),
	latentVars = c("lw", "fb"),
	mxPath("one", "lw", labels="data.w", free=FALSE), # means for w?
	mxPath("fb", arrows=2, labels = "psiB"),
	mxPath("lw", 'fb', labels="phi1")
)

wModel <- mxModel('within', type="RAM", betweenModel,
	mxData(type="raw", observed=withinData, sort=FALSE),  #[abs(withinData$clusterID - 41)<= 25,]
	manifestVars = paste0('y', 1:4),
	latentVars = c('fw', paste0("xe", 1:2)),
	mxPath("one", paste0('y', 1:4), values=runif(4), labels=paste0("gam0", 1:4)),
	mxPath("one", paste0('xe', 1:2), labels=paste0('data.x',1:2), free=FALSE),
	mxPath(paste0('xe', 1:2), "fw", labels=paste0('gam', 1:2, '1')),
	mxPath('fw', arrows=2, values=1.1, labels="varFW"),
	mxPath('fw', paste0('y', 1:4), free=c(FALSE, rep(TRUE, 3)), values=c(1,runif(3)), labels=paste0("loadW", 1:4)),
	mxPath('between.fb', paste0('y', 1:4), values=c(1,runif(3)),free=c(FALSE, rep(TRUE, 3)), labels=paste0("loadB", 1:4), joinKey="clusterID"),
	mxPath(paste0('y', 1:4), arrows=2, values=rlnorm(4), labels=paste0("thetaW", 1:4))
)

mle <- structure(
	c(0.9989, 0.9948, 1.0171, 0.9809, 0.9475, 1.0699, 1.0139, 0.9799, -0.0829, -0.0771, -0.0449, -0.0299, 0.9728, 0.5105, 0.9595, 0.9238, 0.9489, 0.361, 0.3445),
	.Names = c("loadW2", "loadW3", "loadW4", "thetaW1", "thetaW2", "thetaW3", "thetaW4", "varFW", "gam01", "gam02", "gam03", "gam04", "gam11", "gam21", "loadB2", "loadB3", "loadB4", "psiB", "phi1")
)

if (1) {
	pt1 <- omxSetParameters(wModel, labels=names(mle), values=mle)
	#	pt1$expectation$.forceSingleGroup <- TRUE
	#	pt1$expectation$.rampart <- 0L
	plan <- mxComputeSequence(list(
		mxComputeOnce('fitfunction', 'fit'),
		mxComputeReportDeriv(),
		mxComputeReportExpectation()
		)
	)
	pt1 <- mxRun(mxModel(pt1, plan))
	omxCheckCloseEnough(pt1$output$fit, 13088.373, 1e-2)

	## ed = pt1$expectation$debug
	## round(ed$g01$covariance[1:4,1:4],3)
	## round(ed$g01$covariance[1:20,1:20],3)
	## round(ed$g01$S[1:20,1:20],3)
	## round(ed$g01$A[1:20,1:20],3)
	## round(ed$g01$covariance[1:20,1:20],3)
	## round(ed$g01$mean[1:20],3)
}

if (1) {
	wModel <- mxRun(wModel)
	summary(wModel)

	omxCheckCloseEnough(wModel$output$fit, 13088.373, 1e-2)
	omxCheckCloseEnough(mle[names(coef(wModel))], coef(wModel), 1e-3)
	omxCheckCloseEnough(wModel$expectation$debug$rampartUsage, 890)
} else {
	options(width=120)
	plan <- mxComputeSequence(list(
		mxComputeOnce('fitfunction', 'fit'),
		mxComputeNumericDeriv(checkGradient=FALSE, hessian=FALSE, iterations=2),
		mxComputeReportDeriv(),
		mxComputeReportExpectation()
		))

		wModel$expectation$.rampart <- 2L
		#	wModel$expectation$scaleOverride <- c(6, 1)
		rotated <- mxRun(mxModel(wModel, plan))

		wModel$expectation$.rampart <- 0L
		square <- mxRun(mxModel(wModel, plan))

		ex <- rotated$expectation
		eo <- ex$output
		ed <- ex$debug
		print(ed$rampartUsage)
		print(abs(rotated$output$fit - square$output$fit))
		print(max(abs(rotated$output$gradient - square$output$gradient)))
}