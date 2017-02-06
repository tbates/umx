library(lme4)
library(OpenMx)
fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE)

sleepstudy$Subject <- as.integer(levels(sleepstudy$Subject)[unclass(sleepstudy$Subject)])

# ===========================
# = hybrid matrix/path spec =
# ===========================

manifestVars=c("Reaction"), latentVars = "DayEffect"
m1 <- umxRAM("sleep", data = mxData(sleepstudy, type="raw", observed=sort = FALSE),
	umxPath(means = "Reaction"),
	umxPath(means = "DayEffect", free=FALSE, labels="data.Days"),
	umxPath("DayEffect", to = "Reaction"),
	umxPath(var = "Reaction", values=1),

	# this is the between level mapping
	mxMatrix(name="Z", nrow=1, ncol=2, values=1, labels=c('data.Days', NA),
	       dimnames=list(c("Reaction"), c("slope", "intercept")),
	       joinKey = "Subject", joinModel = "bySubject"),
	mxFitFunctionML(fellner=FALSE)
)

m1$expectation$between <- "Z"

m1 <- mxModel(m1, mxModel(
  model="bySubject", type="RAM",
  latentVars=c("slope", "intercept"),
  mxData(type="raw",
         # no data here, only primary key
         observed=data.frame(Subject=unique(sleepstudy$Subject)),
         sort=FALSE, primaryKey = "Subject"),
  mxPath(c("intercept", "slope"), arrows=2, values=1),
  mxPath("intercept", "slope", arrows=2, values=.25, labels="cov1")))

m1$bySubject$fitfunction <- NULL

omxCheckError(mxRun(m1), "Join mapping matrix sleep.Z must have 2 rows: 'Reaction' and 'DayEffect'")

# fix map matrix
map <- mxMatrix(name="Z", nrow=2, ncol=2, dimnames=list(c("Reaction", 'DayEffect'), c("slope", "intercept")), joinKey = "Subject", joinModel = "bySubject")
map$labels['Reaction','slope'] <- 'data.Days'
map$values['Reaction','intercept'] <- 1
m1 <- mxModel(m1, map)

m1$fitfunction$fellner <- TRUE

m1 <- mxRun(m1)

# ------------------- all path spec

m2 <- mxModel(model="sleep", type="RAM", manifestVars=c("Reaction"), latentVars = "DayEffect",
              mxData(type="raw", observed=sleepstudy, sort = FALSE),
              mxPath(c("one"), "Reaction"),
              mxPath(c("one"), "DayEffect", free=FALSE, labels="data.Days"),
              mxPath("DayEffect", "Reaction"),
              mxPath(c("Reaction"), arrows=2, values=1))

omxCheckError(mxModel(m2, mxPath('by_Subject.intercept', 'Reaction',
                   values=1, free=FALSE, joinKey="Subject")),
              "Nice try. You need to create an upper level RAM model called 'by_Subject' and add it as a submodel of 'sleep' before you can create paths between these models.")

bySub <- mxModel(
  model="by_Subject", type="RAM",
  latentVars=c("slope", "intercept"),
  mxData(type="raw",
         # no data here, only primary key
         observed=data.frame(Subject=unique(sleepstudy$Subject)),
         primaryKey = "Subject"),
  mxPath(c("intercept", "slope"), arrows=2, values=1),
  mxPath("intercept", "slope", arrows=2, values=.25, labels="cov1"))

m2 <- mxModel(m2, bySub)

m2 <- mxModel(m2,
	mxPath('by_Subject.intercept', 'Reaction', values=1, free=FALSE, joinKey="Subject"),
	mxPath('by_Subject.slope', 'Reaction', labels='data.Days', free=FALSE, joinKey="Subject")
)

m2 <- mxRun(m2)
