# pain points: the z matrix (join of models)

library(lme4); library(OpenMx)
fm1 = lmer(Reaction ~ Days + (Days | Subject), sleepstudy, REML=FALSE)

sleepstudy$Subject = as.integer(levels(sleepstudy$Subject)[unclass(sleepstudy$Subject)])

# ===========================
# = Hybrid matrix/path spec =
# ===========================

manifestVars = c("Reaction"); latentVars = "DayEffect"

m1 = umxRAM("sleep", data = mxData(sleepstudy, type="raw", sort = FALSE),
	umxPath(means = "Reaction"),
	umxPath(var = "Reaction", values=1),
	umxPath(means = "DayEffect", free=FALSE, labels="data.Days"),

	umxPath("DayEffect", to = "Reaction"),

	# matrix to hold the between level mapping
	mxMatrix(name="Z", nrow=1, ncol=2, values=1, labels=c('data.Days', NA), dimnames=list(c("Reaction"), c("slope", "intercept")), joinKey = "Subject", joinModel = "bySubject"),
	mxFitFunctionML(fellner=FALSE)
)

m1$expectation$between = "Z"

m1 = mxModel(m1,
	mxModel("bySubject", type="RAM", latentVars=c("slope", "intercept"),
		# No data here, only primary key
		mxData(type="raw", observed= data.frame(Subject= unique(sleepstudy$Subject)), sort= FALSE, primaryKey = "Subject"),
		mxPath(c("intercept", "slope"), arrows= 2, values= 1),
		mxPath("intercept", "slope", arrows= 2, values= .25, labels= "cov1")
	)
)

m1$bySubject$fitfunction = NULL

# Fix map matrix
map = mxMatrix(name="Z", nrow=2, ncol=2, dimnames=list(c("Reaction", 'DayEffect'), c("slope", "intercept")), joinKey = "Subject", joinModel = "bySubject")
map$labels['Reaction','slope'] = 'data.Days'
map$values['Reaction','intercept'] = 1
m1 = mxModel(m1, map)

m1$fitfunction$fellner = TRUE

m1 = mxRun(m1)

#####################
# -  All path spec  -
#####################

m2 = mxModel(model="sleep", type="RAM", manifestVars=c("Reaction"), latentVars = "DayEffect",
	mxData(type="raw", observed=sleepstudy, sort = FALSE),
    mxPath(c("one"), "Reaction"),
	mxPath(c("one"), "DayEffect", free=FALSE, labels="data.Days"),
	mxPath("DayEffect", "Reaction"),
	mxPath(c("Reaction"), arrows=2, values=1)
)

bySub = mxModel(model="by_Subject", type="RAM", latentVars=c("slope", "intercept"),
	# no data here, only primary key
	mxData(type="raw", observed=data.frame(Subject=unique(sleepstudy$Subject)), primaryKey = "Subject"),
	mxPath(c("intercept", "slope"), arrows=2, values=1),
	mxPath("intercept", "slope", arrows=2, values=.25, labels="cov1")
)

m2 = mxModel(m2, bySub)

m2 = mxModel(m2,
	mxPath(from= 'by_Subject.intercept', to = 'Reaction', values=1, free=FALSE, joinKey="Subject"),
	mxPath(from= 'by_Subject.slope', to = 'Reaction', labels='data.Days', free=FALSE, joinKey="Subject")
)

m2 = mxRun(m2)
