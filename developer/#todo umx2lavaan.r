fixInNamespace("modelModifyFilter", "OpenMx")

# =============
# = MXModel.R =
# =============

modelModifyFilter <- function(model, entries, action) {
	boundsFilter <- sapply(entries, is, "MxBounds")
	intervalFilter <- sapply(entries, is, "MxInterval")
	namedEntityFilter <- sapply(entries, function(x) {"name" %in% slotNames(x)})
	characterFilter <- sapply(entries, is.character)
	pathFilter <- sapply(entries, is, "MxPath")
  thresholdFilter <- sapply(entries, is, "MxThreshold")
	unknownFilter <- !(boundsFilter | namedEntityFilter | intervalFilter | characterFilter | thresholdFilter)
	if (any(pathFilter)) {
		stop(paste("The model type of model",
			omxQuotes(model@name), "does not recognize paths."),
			"Try setting type to RAM?, i.e.:\n",
			"mxModel(", omxQuotes(model@name), ", type = ", omxQuotes("RAM"), ", ...",
			call. = FALSE)
	}
	if (any(thresholdFilter)) {
	  stop(paste("The model type of model",
	             omxQuotes(model@name), "does not recognize thresholds."),
	       call. = FALSE)
	}
	if (any(unknownFilter)) {
		stop(paste("Cannot", action, "the following item(s)", 
			actionCorrespondingPredicate[[action]], "the model:", 
			omxQuotes(sapply(entries[unknownFilter], deparse))), call. = FALSE)
	}
	if (any(namedEntityFilter) && action == 'remove') {
		stop(paste("Cannot use named entities when remove = TRUE.",
			"Instead give the name of the entity when removing it.",
			"See http://openmx.psyc.virginia.edu/wiki/mxmodel-help#Remove_an_object_from_a_model"))
	}
	if (any(characterFilter) && action == 'add') {
		stop(paste("I don't know what to do with the following strings",
			omxQuotes(entries[characterFilter]),
			"that have been passed into the function:",
			deparse(width.cutoff = 400L, imxLocateFunction("mxModel"))), call. = FALSE)
	}
	if (identical(action, 'add')) {
		return(list(entries[namedEntityFilter], entries[boundsFilter], entries[intervalFilter]))
	} else if (identical(action, 'remove')) {
		return(list(entries[characterFilter], entries[boundsFilter], entries[intervalFilter]))
	} else {
		stop(paste("Internal error, unidentified action:", omxQuotes(action)))
	}
}


data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)
myFADataRaw = myFADataRaw[, manifests]
latents   = c("G")
m1 <- mxModel("m1", type="RAM",
	manifestVars = manifests,
	latentVars   = latents,
	# Factor loadings
	mxPath(from = latents, to = manifests),
	# Residuals and variances
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxData(cov(myFADataRaw, use="complete"), type = "cov", numObs = nrow(myFADataRaw))
)
model = m1
m1 = umxRun(m1, setLabels = T, setStart = T)
umxSummary(m1)



data(myFADataRaw, package="OpenMx")
manifests = names(myFADataRaw)
myFADataRaw = myFADataRaw[, manifests]
latents   = c("G")
m1 <- mxModel("m1", type="RAM",
m1 <- mxModel(
	manifestVars = manifests,
	latentVars   = latents,
	# Factor loadings
	mxPath(from = latents, to = manifests),

	# residuals and variances
	mxPath(from = manifests, arrows = 2), # manifest residuals 
	mxPath(from = latents, arrows = 2, free = F, values = 1), # latents fixed@1
	mxPath(from = c("x1", "x2"), to = "x3", arrows = 1), # manifest causes
	mxPath(from = "one", to = manifests, arrows = 1), # manifest means
	mxData(myFADataRaw, type = "raw")
)
m1 = umxRun(m1, setLabels = T, setStart = T)
umxSummary(m1)

fit <- mxModel(, type="RAM",
   # latent variables
   mxPath(from = "ind60", to = c("x1", "x2", "x3")),
   mxPath(from = "dem60", to = c("y1", "y2", "y3", "y4")),
   mxPath(from = "dem65", to = c("y5", "y6", "y7", "y8")),
   # regressions
   mxPath(from = "dem60", to = "ind60"),
   mxPath(from = "dem65", to = c("ind60", "dem60")),
   # residual covariances
   mxPath(from = "dem60", to = "ind60"),
   mxPath(from = "y1", to = "y5", arrows = 2),
   mxPath(from = "y2", to = c("y4", "y6"), arrows = 2),
   mxPath(from = "y3", to = "y7", arrows = 2),
   mxPath(from = "y4", to = "y8", arrows = 2),
   mxPath(from = "y6", to = "y8", arrows = 2)
)
fit = mxModel(fit, data = PoliticalDemocracy)
fit <- mxRun(fit)
summary(fit)

Lavaan
library("Lavaan")
model <- '
   # latent variables
     ind60 =~ x1 + x2 + x3
     dem60 =~ y1 + y2 + y3 + y4
     dem65 =~ y5 + y6 + y7 + y8
   # regressions
     dem60 ~ ind60
     dem65 ~ ind60 + dem60
   # residual covariances
     y1 ~~ y5
     y2 ~~ y4 + y6
     y3 ~~ y7
     y4 ~~ y8
     y6 ~~ y8
'
fit <- sem(model,
           data=PoliticalDemocracy)
summary(fit)

umxLavaan_2_Mx
from # onwards -> comment line