#' @demoTitle Computing one variable\u2019s effect on another in a complex path diagram.

require(sem)
require(OpenMx)
source("~/bin/omxhelper/genEpi.lib.R")
source("~/bin/umx/umx.lib.R")
 
# Often you will see data presented as a lower diagonal.
# the readMoments() function in the sem package is a nice helper to read this from the screen:
 
data = sem::readMoments(file = "", diag = TRUE)
1
.304 1
.305 .344   1
.100 .156 .158   1
.284 .192 .324 .360   1
.176 .136 .226 .210 .265  1
 
# terminates with an empty line: see ?readMoments for more help
 
# now letsfill in the upper triangle with a flipped version of the lower
data[upper.tri(data, diag=F)] = t(data)[upper.tri(data, diag=F)]
 
# Set up manifest variables
# Just to be helpful to myself, I've made lists of the formative sources, and the reflective receiver variables in this MIMIC model
receivers =  c("church", "member", "friends")
sources   = c("income", "occup", "educ")
manifests = c(sources, receivers)
 
# And latents
latents   = "social" # 1 latent, with three formative inputs, and three reflective outputs (each with residuals)

# Add names to our dataframe
dimnames(data) = list(manifests, manifests)
 
MIMIC <- mxModel("MIMIC", type="RAM",
    manifestVars = manifests,
    latentVars   = latents, 
    # Factor loadings
    mxPath(from = sources , to = "social" ),
    mxPath(from = "social", to = receivers),
 
    # Correlated formative sources for F1, each with variance = 1
    mxPath(from = sources, connect = "unique.bivariate", arrows = 2),
    mxPath(from = sources, arrows = 2, values = 1, free = F ),
 
    # Residual variance on receivers
    mxPath(from = receivers, arrows = 2),
    mxData(data, type = "cor", numObs = 530)
)

MIMIC <- mxRun(MIMIC); summary(MIMIC)

MIMIC = umxLabel(MIMIC)
umxGetParameters(MIMIC , regex = ".")
plot(MIMIC, std = T, precision = 2, dotFilename = "name",  pathLabels = "none", showFixed = F)
umxUnexplainedCausalNexus(from = "educ", delta = 1, to = "church", model = MIMIC)
