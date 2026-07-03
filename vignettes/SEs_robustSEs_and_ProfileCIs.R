## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 5
)
library(umx)
library(OpenMx)


## ----fit_model, message=FALSE, warning=FALSE----------------------------------
# Load data
data(demoOneFactor)
manifests = names(demoOneFactor)

# Fit model
m1 = umxRAM("One_Factor_Model", data = demoOneFactor,
	umxPath("g", to = manifests),
	umxPath(var = manifests),
	umxPath(var = "g", fixedAt = 1)
)


## ----summary_std, message=FALSE-----------------------------------------------
# uncertainty = c("SE", "RobustSE", "CI", "none"),
umxSummary(m1, std = TRUE, uncertainty = "SE")

umxSummary(m1, std = TRUE, uncertainty = "RobustSE")

umxSummary(m1, std = TRUE, uncertainty = "CI")

## ----robust_se, message=FALSE-------------------------------------------------
# Compute robust standard errors
m1Robust = imxRobustSE(m1)

# Extract standard errors to compare
stdSE    = m1$output$standardErrors
robustSE = m1Robust$output$standardErrors

# Compare SEs side-by-side
compareSE = data.frame(
	Parameter = names(stdSE),
	Standard_ML_SE = round(stdSE, 4),
	Robust_SE = round(robustSE, 4)
)
print(compareSE)


## ----profile_ci, message=FALSE, warning=FALSE---------------------------------
# Request profile likelihood CIs for specific parameters
m1CI = umxConfint(m1, parm = c("g_to_x1", "g_to_x2"), run = TRUE)

# View summary showing profile CIs
umxSummary(m1CI, showEstimates = "raw")

