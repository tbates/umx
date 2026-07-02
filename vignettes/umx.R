## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 6,
  fig.height = 5
)
library(umx)


## ----load-data----------------------------------------------------------------
# Load the classic Holzinger & Swineford data (x1:x9)
data("HolzingerSwineford1939", package = "lavaan")

# Define the manifest (observed) variables
manifests <- paste0("x", 1:9)

# Quick look at the correlations
round(cor(HolzingerSwineford1939[, manifests]), 2)


## ----build-model, results = "hide", message = FALSE---------------------------
m1 <- umxRAM("Holzinger_1f", 
  data = HolzingerSwineford1939,
  
  # Factor loadings (G predicts each x)
  umxPath("G", to = manifests),
  
  # Residual variances for the observed variables
  umxPath(var = manifests),
  
  # Fix the latent factor variance to 1 (for identification)
  umxPath(var = "G", fixedAt = 1)
)


## ----summary-basic------------------------------------------------------------
umxSummary(m1)


## ----summary-std--------------------------------------------------------------
umxSummary(m1, std = TRUE)


## ----lavaan-string, results = "hide", message = FALSE-------------------------
m2 <- umxRAM("G =~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9", 
             data = HolzingerSwineford1939)


## ----lavaan-summary-----------------------------------------------------------
umxSummary(m2, std = TRUE)


## ----plot, fig.cap="Standardized one-factor Holzinger model"------------------
plot(m1, std = TRUE)

