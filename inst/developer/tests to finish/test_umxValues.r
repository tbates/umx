data(myFADataRaw, package = "OpenMx")
manifests = paste0("x", 1:3)
latents = c("A", "B")
df = myFADataRaw[, manifests]

m1 <- mxModel("m1", type = "RAM", 
	latentVars = latents,
	manifestVars = manifests,
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	# umxPath(means = manifests),
	umxPath(var = manifests),
	umxPath(var = latents),
	mxData(df, "raw")
)
m1 = umxRun(m1, setLabels = TRUE, setValues = TRUE)
m1 = umxRun(m1); AIC(m1)
umxSummary(m1, show = "std"); # plot(m1, showFixed = T)

# ================
# = Test working =
# ================

testthat::expect_error(
	umxValues(m1), "You do this with mxPath(from = 'one', to = 'var')"
)
