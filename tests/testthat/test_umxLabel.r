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
m1 = umxRun(m1, setLabels = T, setValues = T)

m1 = umxRun(m1); AIC(m1)
umxSummary(m1, show = "std"); # plot(m1, showFixed = T)

# ================
# = Test working =
# ================

testthat::expect_warning(
	xmuLabel_RAM_Model(m1, suffix = "", labelFixedCells = TRUE, overRideExisting = FALSE, verbose = FALSE),
	"You are using raw data, but have not yet added paths for the means"
)



xmuLabel_RAM_Model(m1, suffix = "", labelFixedCells = TRUE, overRideExisting = FALSE, verbose = FALSE)