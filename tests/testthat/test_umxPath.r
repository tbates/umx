data(myFADataRaw, package = "OpenMx")
manifests = paste0("x", 1:3)
latents = c("A","B")
df = myFADataRaw[, manifests]

m1 <- mxModel("m1", type="RAM", 
	latentVars = latents,
	manifestVars = manifests,
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(means = manifests),
	umxPath(var = manifests),
	umxPath(means = latents, fixedAt = 0),
	umxPath(var = latents, fixedAt = 1),
	mxData(df, "raw")
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1)
plot(m1)
umx_show(m1, what = "values", matrices = "M")
umx_show(m1, what = "free")
# ======================
# = Should throw error =
# ======================
# firstAt or fixedAt
testthat::expect_error(umxPath("A", with = "B", fixedAt = 1, firstAt = 1))
testthat::expect_error(umxPath(cov = c("A")))
# fixing first loading requires one item in from 
# # TODO allow one item in to?
testthat::expect_error(umxPath(letters[1:2], to = letters[5:9], firstAt = 1))

# only one of with var cov and means
testthat::expect_error(umxPath(with = "A", var = "A", cov = "A", means = "A", fixedAt = 1, firstAt = 1))
testthat::expect_error(umxPath(with = "A", means = "A", fixedAt = 1, firstAt = 1))
