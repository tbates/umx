data(myFADataRaw, package = "OpenMx")
manifests = paste0("x", 1:3)
latents = c("A", "B")
df = myFADataRaw[, manifests]

m1 <- mxModel("m1", type = "RAM", 
	latentVars = latents,
	manifestVars = manifests,
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(means = manifests),
	umxPath(var = manifests),
	umxPath(v1m0 = latents),
	mxData(df, "raw")
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1, show = "raw"); plot(m1, showFixed = T)
umx_show(m1, what = "values", matrices = "M")
umx_show(m1, what = "free")

m1 = umxRAM("tim", data = mxData(df, "raw"),
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(means = manifests),
	umxPath(var = manifests),
	umxPath(v1m0 = latents),
)
m1 = umxRun(m1); AIC(m1)
umxSummary(m1, show = "std"); plot(m1, showFixed = T)

# ================
# = Test working =
# ================

# =============
# = test with =
# =============
a = umxPath("a", with ="b")
b = mxPath("a", to = "b", arrows=2)
testthat::expect_equal(a,b)

# ============
# = test var =
# ============
a = umxPath(var = letters[1:4])
b = mxPath(letters[1:4], to = letters[1:4], arrows=2)
testthat::expect_equal(a,b)

# ============
# = test cov =
# ============
a = umxPath(cov=c("a","b"))
b = mxPath("a", to = "b", arrows=2)
testthat::expect_equal(a,b)


# =========
# = unique.bivariate =
# =========
a = umxPath(unique.bivariate=c("a","b", "c"))
b = mxPath(from = c("a","b", "c"), connect = "unique.bivariate", arrows = 2)
testthat::expect_equal(a,b)

# =========
# = means =
# =========
a = umxPath(means = c("a","b", "c"))
b = mxPath("one", to = c("a","b", "c"))
testthat::expect_equal(a,b)

# ================
# = test fixedAt =
# ================
a = umxPath(var = letters[1:4], fixedAt = 1)
b = mxPath(letters[1:4], to = letters[1:4], free = F, values = 1 , arrows = 2)
testthat::expect_equal(a,b)

# ================
# = test firstAt =
# ================
a = umxPath("a", to = letters[2:6], firstAt = 1)
b = mxPath("a", to = letters[2:6], free = c(F,T,T,T,T), values = c(1,NA,NA,NA,NA))
testthat::expect_equal(a,b)

# ================
# = test v1m0 =
# ================
a = umxPath(v1m0 = "g")
b = list(mxPath("g", free = F, values = 1 , arrows = 2), mxPath("one", "g", free = F, values = 0))
testthat::expect_equal(a,b)

# ======================
# = Should throw error =
# ======================

# firstAt OR fixedAt
testthat::expect_error(umxPath("A", with = "B", fixedAt = 1, firstAt = 1))
testthat::expect_error(umxPath(cov = c("A")))

# fixing first loading requires one item in from 
# TODO allow more than one item in to?
testthat::expect_error(umxPath(letters[1:2], to = letters[5:9], firstAt = 1))

# Must be n==2 variables for cov
testthat::expect_error(umxPath(cov = c("a","b", "c")))
# From illegal for means
testthat::expect_error(umxPath(from = "z", means=c("a","b", "c")))

# only 1 of with var cov unique.bivaraite and  means can be set
testthat::expect_error(umxPath(with = "A", unique.bivariate = c("A","B"), to = "A", cov = "A", means = "A", fixedAt = 1, firstAt = 1))
testthat::expect_error(umxPath(with = "A", means = "A", fixedAt = 1, firstAt = 1))


# m1@output$matrices               
# m1@output$algebras
# 	m1@output$algebras$m1.fitfunction
# 	attr(m1@output$algebras$m1.fitfunction,"expCov")
# 	attr(,"expCov")
# 	attr(,"expMean")
# 	attr(,"likelihoods")
# 	m1@output$algebras$m1.fitfunction
# m1@output$expectations
# 	attr(,"UnfilteredExpCov")
# 	attr(,"numStats")
# m1@output$data
# 	500
# m1@output$calculatedHessian
# m1@output$gradient               
# m1@output$hessian
# m1@output$computes
# 	[[1]]
# 	[1] 2
# 
# 	[[2]]
# 	[[2]]$output
# 	[[2]]$output$probeCount
# 	[1] 360
# m1@output$fit # [1] 3693.424
# m1@output$Minus2LogLikelihood # ] 3693.424
# m1@output$minimum # 3693.424
# m1@output$estimate
# m1@output$standardErrors
# m1@output$infoDefinite
# m1@output$conditionNumber
# m1@output$status                 
# # synonyms?
# 	m1@output$iterations
# 	m1@output$evaluations
# m1@output$mxVersion
# m1@output$frontendTime
# m1@output$backendTime            
# m1@output$independentTime
# m1@output$wallTime
# m1@output$timestamp
# m1@output$cpuTime
# m1@output$IndependenceLikelihood
# m1@output$SaturatedLikelihood
