data(myFADataRaw, package = "OpenMx")
manifests = paste0("x", 1:3)
latents = c("A", "B")
df = myFADataRaw[, manifests]

m1 <- mxModel("m1", 
	type = "RAM", 
	latentVars = latents,
	manifestVars = manifests,
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(means = manifests),
	umxPath(means = latents, fixedAt = 0),
	umxPath(var = manifests),
	umxPath(var = latents, fixedAt = 1),
	mxData(df, "raw")
)
m1 = umxRun(m1, setLabels = T, setValues = T)
umxSummary(m1, show = "raw"); plot(m1, showFixed = T)
umx_show(m1, what = "values", matrices = "M")
umx_show(m1, what = "free")

m1@output$matrices               
m1@output$algebras
	m1@output$algebras$m1.fitfunction
	attr(m1@output$algebras$m1.fitfunction,"expCov")
	attr(,"expCov")
	attr(,"expMean")
	attr(,"likelihoods")
	m1@output$algebras$m1.fitfunction
m1@output$expectations
	attr(,"UnfilteredExpCov")
	attr(,"numStats")
m1@output$data
	500
m1@output$calculatedHessian
m1@output$gradient               
m1@output$hessian
m1@output$computes
	[[1]]
	[1] 2

	[[2]]
	[[2]]$output
	[[2]]$output$probeCount
	[1] 360
m1@output$fit # [1] 3693.424
m1@output$Minus2LogLikelihood # ] 3693.424
m1@output$minimum # 3693.424
m1@output$estimate
m1@output$standardErrors
m1@output$infoDefinite
m1@output$conditionNumber
m1@output$status                 
# synonyms?
	m1@output$iterations
	m1@output$evaluations
m1@output$mxVersion
m1@output$frontendTime
m1@output$backendTime            
m1@output$independentTime
m1@output$wallTime
m1@output$timestamp
m1@output$cpuTime
m1@output$IndependenceLikelihood
m1@output$SaturatedLikelihood


df = myFADataRaw
m1 = umxRAM("tim", endog.variances = TRUE, data = mxData(df, "raw"),
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(var = latents, fixedAt = 1),  
	umxPath(means = latents, fixedAt = 0)
)
m1 = umxRun(m1); AIC(m1)
umxSummary(m1, show = "raw"); plot(m1, showFixed = T)


m1 = umxRAM("tim", endog.variances = TRUE, data = myFADataRaw,
	umxPath("B", to = manifests),
	umxPath("A", with = "B", fixedAt = 1),
	umxPath(var = latents, fixedAt = 1),  
	umxPath(means = latents, fixedAt = 0)
)
m1 = umxRun(m1); AIC(m1)

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
