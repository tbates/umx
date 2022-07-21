# library(testthat)
# library(umx)
# test_active_file("~/bin/umx/tests/testthat/test_umxRAM_etc.r") 
# test_package("umx")
context("umxRAM()")

test_that("umxRAM handles two covariates", {
	# Test working with more than 1 covariate
	require(umx)
	data(twinData)
	twinData = umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
	twinData$cohort1 = twinData$cohort2 =twinData$part
	mzData = twinData[twinData$zygosity %in% "MZFF",]
	dzData = twinData[twinData$zygosity %in% "DZFF",]
	 expect_error(regexp = NA, {
	 	m1 = umxACE(selDVs = 'ht', selCovs = c("age","cohort"), sep = "", dzData = dzData, mzData = mzData)
	})
})

test_that("umxRAM works", {	
	require(umx)
	data(demoOneFactor) # from OpenMx
	latents  = c("G")
	manifests = names(demoOneFactor)
	m1 = umxRAM("RAM", data = mxData(cov(demoOneFactor), type = "cov", numObs = 500),
		umxPath(latents, to = manifests),
		umxPath(var = manifests),
		umxPath(var = latents, fixedAt = 1.0)
	)

	m2 = mxRun(mxModel("NOT_RAM",
		mxMatrix("Full", 5, 1, values = 0.2, free = TRUE , name = "A"), 
		mxMatrix("Symm", 1, 1, values = 1  , free = FALSE, name = "L"), 
		mxMatrix("Diag", 5, 5, values = 1  , free = TRUE , name = "U"), 
		mxAlgebra(A %*% L %*% t(A) + U, name = "R"), 
		mxExpectationNormal("R", dimnames = names(demoOneFactor)),
		mxFitFunctionML(),
		mxData(cov(demoOneFactor), type = "cov", numObs = 500)
	))
})

test_that("testing umx_is_RAM", {
	expect_equal(umx_is_RAM(m1), TRUE)
	expect_equal(umx_is_RAM(m2), FALSE)
})

test_that("testing umxModel", {
	expect_error(umxModel(), regexp = "You probably meant umxRAM")
})

test_that("testing Rsw data", {
	 # ============================================
	 # = 1. Here's a simple example with raw data =
	 # ============================================
	 mtcars$litres = mtcars$disp/61.02
	 m1 = umxRAM("tim", data = mtcars,
	 	umxPath(c("wt", "litres"), to = "mpg"),
	 	umxPath("wt", with = "litres"),
	 	umxPath(v.m. = c("wt", "litres", "mpg"))
	 )
	
	 # 2. Use parameters to see the parameter estimates and labels
	 parameters(m1)
	
	 # And umxSummary to get standardized parameters, CIs etc from the run model.
	 umxSummary(m1, std=TRUE)
	 # |name           | Std.Estimate| Std.SE|CI                   |
	 # |:--------------|------------:|------:|:--------------------|
	 # |wt_to_mpg      |        -0.54|   0.17|-0.54 [-0.89, -0.2]  |
	 # |disp_to_mpg    |        -0.36|   0.18|-0.36 [-0.71, -0.02] |
	 # |mpg_with_mpg   |         0.22|   0.07|0.22 [0.08, 0.35]    |
	 # |wt_with_wt     |         1.00|   0.00|1 [1, 1]             |
	 # |b1             |         0.89|   0.04|0.89 [0.81, 0.96]    |
	 # |disp_with_disp |         1.00|   0.00|1 [1, 1]             |
	 
	 # 3. Of course you can plot the model
	 plot(m1)
	 plot(m1, std = TRUE, means=FALSE)
	 plot(m1, std = TRUE, means=FALSE, strip= TRUE, resid = "line")
	
	 # ====================================
	 # = A cov model, with steps laid out =
	 # ====================================
	
	 # *note*: The variance of displacement is in cubic inches and is very large.
	 # to help the optimizer, one might, say, multiply disp *.016 to work in litres
	 tmp = mtcars; tmp$disp= tmp$disp *.016
	
	 # We can just give the raw data and ask for it to be made into type cov:
	 m1 = umxRAM("tim", data = tmp, type="cov",
	 	umxPath(c("wt", "disp"), to = "mpg"),
	 	umxPath("wt", with = "disp"),
	 	umxPath(var = c("mpg", "wt", "disp"))
	 )

	 # ===============================================
	 # = lavaan string example (more at ?umxLav2RAM) =
	 # ===============================================
	 m1 = umxRAM(data = tmp, "#modelName
	  mpg ~ wt + disp")
	 
	
	 # =======================
	 # = A multi-group model =
	 # =======================
	
	 mtcars$litres = mtcars$disp/61.02
	 m1 = umxRAM("tim", data = mtcars, group = "am",
	 	umxPath(c("wt", "litres"), to = "mpg"),
	 	umxPath("wt", with = "litres"),
	 	umxPath(v.m. = c("wt", "litres", "mpg"))
	 )
	 # In this model, all parameters are free across the two groups.
	
	
	 # (see ?umxPath for more nifty options making paths...)
	
	 # =========================================
	 # = umxRAM can also accept mxData as data =
	 # =========================================
	 # For convenience, list up the manifests you will be using
	 
	 selVars = c("mpg", "wt", "disp")
	 tmp     = mtcars; tmp$disp= tmp$disp *.016
	 myCov   = mxData(cov(tmp[, selVars]), type = "cov", numObs = nrow(mtcars) )
	
	 m1 = umxRAM("tim", data = myCov,
	 	umxPath(c("wt", "disp"), to = "mpg"),
	 	umxPath("wt", with = "disp"),
	 	umxPath(var = selVars)
	 )
	 
	 
	 # =======================
	 # = umxRAM supports WLS =
	 # =======================
	
	 # 1. Run an all-continuous WLS model
	  mw = umxRAM("raw", data = tmp[, c("mpg", "wt", "disp")], type = "WLS", allContinuousMethod = "cumulants",
	  		umxPath(var = c("wt", "disp", "mpg")),
			umxPath(c("wt", "disp"), to = "mpg"),
	  		umxPath("wt", with = "disp"),
	      	umxPath(var = c("wt", "disp", "mpg"))
	  )
	 # 2. Switch to marginals to support means
	  mw = umxRAM("raw", data = tmp[, c("mpg", "wt", "disp")], type = "WLS", allContinuousMethod= "marginals",
	  		umxPath(var = c("wt", "disp", "mpg")),
			umxPath(c("wt", "disp"), to = "mpg"),
			umxPath("wt", with = "disp"),
			umxPath(v.m. = c("wt", "disp", "mpg"))
	  )
	
	 
	 # ===============================
	 # = Using umxRAM in Sketch mode =
	 # ===============================
	 # No data needed: just list variable names!
	 # Resulting model will be plotted automatically
	 m1 = umxRAM("what does unique pairs do, I wonder", data = c("A", "B", "C"),
		   umxPath(unique.pairs = c("A", "B", "C"))
	 )
	 
	 m1 = umxRAM("ring around the rosey", data = c("B", "C"),
		  umxPath(fromEach = c("A", "B", "C"))
	 )
	 
	 m1 = umxRAM("fromEach with to", data = c("B", "C"),
		   umxPath(fromEach = c("B", "C"), to= "D")
	 )
	
	 m1 = umxRAM("CFA_sketch", data = paste0("x", 1:4),
	 	umxPath("g", to = paste0("x", 1:4)),
	 	umxPath(var = paste0("x", 1:4)),
	 	umxPath(v1m0 = "g")
	 )
	
	 # =================================================
	 # = This is an example of using your own labels:  =
	 #   umxRAM will not over-ride them                =
	 # =================================================
	 m1 = umxRAM("tim", data = mtcars, type="cov",
	 	umxPath(c("wt", "disp"), to = "mpg"),
	 	umxPath(cov = c("wt", "disp"), labels = "b1"),
	 	umxPath(var = c("wt", "disp", "mpg"))
	 )
	 omxCheckEquals(m1$S$labels["disp", "wt"], "b1") # label preserved
	 m1$S$labels
	#      mpg             wt            disp
	# mpg  "mpg_with_mpg"  "mpg_with_wt" "disp_with_mpg"
	# wt   "mpg_with_wt"   "wt_with_wt"  "b1"
	# disp "disp_with_mpg" "b1"          "disp_with_disp"
	 parameters(m1)
 })

