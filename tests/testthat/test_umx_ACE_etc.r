# library(testthat); library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_ACE_etc.r") 
# TODO: Switch on ACE and ACEv examples for testing
# test_package("umx")

test_that("test sep enforcement", {
	# 1. Test sep enforcement
	require(umx)
	data(GFF)
	mzData = subset(GFF, zyg_2grp == "MZ")
	dzData = subset(GFF, zyg_2grp == "DZ")
	selDVs = c("gff","fc","qol","hap","sat","AD") # These will be expanded into "gff_T1" "gff_T2" etc.
	m1 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)
	# m2 = umxIPold(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData)

	# expect_error(m1 = umxIP(selDVs = selDVs, dzData = dzData, mzData = mzData),"Please use sep")
	
	# Use "marginals" method to enable all continuous data with missingness.
	m3 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzData, mzData = mzData, type = "DWLS", allContinuousMethod= 'marginals')
	# omit missing to enable default WLS method to work on all continuous data
	dzD = na.omit(dzData[, tvars(selDVs, "_T")])
	mzD = na.omit(dzData[, tvars(selDVs, "_T")])
	m4 = umxIP(selDVs = selDVs, sep = "_T", dzData = dzD, mzData = mzD, type = "DWLS")
})

test_that("umxACE univariate", {
	require(umx)

	 # ============================
	 # = How heritable is height? =
	 # ============================
	 data(twinData) # ?twinData from Australian twins.
	 # Pick the variables
	 # 1. Height has a tiny variance, and this makes solution finding very hard.
	 # We'll scale height up by 10x to make the Optimizer's task easier.
	 twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
	 
	 # 2. umxACE picks the variables it needs from the data.
	 mzData = twinData[twinData$zygosity %in% "MZFF", ]
	 dzData = twinData[twinData$zygosity %in% "DZFF", ]
	 
	 # 3. umxACE can figure out variable names using sep: 
	 #    e.g. selVars = "wt" + sep= "_T" -> "wt_T1" "wt_T2"
	 m1 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
	 
	 # tip: with report = "html", umxSummary can print the table to your browser!
	 umxSummary(m1, std = FALSE) # un-standardized
	 # tip: plot gives a figure of the model and parameters
	 # plot(m1)
	 
	
	 # =============================================================
	 # = ADE: Evidence for dominance ? (DZ correlation set to .25) =
	 # =============================================================
	 m2 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
	 umxCompare(m2, m1) # ADE is better
	 umxSummary(m2, comparison = m1) 
	 # nb: Although summary is smart enough to print d, the underlying 
	 #     matrices are still called a, c & e.
	
	 # ===================================================
	 # = WLS example using diagonal weight least squares =
	 # ===================================================
	 m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, type = "DWLS", allContinuousMethod = 'marginals')
	
 })
	

 test_that("umxACE wt ht", {
	 # ==============================
	 # = Univariate model of weight =
	 # ==============================
	
	 # Things to note:
	 
	 # 1. Weight has a large variance, and this makes solution finding very hard.
	 # Here, we scale wt to make the Optimizer's task easier.
	 require(umx)
	 data(twinData)
	 tmp = umx_residualize(c("wt", "ht"), cov = "age", suffixes= c(1, 2), data = twinData)
	 mzData = tmp[tmp$zygosity %in% "MZFF", ]
	 dzData = tmp[tmp$zygosity %in% "DZFF", ]
	 
	 # You might also want to explore re-scaling the variable
	 # tmp = twinData$wt1[!is.na(twinData$wt1)]
	 # car::powerTransform(tmp, family="bcPower"); hist(tmp^-0.6848438)
	 # twinData$wt1 = twinData$wt1^-0.6848438
	 # twinData$wt2 = twinData$wt2^-0.6848438
	 
	 # 4. note: the default boundDiag = 0 lower-bounds a, c, and e at 0.
	 #    Prevents mirror-solutions. If not desired: set boundDiag = NULL.
	
	 m2 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = NULL)
	
	 # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
	 m1 = umxACE(selDVs = "wt", sep = "", data = twinData, dzData = c("DZMM", "DZFF", "DZOS"), mzData = c("MZMM", "MZFF"))
	 # |   |   a1|c1 |   e1|
	 # |:--|----:|:--|----:|
	 # |wt | 0.93|.  | 0.38|
	
	 require(umx)
	 # MODEL MODIFICATION
	 # We can modify this model, e.g. test shared environment. 
	 # Set comparison to modify, and show effect in one step.
	 
	 m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
	 # nb: You can see names of free parameters with parameters(m2)
	
 })

 test_that("umxACE bivariate", {
 	require(umx)
	 # =========================================================
	 # = Well done! Now you can make modify twin models in umx =
	 # =========================================================
	
	 # =====================================
	 # = Bivariate height and weight model =
	 # =====================================
	 data(twinData)
	 # We'll scale height (ht1 and ht2) and weight
	 twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
	 mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
	 dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
	 mzData = mzData[1:80,] # quicker run to keep CRAN happy
	 dzData = dzData[1:80,]
	 m1 = umxACE(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
	 umxSummary(m1)
	
 })

 test_that("umxACE ordinal", {
 	require(umx)
	 # ===================
	 # = Ordinal example =
	 # ===================
	 data(twinData)
	 twinData= umx_scale_wide_twin_data(data=twinData,varsToScale=c("wt"),sep="")
	 # Cut BMI column to form ordinal obesity variables
	 obLevels = c('normal', 'overweight', 'obese')
	 cuts = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
	 twinData$obese1=cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels)
	 twinData$obese2=cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels)
	 # Make the ordinal variables into umxFactors
	 ordDVs = c("obese1", "obese2")
	 twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
	 mzData = twinData[twinData$zygosity %in% "MZFF", ]
	 dzData = twinData[twinData$zygosity %in% "DZFF", ]
	 mzData = mzData[1:80, ] # Just top 80 pairs to run fast
	 dzData = dzData[1:80, ]
	 str(mzData) # make sure mz, dz, and t1 and t2 have the same levels!
	 
	 # Data-prep done - here's the model and summary!:
	 m1 = umxACE(selDVs = "obese", dzData = dzData, mzData = mzData, sep = '')
	 # umxSummary(m1)
	
 })

 test_that("umxACE bivariate and ordinal", {
 	require(umx)
	
	 # ============================================
	 # = Bivariate continuous and ordinal example =
	 # ============================================
	 data(twinData)
	 twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
	 # Cut BMI column to form ordinal obesity variables
	 obLevels   = c('normal', 'overweight', 'obese')
	 cuts       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
	 twinData$obese1=cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
	 twinData$obese2=cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
	 # Make the ordinal variables into mxFactors
	 ordDVs = c("obese1", "obese2")
	 twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
	 mzData = twinData[twinData$zygosity %in% "MZFF",] 
	 dzData = twinData[twinData$zygosity %in% "DZFF",]
	 mzData = mzData[1:80,] # just top 80 so example runs in a couple of secs
	 dzData = dzData[1:80,]
	 m1 = umxACE(selDVs= c("wt","obese"), dzData= dzData, mzData= mzData, sep='')
	 
 })

 test_that("umxACE bivariate continuous", {
 	require(umx)
	 # =======================================
	 # = Mixed continuous and binary example =
	 # =======================================
	 data(twinData)
	 twinData= umx_scale_wide_twin_data(data= twinData,varsToScale= "wt", sep="")
	 # Cut to form category of 20% obese subjects
	 # and make into mxFactors (ensure ordered is TRUE, and require levels)
	 obLevels   = c('normal', 'obese')
	 cuts       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
	 twinData$obese1= cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
	 twinData$obese2= cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
	 ordDVs = c("obese1", "obese2")
	 twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
	 
	 selDVs = c("wt", "obese")
	 mzData = twinData[twinData$zygosity %in% "MZFF",]
	 dzData = twinData[twinData$zygosity %in% "DZFF",]
	 m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
	 umxSummary(m1)
	
	 # ===================================
	 # Example with covariance data only =
	 # ===================================
	
	 data(twinData)
	 twinData= umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
	 selDVs = c("wt1", "wt2")
	 mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
	 dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
	 m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=569, numObsMZ=351)
	 umxSummary(m1)
	 plot(m1)
	
})

test_that("umxGxE", {
	require(umx)
	data(twinData) 
	twinData$age1 = twinData$age2 = twinData$age
	selDVs  = "bmi"
	selDefs = "age"
	mzData  = subset(twinData, zygosity == "MZFF")[1:100,]
	dzData  = subset(twinData, zygosity == "DZFF")[1:100,]
	m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep= "", dzData= dzData, mzData= mzData, tryHard= "yes")
	
	# Select the data on the fly with data= and zygosity levels
	m1 = umxGxE(selDVs= "bmi", selDefs= "age", sep="", dzData= "DZFF", mzData= "MZFF", data= twinData)
})

test_that("umxGxE_window", {
	require(umx)
	data(twinData) # Dataset of Australian twins, built into OpenMx
	twinData = twinData[!is.na(twinData["age"]), ]
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")

	m1 = umxGxE_window(selDVs = "bmi", moderator = "age", sep= "", mzData = mzData, dzData = dzData, target = 40, plotWindow = TRUE)
	m1 = umxGxE_window(selDVs = "bmi", moderator = "age", sep= "", mzData = mzData, dzData = dzData, plotWindow = TRUE, tryHard = "yes")

})

test_that("test umxACEv xmu_standardize_ACE xmu_standardize_ACEv", {
	require(umx)
	data(twinData)
	mzData = twinData[twinData$zygosity %in% "MZFF", ]
	dzData = twinData[twinData$zygosity %in% "DZFF", ]
	m1     = umxACE(selDVs = "bmi", sep="", dzData = dzData, mzData = mzData)
	std    = xmu_standardize_ACE(m1)
	m1     = umxACEv(selDVs = "bmi", sep="", dzData = dzData, mzData = mzData)
	std    = umx_standardize(m1)
	umxPlotACEv(m1, std = FALSE) # Don't standardize
	plot(m1, std = FALSE) # don't standardize
	
})


#' require(umx)
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' 
#' # 1. Height in meters has a tiny variance, and this makes optimising hard.
#' #    We'll scale it by 10x to make the Optimizer's task easier.
#' data(twinData) # ?twinData from Australian twins.
#' twinData[, c("ht1", "ht2")] = twinData[, c("ht1", "ht2")] * 10
#'
#' # 2. Make mz & dz data.frames (no need to select variables: umx will do this)
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # 3. Built & run the model, controlling for age in the means model
#' m1 = umxACE(selDVs = "ht", selCovs = "age", sep = "", dzData = dzData, mzData = mzData)
#'
#' # sidebar: umxACE figures out variable names using sep: 
#' #    e.g. selVars = "wt" + sep= "_T" -> "wt_T1" "wt_T2"
#' 
#' umxSummary(m1, std = FALSE) # un-standardized
#'
#' # tip 1: set report = "html" and umxSummary prints the table to your browser!
#' # tip 2: plot works for umx: Get a figure of the model and parameters
#' # plot(m1) # Also, look at the options for ?plot.MxModel.
#' 
#' # ===========================================
#' # = Test ADE, AE, CE, E, and generate table =
#' # ===========================================
#'
#' umxReduce(m1, report="html", silent= TRUE)
#'
#' # ============================
#' # = Model, with 2 covariates =
#' # ============================
#'
#' # Create another covariate: cohort
#' twinData$cohort1 = twinData$cohort2 =twinData$part
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#'
#' # 1. def var approach
#' m2 = umxACE(selDVs = "ht", selCovs = c("age", "cohort"), sep = "", dzData = dzData, mzData = mzData)
#'
#' # 2. Residualized approach: remove height variance accounted-for by age.
#' FFdata = twinData[twinData$zygosity %in% c("MZFF", "DZFF"), ]
#' FFdata = umx_residualize("ht", "age", suffixes = 1:2, data = FFdata)
#' mzData = FFdata[FFdata$zygosity %in% "MZFF", ]
#' dzData = FFdata[FFdata$zygosity %in% "DZFF", ]
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
#'
#' # =============================================================
#' # = ADE: Evidence for dominance ? (DZ correlation set to .25) =
#' # =============================================================
#' m2 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' umxCompare(m2, m1) # ADE is better
#' umxSummary(m2, comparison = m1) 
#' # nb: Although summary is smart enough to print d, the underlying 
#' #     matrices are still called a, c & e.
#'
#' # tip: try umxReduce(m1) to automatically build and compare ACE, ADE, AE, CE
#' # including conditional probabilities!
#'
#' # ===================================================
#' # = WLS example using diagonal weight least squares =
#' # ===================================================
#'
#' m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, 
#' 	type = "DWLS", allContinuousMethod='marginals'
#' )
#'
#'
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#'
#' # Things to note:
#' 
#' # 1. Weight has a large variance, and this makes solution finding very hard.
#' # Here, we residualize the data for age, which also scales weight and height.
#'
#' data(twinData)
#' tmp = umx_residualize(c("wt", "ht"), cov = "age", suffixes= c(1, 2), data = twinData)
#' mzData = tmp[tmp$zygosity %in% "MZFF", ]
#' dzData = tmp[tmp$zygosity %in% "DZFF", ]
#' 
#' # tip: You might also want transform variables
#' # tmp = twinData$wt1[!is.na(twinData$wt1)]
#' # car::powerTransform(tmp, family="bcPower"); hist(tmp^-0.6848438)
#' # twinData$wt1 = twinData$wt1^-0.6848438
#' # twinData$wt2 = twinData$wt2^-0.6848438
#' 
#' # 4. note: the default boundDiag = 0 lower-bounds a, c, and e at 0.
#' #    Prevents mirror-solutions. If not desired: set boundDiag = NULL.
#'
#' m2 = umxACE(selDVs = "wt", dzData = dzData, mzData = mzData, sep = "", boundDiag = NULL)
#'
#' # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
#' m1 = umxACE(selDVs = "wt", sep = "", data = twinData,
#' 	dzData = c("DZMM", "DZFF", "DZOS"), mzData = c("MZMM", "MZFF"))
#' # |   |   a1|c1 |   e1|
#' # |:--|----:|:--|----:|
#' # |wt | 0.93|.  | 0.38|
#'
#' # tip: umx_make_twin_data_nice() will make data into this nice format for you!
#' 
#' # ======================
#' # = MODEL MODIFICATION =
#' # ======================
#' # We can modify this model, e.g. test shared environment. 
#' # Set comparison to modify, and show effect in one step.
#' 
#' m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
#' #*tip* call umxModify(m1) with no parameters, and it will print the labels available to fix!
#' # nb: You can see parameters of any model with parameters(m1)
#'
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' data(twinData)
#' # We'll scale height (ht1 and ht2) and weight
#' twinData = umx_scale_wide_twin_data(data = twinData, varsToScale = c("ht", "wt"), sep = "")
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"),]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' m1 = umxACE(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
#' umxSummary(m1)
#'
#' # ===================
#' # = Ordinal example =
#' # ===================
#' 
#' # Prep data
#' require(umx)
#' data(twinData)
#' # Cut BMI column to form ordinal obesity variables
#' obLevels = c('normal', 'overweight', 'obese')
#' cuts = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels)
#' 
#' # Make the ordinal variables into umxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' # Model and summary!
#' m1 = umxACE(selDVs = "obese", dzData = dzData, mzData = mzData, sep = '')
#'
#' # And controlling age (otherwise manifests appearance as latent C)
#' m1 = umxACE(selDVs = "obese", selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#' # umxSummary(m1)
#'
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData,varsToScale="wt",sep= "")
#' # Cut BMI column to form ordinal obesity variables
#' obLevels   = c('normal', 'overweight', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1=cut(twinData$bmi1,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' twinData$obese2=cut(twinData$bmi2,breaks=c(-Inf,cuts,Inf),labels=obLevels)
#' # Make the ordinal variables into mxFactors
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' mzData = twinData[twinData$zygosity %in% "MZFF",] 
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' mzData = mzData[1:80,] # just top 80 so example runs in a couple of secs
#' dzData = dzData[1:80,]
#' m1 = umxACE(selDVs= c("wt","obese"), dzData= dzData, mzData= mzData, sep='')
#' 
#' # And controlling age
#' m1 = umxACE(selDVs = c("wt","obese"), selCov= "age", dzData = dzData, mzData = mzData, sep = '')
#'
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data= twinData,varsToScale= "wt", sep="")
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' obLevels   = c('normal', 'obese')
#' cuts       = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' twinData$obese1= cut(twinData$bmi1, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' twinData$obese2= cut(twinData$bmi2, breaks=c(-Inf,cuts,Inf), labels=obLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF",]
#' dzData = twinData[twinData$zygosity %in% "DZFF",]
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#'
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#'
#' require(umx)
#' data(twinData)
#' twinData= umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
#' selDVs = c("wt1", "wt2")
#' mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
#' m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=569, numObsMZ=351)
#' umxSummary(m1)
#' plot(m1)

#' 
#' # ==============================
#' # = Univariate model of weight =
#' # ==============================
#' require(umx)
#' data(twinData) # ?twinData from Australian twins.
#'
#' # Things to note: ACE model of weight will return a NEGATIVE variance in C.
#' #  This is exactly why we have ACEv! It suggests we need a different model
#' #  In this case: ADE.
#' # Other things to note:
#' # 1. umxACEv can figure out variable names: provide "sep", and selVars. 
#' #    Function generates: "wt" -> "wt1" "wt2"
#' # 2. umxACEv picks the variables it needs from the data.
#' 
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = "wt", sep = "", dzData = dzData, mzData = mzData)
#' 
#' # A short cut (which is even shorter for "_T" twin data with "MZ"/"DZ" data in zygosity column is:
#' m1 = umxACEv(selDVs = "wt", sep = "", dzData = "MZFF", mzData = "DZFF", data = twinData)
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = "wt", sep = "", dzData = dzData, mzData = mzData, dzCr = .25)
#' # note: the underlying matrices are still called A, C, and E.
#' # I catch this in the summary table, so columns are labeled A, D, and E.
#' # However, currently, the plot will say A, C, E.
#' 
#' # We can modify this model, dropping dominance component (still called C), 
#' # and see a comparison:
#' m3 = umxModify(m2, update = "C_r1c1", comparison = TRUE, name="AE")
#' # =========================================================
#' # = Well done! Now you can make modify twin models in umx =
#' # =========================================================
#' 
#' # ============================
#' # = How heritable is height? =
#' # ============================
#' # 
#' # Note: Height has a small variance. umx can typically picks good starts,
#' #    but scaling is advisable.
#' # 
#' require(umx)
#' # Load data and rescale height to cm (var in m too small)
#' data(twinData) # ?twinData from Australian twins.
#' twinData[,c("ht1", "ht2")]= twinData[,c("ht1", "ht2")]*100
#'
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' m1 = umxACEv(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData)
#' 
#' umxSummary(m1, std = FALSE) # unstandardized
#' plot(m1)
#'
#' # tip: with report = "html", umxSummary can print the table to your browser!
#' # tip: You can turn off auto-plot with umx_set_auto_plot(FALSE)
#' 
#' # ========================================================
#' # = Evidence for dominance ? (DZ correlation set to .25) =
#' # ========================================================
#' m2 = umxACEv("ADE", selDVs = "ht", dzCr = .25, sep="", dzData = dzData, mzData = mzData)
#' umxCompare(m2, m1) # Is ADE better?
#' umxSummary(m2, comparison = m1) # nb: though this is ADE, matrices are still called A,C,E
#'
#' # We can modify this model, dropping shared environment, and see a comparison:
#' m3 = umxModify(m2, update = "C_r1c1", comparison = TRUE, name = "AE")
#'
#' # =====================================
#' # = Bivariate height and weight model =
#' # =====================================
#' 
#' data(twinData)
#' twinData[,c("ht1", "ht2")]= twinData[,c("ht1", "ht2")]*100
#' mzData = twinData[twinData$zygosity %in% c("MZFF", "MZMM"), ]
#' dzData = twinData[twinData$zygosity %in% c("DZFF", "DZMM", "DZOS"), ]
#' mzData = mzData[1:80, ] # Quicker run to keep CRAN happy
#' dzData = dzData[1:80, ]
#' m1 = umxACEv(selDVs = c("ht", "wt"), sep = '', dzData = dzData, mzData = mzData)
#' 
#' # ===================
#' # = Ordinal example =
#' # ===================
#' require(umx)
#' data(twinData)
#'
#' # Cut bmi column to form ordinal obesity variables
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' obesityLevels = c('normal', 'overweight', 'obese')
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#'
#' # Make the ordinal variables into mxFactors (ensure ordered is TRUE, and require levels)
#' twinData[, c("obese1", "obese2")] = umxFactor(twinData[, c("obese1", "obese2")])
#' mzData = twinData[twinData$zygosity %in% "MZFF", ][1:80,] # 80 pairs for speed on CRAN
#' dzData = twinData[twinData$zygosity %in% "DZFF", ][1:80,]
#' m2 = umxACEv(selDVs = "obese", dzData = dzData, mzData = mzData, sep = '')
#'
#' # FYI: Show mz, dz, and t1 and t2 have the same levels!
#' str(mzData)
#' 
#' # ============================================
#' # = Bivariate continuous and ordinal example =
#' # ============================================
#' data(twinData)
#' # Cut bmi column to form ordinal obesity variables
#' ordDVs = c("obese1", "obese2")
#' obesityLevels = c('normal', 'overweight', 'obese')
#' cutPoints = quantile(twinData[, "bmi1"], probs = c(.5, .2), na.rm = TRUE)
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#'
#' # Make the ordinal variables into ordered mxFactors
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#'
#' # umxACEv can trim out unused variables on its own
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]
#' 
#' m1 = umxACEv(selDVs = c("wt", "obese"), dzData = dzData, mzData = mzData, sep = '')
#' plot(m1)
#' 
#' # =======================================
#' # = Mixed continuous and binary example =
#' # =======================================
#' require(umx)
#' data(twinData)
#' # Cut to form category of 20% obese subjects
#' # and make into mxFactors (ensure ordered is TRUE, and require levels)
#' cutPoints = quantile(twinData[, "bmi1"], probs = .2, na.rm = TRUE)
#' obesityLevels = c('normal', 'obese')
#' twinData$obese1 = cut(twinData$bmi1, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' twinData$obese2 = cut(twinData$bmi2, breaks = c(-Inf, cutPoints, Inf), labels = obesityLevels) 
#' ordDVs = c("obese1", "obese2")
#' twinData[, ordDVs] = umxFactor(twinData[, ordDVs])
#' 
#' selDVs = c("wt", "obese")
#' mzData = twinData[twinData$zygosity %in% "MZFF", ]
#' dzData = twinData[twinData$zygosity %in% "DZFF", ]

#' m1 = umxACEv(selDVs = selDVs, dzData = dzData, mzData = mzData, sep = '')
#' umxSummary(m1)
#' 
#' # ===================================
#' # Example with covariance data only =
#' # ===================================
#' 
#' require(umx)
#' data(twinData)
#' selDVs = c("wt")
#' mz = cov(twinData[twinData$zygosity %in% "MZFF", tvars(selDVs, "")], use = "complete")
#' dz = cov(twinData[twinData$zygosity %in% "DZFF", tvars(selDVs, "")], use = "complete")
#' m1 = umxACEv(selDVs = selDVs, sep= "", dzData = dz, mzData= mz, numObsDZ= 569, numObsMZ= 351)
#' umxSummary(m1, std = FALSE)