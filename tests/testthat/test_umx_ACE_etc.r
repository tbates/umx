# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_ACE_etc.r") 
# 
# test_package("umx")

test_that("testing umx twin models", {
	testthat::skip_on_cran() 

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
	 # ============================
	 # = How heritable is height? =
	 # ============================
	 require(umx)
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
	 m3 = umxACE(selDVs = "ht", sep = "", dzData = dzData, mzData = mzData, 
	 	type = "DWLS", allContinuousMethod='marginals'
	 )
	
	 # ==============================
	 # = Univariate model of weight =
	 # ==============================
	
	 # Things to note:
	 
	 # 1. Weight has a large variance, and this makes solution finding very hard.
	 # Here, we scale wt to make the Optimizer's task easier.
	
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
	 m1 = umxACE(selDVs = "wt", sep = "", data = twinData,
	 	dzData = c("DZMM", "DZFF", "DZOS"), mzData = c("MZMM", "MZFF"))
	 # |   |   a1|c1 |   e1|
	 # |:--|----:|:--|----:|
	 # |wt | 0.93|.  | 0.38|
	
	 # MODEL MODIFICATION
	 # We can modify this model, e.g. test shared environment. 
	 # Set comparison to modify, and show effect in one step.
	 
	 m2 = umxModify(m1, update = "c_r1c1", name = "no_C", comparison = TRUE)
	 # nb: You can see names of free parameters with parameters(m2)
	
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
	
	 # ===================
	 # = Ordinal example =
	 # ===================
	 require(umx)
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
	 
	 # =======================================
	 # = Mixed continuous and binary example =
	 # =======================================
	 require(umx)
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
	
	 require(umx)
	 data(twinData)
	 twinData= umx_scale_wide_twin_data(data=twinData, varsToScale= "wt", sep="")
	 selDVs = c("wt1", "wt2")
	 mz = cov(twinData[twinData$zygosity %in%  "MZFF", selDVs], use = "complete")
	 dz = cov(twinData[twinData$zygosity %in%  "DZFF", selDVs], use = "complete")
	 m1 = umxACE(selDVs=selDVs, dzData=dz, mzData=mz, numObsDZ=569, numObsMZ=351)
	 umxSummary(m1)
	 plot(m1)
	
})
