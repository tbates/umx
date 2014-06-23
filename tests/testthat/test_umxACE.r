#' devtools::check_doc("~/bin/umx"); devtools::install("~/bin/umx");
#' devtools::check_doc("~/bin/umx.twin"); devtools::install("~/bin/umx.twin");
#' require(OpenMx)
#' data(twinData)
#' names(twinData) # "fam"   "age"   "zyg"   "part"  "wt1"   "wt2"   "ht1"   "ht2"   "htwt1" "htwt2" "bmi1"  "bmi2"  "ZYG"
#' twinData$ZYG = factor(twinData$zyg, levels = 1:5, labels = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS"))
#' twinData$age1 = twinData$age2 = twinData$age
#' selDVs = c("bmi1","bmi2")
#' mzData <- subset(twinData, ZYG == "MZFF", selDVs)
#' dzData <- subset(twinData, ZYG == "DZFF", selDVs)
#' m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m1 = umxRun(m1); umxSummaryACE(m1)
#' m2 = umxACEcov(selDVs = selDVs, dzData = dzData, mzData = mzData)
#' m2 = umxRun(m2); umxSummaryACE(m2)

#' selDVs   = c("bmi")
#' selCovs  = c("age", "ht", "wt")
#' suffixes = c("1", "2")
#' selVars = umx_suffix(c(selDVs, selCovs), suffixes)			   
#' twinData = umxPadAndPruneForDefVars(df = twinData, varNames = selDVs, defNames = selCovs, suffixes = suffixes, highDefValue = 99, rm = "pad_with_mean")
#' mzData <- subset(twinData, ZYG == "MZFF", selVars) # names(mzData)
#' dzData <- subset(twinData, ZYG == "DZFF", selVars)
#' m3 = umxACEcov(selDVs = selDVs, dzData = dzData, mzData = mzData, covariates = selCovs, suffixes = suffixes, equateMeans = FALSE)
#' m3 = umxRun(m3); umxSummaryACE(m3)
#' b = m3@submodels$top@matrices$betaWeights; cbind(b@labels, round(b@values,2))
