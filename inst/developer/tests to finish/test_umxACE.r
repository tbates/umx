# library(testthat)
# library(umx)
# testthat::test_file("~/bin/umx/tests/testthat/test_umx_is_cov.r")
# 
# test_package("umx")

# ==================
# = Test umxACEcov =
# ==================

require(umx)
data(twinData)
tmpTwin <- twinData
tmpTwin$age1 = tmpTwin$age2 = tmpTwin$age
tr(twinData)
selDVs  = c("bmi")
selCovs = c("age")
selVars = umx_paste_names(c(selDVs, selCovs), textConstant = "", suffixes= 1:2)

# just top 200 so example runs in a couple of secs
mzData = subset(tmpTwin, zyg == 1, selVars)[1:200, ]
dzData = subset(tmpTwin, zyg == 3, selVars)[1:200, ]
# TODO update for new dataset variable zygosity
# mzData = subset(tmpTwin, zygosity == "MZFF", selVars)[1:200, ]
# dzData = subset(tmpTwin, zygosity == "DZFF", selVars)[1:200, ]
m1 = umxACEcov(selDVs = selDVs, selCovs = selCovs, dzData = dzData, mzData = mzData, suffix = "", autoRun = TRUE)
plot(m1)
umxSummary(m1, showStd = TRUE)

# ===============
# = Test umxACE =
# ===============

tmpTwin <- twinData
# Set zygosity to a factor
labList = c("MZFF", "MZMM", "DZFF", "DZMM", "DZOS")
tmpTwin$zyg = factor(tmpTwin$zyg, levels = 1:5, labels = labList)
selDVs = c("bmi1", "bmi2") # nb: Can also give base name, (i.e., "bmi") AND set suffix.
# the function will then make the varnames for each twin using this:
# for example. "VarSuffix1" "VarSuffix2"
mzData <- tmpTwin[tmpTwin$zyg %in% "MZFF", selDVs]
dzData <- tmpTwin[tmpTwin$zyg %in% "DZFF", selDVs]
mzData <- mzData[1:200,] # just top 200 so example runs in a couple of secs
dzData <- dzData[1:200,]
m1 = umxACE(selDVs = selDVs, dzData = dzData, mzData = mzData)
umxSummary(m1)
plot(m1)