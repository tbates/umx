# Analyses for Eric 2016-09-04 04:13PM
umx_set_optimizer("NPSOL")

umxCovxE <- function(name = "MZ", selDVs, def = "def", data = NULL) {
	# You must have a variable called "def" in the dataset.
	# If there isn't one, I will rename the one you provide to "def"
	data$def = data[,def]
	data = data[,c(selDVs, "def")]	
	m1 <- umxRAM(name, data = data, remove_unused_manifests = FALSE,
		umxPath(v.m. = selDVs),
		umxPath(selDVs[1], with = selDVs[2], free = FALSE, label = "covXY[1,1]"),
		mxAlgebra(name = "covXY", XY + betaCov * data.def),
		mxMatrix(name  = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
		mxMatrix(name  = "betaCov", type = "Full", nrow = 1, ncol = 1, free = TRUE),
		# Means
		umxPath("means", to = selDVs),
		umxPath(def, to = selDVs, labels = c("beta_1", "beta_2")),
		umxPath(defn = def, labels = "data.def")
	)
}

# "/Users/tim/Dropbox/current/IQ/MIDUS/GxE IQ/QIMR GxE/R/GxSES IQ QIMR2.R"
# c("FSIQz", "VSIQz", "PSIQz", "FSIQ")
twinData$SEI = twinData$ChildhoodSEIz_T1
suffix = "_T"
DVs = "FSIQz"
selDefs = "SEI"
selDVs  = umx_paste_names(DVs , suffix, 1:2)
selVars = c(selDVs, selDefs)
mzData  = subset(twinData, zyg %in% c("MZFF", "MZMM"), selVars) # MZs
dzData  = subset(twinData, zyg %in% c("DZFF", "DZMM", "DZFM", "DZMF"), selVars) # same and OS Sex DZs
# Exclude cases with missing Def
mzData <- mzData[!is.na(mzData[selDefs[1]]) & !is.na(mzData[selDefs[2]]),]
dzData <- dzData[!is.na(dzData[selDefs[1]]) & !is.na(dzData[selDefs[2]]),]

# ======
# = MZ =
# ======
m1 = umxCovxE("MZ", selDVs = selDVs, def = "SEI", data = mzData)
m2 = umxEquate(m1, master = "one_to_FSIQz_T1", slave = "one_to_FSIQz_T2", free = T, verbose = T, name = "equate means")
umxCompare(m1, m2) # p.001
m3 = umxEquate(m1, master = "beta_1", slave = "beta_2", name = "equate_mean_mod")
umxCompare(m1, m3) # p.514
m4 = umxModify(m1, update = c("beta_1", "beta_2"), name = "no_means_moderation", comparison = F)
umxCompare(m1, m4) # p.001
m5 = umxModify(m1, update = "MZ.betaCov[1,1]", name = "no_XY_moderation", comparison = F)
umxCompare(m1, m5) # p.042
coef(m3); plot(m3)


# ======
# = DZ =
# ======
d1 = umxCovxE("DZ", selDVs = selDVs, def = "SEI", data = dzData)
d2 = umxEquate(d1, master = "one_to_FSIQz_T1", slave = "one_to_FSIQz_T2", free = T, verbose = T, name = "equate means")
umxCompare(d1, d2) # p.186
d3 = umxEquate(d1, master = "beta_1", slave = "beta_2", name = "equate_mean_mod")
umxCompare(d1, d3) # p.456
d4 = umxModify(d1, update = c("beta_1", "beta_2"), name = "no_means_moderation", comparison = F)
umxCompare(d1, d4) # p.001
d5 = umxModify(d1, update = "DZ.betaCov[1,1]", name = "no_XY_moderation", comparison = F)
umxCompare(d1, d5) # p.664
coef(d3); plot(d2)
umxCompare(d1, d3) # looks fine
umxCompare(d1, d4) # ouch: Definately moderation of mean IQ by SES!