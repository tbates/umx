require(umx)
umx_set_optimizer("NPSOL")

data(myFADataRaw)
umx_set_optimizer("NPSOL")
umx_set_cores(4)
manifests = c("x1", "x2", "x3")
latents   = c("F1")
all       = c(manifests, latents)

# Grab  normal data z1:3 from myFADataRaw
df     = myFADataRaw[, manifests]
df_z   = scale(df)
df_int = umx_round(df,0)
df_ord = umxFactor(df_int)
df_cen = df_ord
df_cen[df_cen < 2] <- 2
df_cen = droplevels(df_cen)
table(df_cen$x1, useNA = "alw")
levels(df_tail$x1) <- seq(0, length(levels(df_tail$x1))-1)
plot(df_tail$x1)
df_tail = df_ord
df_tail[df_tail < 3] <- 3
df_tail = droplevels(df_tail)

# ==============
# = Continuous =
# ==============
m_cont <- umxRAM("continuous", data = mxData(df, type = "raw"),
	umxPath("F1", to = manifests),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
plot(m_cont, std=T)
# =============================================
# = Integer throws away fit with imprecision =
# =============================================
m_int = mxRun(mxModel(m_cont, name = "int", mxData(df_int, type = "raw")))
m_z   = mxRun(mxModel(m_cont, name = "z"  , mxData(df_z, type = "raw")))
umxCompare(m_cont, c(m_z, m_int))

# ====================================
# = ==========  Ordinal  =============
# ====================================

# ===============================
# =   Use umxThresholdMatrix    =
# ===============================
m_ord <- umxRAM("ordinal", data = mxData(df_ord, type = "raw"), thresholds = "ignore",
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
m_dir = umxRAM2Ordinal(m_ord, thresholds = "direct", verbose = T, name = "notDev", refModels = F, autoRun = TRUE)
m_dev = umxRAM2Ordinal(m_ord, thresholds = "deviat", verbose = T, name = "devT"  , refModels = F, autoRun = TRUE)

umxCompare(m_dir, m_dev) # identical, yay.
umxCompare(m_cont, c(m_int, m_dir))
umxCompare(m_dir, c(m_cont,m_int ))

plot(m_dev)
m_dev$lowerOnes_for_thresh$values %*% m_dev$deviations_for_thresh$values
m_dir$threshMat$values

# =========================
# =   Censored Ordinal    =
# =========================
m_cen <- umxRAM("censored", data = mxData(df_cen, type = "raw"), thresholds = "deviationBased", refModels=F,
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
plot(m_cen, std = T); ggplot2::qplot(df_cen$x1)
umx_show(m_cen, "values", "fixed", matrices = "thresholds")
umxCompare(m_dir, c(m_cont,m_int,m_cen))
umxCompare(c(m_cont, m_dir), c(m_int,m_cen))

# =========================================================
# = Build a model with thresholds where I "know" they are =
# =========================================================
tmp <- umxRAM("left_censored", data = mxData(df_cen, type = "raw"), thresholds = "direct", autoRun = F,
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)

maxLen = dim(tmp$threshMat$values)[1]
for (varName in manifests) {
	# varName = "x1" # levels are -3:4
	theseLevels = as.numeric(levels(df_cen[,varName]))
	nLevels = length(theseLevels)
	tmp$threshMat$values[, varName] = umx_pad(as.numeric(theseLevels[1:(nLevels-1)]), maxLen)
}
tmp$threshMat$free = FALSE
tmp = mxRun(tmp); umxCompare(m_cen, c(tmp)); # basically looks OK, given we can't capitalize on jiggling the thresholds to recapture noise.
plot(tmp, std = T)


m_tail <- umxRAM("tail", data = mxData(df_tail, type = "raw"), thresholds = "deviationBased",
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
plot(m_tail, std = T); ggplot2::qplot(df_tail$x1)
umx_show(m_tail, "values", "fixed", matrices = "thresholds")

tmp <- umxRAM("left_censored", data = mxData(df_tail, type = "raw"), thresholds = "direct", autoRun = F,
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
maxLen = dim(tmp$threshMat$values)[1]
for (varName in manifests) {
	# varName = "x1" # levels are -3:4
	theseLevels = as.numeric(levels(df_tail[,varName]))
	nLevels = length(theseLevels)
	tmp$threshMat$values[, varName] = umx_pad(as.numeric(theseLevels[1:(nLevels-1)]), maxLen)
}
tmp$threshMat$free = FALSE
tmp = mxRun(tmp); umxCompare(m_tail, c(tmp));

auto <- umxRAM("left_auto", data = mxData(df_tail, type = "raw"), thresholds = "left_", refModels = F,
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)
umxCompare(m_tail, c(tmp, auto));

# Intelligence, compassion, strength and perhaps most notably a strong sense of optimism.

# ========================================
# = How to fix thresholds where they are =
# ========================================
m_fix = mxRename(m_tail, "fixed")
m_fix$deviations_for_thresh$free = FALSE
m_fix = mxRun(m_fix); # m_fix = mxTryHard(m_fix);
umxCompare(m_tail, m_fix)

umx_round(summary(m1)$parameters[, c(1,3:6)], 2)
umx_round(summary(m2)$parameters[, c(1,3:6)], 2)
umx_round(summary(m3)$parameters[1:6, c(1,3:6)], 2)
umx_round(summary(m3)$parameters[7:25,c(1,3:6)], 2)
umx_round(summary(m4)$parameters[1:6, c(1,3:6)], 2)
umxCompare(m3, c(m4, m5))

plot(m1, showFixed = T, showMeans = FALSE);
plot(m2, showFixed = T, showMeans = FALSE);
plot(m3, showFixed = T, showMeans = FALSE);
# m4 = mxModel(m3, name="fixedThresholds", )

# =========================
# = Left censored integer =
# =========================
df = myFADataRaw[, manifests]
df[] = scale(df)
df = umx_round(df, 0)
df$x1[df$x1 < 0] = 0
df$x2[df$x2 < 0] = 0
df$x3[df$x3 < 0] = 0

df <- umxFactor(df)
m6 <- umxRAM("left_censored", data = mxData(df, type = "raw"), thresholds = "direct",
	umxPath("F1", to = manifests, values = .8),
	umxPath(v.m. = manifests),
	umxPath(v1m0 = "F1")
)

# = Fix the thresholds where we know they are =
m7 = mxRename(m6, "fixedAtKnown_LeftCensored")
m7$threshMat$free = FALSE
maxLen = dim(m7$threshMat$values)[1]
for (varName in manifests) {
	theseLevels = levels(df[,varName])
	nLevels     = length(theseLevels)
	m7$threshMat$values[,varName] = umx_pad(as.numeric(theseLevels[2:(nLevels)]), maxLen)
}
m7 = mxRun(m7); umxCompare(m6, m7)

plot(m7, std = T)

# http://stats.stackexchange.com/questions/20523/difference-between-logit-and-probit-models
# http://stats.stackexchange.com/questions/74544/fitting-a-sigmoid-function-why-is-my-fit-so-bad
# http://stats.stackexchange.com/questions/11947/fitting-an-exponential-model-to-data
# http://www.montessori-science.org/Montessori-Genius/Lillard_Montessori_flyer.pdf

# Methods to implement in umx
coef()
anova(fit, fit)
confint()
deviance()
df.residual()
fitted()
formula()
predict()
print()
profile()
residuals()
vcov()
weights()
# done
summary()
logLik()

# =================================
# = Logistic regression in OpenMx =
# =================================
# Do this in OpenMx!!
# [logistic regression](http://stackoverflow.com/questions/10571591/r-logistic-curve-plot-with-aggregate-points)

bodysize = rnorm(20, 30, 2)
bodysize = sort(bodysize)
survive  = c(0,0,0,0,0,1,0,1,0,0,1,1,0,1,1,1,0,1,1,1)
dat      = as.data.frame(cbind(bodysize, survive))
plot(bodysize, survive, xlab = "Body size", ylab = "Probability of survival")
g = glm(survive~bodysize, family = binomial,dat)
curve(predict(g, data.frame(bodysize = x), type = "resp"), add = TRUE)
points(bodysize, fitted(g), pch = 20)
