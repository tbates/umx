# 1. Add this new style of path: umxAlgPath("x", with = "y", free = F, alg = XY)
# 2. Find any matrices in a RAM model and auto label them
# 3. Add umxDefn
	# umxPath(var   = "grp_def", fixedAt = 0),
	# umxPath(means = "grp_def", fixedAt = 0, labels = "data.grp"),

library(MASS)    # to get hold of mvrnorm function
set.seed(200)    # to make the simulation repeatable
N               = 500    # sample size, per group
Sigma           = matrix(c(1, .5, .5, 1), 2, 2)
group1          = mvrnorm(N, c(1, 2), Sigma) # Use mvrnorm from MASS package
group2          = mvrnorm(N, c(0, 0), Sigma)
xy              = rbind(group1, group2)      # Bind groups together by rows
dimnames(xy)[2] = list(c("x", "y"))          # Add names
grp             = rep(c(1, 0), each = N);    # Add def var [2n] for group status
selDVs          = c("x", "y")                # Make selection variables object
df = data.frame(xy, grp)
cov(df)

# =============================
# = Basic x<->y + means model =
# =============================
m1 <- umxRAM("MZ", data = df,remove_unused_manifests = T,
	umxPath(v.m.  = selDVs),
	umxPath("x", with = "y")
)
plot(m1); cov(df)
round(coef(m1), 2)

# =====================================================
# = Let's compute cov XY in a matrix, then read it in =
# =====================================================
m1 <- umxRAM("MZ", data = df,remove_unused_manifests = FALSE,
	umxPath(v.m.  = selDVs),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]"), # if label contains [ free = FALSE
	mxMatrix(name = "covXY", type = "Full", nrow = 1, ncol = 1, free = TRUE)
)
round(coef(m1),2)

# =========================================
# = Let's use an algebra to compute covXY =
# =========================================
m1 <- umxRAM("MZ", data = df,remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]"), # if label contains [ free = FALSE
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY)
)
round(coef(m1),2)

# ===================================================
# = Let's allow group to moderate the XY covariance =
# ===================================================
m1 <- umxRAM("MZ", data = df,remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]"),
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxMatrix(name = "bMod", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY + bMod*data.grp)
)
round(coef(m1), 2)
# test the moderator
m2 = umxModify(m1, update = "MZ.bMod[1,1]", name = "no_XY_moderation", comparison = T)
round(coef(m2), 2)

# ===================================================
# = Allow group to moderate XY covariance and means =
# ===================================================
m1 <- umxRAM("MZ", data = df,remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]"),
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxMatrix(name = "bMod", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY + bMod*data.grp)

	umxPath(var   = "grp_def", fixedAt = 0),
	umxPath(means = "grp_def", fixedAt = 0, labels = "data.grp"),
	mxPath("grp_def", to = selDVs, labels = c("beta_1", "beta_2"))
)

m1 <- umxRAM("MZ", data = df,remove_unused_manifests = FALSE,
	umxPath(v.m.  = selDVs),
	# mxMatrix(name = "b_2", label = "modVal", "Full", nrow=1, ncol=1, free=T, values = 0),
	# mxAlgebra(name = "covXY", (b_2 * 0)  + cov),
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]")
)
round(coef(m1),2)
