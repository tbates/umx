# 1. Add this new style of path: umxAlgPath("x", with = "y", free = F, alg = XY)?
# 2. umxLabel: extend to locate any matrices in a RAM model and auto label them?
# 3. ✓ Add umxPath(defn = "grp_def", labels = "data.grp")
	# umxPath(var   = "grp_def", fixedAt = 0),
	# umxPath(means = "grp_def", fixedAt = 0, labels = "data.grp"),

library(MASS)    # to get hold of mvrnorm function
set.seed(200)    # to make the simulation repeatable
N               = 500    # sample size, per group
Sigma           = matrix(c(1, .5, .5, 1), 2, 2)
group1          = MASS::mvrnorm(N, c(0, 0), Sigma) # Group 1 meanX = 0, meanY = 0
group2          = MASS::mvrnorm(N, c(1, 2), Sigma) # Group 1 meanX = 1, meanY = 2
xy              = rbind(group1, group2)      # Bind groups together by rows
dimnames(xy)[2] = list(c("x", "y"))          # Add names
grp             = rep(c(1, 0), each = N);    # Add def var [2n] for group status
selDVs          = c("x", "y")                # Make selection variables object
df = data.frame(xy, grp)
umxAPA(cov(df)); umxAPA(df)

# =============================
# = Basic x <-> y + means model =
# =============================
m1 <- umxRAM("basic", data = df,remove_unused_manifests = T,
	umxPath(v.m.  = selDVs),
	umxPath("x", with = "y")
)
mxStandardizeRAMpaths(m1, SE = T)
plot(m1);

# =============================================================
# = Compute covXY in an algebra, using value in a matrix  =
# =============================================================
m2 <- umxRAM("algebra", data = df, remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY + 0),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]")
)
round(coef(m2), 2)

# ========================================================
# = Use algebra to allow group to moderate XY covariance =
# ========================================================
m3 <- umxRAM("moderated", data = df, remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	mxMatrix(name = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxMatrix(name = "betaCov", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY + betaCov * data.grp),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]")
)
plot(m3, fixed = TRUE)
# We can test the moderator thus:
m4 = umxModify(m3, update = "moderated.betaCov[1,1]", name = "no_XY_moderation", comparison = TRUE)
round(coef(m4), 2)
# We've lost direct access to our estimate of covXY, either in the plot, or coef. We need to dig deeper with mxEval.
round(coef(m1), 2)

# ===================================================
# = Allow group to moderate XY covariance and means =
# ===================================================
m1 <- umxRAM("MZ", data = df, remove_unused_manifests = FALSE,
	umxPath(v.m. = selDVs),
	mxMatrix(name  = "XY", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxMatrix(name  = "betaCov", type = "Full", nrow = 1, ncol = 1, free = TRUE),
	mxAlgebra(name = "covXY", XY + betaCov * data.grp),
	umxPath("x", with = "y", free = F, label = "covXY[1,1]"),
	# Means
	umxPath("means", to = selDVs),
	umxPath("def", to = selDVs, labels = c("beta_1", "beta_2")),
	umxPath(defn = "def", labels = "data.grp")
)
plot(m1)
m2 = umxModify(m1, update = "MZ.betaCov[1,1]".   , name = "no_XY_moderation", comparison = T)
m3 = umxModify(m1, update = c("beta_1", "beta_2"), name = "no_means_moderation", comparison = F)
m4 = umxEquate(m1, master = "beta_1", slave = "beta_2", free = T, verbose = T, name = "equate means")
m5 = umxModify(m2, update = c("beta_1", "beta_2"), name = "no_means_moderation")