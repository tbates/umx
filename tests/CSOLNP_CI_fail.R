# Test CSOLNP and NPSOL computation of CIs

uri = "http://openmx.psyc.virginia.edu/sites/default/files/issues/m1.txt"
print(load("m1.txt")) # loads "m1", a model which shows this issue. (called txt instead of rda to get around the website suffix list)

# =====================
# = Set opt to NPSOL  =
# =====================
mxOption(NULL, "Default optimizer", "NPSOL")
# Add a CI, and estimate it...
x = mxModel(m1, mxCI("DepressionII_with_heart_disease"))
# NPSOL code reds, but the CI looks fine
x = mxRun(x, intervals = T); summary(x)$CI
#                                  lbound   estimate   ubound note
# DepressionII_with_heart_disease 0.1022891 0.1346589 0.1687101

# ====================
# = Switch optimizer =
# ====================
mxOption(NULL, "Default optimizer", "CSOLNP")

x = mxModel(m1, mxCI("DepressionII_with_heart_disease"))
x = mxRun(x, intervals = T); summary(x)$CI
# CSOLNP claims to run fine, but the CIs are near (but not quite) identical to each other and miles out.
# DepressionII_with_heart_disease 0.1346571 0.1346638 0.1346748
