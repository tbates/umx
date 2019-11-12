# ==================================================
# = ground truth: power at n= 90 for r=.3 is ~ 83% =
# ==================================================
pwr::pwr.r.test(r = .3, n = 90) # power ± 82%

# Repeat with mxPower, again solving for power at n= 90

# 1. Make model of true XY correlation of .3
tmp = umx_make_raw_from_cov(qm(1, .3| .3, 1), n=2000, varNames= c("X", "Y"), empirical= TRUE)
m1 = umxRAM("corXY", data = tmp, umxPath("X", with = "Y"), umxPath(var = c("X", "Y")))
# 2. make null model covXY = 0
m2 = umxModify(m1, "X_with_Y")

# Both methods agree: power is ~83%
mxPower(m1, m2, n= 90, power=NULL, method= "ncp")
mxPower(m1, m2, n= 90, power=NULL, method= "empirical")

# PS: For umx, we default to ncp

# Now test search for power across range of ns
# using ncp gets it correct (~90)
mxPowerSearch(m1, m2, method="ncp")
# N     power
# 86.74 0.82

# , as does empirical
mxPowerSearch(m1, m2)
# N     power
# 90.32 0.87

# =============================================
# = NOW, let's try fixing n where we know we  =
# = have 80% power for our effect (𝛽= .3)     =
# =============================================
mxPowerSearch(m1, m2, n = 90)
# Oops miles off...
# beta  power
# 0.058 0.80

# What is it doing? Let's estimate a model with beta = .058
tmp = umx_make_raw_from_cov(qm(1, .058| .058, 1), n=2000, varNames= c("X", "Y"), empirical= TRUE)
cov(tmp)
#       X     Y
# X 1.000 0.058
# Y 0.058 1.000

m1 = umxRAM("corXY", data = tmp, umxPath("X", with = "Y"), umxPath(var = c("X", "Y")))
m2 = umxModify(m1, "X_with_Y")

mxPower(m1, m2, n= 90, power=NULL, method= "ncp")
#     n = 90
# power = 0.08540375
# Yikes... mxPowerSearch was an order of magnitude off.

# let's drop in the n from our simulated model...
mxPower(m1, m2, n= 2000, power=NULL, method= "ncp")
#     n = 2000
# power = 0.7376339


# Yip: mxPowerSearch is using the n from the trueModel happens to have, not the
	 
umxPower(m1, "X_with_Y", explore = TRUE)
#         N power
# 10  86.74  0.82
# 11  94.42  0.85
# 12 102.09  0.87

umxPower(m1, "X_with_Y", n= 90, method = "empirical", explore = TRUE)

# Search X_with_Y:power relationship for n=90
# |    | X_with_Y | power | lower | upper |
# |:---|:---------|:------|:------|:------|
# | 10 | 0.06     | 0.80  | 0.74  | 0.85  |


