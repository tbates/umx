# umxEFA formula interface
m1 = umxEFA(~ mpg + disp + hp + wt + qsec, factors = 2, rotation = "promax", data = mtcars)
m1 = umxEFA(~ mpg + disp + hp + wt + qsec, factors = 2, rotation = "promax", data = mtcars, return = "loadings")
loadings(m1) # ???

library(umx)
m1 = umxEFA(name= "test", latents = "g", data = mtcars)

# ================
# = Test working =
# ================
manifests <- names(data)

m1 = umxEFA(mtcars, factors = 1)
x = umxFactorScores(m1, type = c('Regression'), minManifests = 3)
names(x) == "F1"

m1 = umxEFA(mtcars, factors = "test")
x = umxFactorScores(m1, type = c('Regression'), minManifests = 3)
names(x) == "test"

# Warning messages:
# 1: Model 'Independence EFA' meets accuracy but did not converge (Mx status GREEN).

# Warning messages:
# 1: In model 'Independence EFA' Optimizer returned a non-zero status code 1. The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. Optimizer was terminated because no further improvement could be made in the merit function (Mx status GREEN).