library(umx)
m1 = umxEFA(name= "test", latents = "g", data = mtcars[, c("mpg", "disp", "hp", "wt")])
name    = "test"
latents = "g"
data    = mtcars[, c("mpg", "disp", "hp", "wt")]

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