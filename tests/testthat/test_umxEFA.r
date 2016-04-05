library(umx)
m1 = umxEFA(name= "test", latents = "g", data = mtcars[, c("mpg", "disp", "hp", "wt")])

name    = "test"
latents = "g"
data    = mtcars[, c("mpg", "disp", "hp", "wt")]

# ================
# = Test working =
# ================

manifests <- names(data)
