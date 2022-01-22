# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxPath.r") 
# test_package("umx")
context("umxPath()")

test_that("test umxPath", {	
	require(umx)
	data(demoOneFactor) # from OpenMx
	latents  = c("G")
	manifests = names(demoOneFactor)

	umxPath("A", to = "B") # One-headed path from A to B
	umxPath("A", to = "B", fixedAt = 1) # same, with value fixed @@1
	umxPath("A", to = c("B", "C"), fixedAt = 1:2) # same, with more than 1 value
	umxPath("A", to = c("B", "C"), firstAt = 1) # Fix only the first path, others free
	umxPath(var = letters[1:5]) # Give a variance to five variables	
	umxPath(var = "A") # Give a variance to A
	umxPath(var = "A", fixedAt = 1) # Give A variance, fixed at 1
	umxPath(means = c("A","B")) # Create a means model for A: from = "one", to = "A"
	umxPath(v1m0 = "A") # Give "A" variance and a mean, fixed at 1 and 0 respectively
	umxPath(v.m. = "A") # Give "A" variance and a mean, leaving both free.
	umxPath(v0m0 = "W") # Give "A" variance and a mean, fixing both @0.
	umxPath(v.m0 = "W"), # Give "A" variance and a mean, fixing mean, and leaving var free
	umxPath(v0m0 = "W", label = c(NA, "data.W")) # use data as values
	umxPath("A", with = "B") # using with: same as "to = B, arrows = 2"
	umxPath("A", with = "B", fixedAt = .5) # 2-head path fixed at .5
	umxPath("A", with = c("B", "C"), firstAt = 1) # first covariance fixed at 1
	umxPath(cov = c("A", "B"))  # Covariance A <-> B
	umxPath(defn = "mpg") # create latent called def_mpg, with 0 mean * var, and label = "data.mpg"
	umxPath(fromEach = c('a','b'), to = c('c','d')) # a->c, a<->d, b<->c, b<->d
	umxPath(unique.bivariate = c('a','b','c')) # bivariate paths a<->b, a<->c, b<->c etc.
	umxPath(unique.pairs = letters[1:3]) # all distinct pairs: a<->a, a<->b, a<->c, b<->b, etc.
	umxPath(Cholesky = c("A1","A2"), to = c("m1", "m2")) # Cholesky
})
