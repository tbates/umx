require(OpenMx)
data(myLongitudinalData)

manifests = paste0("x", 1:5)

umxLatentGrowth <- function(name = "Linear Growth Curve", selVars = NA, data = NA) {
	latents   = c("intercept", "slope")
	nManifests = length(selVars)
	m1 <- umxRAM(name, data = data,
		# residual variances + latent variances and covariance
	    umxPath(var = selVars, labels = "residual"),
	    umxPath(unique.pairs = latents),
		# intercept (1) and slope (increment) loadings
	    umxPath("intercept", to = selVars, fixedAt = 1),
	    umxPath("slope", to = selVars, fixedAt = 0:(nManifests-1)),
		# manifest (@0) and latent (free) means 
	    umxPath(mean = selVars, fixedAt = 0),
	    umxPath(means = latents)
	)
	return(result)
}
coefficients(m1)
plot(m1)