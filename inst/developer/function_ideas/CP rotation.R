# Rotate Loadings
rotate <- function(L, radians = .25 * pi){
	sinx = sin(radians)
	cosx = cos(radians)
	x = matrix(c(cosx, sinx, -sinx, cosx), 2, 2)
	L %*% x
}
rotate(2, radians = .25 * pi)

# Check that my one-liner rotation package works - default of 45 degrees (.25 radians) if done twice should flip the factors
m1$top$cp_loadings$values
x = rotate(m1$top$cp_loadings$values)
rotate(x)
# cool eh?

# Plug them back into CPM and fix *just the loadings*
m2 = m1
m2$top = omxSetParameters(m1$top, labels=m1$top$cp_loadings$labels, values = x, free = FALSE)

m2run <- mxTryHardOrdinal(m2)
plot(m2run)