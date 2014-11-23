# ===================================
# = WLS example from stack overflow =
# ===================================

y <- c(1, 1, 2, 1, 2, 1, 3, 4, 22, 30, 44, 58, 68, 69, 71, 72, 75, 72, 80, 78, 87, 86, 80, 82, 92, 90, 85, 61, 38, 36) / 100
x <- ceiling(exp(seq(log(20), log(500), length.out=length(y))))
counts <- c( 10, 3, 17, 20, 38, 31, 44, 55, 58, 68, 77, 82, 86, 82, 77, 75, 70, 65, 68, 51, 47, 41, 38, 30, 22, 14, 9, 4, 2, 1)

# ================================
# = The least-squares criterion. =
# ================================

# theta[1] is a location, theta[2] an x-scale, and theta[3] a y-scale.


f <- function(theta, x = x, y = y, n = counts) {
	umx_check(length(theta) == 3, "stop", "theta must consist of three items: location, x- and y- scales")
	sum(n * (y - pnorm(x, theta[1], theta[2]) * theta[3])^2) / sum(n)	
}

# =================================================
# = # Perform a count-weighted least-squares fit. =
# =================================================

xi = log(x)
fit <- optim(c(median(xi), sd(xi), max(y) * sd(xi)), f, x=xi, y=y, n=counts)

# Plot the result.
par(mfrow=c(1,1))
plot(x, y, log="x", xlog=TRUE, pch=19, col="Gray", cex=sqrt(counts/12))
points(x, y, cex=sqrt(counts/10))
curve(fit$par[3] * pnorm(log(x), fit$par[1], fit$par[2]), from=10, to=1000, col="Red", add=TRUE)

```