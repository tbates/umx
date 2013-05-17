pp33 <- function(df_, x_) {
	# http://www.p-curve.com/Supplement/R/pp33.r
	# Find critical value of student (xc) that gives p=.05 when df=df_
	xc = qt(p = .975, df = df_)		
	# Find noncentrality parameter (ncp) that leads 33% power to obtain xc
	f <- function(delta, pr, x, df){
		pt(x, df = df, ncp = delta) - pr
	}
	out <- uniroot(f, c(0, 37.62), pr =2/3, x = xc, df = df_)	
	ncp_=out$root	
	# Find probability of getting x_ or larger given ncp
	p_larger = pt(x_, df = df_, ncp = ncp_)
	# Condition on p < .05 (i.e., get pp-value)
	pp = 3 * (p_larger - 2/3)
	# Print results
	return(pp)
}

df = data.frame(matrix(nrow = 3, byrow = T, data = c(
		1.00, .190, .091,
		.190, 1.00, .995,
		.091, .995, 1.00)
), row.names = paste0("X",1:3))


m1 <- mxModel("m1", type="RAM",
	manifestVars = paste0("X",1:3),
	mxPath(from = paste0("X",1:3), arrows = 2, values = 1, free = c(T,FF)), # manifest residuals 
	mxPath(from = c("X2", "X3"), to = "X1", arrows = 1), # manifest causes
	mxData(df, type = "cov", numObs = 100)
)
m1= mxRun(m1); umxReportFit(m1)
summary(m1)