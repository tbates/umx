umx_t.test <- function(x, y = NULL, alternative = c("two.sided", "less", "greater"), mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95, ...) {
	# TODO
	# 1. implement x based analysis
	# 2. implement x,y  based analysis
	# 3. implement y ~ x, data=x based analysis
	# t.test example using myFADataRaw builtin data. 
	# t.test(x1 ~ (x2 < mean(x2)), data=myFADataRaw)

	argList <- list(...);

	if(!require("OpenMx")){
		# ezInstall()
	}

	if(is.data.frame(x)) {
		if(dim(x)[2]>1){
			message("x-based t.test not implemented: use x=x, y=y")
		}
	} else if(!is.null(argList$formula)){
		message("formula not implemented: use x=x, y=y")
	} else {
		# implementing x,y t.test
		group1_data = data.frame(x); names(group1_data) = "x"
		group2_data = data.frame(y); names(group2_data) = "x"
		manifests = "x"
	}
	m1 = mxModel("grp1", type = "RAM",
		manifestVars = "x",
		# Factor loadings
	    mxPath(from = "x", arrows = 2), # variance of x 
	    mxPath(from = "one", to = "x", arrows = 1), # mean of x
		mxData(group1_data, type = "raw")
	)
	m1	= umxLabel(m1, suffix = "_grp1");
	# m1 = mxRun(m1); umxReportFit(m1)
	m2 = mxModel("grp2", type = "RAM",
		manifestVars = "x",
		# Factor loadings
	    mxPath(from = "x", arrows = 2), # variance of x 
	    mxPath(from = "one", to = "x", arrows = 1), # mean of x
		mxData(group2_data, type = "raw")
	)
	m2	= umxLabel(m2, suffix = "_grp2");
	m3 = mxModel("t_test",
		m1, # group 1
		m2, # group 2
	 	# what we want to test is both means models at the same time, so make an objective of the sum of their likelihoods
	 	mxAlgebra(grp1.objective + grp2.objective, name = "t"),
		mxAlgebraObjective("t")
	)
	m3 = mxRun(m3)	
	m4 = umxEquate(m3, master="^x_mean_grp1", slave="^x_mean_grp2", free = T, verbose = T, name="equateMeans")
	m4 = mxRun(m4)	

	print(mxCompare(m3,m4))

	# TODO make the output like this...
	# 
	# Running equateMeans 
	#     base  comparison ep minus2LL  df      AIC    diffLL diffdf         p
	# 1 t_test        <NA>  4 2846.802 996 854.8021        NA     NA        NA
	# 2 t_test equateMeans  3 2846.938 997 852.9377 0.1355591      1 0.7127364
	# Warning message:
	# In model 't_test' NPSOL returned a non-zero status code 1. The final iterate satisfies the optimality conditions to the accuracy requested, but the sequence of iterates has not yet converged. NPSOL was terminated because no further improvement could be made in the merit function (Mx status GREEN). 
	# > t.test(x = data$x, y = data$y, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
	# 
	# 	Welch Two Sample t-test
	# 
	# data:  data$x and data$y 
	# t = 0.3678, df = 997.814, p-value = 0.7131
	# alternative hypothesis: true difference in means is not equal to 0 
	# 95 percent confidence interval:
	#  -0.1014033  0.1481873 
	# sample estimates:
	# mean of x mean of y 
	#  3.011344  2.987952 
	 
}
umx_t.test(x = data$x, y = data$y, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)
t.test(x = data$x, y = data$y, alternative = "two.sided", mu = 0, paired = FALSE, var.equal = FALSE, conf.level = 0.95)

data = myFADataRaw[,1:2]
names(data) = c("y", "x")
group1_data = data.frame(data$x); names(group1_data) = "x"
group2_data = data.frame(data$y); names(group2_data) = "x"


umx_t.test(x, y = x)
umx_t.test(x, formula = y ~ x)

data = within(data, { x = (x < mean(x))})
head(data)



a = c("ABBA")
b = c("_38 Special","Wild-eyed Southern Boys","Zuell")
toMove = b[!(b  %in% a)]
base = "/Users/tim/Music/iTunes/iTunes Music/"
dest = "/Users/tim/Music/iTunes/iTunes Music/Music/"
moveFile(baseFolder = base, fileNameList = toMove, destFolder = dest, test = F)

# Developmental Science aims to represent the very best of contemporary scientific developmental psychology and developmental cognitive neuroscience, both in the presentation of theory and in reporting new data. Developmental Science includes: comparative and biological perspectives, connectionist and computational perspectives, and developmental disorders. Developmental Science publishes work that bridges levels of explanation, such as from brain development to cognitive or social change, or work that specifically attempts to elucidate mechanisms of developmental change at one level. Manuscripts judged to fall outside this remit may be rejected without full refereeing.

# http://jaredknowles.com/r-bootcamp/
# Shirley Williams: complete C
# Foreign in 1968? Admitted East Africans
# Entry to EU
# Education Secreatary: Fucked over the grammar schools created shit comprehensives ("our friends, the muslims who want single sex muslim schools")
# Prison minister: 

