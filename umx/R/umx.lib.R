# setwd("~/bin/umx/umx"); devtools::document(); devtools::install(); devtools::load_all()
# devtools::dev_help("xmuLabel_MATRIX_Model")

# Description notes
# Here's a worked example
# OpenMx has example data suitable for developing models with
# myFADataRaw
# To learn more, see https://github.com/tbates/umx/

# https://github.com/hadley/devtools/wiki/Package-basics

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# ===================================
# = Maybe add some printing helpers =
# ===================================

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

# To USE ME IN YOUR SCRIPTS SAY: 
# source("http://github.com/OpenMx/mxHelper.R")

mx_FakeData <- function(dataset, digits=2, n=NA, 
  use.names=TRUE, use.levels=TRUE, use.miss=TRUE,
  mvt.method="eigen", het.ML=FALSE, het.suppress=TRUE){

  # This function takes as argument an existing dataset, which 
  # must be either a matrix or a data frame. Each column of the 
  # dataset must consist either of numeric variables or ordered 
  # factors. When one or more ordered factors are included, 
  # then a heterogeneous correlation matrix is computed using 
  # John Fox's polycor package. Pairwise complete observations 
  # are used for all covariances, and the exact pattern of 
  # missing data present in the input is placed in the output,
  # provided a new sample size is not requested. Warnings from
  # the hetcor function are suppressed.

  # Author:   Ryne Estabrook
  # Created:  17 Aug 2010

  require(mvtnorm)
  require(polycor)

  # requires data frame or matrix
  if((is.data.frame(dataset)+is.matrix(dataset))==0){
    warning("Data must be a data frame or matrix")
  }

  # organization
  row <- dim(dataset)[1] # number of rows
  if(is.na(n))(n <- row) # sets unspecified sample size to num rows
  col <- dim(dataset)[2] # number of columns
  del <- is.na(dataset)  # records position of NAs in dataset
  if(n!=row){
    select <- round(runif(n, 0.5, row+.49),0)
    del <- del[select,]
  }
  num <- rep(NA, col)    # see what's not a factor
  ord <- rep(NA, col)    # see what's an ordered factor

  # which columns are numeric (the others are factors)?
  for (i in 1:col){
    num[i] <- is.numeric(dataset[,i])
    ord[i] <- is.ordered(dataset[,i])
  }

  # check for unordered factors
  location <- !(num|ord)
  unorder <- sum(location)

  if(unorder>0)warning(
    paste("Unordered factor detected in variable(s):", 
      names(dataset)[location]
    )
  )

  # if everything is numeric, don't invoke polycor
  if(sum(!num)==0){
    # generate data with rmvnorm
    fake <- rmvnorm(n, 
      apply(dataset, 2, mean, na.rm=TRUE),
      cov(dataset, use="pairwise.complete.obs"),
      mvt.method)

    # round the data to the requested digits
    fake <- round(fake, digits)

    # insert the missing data, if so requested
    if(use.miss==TRUE)(fake[del] <- NA)

    # give the variables names, if so requested
    if(use.names==TRUE)(names(fake) <- names(dataset))

    # return the new data
    return(fake)
  }

  # if there are factors, we start here

  # find the variable means (constrain to zero for factors)
  mixedMeans <- rep(0, col)
  mixedMeans[num] <- apply(dataset[,num], 2, mean, na.rm=TRUE)

  # estimate a heterogeneous correlation matrix
  if (het.suppress==TRUE){
    suppressWarnings(het <- hetcor(dataset, ML=het.ML))
  } else (het <- hetcor(dataset, ML=het.ML))
  mixedCov <- het$correlations

  # make a diagonal matrix of standard deviations to turn the 
  # correlation matrix into a covariance matrix
  stand <- matrix(0, col, col)
  diag(stand) <- rep(1, col)
  diag(stand)[num] <- apply(dataset[,num], 2, sd, na.rm=TRUE)
  # pre and post multiply hetero cor matrix by diagonal sd matrix
  mixedCov <- stand %*% mixedCov %*% stand

  # generate the data
  fake <- as.data.frame(rmvnorm(row, mixedMeans, mixedCov, mvt.method))

  # insert the missing data, if so requested
  if(use.miss==TRUE)(fake[del] <- NA)

  # turn the required continuous variables into factors
  for (i in (1:col)[!num]){
    # the original data for this column
    old <- dataset[,i]
   
    # the new data for this column, omiting NAs
    new <- fake[!is.na(fake[,i]),i]

    # what are the levels of the original factor?
    lev <- levels(old)

    # establish cutpoints in new variable from cdf of old factor
    cut <- cumsum(table(old))/(sum(!is.na(old)))

    # put continuous variable into a matrix, repeating value across columns
    wide <- matrix(new, length(new), length(lev))

    # put the cutpoints in a matrix, repeating the cut point values across rows
    crit <- matrix(quantile(new, cut), length(new), length(lev), byrow=TRUE)

    # for each value (row of the wide matrix), 
    # how many cutpoints is the value greater than?
    # number of cutpoints surpassed=category
    fake[!is.na(fake[,i]),i] <- apply(wide>crit, 1, sum)

    # make it a factor
    fake[,i] <- factor(fake[,i], ordered=TRUE)

    # give the new factor the same levels as the old variable
    if(length(levels(fake[,i]))!=length(lev))message(
      paste("Fewer categories in simulated variable", 
      names(fake)[i], "than in input variable", names(dataset)[i]))
    if(use.levels==TRUE&(length(levels(fake[,i]))==length(lev))){
      levels(fake[,i]) <- lev} else (levels(fake[,i]) <- 1:length(lev))
  }

  # round the data to the requested digits
  fake[,num] <- round(fake[,num], digits)

  # give the variables names, if so requested
  if(use.names==TRUE)(names(fake) <- names(dataset))
  
  # return the new data
  return(fake)
}

umxSummaryGxE <- function(model = NA, xlab = NA, location = "topleft", separateGraphs = F) {
	# Plot Moderation
	# umxSummaryGxE(model, location = "topright")
	if(is.na(model)){
		message("umxSummaryGxE creates Plots for twin moderation models. A use example is:\n umxSummaryGxE(model, location = \"topright\")")
		stop();
	}
	# get unique values of moderator
	mzData = model@submodels$MZ@data@observed
	dzData = model@submodels$DZ@data@observed
	selDefs = names(mzData)[3:4]
	if(is.na(xlab)){
		xlab = selDefs[1]
	}
	mz1 = as.vector(mzData[,selDefs[1]])
	mz2 = as.vector(mzData[,selDefs[2]])
	dz1 = as.vector(dzData[,selDefs[1]])
	dz2 = as.vector(dzData[,selDefs[2]])
	allValuesOfDefVar = c(mz1,mz2,dz1,dz2)
	defVarValues = sort(unique(allValuesOfDefVar))
	a   = model@submodels$top@matrices$a@values
	c   = model@submodels$top@matrices$c@values
	e   = model@submodels$top@matrices$e@values
	am  = model@submodels$top@matrices$am@values
	cm  = model@submodels$top@matrices$cm@values
	em  = model@submodels$top@matrices$em@values
	Va  = (a + am*defVarValues)^2
	Vc  = (c + cm*defVarValues)^2
	Ve  = (e + em*defVarValues)^2
	Vt  = Va + Vc + Ve
    # c("total", "genetic", "unique", "shared")
	out    = as.matrix(cbind(Va   , Ve   , Vc, Vt))
	outStd = as.matrix(cbind(Va/Vt, Ve/Vt, Vc/Vt))
	if(separateGraphs){
		print("Outputting two graphs")
		matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = 1:4, xlab = xlab, ylab = "Raw Variance", main = "Raw Moderation Effects")
		legend(location, legend = c("Va", "Vc", "Ve", "Vtot"), lty = 1:4, col = 1:4)
		matplot(defVarValues, outStd, type = "l", lty = 1:4, col = 1:4, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main = "Standardized Moderation Effects")
		legend(location, legend = c("Va", "Vc", "Ve"), lty = 1:4, col = 1:4)
		par(mfrow = c(1, 1)) # back to black
	}else{
		par(mfrow = c(2, 1)) # two rows, one column for raw and std variance
		par(mfrow = c(1, 2)) # one row: raw and std variance

		matplot(x = defVarValues, y = out, type = "l", lty = 1:4, col = 1:4, xlab = xlab, ylab = "Raw Variance", main = "Raw Moderation Effects")
		legend(location, legend = c("genetic", "unique", "shared", "total"), lty = 1:4, col = 1:4)

		matplot(defVarValues, outStd, type = "l", lty = 1:4, col = 1:4, ylim = 0:1, xlab = xlab, ylab = "Standardized Variance", main = "Standardized Moderation Effects")
		legend(location, legend = c("genetic", "unique", "shared"), lty = 1:4, col = 1:4)
		par(mfrow = c(1, 1)) # back to black
	}
}