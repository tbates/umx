# http://www.tcnj.edu/~ruscio/taxometrics.html
####################################################################################################
# To use the program code in this .R file, it should be imported into R using 
# the "Source R code" option from the "File" menu in R.  
#
# To run the main program (EFA.Comp.Data), you need to provide data 
# (arranged as an N [sample size] x k [number 
# of variables] matrix) and specify the largest number of factors to consider.  
# The program may reach a stopping point before it reaches this largest number of factors.
#
# Below is a series of commands that you can run to get a sense for how to use the program.
# First, a data set with N = 500 cases and k = 9 variables is created such that
# there are three correlated factors; each variable loads onto two of the three factors.
# Then, the correlation matrix is displayed.  Finally, the EFA.Comp.Data
# program is run.  To use these commands, remove the "#" symbol at the 
# beginning of each line and then copy and paste them into the R command window.
#
# s1 <- rnorm(500)
# s2 <- rnorm(500)
# s3 <- rnorm(500)
# x1 <- s1 + s2 + rnorm(500)
# x2 <- s1 + s2 + rnorm(500)
# x3 <- s1 + s2 + rnorm(500)
# x4 <- s1 + s3 + rnorm(500)
# x5 <- s1 + s3 + rnorm(500)
# x6 <- s1 + s3 + rnorm(500)
# x7 <- s2 + s3 + rnorm(500)
# x8 <- s2 + s3 + rnorm(500)
# x9 <- s2 + s3 + rnorm(500)
# x <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
# cor(x)
# EFA.Comp.Data(Data = x, F.Max = 5, Graph = T)


#########################################################################################
EFA.Comp.Data <- function(Data, F.Max, N.Pop = 10000, N.Samples = 500, Alpha = .30, Graph = F){
    # Data = N (sample size) x k (number of variables) data matrix
    # F.Max = largest number of factors to consider
    # N.Pop = size of finite populations of comparison data (default = 10,000 cases)
    # N.Samples = number of samples drawn from each population (default = 500)
    # Alpha = alpha level when testing statistical significance of improvement with add'l factor (default = .30) 
    # Graph = whether to plot the fit of eigenvalues to those for comparison data (default = F)

    N <- dim(Data)[1]
    k <- dim(Data)[2]
    cor.Data <- cor(Data)
    Eigs.Data <- eigen(cor.Data)$values
    RMSR.Eigs <- matrix(0, nrow = N.Samples, ncol = F.Max)
    Sig <- T
    F.CD <- 1
    while ((F.CD <= F.Max) & (Sig))
    {
        Pop <- GenData(Data, N.Factors = F.CD, N = N.Pop)
        for (j in 1:N.Samples)
        {
            Samp <- Pop[sample(1:N.Pop, size = N, replace = T),]
            cor.Samp <- cor(Samp)
            Eigs.Samp <- eigen(cor.Samp)$values
            RMSR.Eigs[j,F.CD] <- sqrt(sum((Eigs.Samp - Eigs.Data) * (Eigs.Samp - Eigs.Data)) / k)
        }
        if (F.CD > 1) Sig <- (wilcox.test(RMSR.Eigs[,F.CD], RMSR.Eigs[,(F.CD - 1)], "less")$p.value < Alpha)
        if (Sig) F.CD <- F.CD + 1
    }
    cat("Number of factors to retain: ",F.CD - 1,"\n")
    if (Graph)
    {
        if (Sig) x.max <- F.CD - 1
        else x.max <- F.CD
        ys <- apply(RMSR.Eigs[,1:x.max], 2, mean)
        plot(x = 1:x.max, y = ys, ylim = c(0, max(ys)), xlab = "Factor", ylab = "RMSR Eigenvalue", type = "b", 
            main = "Fit to Comparison Data")
        abline(v = F.CD - 1, lty = 3)
    }
}


################################################################################################################
GenData <- function(Supplied.Data, N.Factors, N, Max.Trials = 5, Initial.Multiplier = 1)
{
    # Steps refer to description in the following article:
    # Ruscio, J., & Kaczetow, W. (2008). Simulating multivariate nonnormal data using an iterative algorithm. 
    # Multivariate Behavioral Research, 43(3), 355-381.

# Initialize variables and (if applicable) set random number seed (step 1) -------------------------------------

   set.seed(1)
   k <- dim(Supplied.Data)[2]
   Data <- matrix(0, nrow = N, ncol = k)            # Matrix to store the simulated data
   Distributions <- matrix(0, nrow = N, ncol = k)   # Matrix to store each variable's score distribution
   Iteration <- 0                                   # Iteration counter
   Best.RMSR <- 1                                   # Lowest RMSR correlation
   Trials.Without.Improvement <- 0                  # Trial counter

# Generate distribution for each variable (step 2) -------------------------------------------------------------

   for (i in 1:k)
      Distributions[,i] <- sort(sample(Supplied.Data[,i], size = N, replace = T))

# Calculate and store a copy of the target correlation matrix (step 3) -----------------------------------------

   Target.Corr <- cor(Supplied.Data)
   Intermediate.Corr <- Target.Corr

# Generate random normal data for shared and unique components, initialize factor loadings (steps 5, 6) --------

   Shared.Comp <- matrix(rnorm(N * N.Factors, 0, 1), nrow = N, ncol = N.Factors)
   Unique.Comp <- matrix(rnorm(N * k, 0, 1), nrow = N, ncol = k)
   Shared.Load <- matrix(0, nrow = k, ncol = N.Factors)
   Unique.Load <- matrix(0, nrow = k, ncol = 1)

# Begin loop that ends when specified number of iterations pass without improvement in RMSR correlation --------

   while (Trials.Without.Improvement < Max.Trials)
   {
      Iteration <- Iteration + 1

# Calculate factor loadings and apply to reproduce desired correlations (steps 7, 8) ---------------------------

      Fact.Anal <- Factor.Analysis(Intermediate.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
      if (N.Factors == 1) Shared.Load[,1] <- Fact.Anal$loadings
      else 
         for (i in 1:N.Factors)
            Shared.Load[,i] <- Fact.Anal$loadings[,i]
      Shared.Load[Shared.Load > 1] <- 1
      Shared.Load[Shared.Load < -1] <- -1
      if (Shared.Load[1,1] < 0) Shared.Load <- Shared.Load * -1
      for (i in 1:k)
         if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1) Unique.Load[i,1] <- 
            (1 - sum(Shared.Load[i,] * Shared.Load[i,]))
         else Unique.Load[i,1] <- 0
      Unique.Load <- sqrt(Unique.Load)
      for (i in 1:k)
         Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]

# Replace normal with nonnormal distributions (step 9) ---------------------------------------------------------

      for (i in 1:k)
      {
         Data <- Data[sort.list(Data[,i]),]
         Data[,i] <- Distributions[,i]
      }

# Calculate RMSR correlation, compare to lowest value, take appropriate action (steps 10, 11, 12) --------------

      Reproduced.Corr <- cor(Data)
      Residual.Corr <- Target.Corr - Reproduced.Corr
      RMSR <- sqrt(sum(Residual.Corr[lower.tri(Residual.Corr)] * Residual.Corr[lower.tri(Residual.Corr)]) / 
              (.5 * (k * k - k)))
      if (RMSR < Best.RMSR)
      {
         Best.RMSR <- RMSR
         Best.Corr <- Intermediate.Corr
         Best.Res <- Residual.Corr
         Intermediate.Corr <- Intermediate.Corr + Initial.Multiplier * Residual.Corr
         Trials.Without.Improvement <- 0
      }
      else 
      {
         Trials.Without.Improvement <- Trials.Without.Improvement + 1
         Current.Multiplier <- Initial.Multiplier * .5 ^ Trials.Without.Improvement
         Intermediate.Corr <- Best.Corr + Current.Multiplier * Best.Res
      }
   }
 
# Construct the data set with the lowest RMSR correlation (step 13) --------------------------------------------

   Fact.Anal <- Factor.Analysis(Best.Corr, Corr.Matrix = TRUE, N.Factors = N.Factors)
   if (N.Factors == 1) Shared.Load[,1] <- Fact.Anal$loadings
   else
      for (i in 1:N.Factors)
         Shared.Load[,i] <- Fact.Anal$loadings[,i]
   Shared.Load[Shared.Load > 1] <- 1
   Shared.Load[Shared.Load < -1] <- -1
   if (Shared.Load[1,1] < 0) {Shared.Load <- Shared.Load * -1
   for (i in 1:k)
      if (sum(Shared.Load[i,] * Shared.Load[i,]) < 1) Unique.Load[i,1] <-(1 - sum(Shared.Load[i,] * Shared.Load[i,]))
      else Unique.Load[i,1] <- 0
   Unique.Load <- sqrt(Unique.Load)
   for (i in 1:k){
      Data[,i] <- (Shared.Comp %*% t(Shared.Load))[,i] + Unique.Comp[,i] * Unique.Load[i,1]
	}
   Data <- apply(Data, 2, scale) # standardizes each variable in the matrix
   for (i in 1:k){
      Data <- Data[sort.list(Data[,i]),]
      Data[,i] <- Distributions[,i]
   }
	# Return the simulated data set (step 14)
   return(Data)
}

################################################################################################################
Factor.Analysis <- function(Data, Corr.Matrix = FALSE, Max.Iter = 50, N.Factors = 0) {
   Data <- as.matrix(Data)
   k <- dim(Data)[2]
   if (N.Factors == 0){
      N.Factors <- k
      Determine <- T
   }else {
   	Determine <- F
   }
   if (!Corr.Matrix){
   	Cor.Matrix <- cor(Data)	
   } else{
   	Cor.Matrix <- Data	
   } 
   Criterion <- .001
   Old.H2    <- rep(99, k)
   H2        <- rep(0, k)
   Change    <- 1
   Iter      <- 0
   Factor.Loadings <- matrix(nrow = k, ncol = N.Factors)
   while ((Change >= Criterion) & (Iter < Max.Iter)) {
      Iter <- Iter + 1
      Eig <- eigen(Cor.Matrix)
      L <- sqrt(Eig$values[1:N.Factors])
	  for (i in 1:N.Factors){
		  Factor.Loadings[,i] <- Eig$vectors[,i] * L[i]
	  }
      for (i in 1:k){
         H2[i] <- sum(Factor.Loadings[i,] * Factor.Loadings[i,])
  		}
		Change <- max(abs(Old.H2 - H2))
      Old.H2 <- H2
      diag(Cor.Matrix) <- H2
   }
   if (Determine) {
	   N.Factors <- sum(Eig$values > 1)
   }
   return(list(loadings = Factor.Loadings[,1:N.Factors], factors = N.Factors))
}
