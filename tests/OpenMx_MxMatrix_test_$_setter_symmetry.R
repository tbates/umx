library(OpenMx)
symmMatrix <- mxMatrix("Symm", nrow = 3, ncol = 3)

symmMatrix$values[1,2] <- 2
# symmMatrix$values[2,1] <- 2 # should be handled implicitly??
# omxCheckIdentical(symmMatrix@values[2,1], symmMatrix@values[1,2])
