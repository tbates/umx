library(OpenMx)
m <- mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:9)
m <- mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:8) # too few
m <- mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:10) # too many
m <- mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:3) # one row exactly: let's assume that's by choice"

m <- mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:6) # let's say "warning: you gave a sub-multiple of values, and I recycled x rows..."

# omxCheckWarning( mxMatrix(name = "m", "Full", nrow = 3, ncol = 3, values = 1:6), '')