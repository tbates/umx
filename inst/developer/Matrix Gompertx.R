#
# Mx script to fit Gompertz growth curve to data collected from
# twins on nocc occasions of measurement
#
nOcc   = 4
nOccx2 = 8

# Group 1: compute factor loading vectors
# Matrices declare matrices
mxMatrix(name="A", type="Full", nrow=1, ncol=1, free=TRUE, values= 100)
mxMatrix(name="I", type="Full", nrow=1, ncol=1, free=TRUE, values= 10)
mxMatrix(name="R", type="Full", nrow=1, ncol=1, free=TRUE, values= 1)
# A full 1 1 free  # asymptote parameter
# I full 1 1 free  # initial parameter
# R full 1 1 free  # rate parameter
mxMatrix(name="T", type="Full", nrow=nOcc, ncol=1, free=TRUE, values=c(1:nOcc))
mxMatrix(name="U", type="Full", nrow=nOcc, ncol=1, free=TRUE, values=.3)
# T full nOcc 1    # vector of occasions t = 1,...nOcc
# U unit nOcc 1
# End Matrices

# A few starting values
# Matrix T 1 2 3 4 5 6 7 8 9 10 # extend if nOcc > 10
# Start 100  a 1 1
# Start 10 i 1 1
# Start 1 r 1 1
# Boundaries for the parameters
Bound -500 500 all 

# Now we use matrix algebra to compute the vectors and paste them together
# into a single matrix of factor loadings, F
mxAlgebra((U- \exp(-(t-u)@r)). ( \EXP ((\ln(i%a))@\exp(-(T-U)@r)) ), name = "K")
K = (U- \exp(-(t-u)@r)). ( \EXP ((\ln(i%a))@\exp(-(T-U)@r)) ) ;
L = (a%i)@( \exp((-(T-U)@r)+(\ln(i%a)@\exp(-(t-u)@r))) ) ;
M = ((-a.\ln(i%a))@(t-u)).( \exp((-(T-U)@r)+(\ln(i%a)@\exp(-(t-u)@r))) ) ;
F = K|L|M ;
End Algebra;

End Group

Group 2 Compute the MZ and DZ twin pairs' predicted factor covariances
Calculation
Matrices
H full 1 1       # to put .5 in for DZ twin genetic covariance
X Lower 3 3 free # lower triangular decomposition used initially
Y Lower 3 3 free
Z Lower 3 3 free
End Matrices

Matrix H .5
Start  2  X 1 1  Y 1 1  Z 1 1
start .1  X 2 2  X 3 3  Y 2 2  Y 3 3  Z 2 2  Z 3 3

Begin Algebra;
A = X*X' ;
C = Y*Y' ;
E = Z*Z' ;
M =  A + C + E | A + C _
         A + C | A + C + E ;
D =  A + C + E | h@A + C _  
          h@A + C | A + C + E /
End Algebra;
End Group

Group 3 fit the model to the MZ data
Data Ninput=nOccx2 nobs=75
CMatrix full file=bayleymz.cov # observed cov matrix
Mean file=bayleymz.mean        # observed means

Matrices
F Computed = F1 # Factor loading matrix
R Computed = M2 # MZ factor covariances
A diag nOcc nOcc free # Residual genetic factors (occasion specific)
C diag nOcc nOcc free # Residual C factors
E diag nOcc nOcc free # Residual E factors
I iden 2 2
U unit 1 2
V unit 2 2
X full 3 1 
End Matrices

Specify X 1 2 0
Start 3 E 1 1 to E 4 4
Bound .5 20 E 1 1 to E 4 4

Means U@(F*X)' /
Covariance (I@F) & R + I@(E*E')+V@(C*C'+A*A') /
Option rs 
End Group

Group 4 fit the model to the DZ data
Data ninput=nOccx2 nobs=75
CMatrix full file=bayleydz.cov
means file=bayleydz.mean

Matrices
F Computed = F1 # Factor loading matrix
R Computed = D2 # DZ factor covariances
A diag nOcc nOcc =A3 # Residual genetic factors (occasion specific)
C diag nOcc nOcc =C3 # Residual C factors
E diag nOcc nOcc =E3 # Residual E factors
H Full 1 1 = H2
I Iden 2 2
U unit 1 2
V unit 2 2
X full 3 1 =X3

Means U@(F*X)' /
Covariance (I@F)&R  + I@(E*E')+V@(C*C')+(H@(V+I))@(A*A') /
Option rs 
End Group


# Group 5: Aftermath.  Compute proportions of covariance in growth components
Calculation
Matrices
G Computed = A2
C Computed = C2
E Computed = E2
End Matrices

Begin Algebra;
P = G + C + E;
R =  \Stnd(P);
I = G%P;
J = C%P;
K = E%P;
End Algebra;

Options iterations=1000 multiple_fit ndecimals=2
End Group

# fit submodels from above solution
# 1. make environment diagonal
save baygom.mxs
drop 17 19 20
opt mu
end