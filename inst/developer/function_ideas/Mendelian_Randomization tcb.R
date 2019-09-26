library(umx)
library(sem) # May require you to install X11

# =================
# = Simulate Data =
# =================

set.seed(999)    # Set seed for random number generator
nInd <- 100000   # 100,000 Individuals
Vq   <- 0.02     # Variance of QTL Z (which affects variable X
b_zx <- sqrt(Vq) # Path coefficient between SNP and variable X
b_xy <- 0.1      # Causal effect of X on Y
b_ux <- 0.5      # Confounding effect of U on X
b_uy <- 0.5      # Confounding effect of U on Y
p <- 0.5         # Decreaser allele frequency
q <- 1-p         # Increaser allele frequency
a <- sqrt(1/(2 * p * q)) # Genotypic value for genetic variable of variance 1.0

Vex  <- (1- Vq - b_ux^2) # Residual variance in variable X (so variance adds up to one)
sdex <- sqrt(Vex)        # Residual standard error in variable X

Vey = 1 - (b_xy^2 + 2 * b_xy * b_ux * b_uy + b_uy^2) # Residual variance for Y variable (so var adds up to 1.0)
sdey <- sqrt(Vey) # Residual standard error in variable Y

# Simulate individuals
Z <- sample(c(-a, 0, a), nInd, replace = TRUE, prob = c(p^2, 2 * p * q, q^2)) # Simulate genotypic values
U <- rnorm(nInd, 0, 1) #Confounding variables
X <- b_zx * Z + b_ux * U + rnorm(nInd, 0, sdex) # X variable
Y <- b_xy * X + b_uy * U + rnorm(nInd, 0, sdey) # Y variable

# Recode SNP Z using traditional 0, 1, 2 coding
Z <- replace(Z, Z ==  a, 2)
Z <- replace(Z, Z ==  0, 1)
Z <- replace(Z, Z == -a, 0)

df = data.frame(U = U, X = X, Y = Y, Z = Z)

# Analysis one: Ordinary least squares regression (WARNING this result is CONFOUNDED!!)
m1 = lm(Y ~ X    , data = df); coef(m1) # "appears" that Y is caused by X:  𝛽= .35
m1 = lm(Y ~ X + U, data = df); coef(m1) # Controlling U reveals the true link: 𝛽= 0.1

# ====================================
# = Mendelian randomization analysis =
# ====================================

# =========================================================================
# = Implementation 1: Two stage least squares using tsls from sem library =
# =========================================================================
m1 = tsls(formula = Y ~ X, instruments = ~ Z, data = df)
coef(m1)
#                 Estimate  Std. Error   t value     Pr(>|t|)
# (Intercept) 0.0009797078 0.003053891 0.3208064 7.483577e-01
# X           0.1013835358 0.021147133 4.7941976 1.635616e-06

# X effect now (correctly) estimated at .1 !!

# ================================================
# = Implementation 2: Model in OpenMx            =
# ================================================
manifests <- c("Z","X","Y")
latents   <- c("e1", "e2")

IVModel <- mxModel("IV Model", type="RAM",
	manifestVars = manifests,
	latentVars = latents,
	mxPath(from = c("Z"), arrows = 2, free = TRUE, values = 1, labels = c("Z") ),  #Variance of SNP 
	mxPath(from = "e1", to = "X", arrows = 1, free = FALSE, values = 1, labels = "e1"), # Residual error X variable. Value set to 1.
	mxPath(from = "e2", to = "Y", arrows = 1, free = FALSE, values = 1, labels = "e2"), # Residual error Y variable. Value set to 1.
	mxPath(from = latents, arrows = 2, free = TRUE, values = 1, labels = c("var_e1", "var_e2") ), # Variance of residual errors
	mxPath(from = "e1", to = "e2", arrows = 2, free = TRUE, values = 0.2, labels = "phi" ), # Correlation between residual errors
	mxPath(from = "Z",  to = "X", arrows = 1, free = TRUE, values = 1, labels = "b_zx"), # SNP effect on X variable
	mxPath(from = "X",  to = "Y", arrows = 1, free = TRUE, values = 0, labels = "b_xy"), # Causal effect of X on Y
	# means and intercepts
	mxPath(from = "one", to = c("Z", "X", "Y"), arrows = 1, free = TRUE, values = 1, labels = c("meansnp", "alpha0", "alpha1") ),
	mxData(df, type = "raw")
)

IVRegFit <- mxRun(IVModel); umx_time(IVRegFit)# IV Model: 14.34 seconds for 100,000 subjects
coef(IVRegFit)
plot(IVRegFit, std = F, showFixed = T, showMeans = F, digits = 3)

# ================================================
# = Implementation 3: Equivalent model in umxRAM =
# ================================================
m3 <- umxRAM("myMR", data = df, autoRun = F,
	umxPath(v.m. = "Z"),
	umxPath(v.m. = "X"),
	umxPath(v.m. = "Y"),
	umxPath("Z", to = "X"),
	umxPath("X", to = "Y")
)
m3 <- umxModify(m3, "X_with_Y", free = T, value = .2)
plot(m3, std = F, digits = 3)