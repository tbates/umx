#' Anthropometric data on twins
#'
#' A dataset containing height, weight, bmi, and skin-fold fat measures in several
#' hundred US twin families participating in the MCV Cardiovascular Twin Study (PI Schieken)
#' 
#' \itemize{
#'   \item fan FamilyID (t1=male,t2=female)
#'   \item zyg Zygosity  1:mzm, 2:mzf, 3:dzm, 4:dzf, 5:dzo
#'   \item ht_T1  Height of twin 1 (cm)
#'   \item wt_T1  Weight of twin 1 (kg)
#'   \item bmi_T1 BMI of twin 1
#'   \item bml_T1 BMI of twin 1
#'   \item bic_T1 Biceps Skinfold of twin 1
#'   \item caf_T1 Calf Skinfold of twin 1
#'   \item ssc_T1 Subscapular SSkinfold of twin 1
#'   \item sil_T1 Suprailiacal Skinfold of twin 1
#'   \item tri_T1 Triceps Skinfold of twin 1
#'   \item ht_T2  Height of twin 2
#'   \item wt_T2  Weight of twin 2
#'   \item bmi_T2 BMI of twin 2
#'   \item bml_T2 BMI of twin 2
#'   \item bic_T2 Biceps Skinfold of twin 2
#'   \item caf_T2 Calf Skinfold of twin 2
#'   \item ssc_T2 Subscapular Skinfold of twin 2
#'   \item sil_T2 Suprailiacal Skinfold of twin 2
#'   \item tri_T2 Triceps Skinfold of twin 2
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name us_skinfold_data
#' @references Moskowitz, W. B., Schwartz, P. F., & Schieken, R. M. (1999).
#' Childhood passive smoking, race, and coronary artery disease risk:
#' the MCV Twin Study. Medical College of Virginia.
#' Archives of Pediatrics and Adolescent Medicine, \strong{153}, 446-453.
#' \url{http://www.ncbi.nlm.nih.gov/pubmed/10323623}
#' @usage data(us_skinfold_data)
#' @format A data frame with 53940 rows and 10 variables
#' @examples
#' data(us_skinfold_data)
#' str(us_skinfold_data)
#' par(mfrow = c(1, 2))  # 1 rows and 3 columns
#' plot(ht_T1 ~ht_T2, ylim = c(130, 165), data = subset(us_skinfold_data, zyg == 1))
#' plot(ht_T1 ~ht_T2, ylim = c(130, 165), data = subset(us_skinfold_data, zyg == 3))
#' par(mfrow = c(1, 1))  # back to as it was
NULL

#' Simulated Mendelian Randomization data
#'
#' A dataset containing a variable of interest (Y), a putative cause (X), a qtl 
#' known to influence X, and a confounding variable (U) affecting both X and Y.
#' 
#' \itemize{
#'   \item X A putative causal variable affecting Y.
#'   \item Y  The dependent variable of interest.
#'   \item qtl A genetic polymorphism or "quantitative trait locus" affecting X.
#'   \item U A confounding variable, affecting both X and Y.
#' }
#' 
#' @docType data
#' @keywords datasets
#' @name MR_data
#' @usage data(MR_data)
#' @format data frame with 50,000 rows and 4 variables
#' @examples
#' \dontrun{
#' data(MR_data)
#' str(MR_data)
#' m1 = umxTwoStage(Y ~ X, ~qtl, data = MR_data)
#' plot(m1)
#' 
#' # ========================================================
#' # =            The code to make these Data               =
#' # = Modified from Dave Evan's 2016 Boulder workshop talk =
#' # ========================================================
#' set.seed(999)   # Set seed for random number generator
#' nInd  = 50000    # 50,000 Individuals
#' Vqtl  = 0.02     # Variance of QTL affecting causal variable X
#' b_qtl_x  = sqrt(Vqtl) # Path coefficient between SNP and X
#' b_xy  = 0.1      # Causal effect of X on Y
#' b_ux  = 0.5      # Confounding effect of U on X
#' b_uy  = 0.5      # Confounding effect of U on Y
#' p     = 0.5      # Decreaser allele frequency
#' q     = 1 - p    # Increaser allele frequency
#' a = sqrt(1/(2 * p * q)) # Genotypic value for genetic variable of variance 1.0
#' 
#' # Residual variance in variable X (so variance adds up to one)
#' Vex  <- (1- Vqtl - b_ux^2)
#' sdex <- sqrt(Vex) # Residual standard error in variable X
#' 
#' # Residual variance for Y variable (so var adds up to 1.0)
#' Vey = 1 - (b_xy^2 + 2*b_xy*b_ux*b_uy + b_uy^2) 
#' sdey <- sqrt(Vey) # Residual standard error in variable Y
#' 
#' # Simulate individual genotypic and phenotypic values
#' qtl <- sample(c(-a,0,a), nInd, replace = TRUE, prob = c(p^2, 2*p*q, q^2)) 
#' U <- rnorm(nInd, 0, 1) #Confounding variables
#' X <- b_qtl_x * qtl + b_ux * U + rnorm(nInd, 0, sdex) # X variable
#' Y <- b_xy * X + b_uy * U + rnorm(nInd, 0, sdey) # Y variable
#' #
# Recode SNP qtl using traditional 0, 1, 2 coding
#' qtl <- replace(qtl, qtl ==  a, 2)
#' qtl <- replace(qtl, qtl ==  0, 1)
#' qtl <- replace(qtl, qtl == -a, 0)
#' #
#' MR_data = data.frame(U = U, X = X, Y = Y, qtl = qtl)
#' save(MR_data, file = "~/bin/umx/data/MR_data.rda")
#' }

NULL