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