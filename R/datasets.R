#
#  Copyright 2007-2017 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        http://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

#' Twin data: General Family Functioning, divorce, and wellbeing.
#'
#' Measures of family functioning, happiness and related variables in twins, and
#' their brothers and sisters. (see details)
#'
#' @details
#' Several scales in the data are described in  van der Aa et al (2010).
#' General Family Functioning (GFF) refers to adolescents' evaluations general family health
#' vs. pathology. It assesses problem solving, communication, roles within the household,
#' affection, and control. GFF was assessed with a Dutch translation of the General Functioning
#' subscale of the McMaster Family Assessment Device (FAD) (Epstein et al., 1983).
#' 
#' Family Conflict (FC) refers to adolescents' evaluations of the amount of openly 
#' expressed anger, aggression, and conflict among family members. Conflict
#' sub-scale of the Family Environment Scale (FES) (Moos, 1974)
#' 
#' Quality of life in general (QLg) was assessed with the 10-step Cantril
#' Ladder from best- to worst-possible life (Cantril, 1965).
#' 
#' \describe{
#'   \item{divorce}{Parent's divorce status: 0 = No, 1 = Yes}
#'   \item{zyg6}{Six-level measure of zygosity}
#'   \item{zyg2}{Two-level measure of zygosity}
#'
#'   \item{sex_T1}{Sex of twin 1}
#'   \item{age_T1}{Age of twin 1}
#'   \item{gff_T1}{General family functioning for twin 1}
#'   \item{fc_T1}{Family conflict subscale of the FES}
#'   \item{qol_T1}{Quality of life for twin 1}
#'   \item{hap_T1}{General happiness for twin 1}
#'   \item{sat_T1}{Satisfaction with life for twin 1}
#'   \item{AD_T1}{Anxiety and Depression for twin 1}
#'   \item{SOMA_T1}{Somatic complaints for twin 1}
#'   \item{SOC_T1}{Social problems for twin 1}
#'   \item{THOU_T1}{Thought disorder problems for twin 1}
#'   \item{sex_T2}{Sex of twin 2}
#'   \item{age_T2}{Age of twin 2}
#'   \item{gff_T2}{General family functioning for twin 2}
#'   \item{fc_T2}{Family conflict subscale of the FES}
#'   \item{qol_T2}{Quality of life for twin 2}
#'   \item{hap_T2}{General happiness for twin 2}
#'   \item{sat_T2}{Satisfaction with life for twin 2}
#'   \item{AD_T2}{Anxiety and Depression for twin 2}
#'   \item{SOMA_T2}{Somatic complaints for twin 2}
#'   \item{SOC_T2}{Social problems for twin 2}
#'   \item{THOU_T2}{Thought disorder problems for twin 2}
#'   \item{sex_a}{Sex of sib 1}
#'   \item{age_a}{Age of sib 1}
#'   \item{gff_a}{General family functioning for sib 1}
#'   \item{fc_a}{Family conflict subscale of the FES}
#'   \item{qol_a}{Quality of life for sib 1}
#'   \item{hap_a}{General happiness for sib 1}
#'   \item{sat_a}{Satisfaction with life for sib 1}
#'   \item{AD_a}{Anxiety and Depression for sib 1}
#'   \item{SOMA_a}{Somatic complaints for sib 1}
#'   \item{SOC_a}{Social problems for sib 1}
#'   \item{THOU_a}{Thought disorder problems for sib 1}
#'   \item{sex_s}{Sex of sib 2}
#'   \item{age_s}{Age of sib 2}
#'   \item{gff_s}{General family functioning for sib 2}
#'   \item{fc_s}{Family conflict subscale of the FES}
#'   \item{qol_s}{Quality of life for sib 2}
#'   \item{hap_s}{General happiness for sib 2}
#'   \item{sat_s}{Satisfaction with life for sib 2}
#'   \item{AD_s}{Anxiety and Depression for sib 2}
#'   \item{SOMA_s}{Somatic complaints for sib 2}
#'   \item{SOC_s}{Social problems for sib 2}
#'   \item{THOU_s}{Thought disorder problems for sib 2}
#' }
#' @docType data
#' @keywords datasets
#' @name GFF
#' @usage data(GFF)
#' @format A data frame with 1000 rows and 8 variables:
#' @references van der Aa, N., Boomsma, D. I., Rebollo-Mesa, I., Hudziak, J. J., & Bartels, 
#' M. (2010). Moderation of genetic factors by parental divorce in adolescents' 
#' evaluations of family functioning and subjective wellbeing. Twin Research 
#' and Human Genetics, 13(2), 143-162. doi:10.1375/twin.13.2.143
#' @examples
#' # How I coded this data from the Boulder example
#' 
# GFF = read.table("~/bin/umx/data/DHBQ_bs.dat", header = T, sep = "\t", as.is = c(T), na.strings=-999)
# x   = umx_rename(GFF, old = "zyg2"     , replace = "zyg_2grp"); names(x)
# x   = umx_rename(x  , old = "zyg"      , replace = "zyg_6grp"); names(x)
# x   = umx_rename(x , grep = "([12bs])$", replace = "_T\\1")   ; names(x)
# table(x$sex_Tb) # all 0 so male = 0
# table(x$sex_Ts) # all 1 so female = 1
# x$sex_T1 = factor(x$sex_T1, levels = 0:1, labels = c("male", "female"))
# x$sex_T2 = factor(x$sex_T2, levels = 0:1, labels = c("male", "female"))
# x$sex_Tb = factor(x$sex_Tb, levels = 0:1, labels = c("male", "female"))
# x$sex_Ts = factor(x$sex_Ts, levels = 0:1, labels = c("male", "female"))
#

#' data(GFF)
#' # Twin 1 variables (end in '_T1')
#' umx_names(GFF, "1$")
NULL


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
#'   \item bml_T1 log BMI of twin 1
#'   \item bic_T1 Biceps Skinfold of twin 1
#'   \item caf_T1 Calf Skinfold of twin 1
#'   \item ssc_T1 Subscapular SSkinfold of twin 1
#'   \item sil_T1 Suprailiacal Skinfold of twin 1
#'   \item tri_T1 Triceps Skinfold of twin 1
#'   \item ht_T2  Height of twin 2
#'   \item wt_T2  Weight of twin 2
#'   \item bmi_T2 BMI of twin 2
#'   \item bml_T2 log BMI of twin 2
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
#' \url{https://www.ncbi.nlm.nih.gov/pubmed/10323623}
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