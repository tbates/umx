#
#  Copyright 2007-2017 Timothy C. Bates
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
# 
#        https://www.apache.org/licenses/LICENSE-2.0
# 
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.

# How I coded this data from the Boulder example
#
# GFF = read.table("~/bin/umx/data/DHBQ_bs.dat", header = T, sep = "\t", as.is = c(T), na.strings = -999)
# x   = umx_rename(GFF, old = "zyg2"     , replace = "zyg_2grp"); names(x)
# x   = umx_rename(x  , old = "zyg"      , replace = "zyg_6grp"); names(x)
# x   = umx_rename(x , grep = "([12bs])$", replace = "_T\\1")   ; names(x)
# x$sex_T1 = factor(x$sex_T1, levels = 0:1, labels = c("male", "female"))
# x$sex_T2 = factor(x$sex_T2, levels = 0:1, labels = c("male", "female"))
# x$sex_Tb = factor(x$sex_Tb, levels = 0:1, labels = c("male", "female"))
# x$sex_Ts = factor(x$sex_Ts, levels = 0:1, labels = c("male", "female"))
# x$zyg_6grp = factor(x$zyg_6grp, levels = 1:6, labels = c("MZMM", "DZMM", "MZFF", "DZFF", "DZFM", "DZMF"))
# GFF$zyg_2grp = factor(GFF$zyg_2grp, levels = 1:2, labels = c("MZ", "DZ"))

# GFF = GFF[, c("zyg_6grp", "zyg_2grp", "divorce", "sex_T1", "age_T1", "gff_T1", "fc_T1", "qol_T1", "hap_T1", "sat_T1", "AD_T1", "SOMA_T1", "SOC_T1", "THOU_T1", "sex_T2", "age_T2", "gff_T2", "fc_T2", "qol_T2", "hap_T2", "sat_T2", "AD_T2", "SOMA_T2", "SOC_T2", "THOU_T2", "sex_Tb", "age_Tb", "gff_Tb", "fc_Tb","qol_Tb", "hap_Tb", "sat_Tb", "AD_Tb","SOMA_Tb","SOC_Tb", "THOU_Tb","sex_Ts", "age_Ts", "gff_Ts", "fc_Ts", "qol_Ts", "hap_Ts", "sat_Ts", "AD_Ts","SOMA_Ts","SOC_Ts", "THOU_Ts")]

# save("GFF", file = "GFF.rda")
# system(paste("open ",shQuote(getwd(), type = "csh")))
# update_wordlist get_wordlist(pkg = "~/bin/umx")

# 1. Figure out what things are.

# table(x$sex_Tb) # all 0 so male = 0
# table(x$sex_Ts) # all 1 so female = 1
# umx_aggregate(sex_T2 ~ zyg_6grp, data = x)
# |zyg_6grp    |sex_T2             |
# |:-----------|:------------------|
# |1 (n = 448) |male 448; female 0 |
# |2 (n = 389) |male 389; female 0 |
# |3 (n = 668) |male 0; female 668 |
# |4 (n = 484) |male 0; female 484 |
# |5 (n = 504) |male 0; female 504 |
# |6 (n = 407) |male 407; female 0 |
# umx_aggregate(sex_T1 ~ zyg_6grp, data = x)
# |zyg_6grp    |sex_T1             |
# |:-----------|:------------------|
# |1 (n = 457) |male 457; female 0 |
# |2 (n = 391) |male 391; female 0 |
# |3 (n = 661) |male 0; female 661 |
# |4 (n = 478) |male 0; female 478 |
# |5 (n = 426) |male 426; female 0 |
# |6 (n = 460) |male 0; female 460 |

# ===================================
# = General Family Functioning data =
# ===================================

#' Twin data: General Family Functioning, divorce, and wellbeing.
#'
#' Measures of family functioning, happiness and related variables in twins, and
#' their brothers and sisters. (see details)
#'
#' @details
#' Several scales in the data are described in  van der Aa et al. (2010).
#' General Family Functioning (GFF) refers to adolescents' evaluations general family health
#' vs. pathology. It assesses problem solving, communication, roles within the household,
#' affection, and control. GFF was assessed with a Dutch translation of the General Functioning
#' sub-scale of the McMaster Family Assessment Device (FAD) (Epstein et al., 1983).
#' 
#' Family Conflict (FC) refers to adolescents' evaluations of the amount of openly 
#' expressed anger, aggression, and conflict among family members. Conflict
#' sub-scale of the Family Environment Scale (FES) (Moos, 1974)
#' 
#' Quality of life in general (QLg) was assessed with the 10-step Cantril
#' Ladder from best- to worst-possible life (Cantril, 1965).
#' 
#' \describe{
#'   \item{zyg_6grp}{Six-level measure of zygosity: 'MZMM', 'DZMM', 'MZFF', 'DZFF', 'DZMF', 'DZFM'}
#'   \item{zyg_2grp}{Two-level measure of zygosity: 'MZ', 'DZ'}
#'   \item{divorce}{Parental divorce status: 0 = No, 1 = Yes}
#'   \item{sex_T1}{Sex of twin 1: 0 = "male", 1 = "female"}
#'   \item{age_T1}{Age of twin 1 (years)}
#'   \item{gff_T1}{General family functioning for twin 1}
#'   \item{fc_T1}{Family conflict sub-scale of the FES}
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
#'   \item{fc_T2}{Family conflict sub-scale of the FES}
#'   \item{qol_T2}{Quality of life for twin 2}
#'   \item{hap_T2}{General happiness for twin 2}
#'   \item{sat_T2}{Satisfaction with life for twin 2}
#'   \item{AD_T2}{Anxiety and Depression for twin 2}
#'   \item{SOMA_T2}{Somatic complaints for twin 2}
#'   \item{SOC_T2}{Social problems for twin 2}
#'   \item{THOU_T2}{Thought disorder problems for twin 2}
#'   \item{sex_Ta}{Sex of sib 1}
#'   \item{age_Ta}{Age of sib 1}
#'   \item{gff_Ta}{General family functioning for sib 1}
#'   \item{fc_Ta}{Family conflict sub-scale of the FES}
#'   \item{qol_Ta}{Quality of life for sib 1}
#'   \item{hap_Ta}{General happiness for sib 1}
#'   \item{sat_Ta}{Satisfaction with life for sib 1}
#'   \item{AD_Ta}{Anxiety and Depression for sib 1}
#'   \item{SOMA_Ta}{Somatic complaints for sib 1}
#'   \item{SOC_Ta}{Social problems for sib 1}
#'   \item{THOU_Ta}{Thought disorder problems for sib 1}
#'   \item{sex_Ts}{Sex of sib 2}
#'   \item{age_Ts}{Age of sib 2}
#'   \item{gff_Ts}{General family functioning for sib 2}
#'   \item{fc_Ts}{Family conflict sub-scale of the FES}
#'   \item{qol_Ts}{Quality of life for sib 2}
#'   \item{hap_Ts}{General happiness for sib 2}
#'   \item{sat_Ts}{Satisfaction with life for sib 2}
#'   \item{AD_Ts}{Anxiety and Depression for sib 2}
#'   \item{SOMA_Ts}{Somatic complaints for sib 2}
#'   \item{SOC_Ts}{Social problems for sib 2}
#'   \item{THOU_Ts}{Thought disorder problems for sib 2}
#' }
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name GFF
#' @usage data(GFF)
#' @format A data frame with 1000 rows and 8 variables:
#' @references van der Aa, N., Boomsma, D. I., Rebollo-Mesa, I., Hudziak, J. J., & Bartels, 
#' M. (2010). Moderation of genetic factors by parental divorce in adolescents' 
#' evaluations of family functioning and subjective wellbeing. Twin Research 
#' and Human Genetics, 13(2), 143-162. doi:10.1375/twin.13.2.143
#' @examples
#' # Twin 1 variables (end in '_T1')
#' data(GFF)
#' umx_names(GFF, "1$") # Just variables ending in 1 (twin 1)
#' str(GFF) # first few rows
#' 
#' m1 = umxACE(selDVs= "gff", sep = "_T",
#' 	mzData = subset(GFF, zyg_2grp == "MZ"), 
#' 	dzData = subset(GFF, zyg_2grp == "DZ")
#' )
#'
NULL

# ================================
# = Anthropometric data on twins =
# ================================
#' Anthropometric data on twins
#'
#' A dataset containing height, weight, BMI, and skin-fold fat measures in several
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
#'   \item ssc_T1 Subscapular Skinfold of twin 1
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
#' @family datasets
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

# Load Data
# iqdat = read.table(file = "~/Desktop/IQ.txt", header = TRUE)
# iqdat$zygosity = NA
# iqdat$zygosity[iqdat$zyg %in% 1] = "MZ"
# iqdat$zygosity[iqdat$zyg %in% 2] = "DZ"
# iqdat = iqdat[, c('zygosity','IQ1_T1','IQ2_T1','IQ3_T1','IQ4_T1','IQ1_T2','IQ2_T2','IQ3_T2','IQ4_T2')]
# head(iqdat); dim(iqdat); str(iqdat)
# names(iqdat) = c('zygosity', 'IQ_age1_T1','IQ_age2_T1','IQ_age3_T1','IQ_age4_T1','IQ_age1_T2','IQ_age2_T2','IQ_age3_T2','IQ_age4_T2')
# save("iqdat", file = "iqdat.rda")
# system(paste("open ",shQuote(getwd(), type = "csh")))

# ==============================
# = IQ measured longitudinally =
# ==============================
#' Twin data: IQ measured longitudinally
#'
#' Measures of IQ across four ages in pairs of identical and fraternal twins. (see details)
#'
#' @details
#' #TODO complete details
#' \itemize{
#'   \item zygosity Zygosity (MZ or DZ)
#'   \item IQ_age1_T1 T1 IQ measured at age 1
#'   \item IQ_age2_T1 T1 IQ measured at age 2
#'   \item IQ_age3_T1 T1 IQ measured at age 3
#'   \item IQ_age4_T1 T1 IQ measured at age 4
#'   \item IQ_age1_T2 T2 IQ measured at age 1
#'   \item IQ_age2_T2 T2 IQ measured at age 2
#'   \item IQ_age3_T2 T2 IQ measured at age 3
#'   \item IQ_age4_T2 T2 IQ measured at age 4
#' }
#' 
#' @docType data
#' @keywords datasets
#' @family datasets
#' @name iqdat
#' @references TODO
#' @usage data(iqdat)
#' @format A data frame with 562 rows and 9 variables
#' @examples
#' data(iqdat)
#' str(iqdat)
# # TODO make a cowplot example
#' par(mfrow = c(1, 2))  # 1 rows and 3 columns
#' plot(IQ_age4_T1 ~ IQ_age4_T2, ylim = c(50, 150), data = subset(iqdat, zygosity == "MZ"))
#' plot(IQ_age4_T1 ~ IQ_age4_T2, ylim = c(50, 150), data = subset(iqdat, zygosity == "DZ"))
#' plot(IQ_age1_T1 ~ IQ_age4_T2, data = subset(iqdat, zygosity == "MZ"))
#' par(mfrow = c(1, 1))  # back to as it was
NULL