#' # =========================================================
#' # Example with Tobin data (this is not profuction quality =
#' # Definately a work in progress!!!                        =
#' # =========================================================
#' 
#' # Analyse a dataset in which only people with a BMI over 22 had their BMI recorded
#' 
#' require(umx)
#' data(twinData)
#' baseNames = c("bmi")
#' selDVs = umx_paste_names(baseNames, "", 1:2)
#' tmp = twinData[, c(selDVs, "zyg")]
#' tmp$bmi1[tmp$bmi1 <= 22] = 22
#' tmp$bmi2[tmp$bmi2 <= 22] = 22
#' tmp = umxFactor(tmp) # BAD! letting it know that there are wide data!
#' tmp = umxFactor(tmp, sep = "") # GOOD same factor for each twin!
#' mz = tmp[tmp$zyg == 1, selDVs]
#' dz = tmp[tmp$zyg == 3, selDVs]
#' x  = umxThresholdMatrix(dz, sep = "", thresholds = "left_censored", verbose = TRUE)
#' m1 = umxACE(selDVs = baseNames, dzData = dz, mzData = mz, suffix = "", thresholds = "left_censored")
#' umxSummary(m1)
#' plot(m1)
