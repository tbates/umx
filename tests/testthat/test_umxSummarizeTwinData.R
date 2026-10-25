# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxSummarizeTwinData.R")

require(umx)
data(twinData)
td = twinData
td$obese1 = as.numeric(td$bmi1 > 22)
td$obese2 = as.numeric(td$bmi2 > 22)
td$wtc1 = cut(td$wt1, c(-Inf, 55, 70, Inf), labels = c("lo", "mid", "hi"))
td$wtc2 = cut(td$wt2, c(-Inf, 55, 70, Inf), labels = c("lo", "mid", "hi"))
quiet = function(expr) { out = NULL; invisible(capture.output(suppressMessages(out <- expr))); out }

test_that("umxSummarizeTwinData continuous output is unchanged", {
	r = quiet(umxSummarizeTwinData(twinData, sep = "", selVars = c("wt", "ht")))
	expect_equal(names(r), c("Var", "Mean", "SD", "rMZFF", "rMZMM", "rDZFF", "rDZMM", "rDZOS"))
	expect_true(is.numeric(r$Mean))
	expect_equal(r$Mean[1], round(mean(c(twinData$wt1, twinData$wt2), na.rm = TRUE), 2))
	r2 = quiet(umxSummarizeTwinData(twinData, sep = "", selVars = "wt", MZ = c("MZFF", "MZMM"), DZ = c("DZFF", "DZMM", "DZOS")))
	expect_equal(names(r2), c("Var", "N (MZ/DZ)", "Mean", "SD", "rMZ", "rDZ"))
})

test_that("umxSummarizeTwinData reports endorsed (SE), thresholds (SE), and polychorics for binary/ordinal", {
	r = quiet(umxSummarizeTwinData(td, sep = "", selVars = c("wt", "obese", "wtc"), MZ = c("MZFF", "MZMM"), DZ = c("DZFF", "DZMM", "DZOS"), show = "se"))
	expect_true("Thresholds" %in% names(r))
	p0 = mean(c(td$obese1, td$obese2) == 0, na.rm = TRUE)
	expect_equal(as.numeric(sub(" .*", "", r$Thresholds[2])), round(qnorm(p0), 2))
	expect_equal(as.numeric(sub(" .*", "", r$Mean[2])), round(1 - p0, 2))
	expect_length(strsplit(r$Thresholds[3], "; ")[[1]], 2)
	# polychoric r (SE) agrees with umx_polychoric on the MZ pairs
	mz = td[td$zygosity %in% c("MZFF", "MZMM"), ]
	pc = umx_polychoric(data.frame(a = umxFactor(mz$obese1), b = umxFactor(mz$obese2)))$polychorics[1, 2]
	expect_equal(as.numeric(sub(" .*", "", r$rMZ[2])), round(pc, 2))
	expect_match(r$rMZ[2], "^[0-9.]+ \\([0-9.]+\\)$")
})

test_that("umxSummarizeTwinData splits MZ/DZ by sex", {
	ref = quiet(umxSummarizeTwinData(twinData, sep = "", selVars = "wt"))

	t3 = twinData
	t3$zyg2  = ifelse(grepl("^MZ", t3$zygosity), "MZ", "DZ")
	t3$sex_1 = ifelse(t3$zygosity %in% c("MZFF", "DZFF", "DZOS"), "F", "M")
	t3$sex_2 = ifelse(t3$zygosity %in% c("MZFF", "DZFF"), "F", ifelse(t3$zygosity == "DZOS", "M", "M"))
	r3 = quiet(umxSummarizeTwinData(t3, sep = "", selVars = "wt", zyg = "zyg2", sex = "sex_"))
	expect_equal(r3, ref)
})

test_that("umxSummarizeTwinData warns and uses unsplit levels when sex codes match nothing", {
	t = twinData
	t$zyg2  = ifelse(grepl("^MZ", t$zygosity), "MZ", "DZ")
	t$sex_1 = 1L
	t$sex_2 = 2L
	expect_warning(r <- quiet(umxSummarizeTwinData(t, sep = "", selVars = "wt", zyg = "zyg2", sex = "sex_")), "no pair sexes recognized")
	expect_equal(names(r), c("Var", "Mean", "SD", "rDZ", "rMZ"))
})
