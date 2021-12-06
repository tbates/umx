# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_residuals.MxModel.r") 
# test_package("umx")
# TODO make tests for residuals!
# [] need to get the text output test working
# [] need to test suppress,
# [] digits
# [] Latents in RAM
# [] Latents non-RAM !
context("umx_score_scale()")

test_that("umx_score_scale works", {
	require(umx)
	library(psych)
	library(psychTools)
	data(bfi)
	# ==============================
	# = Score Agreeableness totals =
	# ==============================

	# Handscore subject 1
	# A1(R)+A2+A3+A4+A5 = (6+1)-2 +4+3+4+4  = 20
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data= bfi, name = "A")
	expect_equal(tmp[1,"A"], 20)
	# ====================
	# = Request the mean =
	# ====================
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data= bfi, name = "A", score = "mean")
	expect_equal(tmp[1,"A"], 4)

	# ================
	# = na.rm=TRUE ! =
	# ================
	tmpDF = bfi
	tmpDF[1, "A1"] = NA
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data= tmpDF, score="mean")
	expect_equal(tmp$A_score[1], 3.75)

	tmp= umx_score_scale("A", pos= 2:5, rev= 1, max = 6, data = tmpDF, score="mean", na.rm=FALSE)
	expect_true( is.na(tmp$A_score[1]) )

	# ===============
	# = Score = max =
	# ===============
	# Subject 1 max = 5 (the reversed item 1)
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, score = "max", data=bfi)
	expect_equal(tmp$A_score[1], 5)

	# =======================
	# = MapStrings examples =
	# =======================
	data(bfi)
	
	bfi= umx_score_scale(name="A" , base="A", pos=2:5, rev=1, max=6, data=bfi)
	mapStrings = c(
	   "Very Inaccurate", "Moderately Inaccurate", 
	   "Slightly Inaccurate", "Slightly Accurate",
	   "Moderately Accurate", "Very Accurate")
	bfi$As1 = factor(bfi$A1, levels = 1:6, labels = mapStrings)
	bfi$As2 = factor(bfi$A2, levels = 1:6, labels = mapStrings)
	bfi$As3 = factor(bfi$A3, levels = 1:6, labels = mapStrings)
	bfi$As4 = factor(bfi$A4, levels = 1:6, labels = mapStrings)
	bfi$As5 = factor(bfi$A5, levels = 1:6, labels = mapStrings)
	bfi= umx_score_scale(name="As", base="As", pos=2:5, rev=1, mapStrings = mapStrings, data= bfi)
	expect_equal(bfi$A, bfi$As)

	bfi$Astr1 = as.character(bfi$As1)
	bfi$Astr2 = as.character(bfi$As2)
	bfi$Astr3 = as.character(bfi$As3)
	bfi$Astr4 = as.character(bfi$As4)
	bfi$Astr5 = as.character(bfi$As5)
	bfi = umx_score_scale(name="Astr", base="Astr", pos=2:5, rev=1, mapStrings = mapStrings, data= bfi)

	expect_equal(bfi$A, bfi$Astr)
	# copes with bad name requests
	expect_error( umx_score_scale(base = "NotPresent", pos=2:5, rev=1, max=6, data=bfi) )

})
