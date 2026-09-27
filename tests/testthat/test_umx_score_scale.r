# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umx_score_scale.r") 
# test_package("umx")
# TODO make tests for residuals!
# [] need to get the text output test working
# [] need to test suppress,
# [] digits
# [] Latents in RAM
# [] Latents non-RAM !

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

test_that("umx_scale_reliabilities accumulates reliabilities", {
	require(umx)
	library(psych)
	library(psychTools)
	data(bfi)
	oldStore = getOption("umx_scale_reliabilities")
	on.exit(options(umx_scale_reliabilities = oldStore), add = TRUE)
	# init starts with an empty three-column store
	umx_scale_reliabilities("init")
	emptyStore = getOption("umx_scale_reliabilities")
	expect_equal(names(emptyStore), c("scale", "reliability", "type"))
	expect_equal(nrow(emptyStore), 0)
	# scoring two scales with alpha = TRUE accumulates alpha + omega_t rows
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data = bfi, name = "A", alpha = TRUE)
	tmp = umx_score_scale("E", pos = 3:5, rev = 1:2, max = 6, data = bfi, name = "E", alpha = TRUE)
	shown = umx_scale_reliabilities("show")
	expect_equal(nrow(shown), 4)
	expect_equal(sort(unique(shown$scale)), c("A", "E"))
	expect_equal(sort(unique(shown$type)), c("alpha", "omega_t"))
	# stored values match independent computation from the same scored items
	dfA = bfi[, paste0("A", 1:5)]
	dfA$A1 = 7 - dfA$A1
	expect_equal(shown[shown$scale == "A" & shown$type == "alpha", "reliability"], as.numeric(umx::reliability(cov(dfA, use = "pairwise.complete.obs"))$alpha))
	# psych::omega warns that omega_h is not meaningful with one factor: same call the production code suppresses
	suppressWarnings({ omegaA = psych::omega(dfA, nfactors = 1) })
	expect_equal(shown[shown$scale == "A" & shown$type == "omega_t", "reliability"], omegaA$omega.tot)
	dfE = bfi[, paste0("E", 1:5)]
	dfE$E1 = 7 - dfE$E1
	dfE$E2 = 7 - dfE$E2
	expect_equal(shown[shown$scale == "E" & shown$type == "alpha", "reliability"], as.numeric(umx::reliability(cov(dfE, use = "pairwise.complete.obs"))$alpha))
	suppressWarnings({ omegaE = psych::omega(dfE, nfactors = 1) })
	expect_equal(shown[shown$scale == "E" & shown$type == "omega_t", "reliability"], omegaE$omega.tot)
	# re-scoring the same scale replaces its rows instead of duplicating them
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data = bfi, name = "A", alpha = TRUE)
	shown = umx_scale_reliabilities("show")
	expect_equal(nrow(shown), 4)
	# omega_h is stored when more than one omega factor is requested
	tmp = umx_score_scale("O", pos = c(1, 3, 4), rev = c(2, 5), max = 6, data = bfi, name = "O", alpha = TRUE, omegaNfactors = 2)
	shown = umx_scale_reliabilities("show")
	expect_equal(nrow(shown), 7)
	expect_equal(sort(shown[shown$scale == "O", "type"]), c("alpha", "omega_h", "omega_t"))
	dfO = bfi[, paste0("O", 1:5)]
	dfO$O2 = 7 - dfO$O2
	dfO$O5 = 7 - dfO$O5
	suppressWarnings({ omegaO = psych::omega(dfO, nfactors = 2) })
	expect_equal(shown[shown$scale == "O" & shown$type == "omega_h", "reliability"], omegaO$omega_h)
	# show accepts a type filter, defaulting to all rows
	alphaOnly = umx_scale_reliabilities("show", type = "alpha")
	expect_equal(nrow(alphaOnly), 3)
	expect_equal(unique(alphaOnly$type), "alpha")
	expect_equal(nrow(umx_scale_reliabilities("show", type = c("alpha", "omega_t"))), 6)
	expect_equal(nrow(umx_scale_reliabilities("show")), 7)
	# unknown types report an empty table with a message
	expect_message(umx_scale_reliabilities("show", type = "omega_x"), "No stored reliabilities of type")
	emptyFilter = umx_scale_reliabilities("show", type = "omega_x")
	expect_equal(nrow(emptyFilter), 0)
	expect_equal(names(emptyFilter), c("scale", "reliability", "type"))
	# digits passes through to the printed table only (stored values keep full precision)
	alphaVal = alphaOnly[alphaOnly$scale == "A", "reliability"]
	lowRes = format(round(alphaVal, 1))
	highRes = format(round(alphaVal, 4))
	oneDigit = capture.output(umx_scale_reliabilities("show", type = "alpha", digits = 1))
	fourDigit = capture.output(umx_scale_reliabilities("show", type = "alpha", digits = 4))
	expect_true(any(grepl(lowRes, oneDigit, fixed = TRUE)))
	expect_false(any(grepl(highRes, oneDigit, fixed = TRUE)))
	expect_true(any(grepl(highRes, fourDigit, fixed = TRUE)))
	# manual add appends, "print" aliases "show", and init clears
	umx_scale_reliabilities("add", scale_name = "N", reliability = 0.5, type = "alpha")
	shown = umx_scale_reliabilities("show")
	expect_equal(nrow(shown), 8)
	expect_equal(shown[shown$scale == "N", "reliability"], 0.5)
	expect_equal(umx_scale_reliabilities("print"), shown)
	umx_scale_reliabilities("init")
	expect_equal(nrow(getOption("umx_scale_reliabilities")), 0)
	expect_message(umx_scale_reliabilities("show"), "No scale reliabilities stored")
	# bad inputs fail with informative errors
	expect_error(umx_scale_reliabilities("explode"))
	expect_error(umx_scale_reliabilities("add", scale_name = "N", reliability = 0.5))
	expect_error(umx_scale_reliabilities("add", scale_name = "N", reliability = "high", type = "alpha"))
	expect_error(umx_scale_reliabilities("show", type = 5))
	# scoring with no initialized store leaves no store behind
	options(umx_scale_reliabilities = NULL)
	tmp = umx_score_scale("A", pos = 2:5, rev = 1, max = 6, data = bfi, name = "A", alpha = TRUE)
	expect_null(getOption("umx_scale_reliabilities"))
	expect_message(umx_scale_reliabilities("show"), "No scale reliabilities stored")
})
