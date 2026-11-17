library(testthat)
library(umx)
library(OpenMx)

test_that("umxSummary points at the concrete summary function", {
	# Defect: umxSummary(model, ...) hid the interface; users never learned e.g. umxSummaryCP
	data(twinData)
	data(GFF)
	data(demoOneFactor)
	# Silent mode leaks across files in full-suite runs; force it off (restored on exit)
	oldSilent = umx_set_silent(FALSE)
	on.exit(umx_set_silent(oldSilent), add = TRUE)
	mzData = subset(twinData, zygosity == "MZFF")
	dzData = subset(twinData, zygosity == "DZFF")
	mCP = umxCP("ptrcp", selDVs = c("gff", "fc"), nFac = 1, data = GFF, zyg = "zyg_2grp")
	mACE = umxACE("ptrace", selDVs = "bmi", dzData = dzData, mzData = mzData, sep = "")
	v = names(demoOneFactor)[1:3]
	mRAM = umxRAM("ptrram", data = demoOneFactor[, v], type = "cov", autoRun = FALSE,
		umxPath("G", to = v),
		umxPath(var = v),
		umxPath(var = "G", fixedAt = 1)
	)
	mRAM = mxRun(mRAM, silent = TRUE)
	cases = list(
		list(model = mCP, fun = "umxSummaryCP"),
		list(model = mACE, fun = "umxSummaryACE"),
		list(model = mRAM, fun = "umxSummary.MxModel")
	)
	for (cc in cases) {
		notes = character(0)
		withCallingHandlers(umxSummary(cc$model), message = function(m) {
			notes <<- c(notes, conditionMessage(m))
			invokeRestart("muffleMessage")
		})
		pointer = notes[grepl("parameter interface", notes, fixed = TRUE)]
		expect_equal(length(pointer), 1)
		expect_true(grepl(cc$fun, pointer, fixed = TRUE))
	}
	namedNotes = character(0)
	withCallingHandlers(umxSummary(mRAM), message = function(m) {
		namedNotes <<- c(namedNotes, conditionMessage(m))
		invokeRestart("muffleMessage")
	})
	expect_true(any(grepl("umxSummary.MxModel(mRAM)", namedNotes, fixed = TRUE)))
	directNotes = character(0)
	withCallingHandlers(umxSummaryCP(mCP), message = function(m) {
		directNotes <<- c(directNotes, conditionMessage(m))
		invokeRestart("muffleMessage")
	})
	expect_false(any(grepl("parameter interface", directNotes, fixed = TRUE)))
})
