# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_umxConfint.r") 
# test_package("umx")

context("umxConfint()")

test_that("umxConfint works", {
	require(umx)
	data(demoOneFactor)
	manifests = names(demoOneFactor)
	m1 = umxRAM("OneFactor", data = demoOneFactor, type = "cov",
		umxPath(from = "G", to = manifests),
		umxPath(var = manifests),
		umxPath(var = "G", fixedAt = 1)
	)
	
	# There are no existing CI requests...
	expect_message(umxConfint(m1, run = TRUE), regexp = "Polite note: This model has no CIs yet")
	
	# Add a CI request for "G_to_x1", run, and report. Save with this CI computed
	m2 = umxConfint(m1, parm = "G_to_x1", run = TRUE) 
	
	# Just print out any existing CIs
	umxConfint(m2)
	
	# CI requests added for free matrix parameters. User prompted to set run = TRUE
	expect_message(umxConfint(m1, "all"), regexp = "Polite note: Some CIs have been requested but not run. Add 'run = TRUE'")
	
	# Run the requested CIs
	m3 = umxConfint(m3, run = TRUE) 
	
	# Run CIs for free one-headed (asymmetric) paths in RAM model. 
	#   note: Deletes other existing requests,
	tmp = umxConfint(m1, parm = "A", run = TRUE)
	
	# Wipe existing CIs, add G_to_x1
	tmp = umxConfint(m1, parm = "G_to_x1", run = TRUE, wipeExistingRequests = TRUE) 
	
	# For some twin models, a "smart" mode is implemented
	# note: only implemented for umxCP so far
	expect_error(umxConfint(m1, "smart"))

})
