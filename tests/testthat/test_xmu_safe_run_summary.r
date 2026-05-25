# library(testthat)
# library(umx)
# test_file("~/bin/umx/tests/testthat/test_xmu_make_mxData.r") 
# 
# test_package("umx")

test_that("testing xmu_make_mxData", {	
	tmp = mtcars
	tmp$disp = tmp$disp/100
	m1 = umxRAM("tim", data = tmp,
		umxPath(c("wt", "disp"), to = "mpg"),
		umxPath("wt", with = "disp"),
		umxPath(v.m. = c("wt", "disp", "mpg"))
	)
	m2 = umxModify(m1, "wt_to_mpg")
	
  # TODO add expected conditions!!
	# Summary ignored if run is false
	expect_silent(xmu_safe_run_summary(m1, autoRun = FALSE, summary = TRUE))
	# Run, no summary
	xmu_safe_run_summary(m1, autoRun = TRUE, summary = FALSE)
	# Default summary is just fit string
	xmu_safe_run_summary(m1, autoRun = TRUE, summary = TRUE)
	# Show std parameters
	xmu_safe_run_summary(m1, autoRun = TRUE, summary = TRUE, std = TRUE)
	# Run + Summary + comparison
	xmu_safe_run_summary(m1, m2, autoRun = TRUE, summary = TRUE, intervals = TRUE)
	# Run + Summary + no comparison
	xmu_safe_run_summary(m1, m2, autoRun = TRUE, summary = TRUE, std = TRUE, comparison= FALSE)
	
})
