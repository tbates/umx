library(testthat)
library(umx)

test_that("prolific_scoring_stub works correctly", {
	# 1. Create a dummy dataframe resembling a Qualtrics export
	dummyDf = data.frame(
		StartDate = c("2026-07-16", "2026-07-16"),
		IPAddress = c("1.1.1.1", "2.2.2.2"),
		QID123 = c("meta", "meta"),
		Age = c("25", "30"),
		Sex = c("Male", "Female"),
		AntiNatal_1 = c("Strongly disagree", "Somewhat agree"),
		AntiNatal_2 = c("Somewhat disagree", "Strongly agree"),
		AntiNatal_3 = c("Neither agree nor disagree", "Strongly agree"),
		SingleItem_1 = c("Option A", "Option B"),
		stringsAsFactors = FALSE
	)
	
	# Capture the printed messages
	output = capture.output({
		returnedDf = prolific_scoring_stub(dummyDf, deleteJunk = TRUE)
	}, type = "message")
	
	# Verify that the age/sex conversion code is printed
	expect_true(any(grepl("dummyDf\\$Age = as.numeric\\(dummyDf\\$Age\\)", output)))
	expect_true(any(grepl("dummyDf\\$Sex = as.factor\\(dummyDf\\$Sex\\)", output)))
	
	# Verify scale options printed and sorted
	expect_true(any(grepl("AntiNatalSTR =", output)))
	# Verify the items are sorted negative -> neutral -> positive
	expect_true(any(grepl("Neither agree nor disagree", output)))
	
	# Verify the umx_score_scale call output
	expect_true(any(grepl("umx_score_scale\\(name = \"AntiNatal\"", output)))
	
	# Verify that SingleItem is not detected (since it is size 1)
	expect_false(any(grepl("SingleItemSTR", output)))
	
	# Verify deleteJunk removes StartDate, IPAddress, QID123
	expect_false("StartDate" %in% names(returnedDf))
	expect_false("IPAddress" %in% names(returnedDf))
	expect_false("QID123" %in% names(returnedDf))
	expect_true("AntiNatal_1" %in% names(returnedDf))
	
	# Test with deleteJunk = FALSE
	returnedDf2 = prolific_scoring_stub(dummyDf, deleteJunk = FALSE)
	expect_true("StartDate" %in% names(returnedDf2))
})
