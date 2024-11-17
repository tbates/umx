# library(testthat)
# library(umx)
# test_package("umx")
# test_active_file("~/bin/umx/tests/testthat/test_umx_CLPM.r")

# Test for the umxCLPM function
test_that("testing umxCLPM", {
	
	# Test valid input for Hamaker2015 model
	hamaker <- umxCLPM(waves = 4, name = "test_hamaker", model = "Hamaker2015", type = "WLS")
	expect_true(inherits(hamaker, "MxModel"))
	
	# Test valid input for Heise1969 model
	heise <- umxCLPM(waves = 4, name = "test_heise", model = "Heise1969")
	expect_true(inherits(heise, "MxModel"))
	
	# Test valid input for STARTS1995 model
	starts <- umxCLPM(waves = 4, name = "test_starts", model = "STARTS1995")
	expect_true(inherits(starts, "MxModel"))
	
	# Test valid input for IV_RI_CLPM model
	iv_ri <- umxCLPM(waves = 4, name = "test_iv_ri", model = "IV_RI_CLPM", type = "WLS")
	expect_true(inherits(iv_ri, "MxModel"))

	# Test for invalid model input
	expect_error({
		umxCLPM(waves = 4, name = "invalid_model", model = "InvalidModel")
	}, "Model must be one of Hamaker2015, Heise1969, IV_RI_CLPM, or STARTS1995")
	
	
})


test_that("testing xmu_relevel_factors", {
	
	# Create a sample data frame with factors (longitudinal data)
	df <- data.frame(
		group = factor(c("A", "B", "B", "C", "D", "E", "E", "E")),
		score = c(10, 15, 15, 20, 25, 30, 30, 30),
		other_factor = factor(c("one", "two", "two", "two", "three", "one", "four", "five"))
	)
	
	# Test that the function collapses levels correctly
	df_releveled <- xmu_relevel_factors(df, cols = c("group"), prop = 0.1, min = 2) 
	
	# Check that levels are collapsed
	expect_true(all(levels(df_releveled$group) %in% c("A", "B", "C", "D", "E"))) # Ensure no new levels created
	expect_equal(length(levels(df_releveled$group)), 5)  # Check the number of levels

	# Test that no changes are made when all levels are above minimum
	df_no_change <- data.frame(
		group = factor(c("A", "B", "C", "D", "E", "F")),
		score = c(10, 20, 30, 40, 50, 60)
	)
	df_releveled_no_change <- xmu_relevel_factors(df_no_change, cols = c("group"), prop = 0.1, min = 3) 
	expect_equal(levels(df_releveled_no_change$group), levels(df_no_change$group)) # No change in levels
	
	# Test that an error is thrown if columns are not factors
	df_error <- data.frame(
		group = c("A", "B", "C", "D", "E"),
		score = c(10, 20, 30, 40, 50) # Not a factor
	)
	
	expect_error(xmu_relevel_factors(df_error, cols = c("group"), prop = 0.1), 
							 "All specified columns must be factors.") # Expecting an error due to non-factor column

	# Test that a message is shown when levels are less than min
	df_low_levels <- data.frame(
		group = factor(c("A", "B")),
		score = c(10, 20)
	)
	
	expect_message(xmu_relevel_factors(df_low_levels, cols = c("group"), prop = 0.1, min = 3), 
								 "Number of levels in the first column is less than 3. No changes made.")
})
